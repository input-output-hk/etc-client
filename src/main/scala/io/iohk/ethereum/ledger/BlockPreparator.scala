package io.iohk.ethereum.ledger

import io.iohk.ethereum.consensus.validators.SignedTransactionError.TransactionSignatureError
import io.iohk.ethereum.consensus.validators.{SignedTransactionError, SignedTransactionValid, SignedTransactionValidator}
import io.iohk.ethereum.domain.UInt256._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{StateBeforeFailure, TxsExecutionError}
import io.iohk.ethereum.ledger.Ledger._
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.vm.{PC => _, _}

import scala.annotation.tailrec

/**
 * This is used from a [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]].
 *
 * With its introduction, we:
 *
 *   1. Avoid a direct dependency of [[io.iohk.ethereum.consensus.Consensus Consensus]] on
 *      [[io.iohk.ethereum.ledger.Ledger Ledger]].
 *   2. Extract a substantial chunk of functionality outside [[io.iohk.ethereum.ledger.Ledger Ledger]],
 *      in an attempt to modularize it.
 *
 */
class BlockPreparator(
  vm: VMImpl,
  signedTxValidator: SignedTransactionValidator,
  blockchain: BlockchainImpl, // FIXME Depend on the interface
  blockchainConfig: BlockchainConfig
) extends Logger {

  // NOTE We need a lazy val here, not a plain val, otherwise a mocked BlockChainConfig
  //      in some irrelevant test can throw an exception.
  private[ledger] lazy val blockRewardCalculator = new BlockRewardCalculator(
      blockchainConfig.monetaryPolicyConfig,
      blockchainConfig.byzantiumBlockNumber,
      blockchainConfig.constantinopleBlockNumber
    )

  /**
   * This function updates state in order to pay rewards based on YP section 11.3
   *
   * @param block
   * @param worldStateProxy
   * @return
   */
  private[ledger] def payBlockReward(block: Block, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def getAccountToPay(address: Address, ws: InMemoryWorldStateProxy): Account =
      ws.getAccount(address).getOrElse(Account.empty(blockchainConfig.accountStartNonce))

    val blockNumber = block.header.number

    val minerAddress = Address(block.header.beneficiary)
    val minerAccount = getAccountToPay(minerAddress, worldStateProxy)
    val minerReward = blockRewardCalculator.calcBlockMinerReward(blockNumber, block.body.uncleNodesList.size)

    val afterMinerReward = worldStateProxy.saveAccount(minerAddress, minerAccount.increaseBalance(UInt256(minerReward)))
    log.debug(s"Paying block $blockNumber reward of $minerReward to miner with account address $minerAddress")

    block.body.uncleNodesList.foldLeft(afterMinerReward) { (ws, ommer) =>
      val ommerAddress = Address(ommer.beneficiary)
      val account = getAccountToPay(ommerAddress, ws)

      val ommerReward = blockRewardCalculator.calcOmmerMinerReward(blockNumber, ommer.number)

      log.debug(s"Paying block $blockNumber reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.increaseBalance(UInt256(ommerReward)))
    }
  }

  /**
   * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price). See YP equation number (68)
   *
   * @param tx Target transaction
   * @return Upfront cost
   */
  private[ledger] def calculateUpfrontGas(tx: Transaction): UInt256 = UInt256(tx.gasLimit * tx.gasPrice)

  /**
   * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price) + Tv (Tx value). See YP equation number (65)
   *
   * @param tx Target transaction
   * @return Upfront cost
   */
  private[ledger] def calculateUpfrontCost(tx: Transaction): UInt256 =
    UInt256(calculateUpfrontGas(tx) + tx.value)

  /**
   * Increments account nonce by 1 stated in YP equation (69) and
   * Pays the upfront Tx gas calculated as TxGasPrice * TxGasLimit from balance. YP equation (68)
   *
   * @param stx
   * @param worldStateProxy
   * @return
   */
  private[ledger] def updateSenderAccountBeforeExecution(stx: SignedTransaction,
                                                         senderAddress: Address,
                                                         worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val account = worldStateProxy.getGuaranteedAccount(senderAddress)
    worldStateProxy.saveAccount(senderAddress, account.increaseBalance(-calculateUpfrontGas(stx.tx)).increaseNonce())
  }

  private[ledger] def runVM(stx: SignedTransaction, senderAddress: Address, blockHeader: BlockHeader, world: InMemoryWorldStateProxy): PR = {
    val evmConfig = EvmConfig.forBlock(blockHeader.number, blockchainConfig)
    val context: PC = ProgramContext(stx, blockHeader, senderAddress, world, evmConfig)
    vm.run(context)
  }

  /**
   * Calculate total gas to be refunded
   * See YP, eq (72)
   */
  private[ledger] def calcTotalGasToRefund(stx: SignedTransaction, result: PR): BigInt = {
    result.error.map(_.useWholeGas) match {
      case Some(true)   => 0
      case Some(false)  => result.gasRemaining
      case None         =>
        val gasUsed = stx.tx.gasLimit - result.gasRemaining
        result.gasRemaining + (gasUsed / 2).min(result.gasRefund)
    }
  }

  private[ledger] def pay(address: Address, value: UInt256, withTouch: Boolean)(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    if (world.isZeroValueTransferToNonExistentAccount(address, value)) {
      world
    } else {
      val account = world.getAccount(address).getOrElse(Account.empty(blockchainConfig.accountStartNonce)).increaseBalance(value)
      val savedWorld = world.saveAccount(address, account)

      if (withTouch) savedWorld.touchAccounts(address) else savedWorld
    }
  }

  /**
   * Delete all accounts (that appear in SUICIDE list). YP eq (78).
   * The contract storage should be cleared during pruning as nodes could be used in other tries.
   * The contract code is also not deleted as there can be contracts with the exact same code, making it risky to delete
   * the code of an account in case it is shared with another one.
   * FIXME: [EC-242]
   *   Should we delete the storage associated with the deleted accounts?
   *   Should we keep track of duplicated contracts for deletion?
   *
   * @param addressesToDelete
   * @param worldStateProxy
   * @return a worldState equal worldStateProxy except that the accounts from addressesToDelete are deleted
   */
  private[ledger] def deleteAccounts(addressesToDelete: Set[Address])(worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    addressesToDelete.foldLeft(worldStateProxy){ case (world, address) => world.deleteAccount(address) }

  /**
   * EIP161 - State trie clearing
   * Delete all accounts that have been touched (involved in any potentially state-changing operation) during transaction execution.
   *
   * All potentially state-changing operation are:
   * Account is the target or refund of a SUICIDE operation for zero or more value;
   * Account is the source or destination of a CALL operation or message-call transaction transferring zero or more value;
   * Account is the source or newly-creation of a CREATE operation or contract-creation transaction endowing zero or more value;
   * as the block author ("miner") it is recipient of block-rewards or transaction-fees of zero or more.
   *
   * Deletion of touched account should be executed immediately following the execution of the suicide list
   *
   * @param world world after execution of all potentially state-changing operations
   * @return a worldState equal worldStateProxy except that the accounts touched during execution are deleted and touched
   *         Set is cleared
   */
  private[ledger] def deleteEmptyTouchedAccounts(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def deleteEmptyAccount(world: InMemoryWorldStateProxy, address: Address) = {
      if (world.getAccount(address).exists(_.isEmpty(blockchainConfig.accountStartNonce)))
        world.deleteAccount(address)
      else
        world
    }

    world.touchedAccounts
      .foldLeft(world)(deleteEmptyAccount)
      .clearTouchedAccounts
  }

  private[ledger] def executeTransaction(stx: SignedTransaction, senderAddress: Address, blockHeader: BlockHeader, world: InMemoryWorldStateProxy): TxResult = {
    log.debug(s"Transaction ${stx.hashAsHexString} execution start")
    val gasPrice = UInt256(stx.tx.gasPrice)
    val gasLimit = stx.tx.gasLimit

    val checkpointWorldState = updateSenderAccountBeforeExecution(stx, senderAddress, world)
    val result = runVM(stx, senderAddress, blockHeader, checkpointWorldState)

    val resultWithErrorHandling: PR =
      if (result.error.isDefined) {
        //Rollback to the world before transfer was done if an error happened
        result.copy(world = checkpointWorldState, addressesToDelete = Set.empty, logs = Nil)
      } else
        result

    val totalGasToRefund = calcTotalGasToRefund(stx, resultWithErrorHandling)
    val executionGasToPayToMiner = gasLimit - totalGasToRefund

    val refundGasFn = pay(senderAddress, (totalGasToRefund * gasPrice).toUInt256, withTouch = false) _
    val payMinerForGasFn = pay(Address(blockHeader.beneficiary), (executionGasToPayToMiner * gasPrice).toUInt256, withTouch = true) _

    val worldAfterPayments = (refundGasFn andThen payMinerForGasFn)(resultWithErrorHandling.world)

    val deleteAccountsFn = deleteAccounts(resultWithErrorHandling.addressesToDelete) _
    val deleteTouchedAccountsFn = deleteEmptyTouchedAccounts _
    val persistStateFn = InMemoryWorldStateProxy.persistState _

    val world2 = (deleteAccountsFn andThen deleteTouchedAccountsFn andThen persistStateFn)(worldAfterPayments)

    log.debug(
      s"""Transaction ${stx.hashAsHexString} execution end. Summary:
         | - Error: ${result.error}.
         | - Total Gas to Refund: $totalGasToRefund
         | - Execution gas paid to miner: $executionGasToPayToMiner""".stripMargin)

    TxResult(world2, executionGasToPayToMiner, resultWithErrorHandling.logs, result.returnData, result.error)
  }

  // scalastyle:off method.length
  /**
   * This functions executes all the signed transactions from a block (till one of those executions fails)
   *
   * @param signedTransactions from the block that are left to execute
   * @param world that will be updated by the execution of the signedTransactions
   * @param blockHeader of the block we are currently executing
   * @param acumGas, accumulated gas of the previoulsy executed transactions of the same block
   * @param acumReceipts, accumulated receipts of the previoulsy executed transactions of the same block
   * @return a BlockResult if the execution of all the transactions in the block was successful or a BlockExecutionError
   *         if one of them failed
   */
  @tailrec
  private[ledger] final def executeTransactions(
    signedTransactions: Seq[SignedTransaction],
    world: InMemoryWorldStateProxy,
    blockHeader: BlockHeader,
    acumGas: BigInt = 0,
    acumReceipts: Seq[Receipt] = Nil
  ): Either[TxsExecutionError, BlockResult] =
    signedTransactions match {
      case Nil =>
        Right(BlockResult(worldState = world, gasUsed = acumGas, receipts = acumReceipts))

      case Seq(stx, otherStxs@_*) =>
        val upfrontCost = calculateUpfrontCost(stx.tx)
        val senderAddress = SignedTransaction.getSender(stx)

        val accountData = senderAddress.map { address =>
          world
            .getAccount(address)
            .map(a => (a, address))
            .getOrElse((Account.empty(blockchainConfig.accountStartNonce), address))
        }.toRight(TransactionSignatureError)

        val validatedStx: Either[SignedTransactionError, ((Account, Address), SignedTransactionValid)] = for {
          accData <- accountData
          result  <- signedTxValidator.validate(stx, accData._1, blockHeader, upfrontCost, acumGas)
        } yield (accData, result)

        validatedStx match {
          case Right(((account, address), _)) =>
            val TxResult(newWorld, gasUsed, logs, _, vmError) = executeTransaction(stx, address, blockHeader, world.saveAccount(address, account))

            // spec: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-658.md
            val transactionOutcome = if (
              blockHeader.number >= blockchainConfig.byzantiumBlockNumber ||
              blockHeader.number >= blockchainConfig.atlantisBlockNumber
            ) {
              if (vmError.isDefined) FailureOutcome else SuccessOutcome
            } else {
              HashOutcome(newWorld.stateRootHash)
            }

            val receipt = Receipt(
              postTransactionStateHash = transactionOutcome,
              cumulativeGasUsed = acumGas + gasUsed,
              logsBloomFilter = BloomFilter.create(logs),
              logs = logs
            )

            log.debug(s"Receipt generated for tx ${stx.hashAsHexString}, $receipt")

            executeTransactions(otherStxs, newWorld, blockHeader, receipt.cumulativeGasUsed, acumReceipts :+ receipt)
          case Left(error) => Left(TxsExecutionError(stx, StateBeforeFailure(world, acumGas, acumReceipts), error.toString))
        }
    }

  @tailrec
  private[ledger] final def executePreparedTransactions(
    signedTransactions: Seq[SignedTransaction],
    world: InMemoryWorldStateProxy,
    blockHeader: BlockHeader,
    acumGas: BigInt = 0,
    acumReceipts: Seq[Receipt] = Nil,
    executed: Seq[SignedTransaction] = Nil
  ): (BlockResult, Seq[SignedTransaction]) = {

    val result = executeTransactions(signedTransactions, world, blockHeader, acumGas, acumReceipts)

    result match {
      case Left(TxsExecutionError(stx, StateBeforeFailure(worldState, gas, receipts), reason)) =>
        log.debug(s"failure while preparing block because of $reason in transaction with hash ${stx.hashAsHexString}")
        val txIndex = signedTransactions.indexWhere(tx => tx.hash == stx.hash)
        executePreparedTransactions(signedTransactions.drop(txIndex + 1),
          worldState, blockHeader, gas, receipts, executed ++ signedTransactions.take(txIndex))
      case Right(br) => (br, executed ++ signedTransactions)
    }
  }

  def prepareBlock(block: Block): BlockPreparationResult = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val initialWorld = blockchain.getReadOnlyWorldStateProxy(None, blockchainConfig.accountStartNonce, parentStateRoot,
      noEmptyAccounts = false,
      ethCompatibleStorage = blockchainConfig.ethCompatibleStorage)
    val prepared = executePreparedTransactions(block.body.transactionList, initialWorld, block.header)

    prepared match {
      case (execResult@BlockResult(resultingWorldStateProxy, _, _), txExecuted) =>
        val worldToPersist = payBlockReward(block, resultingWorldStateProxy)
        val worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist)
        BlockPreparationResult(block.copy(body = block.body.copy(transactionList = txExecuted)), execResult, worldPersisted.stateRootHash, worldToPersist)
    }
  }
}
