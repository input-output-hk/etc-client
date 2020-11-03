package io.iohk.ethereum.ledger

import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger.BlockResult
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, Logger}
import io.iohk.ethereum.vm.EvmConfig

import scala.annotation.tailrec

class BlockExecution(
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    blockPreparator: BlockPreparator,
    blockValidation: BlockValidation
) extends Logger {

  /** Executes and validate a block
    *
    * @param alreadyValidated should we skip pre-execution validation (if the block has already been validated,
    *                         eg. in the importBlock method)
    */
  def executeAndValidateBlock(
      block: Block,
      alreadyValidated: Boolean = false
  ): Either[BlockExecutionError, Seq[Receipt]] = {
    val preExecValidationResult =
      if (alreadyValidated) Right(block) else blockValidation.validateBlockBeforeExecution(block)

    val blockExecResult = {
      if (block.hasCheckpoint) {
        // block with checkpoint is not executed normally - it's not need to do after execution validation
        preExecValidationResult
          .map(_ => Seq.empty[Receipt])
      } else {
        for {
          _ <- preExecValidationResult
          result <- executeBlock(block)
          _ <- blockValidation.validateBlockAfterExecution(
            block,
            result.worldState.stateRootHash,
            result.receipts,
            result.gasUsed
          )
        } yield result.receipts
      }
    }

    if (blockExecResult.isRight) {
      log.debug(s"Block ${block.header.number} (with hash: ${block.header.hashAsHexString}) executed correctly")
    }

    blockExecResult
  }

  /** Executes a block (executes transactions and pays rewards) */
  private def executeBlock(block: Block): Either[BlockExecutionError, BlockResult] = {
    for {
      execResult <- executeBlockTransactions(block)
      worldToPersist = blockPreparator.payBlockReward(block, execResult.worldState)
      // State root hash needs to be up-to-date for validateBlockAfterExecution
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist)
    } yield execResult.copy(worldState = worldPersisted)
  }

  /** This function runs transactions
    *
    * @param block the block with transactions to run
    */
  private[ledger] def executeBlockTransactions(block: Block): Either[BlockExecutionError, BlockResult] = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val blockHeaderNumber = block.header.number
    val initialWorld = blockchain.getWorldStateProxy(
      blockNumber = blockHeaderNumber,
      accountStartNonce = blockchainConfig.accountStartNonce,
      stateRootHash = parentStateRoot,
      noEmptyAccounts = EvmConfig.forBlock(blockHeaderNumber, blockchainConfig).noEmptyAccounts,
      ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
    )

    val inputWorld = blockchainConfig.daoForkConfig match {
      case Some(daoForkConfig) if daoForkConfig.isDaoForkBlock(blockHeaderNumber) =>
        drainDaoForkAccounts(initialWorld, daoForkConfig)
      case _ => initialWorld
    }

    val hashAsHexString = block.header.hashAsHexString
    val transactionList = block.body.transactionList
    log.debug(
      s"About to execute ${transactionList.size} txs from block $blockHeaderNumber (with hash: $hashAsHexString)"
    )
    val blockTxsExecResult = blockPreparator.executeTransactions(transactionList, inputWorld, block.header)
    blockTxsExecResult match {
      case Right(_) => log.debug(s"All txs from block $hashAsHexString were executed successfully")
      case Left(error) =>
        log.debug(s"Not all txs from block $hashAsHexString were executed correctly, due to ${error.reason}")
    }
    blockTxsExecResult
  }

  /** This function updates worldState transferring balance from drainList accounts to refundContract address
    *
    * @param worldState     initial world state
    * @param daoForkConfig  dao fork configuration with drainList and refundContract config
    * @return updated world state proxy
    */
  private def drainDaoForkAccounts(
      worldState: InMemoryWorldStateProxy,
      daoForkConfig: DaoForkConfig
  ): InMemoryWorldStateProxy = {
    daoForkConfig.refundContract match {
      case Some(refundContractAddress) =>
        daoForkConfig.drainList.foldLeft(worldState) { (ws, address) =>
          ws.getAccount(address)
            .map(account => ws.transfer(from = address, to = refundContractAddress, account.balance))
            .getOrElse(ws)
        }
      case None => worldState
    }
  }

  /** Executes and validates a list of blocks, storing the results in the blockchain.
    *
    * @param blocks   blocks to be executed
    * @param parentTd transaction difficulty of the parent
    *
    * @return a list of blocks that were correctly executed and an optional [[BlockExecutionError]]
    */
  def executeAndValidateBlocks(
      blocks: List[Block],
      parentTd: BigInt
  ): (List[BlockData], Option[BlockExecutionError]) = {
    @tailrec
    def go(
        executedBlocks: List[BlockData],
        remainingBlocks: List[Block],
        parentTd: BigInt,
        error: Option[BlockExecutionError]
    ): (List[BlockData], Option[BlockExecutionError]) = {
      if (remainingBlocks.isEmpty) {
        (executedBlocks.reverse, None)
      } else if (error.isDefined) {
        (executedBlocks.reverse, error)
      } else {
        val blockToExecute = remainingBlocks.head
        executeAndValidateBlock(blockToExecute, alreadyValidated = true) match {
          case Right(receipts) =>
            val td = parentTd + blockToExecute.header.difficulty
            val newBlockData = BlockData(blockToExecute, receipts, td)
            blockchain.save(newBlockData.block, newBlockData.receipts, newBlockData.td, saveAsBestBlock = true)
            go(newBlockData :: executedBlocks, remainingBlocks.tail, td, None)
          case Left(executionError) =>
            go(executedBlocks, remainingBlocks, 0, Some(executionError))
        }
      }
    }

    go(List.empty[BlockData], blocks, parentTd, None)
  }

}

sealed trait BlockExecutionError {
  val reason: Any
}

sealed trait BlockExecutionSuccess

case object BlockExecutionSuccess extends BlockExecutionSuccess

object BlockExecutionError {
  case class ValidationBeforeExecError(reason: Any) extends BlockExecutionError

  case class StateBeforeFailure(worldState: InMemoryWorldStateProxy, acumGas: BigInt, acumReceipts: Seq[Receipt])

  case class TxsExecutionError(stx: SignedTransaction, stateBeforeError: StateBeforeFailure, reason: String)
      extends BlockExecutionError

  case class ValidationAfterExecError(reason: String) extends BlockExecutionError

  case class UnKnownExecutionError(reason: String) extends BlockExecutionError
}
