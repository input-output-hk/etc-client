package io.iohk.ethereum.consensus.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparationError, BlockPreparator}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.BlockchainConfig


abstract class NoOmmersBlockGenerator(
  blockchain: Blockchain,
  blockchainConfig: BlockchainConfig,
  consensusConfig: ConsensusConfig,
  blockPreparator: BlockPreparator,
  blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends BlockGeneratorSkeleton(
  blockchain,
  blockchainConfig,
  consensusConfig,
  blockPreparator,
  blockTimestampProvider
) {

  type X = Nil.type

  protected def newBlockBody(transactions: Seq[SignedTransaction], ommers: Nil.type): BlockBody = {
    BlockBody(transactions, ommers)
  }

  protected def prepareHeader(
    blockNumber: BigInt, parent: Block,
    beneficiary: Address, blockTimestamp: Long,
    ommers: Nil.type
  ): BlockHeader =
    defaultPrepareHeader(blockNumber, parent, beneficiary, blockTimestamp, ommers)


  /** An empty `X` */
  def emptyX: Nil.type = Nil

  def generateBlock(
    parent: Block,
    transactions: Seq[SignedTransaction],
    beneficiary: Address,
    ommers: Nil.type
  ): Either[BlockPreparationError, PendingBlock] = {

    val pHeader = parent.header
    val blockNumber = pHeader.number + 1

    val prepared = prepareBlock(parent, transactions, beneficiary, blockNumber, blockPreparator, ommers)
    cache.updateAndGet((t: List[PendingBlockAndState]) => (prepared :: t).take(blockCacheSize))

    Right(prepared.pendingBlock)
  }
}
