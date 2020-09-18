package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain.{ Block, BlockHeader, Blockchain, Receipt }
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError

class BlockValidation(consensus: Consensus, blockchain: Blockchain, blockQueue: BlockQueue) {

  def validateBlockBeforeExecution(block: Block): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {
    consensus.validators.validateBlockBeforeExecution(
      block = block,
      getBlockHeaderByHash = getBlockHeaderFromChainOrQueue,
      getNBlocksBack = getNBlocksBackFromChainOrQueue
    )
  }

  private def getBlockHeaderFromChainOrQueue(hash: ByteString): Option[BlockHeader] = {
    blockchain.getBlockHeaderByHash(hash).orElse(blockQueue.getBlockByHash(hash).map(_.header))
  }

  private def getNBlocksBackFromChainOrQueue(hash: ByteString, n: Int): List[Block] = {
    val queuedBlocks = blockQueue.getBranch(hash, dequeue = false).take(n)
    if (queuedBlocks.length == n) {
      queuedBlocks
    } else {
      val chainedBlockHash = queuedBlocks.headOption.map(_.header.parentHash).getOrElse(hash)
      blockchain.getBlockByHash(chainedBlockHash) match {
        case None =>
          Nil

        case Some(block) =>
          val remaining = n - queuedBlocks.length - 1
          val numbers = (block.header.number - remaining) until block.header.number
          val blocks = (numbers.toList.flatMap(blockchain.getBlockByNumber) :+ block) ::: queuedBlocks
          blocks
      }
    }
  }

  def validateBlockAfterExecution(
    block: Block,
    hash: ByteString,
    receipts: Seq[Receipt],
    gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = {
    consensus.validators.validateBlockAfterExecution(
      block = block,
      stateRootHash = hash,
      receipts = receipts,
      gasUsed = gasUsed
    )
  }
}
