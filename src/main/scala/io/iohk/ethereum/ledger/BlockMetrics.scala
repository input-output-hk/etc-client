package io.iohk.ethereum.ledger

import akka.util.ByteString

import com.google.common.util.concurrent.AtomicDouble

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.metrics.MetricsContainer

case object BlockMetrics extends MetricsContainer {

  final private[this] val BlockNumberGauge =
    metrics.registry.gauge("sync.block.number.gauge", new AtomicDouble(0d))
  final private[this] val CheckpointBlockNumberGauge =
    metrics.registry.gauge("sync.block.checkpoint.number.gauge", new AtomicDouble(0d))
  final private[this] val BlockGasLimitGauge =
    metrics.registry.gauge("sync.block.gasLimit.gauge", new AtomicDouble(0d))
  final private[this] val BlockGasUsedGauge =
    metrics.registry.gauge("sync.block.gasUsed.gauge", new AtomicDouble(0d))
  final private[this] val BlockDifficultyGauge =
    metrics.registry.gauge("sync.block.difficulty.gauge", new AtomicDouble(0d))
  final private[this] val BlockTransactionsGauge =
    metrics.registry.gauge("sync.block.transactions.gauge", new AtomicDouble(0d))
  final private[this] val BlockUnclesGauge =
    metrics.registry.gauge("sync.block.uncles.gauge", new AtomicDouble(0d))
  final private[this] val TimeBetweenParentGauge =
    metrics.registry.gauge("sync.block.timeBetweenParent.seconds.gauge", new AtomicDouble(0d))

  def measure(block: Block, getBlockByHashFn: ByteString => Option[Block]): Unit = {
    BlockNumberGauge.set(block.number.toDouble)
    if (block.hasCheckpoint)
      CheckpointBlockNumberGauge.set(block.number.toDouble)
    BlockGasLimitGauge.set(block.header.gasLimit.toDouble)
    BlockGasUsedGauge.set(block.header.gasUsed.toDouble)
    BlockDifficultyGauge.set(block.header.difficulty.toDouble)
    BlockTransactionsGauge.set(block.body.numberOfTxs)
    BlockUnclesGauge.set(block.body.numberOfUncles)

    getBlockByHashFn(block.header.parentHash) match {
      case Some(parentBlock) =>
        val timeBetweenBlocksInSeconds: Long =
          block.header.unixTimestamp - parentBlock.header.unixTimestamp
        TimeBetweenParentGauge.set(timeBetweenBlocksInSeconds.toDouble)
      case None => ()
    }
  }

}
