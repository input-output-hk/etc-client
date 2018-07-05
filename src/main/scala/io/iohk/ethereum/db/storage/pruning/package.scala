package io.iohk.ethereum.db.storage

import io.iohk.ethereum.mpt.NodesKeyValueStorage

package object pruning {

  sealed trait PruningMode
  case object ArchivePruning extends PruningMode
  case class FastSyncPruning(history: Int) extends PruningMode
  case class BasicPruning(history: Int) extends PruningMode

  trait PruneSupport {
    /**
      * Remove unused data for the given block number
      * @param blockNumber BlockNumber to prune
      * @param nodeStorage NodeStorage
      */
    def prune(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean)

    /**
      * Rollbacks blocknumber changes
      * @param blockNumber BlockNumber to rollback
      * @param nodeStorage NodeStorage
      */
    def rollback(blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean)
  }

  object PruningMode {
    /**
      * Create a NodesKeyValueStorage to be used within MerklePatriciaTrie
      *
      * @param blockNumber block number to be used as tag when doing update / removal operations. None can be sent if read only
      * @return Storage to be used
      */
    def nodesKeyValueStorage(pruningMode: PruningMode, nodeStorage: NodesStorage)(blockNumber: Option[BigInt]): NodesKeyValueStorage =
      pruningMode match {
        case ArchivePruning => new ArchiveNodeStorage(nodeStorage)
        case BasicPruning(_) => new ReferenceCountNodeStorage(nodeStorage, blockNumber, true)
        case FastSyncPruning(_) => new ReferenceCountNodeStorage(nodeStorage, blockNumber, false)
      }

    /**
      * Prunes node storage for a given pruning mode and block number
      * @param pruningMode Pruning mode tobe used
      * @param blockNumber Block number to prune
      * @param nodeStorage NodeStorage
      */
    def prune(pruningMode: PruningMode, blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit =
      pruningMode match {
        case ArchivePruning => ArchiveNodeStorage.prune(blockNumber, nodeStorage, inMemory)
        case BasicPruning(history) => ReferenceCountNodeStorage.prune(blockNumber - history, nodeStorage, inMemory)
        case FastSyncPruning(history) => ReferenceCountNodeStorage.prune(blockNumber - history, nodeStorage, inMemory)
      }

    /**
      * Rollback changed made for a given pruning mode and block number
      * @param pruningMode Pruning mode tobe used
      * @param blockNumber Block number to rollback
      * @param nodeStorage NodeStorage
      */
    def rollback(pruningMode: PruningMode, blockNumber: BigInt, nodeStorage: NodesStorage, inMemory: Boolean): Unit = {
      val pruneSupport: PruneSupport = pruningMode match {
        case ArchivePruning => ArchiveNodeStorage
        case BasicPruning(_) => ReferenceCountNodeStorage
        case FastSyncPruning(_) => ReferenceCountNodeStorage
      }
      pruneSupport.rollback(blockNumber, nodeStorage, inMemory)
    }
  }
}
