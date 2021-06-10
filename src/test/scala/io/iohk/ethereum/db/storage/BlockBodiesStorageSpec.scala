package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.network.p2p.messages.CommonMessages
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

class BlockBodiesStorageSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with ObjectGenerators
    with SecureRandomBuilder {

  val chainId: Option[BigInt] = Some(1)

  "BlockBodiesStorage" should {

    "insert block body properly" in {
      forAll(Gen.listOfN(32, ObjectGenerators.newBlockGen(secureRandom, chainId))) { newBlocks =>
        val blocks = newBlocks.distinct
        val totalStorage = insertBlockBodiesMapping(newBlocks)

        blocks.foreach { case NewBlock(block, _) =>
          assert(totalStorage.get(block.header.hash).contains(block.body))
        }
      }
    }

    "delete block body properly" in {
      forAll(Gen.listOfN(32, ObjectGenerators.newBlockGen(secureRandom, chainId))) { newBlocks =>
        val blocks = newBlocks.distinct
        val storage = insertBlockBodiesMapping(newBlocks)

        // Mapping of block bodies is deleted
        val (toDelete, toLeave) = blocks.splitAt(Gen.choose(0, blocks.size).sample.get)

        val batchUpdates = toDelete.foldLeft(storage.emptyBatchUpdate) { case (updates, NewBlock(block, _)) =>
          updates.and(storage.remove(block.header.hash))
        }

        batchUpdates.commit()

        toLeave.foreach { case NewBlock(block, _) =>
          assert(storage.get(block.header.hash).contains(block.body))
        }
        toDelete.foreach { case NewBlock(block, _) => assert(storage.get(block.header.hash).isEmpty) }
      }
    }

    def insertBlockBodiesMapping(newBlocks: Seq[CommonMessages.NewBlock]): BlockBodiesStorage = {
      val storage = new BlockBodiesStorage(EphemDataSource())

      val batchUpdates = newBlocks.foldLeft(storage.emptyBatchUpdate) { case (updates, NewBlock(block, _)) =>
        updates.and(storage.put(block.header.hash, block.body))
      }

      batchUpdates.commit()
      storage
    }
  }
}
