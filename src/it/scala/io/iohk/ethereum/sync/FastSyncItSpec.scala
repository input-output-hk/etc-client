package io.iohk.ethereum.sync

import java.net.{InetSocketAddress, ServerSocket}

import akka.util.ByteString
import io.iohk.ethereum.FlatSpecBase
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.sync.FastSyncItSpec._
import io.iohk.ethereum.sync.FastSyncItSpecUtils.{FakePeer, FakePeerCustomConfig, HostConfig}
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class FastSyncItSpec extends FlatSpecBase with Matchers with BeforeAndAfter {
  implicit val testScheduler = Scheduler.fixedPool("test", 16)

  "FastSync" should "should sync blockchain without state nodes" in customTestCaseResourceM(
    FakePeer.start3FakePeersRes()
  ) { case (peer1, peer2, peer3) =>
    for {
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer3.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
      assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
    }
  }

  it should "should sync blockchain with state nodes" in customTestCaseResourceM(FakePeer.start3FakePeersRes()) {
    case (peer1, peer2, peer3) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer3.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
        _ <- peer1.startFastSync().delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        val trie = peer1.getBestBlockTrie()
        val synchronizingPeerHaveAllData = peer1.containsExpectedDataUpToAccountAtBlock(1000, 500)
        // due to the fact that function generating state is deterministic both peer2 and peer3 ends up with exactly same
        // state, so peer1 can get whole trie from both of them.
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
        assert(trie.isDefined)
        assert(synchronizingPeerHaveAllData)
      }
  }

  it should "should sync blockchain with state nodes when peer do not response with full responses" in
    customTestCaseResourceM(
      FakePeer.start3FakePeersRes(
        fakePeerCustomConfig2 = FakePeerCustomConfig(HostConfig()),
        fakePeerCustomConfig3 = FakePeerCustomConfig(HostConfig())
      )
    ) { case (peer1, peer2, peer3) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer3.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
        _ <- peer1.startFastSync().delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        val trie = peer1.getBestBlockTrie()
        val synchronizingPeerHaveAllData = peer1.containsExpectedDataUpToAccountAtBlock(1000, 500)
        // due to the fact that function generating state is deterministic both peer2 and peer3 ends up with exactly same
        // state, so peer1 can get whole trie from both of them.
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
        assert(trie.isDefined)
        assert(synchronizingPeerHaveAllData)
      }
    }

  it should "should sync blockchain with state nodes when one of the peers send empty state responses" in
    customTestCaseResourceM(
      FakePeer.start3FakePeersRes(
        fakePeerCustomConfig2 = FakePeerCustomConfig(HostConfig()),
        fakePeerCustomConfig3 = FakePeerCustomConfig(HostConfig().copy(maxMptComponentsPerMessage = 0))
      )
    ) { case (peer1, peer2, peer3) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer3.importBlocksUntil(1000)(updateStateAtBlock(500))
        _ <- peer1.connectToPeers(Set(peer2.node, peer3.node))
        _ <- peer1.startFastSync().delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        val trie = peer1.getBestBlockTrie()
        val synchronizingPeerHaveAllData = peer1.containsExpectedDataUpToAccountAtBlock(1000, 500)
        // due to the fact that function generating state is deterministic both peer2 and peer3 ends up with exactly same
        // state, so peer1 can get whole trie from both of them.
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
        assert(peer1.bl.getBestBlockNumber() == peer3.bl.getBestBlockNumber() - peer3.testSyncConfig.pivotBlockOffset)
        assert(trie.isDefined)
        assert(synchronizingPeerHaveAllData)
      }
    }

  it should "should update pivot block" in customTestCaseResourceM(FakePeer.start2FakePeersRes()) {
    case (peer1, peer2) =>
      for {
        _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
        _ <- peer1.connectToPeers(Set(peer2.node))
        _ <- peer2.importBlocksUntil(2000)(IdentityUpdate).startAndForget
        _ <- peer1.startFastSync().delayExecution(50.milliseconds)
        _ <- peer1.waitForFastSyncFinish()
      } yield {
        assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
      }
  }

  it should "should update pivot block and sync this new pivot block state" in customTestCaseResourceM(
    FakePeer.start2FakePeersRes()
  ) { case (peer1, peer2) =>
    for {
      _ <- peer2.importBlocksUntil(1000)(IdentityUpdate)
      _ <- peer1.connectToPeers(Set(peer2.node))
      _ <- peer2.importBlocksUntil(2000)(updateStateAtBlock(1500)).startAndForget
      _ <- peer1.startFastSync().delayExecution(50.milliseconds)
      _ <- peer1.waitForFastSyncFinish()
    } yield {
      assert(peer1.bl.getBestBlockNumber() == peer2.bl.getBestBlockNumber() - peer2.testSyncConfig.pivotBlockOffset)
    }
  }

}

object FastSyncItSpec {
  def randomAddress(): InetSocketAddress = {
    val s = new ServerSocket(0)
    try {
      new InetSocketAddress("localhost", s.getLocalPort)
    } finally {
      s.close()
    }
  }

  final case class BlockchainState(bestBlock: Block, currentWorldState: InMemoryWorldStateProxy, currentTd: BigInt)

  val IdentityUpdate: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = (_, world) => world

  def updateWorldWithNAccounts(n: Int, world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val resultWorld = (0 until n).foldLeft(world) { (world, num) =>
      val randomBalance = num
      val randomAddress = Address(num)
      val codeBytes = BigInt(num).toByteArray
      val storage = world.getStorage(randomAddress)
      val changedStorage = (num until num + 20).foldLeft(storage)((storage, value) => storage.store(value, value))
      world
        .saveAccount(randomAddress, Account.empty().copy(balance = randomBalance))
        .saveCode(randomAddress, ByteString(codeBytes))
        .saveStorage(randomAddress, changedStorage)
    }
    InMemoryWorldStateProxy.persistState(resultWorld)
  }

  def updateStateAtBlock(blockWithUpdate: BigInt): (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = {
    (blockNr: BigInt, world: InMemoryWorldStateProxy) =>
      if (blockNr == blockWithUpdate) {
        updateWorldWithNAccounts(1000, world)
      } else {
        IdentityUpdate(blockNr, world)
      }
  }
}
