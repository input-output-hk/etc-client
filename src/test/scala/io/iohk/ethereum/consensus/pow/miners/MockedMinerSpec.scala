package io.iohk.ethereum.consensus.pow.miners

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit}
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MineBlocks
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses._
import io.iohk.ethereum.domain.{Block, SignedTransaction}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.utils.ByteStringUtils
import monix.eval.Task
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._

class MockedMinerSpec
    extends TestKit(ActorSystem("MockedPowMinerSpec_System"))
    with AnyWordSpecLike
    with Matchers
    with WithActorSystemShutDown {

  private implicit val timeout: Duration = 1.minute

  "MockedPowMiner" should {
    "not mine blocks" when {
      "there is no request" in new MockedMinerSetup {
        expectNoNewBlockMsg(noMessageTimeOut)
      }
    }

    "not mine block and return MinerNotSupport msg" when {
      "the request comes before miner started" in new MockedMinerSetup {
        val msg = MineBlocks(1, false, None)
        sendToMiner(msg)
        expectNoNewBlockMsg(noMessageTimeOut)
        parentActor.expectMsg(MinerNotSupported(msg))
      }
    }

    "stop mining in case of error" when {
      "Unable to get block for mining" in new MockedMinerSetup {
        val parent = origin
        val bfm1 = blockForMining(parent.header, Seq.empty)

        blockCreatorBehaviour(parent, false, bfm1)

        (blockCreator
          .getBlockForMining(_: Block, _: Boolean, _: Option[InMemoryWorldStateProxy]))
          .expects(bfm1, false, *)
          .returning(
            Task.raiseError(new RuntimeException("error"))
          )
          .atLeastOnce()

        withStartedMiner {
          sendToMiner(MineBlocks(2, false, None))

          parentActor.expectMsg(MiningOrdered)

          val block1 = waitForMinedBlock

          expectNoNewBlockMsg(noMessageTimeOut)

          parentActor.expectNoMessage(noMessageTimeOut)

          validateBlock(block1, parent)
        }
      }

      "Unable to get parent block for mining" in new MockedMinerSetup {
        val parentHash = origin.hash

        val errorMsg = s"Unable to get parent block with hash ${ByteStringUtils.hash2string(parentHash)} for mining"

        (blockchain.getBlockByHash _).expects(parentHash).returns(None)

        withStartedMiner {
          sendToMiner(MineBlocks(2, false, Some(parentHash)))

          expectNoNewBlockMsg(noMessageTimeOut)

          parentActor.expectMsg(MiningError(errorMsg))
        }
      }
    }

    "return MinerIsWorking to requester" when {
      "miner is working during next mine request" in new MockedMinerSetup {
        val parent = origin
        val bfm = blockForMining(parent.header, Seq.empty)

        blockCreatorBehaviour(parent, false, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, false, None))
          parentActor.expectMsg(MiningOrdered)
          sendToMiner(MineBlocks(1, false, None))
          parentActor.expectMsg(MinerIsWorking)

          val block = waitForMinedBlock

          expectNoNewBlockMsg(noMessageTimeOut)

          validateBlock(block, parent)
        }
      }
    }

    "mine valid blocks" when {
      "there is request for block with other parent than best block" in new MockedMinerSetup {
        val parent = origin
        val parentHash = origin.hash
        val bfm = blockForMining(parent.header, Seq.empty)

        (blockchain.getBlockByHash _).expects(parentHash).returns(Some(parent))

        blockCreatorBehaviour(parent, false, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, false, Some(parentHash)))

          parentActor.expectMsg(MiningOrdered)

          val block = waitForMinedBlock

          validateBlock(block, parent)
        }
      }

      "there is request for one block without transactions" in new MockedMinerSetup {
        val parent = origin
        val bfm = blockForMining(parent.header, Seq.empty)

        blockCreatorBehaviour(parent, false, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, false, None))

          val block = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block, parent)
        }
      }

      "there is request for one block with transactions" in new MockedMinerSetup {
        val parent = origin
        val bfm = blockForMining(parent.header)

        blockCreatorBehaviour(parent, true, bfm)

        withStartedMiner {
          sendToMiner(MineBlocks(1, true, None))

          val block = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block, parent, Seq(txToMine))
        }
      }

      "there is request for few blocks without transactions" in new MockedMinerSetup {
        val parent = origin
        val bfm1 = blockForMining(parent.header, Seq.empty)
        val bfm2 = blockForMining(bfm1.header, Seq.empty)

        blockCreatorBehaviour(parent, false, bfm1)

        blockCreatorBehaviourExpectingInitialWorld(bfm1, false, bfm2)

        withStartedMiner {
          sendToMiner(MineBlocks(2, false, None))

          val block1 = waitForMinedBlock
          val block2 = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block1, parent)
          validateBlock(block2, block1)
        }
      }

      "there is request for few blocks with transactions" in new MockedMinerSetup {
        val parent = origin
        val bfm1 = blockForMining(parent.header)
        val bfm2 = blockForMining(bfm1.header, Seq.empty)

        blockCreatorBehaviour(parent, true, bfm1)

        blockCreatorBehaviourExpectingInitialWorld(bfm1, true, bfm2)

        withStartedMiner {
          sendToMiner(MineBlocks(2, true, None))

          val block1 = waitForMinedBlock

          val block2 = waitForMinedBlock

          parentActor.expectMsg(MiningOrdered)

          validateBlock(block1, parent, Seq(txToMine))
          validateBlock(block2, block1)
        }
      }
    }
  }

  class MockedMinerSetup extends MinerSpecSetup {
    val noMessageTimeOut = 3.seconds

    val miner = TestActorRef(
      MockedMiner.props(
        blockchain,
        blockCreator,
        sync.ref
      )
    )

    (blockchain.getBestBlock _).expects().returns(Some(origin))

    def validateBlock(block: Block, parent: Block, txs: Seq[SignedTransaction] = Seq.empty): Assertion = {
      block.body.transactionList shouldBe txs
      block.header.nonce.length shouldBe 0
      block.header.parentHash shouldBe parent.hash
    }
  }
}
