package io.iohk.ethereum.jsonrpc

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.jsonrpc.DebugService.{ ListPeersInfoRequest, ListPeersInfoResponse }
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.{ EtcPeerManagerActor, Peer, PeerActor, PeerManagerActor }
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ FlatSpec, Matchers }

class DebugServiceSpec extends FlatSpec with Matchers with MockFactory with ScalaFutures {

  "DebugService" should "return list of peers info" in new TestSetup {
    val result: ServiceResponse[ListPeersInfoResponse] =
      debugService.listPeersInfo(ListPeersInfoRequest())

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> PeerActor.Status.Connecting)))

    etcPeerManager.expectMsg(EtcPeerManagerActor.PeerInfoRequest(peer1.id))
    etcPeerManager.reply(EtcPeerManagerActor.PeerInfoResponse(Some(peer1Info)))

    result.futureValue shouldBe Right(ListPeersInfoResponse(List(peer1Info)))

  }

  it should "return empty list if there are no peers available" in new TestSetup {
    val result: ServiceResponse[ListPeersInfoResponse] =
      debugService.listPeersInfo(ListPeersInfoRequest())

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map.empty))

    result.futureValue shouldBe Right(ListPeersInfoResponse(List.empty))
  }

  it should "return empty list if there is no peer info" in new TestSetup {
    val result: ServiceResponse[ListPeersInfoResponse] =
      debugService.listPeersInfo(ListPeersInfoRequest())

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> PeerActor.Status.Connecting)))

    etcPeerManager.expectMsg(EtcPeerManagerActor.PeerInfoRequest(peer1.id))
    etcPeerManager.reply(EtcPeerManagerActor.PeerInfoResponse(None))

    result.futureValue shouldBe Right(ListPeersInfoResponse(List.empty))
  }

  trait TestSetup {
    implicit val system: ActorSystem = ActorSystem("debug-service-test")

    val peerManager = TestProbe()
    val etcPeerManager = TestProbe()
    val debugService = new DebugService(peerManager.ref, etcPeerManager.ref)

    val peerStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 1,
      totalDifficulty = BigInt("10000"),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      totalDifficulty = peerStatus.totalDifficulty,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number
    )
    val peer1Probe = TestProbe()
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 1), peer1Probe.ref, incomingConnection = false)
    val peer1Info: PeerInfo = initialPeerInfo.withForkAccepted(false)
  }
}
