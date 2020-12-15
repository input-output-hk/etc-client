package io.iohk.ethereum.network

import akka.actor._
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.p2p.messages.Codes
import java.time.Clock
import scala.concurrent.duration.FiniteDuration

class PeerStatisticsActor(
    peerEventBus: ActorRef,
    var maybeStats: Option[TimeSlotStats[PeerId, PeerStat]]
) extends Actor {
  import PeerStatisticsActor._

  override def preStart(): Unit = {
    // Subscribe to messages received from handshaked peers to maintain stats.
    peerEventBus ! Subscribe(MessageSubscriptionClassifier)
    // Removing peers is an optimisation to free space, but eventually the stats would be overwritten anyway.
    peerEventBus ! Subscribe(SubscriptionClassifier.PeerDisconnectedClassifier(PeerSelector.AllPeers))
  }

  def receive: Receive = handlePeerEvents orElse handleStatsRequests

  private def handlePeerEvents: Receive = {
    case PeerEvent.MessageFromPeer(msg, peerId) =>
      val now = System.currentTimeMillis()
      val obs = PeerStat(
        responsesReceived = if (ResponseCodes(msg.code)) 1 else 0,
        requestsReceived = if (RequestCodes(msg.code)) 1 else 0,
        firstSeenTimeMillis = Some(now),
        lastSeenTimeMillis = Some(now)
      )
      maybeStats = maybeStats.map(_.add(peerId, obs))

    case PeerEvent.PeerDisconnected(peerId) =>
      maybeStats = maybeStats.map(_.remove(peerId))
  }

  private def handleStatsRequests: Receive = {
    case GetStatsForAll(window) =>
      val stats = maybeStats.map(_.getAll(Some(window))).getOrElse(Map.empty)
      sender ! StatsForAll(stats)

    case GetStatsForPeer(window, peerId) =>
      val stats = maybeStats.map(_.get(peerId, Some(window))).getOrElse(PeerStat.empty)
      sender ! StatsForPeer(peerId, stats)
  }
}

object PeerStatisticsActor {
  def props(peerEventBus: ActorRef, slotDuration: FiniteDuration, slotCount: Int): Props =
    Props {
      implicit val clock = Clock.systemUTC()
      new PeerStatisticsActor(peerEventBus, TimeSlotStats[PeerId, PeerStat](slotDuration, slotCount))
    }

  case class GetStatsForAll(window: FiniteDuration)
  case class StatsForAll(stats: Map[PeerId, PeerStat])
  case class GetStatsForPeer(window: FiniteDuration, peerId: PeerId)
  case class StatsForPeer(peerId: PeerId, stat: PeerStat)

  val ResponseCodes = Set(
    Codes.NewBlockCode,
    Codes.NewBlockHashesCode,
    Codes.SignedTransactionsCode,
    Codes.BlockHeadersCode,
    Codes.BlockBodiesCode,
    Codes.BlockHashesFromNumberCode,
    Codes.NodeDataCode,
    Codes.ReceiptsCode
  )

  val RequestCodes = Set(
    Codes.GetBlockHeadersCode,
    Codes.GetBlockBodiesCode,
    Codes.GetNodeDataCode,
    Codes.GetReceiptsCode
  )

  val MessageSubscriptionClassifier =
    SubscriptionClassifier.MessageClassifier(
      messageCodes = RequestCodes union ResponseCodes,
      peerSelector = PeerSelector.AllPeers
    )
}
