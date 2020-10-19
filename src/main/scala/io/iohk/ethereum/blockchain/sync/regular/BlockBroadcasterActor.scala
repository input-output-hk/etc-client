package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, BlockBroadcast, PeerListSupport}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.utils.Config.SyncConfig

class BlockBroadcasterActor(
    broadcast: BlockBroadcast,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {
  import BlockBroadcasterActor._

  override def receive: Receive = handlePeerListMessages orElse handleBlacklistMessages orElse handleBroadcastMessages

  private def handleBroadcastMessages: Receive = {
    case BroadcastBlock(newBlock) => broadcast.broadcastBlock(newBlock, handshakedPeers)
    case BroadcastBlocks(blocks) => blocks.foreach(broadcast.broadcastBlock(_, handshakedPeers))
  }
}
object BlockBroadcasterActor {
  sealed trait BroadcasterMsg
  case class BroadcastBlock(block: NewBlock) extends BroadcasterMsg
  case class BroadcastBlocks(blocks: List[NewBlock]) extends BroadcasterMsg

  def props(
      broadcast: BlockBroadcast,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(
      new BlockBroadcasterActor(
        broadcast = broadcast,
        peerEventBus = peerEventBus,
        etcPeerManager = etcPeerManager,
        syncConfig = syncConfig,
        scheduler = scheduler
      )
    )
}
