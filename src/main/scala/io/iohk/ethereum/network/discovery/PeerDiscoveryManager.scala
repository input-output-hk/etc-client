package io.iohk.ethereum.network.discovery

import java.net.{InetSocketAddress, URI}
import java.time.Clock

import io.iohk.ethereum.network._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}

import scala.concurrent.ExecutionContext.Implicits.global

class PeerDiscoveryManager(
    discoveryListener: ActorRef,
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    nodeStatusHolder: Agent[NodeStatus],
    clock: Clock) extends Actor with ActorLogging {

  import PeerDiscoveryManager._

  val expirationTimeSec = discoveryConfig.messageExpiration.toSeconds

  var nodesInfo: Map[ByteString, DiscoveryNodeInfo] = {
    val bootStrapNodesInfo = discoveryConfig.bootstrapNodes.map(DiscoveryNodeInfo.fromNode)
    val knownNodesURIs =
      if (discoveryConfig.discoveryEnabled) knownNodesStorage.getKnownNodes()
      else Set.empty
    val nodesInfo = bootStrapNodesInfo ++ knownNodesURIs.map(DiscoveryNodeInfo.fromUri)

    nodesInfo.map { nodeInfo => nodeInfo.node.id -> nodeInfo }.toMap
  }

  if (discoveryConfig.discoveryEnabled) {
    discoveryListener ! DiscoveryListener.Subscribe
    context.system.scheduler.schedule(discoveryConfig.scanInitialDelay, discoveryConfig.scanInterval, self, Scan)
  }

  def scan(): Unit = {
    nodesInfo.values.toSeq
      .sortBy(_.addTimestamp) // take 10 most recent nodes
      .takeRight(discoveryConfig.scanMaxNodes)
      .foreach { nodeInfo =>
        sendPing(Endpoint.makeEndpoint(nodeInfo.node.addr, nodeInfo.node.addr.getPort), nodeInfo.node.addr)
      }
  }

  override def receive: Receive = {
    case DiscoveryListener.MessageReceived(ping: Ping, from, packet) =>
      val to = Endpoint.makeEndpoint(from, ping.from.tcpPort)
      sendMessage(Pong(to, packet.mdc, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(pong: Pong, from, packet) =>
      val newNodeInfo = DiscoveryNodeInfo.fromNode(Node(packet.nodeId, from))

      if (nodesInfo.size < discoveryConfig.nodesLimit) {
        nodesInfo += newNodeInfo.node.id -> newNodeInfo
        sendMessage(FindNode(ByteString(nodeStatusHolder().nodeId), expirationTimestamp), from)
      } else {
        val (earliestNode, _) = nodesInfo.minBy { case (_, node) => node.addTimestamp }
        nodesInfo -= earliestNode
        nodesInfo += newNodeInfo.node.id -> newNodeInfo
      }

    case DiscoveryListener.MessageReceived(findNode: FindNode, from, packet) =>
      sendMessage(Neighbours(Nil, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(neighbours: Neighbours, from, packet) =>
      val toPing = neighbours.nodes
        .filterNot(n => nodesInfo.contains(n.nodeId)) // not already on the list
        .take(discoveryConfig.nodesLimit - nodesInfo.size)

      toPing.foreach { n =>
        Endpoint.toUdpAddress(n.endpoint).foreach(address =>
          sendPing(n.endpoint, address)
        )
      }

    case GetDiscoveredNodesInfo =>
      sender() ! DiscoveredNodesInfo(nodesInfo.values.toSet)

    case Scan => scan()
  }

  private def sendPing(toEndpoint: Endpoint, toAddr: InetSocketAddress): Unit = {
    val tcpPort = nodeStatusHolder().serverStatus match {
      case ServerStatus.Listening(addr) => addr.getPort
      case _ => 0
    }
    nodeStatusHolder().discoveryStatus match {
      case ServerStatus.Listening(address) =>
        val from = Endpoint.makeEndpoint(address, tcpPort)
        sendMessage(Ping(ProtocolVersion, from, toEndpoint, expirationTimestamp), toAddr)
      case _ =>
        log.warning("UDP server not running. Not sending ping message.")
    }
  }

  private def sendMessage[M <: Message](message: M, to: InetSocketAddress)(implicit rlpEnc: RLPEncoder[M]): Unit = {
    nodeStatusHolder().discoveryStatus match {
      case ServerStatus.Listening(_) =>
        discoveryListener ! DiscoveryListener.SendMessage(message, to)
      case _ =>
        log.warning(s"UDP server not running. Not sending message $message.")
    }
  }

  private def expirationTimestamp = clock.instant().plusSeconds(expirationTimeSec).getEpochSecond
}

object PeerDiscoveryManager {
  def props(discoveryListener: ActorRef,
            discoveryConfig: DiscoveryConfig,
            knownNodesStorage: KnownNodesStorage,
            nodeStatusHolder: Agent[NodeStatus],
            clock: Clock): Props =
    Props(new PeerDiscoveryManager(discoveryListener, discoveryConfig, knownNodesStorage, nodeStatusHolder, clock))

  object DiscoveryNodeInfo {

    def fromUri(uri: URI): DiscoveryNodeInfo = fromNode(Node.fromUri(uri))

    def fromNode(node: Node): DiscoveryNodeInfo = DiscoveryNodeInfo(node, System.currentTimeMillis())

  }

  case class DiscoveryNodeInfo(node: Node, addTimestamp: Long)

  case object GetDiscoveredNodesInfo
  case class DiscoveredNodesInfo(nodes: Set[DiscoveryNodeInfo])

  private case object Scan
}
