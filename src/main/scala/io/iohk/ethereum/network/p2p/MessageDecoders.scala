package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status._
import io.iohk.ethereum.network.p2p.messages.CommonMessages._
import io.iohk.ethereum.network.p2p.messages.PV61.BlockHashesFromNumber._
import io.iohk.ethereum.network.p2p.messages.PV62.NewBlockHashes._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBodies._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaders._
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockBodies._
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders._
import io.iohk.ethereum.network.p2p.messages.PV63.GetNodeData._
import io.iohk.ethereum.network.p2p.messages.PV63.GetReceipts._
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData._
import io.iohk.ethereum.network.p2p.messages.PV63.Receipts._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Ping._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.messages.{PV61 => pv61, PV62 => pv62, PV63 => pv63}
import io.iohk.ethereum.network.p2p.messages.Versions._

object NetworkMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte], protocolVersion: Version): Message =
    (protocolVersion, msgCode) match {
      case (_, Disconnect.code) => payload.toDisconnect
      case (_, Ping.code) => payload.toPing
      case (_, Pong.code) => payload.toPong
      case _ => throw new RuntimeException(s"Unknown message type: ${msgCode}")
    }

}

// scalastyle:off
object EthereumMessageDecoder extends MessageDecoder {

  override def fromBytes(msgCode: Int, payload: Array[Byte], protocolVersion: Version): Message =
    (protocolVersion, msgCode) match {
      //wire protocol
      case (_, Hello.code) => payload.toHello

      //FIXME: I still have an issue with protocolVersion, we are use PV62 and PV63 and code names for Status and NewBlock
      // suggest that there is PV64, but there isn't
      //common
      case (_, Status.code63 | Status.code64) => payload.toStatus(msgCode)
      case (_, SignedTransactions.code) => payload.toSignedTransactions
      case (_, NewBlock.code63 | NewBlock.code64) => payload.toNewBlock(msgCode)

      case (PV61, t) => handlePV61(t, payload)

      case (PV62 | PV63, pv62.NewBlockHashes.code) => payload.toNewBlockHashes
      case (PV62 | PV63, pv62.GetBlockHeaders.code) => payload.toGetBlockHeaders
      case (PV62 | PV63, pv62.BlockHeaders.code) => payload.toBlockHeaders
      case (PV62 | PV63, pv62.GetBlockBodies.code) => payload.toGetBlockBodies
      case (PV62 | PV63, pv62.BlockBodies.code) => payload.toBlockBodies

      case (PV63, t) => handlePV63(t, payload)

      case _ => throw new RuntimeException(s"Unknown message type: ${msgCode}")
    }

  private def handlePV61(msgCode: Int, payload: Array[Byte]): Message = {
    import io.iohk.ethereum.network.p2p.messages.PV61.NewBlockHashes._
    msgCode match {
      case pv61.NewBlockHashes.code => payload.toNewBlockHashes
      case pv61.BlockHashesFromNumber.code => payload.toBlockHashesFromNumber
      case _ => throw new RuntimeException(s"Unknown message type: ${msgCode}")
    }
  }

  private def handlePV63(msgCode: Int, payload: Array[Byte]): Message = msgCode match {
    case pv63.GetNodeData.code => payload.toGetNodeData
    case pv63.NodeData.code => payload.toNodeData
    case pv63.GetReceipts.code => payload.toGetReceipts
    case pv63.Receipts.code => payload.toReceipts
    case _ => throw new RuntimeException(s"Unknown message type: ${msgCode}")
  }
}
