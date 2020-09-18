package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import io.iohk.ethereum.network._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters

sealed trait ServerStatus
object ServerStatus {
  case object NotListening extends ServerStatus
  case class Listening(address: InetSocketAddress) extends ServerStatus
}

case class NodeStatus(
    key: AsymmetricCipherKeyPair,
    serverStatus: ServerStatus,
    discoveryStatus: ServerStatus) {

  val nodeId = key.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
}
