package io.iohk.ethereum.network.p2p

/** Helper class
  */
//FIXME: msg is redundant since `MessageSerializable` already exposes `underlyingMessage`
abstract class MessageSerializableImplicit[T <: Message](val msg: T) extends MessageSerializable {

  override def equals(that: Any): Boolean = that match {
    case that: MessageSerializableImplicit[T] => that.msg.equals(msg)
    case _                                    => false
  }

  override def hashCode(): Int = msg.hashCode()

  override def toShortString: String = msg.toShortString
}
