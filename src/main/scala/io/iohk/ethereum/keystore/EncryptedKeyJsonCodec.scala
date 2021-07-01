package io.iohk.ethereum.keystore

import java.util.UUID

import akka.util.ByteString

import scala.util.Try

import org.bouncycastle.util.encoders.Hex
import org.json4s.CustomSerializer
import org.json4s.DefaultFormats
import org.json4s.Extraction
import org.json4s.Formats
import org.json4s.JField
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.keystore.EncryptedKey._

object EncryptedKeyJsonCodec {

  private val byteStringSerializer = new CustomSerializer[ByteString](_ =>
    (
      { case JString(s) => ByteString(Hex.decode(s)) },
      { case bs: ByteString => JString(Hex.toHexString(bs.toArray)) }
    )
  )

  implicit private val formats: Formats = DefaultFormats + byteStringSerializer

  private def asHex(bs: ByteString): String =
    Hex.toHexString(bs.toArray)

  def toJson(encKey: EncryptedKey): String = {
    import encKey._
    import cryptoSpec._

    val json =
      ("id" -> id.toString) ~
        ("address" -> asHex(address.bytes)) ~
        ("version" -> version) ~
        ("crypto" -> (
          ("cipher" -> cipher) ~
            ("ciphertext" -> asHex(ciphertext)) ~
            ("cipherparams" -> ("iv" -> asHex(iv))) ~
            encodeKdf(kdfParams) ~
            ("mac" -> asHex(mac))
        ))

    pretty(render(json))
  }

  def fromJson(jsonStr: String): Either[String, EncryptedKey] = Try {
    val json = parse(jsonStr).transformField { case JField(k, v) => JField(k.toLowerCase, v) }

    val uuid = UUID.fromString((json \ "id").extract[String])
    val address = Address((json \ "address").extract[String])
    val version = (json \ "version").extract[Int]

    val crypto = json \ "crypto"
    val cipher = (crypto \ "cipher").extract[String]
    val ciphertext = (crypto \ "ciphertext").extract[ByteString]
    val iv = (crypto \ "cipherparams" \ "iv").extract[ByteString]
    val mac = (crypto \ "mac").extract[ByteString]

    val kdfParams = extractKdf(crypto)
    val cryptoSpec = CryptoSpec(cipher, ciphertext, iv, kdfParams, mac)
    EncryptedKey(uuid, address, cryptoSpec, version)

  }.fold(ex => Left(ex.toString), encKey => Right(encKey))

  private def encodeKdf(kdfParams: KdfParams): JObject =
    kdfParams match {
      case ScryptParams(_, _, _, _, _) =>
        ("kdf" -> Scrypt) ~
          ("kdfparams" -> Extraction.decompose(kdfParams))

      case Pbkdf2Params(_, _, _, _) =>
        ("kdf" -> Pbkdf2) ~
          ("kdfparams" -> Extraction.decompose(kdfParams))
    }

  private def extractKdf(crypto: JValue): KdfParams = {
    val kdf = (crypto \ "kdf").extract[String]
    kdf.toLowerCase match {
      case Scrypt =>
        (crypto \ "kdfparams").extract[ScryptParams]

      case Pbkdf2 =>
        (crypto \ "kdfparams").extract[Pbkdf2Params]
    }
  }

}
