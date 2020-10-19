package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPList, RLPSerializable, rawDecode, encode => rlpEncode}
import BlockHeaderImplicits._
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields._
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields

/**
  * @param extraFields contains the new fields added in ECIPs 1097 and 1098 and can contain values:
  *  - HefPreECIP1098: represents the ETC blocks without checkpointing nor treasury enabled
  *  - HefPostECIP1098: represents the ETC blocks with treasury enabled but not checkpointing
  *  - HefPostECIP1097: represents the ETC blocks with both checkpointing and treasury enabled
  */
case class BlockHeader(
    parentHash: ByteString,
    ommersHash: ByteString,
    beneficiary: ByteString,
    stateRoot: ByteString,
    transactionsRoot: ByteString,
    receiptsRoot: ByteString,
    logsBloom: ByteString,
    difficulty: BigInt,
    number: BigInt,
    gasLimit: BigInt,
    gasUsed: BigInt,
    unixTimestamp: Long,
    extraData: ByteString,
    mixHash: ByteString,
    nonce: ByteString,
    extraFields: HeaderExtraFields = HefEmpty
) {

  val treasuryOptOut: Option[Boolean] = extraFields match {
    case HefPostEcip1097(definedOptOut, _) => Some(definedOptOut)
    case HefPostEcip1098(definedOptOut) => Some(definedOptOut)
    case HefEmpty => None
  }

  val checkpoint: Option[Checkpoint] = extraFields match {
    case HefPostEcip1097(_, maybeCheckpoint) => maybeCheckpoint
    case _ => None
  }

  val hasCheckpoint: Boolean = checkpoint.isDefined

  override def toString: String = {
    val (treasuryOptOutString: String, checkpointString: String) = extraFields match {
      case HefPostEcip1097(definedOptOut, maybeCheckpoint) =>
        (definedOptOut.toString, maybeCheckpoint.isDefined.toString)

      case HefPostEcip1098(definedOptOut) =>
        (definedOptOut.toString, "Pre-ECIP1097 block")

      case HefEmpty =>
        ("Pre-ECIP1098 block", "Pre-ECIP1097 block")
    }

    s"""BlockHeader {
       |hash: $hashAsHexString
       |parentHash: ${ByteStringUtils.hash2string(parentHash)}
       |ommersHash: ${ByteStringUtils.hash2string(ommersHash)}
       |beneficiary: ${ByteStringUtils.hash2string(beneficiary)}
       |stateRoot: ${ByteStringUtils.hash2string(stateRoot)}
       |transactionsRoot: ${ByteStringUtils.hash2string(transactionsRoot)}
       |receiptsRoot: ${ByteStringUtils.hash2string(receiptsRoot)}
       |logsBloom: ${ByteStringUtils.hash2string(logsBloom)}
       |difficulty: $difficulty,
       |number: $number,
       |gasLimit: $gasLimit,
       |gasUsed: $gasUsed,
       |unixTimestamp: $unixTimestamp,
       |extraData: ${ByteStringUtils.hash2string(extraData)}
       |mixHash: ${ByteStringUtils.hash2string(mixHash)}
       |nonce: ${ByteStringUtils.hash2string(nonce)},
       |treasuryOptOut: $treasuryOptOutString
       |isCheckpointing: $checkpointString
       |}""".stripMargin
  }

  /**
    * calculates blockHash for given block header
    * @return - hash that can be used to get block bodies / receipts
    */
  lazy val hash: ByteString = ByteString(kec256(this.toBytes: Array[Byte]))

  lazy val hashAsHexString: String = ByteStringUtils.hash2string(hash)

  def idTag: String =
    s"$number: $hashAsHexString"
}

object BlockHeader {

  import io.iohk.ethereum.rlp.RLPImplicits._

  /** Empty MPT root hash. Data type is irrelevant */
  val EmptyMpt: ByteString = ByteString(crypto.kec256(rlp.encode(Array.emptyByteArray)))

  val EmptyBeneficiary: ByteString = Address(0).bytes

  val EmptyOmmers: ByteString = ByteString(crypto.kec256(rlp.encode(RLPList())))

  /**
    * Given a block header, returns it's rlp encoded bytes without nonce and mix hash
    *
    * @param blockHeader to be encoded without PoW fields
    * @return rlp.encode( [blockHeader.parentHash, ..., blockHeader.extraData] + extra fields )
    */
  def getEncodedWithoutNonce(blockHeader: BlockHeader): Array[Byte] = {
    // toRLPEncodeable is guaranteed to return a RLPList
    val rlpList: RLPList = blockHeader.toRLPEncodable.asInstanceOf[RLPList]

    val numberOfPowFields = 2
    val numberOfExtraFields = blockHeader.extraFields match {
      case HefPostEcip1097(_, _) => 2
      case HefPostEcip1098(_) => 1
      case HefEmpty => 0
    }

    val preECIP1098Fields = rlpList.items.dropRight(numberOfPowFields + numberOfExtraFields)
    val extraFieldsEncoded = rlpList.items.takeRight(numberOfExtraFields)

    val rlpItemsWithoutNonce = preECIP1098Fields ++ extraFieldsEncoded
    rlpEncode(RLPList(rlpItemsWithoutNonce: _*))
  }

  sealed trait HeaderExtraFields
  object HeaderExtraFields {
    case object HefEmpty extends HeaderExtraFields
    case class HefPostEcip1098(treasuryOptOut: Boolean) extends HeaderExtraFields
    case class HefPostEcip1097(treasuryOptOut: Boolean, checkpoint: Option[Checkpoint]) extends HeaderExtraFields
  }
}

object BlockHeaderImplicits {

  import io.iohk.ethereum.rlp.RLPImplicitConversions._
  import io.iohk.ethereum.rlp.RLPImplicits._

  implicit class BlockHeaderEnc(blockHeader: BlockHeader) extends RLPSerializable {
    // scalastyle:off method.length
    override def toRLPEncodable: RLPEncodeable = {
      import blockHeader._
      extraFields match {
        case HefPostEcip1097(definedOptOut, maybeCheckpoint) =>
          RLPList(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce,
            definedOptOut,
            maybeCheckpoint
          )

        case HefPostEcip1098(definedOptOut) =>
          RLPList(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce,
            definedOptOut
          )

        case HefEmpty =>
          RLPList(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce
          )
      }
    }
  }

  implicit class BlockHeaderByteArrayDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockHeader: BlockHeader = BlockHeaderDec(rawDecode(bytes)).toBlockHeader
  }

  implicit class BlockHeaderDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
    // scalastyle:off method.length
    def toBlockHeader: BlockHeader = {
      val checkpointOptionDecoder = implicitly[RLPDecoder[Option[Checkpoint]]]
      val treasuryOptOutDecoder = implicitly[RLPDecoder[Boolean]]

      rlpEncodeable match {
        case RLPList(
              parentHash,
              ommersHash,
              beneficiary,
              stateRoot,
              transactionsRoot,
              receiptsRoot,
              logsBloom,
              difficulty,
              number,
              gasLimit,
              gasUsed,
              unixTimestamp,
              extraData,
              mixHash,
              nonce,
              encodedOptOut,
              encodedCheckpoint
            ) =>
          val extraFields = HefPostEcip1097(
            treasuryOptOutDecoder.decode(encodedOptOut),
            checkpointOptionDecoder.decode(encodedCheckpoint)
          )
          BlockHeader(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce,
            extraFields
          )

        case RLPList(
              parentHash,
              ommersHash,
              beneficiary,
              stateRoot,
              transactionsRoot,
              receiptsRoot,
              logsBloom,
              difficulty,
              number,
              gasLimit,
              gasUsed,
              unixTimestamp,
              extraData,
              mixHash,
              nonce,
              encodedOptOut
            ) =>
          val extraFields = HefPostEcip1098(treasuryOptOutDecoder.decode(encodedOptOut))
          BlockHeader(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce,
            extraFields
          )

        case RLPList(
              parentHash,
              ommersHash,
              beneficiary,
              stateRoot,
              transactionsRoot,
              receiptsRoot,
              logsBloom,
              difficulty,
              number,
              gasLimit,
              gasUsed,
              unixTimestamp,
              extraData,
              mixHash,
              nonce
            ) =>
          BlockHeader(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce
          )

        case _ =>
          throw new Exception("BlockHeader cannot be decoded")
      }
    }
  }
}
