package io.iohk.ethereum.domain

import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode}

import scala.collection.immutable

class BlockBody private(
                        private val transactionList: Array[SignedTransaction],
                        val uncleNodesList: Seq[BlockHeader]
                      ) extends Iterable[SignedTransaction] {


  lazy val numberOfTxs: Int = transactionList.length

  lazy val numberOfUncles: Int = uncleNodesList.size

  /**
    * Works as `copy` for case classes
    * @param transactions - new transactions for the block
    *        Beware using array-based collections as an argument, because:
    *  {{{
    *  val ar1 = Array(1, 2, 3)
    *  val seq = arr.toSeq
    *  val ar2 = seq.toArray
    *  ar2.update(0,0)
    *  ar1
    *  }}}
    *  would return (0, 2, 3), because ar1 === ar2
    * @return new BlockBody with same uncles but new `transactions`.
    */
  def withTransactions(transactions: Seq[SignedTransaction]): BlockBody = {
    new BlockBody(transactions.toArray, this.uncleNodesList)
  }

  /**
    * @param uncles - new uncles for the block
    * @return new BlockBody with same `transactions` but replaced uncles.
    */
  def withUncles(uncles: Seq[BlockHeader]): BlockBody = {
    BlockBody(this.transactionList, uncles)
  }

  /**
   * @return An inner array size.
   */
  override def size: Int = numberOfTxs


  override def iterator: Iterator[SignedTransaction] = {
    transactionList.iterator
  }

  /**
    * @return A WrappedArray instance, no memory copied
    */
  override def toIndexedSeq: immutable.IndexedSeq[SignedTransaction] = {
    transactionList.toIndexedSeq
  }

  /**
    * @return A WrappedArray instance, no memory copied
    */
  override def toSeq: Seq[SignedTransaction] = {
    transactionList.toSeq
  }

  /**
    * @param i - Index of transaction in this body.
    * Guaranteed to return `None` when negative or out of bounds.
    * @return Optional transaction at index i, zero-based
    */
  def getTransactionByIndex(i: Int): Option[SignedTransaction] = {
    transactionList.lift(i)
  }

  /**
    * @return an iterator that allows traversing backwards
    * constant-time access is guaranteed by `Array`
    */
  def reverseIterator: Iterator[SignedTransaction] = {
    transactionList.reverseIterator
  }

  /**
    * @param p - predicate on transaction
    * @return first transaction on which `p` is `true` along with its index
    */
  def findWhere(p: SignedTransaction => Boolean): Option[(Int, SignedTransaction)] = {
    transactionList.indexWhere(p) match {
      case -1 => None
      case i  => Some((i -> transactionList(i)))
    }
  }

  /**
    * @return an iterator of Tuple2[SignedTransaction, Int]
    *         Serves as a faster alternative to `List[_].zipWithIndex().iterator()`
    */
  def enumerate: Iterator[(SignedTransaction, Int)] = new Iterator[(SignedTransaction, Int)]() {
    private var i = 0;
    private val inner = BlockBody.this.iterator
    override def hasNext: Boolean = inner.hasNext
    override def next(): (SignedTransaction, Int) = {
      val result = inner.next -> i
      i = i + 1
      result
    }
  }


  override def toString: String =
    s"BlockBody{ transactionList: ${transactionList.toSeq}, uncleNodesList: $uncleNodesList }"


  override def canEqual(other: Any): Boolean = {
    other.isInstanceOf[BlockBody]
  }

  override def equals(other: Any): Boolean = other match {
    case that: BlockBody =>
      (that canEqual this) &&
        (transactionList sameElements that.transactionList) &&
        uncleNodesList == that.uncleNodesList
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(transactionList.toSeq, uncleNodesList)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

}

object BlockBody {

  val empty = BlockBody(Seq.empty, Seq.empty)

  def apply(transactionList: Seq[SignedTransaction], uncleNodesList: Seq[BlockHeader]): BlockBody = {
    new BlockBody(transactionList.toArray, uncleNodesList)
  }

  def blockBodyToRlpEncodable(
      blockBody: BlockBody,
      signedTxToRlpEncodable: SignedTransaction => RLPEncodeable,
      blockHeaderToRlpEncodable: BlockHeader => RLPEncodeable
  ): RLPEncodeable =
    RLPList(
      RLPList(blockBody.transactionList.map(signedTxToRlpEncodable): _*),
      RLPList(blockBody.uncleNodesList.map(blockHeaderToRlpEncodable): _*)
    )

  implicit class BlockBodyEnc(msg: BlockBody) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

      blockBodyToRlpEncodable(
        msg,
        stx => SignedTransactionEnc(stx).toRLPEncodable,
        header => BlockHeaderEnc(header).toRLPEncodable
      )
    }
  }

  implicit class BlockBlodyDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockBody: BlockBody = BlockBodyRLPEncodableDec(rawDecode(bytes)).toBlockBody
  }

  def rlpEncodableToBlockBody(
      rlpEncodeable: RLPEncodeable,
      rlpEncodableToSignedTransaction: RLPEncodeable => SignedTransaction,
      rlpEncodableToBlockHeader: RLPEncodeable => BlockHeader
  ): BlockBody =
    rlpEncodeable match {
      case RLPList((transactions: RLPList), (uncles: RLPList)) =>
        BlockBody(
          transactions.items.map(rlpEncodableToSignedTransaction),
          uncles.items.map(rlpEncodableToBlockHeader)
        )
      case _ => throw new RuntimeException("Cannot decode BlockBody")
    }

  implicit class BlockBodyRLPEncodableDec(val rlpEncodeable: RLPEncodeable) {
    def toBlockBody: BlockBody = {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

      rlpEncodableToBlockBody(
        rlpEncodeable,
        rlp => SignedTransactionRlpEncodableDec(rlp).toSignedTransaction,
        rlp => BlockHeaderDec(rlp).toBlockHeader
      )

    }
  }

}
