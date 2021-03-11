package io.iohk.ethereum.blockchain.sync.fast

import io.iohk.ethereum.blockchain.sync.fast.HeaderSkeleton._
import io.iohk.ethereum.domain.BlockHeader

/**
  * This class contains the state of the current skeleton being downloaded. This state is represented as the downloaded
  * skeleton headers plus the downloaded batches.
  * A skeleton of block headers consists of `limit` headers, separated by `gapSize` blocks in between.
  * A batch of blocks is a sequence of `gapSize + 1` block headers starting one block after the previous skeleton
  * header up to the next skeleton header inclusive.
  * When a batch of headers is downloaded, it is checked against the current skeleton and if it is correct, we save it
  * into the state.
  * When all batches filling the gaps are downloaded, this skeleton is considered full and the `fullChain` can be
  * requested.
  *
  * Example:
  * Given from = 0, to = 10, maxSkeletonHeaders = 3
  * Then:
  * - firstSkeletonHeaderNumber = 2
  * - gapSize = 2
  * - batchSize = 3
  * - skeletonHeaderNumbers = Seq(2, 5, 8)
  * - batchStartingHeaderNumbers = Seq(0, 3, 6)
  *
  *         batch                    gap                          batch
  *  /-------------------\      /-----------\              /------------------\
  *   0        1        2        3        4        5        6        7       8        9         10
  *   |                 |                          |                         |                  |
  * from          1stSkeletonHeader          2ndSkeletonHeader       lastSkeletonHeader         to
  *
  * @param from Lower bound for this skeleton, inclusive
  * @param to Upper bound for this skeleton, inclusive
  * @param maxSkeletonHeaders Maximum number of skeleton headers
  * @param skeletonHeaders The currently downloaded skeleton headers. May be empty if none were downloaded. This is set
  *                        by using `setSkeletonHeaders`
  * @param batches The currently downloaded batches. This is filled in by using `addBatch`
  */
final case class HeaderSkeleton(
    from: BigInt,
    to: BigInt,
    maxSkeletonHeaders: Int,
    private val skeletonHeaders: Seq[BlockHeader] = Seq.empty,
    private val batches: Map[BigInt, Seq[BlockHeader]] = Map.empty
) {

  private val remainingBlocks: BigInt = to - from + 1

  /**
    * Number of batched headers to request to a peer
    */
  val batchSize: BigInt = remainingBlocks.min(maxSkeletonHeaders)

  /**
    * Number of blocks in between each skeleton header
    */
  val gapSize: BigInt = batchSize - 1

  /**
    * Not to be confused with `from`. This is the number of the first header in the skeleton.
    */
  val firstSkeletonHeaderNumber: BigInt = from + gapSize

  /**
    * Maximum number of blocks to be downloaded at once. This is the total number of blocks that the skeleton contains.
    */
  val limit: BigInt = {
    val remainingSkeletonHeaders = remainingBlocks / batchSize + (remainingBlocks % batchSize).min(1)
    remainingSkeletonHeaders.min(maxSkeletonHeaders)
  }

  val lastSkeletonHeaderNumber: BigInt = from + (batchSize * limit) - 1
  private val skeletonHeaderNumbers: Seq[BigInt] =
    firstSkeletonHeaderNumber to lastSkeletonHeaderNumber by batchSize

  def validateAndSetSkeletonHeaders(headers: Seq[BlockHeader]): Either[HeaderSkeletonError, HeaderSkeleton] =
    for {
      _ <- checkSkeletonHeadersTotal(headers)
      _ <- checkSkeletonHeaderNumbers(headers)
    } yield copy(skeletonHeaders = headers)

  /**
    * Use this method to update this state with the downloaded skeleton
    * @param headers The downloaded skeleton
    * @return Either the updated structure if the validation succeeded or an error
    */
  def setSkeletonHeaders(headers: Seq[BlockHeader]): Either[HeaderSkeletonError, HeaderSkeleton] =
    for {
      _ <- checkSkeletonHeadersTotal(headers)
      _ <- checkSkeletonHeaderNumbers(headers)
    } yield copy(skeletonHeaders = headers)

  private def checkSkeletonHeadersTotal(headers: Seq[BlockHeader]): Either[HeaderSkeletonError, Unit] =
    Either.cond(headers.size == limit, (), InvalidTotalHeaders(headers.size, limit.toInt))

  private def checkSkeletonHeaderNumbers(headers: Seq[BlockHeader]): Either[HeaderSkeletonError, Unit] = {
    val downloadedHeaderNumbers = headers.map(_.number)
    val isValid = downloadedHeaderNumbers.zip(skeletonHeaderNumbers).forall {
      case (downloadedHeaderNumber, skeletonNumber) => downloadedHeaderNumber == skeletonNumber
    }
    Either.cond(isValid, (), InvalidHeaderNumber(downloadedHeaderNumbers, skeletonHeaderNumbers))
  }

  /**
    * An ordered sequence with the numbers of the first block of each batch
    */
  val batchStartingHeaderNumbers: Seq[BigInt] = from +: skeletonHeaderNumbers.dropRight(1).map(_ + 1)

  /**
    * Use this method to update this state with a downloaded batch of headers
    * @param batchHeaders The downloaded batch of headers
    * @return Either the updated structure if the validation succeeded or an error
    */
  def addBatch(batchHeaders: Seq[BlockHeader]): Either[HeaderSkeletonError, HeaderSkeleton] =
    for {
      skeletonHeader <- findSkeletonHeader(batchHeaders)
      _ <- checkSkeletonParentHash(batchHeaders, skeletonHeader)
      batchStartingNumber <- findBatchStartingNumber(batchHeaders)
    } yield copy(batches = batches + (batchStartingNumber -> batchHeaders))

  private def findSkeletonHeader(batchHeaders: Seq[BlockHeader]): Either[HeaderBatchError, BlockHeader] = {
    batchHeaders.lastOption match {
      case Some(header) =>
        for {
          byNumber <- findSkeletonHeaderByNumber(header)
          byHash <- Either.cond(byNumber.hash == header.hash, byNumber, InvalidBatchHash(header, byNumber))
        } yield byHash
      case None =>
        Left(EmptyDownloadedBatch(skeletonHeaderNumbers))
    }
  }

  private def findSkeletonHeaderByNumber(header: BlockHeader): Either[InvalidBatchLastNumber, BlockHeader] = {
    skeletonHeaders
      .find(_.number == header.number)
      .toRight(InvalidBatchLastNumber(header.number, skeletonHeaderNumbers))
  }

  private def checkSkeletonParentHash(
      batchHeaders: Seq[BlockHeader],
      skeletonHeader: BlockHeader
  ): Either[HeaderBatchError, Unit] = {
    batchHeaders.dropRight(1).lastOption match {
      case Some(penultimateBatchHeader) if penultimateBatchHeader.hash != skeletonHeader.parentHash =>
        Left(InvalidPenultimateHeader(penultimateBatchHeader, skeletonHeader))
      case _ =>
        Right(())
    }
  }

  private def findBatchStartingNumber(batchHeaders: Seq[BlockHeader]): Either[HeaderBatchError, BigInt] = {
    batchHeaders.headOption.map(_.number) match {
      case Some(firstBatchHeader) =>
        val found = batchStartingHeaderNumbers.find(_ == firstBatchHeader)
        found.toRight(InvalidBatchFirstNumber(firstBatchHeader, batchStartingHeaderNumbers))
      case None =>
        Left(EmptyDownloadedBatch(skeletonHeaderNumbers))
    }
  }

  private val isFull: Boolean = batchStartingHeaderNumbers.forall(batches.contains)

  /**
    * The complete skeleton plus the filled in batches, or `None` if not everything was downloaded
    */
  val fullChain: Option[Seq[BlockHeader]] =
    if (isFull) Some(batchStartingHeaderNumbers.flatMap(batches.apply))
    else None
}

object HeaderSkeleton {

  sealed trait HeaderSkeletonError {
    def msg: String
  }
  final case class NoCurrentHeaderSkeleton(downloaded: Int, expected: Int) extends HeaderSkeletonError {
    override def msg: String = s"Invalid downloaded total headers. Expected $expected but was $downloaded"
  }
  final case class InvalidTotalHeaders(downloaded: Int, expected: Int) extends HeaderSkeletonError {
    override def msg: String = s"Invalid downloaded total headers. Expected $expected but was $downloaded"
  }
  final case class InvalidHeaderNumber(downloaded: Seq[BigInt], expected: Seq[BigInt]) extends HeaderSkeletonError {
    override def msg: String =
      s"Invalid sequence of skeleton headers. Expected [${expected.mkString(",")}] but was [${downloaded.mkString(",")}]"
  }

  sealed trait HeaderBatchError extends HeaderSkeletonError
  final case class InvalidBatchLastNumber(downloaded: BigInt, expected: Seq[BigInt]) extends HeaderBatchError {
    override def msg: String = s"Invalid batch last number. $downloaded wasn't found in [${expected.mkString(",")}]"
  }
  final case class InvalidBatchHash(downloaded: BlockHeader, expected: BlockHeader) extends HeaderBatchError {
    override def msg: String = s"Invalid batch last block hash. Expected $expected but was $downloaded"
  }
  final case class EmptyDownloadedBatch(expected: Seq[BigInt]) extends HeaderBatchError {
    override def msg: String = s"Downloaded empty headers batch. Expected [${expected.mkString(",")}]"
  }
  final case class InvalidPenultimateHeader(penultimateBatchHeader: BlockHeader, skeletonHeader: BlockHeader)
      extends HeaderBatchError {
    override def msg: String =
      s"Invalid batch penultimate header. $penultimateBatchHeader isn't parent of $skeletonHeader"
  }
  final case class InvalidBatchFirstNumber(downloaded: BigInt, expected: Seq[BigInt]) extends HeaderBatchError {
    override def msg: String = s"Invalid batch first number. $downloaded wasn't found in [${expected.mkString(",")}]"
  }
  final case class InvalidDownloadedChain(downloaded: Seq[BlockHeader]) extends HeaderBatchError {
    override def msg: String = s"Invalid downloaded batch: ${downloaded.map(h => h.number -> h.hash).mkString(", ")}"
  }
}
