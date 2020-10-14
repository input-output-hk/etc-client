package io.iohk.ethereum.blockchain.sync

import java.time.Instant

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsValidator.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.SyncBlocksValidator.BlockBodyValidationResult
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{StartSyncingTo, StateSyncFinished}
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.db.storage.{AppStateStorage, FastSyncStateStorage}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.Random

// scalastyle:off file.size.limit
class FastSync(
    val fastSyncStateStorage: FastSyncStateStorage,
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val validators: Validators,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport
    with FastSyncReceiptsValidator
    with SyncBlocksValidator {

  import FastSync._
  import syncConfig._

  val syncController: ActorRef = context.parent

  override def receive: Receive = idle

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  def idle: Receive = handleCommonMessages orElse { case Start =>
    start()
  }

  def start(): Unit = {
    log.info("Trying to start block synchronization (fast mode)")
    fastSyncStateStorage.getSyncState() match {
      case Some(syncState) => startWithState(syncState)
      case None => startFromScratch()
    }
  }

  def startWithState(syncState: SyncState): Unit = {
    if (syncState.updatingPivotBlock) {
      log.info(s"FastSync interrupted during pivot block update, choosing new pivot block")
      val syncingHandler = new SyncingHandler(syncState)
      val pivotBlockSelector = context.actorOf(
        PivotBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler, context.self),
        "pivot-block-selector"
      )
      pivotBlockSelector ! PivotBlockSelector.SelectPivotBlock
      context become syncingHandler.waitingForPivotBlockUpdate(ImportedLastBlock)
    } else {
      log.info(
        s"Starting block synchronization (fast mode), pivot block ${syncState.pivotBlock.number}, " +
          s"block to download to ${syncState.safeDownloadTarget}"
      )
      val syncingHandler = new SyncingHandler(syncState)
      context become syncingHandler.receive
      if (syncState.isBlockchainWorkFinished && !syncState.stateSyncFinished) {
        log.info(s"Blockchain sync was completed, starting state sync to block ${syncState.pivotBlock.idTag}")
        // chain has already been downloaded we can start state sync
        syncingHandler.startStateSync(syncState.pivotBlock)
      }
      syncingHandler.processSyncing()
    }
  }

  def startFromScratch(): Unit = {
    val pivotBlockSelector = context.actorOf(
      PivotBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler, context.self),
      "pivot-block-selector"
    )
    pivotBlockSelector ! PivotBlockSelector.SelectPivotBlock
    context become waitingForPivotBlock
  }

  def waitingForPivotBlock: Receive = handleCommonMessages orElse { case PivotBlockSelector.Result(pivotBlockHeader) =>
    if (pivotBlockHeader.number < 1) {
      log.info("Unable to start block synchronization in fast mode: pivot block is less than 1")
      appStateStorage.fastSyncDone().commit()
      context become idle
      syncController ! Done
    } else {
      val initialSyncState =
        SyncState(
          pivotBlockHeader,
          safeDownloadTarget = pivotBlockHeader.number + syncConfig.fastSyncBlockValidationX
        )
      startWithState(initialSyncState)
    }
  }

  // scalastyle:off number.of.methods
  private class SyncingHandler(initialSyncState: SyncState) {

    private val BlockHeadersHandlerName = "block-headers-request-handler"

    private var requestedHeaders: Map[Peer, BigInt] = Map.empty

    private var syncState = initialSyncState

    private var assignedHandlers: Map[ActorRef, Peer] = Map.empty
    private var peerRequestsTime: Map[Peer, Instant] = Map.empty

    private var requestedBlockBodies: Map[ActorRef, Seq[ByteString]] = Map.empty
    private var requestedReceipts: Map[ActorRef, Seq[ByteString]] = Map.empty

    private val syncStateStorageActor = context.actorOf(Props[FastSyncStateStorageActor], "state-storage")

    syncStateStorageActor ! fastSyncStateStorage

    private val syncStateDownloader = context.actorOf(
      SyncStateDownloaderActor.props(etcPeerManager, peerEventBus, syncConfig, scheduler),
      "state-downloader"
    )
    private val syncStateScheduler = context.actorOf(
      SyncStateSchedulerActor.props(syncStateDownloader, SyncStateScheduler(blockchain)),
      "state-scheduler"
    )

    //Delay before starting to persist snapshot. It should be 0, as the presence of it marks that fast sync was started
    private val persistStateSnapshotDelay: FiniteDuration = 0.seconds
    private val syncStatePersistCancellable =
      scheduler.scheduleWithFixedDelay(persistStateSnapshotDelay, persistStateSnapshotInterval, self, PersistSyncState)
    private val printStatusCancellable =
      scheduler.scheduleWithFixedDelay(printStatusInterval, printStatusInterval, self, PrintStatus)
    private val heartBeat =
      scheduler.scheduleWithFixedDelay(syncRetryInterval, syncRetryInterval * 2, self, ProcessSyncing)

    def startStateSync(targetBlockHeader: BlockHeader): Unit = {
      syncStateScheduler ! StartSyncingTo(targetBlockHeader.stateRoot, targetBlockHeader.number)
    }

    def receive: Receive = handleCommonMessages orElse {
      case UpdatePivotBlock(state) => updatePivotBlock(state)
      case ProcessSyncing => processSyncing()
      case PrintStatus => printStatus()
      case PersistSyncState => persistSyncState()
      case StateSyncFinished =>
        syncState = syncState.copy(stateSyncFinished = true)
        processSyncing()

      case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
        log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
        requestedHeaders.get(peer).foreach { requestedNum =>
          removeRequestHandler(sender())
          requestedHeaders -= peer
          if (
            blockHeaders.nonEmpty && blockHeaders.size <= requestedNum && blockHeaders.head.number == syncState.bestBlockHeaderNumber + 1
          )
            handleBlockHeaders(peer, blockHeaders)
          else
            blacklist(peer.id, blacklistDuration, "wrong blockheaders response (empty or not chain forming)")
        }

      case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
        log.info("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
        val requestedBodies = requestedBlockBodies.getOrElse(sender(), Nil)
        requestedBlockBodies -= sender()
        removeRequestHandler(sender())
        handleBlockBodies(peer, requestedBodies, blockBodies)

      case ResponseReceived(peer, Receipts(receipts), timeTaken) =>
        log.info("Received {} receipts in {} ms", receipts.size, timeTaken)
        val requestedHashes = requestedReceipts.getOrElse(sender(), Nil)
        requestedReceipts -= sender()
        removeRequestHandler(sender())
        handleReceipts(peer, requestedHashes, receipts)

      case PeerRequestHandler.RequestFailed(peer, reason) =>
        handleRequestFailure(peer, sender(), reason)

      case Terminated(ref) if assignedHandlers.contains(ref) =>
        handleRequestFailure(assignedHandlers(ref), ref, "Unexpected error")
    }

    def waitingForPivotBlockUpdate(processState: FinalBlockProcessingResult): Receive = handleCommonMessages orElse {
      case PivotBlockSelector.Result(pivotBlockHeader) =>
        log.info(s"New pivot block with number ${pivotBlockHeader.number} received")
        if (pivotBlockHeader.number >= syncState.pivotBlock.number) {
          updatePivotSyncState(processState, pivotBlockHeader)
          syncState = syncState.copy(updatingPivotBlock = false)
          context become this.receive
          processSyncing()
        } else {
          syncState = syncState.copy(pivotBlockUpdateFailures = syncState.pivotBlockUpdateFailures + 1)
          scheduler.scheduleOnce(syncRetryInterval, self, UpdatePivotBlock(processState))
        }

      case PersistSyncState => persistSyncState()

      case UpdatePivotBlock(state) => updatePivotBlock(state)
    }

    private def updatePivotBlock(state: FinalBlockProcessingResult): Unit = {
      if (syncState.pivotBlockUpdateFailures <= syncConfig.maximumTargetUpdateFailures) {
        if (assignedHandlers.nonEmpty || syncState.blockChainWorkQueued) {
          log.info(s"Still waiting for some responses, rescheduling pivot block update")
          scheduler.scheduleOnce(1.second, self, UpdatePivotBlock(state))
          processSyncing()
        } else {
          syncState = syncState.copy(updatingPivotBlock = true)
          log.info("Asking for new pivot block")
          val pivotBlockSelector = {
            context.actorOf(
              PivotBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler, context.self)
            )
          }
          pivotBlockSelector ! PivotBlockSelector.SelectPivotBlock
          context become waitingForPivotBlockUpdate(state)
        }
      } else {
        log.warning(s"Sync failure! Number of pivot block update failures reached maximum.")
        sys.exit(1)
      }
    }

    private def updatePivotSyncState(state: FinalBlockProcessingResult, pivotBlockHeader: BlockHeader): Unit =
      state match {
        case ImportedLastBlock =>
          if (pivotBlockHeader.number - syncState.pivotBlock.number <= syncConfig.maxTargetDifference) {
            log.info(s"Current pivot block is fresh enough, starting state download")
            // Empty root has means that there were no transactions in blockchain, and Mpt trie is empty
            // Asking for this root would result only with empty transactions
            if (syncState.pivotBlock.stateRoot == ByteString(MerklePatriciaTrie.EmptyRootHash)) {
              syncState = syncState.copy(stateSyncFinished = true)
            } else {
              syncStateScheduler ! StartSyncingTo(pivotBlockHeader.stateRoot, pivotBlockHeader.number)
            }
          } else {
            syncState = syncState.updatePivotBlock(
              pivotBlockHeader,
              syncConfig.fastSyncBlockValidationX,
              updateFailures = false
            )
            log.info(
              s"Changing pivot block to ${pivotBlockHeader.number}, new safe target is ${syncState.safeDownloadTarget}"
            )
          }

        case LastBlockValidationFailed =>
          log.info(
            s"Changing pivot block after failure, to ${pivotBlockHeader.number}, new safe target is ${syncState.safeDownloadTarget}"
          )
          syncState =
            syncState.updatePivotBlock(pivotBlockHeader, syncConfig.fastSyncBlockValidationX, updateFailures = true)
      }

    private def removeRequestHandler(handler: ActorRef): Unit = {
      context unwatch handler
      assignedHandlers -= handler
    }

    private def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): Unit = {
      (startBlock to ((startBlock - blocksToDiscard) max 1) by -1).foreach { n =>
        blockchain.getBlockHeaderByNumber(n).foreach { headerToRemove =>
          blockchain.removeBlock(headerToRemove.hash, withState = false)
        }
      }
      // TODO (maybe ETCM-77): Manage last checkpoint number too
      appStateStorage.putBestBlockNumber((startBlock - blocksToDiscard - 1) max 0).commit()
    }

    @tailrec
    private def processHeaders(peer: Peer, headers: Seq[BlockHeader]): HeaderProcessingResult = {
      if (headers.nonEmpty) {
        val header = headers.head
        processHeader(header, peer) match {
          case Left(result) => result
          case Right(headerAndDif) =>
            updateSyncState(headerAndDif._1, headerAndDif._2)
            if (header.number == syncState.safeDownloadTarget) {
              ImportedPivotBlock
            } else {
              processHeaders(peer, headers.tail)
            }
        }
      } else
        HeadersProcessingFinished
    }

    private def validateHeader(header: BlockHeader, peer: Peer): Either[HeaderProcessingResult, BlockHeader] = {
      val shouldValidate = header.number >= syncState.nextBlockToFullyValidate

      if (shouldValidate) {
        validators.blockHeaderValidator.validate(header, blockchain.getBlockHeaderByHash) match {
          case Right(_) =>
            updateValidationState(header)
            Right(header)

          case Left(error) =>
            log.warning(s"Block header validation failed during fast sync at block ${header.number}: $error")
            Left(ValidationFailed(header, peer))
        }
      } else {
        Right(header)
      }
    }

    private def updateSyncState(header: BlockHeader, parentTd: BigInt): Unit = {
      blockchain
        .storeBlockHeader(header)
        .and(blockchain.storeTotalDifficulty(header.hash, parentTd + header.difficulty))
        .commit()

      if (header.number > syncState.bestBlockHeaderNumber) {
        syncState = syncState.copy(bestBlockHeaderNumber = header.number)
      }

      syncState = syncState
        .enqueueBlockBodies(Seq(header.hash))
        .enqueueReceipts(Seq(header.hash))
    }

    private def updateValidationState(header: BlockHeader): Unit = {
      import syncConfig.{fastSyncBlockValidationK => K, fastSyncBlockValidationX => X}
      syncState = syncState.updateNextBlockToValidate(header, K, X)
    }

    private def processHeader(header: BlockHeader, peer: Peer): Either[HeaderProcessingResult, (BlockHeader, BigInt)] =
      for {
        validatedHeader <- validateHeader(header, peer)
        parentDifficulty <- getParentDifficulty(header)
      } yield (validatedHeader, parentDifficulty)

    private def getParentDifficulty(header: BlockHeader) = {
      blockchain.getTotalDifficultyByHash(header.parentHash).toRight(ParentDifficultyNotFound(header))
    }

    private def handleRewind(header: BlockHeader, peer: Peer, N: Int): Unit = {
      blacklist(peer.id, blacklistDuration, "block header validation failed")
      if (header.number <= syncState.safeDownloadTarget) {
        discardLastBlocks(header.number, N)
        syncState = syncState.updateDiscardedBlocks(header, N)
        if (header.number >= syncState.pivotBlock.number) {
          updatePivotBlock(LastBlockValidationFailed)
        } else {
          processSyncing()
        }
      } else {
        processSyncing()
      }
    }

    private def handleBlockHeaders(peer: Peer, headers: Seq[BlockHeader]) = {
      if (checkHeadersChain(headers)) {
        processHeaders(peer, headers) match {
          case ParentDifficultyNotFound(header) =>
            // We could end in wrong fork and get blocked so we should rewind our state a little
            // we blacklist peer just in case we got malicious peer which would send us bad blocks, forcing us to rollback
            // to genesis
            log.warning("Parent difficulty not found for block {}, not processing rest of headers", header.idTag)
            handleRewind(header, peer, syncConfig.fastSyncBlockValidationN)
          case HeadersProcessingFinished =>
            processSyncing()
          case ImportedPivotBlock =>
            updatePivotBlock(ImportedLastBlock)
          case ValidationFailed(header, peerToBlackList) =>
            log.warning(s"validation fo header ${header.idTag} failed")
            handleRewind(header, peerToBlackList, syncConfig.fastSyncBlockValidationN)
        }
      } else {
        blacklist(peer.id, blacklistDuration, "error in block headers response")
        processSyncing()
      }
    }

    private def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]) = {
      if (blockBodies.isEmpty) {
        val reason =
          s"got empty block bodies response for known hashes: ${requestedHashes.map(ByteStringUtils.hash2string)}"
        blacklist(peer.id, blacklistDuration, reason)
        syncState = syncState.enqueueBlockBodies(requestedHashes)
      } else {
        validateBlocks(requestedHashes, blockBodies) match {
          case BlockBodyValidationResult.Valid =>
            insertBlocks(requestedHashes, blockBodies)
          case BlockBodyValidationResult.Invalid =>
            blacklist(
              peer.id,
              blacklistDuration,
              s"responded with block bodies not matching block headers, blacklisting for $blacklistDuration"
            )
            syncState = syncState.enqueueBlockBodies(requestedHashes)
          case BlockBodyValidationResult.DbError =>
            redownloadBlockchain()
        }
      }

      processSyncing()
    }

    private def handleReceipts(peer: Peer, requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]]) = {
      if (receipts.isEmpty) {
        val reason = s"got empty receipts for known hashes: ${requestedHashes.map(ByteStringUtils.hash2string)}"
        blacklist(peer.id, blacklistDuration, reason)
        syncState = syncState.enqueueReceipts(requestedHashes)
      } else {
        validateReceipts(requestedHashes, receipts) match {
          case ReceiptsValidationResult.Valid(blockHashesWithReceipts) =>
            blockHashesWithReceipts
              .map { case (hash, receiptsForBlock) =>
                blockchain.storeReceipts(hash, receiptsForBlock)
              }
              .reduce(_.and(_))
              .commit()

            val receivedHashes = blockHashesWithReceipts.unzip._1
            updateBestBlockIfNeeded(receivedHashes)

            val remainingReceipts = requestedHashes.drop(receipts.size)
            if (remainingReceipts.nonEmpty) {
              syncState = syncState.enqueueReceipts(remainingReceipts)
            }

          case ReceiptsValidationResult.Invalid(error) =>
            val reason =
              s"got invalid receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}" +
                s" due to: $error"
            blacklist(peer.id, blacklistDuration, reason)
            syncState = syncState.enqueueReceipts(requestedHashes)

          case ReceiptsValidationResult.DbError =>
            redownloadBlockchain()
        }
      }

      processSyncing()
    }

    private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String) = {
      removeRequestHandler(handler)

      syncState = syncState
        .enqueueBlockBodies(requestedBlockBodies.getOrElse(handler, Nil))
        .enqueueReceipts(requestedReceipts.getOrElse(handler, Nil))

      requestedBlockBodies = requestedBlockBodies - handler
      requestedReceipts = requestedReceipts - handler

      requestedHeaders -= peer
      if (handshakedPeers.contains(peer)) {
        blacklist(peer.id, blacklistDuration, reason)
      }
    }

    /**
      * Restarts download from a few blocks behind the current best block header, as an unexpected DB error happened
      */
    private def redownloadBlockchain(): Unit = {
      syncState = syncState.copy(
        blockBodiesQueue = Seq.empty,
        receiptsQueue = Seq.empty,
        //todo adjust the formula to minimize redownloaded block headers
        bestBlockHeaderNumber = (syncState.bestBlockHeaderNumber - 2 * blockHeadersPerRequest).max(0)
      )
      log.debug("missing block header for known hash")
    }

    private def persistSyncState(): Unit = {
      syncStateStorageActor ! syncState.copy(
        blockBodiesQueue = requestedBlockBodies.values.flatten.toSeq.distinct ++ syncState.blockBodiesQueue,
        receiptsQueue = requestedReceipts.values.flatten.toSeq.distinct ++ syncState.receiptsQueue
      )
    }

    private def printStatus() = {
      val formatPeer: (Peer) => String = peer =>
        s"${peer.remoteAddress.getAddress.getHostAddress}:${peer.remoteAddress.getPort}"
      log.info(s"""|Block: ${appStateStorage.getBestBlockNumber()}/${syncState.pivotBlock.number}.
            |Peers waiting_for_response/connected: ${assignedHandlers.size}/${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).
            |State: ${syncState.downloadedNodesCount}/${syncState.totalNodesCount} nodes.
            |""".stripMargin.replace("\n", " "))
      log.debug(
        s"""|Connection status: connected(${assignedHandlers.values.map(formatPeer).toSeq.sorted.mkString(", ")})/
            |handshaked(${handshakedPeers.keys.map(formatPeer).toSeq.sorted.mkString(", ")})
            | blacklisted(${blacklistedPeers.map { case (id, _) => id.value }.mkString(", ")})
            |""".stripMargin.replace("\n", " ")
      )
    }

    private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
      (requestedHashes zip blockBodies)
        .map { case (hash, body) =>
          blockchain.storeBlockBody(hash, body)
        }
        .reduce(_.and(_))
        .commit()

      val receivedHashes = requestedHashes.take(blockBodies.size)
      updateBestBlockIfNeeded(receivedHashes)
      val remainingBlockBodies = requestedHashes.drop(blockBodies.size)
      if (remainingBlockBodies.nonEmpty) {
        syncState = syncState.enqueueBlockBodies(remainingBlockBodies)
      }
    }

    def processSyncing(): Unit = {
      if (fullySynced) {
        finish()
      } else {
        if (blockchainDataToDownload) {
          processDownloads()
        } else if (syncState.isBlockchainWorkFinished && !syncState.stateSyncFinished) {
          // TODO ETCM-103 we are waiting for state sync to finish, we should probably cancel this loop, or look only
          // for new target block
        } else {
          log.info("No more items to request, waiting for {} responses", assignedHandlers.size)
        }
      }
    }

    def finish(): Unit = {
      log.info("Block synchronization in fast mode finished, switching to regular mode")
      // We have downloaded to target + fastSyncBlockValidationX, se we must discard those last blocks
      discardLastBlocks(syncState.safeDownloadTarget, syncConfig.fastSyncBlockValidationX - 1)
      cleanup()
      appStateStorage.fastSyncDone().commit()
      context become idle
      peerRequestsTime = Map.empty
      syncController ! Done
    }

    def cleanup(): Unit = {
      heartBeat.cancel()
      syncStatePersistCancellable.cancel()
      printStatusCancellable.cancel()
      syncStateStorageActor ! PoisonPill
      fastSyncStateStorage.purge()
    }

    def processDownloads(): Unit = {
      if (unassignedPeers.isEmpty) {
        if (assignedHandlers.nonEmpty) {
          log.debug("There are no available peers, waiting for responses")
        } else {
          scheduler.scheduleOnce(syncRetryInterval, self, ProcessSyncing)
        }
      } else {
        val now = Instant.now()
        val peers = unassignedPeers
          .filter(p => peerRequestsTime.get(p).forall(d => d.plusMillis(fastSyncThrottle.toMillis).isBefore(now)))
        peers
          .take(maxConcurrentRequests - assignedHandlers.size)
          .toSeq
          .sortBy(_.ref.toString())
          .foreach(assignBlockchainWork)
      }
    }

    def assignBlockchainWork(peer: Peer): Unit = {
      if (syncState.receiptsQueue.nonEmpty) {
        requestReceipts(peer)
      } else if (syncState.blockBodiesQueue.nonEmpty) {
        requestBlockBodies(peer)
      } else if (
        requestedHeaders.isEmpty &&
        context.child(BlockHeadersHandlerName).isEmpty &&
        syncState.bestBlockHeaderNumber < syncState.safeDownloadTarget
      ) {
        requestBlockHeaders(peer)
      }
    }

    def requestReceipts(peer: Peer): Unit = {
      val (receiptsToGet, remainingReceipts) = syncState.receiptsQueue.splitAt(receiptsPerRequest)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetReceipts, Receipts](
          peer,
          peerResponseTimeout,
          etcPeerManager,
          peerEventBus,
          requestMsg = GetReceipts(receiptsToGet),
          responseMsgCode = Receipts.code
        )
      )

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = syncState.copy(receiptsQueue = remainingReceipts)
      requestedReceipts += handler -> receiptsToGet
    }

    def requestBlockBodies(peer: Peer): Unit = {
      val (blockBodiesToGet, remainingBlockBodies) = syncState.blockBodiesQueue.splitAt(blockBodiesPerRequest)

      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockBodies, BlockBodies](
          peer,
          peerResponseTimeout,
          etcPeerManager,
          peerEventBus,
          requestMsg = GetBlockBodies(blockBodiesToGet),
          responseMsgCode = BlockBodies.code
        )
      )

      context watch handler
      assignedHandlers += (handler -> peer)
      peerRequestsTime += (peer -> Instant.now())
      syncState = syncState.copy(blockBodiesQueue = remainingBlockBodies)
      requestedBlockBodies += handler -> blockBodiesToGet
    }

    def requestBlockHeaders(peer: Peer): Unit = {
      val limit: BigInt =
        if (blockHeadersPerRequest < (syncState.safeDownloadTarget - syncState.bestBlockHeaderNumber))
          blockHeadersPerRequest
        else
          syncState.safeDownloadTarget - syncState.bestBlockHeaderNumber

      val handler = context.actorOf(
        PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
          peer,
          peerResponseTimeout,
          etcPeerManager,
          peerEventBus,
          requestMsg = GetBlockHeaders(Left(syncState.bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false),
          responseMsgCode = BlockHeaders.code
        ),
        BlockHeadersHandlerName
      )

      context watch handler
      assignedHandlers += (handler -> peer)
      requestedHeaders += (peer -> limit)
      peerRequestsTime += (peer -> Instant.now())
    }

    def unassignedPeers: Set[Peer] = peersToDownloadFrom.keySet diff assignedHandlers.values.toSet

    def blockchainDataToDownload: Boolean =
      syncState.blockChainWorkQueued || syncState.bestBlockHeaderNumber < syncState.safeDownloadTarget

    def fullySynced: Boolean = {
      syncState.isBlockchainWorkFinished && assignedHandlers.isEmpty && syncState.stateSyncFinished
    }
  }

  private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
    val fullBlocks = receivedHashes.flatMap { hash =>
      for {
        header <- blockchain.getBlockHeaderByHash(hash)
        _ <- blockchain.getBlockBodyByHash(hash)
        _ <- blockchain.getReceiptsByHash(hash)
      } yield header
    }

    if (fullBlocks.nonEmpty) {
      val bestReceivedBlock = fullBlocks.maxBy(_.number)
      if (appStateStorage.getBestBlockNumber() < bestReceivedBlock.number) {
        appStateStorage.putBestBlockNumber(bestReceivedBlock.number).commit()
      }
    }

  }
}

object FastSync {
  // scalastyle:off parameter.number
  def props(
      fastSyncStateStorage: FastSyncStateStorage,
      appStateStorage: AppStateStorage,
      blockchain: Blockchain,
      validators: Validators,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(
      new FastSync(
        fastSyncStateStorage,
        appStateStorage,
        blockchain,
        validators,
        peerEventBus,
        etcPeerManager,
        syncConfig,
        scheduler
      )
    )

  private case class UpdatePivotBlock(state: FinalBlockProcessingResult)
  private case object ProcessSyncing

  private[sync] case object PersistSyncState

  private case object PrintStatus

  case class SyncState(
      pivotBlock: BlockHeader,
      safeDownloadTarget: BigInt = 0,
      blockBodiesQueue: Seq[ByteString] = Nil,
      receiptsQueue: Seq[ByteString] = Nil,
      downloadedNodesCount: Int = 0,
      totalNodesCount: Int = 0,
      bestBlockHeaderNumber: BigInt = 0,
      nextBlockToFullyValidate: BigInt = 1,
      pivotBlockUpdateFailures: Int = 0,
      updatingPivotBlock: Boolean = false,
      stateSyncFinished: Boolean = false
  ) {

    def enqueueBlockBodies(blockBodies: Seq[ByteString]): SyncState =
      copy(blockBodiesQueue = blockBodiesQueue ++ blockBodies)

    def enqueueReceipts(receipts: Seq[ByteString]): SyncState =
      copy(receiptsQueue = receiptsQueue ++ receipts)

    def blockChainWorkQueued: Boolean = blockBodiesQueue.nonEmpty || receiptsQueue.nonEmpty

    def updateNextBlockToValidate(header: BlockHeader, K: Int, X: Int): SyncState = copy(
      nextBlockToFullyValidate =
        if (bestBlockHeaderNumber >= pivotBlock.number - X)
          header.number + 1
        else
          (header.number + K / 2 + Random.nextInt(K)).min(pivotBlock.number - X)
    )

    def updateDiscardedBlocks(header: BlockHeader, N: Int): SyncState = copy(
      blockBodiesQueue = Seq.empty,
      receiptsQueue = Seq.empty,
      bestBlockHeaderNumber = (header.number - N - 1) max 0,
      nextBlockToFullyValidate = (header.number - N) max 1
    )

    def updatePivotBlock(newPivot: BlockHeader, numberOfSafeBlocks: BigInt, updateFailures: Boolean): SyncState =
      copy(
        pivotBlock = newPivot,
        safeDownloadTarget = newPivot.number + numberOfSafeBlocks,
        pivotBlockUpdateFailures = if (updateFailures) pivotBlockUpdateFailures + 1 else pivotBlockUpdateFailures
      )

    def isBlockchainWorkFinished: Boolean = {
      bestBlockHeaderNumber >= safeDownloadTarget && !blockChainWorkQueued
    }
  }

  sealed trait HashType {
    def v: ByteString
  }

  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  case object Start
  case object Done

  sealed abstract class HeaderProcessingResult

  case object HeadersProcessingFinished extends HeaderProcessingResult

  case class ParentDifficultyNotFound(header: BlockHeader) extends HeaderProcessingResult

  case class ValidationFailed(header: BlockHeader, peer: Peer) extends HeaderProcessingResult

  case object ImportedPivotBlock extends HeaderProcessingResult

  sealed abstract class FinalBlockProcessingResult

  case object ImportedLastBlock extends FinalBlockProcessingResult
  case object LastBlockValidationFailed extends FinalBlockProcessingResult
}
