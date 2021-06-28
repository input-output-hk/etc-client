package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.{BlockHeader, Blockchain, BlockchainReader}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.network.p2p.messages.ETH62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.ETH63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.p2p.messages.ETH63.MptNodeEncoders._
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.p2p.messages.Codes

/**
  * BlockchainHost actor is in charge of replying to the peer's requests for blockchain data, which includes both
  * node and block data.
  */
class BlockchainHostActor(
    blockchainReader: BlockchainReader,
    evmCodeStorage: EvmCodeStorage,
    peerConfiguration: PeerConfiguration,
    peerEventBusActor: ActorRef,
    etcPeerManagerActor: ActorRef
) extends Actor
    with ActorLogging {

  private val requestMsgsCodes =
    Set(Codes.GetNodeDataCode, Codes.GetReceiptsCode, Codes.GetBlockBodiesCode, Codes.GetBlockHeadersCode)
  peerEventBusActor ! Subscribe(MessageClassifier(requestMsgsCodes, PeerSelector.AllPeers))

  override def receive: Receive = { case MessageFromPeer(message, peerId) =>
    val responseOpt = handleBlockFastDownload(message) orElse handleEvmCodeMptFastDownload(message)
    responseOpt.foreach { response =>
      etcPeerManagerActor ! EtcPeerManagerActor.SendMessage(response, peerId)
    }
  }

  /**
    * Handles requests for node data, which includes both mpt nodes and evm code (both requested by hash).
    * Both types of node data are requested by the same GetNodeData message
    *
    * @param message to be processed
    * @return message response if message is a request for node data or None if not
    */
  private def handleEvmCodeMptFastDownload(message: Message): Option[MessageSerializable] = message match {
    case GetNodeData(mptElementsHashes) =>
      val hashesRequested =
        mptElementsHashes.take(peerConfiguration.fastSyncHostConfiguration.maxMptComponentsPerMessage)

      val nodeData: Seq[ByteString] = hashesRequested.flatMap { hash =>
        //Fetch mpt node by hash
        val maybeMptNodeData = blockchainReader.getMptNodeByHash(hash).map(e => e.toBytes: ByteString)

        //If no mpt node was found, fetch evm by hash
        maybeMptNodeData.orElse(evmCodeStorage.get(hash))
      }

      Some(NodeData(nodeData))

    case _ => None
  }

  /**
    * Handles request for block data, which includes receipts, block bodies and headers (all requested by hash)
    *
    * @param message to be processed
    * @return message response if message is a request for block data or None if not
    */
  private def handleBlockFastDownload(message: Message): Option[MessageSerializable] = message match {
    case request: GetReceipts =>
      val receipts = request.blockHashes
        .take(peerConfiguration.fastSyncHostConfiguration.maxReceiptsPerMessage)
        .flatMap(hash => blockchainReader.getReceiptsByHash(hash))

      Some(Receipts(receipts))

    case request: GetBlockBodies =>
      val blockBodies = request.hashes
        .take(peerConfiguration.fastSyncHostConfiguration.maxBlocksBodiesPerMessage)
        .flatMap(hash => blockchainReader.getBlockBodyByHash(hash))

      Some(BlockBodies(blockBodies))

    case request: GetBlockHeaders =>
      val blockNumber = request.block.fold(a => Some(a), b => blockchainReader.getBlockHeaderByHash(b).map(_.number))

      blockNumber match {
        case Some(startBlockNumber) if startBlockNumber >= 0 && request.maxHeaders >= 0 && request.skip >= 0 =>
          val headersCount: BigInt =
            request.maxHeaders min peerConfiguration.fastSyncHostConfiguration.maxBlocksHeadersPerMessage

          val range = if (request.reverse) {
            startBlockNumber to (startBlockNumber - (request.skip + 1) * headersCount + 1) by -(request.skip + 1)
          } else {
            startBlockNumber to (startBlockNumber + (request.skip + 1) * headersCount - 1) by (request.skip + 1)
          }

          val blockHeaders: Seq[BlockHeader] = range.flatMap { a: BigInt => blockchainReader.getBlockHeaderByNumber(a) }

          Some(BlockHeaders(blockHeaders))

        case _ =>
          log.warning("got request for block headers with invalid block hash/number: {}", request)
          None
      }

    case _ => None

  }

}

object BlockchainHostActor {

  def props(
      blockchainReader: BlockchainReader,
      evmCodeStorage: EvmCodeStorage,
      peerConfiguration: PeerConfiguration,
      peerEventBusActor: ActorRef,
      etcPeerManagerActor: ActorRef
  ): Props =
    Props(
      new BlockchainHostActor(
        blockchainReader,
        evmCodeStorage,
        peerConfiguration,
        peerEventBusActor,
        etcPeerManagerActor
      )
    )

}
