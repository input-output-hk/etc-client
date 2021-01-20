package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import com.softwaremill.diffx.scalatest.DiffMatcher
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.domain.{Account, Address, Block, EthereumUInt256Mpt, UInt256}
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{Ledger, StxLedger}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.utils._
import io.iohk.ethereum._
import io.iohk.ethereum.jsonrpc.ProofService.{GetProofRequest, StorageProofKey}
import monix.execution.Scheduler.Implicits.global
import org.bouncycastle.util.encoders.Hex
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import io.iohk.ethereum.jsonrpc.EthUserService.GetBalanceResponse
import io.iohk.ethereum.jsonrpc.EthUserService.GetBalanceRequest
import io.iohk.ethereum.jsonrpc.EthUserService.GetTransactionCountRequest
import io.iohk.ethereum.jsonrpc.EthUserService.GetStorageAtRequest

class EthProofServiceSpec
    extends TestKit(ActorSystem("EthGetProofSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals
    with DiffMatcher {

  "EthProofService" should "handle getStorageAt request" in new TestSetup {
    // given
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    val key = 333
    val value = 123

    val storageMpt = EthereumUInt256Mpt
      .storageMpt(
        ByteString(MerklePatriciaTrie.EmptyRootHash),
        storagesInstance.storages.stateStorage.getBackingStorage(0)
      )
      .put(UInt256(key), UInt256(value))

    val account = Account(
      nonce = 0,
      balance = UInt256(0),
      storageRoot = ByteString(storageMpt.getRootHash),
      codeHash = ByteString("")
    )

    import MerklePatriciaTrie.defaultByteArraySerializable
    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.stateStorage.getBackingStorage(0))
        .put(
          crypto.kec256(address.bytes.toArray[Byte]),
          account
        )

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.header.number)

    val ethGetProof = new EthProofService(blockchain, blockGenerator, blockchainConfig.ethCompatibleStorage)
    val storageKeys = Seq(StorageProofKey(key))
    val blockNumber = BlockParam.Latest
    val request = GetProofRequest(address, storageKeys, blockNumber)

    // when
    val result = ethGetProof.getProof(request)

    // then
    val balanceResponse: GetBalanceResponse = ethUserService
      .getBalance(GetBalanceRequest(address, BlockParam.Latest))
      .runSyncUnsafe()
      .getOrElse(fail("ethUserService.getBalance did not get valid response"))

    val transactionCountResponse = ethUserService
      .getTransactionCount(GetTransactionCountRequest(address, BlockParam.Latest))
      .runSyncUnsafe()
      .getOrElse(fail("ethUserService.getTransactionCount did not get valid response"))

    val storageValues: Seq[ByteString] = storageKeys.map { position =>
      ethUserService
        .getStorageAt(GetStorageAtRequest(address, position.v, BlockParam.Latest))
        .runSyncUnsafe()
        .getOrElse(fail("ethUserService.getStorageAt did not get valid response"))
        .value
    }

    val givenResult = result
      .runSyncUnsafe()
      .getOrElse(fail())
      .proofAccount

    givenResult.address should matchTo(address)
    givenResult.codeHash shouldBe account.codeHash
    givenResult.storageHash shouldBe account.storageRoot

    givenResult.nonce shouldBe UInt256(transactionCountResponse.value)

    givenResult.balance shouldBe balanceResponse.value

    givenResult.storageProof.map(_.key) shouldBe storageKeys
    givenResult.storageProof.map(_.value.toString) shouldBe storageValues.map(_.mkString)
    givenResult.storageProof.map(_.proof).foreach { p =>
      p should not be empty
    }
  }

  "EthProofService" should "return an error when the proof is requested for non-existing account" in new TestSetup {
    val ethGetProof = new EthProofService(blockchain, blockGenerator, blockchainConfig.ethCompatibleStorage)
    val key = 999
    val storageKeys = Seq(StorageProofKey(key))
    val blockNumber = BlockParam.Latest
    val wrongAddress = Address(666)
    val request = GetProofRequest(wrongAddress, storageKeys, blockNumber)
    val retrievedAccountProofWrong: ServiceResponse[ProofService.GetProofResponse] = ethGetProof.getProof(request)
    retrievedAccountProofWrong.runSyncUnsafe().isLeft shouldBe true
  }

  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup with ApisBuilder {
    val blockGenerator = mock[EthashBlockGenerator]

    override lazy val ledger = mock[Ledger]

    val ethUserService = new EthUserService(
      blockchain,
      ledger,
      blockchainConfig
    )
  }
}
