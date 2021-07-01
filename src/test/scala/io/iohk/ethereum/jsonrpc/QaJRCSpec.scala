package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.json4s.Extraction
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.scalamock.handlers.CallHandler1
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import io.iohk.ethereum.ByteGenerators
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MineBlocks
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponse
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Checkpoint
import io.iohk.ethereum.jsonrpc.QAService.MineBlocksResponse.MinerResponseType._
import io.iohk.ethereum.jsonrpc.QAService._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config

class QaJRCSpec
    extends AnyWordSpec
    with Matchers
    with PatienceConfiguration
    with NormalPatience
    with JsonMethodsImplicits {

  "QaJRC" should {
    "request block mining and return valid response with correct message" when {
      "mining ordered" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MockedMinerResponses.MiningOrdered)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).runSyncUnsafe()

        response should haveObjectResult(responseType(MiningOrdered), nullMessage)
      }

      "miner is working" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MockedMinerResponses.MinerIsWorking)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).runSyncUnsafe()

        response should haveObjectResult(responseType(MinerIsWorking), nullMessage)
      }

      "miner doesn't exist" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MockedMinerResponses.MinerNotExist)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).runSyncUnsafe()

        response should haveObjectResult(responseType(MinerNotExist), nullMessage)
      }

      "miner not support current msg" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MockedMinerResponses.MinerNotSupported(MineBlocks(1, true)))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).runSyncUnsafe()

        response should haveObjectResult(responseType(MinerNotSupport), msg("MineBlocks(1,true,None)"))
      }

      "miner return error" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MockedMinerResponses.MiningError("error"))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).runSyncUnsafe()

        response should haveObjectResult(responseType(MiningError), msg("error"))
      }
    }

    "request block mining and return InternalError" when {
      "communication with miner failed" in new TestSetup {
        (qaService.mineBlocks _)
          .expects(mineBlocksReq)
          .returning(Task.raiseError(new ClassCastException("error")))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).runSyncUnsafe()

        response should haveError(JsonRpcError.InternalError)
      }
    }

    "request generating checkpoint and return valid response" when {
      "given block to be checkpointed exists and checkpoint is generated correctly" in new TestSetup {
        (qaService.generateCheckpoint _)
          .expects(generateCheckpointReq)
          .returning(Task.now(Right(GenerateCheckpointResponse(checkpoint))))

        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(generateCheckpointRpcRequest).runSyncUnsafe()

        response should haveResult(Extraction.decompose(checkpoint))
      }
    }

    "request generating block with checkpoint and return valid response" when {
      "requested best block to be checkpointed and block with checkpoint is generated correctly" in new TestSetup {
        val req = generateCheckpointRpcRequest.copy(
          params = Some(
            JArray(
              List(
                JArray(
                  privateKeysAsJson
                )
              )
            )
          )
        )
        val expectedServiceReq = generateCheckpointReq.copy(blockHash = None)
        (qaService.generateCheckpoint _)
          .expects(expectedServiceReq)
          .returning(Task.now(Right(GenerateCheckpointResponse(checkpoint))))

        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(req).runSyncUnsafe()

        response should haveResult(Extraction.decompose(checkpoint))
      }
    }

    "request generating block with checkpoint and return InvalidParams" when {
      "block hash is not valid" in new TestSetup {
        val req = generateCheckpointRpcRequest.copy(
          params = Some(
            JArray(
              List(
                JArray(
                  privateKeysAsJson
                ),
                JInt(1)
              )
            )
          )
        )
        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(req).runSyncUnsafe()

        response should haveError(JsonRpcError.InvalidParams())
      }

      "private keys are not valid" in new TestSetup {
        val req = generateCheckpointRpcRequest.copy(
          params = Some(
            JArray(
              List(
                JArray(
                  privateKeysAsJson :+ JInt(1)
                ),
                JString(blockHashAsString)
              )
            )
          )
        )
        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(req).runSyncUnsafe()

        response should haveError(
          JsonRpcError.InvalidParams("Unable to parse private key, expected byte data but got: JInt(1)")
        )
      }

      "bad params structure" in new TestSetup {
        val req = generateCheckpointRpcRequest.copy(
          params = Some(
            JArray(
              List(
                JString(blockHashAsString),
                JArray(
                  privateKeysAsJson
                )
              )
            )
          )
        )
        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(req).runSyncUnsafe()

        response should haveError(JsonRpcError.InvalidParams())
      }
    }

    "request generating block with checkpoint and return InternalError" when {
      "generating failed" in new TestSetup {
        (qaService.generateCheckpoint _)
          .expects(generateCheckpointReq)
          .returning(Task.raiseError(new RuntimeException("error")))

        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(generateCheckpointRpcRequest).runSyncUnsafe()

        response should haveError(JsonRpcError.InternalError)
      }
    }

    "request federation members info and return valid response" when {
      "getting federation public keys is successful" in new TestSetup {
        val checkpointPubKeys: Seq[ByteString] = blockchainConfig.checkpointPubKeys.toList
        (qaService.getFederationMembersInfo _)
          .expects(GetFederationMembersInfoRequest())
          .returning(Task.now(Right(GetFederationMembersInfoResponse(checkpointPubKeys))))

        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(getFederationMembersInfoRpcRequest).runSyncUnsafe()

        val result = JObject(
          "membersPublicKeys" -> JArray(
            checkpointPubKeys.map(encodeAsHex).toList
          )
        )

        response should haveResult(result)
      }
    }

    "request federation members info and return InternalError" when {
      "getting federation members info failed" in new TestSetup {
        (qaService.getFederationMembersInfo _)
          .expects(GetFederationMembersInfoRequest())
          .returning(Task.raiseError(new RuntimeException("error")))

        val response: JsonRpcResponse =
          jsonRpcController.handleRequest(getFederationMembersInfoRpcRequest).runSyncUnsafe()

        response should haveError(JsonRpcError.InternalError)
      }
    }
  }

  trait TestSetup
      extends MockFactory
      with JRCMatchers
      with ByteGenerators
      with BlockchainConfigBuilder
      with ApisBuilder {
    def config: JsonRpcConfig = JsonRpcConfig(Config.config, available)

    val appStateStorage: AppStateStorage = mock[AppStateStorage]
    val web3Service: Web3Service = mock[Web3Service]
    val netService: NetService = mock[NetService]
    val personalService: PersonalService = mock[PersonalService]
    val debugService: DebugService = mock[DebugService]
    val ethService: EthInfoService = mock[EthInfoService]
    val ethMiningService: EthMiningService = mock[EthMiningService]
    val ethBlocksService: EthBlocksService = mock[EthBlocksService]
    val ethTxService: EthTxService = mock[EthTxService]
    val ethUserService: EthUserService = mock[EthUserService]
    val ethFilterService: EthFilterService = mock[EthFilterService]
    val checkpointingService: CheckpointingService = mock[CheckpointingService]
    val mantisService: MantisService = mock[MantisService]
    val qaService: QAService = mock[QAService]

    val jsonRpcController =
      new JsonRpcController(
        web3Service,
        netService,
        ethService,
        ethMiningService,
        ethBlocksService,
        ethTxService,
        ethUserService,
        ethFilterService,
        personalService,
        None,
        debugService,
        qaService,
        checkpointingService,
        mantisService,
        ProofServiceDummy,
        config
      )

    val mineBlocksReq: MineBlocksRequest = MineBlocksRequest(1, withTransactions = true, None)

    val mineBlocksRpcRequest: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "qa_mineBlocks",
      Some(
        JArray(
          List(
            JInt(1),
            JBool(true)
          )
        )
      ),
      Some(JInt(1))
    )

    val blockHash: ByteString = byteStringOfLengthNGen(32).sample.get
    val blockHashAsString: String = ByteStringUtils.hash2string(blockHash)
    val privateKeys: List[ByteString] = seqByteStringOfNItemsOfLengthMGen(3, 32).sample.get.toList
    val keyPairs: List[AsymmetricCipherKeyPair] = privateKeys.map { key =>
      crypto.keyPairFromPrvKey(key.toArray)
    }
    val signatures: List[ECDSASignature] = keyPairs.map(ECDSASignature.sign(blockHash.toArray, _))
    val checkpoint: Checkpoint = Checkpoint(signatures)
    val privateKeysAsJson: List[JString] = privateKeys.map { key =>
      JString(ByteStringUtils.hash2string(key))
    }

    val generateCheckpointReq: GenerateCheckpointRequest = GenerateCheckpointRequest(privateKeys, Some(blockHash))

    val generateCheckpointRpcRequest: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "qa_generateCheckpoint",
      Some(
        JArray(
          List(
            JArray(
              privateKeysAsJson
            ),
            JString(blockHashAsString)
          )
        )
      ),
      Some(1)
    )

    val getFederationMembersInfoRpcRequest: JsonRpcRequest = JsonRpcRequest(
      "2.0",
      "qa_getFederationMembersInfo",
      Some(
        JArray(
          List()
        )
      ),
      Some(1)
    )

    def msg(str: String): JField = "message" -> JString(str)
    val nullMessage: JField = "message" -> JNull

    def responseType(expectedType: MineBlocksResponse.MinerResponseType): JField =
      "responseType" -> JString(expectedType.entryName)

    def mockSuccessfulMineBlocksBehaviour(
        resp: MockedMinerResponse
    ): CallHandler1[MineBlocksRequest, Task[Either[JsonRpcError, MineBlocksResponse]]] =
      (qaService.mineBlocks _)
        .expects(mineBlocksReq)
        .returning(Task.now(Right(MineBlocksResponse(resp))))

    val fakeChainId: Byte = 42.toByte
  }
}
