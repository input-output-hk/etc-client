package io.iohk.ethereum.jsonrpc

import monix.eval.Task

import org.json4s.Extraction
import org.json4s.JArray
import org.json4s.JBool
import org.json4s.JInt
import org.json4s.JLong
import org.json4s.JObject
import org.json4s.JString
import org.scalamock.scalatest.AsyncMockFactory

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.FreeSpecBase
import io.iohk.ethereum.SpecFixtures
import io.iohk.ethereum.jsonrpc.MantisService.GetAccountTransactionsResponse
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.transactions.TransactionHistoryService.ExtendedTransactionData
import io.iohk.ethereum.transactions.TransactionHistoryService.MinedTransactionData
import io.iohk.ethereum.utils.Config

class MantisJRCSpec extends FreeSpecBase with SpecFixtures with AsyncMockFactory with JRCMatchers {
  import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.formats

  class Fixture extends ApisBuilder {
    def config: JsonRpcConfig = JsonRpcConfig(Config.config, available)

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
    val qaService: QAService = mock[QAService]
    val checkpointingService: CheckpointingService = mock[CheckpointingService]
    val mantisService: MantisService = mock[MantisService]

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

  }
  def createFixture() = new Fixture

  "Mantis JRC" - {
    "should handle mantis_getAccountTransactions" in testCaseM { fixture =>
      import fixture._
      val block = Fixtures.Blocks.Block3125369
      val sentTx = block.body.transactionList.head
      val receivedTx = block.body.transactionList.last

      (mantisService.getAccountTransactions _)
        .expects(*)
        .returning(
          Task.now(
            Right(
              GetAccountTransactionsResponse(
                List(
                  ExtendedTransactionData(
                    sentTx,
                    isOutgoing = true,
                    Some(MinedTransactionData(block.header, 0, 42, false))
                  ),
                  ExtendedTransactionData(
                    receivedTx,
                    isOutgoing = false,
                    Some(MinedTransactionData(block.header, 1, 21, true))
                  )
                )
              )
            )
          )
        )

      val request: JsonRpcRequest = JsonRpcRequest(
        "2.0",
        "mantis_getAccountTransactions",
        Some(
          JArray(
            List(
              JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
              JInt(100),
              JInt(200)
            )
          )
        ),
        Some(JInt(1))
      )

      val expectedTxs = Seq(
        JObject(
          Extraction
            .decompose(TransactionResponse(sentTx, Some(block.header), Some(0)))
            .asInstanceOf[JObject]
            .obj ++ List(
            "isPending" -> JBool(false),
            "isCheckpointed" -> JBool(false),
            "isOutgoing" -> JBool(true),
            "timestamp" -> JLong(block.header.unixTimestamp),
            "gasUsed" -> JString(s"0x${BigInt(42).toString(16)}")
          )
        ),
        JObject(
          Extraction
            .decompose(TransactionResponse(receivedTx, Some(block.header), Some(1)))
            .asInstanceOf[JObject]
            .obj ++ List(
            "isPending" -> JBool(false),
            "isCheckpointed" -> JBool(true),
            "isOutgoing" -> JBool(false),
            "timestamp" -> JLong(block.header.unixTimestamp),
            "gasUsed" -> JString(s"0x${BigInt(21).toString(16)}")
          )
        )
      )

      for {
        response <- jsonRpcController.handleRequest(request)
      } yield response should haveObjectResult("transactions" -> JArray(expectedTxs.toList))
    }
  }
}
