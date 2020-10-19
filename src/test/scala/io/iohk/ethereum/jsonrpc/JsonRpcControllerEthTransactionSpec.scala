package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.FilterManager.TxLog
import io.iohk.ethereum.jsonrpc.JsonSerializers.{
  OptionNoneToJNullSerializer,
  QuantitiesSerializer,
  UnformattedDataJsonSerializer
}
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransaction
import io.iohk.ethereum.{Fixtures, LongPatience}
import org.bouncycastle.util.encoders.Hex
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, Extraction, Formats}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.Future

// scalastyle:off magic.number
class JsonRpcControllerEthTransactionSpec
    extends AnyFlatSpec
    with Matchers
    with JRCMatchers
    with ScalaCheckPropertyChecks
    with ScalaFutures
    with LongPatience
    with Eventually {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  it should "handle eth_getTransactionByBlockHashAndIndex request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockHashAndIndex",
      List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(txIndexToRequest).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndexToRequest))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "handle eth_getRawTransactionByBlockHashAndIndex request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockHashAndIndex",
      List(
        JString(s"0x${blockToRequest.header.hashAsHexString}"),
        JString(s"0x${Hex.toHexString(BigInt(txIndexToRequest).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndexToRequest)

    response should haveResult(expectedTxResponse)
  }

  it should "handle eth_getRawTransactionByHash request" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    val txResponse: SignedTransaction = Fixtures.Blocks.Block3125369.body.transactionList.head
    (mockEthService.getRawTransactionByHash _)
      .expects(*)
      .returning(Future.successful(Right(RawTransactionResponse(Some(txResponse)))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByHash",
      List(
        JString("0xe9b2d3e8a2bc996a1c7742de825fdae2466ae783ce53484304efffe304ff232d")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(encodeSignedTrx(txResponse))
  }

  it should "eth_sendTransaction" in new JsonRpcControllerFixture {
    val params = JObject(
      "from" -> Address(42).toString,
      "to" -> Address(123).toString,
      "value" -> 1000
    ) :: Nil

    val txHash = ByteString(1, 2, 3, 4)

    (personalService
      .sendTransaction(_: SendTransactionRequest))
      .expects(*)
      .returning(Future.successful(Right(SendTransactionResponse(txHash))))

    val rpcRequest = newJsonRpcRequest("eth_sendTransaction", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveResult(JString(s"0x${Hex.toHexString(txHash.toArray)}"))
  }

  it should "eth_getTransactionByBlockNumberAndIndex by tag" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockNumberAndIndex",
      List(
        JString(s"latest"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getTransactionByBlockNumberAndIndex by hex number" in new JsonRpcControllerFixture {
    val blockToRequest =
      Block(Fixtures.Blocks.Block3125369.header.copy(number = BigInt(0xc005)), Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockNumberAndIndex",
      List(
        JString(s"0xC005"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getTransactionByBlockNumberAndIndex by number" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByBlockNumberAndIndex",
      List(
        JInt(Fixtures.Blocks.Block3125369.header.number),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedStx = blockToRequest.body.transactionList(txIndex)
    val expectedTxResponse = Extraction.decompose(
      TransactionResponse(expectedStx, Some(blockToRequest.header), Some(txIndex))
    )

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getRawTransactionByBlockNumberAndIndex by tag" in new JsonRpcControllerFixture {
    // given
    val blockToRequest: Block = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockNumberAndIndex",
      List(
        JString(s"latest"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )

    // when
    val response = jsonRpcController.handleRequest(request).futureValue

    // then
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndex)

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getRawTransactionByBlockNumberAndIndex by hex number" in new JsonRpcControllerFixture {
    // given
    val blockToRequest =
      Block(Fixtures.Blocks.Block3125369.header.copy(number = BigInt(0xc005)), Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockNumberAndIndex",
      List(
        JString(s"0xC005"),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )

    // when
    val response = jsonRpcController.handleRequest(request).futureValue

    // then
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndex)

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getRawTransactionByBlockNumberAndIndex by number" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val txIndex = 1

    blockchain.storeBlock(blockToRequest).commit()

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getRawTransactionByBlockNumberAndIndex",
      List(
        JInt(Fixtures.Blocks.Block3125369.header.number),
        JString(s"0x${Hex.toHexString(BigInt(txIndex).toByteArray)}")
      )
    )
    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedTxResponse = rawTrnHex(blockToRequest.body.transactionList, txIndex)

    response should haveResult(expectedTxResponse)
  }

  it should "eth_getTransactionByHash" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    val txResponse = TransactionResponse(Fixtures.Blocks.Block3125369.body.transactionList.head)
    (mockEthService.getTransactionByHash _)
      .expects(*)
      .returning(Future.successful(Right(GetTransactionByHashResponse(Some(txResponse)))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionByHash",
      List(
        JString("0xe9b2d3e8a2bc996a1c7742de825fdae2466ae783ce53484304efffe304ff232d")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(Extraction.decompose(txResponse))
  }

  it should "eth_getTransactionCount" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getTransactionCount _)
      .expects(*)
      .returning(Future.successful(Right(GetTransactionCountResponse(123))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionCount",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JString(s"latest")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x7b")
  }

  it should "eth_getBlockTransactionCountByNumber " in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    (mockEthService.getBlockTransactionCountByNumber _)
      .expects(*)
      .returning(Future.successful(Right(GetBlockTransactionCountByNumberResponse(17))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getBlockTransactionCountByNumber",
      List(
        JString(s"0x123")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x11")
  }

  it should "handle eth_getBlockTransactionCountByHash request" in new JsonRpcControllerFixture {
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)

    blockchain.storeBlock(blockToRequest).commit()

    val rpcRequest = newJsonRpcRequest(
      "eth_getBlockTransactionCountByHash",
      List(JString(s"0x${blockToRequest.header.hashAsHexString}"))
    )
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    val expectedTxCount = Extraction.decompose(BigInt(blockToRequest.body.transactionList.size))
    response should haveResult(expectedTxCount)
  }

  it should "eth_getTransactionReceipt" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    val arbitraryValue = 42

    val mockRequest = GetTransactionReceiptRequest(
      ByteString(Hex.decode("b903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"))
    )

    val mockResponse = Right(
      GetTransactionReceiptResponse(
        Some(
          TransactionReceiptResponse(
            transactionHash = ByteString(Hex.decode("23" * 32)),
            transactionIndex = 1,
            blockNumber = Fixtures.Blocks.Block3125369.header.number,
            blockHash = Fixtures.Blocks.Block3125369.header.hash,
            cumulativeGasUsed = arbitraryValue * 10,
            gasUsed = arbitraryValue,
            contractAddress = Some(Address(arbitraryValue)),
            logs = Seq(
              TxLog(
                logIndex = 0,
                transactionIndex = 1,
                transactionHash = ByteString(Hex.decode("23" * 32)),
                blockHash = Fixtures.Blocks.Block3125369.header.hash,
                blockNumber = Fixtures.Blocks.Block3125369.header.number,
                address = Address(arbitraryValue),
                data = ByteString(Hex.decode("43" * 32)),
                topics = Seq(ByteString(Hex.decode("44" * 32)), ByteString(Hex.decode("45" * 32)))
              )
            )
          )
        )
      )
    )

    (mockEthService.getTransactionReceipt _).expects(*).returning(Future.successful(mockResponse))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "eth_getTransactionReceipt",
      List(JString(s"0xb903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"))
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveResult(
      JObject(
        JField("transactionHash", JString("0x" + "23" * 32)),
        JField("transactionIndex", JString("0x1")),
        JField("blockNumber", JString("0x2fb079")),
        JField("blockHash", JString("0x" + Hex.toHexString(Fixtures.Blocks.Block3125369.header.hash.toArray[Byte]))),
        JField("cumulativeGasUsed", JString("0x1a4")),
        JField("gasUsed", JString("0x2a")),
        JField("contractAddress", JString("0x000000000000000000000000000000000000002a")),
        JField(
          "logs",
          JArray(
            List(
              JObject(
                JField("logIndex", JString("0x0")),
                JField("transactionIndex", JString("0x1")),
                JField("transactionHash", JString("0x" + "23" * 32)),
                JField(
                  "blockHash",
                  JString("0x" + Hex.toHexString(Fixtures.Blocks.Block3125369.header.hash.toArray[Byte]))
                ),
                JField("blockNumber", JString("0x2fb079")),
                JField("address", JString("0x000000000000000000000000000000000000002a")),
                JField("data", JString("0x" + "43" * 32)),
                JField("topics", JArray(List(JString("0x" + "44" * 32), JString("0x" + "45" * 32))))
              )
            )
          )
        )
      )
    )
  }

  "eth_pendingTransactions" should "request pending transactions and return valid response when mempool is empty" in new JsonRpcControllerFixture {
    val mockEthService = mock[EthService]
    (mockEthService.ethPendingTransactions _)
      .expects(*)
      .returning(Future.successful(Right(EthPendingTransactionsResponse(List()))))
    val jRpcController =
      new JsonRpcController(
        web3Service,
        netService,
        mockEthService,
        personalService,
        None,
        debugService,
        qaService,
        checkpointingService,
        config
      )

    val request = JsonRpcRequest(
      "2.0",
      "eth_pendingTransactions",
      Some(
        JArray(
          List()
        )
      ),
      Some(JInt(1))
    )

    val response: JsonRpcResponse = jRpcController.handleRequest(request).futureValue

    response should haveResult(JArray(List()))
  }

  it should "request pending transactions and return valid response when mempool has transactions" in new JsonRpcControllerFixture {
    val transactions = (0 to 1).map(_ => {
      val fakeTransaction = SignedTransactionWithSender(
        Transaction(
          nonce = 0,
          gasPrice = 123,
          gasLimit = 123,
          receivingAddress = Address("0x1234"),
          value = 0,
          payload = ByteString()
        ),
        signature = ECDSASignature(0, 0, 0.toByte),
        sender = Address("0x1234")
      )
      PendingTransaction(fakeTransaction, System.currentTimeMillis)
    })

    val mockEthService = mock[EthService]
    (mockEthService.ethPendingTransactions _)
      .expects(*)
      .returning(Future.successful(Right(EthPendingTransactionsResponse(transactions))))
    val jRpcController =
      new JsonRpcController(
        web3Service,
        netService,
        mockEthService,
        personalService,
        None,
        debugService,
        qaService,
        checkpointingService,
        config
      )
    val request = JsonRpcRequest(
      "2.0",
      "eth_pendingTransactions",
      Some(
        JArray(
          List()
        )
      ),
      Some(JInt(1))
    )

    val response: JsonRpcResponse = jRpcController.handleRequest(request).futureValue

    val result = JArray(
      transactions
        .map(tx => {
          encodeAsHex(tx.stx.tx.hash)
        })
        .toList
    )

    response should haveResult(result)
  }
}
