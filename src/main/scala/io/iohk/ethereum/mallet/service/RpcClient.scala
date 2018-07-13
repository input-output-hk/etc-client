package io.iohk.ethereum.mallet.service


import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Json}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.{JsonRpcError, TransactionReceiptResponse}
import io.iohk.ethereum.mallet.common.{Constants, Err, RpcClientError, Util}
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object RpcClient {

  /**
    * This factory method is defining an ActorSystem, ActorMaterializer and ExecutionContext for
    * the [[RpcClient]]. To customize these dependencies use [[RpcClient]]'s constructor
    */
  def apply(node: Uri): RpcClient = {
    // TODO: CL option to enable akka logging
    val akkaConfig = ConfigFactory.load("mallet")

    implicit val system = ActorSystem("mallet_rpc", akkaConfig)
    implicit val mat = ActorMaterializer()
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

    new RpcClient(node)
  }
}

/**
  * Talks to a node over HTTP(S) JSON-RPC
  * Note: the URI schema determines whether HTTP or HTTPS is used
  */
class RpcClient(node: Uri)(implicit system: ActorSystem, mat: ActorMaterializer, ec: ExecutionContext) extends Logger {
  import CommonJsonCodecs._

  //TODO: CL option
  private val httpTimeout = 5.seconds

  def shutdown(): Unit = {
    Await.ready(system.terminate(), 5.seconds)
  }

  def sendTransaction(rawTx: ByteString): Either[Err, ByteString] =
    doRequest[ByteString]("eth_sendRawTransaction", List(rawTx.asJson))

  def getNonce(address: Address): Either[Err, BigInt] =
    doRequest[BigInt]("eth_getTransactionCount", List(address.asJson, "latest".asJson))

  def getBalance(address: Address): Either[Err, BigInt] =
    doRequest[BigInt]("eth_getBalance", List(address.asJson, "latest".asJson))

  def getReceipt(txHash: ByteString): Either[Err, Option[TransactionReceiptResponse]] =
    doRequest[Option[TransactionReceiptResponse]]("eth_getTransactionReceipt", List(txHash.asJson))

  private def doRequest[T: Decoder](method: String, args: Seq[Json]): Either[Err, T] = {
    val requestId = s"${Constants.AppName}_${UUID.randomUUID()}"
    val jsonRequest = prepareJsonRequest(method, args, requestId)
    makeRpcCall(jsonRequest, requestId).flatMap(getResult[T])
  }

  private def getResult[T: Decoder](jsonResponse: Json): Either[Err, T] = {
    jsonResponse.hcursor.downField("error").as[JsonRpcError] match {
      case Right(error) =>
        Left(RpcClientError(s"Node returned an error: ${error.message} (${error.code})"))
      case Left(_) =>
        jsonResponse.hcursor.downField("result").as[T].left.map(f => RpcClientError(f.message))
    }
  }

  private def makeRpcCall(jsonRequest: Json, requestId: String): Either[Err, Json] = {
    val entity = HttpEntity(ContentTypes.`application/json`, jsonRequest.noSpaces)
    val request = HttpRequest(method = HttpMethods.POST, uri = node, entity = entity)

    log.info(s"Sending RPC request [$requestId] to $node:\n" + jsonRequest.spaces2)

    val responseF: Future[Either[Err, Json]] = Http().singleRequest(request)
      .flatMap(_.entity.toStrict(httpTimeout))
      .map { e =>
        val result = parse(e.data.utf8String).left.map(e => RpcClientError(e.message))
        result match {
          case Right(good) =>
            log.info(s"Received response for RPC request [$requestId]:\n" + good.spaces2)

          case Left(bad) =>
            log.error(s"Invalid response to RPC request [$requestId]:\n" + bad.msg)
        }
        result
      }
      .recover { case ex =>
        log.error(s"RPC request [$requestId] failed", ex)
        Left(RpcClientError("RPC request failed: " + Util.exceptionToString(ex)))
      }

    Try(Await.result(responseF, httpTimeout)) match {
      case Success(res) =>
        res

      case Failure(_) =>
        log.error(s"RPC request [$requestId] timed out after $httpTimeout")
        Left(RpcClientError(s"RPC request to '$node' timed out after $httpTimeout"))
    }
  }

  private def prepareJsonRequest(method: String, args: Seq[Json], requestId: String): Json = {
    Map(
      "jsonrpc" -> "2.0".asJson,
      "method" -> method.asJson,
      "params" -> args.asJson,
      "id" -> requestId.asJson
    ).asJson
  }
}
