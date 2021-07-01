package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST
import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JBool
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue

import io.iohk.ethereum.jsonrpc.EthMiningService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder

object EthMiningJsonMethodsImplicits extends JsonMethodsImplicits {
  implicit val eth_mining: NoParamsMethodDecoder[GetMiningRequest] with JsonEncoder[GetMiningResponse] =
    new NoParamsMethodDecoder(GetMiningRequest()) with JsonEncoder[GetMiningResponse] {
      override def encodeJson(t: GetMiningResponse): JValue = JBool(t.isMining)
    }

  implicit val eth_getWork: NoParamsMethodDecoder[GetWorkRequest] with JsonEncoder[GetWorkResponse] =
    new NoParamsMethodDecoder(GetWorkRequest()) with JsonEncoder[GetWorkResponse] {
      override def encodeJson(t: GetWorkResponse): JsonAST.JValue = {
        val powHeaderHash = encodeAsHex(t.powHeaderHash)
        val dagSeed = encodeAsHex(t.dagSeed)
        val target = encodeAsHex(t.target)
        JArray(List(powHeaderHash, dagSeed, target))
      }
    }

  implicit val eth_submitHashrate: JsonMethodDecoder[SubmitHashRateRequest] with JsonEncoder[SubmitHashRateResponse] =
    new JsonMethodDecoder[SubmitHashRateRequest] with JsonEncoder[SubmitHashRateResponse] {
      override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitHashRateRequest] =
        params match {
          case Some(JArray(hashRate :: JString(id) :: Nil)) =>
            val result: Either[JsonRpcError, SubmitHashRateRequest] = for {
              rate <- extractQuantity(hashRate)
              miner <- extractHash(id)
            } yield SubmitHashRateRequest(rate, miner)
            result
          case _ =>
            Left(InvalidParams())
        }

      override def encodeJson(t: SubmitHashRateResponse): JValue = JBool(t.success)
    }

  implicit val eth_hashrate: NoParamsMethodDecoder[GetHashRateRequest] with JsonEncoder[GetHashRateResponse] =
    new NoParamsMethodDecoder(GetHashRateRequest()) with JsonEncoder[GetHashRateResponse] {
      override def encodeJson(t: GetHashRateResponse): JsonAST.JValue = encodeAsHex(t.hashRate)
    }

  implicit val eth_coinbase: NoParamsMethodDecoder[GetCoinbaseRequest] with JsonEncoder[GetCoinbaseResponse] =
    new NoParamsMethodDecoder(GetCoinbaseRequest()) with JsonEncoder[GetCoinbaseResponse] {
      override def encodeJson(t: GetCoinbaseResponse): JsonAST.JValue =
        encodeAsHex(t.address.bytes)
    }

  implicit val eth_submitWork: JsonMethodDecoder[SubmitWorkRequest] with JsonEncoder[SubmitWorkResponse] =
    new JsonMethodDecoder[SubmitWorkRequest] with JsonEncoder[SubmitWorkResponse] {
      override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitWorkRequest] = params match {
        case Some(JArray(JString(nonce) :: JString(powHeaderHash) :: JString(mixHash) :: Nil)) =>
          for {
            n <- extractBytes(nonce)
            p <- extractBytes(powHeaderHash)
            m <- extractBytes(mixHash)
          } yield SubmitWorkRequest(n, p, m)
        case _ =>
          Left(InvalidParams())
      }

      override def encodeJson(t: SubmitWorkResponse): JValue = JBool(t.success)
    }

}
