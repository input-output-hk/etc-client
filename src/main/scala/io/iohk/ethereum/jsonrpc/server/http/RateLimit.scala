package io.iohk.ethereum.jsonrpc.server.http

import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import akka.http.scaladsl.server.{Directive0, Route}
import io.iohk.ethereum.db.cache.SimpleLRU
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.RateLimitConfig
import akka.http.scaladsl.server.Directives._
import io.iohk.ethereum.jsonrpc.JsonRpcError
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers
import io.iohk.ethereum.utils.Logger
import org.json4s.{DefaultFormats, Formats, Serialization, native}

class RateLimit(config: RateLimitConfig) extends Directive0 with Json4sSupport {

  private implicit val serialization: Serialization = native.Serialization
  private implicit val formats: Formats = DefaultFormats + JsonSerializers.RpcErrorJsonSerializer

  lazy val lru = new SimpleLRU[RemoteAddress](
    config.latestTimestampCacheSize,
    config.minRequestInterval.toMillis
  )

  // Such algebras prevent if-elseif-else boilerplate in the JsonRPCServer code
  // It is also guaranteed that:
  //   1) config.enabled is checked only once - on route init
  //   2) no LRU is created in case of config.enabled == false
  val rateLimitAlgebra: (Unit => Route) => Route = {
    if (config.enabled) {
      val minInterval = config.minRequestInterval.toSeconds
      f =>
        extractClientIP { ip =>
          if (lru.checkAndRefreshEntry(ip)) {
            val err = JsonRpcError.RateLimitError(minInterval)
            complete((StatusCodes.TooManyRequests, err))
          } else f.apply(())
        }
    } else _.apply(())
  }

  override def tapply(f: Unit => Route): Route = rateLimitAlgebra(f)

}
