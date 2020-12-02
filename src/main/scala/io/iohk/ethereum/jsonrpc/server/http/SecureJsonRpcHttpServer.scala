package io.iohk.ethereum.jsonrpc.server.http

import akka.actor.ActorSystem
import akka.http.scaladsl.{ConnectionContext, Http}
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import io.iohk.ethereum.jsonrpc.JsonRpcHealthChecker
import io.iohk.ethereum.security.SSLError
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import java.security.SecureRandom

import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.utils.Logger
import javax.net.ssl.SSLContext

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class SecureJsonRpcHttpServer(
    val jsonRpcController: JsonRpcBaseController,
    val jsonRpcHealthChecker: JsonRpcHealthChecker,
    val config: JsonRpcHttpServerConfig,
    secureRandom: SecureRandom,
    getSSLContext: () => Either[SSLError, SSLContext]
)(implicit val actorSystem: ActorSystem)
    extends JsonRpcHttpServer
    with Logger {

  def run(): Unit = {
    val maybeHttpsContext = getSSLContext().map(sslContext => ConnectionContext.httpsServer(sslContext))

    maybeHttpsContext match {
      case Right(httpsContext) =>
        val bindingResultF = Http().newServerAt(config.interface, config.port).enableHttps(httpsContext).bind(route)

        bindingResultF onComplete {
          case Success(serverBinding) => log.info(s"JSON RPC HTTPS server listening on ${serverBinding.localAddress}")
          case Failure(ex) => log.error("Cannot start JSON HTTPS RPC server", ex)
        }
      case Left(error) =>
        log.error(s"Cannot start JSON HTTPS RPC server due to: $error")
        throw new IllegalStateException(error.reason)
    }
  }

  override def corsAllowedOrigins: HttpOriginMatcher = config.corsAllowedOrigins
}
