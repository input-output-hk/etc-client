package io.iohk.ethereum.faucet

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.OneForOneStrategy
import akka.actor.Props
import akka.actor.SupervisorStrategy
import akka.pattern.BackoffOpts
import akka.pattern.BackoffSupervisor

import scala.concurrent.duration._

import io.iohk.ethereum.faucet.FaucetHandler.WalletException
import io.iohk.ethereum.faucet.jsonrpc.WalletService
import io.iohk.ethereum.utils.Logger

object FaucetSupervisor {
  val name = "FaucetSupervisor"
}

class FaucetSupervisor(walletService: WalletService, config: FaucetConfig, shutdown: () => Unit)(implicit
    system: ActorSystem
) extends Logger {

  val childProps: Props = FaucetHandler.props(walletService, config)

  val minBackoff: FiniteDuration = config.supervisor.minBackoff
  val maxBackoff: FiniteDuration = config.supervisor.maxBackoff
  val randomFactor: Double = config.supervisor.randomFactor
  val autoReset: FiniteDuration = config.supervisor.autoReset

  val supervisorProps: Props = BackoffSupervisor.props(
    BackoffOpts
      .onFailure(
        childProps,
        childName = FaucetHandler.name,
        minBackoff = minBackoff,
        maxBackoff = maxBackoff,
        randomFactor = randomFactor
      )
      .withAutoReset(autoReset)
      .withSupervisorStrategy(OneForOneStrategy() {
        case error: WalletException =>
          log.error(s"Stop ${FaucetHandler.name}", error)
          shutdown()
          SupervisorStrategy.Stop
        case error =>
          log.error(s"Restart ${FaucetHandler.name}", error)
          SupervisorStrategy.Restart
      })
  )
  val supervisor: ActorRef = system.actorOf(supervisorProps, FaucetSupervisor.name)
}
