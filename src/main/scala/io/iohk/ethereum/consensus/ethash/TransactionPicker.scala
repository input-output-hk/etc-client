package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorRef
import akka.util.Timeout
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.Logger
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

trait TransactionPicker extends Logger {

  protected def pendingTransactionsManager: ActorRef
  protected def getTransactionFromPoolTimeout: FiniteDuration

  implicit val timeout: Timeout = Timeout(getTransactionFromPoolTimeout)

  protected def getTransactionsFromPool: Task[PendingTransactionsResponse] = {
    pendingTransactionsManager
      .askFor[PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .onErrorHandle { ex =>
        log.error("Failed to get transactions, mining block with empty transactions list", ex)
        PendingTransactionsResponse(Nil)
      }
  }
}
