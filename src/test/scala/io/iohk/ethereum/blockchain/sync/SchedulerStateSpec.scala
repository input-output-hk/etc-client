package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.SchedulerState
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.StateNode
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.StateNodeRequest

class SchedulerStateSpec extends AnyFlatSpec with Matchers {
  "SchedulerState" should "schedule node hashes for retrieval" in new TestSetup {
    val stateWithRequest = schedulerState.schedule(request1)
    assert(stateWithRequest != schedulerState)
    assert(stateWithRequest.getPendingRequestByHash(request1.nodeHash).contains(request1))
  }

  it should "return enqueued elements in depth order" in new TestSetup {
    val stateWithRequests = schedulerState.schedule(request2).schedule(request3).schedule(request1).schedule(request4)
    assert(stateWithRequests != schedulerState)
    val (allMissingElements, newState) = stateWithRequests.getAllMissingHashes
    assert(allMissingElements == reqestsInDepthOrder.map(_.nodeHash))
    val (allMissingElements1, _) = newState.getAllMissingHashes
    assert(allMissingElements1.isEmpty)
  }

  it should "return at most n enqueued elements in depth order" in new TestSetup {
    val stateWithRequests = schedulerState.schedule(request2).schedule(request3).schedule(request1).schedule(request4)
    assert(stateWithRequests != schedulerState)
    val (twoMissingElements, newState) = stateWithRequests.getMissingHashes(2)
    assert(twoMissingElements == reqestsInDepthOrder.take(2).map(_.nodeHash))
    val (allMissingElements1, _) = newState.getAllMissingHashes
    assert(allMissingElements1.size == 2)
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    val schedulerState: SchedulerState = SchedulerState()
    val request1: StateNodeRequest = StateNodeRequest(ByteString(1), None, StateNode, Seq(), 1, 0)
    val request2: StateNodeRequest = StateNodeRequest(ByteString(2), None, StateNode, Seq(), 2, 0)
    val request3: StateNodeRequest = StateNodeRequest(ByteString(3), None, StateNode, Seq(), 3, 0)
    val request4: StateNodeRequest = StateNodeRequest(ByteString(4), None, StateNode, Seq(), 4, 0)

    val reqestsInDepthOrder: List[StateNodeRequest] = List(request4, request3, request2, request1)
  }
}
