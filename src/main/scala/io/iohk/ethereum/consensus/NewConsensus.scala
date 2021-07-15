package io.iohk.ethereum.consensus

import io.iohk.ethereum.db.storage.ChainWeightStorage
import io.iohk.ethereum.domain.{Block, Blockchain, BlockchainReader}
import io.iohk.ethereum.domain.branch.BlockchainBranch
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockImport}

// TODO do we want metrics? How often an attempt to switch to a new branch fails?

case class Branch(blocks: List[Block])

// For now we work only with the canonical (best) branch and later add the possibility of working with any branch
class NewConsensus(
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    executingSync: ExecutingSync,
    blockimport: BlockImport,
    chainWeightStorage: ChainWeightStorage
) {

  /** Answer which branch is best
    * @return BlockchainBranch
    */
  def getBestBranch(): BlockchainBranch = blockchainReader.getBestBranch()

  /** @param branch
    * This methods received a Branch that was updated by ChainManagement.
    * When a Branch is updated we need to compare the weight of the current best branch with the
    * updated one.
    * If the current best branch is still the best then nothing needs to be done.
    * If the updated branch is heavier than an attempt to set the updated branch as best branch is done by
    * executing the blocks in the updated branch to see if it is a valid branch.
    * If it is not a valid branch then ExecutingSync has to be informed, otherwise update state with new best branch.
    */
  def updatedBranch(branch: Branch): Unit =
    if (extendsBestBranch())
      // just validate the latest block
      if (isHeavierThanBestBranch(branch)) {
        attemptToSetNewBestBranch(branch) match {
          case Right(result) => // save pointer to new best branch
          case Left(error)   => executingSync.informErrorDuringAttemptToSetNewBestBranch(error)
        }
      } else {
        // nothing
      }

  private def extendsBestBranch(): Boolean = ???

  /** Compares the weight of the updatedBranch with the weight of the current best branch
    * @param updatedBranch
    * @return true if updatedBranch is heavier than current best branch, false otherwise
    */
  private def isHeavierThanBestBranch(updatedBranch: Branch): Boolean = {
    val bestBlock = blockchainReader.getBestBlock()
    val bestHash = bestBlock.map(_.header.hash)
    val newBranchWeight = chainWeightStorage.get(???)

    bestHash
      .flatMap(blockchain.getChainWeightByHash)
      .exists(weight => newBranchWeight.totalDifficulty > weight.totalDifficulty)
  }

  /** Tries to set a new best branch by executing all blocks in the branch, from the HCB to the branch tip.
    * We assume the pre validation of the blocks of the branch was done already
    * @param branch
    * @return  Either[BlockExecutionError, Boolean]
    */
  private def attemptToSetNewBestBranch(branch: Branch): Either[BlockExecutionError, Boolean] =
    blockimport.reorganiseChainFromQueue(branch.tipOfTheBranch)

}
