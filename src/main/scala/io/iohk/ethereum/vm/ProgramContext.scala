package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.Account

/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param env set of constants for the execution
  * @param startGas initial gas for the execution
  * @param storage representation of the storage accociated with the contract to be executed
  * @param account this contract's account
  * @param accounts used to retrieve an account's code and storage
  */
case class ProgramContext(
  env: ExecEnv,
  startGas: BigInt,
  storage: Storage,
  account: Account,
  accounts: AccountRetriever)
