package io.iohk.ethereum.cli

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CliCommandsSpec extends AnyFlatSpec with Matchers with EitherValues {

  import CliCommands._
  import Fixture._

  behavior of generatePrivateKeyCommand
  it should "generate correct private key" in {
    api.parse(Seq(generatePrivateKeyCommand)) shouldBe a[Right[_, _]]
  }

  behavior of deriveAddressCommand
  it should "derive address from private key" in {
    api.parse(Seq(deriveAddressCommand, privateKey)).right.value shouldBe address
  }

  it should "return an error when called without private key" in {
    api.parse(Seq(deriveAddressCommand)) shouldBe a[Left[_, _]]
  }

  behavior of generateAllocsCommand
  it should "generate correct alloc using private key" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(keyOption, Some(privateKey)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .right
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }}"""
  }

  it should "generate more than one alloc" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(keyOption, Some(privateKey)),
          argument(keyOption, Some(privateKey2)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .right
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }, $address2: { "balance": $requestedBalance }}"""
  }

  it should "generate allocs using addresses" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(addressOption, Some(address)),
          argument(addressOption, Some(address2)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .right
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }, $address2: { "balance": $requestedBalance }}"""
  }

  it should "generate allocs using both keys and addresses" in {
    api
      .parse(
        Seq(
          generateAllocsCommand,
          argument(addressOption, Some(address)),
          argument(keyOption, Some(privateKey2)),
          argument(addressOption, Some(address3)),
          argument(balanceOption, Some(requestedBalance))
        )
      )
      .right
      .value shouldBe s""""alloc": {$address: { "balance": $requestedBalance }, $address3: { "balance": $requestedBalance }, $address2: { "balance": $requestedBalance }}"""
  }

}

object Fixture {

  def argument(name: String, value: Option[Any] = None): String = s"--$name${value.fold("")(v => s"=${v.toString}")}"

  val privateKey = "00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d0"
  val privateKey2 = "00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d1"
  val privateKey3 = "00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d2"

  val address = "8b196738d90cf3d9fc299e0ec28e15ebdcbb0bdcb281d9d5084182c9c66d5d12"
  val address2 = "add8c627e14480b36b30811758240d8acb282aae883043990d8a2d7e2e75cf3b"
  val address3 = "1e9cd60cf3b2c902e60f809e604542f9a9fb55d3e8004ff122f662f88eb32b4a"

  val requestedBalance = 42

}
