package io.iohk.ethereum.vm

import akka.util.ByteString

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import Generators._

class ProgramSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  val CodeSize = Byte.MaxValue
  val PositionsSize = 10

  val nonPushOp: Byte = JUMP.code
  val invalidOpCode: Byte = 0xef.toByte

  def positionsSetGen: Gen[Set[Int]] =
    getListGen(minSize = 0, maxSize = PositionsSize, genT = intGen(0, CodeSize)).map(_.toSet)

  it should "detect all jump destinations if there are no push op" in {
    forAll(positionsSetGen) { jumpDestLocations =>
      val code = ByteString((0 to CodeSize).map { i =>
        if (jumpDestLocations.contains(i)) JUMPDEST.code
        else nonPushOp
      }.toArray)
      val program = Program(code)
      program.validJumpDestinations shouldBe jumpDestLocations
    }
  }

  it should "detect all jump destinations if there are push op" in {
    forAll(positionsSetGen, positionsSetGen) { (jumpDestLocations, pushOpLocations) =>
      val code = ByteString((0 to CodeSize).map { i =>
        if (jumpDestLocations.contains(i)) JUMPDEST.code
        else if (pushOpLocations.contains(i)) PUSH1.code
        else nonPushOp
      }.toArray)
      val program = Program(code)

      //Removing the PUSH1 that would be used as a parameter of another PUSH1
      //  Example: In "PUSH1 PUSH1 JUMPDEST", the JUMPDEST is a valid jump destination
      val pushOpLocationsNotParameters = pushOpLocations
        .diff(jumpDestLocations)
        .toList
        .sorted
        .foldLeft(List.empty[Int]) { case (recPushOpLocations, i) =>
          if (recPushOpLocations.lastOption.contains(i - 1)) recPushOpLocations else recPushOpLocations :+ i
        }

      val jumpDestLocationsWithoutPushBefore = jumpDestLocations
        .filterNot(i => pushOpLocationsNotParameters.contains(i - 1))
        .filter(i => 0 <= i && i <= CodeSize)
      program.validJumpDestinations shouldBe jumpDestLocationsWithoutPushBefore
    }
  }

  it should "detect all jump destinations if there are invalid ops" in {
    forAll(positionsSetGen, positionsSetGen) { (jumpDestLocations, invalidOpLocations) =>
      val code = ByteString((0 to CodeSize).map { i =>
        if (jumpDestLocations.contains(i)) JUMPDEST.code
        else if (invalidOpLocations.contains(i)) invalidOpCode
        else nonPushOp
      }.toArray)
      val program = Program(code)
      program.validJumpDestinations shouldBe jumpDestLocations
    }
  }

  it should "detect all instructions as jump destinations if they are" in {
    val code = ByteString((0 to CodeSize).map(_ => JUMPDEST.code).toArray)
    val program = Program(code)
    program.validJumpDestinations shouldBe (0 to CodeSize).toSet
  }
}
