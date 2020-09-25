package io.iohk.ethereum

import cats.effect.implicits._
import cats.effect.{Bracket, Effect, Resource}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.freespec.AsyncFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpecLike

trait SpecBase extends TypeCheckedTripleEquals with Diagrams with Matchers { self: AsyncTestSuite =>

  override val executionContext = ExecutionContext.global
  implicit val scheduler: Scheduler = Scheduler(executionContext)

  def customTestCaseResourceM[M[_]: Effect, T](
      fixture: Resource[M, T]
  )(theTest: T => M[Assertion])(implicit bracket: Bracket[M, Throwable]): Future[Assertion] = {
    fixture.use(theTest).toIO.unsafeToFuture()
  }

  def customTestCaseResourceF[T](fixture: Resource[Task, T])(theTest: T => Future[Assertion]): Future[Assertion] =
    customTestCaseResourceM(fixture)(f => Task.deferFuture(theTest(f)))

  def customTestCaseM[M[_]: Effect, T](fixture: => T)(theTest: T => M[Assertion]): Future[Assertion] =
    customTestCaseResourceM(Resource.pure[M, T](fixture))(theTest)

  def customTestCaseF[T](fixture: => T)(theTest: T => Future[Assertion]): Future[Assertion] =
    customTestCaseResourceF(Resource.pure[Task, T](fixture))(theTest)

  def testCaseM[M[_]: Effect](theTest: => M[Assertion]): Future[Assertion] = customTestCaseM(())(_ => theTest)

  def testCaseF(theTest: => Future[Assertion]): Future[Assertion] = customTestCaseF(())(_ => theTest)

  def testCase(theTest: => Assertion): Future[Assertion] = testCaseM(Task.pure(theTest))
}

trait FlatSpecBase extends AsyncFlatSpecLike with SpecBase {}

trait FreeSpecBase extends AsyncFreeSpecLike with SpecBase {}

trait WordSpecBase extends AsyncWordSpecLike with SpecBase {}

trait SpecFixtures { self: SpecBase =>
  type Fixture

  def createFixture(): Fixture

  def testCaseM[M[_]: Effect](theTest: Fixture => M[Assertion]): Future[Assertion] =
    customTestCaseM(createFixture())(theTest)

  def testCaseF(theTest: Fixture => Future[Assertion]): Future[Assertion] = customTestCaseF(createFixture())(theTest)

  def testCase(theTest: Fixture => Assertion): Future[Assertion] =
    testCaseM((fixture: Fixture) => Task.pure(theTest(fixture)))
}

trait ResourceFixtures { self: SpecBase =>
  type Fixture

  def fixtureResource: Resource[Task, Fixture]

  def testCaseM[M[_]: Effect](theTest: Fixture => M[Assertion]): Future[Assertion] =
    customTestCaseResourceM(fixtureResource.mapK(Task.liftTo[M]))(theTest)

  /**
    * Task-specific method to avoid type inference issues in [[testCaseM]]
    */
  def testCaseT(theTest: Fixture => Task[Assertion]): Future[Assertion] =
    customTestCaseResourceM(fixtureResource)(theTest)

  def testCaseF(theTest: Fixture => Future[Assertion]): Future[Assertion] =
    customTestCaseResourceF(fixtureResource)(theTest)

  def testCase(theTest: Fixture => Assertion): Future[Assertion] =
    customTestCaseResourceM(fixtureResource)(fixture => Task.pure(theTest(fixture)))
}
