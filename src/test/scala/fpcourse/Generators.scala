package fpcourse

import org.scalacheck.{Arbitrary, Gen}
import cats._
import cats.implicits._
import Get._

trait Generators {
  /**
   * TODO 10
   * Instance of Arbitrary for functions A => A.
   * No constraints on what the function does.
   * Simple solutions should be enough.
   */
  implicit def arbFun[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] = {
    val genFun = for {
      a <- arbA.arbitrary
    } yield {x: A => a}
    Arbitrary(genFun)
  }


  /**
   * TODO 11
   * Instance of Arbitrary for the Get monad.
   * Any solution should suffice.
   * Some optional constraints if you want to spice things up:
   * - allow for both successful and failed return values to be produced
   * - when running the Get successfully, the remaining bytes should be
   *   a suffix of the bytes passed as argument to the run function; that is,
   *   you should simulate actual 'consumption' of the bytes.
   */
  implicit def arbGet[A](implicit arbA: Arbitrary[A]): Arbitrary[Get[A]] = {
    def correctGet(n: Int, a: A): Get[A] = Get.skip(n).map{case () => a }
    def incorrectGet(e: String): Get[A] = MonadError[Get, String].raiseError(e)
    val genGet = for {
      a <- arbA.arbitrary
      n <- Arbitrary.arbInt.arbitrary
      e <- Arbitrary.arbString.arbitrary
      x <- Gen.oneOf(correctGet(n, a), incorrectGet(e))
    } yield x
    Arbitrary(genGet)
  }
}
