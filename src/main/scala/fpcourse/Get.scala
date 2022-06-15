package fpcourse

import cats._
import cats.implicits._
import cats.kernel.laws.IsEq
import org.scalacheck._
import cats.laws._

import java.nio.{ByteBuffer, ByteOrder}

/**
 * The Get monad parses values from a list of bytes, keeping track of the
 * remaining input after each operation.
 *
 * The run function reads and consumes the bytes needed to construct a value of A.
 * If there is any issue (i.e: insufficient input) it should signal it via a Left result.
 * If everything goes ok, it should return the remaining input along with the parsed value.
 *
 * For more information of a real implementation for Haskell, check out:
 * https://wiki.haskell.org/Dealing_with_binary_data
 */
case class Get[A](run: List[Byte] => Either[String, (List[Byte], A)])

object Get {
  /**
   * TODO 1
   * Consumes n bytes of input parsing no value.
   */
//  def skip(n: Int): Get[Unit] = Applicative[Get].pure[Unit](())

  def skip(n: Int): Get[Unit] = Get[Unit](ls =>
    Either.cond(
      test = ls.length >= n,
      right = (ls.drop(n), ()),
      left = "Insufficient input"
    )
  )

  /**
   * TODO 2
   * True if the input is fully consumed
   */
  def isEmpty: Get[Boolean] = Get[Boolean](ls => Right((ls, ls.isEmpty)))

  /**
   * TODO 3
   * Reads one byte from input
   */
  def getByte: Get[Byte] = Get[Byte] {
    case Nil => Left("Insufficient input")
    case x :: xs => Right(xs, x)
  }

  /**
   * TODO 4
   * Reads an Int from input using Big Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  def getIntBE: Get[Int] = Applicative[Get].replicateA(4, getByte).map(bList => bytesToIntUnsafe(bList.toArray, ByteOrder.BIG_ENDIAN))
  /**
   * TODO 5
   * Reads an Int from input using Little Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  def getIntLE: Get[Int] = Applicative[Get].replicateA(4, getByte).map(bList => bytesToIntUnsafe(bList.toArray, ByteOrder.LITTLE_ENDIAN))

  /**
   * TODO 6
   * Reads a String of n characters from input.
   */
  def getString(n: Int): Get[String] = Applicative[Get].replicateA(n, getByte).map(_.map(_.toChar).mkString)

  /**
   * Helper function that turns four bytes into an Int. It doesn't check the
   * length of the array, so please make sure to provide 4 bytes.
   */
  private def bytesToIntUnsafe(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }

  /**
   * TODO 7
   * Instance of monad error for Get.
   */
  implicit val monadGet: MonadError[Get, String] = new MonadError[Get, String] {
    override def flatMap[A, B](fa: Get[A])(f: A => Get[B]): Get[B] = Get { bytes =>
      fa.run(bytes).flatMap{case (newBytes, x) => f(x).run(newBytes)}
    }

    override def pure[A](x: A): Get[A] = Get(ls => Right((ls, x)))

    override def tailRecM[A, B](a: A)(f: A => Get[Either[A, B]]): Get[B] = {
      Get { bytes =>
        Monad[Either[String, *]].tailRecM((bytes, a)) { case (bytes, a) =>
          f(a).run(bytes).map { case (bytes, eab) =>
            eab match {
              case Right(b) => Right((bytes, b))
              case Left(a) => Left((bytes, a))
            }
          }
        }
      }
    }

    override def raiseError[A](e: String): Get[A] = Get (_ => Left(e))

    override def handleErrorWith[A](fa: Get[A])(f: String => Get[A]): Get[A] = Get { bytes =>
      fa.run(bytes).handleErrorWith{str =>
        f(str).run(bytes)
      }
    }
  }

  /**
   * TODO 8
   * Instance of Eq for Get. A full comparison is impossible, so we just
   * compare on a given number of List[Byte] samples and assume that
   * if both Get compute the same result, they are equal.
   *
   * Hint: One possible way of doing this is to use scalacheck to build
   * a generator of List[Byte], then sample it several times (e.g. 32)
   * and check that running both Gets yields the same result every time.
   */
  implicit def eqGet[A: Eq]: Eq[Get[A]] = new Eq[Get[A]] {
    override def eqv(x: Get[A], y: Get[A]): Boolean = {
      val arbByteList = Arbitrary.arbitrary[List[Byte]]
      val eqList = for {
        i <- 1 to 32
        bList <- arbByteList.sample
      } yield x.run(bList) === y.run(bList)
      eqList.forall(identity)
    }
    }

  /**
   * TODO 9
   * Monoid instance for Get.
   */
  implicit def monoid[A: Monoid]: Monoid[Get[A]] = new Monoid[Get[A]] {
    /**
     * Read the docs for combine and come up with an instance that does not
     * alter the behaviour of any Get it is combined with.
     *
     * Think about what should happen to the input bytes, and what would be a
     * suitable result.
     */
    override def empty: Get[A] = Get { bytes =>
      Right((bytes, Monoid[A].empty))
    }

    /**
     * Combining two Get[A] instances should yield a new Get[A] instance which
     * runs both Gets in sequence and yields the combined result.
     *
     * If any of the Gets fails, the combined Get should fail with that same error.
     *
     * Check the tests for details.
     */
    override def combine(x: Get[A], y: Get[A]): Get[A] = Get { bytes =>
      x.run(bytes).flatMap{ case (bytes2, a) =>
        y.run(bytes2).map{ case (bytes3, b) =>
          (bytes3, Monoid[A].combine(a, b))
        }
      }
    }
  }
}