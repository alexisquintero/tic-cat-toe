import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

import cats.{ Show, Eq }

import cats.effect.{ IO, Sync }

import Board._
import Domain.Symbols
import Domain._
import Positions._

case class BoardSpec() extends AnyFlatSpec with Checkers with Matchers {

  def emptyBoardGen[A: Show: Eq](gen: Gen[A]): Gen[Board[A]] =
    for {
      human <- gen
      cpu   <- gen
      empty <- gen
      symbols = Symbols(human, cpu, empty)
      if (human != cpu) && (human != empty) && (cpu != empty)
    } yield Board[A](symbols)

  import cats.instances.char.catsStdShowForChar
  import cats.instances.char.catsKernelStdOrderForChar

  val emptyBoardCharGen: Gen[Board[Char]] =
    emptyBoardGen[Char](Gen.alphaChar)

  import cats.instances.string.catsStdShowForString
  import cats.instances.string.catsKernelStdOrderForString

  val emptyBoardStringGen: Gen[Board[String]] =
    emptyBoardGen[String](Gen.alphaNumStr)

  import cats.instances.int.catsStdShowForInt
  import cats.instances.int.catsKernelStdOrderForInt

  val emptyBoardPosIntGen: Gen[Board[Int]] =
    emptyBoardGen[Int](Gen.posNum[Int])

  def oneMoveBoardCharGen[F[_]: Sync, A](gen: Gen[Board[A]]) =
    gen.flatMap(board => board.move[F](TopLeft, Human))

  def validPositionsSpec[A](genType: String, gen: Gen[Board[A]]): Unit = {
    s"validPositions with $genType" should "return all valid positions" in {
      check {
        forAll(gen) { board =>
          board.validPositions[IO].unsafeRunSync.size == Position.allPositions.size
        }
      }
    }
  }

  validPositionsSpec("Char", emptyBoardCharGen)
  validPositionsSpec("String", emptyBoardStringGen)
  validPositionsSpec("Positive Int", emptyBoardPosIntGen)
}
