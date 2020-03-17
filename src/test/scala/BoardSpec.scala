import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

import cats.effect.IO

import Board._
import Domain.Symbols
import Domain._
import Positions._

case class BoardSpec() extends AnyFlatSpec with Checkers with Matchers {

  import cats.instances.char.catsStdShowForChar
  import cats.instances.char.catsKernelStdOrderForChar

  val emptyBoardCharGen =
    for {
      human <- Gen.alphaChar
      cpu   <- Gen.alphaChar
      empty <- Gen.alphaChar
      players = Map[Player, Char](Human -> human, Cpu -> cpu)
      symbols = Symbols(players, empty)
      if (human != cpu) && (human != empty) && (cpu != empty)
    } yield Board[Char](symbols)

  "validPositions with char" should "return all valid positions for this board" in {
    check {
      forAll(emptyBoardCharGen) { board =>
        board.validPositions[IO].unsafeRunSync.size == Position.allPositions.size
      }
    }
  }

  import cats.instances.string.catsStdShowForString
  import cats.instances.string.catsKernelStdOrderForString

  val emptyBoardStringGen =
    for {
      human <- Gen.alphaStr
      cpu   <- Gen.alphaStr
      empty <- Gen.alphaStr
      players = Map[Player, String](Human -> human, Cpu -> cpu)
      symbols = Symbols(players, empty)
      if (human != cpu) && (human != empty) && (cpu != empty)
    } yield Board[String](symbols)

  "validPositions with string" should "return all valid positions for this board" in {
    check {
      forAll(emptyBoardStringGen) { board =>
        board.validPositions[IO].unsafeRunSync.size == Position.allPositions.size
      }
    }
  }

  import cats.instances.int.catsStdShowForInt
  import cats.instances.int.catsKernelStdOrderForInt

  val emptyBoardPosIntGen =
    for {
      human <- Gen.posNum[Int]
      cpu   <- Gen.posNum[Int]
      empty <- Gen.posNum[Int]
      players = Map[Player, Int](Human -> human, Cpu -> cpu)
      symbols = Symbols(players, empty)
      if (human != cpu) && (human != empty) && (cpu != empty)
    } yield Board[Int](symbols)

  "validPositions with positive integers" should "return all valid positions for this board" in {
    check {
      forAll(emptyBoardPosIntGen) { board =>
        board.validPositions[IO].unsafeRunSync.size == Position.allPositions.size
      }
    }
  }
}
