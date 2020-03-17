import org.scalacheck.Prop.{ forAll, propBoolean }
import org.scalacheck.Gen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

import cats.syntax.option._

import Positions._

case class PositionsSpec() extends AnyFlatSpec with Checkers with Matchers {
  val positionGen =
    Gen.oneOf(Position.allPositions)

  "fromString" should "parse valid Positions" in {
    check {
      forAll(positionGen) { position =>
        Position.fromString(position.toString.toLowerCase) == position.some
      }
    }
  }

  "fromString" should "return none with invalid Positions" in {
    check {
      val validPositions: List[String] = Position.allPositions.map(_.toString)
      val nonValidPositions =
        Gen.alphaStr suchThat (position => validPositions.exists(_ != position))

      forAll(nonValidPositions) { position =>
        (Position.fromString(position.toString.toLowerCase) == none)
      }
    }
  }
}
