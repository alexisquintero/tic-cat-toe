package Positions

sealed trait Position

object Position {
  import cats.syntax.option._
  import cats.Foldable
  import cats.instances.list._
  import cats.instances.string._

  val rows: List[List[Position]] =
    List(
      List(TopLeft, TopMiddle, TopRight),
      List(MiddleLeft, MiddleMiddle, MiddleRight),
      List(BottomLeft, BottomMiddle, BottomRight))

  val columns: List[List[Position]] =
    List(
      List(TopLeft, MiddleLeft, BottomLeft),
      List(TopMiddle, MiddleMiddle, BottomMiddle),
      List(TopRight, MiddleRight, BottomRight))

  val diagonals: List[List[Position]] =
    List(
      List(TopLeft, MiddleMiddle, BottomRight),
      List(TopRight, MiddleMiddle, BottomLeft))

  val allPaths: List[List[Position]] =
    (rows ++ columns ++ diagonals)

  val allPositions: List[Position] =
    List(TopLeft, TopMiddle, TopRight, MiddleLeft, MiddleMiddle, MiddleRight, BottomLeft, BottomMiddle, BottomRight)

  def showAllPositions(): String =
    Foldable[List].intercalate(allPositions.map(_.toString), ", ")

  def fromString(s: String): Option[Position] =
    s.toLowerCase match {
      case "topleft"      => TopLeft.some
      case "topmiddle"    => TopMiddle.some
      case "topright"     => TopRight.some
      case "middleleft"   => MiddleLeft.some
      case "middlemiddle" => MiddleMiddle.some
      case "middleright"  => MiddleRight.some
      case "bottomleft"   => BottomLeft.some
      case "bottommiddle" => BottomMiddle.some
      case "bottomright"  => BottomRight.some
      case _              => none
    }
}

case object TopLeft      extends Position
case object TopMiddle    extends Position
case object TopRight     extends Position
case object MiddleLeft   extends Position
case object MiddleMiddle extends Position
case object MiddleRight  extends Position
case object BottomLeft   extends Position
case object BottomMiddle extends Position
case object BottomRight  extends Position
