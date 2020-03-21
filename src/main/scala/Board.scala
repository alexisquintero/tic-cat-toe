package Board

import cats.{ Show, Eq }

import cats.data.NonEmptyList

import cats.effect.Sync

import Domain._
import Positions._
import Errors._

case class Board[A: Show: Eq](
  topLeft: A,
  topMiddle: A,
  topRight: A,
  middleLeft: A,
  middleMiddle: A,
  middleRight: A,
  bottomLeft: A,
  bottomMiddle: A,
  bottomRight: A,
  symbols: Symbols[A]
) {

  import cats.syntax.eq._

  def get(position: Position): A =
    position match {
      case TopLeft      => topLeft
      case TopMiddle    => topMiddle
      case TopRight     => topRight
      case MiddleLeft   => middleLeft
      case MiddleMiddle => middleMiddle
      case MiddleRight  => middleRight
      case BottomLeft   => bottomLeft
      case BottomMiddle => bottomMiddle
      case BottomRight  => bottomRight
    }

  def updateBoard(position: Position, player: Player): Board[A] =
    position match {
      case TopLeft      => this.copy(topLeft = symbols.playerSymbol(player))
      case TopMiddle    => this.copy(topMiddle = symbols.playerSymbol(player))
      case TopRight     => this.copy(topRight = symbols.playerSymbol(player))
      case MiddleLeft   => this.copy(middleLeft = symbols.playerSymbol(player))
      case MiddleMiddle => this.copy(middleMiddle = symbols.playerSymbol(player))
      case MiddleRight  => this.copy(middleRight = symbols.playerSymbol(player))
      case BottomLeft   => this.copy(bottomLeft = symbols.playerSymbol(player))
      case BottomMiddle => this.copy(bottomMiddle = symbols.playerSymbol(player))
      case BottomRight  => this.copy(bottomRight = symbols.playerSymbol(player))
    }

  implicit def showBoard(): Show[Board[A]] =
    Show.show { b =>
      def showA(a: A): String =
        Show[A].show(a)
      def posString(position: Position): String =
        showA(b.get(position))
      s"""
        +-----+
        |${posString(TopLeft)}|${posString(TopMiddle)}|${posString(TopRight)}|
        +-----+
        |${posString(MiddleLeft)}|${posString(MiddleMiddle)}|${posString(MiddleRight)}|
        +-----+
        |${posString(BottomLeft)}|${posString(BottomMiddle)}|${posString(BottomRight)}|
        +-----+
        """
    }

    def show(): String =
      Show[Board[A]](showBoard).show(this)

    def move[F[_]: Sync](position: Position, player: Player): F[Board[A]] =
        if (get(position) == symbols.empty) Sync[F].delay(updateBoard(position, player))
        else Sync[F].raiseError(InvalidMove)

    def validPositions[F[_]: Sync](): F[NonEmptyList[Position]] =
      NonEmptyList.fromList(Position.allPositions.filter(get(_) === symbols.empty)) match {
        case None => Sync[F].raiseError(EmptyValidMoves)
        case Some(value) => Sync[F].pure(value)
      }
}

case object Board {
  def apply[A: Show: Eq](symbols: Symbols[A]): Board[A] =
    Board(
      symbols.empty,
      symbols.empty,
      symbols.empty,
      symbols.empty,
      symbols.empty,
      symbols.empty,
      symbols.empty,
      symbols.empty,
      symbols.empty,
      symbols)
}
