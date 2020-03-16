package Board

import cats.{ Show, Eq }

import cats.data.NonEmptyList

import cats.effect.Sync

import Domain._
import Positions._
import Errors._

case class Board[A: Show: Eq](positions: Map[Position, A], symbols: Symbols[A]) {
  import cats.syntax.eq._

  implicit def showBoard(): Show[Board[A]] =
    Show.show { b =>
      def showA(a: A): String =
        Show[A].show(a)
      def posString(position: Position): String = b.positions.get(position) match {
        case None => ""
        case Some(value) => showA(value)
      }
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

    def move[F[_]: Sync](position: Position, player: Player): F[Board[A]] = {
      import cats.syntax.eq._
      import cats.syntax.functor._
      import cats.syntax.flatMap._

      for {
        symbol <- symbols.playerSymbols.get(player) match {
                    case None => Sync[F].raiseError(PlayerHasNoSymbol)
                    case Some(value) => Sync[F].pure(value)
                  }
        empty = positions.get(position).map(_ === symbols.empty).getOrElse(false)
        newBoard <- if (empty) Sync[F].pure(this.copy(positions = this.positions + (position -> symbol)))
                    else Sync[F].raiseError(InvalidMove)
      } yield newBoard
    }

    def validPositions[F[_]: Sync](): F[NonEmptyList[Position]] = {
      NonEmptyList.fromList(positions.filter(_._2 === symbols.empty).keys.toList) match {
        case None => Sync[F].raiseError(EmptyValidMoves)
        case Some(value) => Sync[F].pure(value)
      }
    }
}

case object Board {
  def apply[A: Show: Eq](symbols: Symbols[A]): Board[A] =
    Board(
      (Position.allPositions zip List.fill(Position.allPositions.size)(symbols.empty)).toMap,
      symbols)
}
