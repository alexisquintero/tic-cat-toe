package Game

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

case object Game {
  import scala.io.StdIn

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.applicativeError._

  private def checkCoward[F[_]: Sync, S: Eq](input: S, endValue: S): F[Unit] = {
    import cats.syntax.eq._

    if (input === endValue) Sync[F].raiseError(PlayerIsCoward)
    else Sync[F].unit
  }

  private def inputToPosition[F[_]: Sync](input: String): F[Position] =
      Position.fromString(input) match {
        case None => Sync[F].raiseError(InvalidMove)
        case Some(position) => Sync[F].pure(position)
      }

  private def readPosition[F[_]: Sync](): F[Position] = {
    import cats.syntax.apply._
    import cats.instances.string.catsKernelStdOrderForString

    for {
      _        <- Sync[F].delay(print("Your move: "))
      input    <- Sync[F].delay(StdIn.readLine.toLowerCase)
      position <- checkCoward(input, "exit") *> inputToPosition(input)
    } yield position
  }

  private def humanAction[F[_]: Sync, A](board: Board[A]): F[Board[A]] =
      for {
        _        <- board.validPositions[F]
        position <- readPosition[F]()
        newBoard <- board.move(position, Human)
      } yield newBoard

  // TODO: check if can win else pick empty non game terminating place else defeat
  private def cpuAction[F[_]: Sync, A: Eq](board: Board[A]): F[Board[A]] =
    for {
      _        <- Sync[F].delay(println("Cpu move"))
      validPositions <- board.validPositions[F]
      newBoard <- board.move(validPositions.head, Cpu)
    } yield newBoard

  // How can this be so ugly?
  private def endCondition[A: Eq](board: Board[A]): Boolean = {
    import cats.instances.int.catsKernelStdOrderForInt
    import cats.syntax.eq._

    val pathVals: List[List[Option[A]]] = Position.allPaths.map(path => path.map(board.positions.get))
    val pathSet: List[Set[A]] = pathVals.map(pathVal => pathVal.flatten.toSet)
    val nonEmptyPathSize: List[Int] = pathSet.filter(_.exists(_ =!= board.symbols.empty)).map(_.size)
    nonEmptyPathSize.exists(_ === 1)
  }

  private def moveLoop[F[_]: Sync, A: Eq](board: Board[A], player: Player): F[Board[A]] =
    player match {
      case Human => Game.humanAction[F, A](board)
      case Cpu   => Game.cpuAction[F, A](board)
    }

  private def changePlayer(player: Player): Player =
    player match {
      case Cpu   => Human
      case Human => Cpu
    }

  private def showWinner[F[_]: Sync, A](board: Board[A], player: Player): F[Unit] =
    for {
      _ <- Sync[F].delay(println(s"Winner: ${player.toString}"))
      _ <- Sync[F].delay(println(board.show))
    } yield ()

  def gameLoop[F[_]: Sync, A: Eq](board: Board[A], player: Player): F[Unit] = {
    val game: F[Unit] =
      for {
        _          <- Sync[F].delay(println(board.show))
        newBoard   <- moveLoop[F, A](board, player)
        end        =  endCondition(newBoard)
        _          <- if (end) showWinner[F, A](newBoard, player)
                      else gameLoop(newBoard, changePlayer(player))
      } yield ()

    game.recoverWith {
      case InvalidMove =>
        for {
          _ <- Sync[F].delay(println("Invalid move"))
          _ <- gameLoop(board, player)
        } yield ()
    }
  }
}
