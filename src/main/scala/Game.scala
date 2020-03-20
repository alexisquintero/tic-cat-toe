package Game

import cats.Eq

import cats.effect.Sync

import Domain._
import Positions._
import Errors._
import Board._

case object Game {
  import scala.io.StdIn

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.applicativeError._

  def checkCoward[F[_]: Sync, S: Eq](input: S, endValue: S): F[Unit] = {
    import cats.syntax.eq._

    if (input === endValue) Sync[F].raiseError(PlayerIsCoward)
    else Sync[F].unit
  }

  def inputToPosition[F[_]: Sync](input: String): F[Position] =
      Position.fromString(input) match {
        case None => Sync[F].raiseError(InvalidMove)
        case Some(position) => Sync[F].pure(position)
      }

  def readPosition[F[_]: Sync](): F[Position] = {
    import cats.syntax.apply._
    import cats.instances.string.catsKernelStdOrderForString

    for {
      _        <- Sync[F].delay(print("Your move: "))
      input    <- Sync[F].delay(StdIn.readLine.toLowerCase)
      position <- checkCoward(input, "exit") *> inputToPosition(input)
    } yield position
  }

  def humanAction[F[_]: Sync, A](board: Board[A]): F[Board[A]] =
      for {
        _        <- board.validPositions[F]
        position <- readPosition[F]()
        newBoard <- board.move(position, Human)
      } yield newBoard

  // TODO: check if can win else pick empty non game terminating place else defeat
  def cpuAction[F[_]: Sync, A: Eq](board: Board[A]): F[Board[A]] =
    for {
      _        <- Sync[F].delay(println("Cpu move"))
      validPositions <- board.validPositions[F]
      newBoard <- board.move(validPositions.head, Cpu)
    } yield newBoard

  // How can this be so ugly?
  def endCondition[A: Eq](board: Board[A]): Boolean = {
    import cats.instances.int.catsKernelStdOrderForInt
    import cats.syntax.eq._

    val pathVals: List[List[A]] = Position.allPaths.map(path => path.map(board.get))
    val pathSet: List[Set[A]] = pathVals.map(pathVal => pathVal.toSet)
    val nonEmptyPathSize: List[Int] = pathSet.filter(_.exists(_ =!= board.symbols.empty)).map(_.size)
    nonEmptyPathSize.exists(_ === 1)
  }

  def moveLoop[F[_]: Sync, A: Eq](board: Board[A], player: Player): F[Board[A]] =
    player match {
      case Human => Game.humanAction[F, A](board)
      case Cpu   => Game.cpuAction[F, A](board)
    }

  def changePlayer(player: Player): Player =
    player match {
      case Cpu   => Human
      case Human => Cpu
    }

  def showWinner[F[_]: Sync, A](board: Board[A], player: Player): F[Unit] =
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
      case error: BoardError =>
        error match {
          case InvalidMove =>
            for {
              _ <- Sync[F].delay(println("Invalid move"))
              _ <- gameLoop(board, player)
            } yield ()
        }
    }
  }
}
