package Game

import cats.Eq
import cats.data.{ WriterT, NonEmptyChain }

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

  def humanAction[F[_]: Sync, A](board: Board[A]): F[Board[A]] = {
      for {
        _        <- board.validPositions[F]
        position <- readPosition[F]()
        newBoard <- board.move(position, Human)
      } yield newBoard
  }.recoverWith {
    case InvalidMove => Sync[F].delay(println("Invalid move")) >> humanAction(board)
  }

  // TODO: pick random from available moves, stop enemy from winning
  def cpuAction[F[_]: Sync, A: Eq](board: Board[A]): F[Board[A]] =
    for {
      _              <- Sync[F].delay(println("Cpu move"))
      validPositions <- board.validPositions[F]
      nextBoards     <- validPositions
                          .traverse[F, (Board[A], Boolean)](position => board.move(position, Cpu)
                          .map(board => (board, endCondition(board))))
      (win, cont)    = nextBoards.toList.partition(_._2)
      newBoard       = win.headOption match {
                         case Some(value) => value._1
                         case None => cont.head._1
                       }
    } yield newBoard

  def endCondition[A: Eq](board: Board[A]): Boolean = {
    import cats.instances.int.catsKernelStdOrderForInt
    import cats.syntax.eq._

    Position.allPaths
      .map(_.map(board.get).toSet)
      .filter(s => s.size === 1 && ! s.contains(board.symbols.empty))
      .nonEmpty
  }

  def moveLoop[F[_]: Sync, A: Eq](board: Board[A], player: Player): F[Board[A]] = {
    Sync[F].pure(println(board.show))
    player match {
      case Human => Game.humanAction[F, A](board)
      case Cpu   => Game.cpuAction[F, A](board)
    }
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

    def gameWriter(board: Board[A], player: Player): WriterT[F, NonEmptyChain[Board[A]], Board[A]] =
      for {
        newBoard <- WriterT.putT(moveLoop(board, player))(NonEmptyChain.one(board))
        end      =  endCondition(newBoard)
        _        <- if (end) WriterT.tell(NonEmptyChain.one(newBoard))
                    else gameWriter(newBoard, changePlayer(player))
      } yield newBoard

    for {
      writer <- gameWriter(board, player).run
      (log, newBoard) = writer
      _ <- Sync[F].delay(log.map(board => print(board.show)))
    } yield ()
  }
}
