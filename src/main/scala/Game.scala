package Game

import cats.Eq
import cats.data.{ WriterT, NonEmptyChain }

import cats.effect.Sync

import Domain._
import Positions._
import Errors._
import Board._
import TypeClasses.{ Console, IntRandom }

case object Game {
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.applicativeError._

  import cats.instances.string.catsStdShowForString

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

  def readPosition[F[_]: Sync: Console](): F[Position] = {
    import cats.syntax.apply._
    import cats.instances.string.catsKernelStdOrderForString

    for {
      _        <- Console[F].putStr("Your move: ")
      input    <- Console[F].readLn.map(_.toLowerCase)
      position <- checkCoward(input, "exit") *> inputToPosition(input)
    } yield position
  }

  def humanAction[F[_]: Sync: Console, A](board: Board[A]): F[Board[A]] = {
      for {
        _        <- board.validPositions[F]
        position <- readPosition[F]()
        newBoard <- board.move(position, Human)
      } yield newBoard
  }.recoverWith {
    case InvalidMove => Console[F].putStrLn("Invalid move") >> humanAction(board)
  }

  def cpuAction[F[_]: Sync: Console: IntRandom, A: Eq](board: Board[A]): F[Board[A]] =
    for {
      _              <- Console[F].putStrLn("Cpu move")
      validPositions <- board.validPositions[F]
      nextBoards     <- validPositions
                          .traverse[F, (Board[A], Boolean)](position => board.move(position, Cpu)
                          .map(board => (board, endCondition(board))))
      (win, cont)    = nextBoards.toList.partition(_._2)
      possibleBoard  <- implicitly[IntRandom[F]].next(cont.size)
      newBoard       = win.headOption match {
                         case Some(value) => value._1
                         case None => cont(possibleBoard)._1
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

  def moveLoop[F[_]: Sync: Console: IntRandom, A: Eq](board: Board[A], player: Player): F[Board[A]] =
    for {
      _ <- Console[F].putStrLn(board.show)
      newBoard <- player match {
        case Human => Game.humanAction[F, A](board)
        case Cpu   => Game.cpuAction[F, A](board)
      }
    } yield newBoard

  def changePlayer(player: Player): Player =
    player match {
      case Cpu   => Human
      case Human => Cpu
    }

  def showWinner[F[_]: Sync: Console, A](board: Board[A], player: Player): F[Unit] =
    for {
      _ <- Console[F].putStrLn(s"Winner: ${player.toString}")
      _ <- Console[F].putStrLn(board.show)
    } yield ()

  def gameLoop[F[_]: Sync: Console: IntRandom, A: Eq](board: Board[A], player: Player): F[Unit] = {
    import cats.syntax.traverse._

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
      _ <- log.traverse(l => Console[F].putStrLn(l.show))
    } yield ()
  }
}
