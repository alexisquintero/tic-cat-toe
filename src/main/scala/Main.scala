import cats.Eq

import cats.effect.{ IO, IOApp, ExitCode, Sync }

import Positions._
import Errors._
import Domain._
import Game._
import Board._
import TypeClasses.{ Console, IntRandom }
import TypeClasses.GameIO._

object Main extends IOApp {
  import cats.instances.char.catsStdShowForChar
  import cats.instances.char.catsKernelStdOrderForChar
  import cats.instances.string.catsStdShowForString

  val symbols: Symbols[Char] = Symbols('x', 'o', ' ')
  val initialPlayer: Player = Human
  val initialBoard: Board[Char] = Board(symbols)

  def errorHandling[F[_]: Sync: Console: IntRandom, A: Eq](board: Board[A], player: Player): F[Unit] = {
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    import cats.syntax.applicativeError._

    for {
      _ <- Console[F].putStrLn(s"Posible positions: ${Position.showAllPositions}")
      _ <- Game.gameLoop[F, A](board, player).handleErrorWith {
             case error: FatalError =>
               error match {
                 case PlayerIsCoward    => Console[F].putStrLn(PlayerIsCoward.toString)
                 case EmptyValidMoves   => Console[F].putStrLn(EmptyValidMoves.toString)
               }
             case e => Console[F].putStrLn(s"Catastrophic failure: $e")
           }
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- errorHandling[IO, Char](initialBoard, initialPlayer)
    } yield (ExitCode.Success)
}
