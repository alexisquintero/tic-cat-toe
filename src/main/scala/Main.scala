import cats.Eq

import cats.effect.{ IO, IOApp, ExitCode, Sync }

import Positions._
import Errors._
import Domain._
import Game._
import Board._

object Main extends IOApp {
  import cats.instances.char.catsStdShowForChar
  import cats.instances.char.catsKernelStdOrderForChar

  val symbols: Symbols[Char] = Symbols('x', 'o', ' ')
  val initialPlayer: Player = Human
  val initialBoard: Board[Char] = Board(symbols)

  def errorHandling[F[_]: Sync, A: Eq](board: Board[A], player: Player): F[Unit] = {
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    import cats.syntax.applicativeError._

    for {
      _ <- Sync[F].delay(println(s"Posible positions: ${Position.showAllPositions}"))
      _ <- Game.gameLoop[F, A](board, player).handleErrorWith {
             case error: FatalError =>
               error match {
                 case PlayerIsCoward    => Sync[F].pure(println(PlayerIsCoward.toString))
                 case EmptyValidMoves   => Sync[F].pure(println(EmptyValidMoves.toString))
               }
             case e => Sync[F].delay(println(s"Catastrophic failure: $e"))
           }
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- errorHandling[IO, Char](initialBoard, initialPlayer)
    } yield (ExitCode.Success)
}
