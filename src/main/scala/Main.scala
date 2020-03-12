import cats.Show

import cats.effect.{ IO, IOApp, ExitCode, Sync }
import cats.effect.concurrent.Ref

object Main extends IOApp {

  implicit val showChar: Show[Char] =
    Show.show { c =>
      c.toString
    }

  def run(args: List[String]): IO[ExitCode] =
    for {
      board <- IO(Board(List('q', 'w', 'e', 'a', 'b', 'd', 'z', 'x', 'c')))
      Game  <- Ref.of[IO, Board[Char]](board)
      // _     <- Game.showBoard[IO](board)
      // x     <- Game.humanAction[IO]
      // y     <- Game.cpuAction[IO]
    } yield (ExitCode.Success)
}

trait Position

case object TopLeft      extends Position
case object TopMiddle    extends Position
case object TopRight     extends Position
case object MiddleLeft   extends Position
case object MiddleMiddle extends Position
case object MiddleRight  extends Position
case object BottomLeft   extends Position
case object BottomMiddle extends Position
case object BottomRight  extends Position

case class Board[A: Show](positions: Map[Position, A]) {
}

case object Board {

  def apply[A: Show](initialValues: List[A]): Board[A] = {
    val allPositions: List[Position] =
      List(TopLeft, TopMiddle, TopRight, MiddleLeft, MiddleMiddle, MiddleRight, BottomLeft, BottomMiddle, BottomRight)
    val m: Map[Position, A] = (allPositions zip initialValues).toMap
    Board(m)
  }
}

case object Game {
  import scala.io.StdIn

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def showBoard[F[_]: Sync](board: Ref[F, Vector[Vector[Char]]]): F[Unit] =
    for {
      pieces <- board.get
      _ <- Sync[F].delay(println(pieces))
    } yield ()

  def humanAction[F[_]: Sync](): F[Unit] =
      for {
        _ <- Sync[F].delay(println("Your move: "))
        ans <- Sync[F].delay(StdIn.readLine)
        _ <- Sync[F].delay(println(ans))
      } yield ()

  def cpuAction[F[_]: Sync](): F[Unit] =
    for {
      _ <- Sync[F].delay(println("My move: "))
      _ <- Sync[F].delay(println("move"))
    } yield ()
}
