import cats.Show

import cats.effect.{ IO, IOApp, ExitCode, Sync }
import cats.effect.concurrent.Ref

object Main extends IOApp {
  import cats.instances.char._

  def run(args: List[String]): IO[ExitCode] =
    for {
      initialValues <- IO(List('q', 'w', 'e', 'a', 'b', 'd', 'z', 'x', 'c'))
      board <- IO(Board(initialValues))
      game  <- Game.newGame[IO, Char](board)
      _     <- Game.showBoard[IO, Char](board)
      // x     <- Game.humanAction[IO]
      // y     <- Game.cpuAction[IO]
    } yield (ExitCode.Success)
}

trait Position

object Position {
  val allPositions: List[Position] =
    List(TopLeft, TopMiddle, TopRight, MiddleLeft, MiddleMiddle, MiddleRight, BottomLeft, BottomMiddle, BottomRight)
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

case class Board[A: Show](positions: Map[Position, A]) {

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

}

case object Board {
  def apply[A: Show](initialValues: List[A]): Board[A] = {
    val m: Map[Position, A] = (Position.allPositions zip initialValues).toMap
    Board(m)
  }
}

case object Game {
  import scala.io.StdIn

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def newGame[F[_]: Sync, A](board: Board[A]): F[Ref[F, Board[A]]] =
    Ref.of[F, Board[A]](board)

  def showBoard[F[_]: Sync, A](board: Board[A]): F[Unit] =
    Sync[F].delay(println(board.show))

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
