import cats.Show

import cats.effect.{ IO, IOApp, ExitCode, Sync }
import cats.effect.concurrent.Ref

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      board <- Ref.of[IO, Vector[Vector[Char]]](Vector[Vector[Char]](
                 Vector('q', 'w', 'e'),
                 Vector('a', 'b', 'd'),
                 Vector('z', 'x', 'c')))
      _ <- Game.showBoard[IO](board)
      x <- Game.humanAction[IO]
      y <- Game.cpuAction[IO]
    } yield (ExitCode.Success)
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

case class Cell[F[_]]() {

  val value: Option[Char] = None

  implicit val showCell: Show[Cell[F]] =
    Show.show { cell =>
      val i: Char = cell.value.getOrElse(' ')
      s"""+---+
          || $i |
          |+---+""".stripMargin
    }

  def show(): String =
    Show[Cell[F]].show(this)
}
