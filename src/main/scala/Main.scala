import cats.Show

import cats.effect.{ IO, IOApp, ExitCode }

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val c = new Cell[IO]
    println(c.show)
    IO(ExitCode.Success)
  }
}

case class Cell[F[_]]() {
  // import cats.effect.concurrent.Semaphore

  // val used: Semaphore[F] = ???
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
