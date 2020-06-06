package TypeClasses

import cats.effect.IO
import cats.Show
import cats.syntax.show._
import scala.io.StdIn
import scala.util.{ Random => UtilRandom }

sealed trait Console[F[_]] {
  def putStrLn[A: Show](a: A): F[Unit]
  def putStr[A: Show](a: A): F[Unit]
  def readLn(): F[String]
}

object Console {
  def apply[F[_]: Console]: Console[F] = implicitly[Console[F]]
}

sealed trait Random[F[_], N] {
  def next(n: N): F[N]
}

trait IntRandom[F[_]] extends Random[F, Int]

object GameIO {

  implicit val consoleIO: Console[IO] = new Console[IO] {
    def putStrLn[A: Show](a: A): IO[Unit] =
      IO.delay(println(a.show))
    def putStr[A: Show](a: A): IO[Unit] =
      IO.delay(print(a.show))
    def readLn(): IO[String] =
      IO.delay(StdIn.readLine)
  }

  implicit val randomIntIO: IntRandom[IO] = new IntRandom[IO] {
    def next(n: Int): IO[Int] =
      IO.delay(UtilRandom.nextInt(n))
  }

}
