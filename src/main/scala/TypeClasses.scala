package TypeClasses

import cats.effect.Sync
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

  implicit def syncConsole[F[_]: Sync]: Console[F] = new Console[F] {
    def putStrLn[A: Show](a: A): F[Unit] =
      Sync[F].delay(println(a.show))
    def putStr[A: Show](a: A): F[Unit] =
      Sync[F].delay(print(a.show))
    def readLn(): F[String] =
      Sync[F].delay(StdIn.readLine)
  }
}

sealed trait Random[F[_], N] {
  def next(n: N): F[N]
}

trait IntRandom[F[_]] extends Random[F, Int]

object IntRandom {
  implicit def intRandom[F[_]: Sync]: IntRandom[F] = new IntRandom[F] {
    def next(n: Int): F[Int] =
      Sync[F].delay(UtilRandom.nextInt(n))
  }
}
