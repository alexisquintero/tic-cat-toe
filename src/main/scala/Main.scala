import cats.{ Show, Eq }

import cats.effect.{ IO, IOApp, ExitCode, Sync }
import cats.effect.concurrent.Ref

object Main extends IOApp {
  import cats.instances.char.catsStdShowForChar
  import cats.instances.char.catsKernelStdOrderForChar

  def run(args: List[String]): IO[ExitCode] =
    for {
      emptySymbol <- IO(' ')
      userSymbol  <- IO('x')
      cpuSymbol   <- IO('o')
      board <- IO(Board(emptySymbol))
      _     <- IO(println(s"Posible positions: ${Position.showAllPositions}"))
      game  <- Game.newGame[IO, Char](board)
      _     <- Game.gameLoop[IO, Char](board, userSymbol, cpuSymbol, emptySymbol)
    } yield (ExitCode.Success)
}
trait Position

object Position {
  import cats.syntax.option._
  import cats.Foldable
  import cats.instances.list._
  import cats.instances.string._

  val rows: List[List[Position]] =
    List(
      List(TopLeft, TopMiddle, TopRight),
      List(MiddleLeft, MiddleMiddle, MiddleRight),
      List(BottomLeft, BottomMiddle, BottomRight))

  val columns: List[List[Position]] =
    List(
      List(TopLeft, MiddleLeft, BottomLeft),
      List(TopMiddle, MiddleMiddle, BottomMiddle),
      List(TopRight, MiddleRight, BottomRight))

  val diagonals: List[List[Position]] =
    List(
      List(TopLeft, MiddleMiddle, BottomRight),
      List(TopRight, MiddleMiddle, BottomLeft))

  val allPaths: List[List[Position]] =
    (rows ++ columns ++ diagonals)

  val allPositions: List[Position] =
    List(TopLeft, TopMiddle, TopRight, MiddleLeft, MiddleMiddle, MiddleRight, BottomLeft, BottomMiddle, BottomRight)

  def showAllPositions(): String =
    Foldable[List].intercalate(allPositions.map(_.toString), ", ")

  def fromString(s: String): Option[Position] =
    s.toLowerCase match {
      case "topleft"      => TopLeft.some
      case "topmiddle"    => TopMiddle.some
      case "topright"     => TopRight.some
      case "middleleft"   => MiddleLeft.some
      case "middlemiddle" => MiddleMiddle.some
      case "middleright"  => MiddleRight.some
      case "bottomleft"   => BottomLeft.some
      case "bottommiddle" => BottomMiddle.some
      case "bottomright"  => BottomRight.some
      case _              => none
    }
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

    def move(position: Position, symbol: A): Board[A] =
      this.copy(positions = this.positions + (position -> symbol))
}

case object Board {
  def apply[A: Show](emptySymbol: A): Board[A] =
    Board(
      (Position.allPositions zip List.fill(Position.allPositions.size)(emptySymbol)).toMap)
}

case object Game {
  import scala.io.StdIn

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def newGame[F[_]: Sync, A](board: Board[A]): F[Ref[F, Board[A]]] =
    Ref.of[F, Board[A]](board)

  def showBoard[F[_]: Sync, A](board: Board[A]): F[Unit] =
    Sync[F].delay(println(board.show))

  //TODO: Better error handling, raiseError and handleErrorWith?
  //TODO: Handle exiting
  def humanAction[F[_]: Sync, A](board: Board[A], playerA: A): F[Board[A]] =
      for {
        _        <- Sync[F].delay(print("Your move: "))
        ans      <- Sync[F].delay(StdIn.readLine)
        posObj   =  Position.fromString(ans)
        newBoard <- posObj match {
                      case None => for {
                        _ <- Sync[F].delay(println("Invalid position"))
                      } yield board
                      case Some(position) => Sync[F].delay(board.move(position, playerA))
                    }
      } yield newBoard

  // TODO: pick empty non game terminating place, else defeat
  def cpuAction[F[_]: Sync, A](board: Board[A], cpuA: A): F[Unit] =
    for {
      _ <- Sync[F].delay(println("My move: "))
      _ <- Sync[F].delay(println("move"))
    } yield ()

  def endCondition[F[_]: Sync, A: Eq](board: Board[A], emptySymbol: A): F[Boolean] = {
    import cats.instances.int._
    import cats.syntax.eq._

    val pathVals: List[List[Option[A]]] = Position.allPaths.map(path => path.map(board.positions.get))
    val pathSet: List[Set[A]] = pathVals.map(pathVal => pathVal.flatten.toSet)
    val nonEmptyPathSize: List[Int] = pathSet.filter(_.exists(_ =!= emptySymbol)).map(_.size)
    Sync[F].pure(nonEmptyPathSize.exists(_ === 1))
  }

  def gameLoop[F[_]: Sync, A: Eq](board: Board[A], playerA: A, cpuA: A, emptySymbol: A): F[Unit] =
    for {
      _          <- Game.showBoard[F, A](board)
      humanBoard <- Game.humanAction[F, A](board, playerA)
      cpuBoard   <- Game.cpuAction[F, A](humanBoard, cpuA)
      end        <- Game.endCondition(humanBoard, emptySymbol)
      _          <- if (end) Sync[F].unit
                    else gameLoop(humanBoard, playerA, cpuA, emptySymbol)
    } yield ()
}
