import cats.{ Show, Eq }

import cats.effect.{ IO, IOApp, ExitCode, Sync }
import cats.effect.concurrent.Semaphore

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
      s     <- Semaphore[IO](1)
      _     <- Game.gameLoop[IO, Char](board, userSymbol, cpuSymbol, emptySymbol, s)
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

trait Error

case object InvalidMove extends Error

case class Board[A: Show: Eq](positions: Map[Position, A]) {

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

    def move(position: Position, symbol: A, empty: A): Either[Error, Board[A]] = {
      import cats.syntax.eq._
      import cats.syntax.either._

      if (positions.get(position).map(_ === empty).getOrElse(false))
        this.copy(positions = this.positions + (position -> symbol)).asRight[Error]
      else
        InvalidMove.asLeft[Board[A]]
    }
}

case object Board {
  def apply[A: Show: Eq](emptySymbol: A): Board[A] =
    Board(
      (Position.allPositions zip List.fill(Position.allPositions.size)(emptySymbol)).toMap)
}

case object Game {
  import scala.io.StdIn

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def showBoard[F[_]: Sync, A](board: Board[A]): F[Unit] =
    Sync[F].delay(println(board.show))

  def readPosition[F[_]: Sync](): F[Position] =
    for {
      _        <- Sync[F].delay(print("Your move: "))
      ans      <- Sync[F].delay(StdIn.readLine)
      posObj   =  Position.fromString(ans)
      position <- posObj match {
                    case None => for {
                      _   <- Sync[F].delay(println("Invalid position"))
                      pos <- readPosition
                    } yield pos
                    case Some(position) => Sync[F].pure(position)
                  }
    } yield position

  //TODO: Better error handling, raiseError and handleErrorWith?
  //TODO: Handle exiting
  def humanAction[F[_]: Sync, A](board: Board[A], playerA: A, emptyA: A): F[Board[A]] =
      for {
        position <- readPosition[F]()
        move     = board.move(position, playerA, emptyA)
        newBoard <- move match {
                      case Left(value) =>
                        for {
                          _            <- Sync[F].delay(println(value.toString))
                          correctBoard <- humanAction(board, playerA, emptyA)
                        } yield correctBoard
                      case Right(value) =>
                        Sync[F].pure(value)
                    }
      } yield newBoard

  // TODO: check if can win else pick empty non game terminating place else defeat
  def cpuAction[F[_]: Sync, A: Eq](board: Board[A], cpuA: A, emptySymbol: A, semaphore: Semaphore[F]): F[Board[A]] = {
    val positionResult: List[(Position, Boolean, Board[A])] =
      for {
        position <- Position.allPositions
        move     = board.move(position, cpuA, emptySymbol)
        (invalid, newBoard)  = move match {
                                 case Left(_) => (true, board)
                                 case Right(moveBoard) => (endCondition(moveBoard, emptySymbol), moveBoard)
                               }
      } yield(position, invalid, newBoard)

    val validBoard: List[Board[A]] =
      positionResult.filter{ case (_, result, _) => !result }.map(_._3)

    Sync[F].pure(validBoard.headOption.getOrElse(board))
  }

  def endCondition[A: Eq](board: Board[A], emptySymbol: A): Boolean = {
    import cats.instances.int._
    import cats.syntax.eq._

    val pathVals: List[List[Option[A]]] = Position.allPaths.map(path => path.map(board.positions.get))
    val pathSet: List[Set[A]] = pathVals.map(pathVal => pathVal.flatten.toSet)
    val nonEmptyPathSize: List[Int] = pathSet.filter(_.exists(_ =!= emptySymbol)).map(_.size)
    nonEmptyPathSize.exists(_ === 1)
  }

  def gameLoop[F[_]: Sync, A: Eq](board: Board[A], playerA: A, cpuA: A, emptySymbol: A, semaphore: Semaphore[F]): F[Unit] =
    for {
      _          <- Game.showBoard[F, A](board)
      humanBoard <- Game.humanAction[F, A](board, playerA, emptySymbol)
      cpuBoard   <- Game.cpuAction[F, A](humanBoard, cpuA, emptySymbol, semaphore)
      end        =  Game.endCondition(cpuBoard, emptySymbol)
      _          <- if (end) Game.showBoard[F, A](cpuBoard)
                    else gameLoop(cpuBoard, playerA, cpuA, emptySymbol, semaphore)
    } yield ()
}
