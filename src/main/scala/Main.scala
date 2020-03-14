import cats.{ Show, Eq }

import cats.effect.{ IO, IOApp, ExitCode, Sync }

object Main extends IOApp {
  import cats.instances.char.catsStdShowForChar
  import cats.instances.char.catsKernelStdOrderForChar

  val symbols: Symbols[Char] = Symbols('x', 'o', ' ')

  val player: Player = Human

  def run(args: List[String]): IO[ExitCode] =
    for {
      board <- IO(Board(symbols))
      _     <- IO(println(s"Posible positions: ${Position.showAllPositions}"))
      _     <- Game.gameLoop[IO, Char](board, player)
    } yield (ExitCode.Success)
}

case class Symbols[A](user: A, cpu: A, empty: A)

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

trait Error extends Throwable

case object InvalidMove extends Error

case class Board[A: Show: Eq](positions: Map[Position, A], symbols: Symbols[A]) {
  import cats.syntax.eq._

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

    def move[F[_]: Sync](position: Position, symbol: A): F[Board[A]] = {
      import cats.syntax.eq._

      if (positions.get(position).map(_ === symbols.empty).getOrElse(false))
        Sync[F].pure(this.copy(positions = this.positions + (position -> symbol)))
      else
        Sync[F].raiseError(InvalidMove)
    }

    def validPositions: List[Position] =
      positions.filter(_._2 === symbols.empty).keys.toList
}

case object Board {
  def apply[A: Show: Eq](symbols: Symbols[A]): Board[A] =
    Board(
      (Position.allPositions zip List.fill(Position.allPositions.size)(symbols.empty)).toMap,
      symbols)
}

trait Player

case object Human extends Player
case object Cpu   extends Player

case object Game {
  import scala.io.StdIn

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  // TODO: raiseError and handleErrorWith?
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

  //TODO: handleErrorWith
  //TODO: Handle exiting
  def humanAction[F[_]: Sync, A](board: Board[A]): F[Board[A]] =
      for {
        position <- readPosition[F]()
        newBoard <- board.move(position, board.symbols.user)
      } yield newBoard

  // TODO: check if can win else pick empty non game terminating place else defeat
  def cpuAction[F[_]: Sync, A: Eq](board: Board[A]): F[Board[A]] =
    for {
      _        <- Sync[F].delay(println("Cpu move"))
      newBoard <- board.move(board.validPositions.head, board.symbols.cpu)
    } yield newBoard

  // How can this be so ugly?
  def endCondition[A: Eq](board: Board[A]): Boolean = {
    import cats.instances.int._
    import cats.syntax.eq._

    val pathVals: List[List[Option[A]]] = Position.allPaths.map(path => path.map(board.positions.get))
    val pathSet: List[Set[A]] = pathVals.map(pathVal => pathVal.flatten.toSet)
    val nonEmptyPathSize: List[Int] = pathSet.filter(_.exists(_ =!= board.symbols.empty)).map(_.size)
    nonEmptyPathSize.exists(_ === 1)
  }

  def moveLoop[F[_]: Sync, A: Eq](board: Board[A], player: Player): F[Board[A]] =
    player match {
      case Human => Game.humanAction[F, A](board)
      case Cpu   => Game.cpuAction[F, A](board)
    }

  def changePlayer(player: Player): Player =
    player match {
      case Cpu   => Human
      case Human => Cpu
    }

  def showWinner[F[_]: Sync, A](board: Board[A], player: Player): F[Unit] =
    for {
      _ <- Sync[F].delay(println(s"Winner: ${player.toString}"))
      _ <- Sync[F].delay(println(board.show))
    } yield ()

  def gameLoop[F[_]: Sync, A: Eq](board: Board[A], player: Player): F[Unit] =
    for {
      _          <- Sync[F].delay(println(board.show))
      newBoard   <- moveLoop[F, A](board, player)
      end        =  endCondition(newBoard)
      _          <- if (end) showWinner[F, A](newBoard, player)
                    else gameLoop(newBoard, changePlayer(player))
    } yield ()
}
