package Domain

case class Symbols[A](playerSymbols: Map[Player, A], empty: A)

object Symbols {
  def apply[A](user: A, cpu: A, empty: A): Symbols[A] =
    Symbols(Map[Player, A](Human -> user, Cpu -> cpu), empty)
}

trait Player

case object Human extends Player
case object Cpu   extends Player
