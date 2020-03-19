package Domain

case class Symbols[A](human: A, cpu: A, empty: A) {
  def playerSymbol(player: Player): A =
    player match {
      case Human => human
      case Cpu   => cpu
    }
}

sealed trait Player

case object Human extends Player
case object Cpu   extends Player
