package Errors

trait Error extends Throwable
trait FatalError extends Error
trait BoardError extends Error

case object InvalidMove extends BoardError

case object PlayerHasNoSymbol extends FatalError
case object PlayerIsCoward    extends FatalError
