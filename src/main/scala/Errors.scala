package Errors

sealed trait Error extends Throwable
sealed trait FatalError extends Error
sealed trait BoardError extends Error

case object InvalidMove extends BoardError

case object PlayerIsCoward    extends FatalError
case object EmptyValidMoves   extends FatalError
