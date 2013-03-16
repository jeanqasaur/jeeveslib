package cap.jeeveslib.ast

sealed trait UpdateResult extends Atom
case object Success extends UpdateResult
case object Failure extends UpdateResult
case object Unresolved extends UpdateResult
