package cap.jeeveslib.ast

sealed trait UpdateResult
case object Success extends UpdateResult
case object Failure extends UpdateResult
case object Unresolved extends UpdateResult
