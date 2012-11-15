package cap.jeeves

/*
 * Jeeves type synonym definitions.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import scala.collection.mutable.WeakHashMap;
import scala.collection.mutable.Stack;
import Debug.debug

object JeevesTypes {
  type LevelVar = BoolVar;
  type WriteVar = BoolVar;

  type Sensitive = ObjectExpr[Atom];

  type ConfPolicy = Sensitive => Formula;
  type WritePolicy = (Sensitive, Sensitive) => Formula;
}
