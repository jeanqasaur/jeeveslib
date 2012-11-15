package cap.jeeveslib.ast

/*
 * Jeeves type synonym definitions.
 * @author jeanyang
 */

import cap.jeeveslib._

object JeevesTypes {
  type LevelVar = BoolVar;
  type WriteVar = BoolVar;

  type Sensitive = ObjectExpr[Atom];

  type ConfPolicy = Sensitive => Formula;
  type WritePolicy = (Sensitive, Sensitive) => Formula;
}
