package cap.jeeves

/*
 * An abstraction that supports "
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import JeevesTypes._

trait PermitPolicies {
  private val _storedPolicies: Map[LevelVar, List[ConfPolicy]] = Map()

  /**
   * Programming with only "restrict" is a pain, so we allow people to collect
   * "permit" policies on things.
   */
  def permit(lvar: LevelVar, p: Sensitive => Formula)
    (implicit lvars: LevelVars) = {
    val guardedConfPolicy = lvars.mkGuardedConfPolicy (p);
    _storedPolicies.get(lvar) match {
      case Some(policies: List[Sensitive => Formula]) =>
       _storedPolicies += (lvar -> (guardedConfPolicy :: policies))
      case None => _storedPolicies += (lvar -> List(guardedConfPolicy))
    }
  }
  // This function restricts everything except for what has been permitted.
  def commitPolicies(lvar: LevelVar) (implicit lvars: LevelVars) = {
    def mkSingleFormula (f_acc: Sensitive => Formula, f: Sensitive => Formula)
      : Sensitive => Formula = {
      (ctxt : Sensitive) => Or (f_acc (ctxt), f (ctxt))
    }

    _storedPolicies.get(lvar) match {
      case Some(policies) =>
        val policyFormula: Sensitive => Formula =
          policies.foldLeft(
            (ctxt: Sensitive) => BoolVal(true): Formula)(mkSingleFormula)
        lvars.restrict (lvar, policyFormula)
        _storedPolicies.remove(lvar)
      case None => ()
    }
  }
}
