package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.JeevesTypes._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

/**
 * Based on the Fine benchmarks from PLDI '10.
 *
 * Semantic mapping between examples:
 * - Labels correspond to level variables.
 * - The faceted evaluation automatically handles joins.
 * - Labeled values correspond to faceted values.
 * - The flow is automatically checked.
 *
 * @author jeanyang
 */
class IFlow extends FunSuite with JeevesLib {
  /**
   * If l1 is high, l2 can be high as well.  This function is equivalent to the
   * following:
   * - assume A3:forall (l:label). CanFlow l l
   *   Default "high" yields this.
   * - assume A4:forall (l:label). CanFlow Low l
   *   This rule is saying that for any value, it is fine for the low value to
   *   be there.  This is equivalent to the Jeeves* property that there is always
   *   an assignment of each level variable to LOW.
   * - assume A5:forall (src1:label) (src2:label) (dst:label).
   *    (CanFlow src1 dst && CanFlow src2 dst) =>
   *      CanFlow (Join src1 src2) dst
   *   The faceted evaluation automatically handles joins.
   * - assume A6:forall (src:label) (dst1:label) (dst2:label).
   *    (CanFlow src dst1 && CanFlow src dst2) => CanFlow src (Join dst1 dst2)
   *   (Same.)
   *   
   */
  private def canFlow (l1: LevelVar, l2: LevelVar): Unit = {
    permit(l2, (ctxt: Sensitive) => (l1 === HIGH))
  }

  /**
   * Notes:
   * - Flows are checked automatically.
   * - The programmer introduces level variables, declares "canFlow" for
   *   which labels are allowed to flow where, and then calls "commitPolicies"
   *   to save the policies for each level variable.
   * - Flows are checked automatically upon output: the appropriate value (or
   *   corresponding default) is produced by the runtime system.
   */
  // TODO: Implement some examples demonstrating this.
}
