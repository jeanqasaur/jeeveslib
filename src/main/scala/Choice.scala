package cap.scalasmt

/**
 * Angelic choice statement for arithmetic.
 * @author kuat
 */
object ChoiceStmt {
  import SMT._

  def choose(spec: IntVar => Formula) = {
    val x = Var.makeInt
    val out = solve(spec(x)).get
    out(x).toInt;
  }

  def choose(spec: (IntVar, IntVar) => Formula) = {
    val x = Var.makeInt
    val y = Var.makeInt
    val out = solve(spec(x, y)).get
    (out(x).toInt, out(y).toInt)
  }

  def choose(spec: (IntVar, IntVar, IntVar) => Formula) = {
    val x = Var.makeInt
    val y = Var.makeInt
    val z = Var.makeInt
    val out = solve(spec(x,y,z)).get
    (out(x).toInt, out(y).toInt, out(z).toInt)
  }
}
