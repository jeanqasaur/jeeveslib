package cap.jeeveslib.smt

import java.io._

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.Zeros._
import cap.jeeveslib.env.{DefaultEnv, VarEnv}
import cap.jeeveslib.util.Debug._

/* 
 * Translator to SMT-LIB2.
 * @author kuat
 */
private object UnsatException extends RuntimeException("inconsistent model")

case class SolverException(msg: String) extends RuntimeException(msg)

/** SMT-LIB2 compliant solver. */
trait Solver {
  /** Issue a command and expect success. */
  def command(s: String) {
    >(s);
    val reply = <();
    if (reply != "success") 
      throw SolverException("unexpected reply: " + reply)
  }
  /** Check satisfiability. */
  def check(): Boolean = {
    >("(check-sat)"); 
    val reply = <();
    if (reply == "sat") 
      true
    else if (reply == "unsat")
      false
    else 
      throw SolverException("unexpected reply: " + reply)
  }
  /** Evaluate the term in the current model. */
  def eval(term: String): String = {>("(get-value (" + term + "))"); <<()}
  /** Assert a boolean condition. */
  def assert(s: String) = command("(assert " + s + ")")
  /** Push a context */
  def push() = command("(push)")
  /** Pop a context */
  def pop() = command("(pop)")
  /** Reset the solver */
  def reset() = command("(reset)")
  /** Terminate the solver. */
  def close()
  /** Send to solver. */ 
  protected def >(s: String)
  /** Receive a line from the solver. */
  protected def <(): String
  /** Receive a term from the solver. */
  def <<() = {
    import scala.collection.mutable
    val out = new mutable.ListBuffer[String]
    var balance = 0;
    do {
      var line = <();
      balance = balance + line.count(_ == '(') - line.count(_ == ')');
      out += line;
    } while (balance > 0)
    out.toList.mkString;
  }
}

/** Log input and output of a solver. */
trait Logging extends Solver {
  abstract override def >(s: String) {debug("> " + s); super.>(s)}
  abstract override def <() = {val s = super.<(); debug("< " + s); s}
  abstract override def close() = {super.close(); debug(this + " closed")}
}

/** Retrieve solver metadata. */
trait SolverDescription extends Solver {
  override val toString = {
    >("(get-info :name)")
    val name = <()
      >("(get-info :version)")
    val version = <()
      name + version
  } 
}
