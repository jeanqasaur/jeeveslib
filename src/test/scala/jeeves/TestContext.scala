package test.cap.jeeveslib.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

case class Dummy(id: BigInt) extends Atom
class TestContext extends FunSuite with JeevesLib[Dummy] {
  val defaultVal = Dummy(-1)

  def mkElt(x: Dummy): ObjectExpr[Dummy] = {
    val l = mkLevel();
    restrict(l, (ctxt: ObjectExpr[Dummy]) => ctxt.id === IntVal(1));
    mkSensitive(l, x, defaultVal)
  }

  val x: Dummy = Dummy(1);
  val highCtxt: Dummy = Dummy(1)
  val lowCtxt: Dummy = Dummy(0)
  val x_s: ObjectExpr[Dummy] = mkElt(x);
  val y: Dummy = Dummy(2);
  val y_s: ObjectExpr[Dummy] = mkElt(y); 
 
  val c = (1 to 3).toList.map(Dummy(_))
  val s = c.map(mkElt)

  test ("comparing two symbolic fields") {
    expectResult(true) { concretize(lowCtxt, x_s === y_s) }
    expectResult(true) { concretize(lowCtxt, x_s~'id === y_s~'id) }
    expectResult(false) { concretize(highCtxt, x_s === y_s) }
    expectResult(false) { concretize(highCtxt, x_s~'id === y_s~'id) }
  }

  test ("formula with context id and object field - false") {
    expectResult(-1) { concretize(lowCtxt, x_s~'id) }
  }

  test ("formula with context viewer and object field - false") {
    expectResult(-1) { concretize(lowCtxt, x_s~'id) }
  }

  test ("high confidentiality context") {
    expectResult (x) { concretize(highCtxt, x_s) }
  }

  test ("low confidentiality context") {
    expectResult (defaultVal) { concretize(NULL, x_s) }
  } 

  test ("context field") {
    expectResult (x.id) { concretize(highCtxt, x_s~'id) }
    expectResult (true) { concretize(highCtxt, x_s~'id === IntVal(x.id)) }
  }

  /* Lists. */
  test ("low confidentiality context - list") {
    expectResult(true) { concretize(NULL, s.has(defaultVal)) }
  }

  test ("high confidentiality context - list") {
    expectResult(true) { concretize(highCtxt, s.has(Dummy(1))) }
  }

  test("circular dependency") {
    val a = mkLevel()
    val v = mkSensitive(a, Dummy(1), Dummy(0))
    restrict(a, (ctxt: ObjectExpr[Dummy]) => ctxt === Dummy(1))
    expectResult(Dummy(1)) {
      concretize(v, v)
    }
  }
}
