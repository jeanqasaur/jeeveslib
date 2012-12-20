package test.cap.jeeveslib.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

class TestContext extends FunSuite with JeevesLib {

  case class Dummy(ID: BigInt) extends Atom
  case class DummyContext(id: BigInt, viewer: Dummy) extends Atom

  val defaultVal = Dummy(-1)

  def mkElt(x: Dummy): ObjectExpr[Dummy] = {
    val l = mkLevel();
    restrict(l
      , (ctxt: ObjectExpr[DummyContext]) => ctxt.viewer.ID === IntVal(1));
    mkSensitive(l, x, defaultVal)
  }

  val x: Dummy = Dummy(1);
  val highCtxt: DummyContext = DummyContext(1, Dummy(1))
  val lowCtxt: DummyContext = DummyContext(0, Dummy(0))
  val x_s: ObjectExpr[Dummy] = mkElt(x);
  val y: Dummy = Dummy(2);
  val y_s: ObjectExpr[Dummy] = mkElt(y); 
 
  val c = (1 to 3).toList.map(Dummy(_))
  val s = c.map(mkElt)

  test ("comparing two symbolic fields") {
    expect(true) { concretize(lowCtxt, x_s === y_s) }
    expect(true) { concretize(lowCtxt, x_s~'ID === y_s~'ID) }
    expect(false) { concretize(highCtxt, x_s === y_s) }
    expect(false) { concretize(highCtxt, x_s~'ID === y_s~'ID) }
  }

  test ("formula with context id and object field - false") {
    expect(-1) { concretize(lowCtxt, x_s~'ID) }
  }

  test ("formula with context viewer and object field - false") {
    expect(-1) { concretize(lowCtxt, x_s~'ID) }
  }

  test ("high confidentiality context") {
    expect (x) { concretize(highCtxt, x_s) }
  }

  test ("low confidentiality context") {
    expect (defaultVal) { concretize(NULL, x_s) }
  } 

  test ("context field") {
    expect (x.ID) { concretize(highCtxt, x_s~'ID) }
    expect (true) { concretize(highCtxt, x_s~'ID === IntVal(x.ID)) }
  }

  /* Lists. */
  test ("low confidentiality context - list") {
    expect(true) { concretize(NULL, s.has(defaultVal)) }
  }

  test ("high confidentiality context - list") {
    expect(true) { concretize(highCtxt, s.has(Dummy(1))) }
  }

  test("circular dependency") {
    val a = mkLevel()
    val v = mkSensitive(a, Dummy(1), Dummy(0))
    restrict(a, (ctxt: ObjectExpr[DummyContext]) => ctxt === Dummy(1))
    expect(Dummy(1)) {
      concretize(v, v)
    }
  }
}
