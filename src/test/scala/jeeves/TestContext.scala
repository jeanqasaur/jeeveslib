package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.jeeves._

class ExampleContext extends FunSuite with JeevesLib {

  case class Dummy(ID: BigInt) extends JeevesRecord
  case class DummyContext(id: BigInt, viewer: Dummy) extends JeevesRecord

  val defaultVal = Dummy(-1)

  def mkElt(x: Dummy): Symbolic = {
    val l = mkLevel();
    policy(l, !(CONTEXT.viewer.ID === IntVal(1)));
    mkSensitive(l, x, defaultVal)
  }

  val x: Dummy = Dummy(1);
  val highCtxt: DummyContext = DummyContext(1, Dummy(1))
  val lowCtxt: DummyContext = DummyContext(0, Dummy(0))
  val x_s: Symbolic = mkElt(x);
  val y: Dummy = Dummy(2);
  val y_s: Symbolic = mkElt(y); 
 
  val c = (1 to 3).toList.map(Dummy(_))
  val s = c.map(mkElt)

  test ("comparing two symbolic fields") {
    expect(true) { concretize(lowCtxt, x_s === y_s) }
    expect(true) { concretize(lowCtxt, x_s~'ID === y_s~'ID) }
    expect(false) { concretize(highCtxt, x_s === y_s) }
    expect(false) { concretize(highCtxt, x_s~'ID === y_s~'ID) }
  }

  test ("context viewer values") {
    expect(1) { concretize(highCtxt, CONTEXT.viewer~'ID) }
    expect(0) { concretize(lowCtxt, CONTEXT.viewer~'ID) }
    expect(true) {
      concretize(highCtxt, CONTEXT.viewer~'ID === IntVal(1))
    }
    expect(false) {
      concretize(highCtxt, CONTEXT.viewer~'ID === IntVal(0))
    }
  }

  test ("formula with context id and object field - false") {
    expect(0) { concretize(lowCtxt, CONTEXT~'id) }
    expect(-1) { concretize(lowCtxt, x_s~'ID) }
    expect(false) {
      concretize(lowCtxt, CONTEXT~'id === x_s~'ID)
    }
  }

  test ("formula with context viewer and object field - true") {
    expect(true) {
      concretize(highCtxt, CONTEXT.viewer~'ID === x_s~'ID)
    }
  }

  test ("formula with context viewer and object field - false") {
    expect(0) { concretize(lowCtxt, CONTEXT.viewer~'ID) }
    expect(-1) { concretize(lowCtxt, x_s~'ID) }
    expect(false) {
      concretize(lowCtxt, CONTEXT.viewer~'ID === x_s~'ID)
    }
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

  /*
  test ("hasFormula") {
    expect(true) {
      concretize(highCtxt, s.hasFormula(x =>
        x.ID === CONTEXT.viewer.ID))
    }
    expect(false) {
      concretize(lowCtxt, s.hasFormula(x =>
        x.ID === CONTEXT.viewer.ID))
    }
  }
  */
}
