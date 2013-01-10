package test.cap.jeeveslib

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}

import cap.jeeveslib.ast._
import cap.jeeveslib.env.ConstraintEnv

class ExampleConstraintEnv extends FunSuite with ConstraintEnv {
  test ("bool var") {
    val x = pickBool();
    assume (! x);
    expectResult (false) {concretize(x)};
  }

  /*
  test("symbolic fields") {
    case class Dummy(uid: BigInt) extends Atom
    val x: Dummy = Dummy(0);
    val xVar: ObjectVar[Dummy] = pickObject(x);
    assume(xVar === x);

    val y: Dummy = Dummy(1);
    val yVar: ObjectVar[Dummy] = pickObject(y);
    assume(yVar === y);

    expectResult(true) { concretize(xVar === xVar) }
    expectResult(true) { concretize(xVar~'uid === xVar~'uid) }
    expectResult(false) { concretize(xVar === yVar) }
    expectResult(false) { concretize(xVar~'uid === yVar~'uid) }
  }
  */
}
