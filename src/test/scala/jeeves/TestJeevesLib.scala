package test.cap.jeeveslib.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}
import scala.collection.immutable.Map

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

class ExampleJeevesLib extends FunSuite with JeevesLib {
  case class Dummy(id: BigInt) extends Atom

  test ("sensitive int") {
    val l = mkLevel();
    val x = mkSensitiveInt(l, 42, -1);

    expectResult(42) {concretize(l === HIGH, x)};
    expectResult(-1) {concretize(l === LOW, x)};
  }

  test ("sensitive object") {
    val l = mkLevel();
    val t = Dummy(1);

    val x = mkSensitive(l, t, NULL);
    
    expectResult(t) {concretize(l === HIGH, x)};
    expectResult(null) {concretize(l === LOW, x)};
  }

  test ("test restrict") {
    val x = Dummy(1)
    val a = mkLevel ()
    restrict (a, (ctxt: ObjectExpr[Dummy]) => ctxt === x)
    val xS = mkSensitive (a, x, Dummy(-1))
    expectResult (x) { concretize (x, xS) }
  }

  test ("jif with IntExpr") {
    val a = mkLevel();
    val x = mkSensitiveInt(a, 0, 1)
    // If ctxt != 0, then a is LOW.
    restrict (a, (ctxt: ObjectExpr[Dummy]) => ctxt === Dummy(0))
    val r = jif (x === 0, (_: Unit) => IntVal(3), (_: Unit) => IntVal(4))
    expectResult (IntFacet(a, IntVal(3), IntVal(4))) { r }
    expectResult (3) { concretize(Dummy(0), r) }
    expectResult (4) { concretize(Dummy(1), r) }
  }

  test ("jif with ObjectExpr") {
    val a = mkLevel();
    val x = mkSensitive(a, Dummy(0), Dummy(1))
    // If ctxt != 0, then a is LOW.
    restrict (a, (ctxt: ObjectExpr[Dummy]) => ctxt === Dummy(0))
    val r =
      jif (x === Dummy(0)
        , (_: Unit) => Object(Dummy(3)), (_: Unit) => Object(Dummy(4)))
    expectResult (ObjectFacet(a, Dummy(3), Dummy(4))) { r }
    expectResult (Dummy(3)) { concretize(Dummy(0), r) }
    expectResult (Dummy(4)) { concretize(Dummy(1), r) }
  }

  test ("restrict under conditional") {
    // TODO: Do we want this?
  }

  test ("nested conditionals with shared path condition") {
    val a = mkLevel();
    val x = mkSensitiveInt(a, 0, 1)
    val y = mkSensitiveInt(a, 2, 3)
    restrict (a, (ctxt: ObjectExpr[Dummy]) => ctxt === Dummy(0))
    val r =
      jif ( x === 0
        , ((_: Unit) =>
            jif (y === 2, (_: Unit) => IntVal (7), (_: Unit) => IntVal (8)))
        , ((_: Unit) =>
            IntVal (9)) )
    expectResult (IntFacet(a, IntVal(7), IntVal(9))) { r }
    expectResult (7) { concretize(Dummy(0), r) }
    expectResult (9) { concretize(Dummy(1), r) }
  }

  test ("nested conditionals with no shared path condition") {
    val a = mkLevel();
    val b = mkLevel();

    val x = mkSensitiveInt(a, 0, 1)
    val y = mkSensitiveInt(b, 2, 3)

    restrict (a, (ctxt: ObjectExpr[Dummy]) => ctxt === Dummy(0))
    restrict (b, (ctxt: ObjectExpr[Dummy]) => ctxt === Dummy(1))

    val r =
      jif ( x === 0
        , ((_: Unit) =>
            jif (y === 2, (_: Unit) => IntVal (7), (_: Unit) => IntVal (8)))
        , ((_: Unit) =>
            IntVal (9)) )
    expectResult (IntFacet(a, IntFacet(b, IntVal(7), IntVal(8)), IntVal(9))) { r }
    expectResult (8) { concretize(Dummy(0), r) }
    expectResult (9) { concretize(Dummy(1), r) }
  }

  /* Function facets. */
  test ("function facets") {
    def id[T](x: T): T = x
    def inc(x: IntExpr): IntExpr = x + 1

    val a = mkLevel ()
    restrict (a, (ctxt: ObjectExpr[Dummy]) => ctxt === Dummy(0))
    val f: FunctionExpr[IntExpr, IntExpr] =
      mkSensitiveIntFunction (a, FunctionVal(id[IntExpr]_), FunctionVal(inc))
    expectResult (IntFacet(a, IntVal(1), Plus(IntVal(1), IntVal(1)))) {
      jfun[IntExpr, IntExpr](f, 1)
    }
    expectResult (1) { concretize(Dummy(0), jfun[IntExpr, IntExpr](f, 1)) }
    expectResult (2) { concretize(Dummy(1), jfun[IntExpr, IntExpr](f, 1)) }
  }
}
