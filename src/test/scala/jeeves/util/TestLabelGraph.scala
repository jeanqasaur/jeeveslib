package test.cap.jeeveslib.jeeves.util

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}
import scala.collection.mutable.HashMap

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._
import cap.jeeveslib.util._

class TestLabelGraph extends FunSuite with cap.jeeveslib.env.ConstraintEnv {
  val a = pickBool()
  val b = pickBool()
  val c = pickBool()

  test ("sorting lists with no elements") {
    val m: HashMap[LabelVar, Set[LabelVar]] = HashMap()
    expectResult(List()) {
      LabelGraph(m).topSort()
    }
  }

  test("sorting lists with one element") {
    val m: HashMap[LabelVar, Set[LabelVar]] = HashMap(a -> Set())
    expectResult(List(a)) {
      LabelGraph(m).topSort()
    }
  }

  test("sorting lists with two elements") {
    val m: HashMap[LabelVar, Set[LabelVar]] = HashMap(a -> Set(b), b -> Set())
    expectResult(List(b, a)) {
      LabelGraph(m).topSort()
    }
  }

  test("sorting lists with three elements") {
    val m: HashMap[LabelVar, Set[LabelVar]] =
      HashMap(a -> Set(b), b -> Set(c), c -> Set())
    expectResult(List(c, b, a)) {
      LabelGraph(m).topSort()
    }
  }
}
