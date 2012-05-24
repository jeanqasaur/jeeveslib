package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class ExampleJeevesLib extends FunSuite with JeevesLib {
  test ("sensitive int") {
    val l = mkLevel();
    val x = mkSensitiveInt(l, 42, -1);

    expect(42) {concretize(l === HIGH, x)};
    expect(-1) {concretize(l === LOW, x)};
  }

  test ("sensitive object") {
    val l = mkLevel();
    val t = Test(1);

    val x = mkSensitive(l, t, NULL);
    
    expect(t) {concretize(l === HIGH, x)};
    expect(null) {concretize(l === LOW, x)};
  }

  case class Test(id: Int) extends JeevesRecord
  
  test ("concretizeList non-null") {
    val x = pickObject[Test];
    assume(x === Test(0));
    val symList = List(x);
    val cList : List[Test] = concretize(NULL, symList);
    expect(List(Test(0))) {cList};
  }

  test ("concretizeList null") {
    val x = pickObject[Test](NULL);
    val symList = List(x);
    val cList : List[Test] = concretize(NULL, symList);
    expect(Nil) {cList};
  }
}
