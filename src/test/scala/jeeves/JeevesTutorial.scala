package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.JeevesTypes._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class JeevesTutorial extends FunSuite with JeevesLib {
  case class DummyUser(id: BigInt) extends JeevesRecord
  val nobody = DummyUser(-1);
  val alice = DummyUser(0);
  val bob = DummyUser(1);
  val carol = DummyUser(2);

  // TODO: Faceted function depends on permissions...
}
