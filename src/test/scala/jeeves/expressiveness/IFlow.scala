package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

/**
 * Based on the Fine benchmarks from PLDI '10.
 * @author jeanyang
 */
class IFlow extends FunSuite with JeevesLib {
  case class StringVal (v: String) extends JeevesRecord

  class Label extends JeevesRecord
  case object High extends Label
  case object Low extends Label
  case class Join(l1: Label, l2: Label) extends Label

  // TODO: Rest of the example.
}
