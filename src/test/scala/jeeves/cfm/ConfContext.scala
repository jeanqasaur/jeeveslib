package test.cap.jeeveslib.jeeves.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.jeeveslib.ast._
import scala.collection.mutable.Map;

import CfmBackend._

case class ConfContext( viewer : ConfUser
                      , stage  : PaperStage ) extends Atom
