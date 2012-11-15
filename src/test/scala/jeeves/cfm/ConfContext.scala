package cap.jeeves.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;

import CfmBackend._

case class ConfContext( viewer : ConfUser
                      , stage  : PaperStage ) extends Atom
