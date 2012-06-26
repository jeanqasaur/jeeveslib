package cap.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */

import cap.scalasmt._
import HealthDBBackend._

case class UserRecord(id: Int, _name: S) extends JeevesRecord
