package cap.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */

import cap.scalasmt._
import HealthDBBackend._

class PatientRecord(
  private val _identity: UserRecord) extends JeevesRecord

