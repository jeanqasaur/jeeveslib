package cap.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */

import cap.scalasmt._
import HealthDBBackend._

import java.util.{Calendar, Date, GregorianCalendar, Locale, TimeZone}

class HealthContext(user: UserRecord) extends Atom {
  val time =
    new GregorianCalendar(TimeZone.getTimeZone("EST"), Locale.US)
}
