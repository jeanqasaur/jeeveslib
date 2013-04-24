/**
 * TODO: Make tables for donors, donor names, and donation amounts.
 */
package test.cap.jeeveslib.jeeves.jeevesdb.sorting

//import JConfBackend._

import org.squeryl.KeyedEntity
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
// import org.squeryl.customtypes.CustomTypesMode._
// import org.squeryl.customtypes._
import org.squeryl.Schema
import org.squeryl.Session
import org.squeryl.SessionFactory

class DonorRecord(
    val email: String
  , var amount: Int
  ) extends KeyedEntity[Int] {
  def this() = this("", -1)
  val id = 0;
}

object Tables extends Schema {
  /* Users. */
  val donors = table[DonorRecord]("Donors")
  on(donors)(u => declare(
      u.id        is(autoIncremented)
    , u.email     is(unique)
  ))
  def writeDonor(donorRecord: DonorRecord): Unit = {
    transaction { donors.insert(donorRecord) }
  }
}
