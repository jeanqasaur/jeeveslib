/**
 * TODO: Make tables for donors, donor names, and donation amounts.
 */
package test.cap.jeeveslib.jeeves.jeevesdb.sorting

import org.squeryl._
import org.squeryl.KeyedEntity
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.Session
import org.squeryl.SessionFactory

// TODO: Obviously we will have to build infrastructure on top of this to
// generate some of this automatically...

class DonorRecord(
    val email: String
  , var amountHigh: Int
  , var amountLow: Int
  , var amountLabel: String
  ) extends KeyedEntity[Int] {
  def this() = this("", -1, -1, "")
  val id = 0;
}

object Tables extends Schema {
  /* Users. */
  val donors = table[DonorRecord]("Donors")
  on(donors)(u => declare(
      u.id        is(autoIncremented)
    , u.email     is(unique)
  ))
  def writeDonor(donorRecord: DonorRecord): DonorRecord = {
    transaction { donors.insert(donorRecord) }
  }

  def getDistinctLabels(): List[String] = {
    val labels = transaction {
      from(donors)(s => select(s.amountLabel))
    }
    labels.iterator.toList.distinct
  }

  def generateLabelViews(): List[Queryable[DonorRecord]] = {
    ???
  }

  def sortDonorsByAmount(): Queryable[DonorRecord] = {
    ???
  }

  // TODO: Think of way to store policies with the database.
}
