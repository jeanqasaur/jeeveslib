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

class DonorFullRecord(
    val email: String
  , var amountHigh: Int
  , var amountLow: Int
  , var amountLabel: String
  ) extends KeyedEntity[Int] {
  def this() = this("", -1, -1, "")
  val id = 0;
}
class DonorRecord(val email: String, val amount: Int) {
  def this() = this("", -1)
}

object Tables extends Schema {
  /* Users. */
  val donors = table[DonorFullRecord]("Donors")
  on(donors)(u => declare(
      u.id        is(autoIncremented)
    , u.email     is(unique)
  ))
  def writeDonor(donorRecord: DonorFullRecord): DonorFullRecord = {
    transaction { donors.insert(donorRecord) }
  }

  /**
   * Gets distinct labels of the fields from the database.
   */
  def getDistinctLabels(): List[String] = {
    val labels = transaction {
      from(donors)(d => select(d.amountLabel))
    }
    labels.iterator.toList.distinct
  }

  /**
   * Given a list of n labels, generates a list of 2*n lists each containing
   * a different boolean assignment configuration for the labels.
   */
  def getLabelAssignments(labels: List[String])
    : List[List[(String, Boolean)]] = {
    labels.foldLeft(List(List[(String, Boolean)]()))(
        (acc, cur) =>
          acc.map(elts => (cur, true)::elts) ++
          acc.map(elts => (cur, false)::elts))
  }


  def assignmentToView(labelAssts: List[(String, Boolean)])
    : Queryable[DonorRecord] = {
    ???
    /*
    transaction {
      from(donors)(
        d => select(d.email, d.amountHigh)).map(
          d => new DonorRecord(d._1, d._2))
    }
    */
  }

  // TODO: Generate views for the different labels.
  def generateLabelViews(): List[Queryable[DonorRecord]] = {
    val labelAssts: List[List[(String, Boolean)]] =
      getLabelAssignments(getDistinctLabels())
    labelAssts.map(assignmentToView)
  }

  // TODO: Sort on each generated view.
  def sortDonorsByAmount(): Queryable[DonorRecord] = {
    ???
  }

  // TODO: This about how this interfaces with the rest of the program...

  // TODO: Think of way to store policies with the database.
}
