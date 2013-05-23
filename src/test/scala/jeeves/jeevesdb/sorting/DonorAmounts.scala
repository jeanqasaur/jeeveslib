package test.cap.jeeveslib.jeeves.jeevesdb.sorting

import org.specs2.mutable._
import org.specs2.specification.BeforeExample
import org.specs2.specification._
 
import org.squeryl._
import adapters.H2Adapter
import PrimitiveTypeMode._

class DatabaseUnitSpec extends Specification with BeforeExample{
  def before = initialize

  sequential

  val donor1 = new DonorFullRecord("donor1@donor.org", 100, 0, "x")
  val donor2 = new DonorFullRecord("donor2@donor.org", 200, 0, "y")
  val donor3 = new DonorFullRecord("donor3@donor.org", 300, 0, "x")

  "Schema stored in database should" >> {
    "allow creation of new entries" >> {
      transaction {
        val newDonor =
          Tables.writeDonor(donor1)
          newDonor.id must_== 1
      }
    }
    "allow retrieval of entries" >> {
      transaction {
        Tables.writeDonor(donor2)
        val lookupDonor = Tables.donors.where(a => a.amountHigh === 200).single
        lookupDonor.id must_== 1
      }
    }
    "find distinct labels when labels are distinct" >> {
      transaction {
        Tables.writeDonor(donor1)
        Tables.writeDonor(donor2)
        val labels = Tables.getDistinctLabels()
        labels.length must_==2
        labels.exists(_ == "x") must_==true
        labels.exists(_ == "y") must_==true
      }
    }
    "find distinct labels when labels are not distinct" >> {
      transaction {
        Tables.writeDonor(donor1)
        Tables.writeDonor(donor2)
        Tables.writeDonor(donor3)
        val labels = Tables.getDistinctLabels()
        labels.length must_==2
        labels.exists(_ == "x") must_==true
        labels.exists(_ == "y") must_==true
      }
    }
    "correctly get all possible label assignments" >> {
      transaction {
        Tables.writeDonor(donor1)
        Tables.writeDonor(donor2)
        Tables.writeDonor(donor3)
        var labels = Tables.getDistinctLabels()
        var labelAssignments = Tables.getLabelAssignments(labels)
        labelAssignments.length must_==4
      }
    }
  }

  // Initialization & tear-down functions
  def initialize = {
    println("Setting up data.")
    Class.forName("org.h2.Driver")
    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        java.sql.DriverManager.getConnection("jdbc:h2:~/example", "sa", ""),
        new H2Adapter)
    )

    inTransaction {
      Tables.drop
      Tables.create
      Tables.printDdl
    }
  }
} 
