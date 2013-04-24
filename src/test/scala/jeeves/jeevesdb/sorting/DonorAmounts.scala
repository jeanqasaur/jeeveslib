package test.cap.jeeveslib.jeeves.jeevesdb.sorting

import org.specs2.mutable._
import org.specs2.specification.BeforeExample
import org.specs2.specification._
 
import org.squeryl._
import adapters.H2Adapter
import PrimitiveTypeMode._

import Tables._

class DatabaseUnitSpec extends Specification with BeforeExample{
  def before = initialize

  sequential

  "Schema stored in database should" >> {
    "allow creation of new entries" >> {
      transaction {
        val newDonor = donors.insert(new DonorRecord("donor1@donor.org", 100))
          newDonor.id must_== 1
      }
    }
    "allow retrieval of entries" >> {
      transaction {
        donors.insert(new DonorRecord("donor2@donor.org", 200))
        val lookupDonor = donors.where(a => a.amount === 200).single
        lookupDonor.id must_== 1
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
      printDdl
    }
  }
} 
