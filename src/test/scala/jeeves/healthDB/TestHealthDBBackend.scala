package test.cap.jeeveslib.jeeves.healthDB

import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

import HealthDBBackend._

class TestHealthDBBackend extends FunSuite {
  val admin = UserRecord(0, S("Admin"), Admin)

  // Doctors.
  val doctor0 = UserRecord(1, S("Doctor 1"), Doctor)
  val doctor1 = UserRecord(2, S("Doctor 2"), Doctor)

  // Patients.
  val patientUser0 = UserRecord(3, S("Alice"), Patient)
  val patient0 = PatientRecord(patientUser0, doctor0, Nil)
  val patientUser1 = UserRecord(4, S("Bob"), Patient)
  val patient1 = PatientRecord(patientUser1, doctor1, Nil)

  test ("patient can see own doctor") {
    expectResult(doctor0) {
      concretize(HealthContext(patientUser0), patient0.getDoctor())
    }
  }
  test ("other patients cannot see doctor") {
    expectResult(defaultUser) {
      concretize(HealthContext(patientUser1), patient0.getDoctor())
    }
  }
  test ("doctor can see patient's doctor") {
    expectResult(doctor0) {
      concretize(HealthContext(doctor0), patient0.getDoctor())
    }
  }
  test ("other doctor cannot see patient's doctor") {
    expectResult(defaultUser) {
      concretize(HealthContext(doctor1), patient0.getDoctor())
    }
  }
  // Test integrity
  test ("non-admin cannot set doctor") {
    patient0.setDoctor(doctor1)(HealthContext(patientUser0))
    expectResult(doctor0) {
      concretize(HealthContext(patientUser0), patient0.getDoctor())
    }
  }
  test ("admin can set doctor") {
    patient0.setDoctor(doctor1)(HealthContext(admin))
    expectResult(doctor1) {
      concretize(HealthContext(patientUser0), patient0.getDoctor())
    }
  }
}
