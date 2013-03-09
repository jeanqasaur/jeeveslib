package cap.jeeveslib.demo.cfm

import java.security.SecureRandom;
import java.math.BigInteger;

object RandomGenerator {
  private val random: SecureRandom = new SecureRandom()
  private def generateString(len: Int): String = {
    (new BigInteger(130, random).toString(32)).substring(0, len)
  }

  def generatePassword(): String = generateString(8)
  def generateSecretId(): String = generateString(6)
}
