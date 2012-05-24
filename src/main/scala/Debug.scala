package cap.scalasmt

/**
 * Diagnostic output.
 * @author kuat
 */
object Debug {
  var DEBUG = false;

  def debug(m: => String) = 
    if (DEBUG) 
      Console.err.println(Console.YELLOW + m + Console.RESET)

  def warning(m: => String) =
    Console.out.println(Console.RED + m + Console.RESET)
}
