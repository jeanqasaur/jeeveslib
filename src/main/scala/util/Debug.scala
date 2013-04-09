package cap.jeeveslib.util

/**
 * Diagnostic output.
 * @author kuat
 */
object Debug {
  var DEBUG = false;
  var DEBUGFILE = false;

  // Whether we track the variable names that go with labels.
  var TRACE = true;

  val file: java.io.File = new java.io.File("output.txt");
 
  // if file doesnt exists, then create it
  if (!file.exists()) {
    file.createNewFile();
  }
 
  val fw: java.io.FileWriter = new java.io.FileWriter(file.getAbsoluteFile());
  val bw: java.io.BufferedWriter = new java.io.BufferedWriter(fw);
  // bw.write(content);
  //    bw.close();

  def debug(m: => String) = {
    if (DEBUGFILE) bw.write(m + "\n")
    if (DEBUG) 
      Console.err.println(Console.YELLOW + m + Console.RESET)
  }

  def warning(m: => String) =
    Console.out.println(Console.RED + m + Console.RESET)
}
