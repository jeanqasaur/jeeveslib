package cap.jeeveslib.smt

import java.io._

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.Zeros._
import cap.jeeveslib.env.{DefaultEnv, VarEnv}
import cap.jeeveslib.util.Debug._

/* 
 * Translator to SMT-LIB2.
 * @author kuat
 */
class Z3 extends Solver {
  import java.io._

  private def BINARY = Option(System.getProperty("smt.home")) match {
    case Some(path) => path
    case None => System.getProperty("user.home") + "/opt/z3/bin/z3"
  }

  private def PARAMS ="-smt2" :: "-in" :: Nil
  
  private var process = {
    val pb = new ProcessBuilder((BINARY :: PARAMS).toArray: _*);
    pb.redirectErrorStream(true);
    pb.start;
  }
  
  private var input = new BufferedWriter(new OutputStreamWriter(process.getOutputStream));
  private var output = new BufferedReader(new InputStreamReader(process.getInputStream));
  
  override def >(s: String) = {input.write(s); input.newLine; input.flush}
  override def <() = output.readLine 
  
  command("(set-option :print-success true)")
  command("(set-option :produce-models true)")
  command("(set-option :elim-quantifiers true)")
  command("(set-option :mbqi false)")
  command("(set-option :auto-config false)") // disable saturation engine, use all theories
  command("(set-option :ematching false)")

  override def close() {
    input.close; 
    output.close; 
    process.destroy;
  }
}
