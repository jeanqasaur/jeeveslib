package cap.jeeveslib.smt

import java.io._

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.Zeros._
import cap.jeeveslib.env.{DefaultEnv, VarEnv}
import cap.jeeveslib.util.Debug._

/* 
 * Expression translators to SMT-LIB2.
 * @author kuat
 */
object SMT {
  private def variable(v: Var[_])(implicit env: VarEnv) =
    if (env.has(v))
      env(v).toString
    else
      v.toString

  private def formula(f: FExpr[Boolean])(implicit env: VarEnv, sc: Scope)
    : String = f match {
    case And(a,b) => "(and " + formula(a) + " " + formula(b) + ")"
    case Or(a,b) => "(or " + formula(a) + " " + formula(b) + ")"
    case Not(a) => "(not " + formula(a) + ")"
    case BoolEq(a,b) => "(= " + formula(a) + " " + formula(b) + ")"
    case BoolVal(v) => v.toString
    case IntEq(a,b) => "(= " + integer(a) + " " + integer(b) + ")"
    case Leq(a,b) => "(<= " + integer(a) + " " + integer(b) + ")"
    case Geq(a,b) => "(>= " + integer(a) + " " + integer(b) + ")" 
    case LT(a,b) => "(< " + integer(a) + " " + integer(b) + ")"
    case GT(a,b) => "(> " + integer(a) + " " + integer(b) + ")"  
    case BoolFacet(c,a,b) => "(if " + formula(c) + " " + 
      formula(a) + " " + formula(b) + ")" 
    case ObjectEq(a,b) => "(= " +  atom(a) + " " + atom(b) + ")"
    case RelEq(a,b) => "(forall ((x Object)) (iff " + 
      set(a)("x", env, sc) + " " + set(b)("x", env, sc) + "))"
    case RelSub(a,b) => "(forall ((x Object)) (=> " + 
      set(a)("x", env, sc) + " " + set(b)("x", env, sc) + "))"
    case v: BoolVar => variable(v)
  }

  private def integer(e: FExpr[BigInt])(implicit env: VarEnv, sc: Scope)
  : String = e match {
    case Plus(a,b) => "(+ " + integer(a) + " " + integer(b) + ")"
    case Minus(a,b) => "(- " + integer(a) + " " + integer(b) + ")"
    case Times(a,b) => "(* " + integer(a) + " " + integer(b) + ")"
    case IntFacet(c,a,b) => "(if " + formula(c) + " " + integer(a) + " " + integer(b) + ")"
    case IntVal(i) => if (i >= 0) i.toString else "(- " + i.abs.toString + ")"
    case ObjectIntField(root, f) => "(" + f + " " + atom(root) + ")"
  }

  private def atom(e: FExpr[Atom])(implicit env: VarEnv, sc: Scope): String =
  e match {
    case ObjectFacet(cond, thn, els) => "(if " + formula(cond) + " " + atom(thn) + " " + atom(els) + ")"
    case Object(o) => sc.encode(o)
    case ObjectField(root, f) => "(" + f + " " + atom(root) + ")"
  }

  private def set(e: FExpr[Set[Atom]])
    (implicit q: String, env: VarEnv, sc: Scope): String = e match {
    case Union(a,b) => "(or " + set(a) + " " + set(b) + ")"
    case Diff(a,b) => "(and " + set(a) + " (not " + set(b) + "))"
    case Intersect(a,b) => "(and " + set(a) + " " + set(b) + ")"
    case Singleton(o) => "(= " + q + " " + atom(o) + ")"
    case ObjectSet(os) => if (os.size == 0) "false" else "(or " + os.map("(= " + q + " " + sc.encode(_) + ")").mkString(" ") + ")"
    case RelJoin(root, f) => 
      val r = q + "0";
      "(exists ((" + r + " Object)) (and (= " + q + 
        " (" + f + " " + r + ")) " + set(root)(r, env, sc) + "))"
  }

  private def sanitize(s: String) = 
    "|" + s.replace("|",".").replace("\\", ".") + "|"

  /* Bounded universe of atoms (and their fields). */
  private object Scope {
    implicit def fromField(f: FieldDesc[_]) = Scope(fields = Set(f))
    implicit def fromVar(v: Var[_]) = Scope(vars = Set(v))
    implicit def fromAtom(o: Atom) = Scope(objects = Set(o))
    implicit def fromAtomSet(o: Set[Atom]) = Scope(objects = o)
  }

  private case class Scope(objects: Set[Atom] = Set(), 
                       fields: Set[FieldDesc[_]] = Set(), 
                       vars: Set[Var[_]] = Set()) {
    def ++ (that: Scope) = Scope(
      this.objects ++ that.objects, 
      this.fields ++ that.fields, 
      this.vars ++ that.vars
    )

    lazy val ENCODING: List[(Atom, String)] = {
      val result = objects.toList.map(o =>
        (o, if (o == null) "|null|" else sanitize(o.toString)))
      if (result.map(_._2).toSet.size != objects.size)
        throw SolverException("atom name collision detected")
      result
    }

    def encode(o: Atom) = 
      ENCODING.find(_._1 == o) match {
        case Some(p) => p._2
        case _ => throw SolverException("cannot encode atom " + o)
      }

    def decode(s: String) = 
      ENCODING.find(_._2 == (
        if (s.startsWith("|")) 
          s 
        else 
          "|" + s + "|")) match {
        case Some(p) => p._1
        case _ => throw SolverException("cannot decode atom " + s)
      }
    
    def classId(klas: Class[_]): String = 
      if (klas == null) "Null" else sanitize(klas.getName)
   
    def atomClassId(o: Atom): String = 
      if (o == null) classId(null) else classId(o.getClass)

    def size = objects.size + fields.size + vars.size
    
    override def toString =
      objects.size + " objects; " + fields.size + " fields; " +  vars.size +
        " vars"
  }

  /**
   * Returns the set of variables in the "universe" of the current function: the
   * objects involved in the current computation.
   *
   * This computation is necessary because we need to capture all objects relevant
   * to the objects involved in our constraints.
   */
  private def univ(f: FExpr[_])(implicit env: VarEnv): Scope = {
    (f: @unchecked) match {
      case f: BinaryExpr[_] => univ(f.left) ++ univ(f.right)
      case f: Ite[_] => univ(f.cond) ++ univ(f.thn) ++ univ(f.els)
      case f: UnaryExpr[_] => univ(f.sub)
      case v: Var[_] => v
      case os: ObjectSet => os.eval
      case o: Object[_] => o.eval
      case RelJoin(root, f) => univ(root) ++ f
      case ObjectIntField(root, f) => univ(root) ++ f
      case ObjectField(root, f) => univ(root) ++ f
      case _: Constant[_] => Scope()
    }
  }

  /**
   * Iterate over variables we need until we get to a fixed point.
   */
  @annotation.tailrec 
  private def closure(cur: Scope)(implicit env: VarEnv): Scope = {
    // Gets the fields of each object.
    val fields = for (o <- cur.objects; f <- cur.fields) yield f(o)

    // Adds the values mapped to variables in the scope.
    val variables = {for (v <- cur.vars; if env.has(v)) yield v match {
      case _ => Set[Atom]()
    }}.flatten

    val more = cur ++ 
      (null: Atom) ++ 
      variables ++ 
      fields.map(univ(_)).foldLeft(Scope())(_ ++ _)

    if (more.size > cur.size)
      closure(more)
    else 
      cur
  }
  
  /** Encoding of the universe as a prelude. */
  private def prelude(implicit env: VarEnv, sc: Scope): List[String] = {
    val Scope(objects, fields, vars) = sc;

    // declare all objects
    "(declare-datatypes () ((Object " + objects.map(sc.encode(_)).mkString(" ") + ")))" :: 
    // declare all fields
    {for (f <- fields) yield "(declare-fun " + f + {f match {
      case _: ObjectFieldDesc => " (Object) Object)"
      case _: IntFieldDesc => " (Object) Int)"
    }}}.toList :::
    // declare all variables
    {for (v <- vars; if ! env.has(v))
      yield "(declare-fun " + v + {v match {
        case _: BoolVar => " () Bool)"
//        case _: ObjectVar[_] => " () Object)"
//        case _: ObjectSetVar => " (Object) Bool)"
    }}}.toList :::
    // declare types
    "(declare-datatypes () ((Type " + objects.map(sc.atomClassId(_)).mkString(" ") + ")))" ::
    "(declare-fun $type (Object) Type)" ::
    {for (o <- objects) yield "(assert (= ($type " + sc.encode(o) + ") " + sc.atomClassId(o) + "))"}.toList:::
    // declare field values
    {for (o <- objects; f <- fields) 
      yield "(assert (= (" + f + " " + sc.encode(o) + ") " + {f match {
        case f: ObjectFieldDesc => atom(f(o))
        case f: IntFieldDesc => integer(f(o))
      }} + 
    "))"}.toList :::
    Nil
  }

  /**
   * Solves for an assignment to satisfy the formula.
   * Some variables might be left without assignment.
   * 
   * Super-normal default logic is decided to saturating context in the order
   * the judgements are supplied.
   * 
   * Initial scope of objects is used to make sound equality theory for objects.
   */
  def solve(f: Formula, defaults: List[Formula] = Nil
    , initial: Set[Atom] = Set())
    (implicit env: VarEnv = DefaultEnv): Option[VarEnv] = {
    // Compute the scope of our current objects that is the transitive closure of the
    // objects mentioned in our current set of values.
    implicit val scope = closure(univ(f :: defaults) ++ initial)

    debug("\n *** SMT SOLVING STATISTICS *** ")
    debug("SCOPE: " + scope);
    debug("# DEFAULTS: " + defaults.size)
    debug("INITIAL SCOPE: " + initial.size)
    debug("# FORMULA CLAUSES: " + f.clauses.size)

    implicit var solver = new Z3 with Logging with SolverDescription;

    val start = System.currentTimeMillis
 
    try {
      for (s <- prelude) 
        solver.command(s)  
     
      for (clause <- f.clauses) 
        solver.assert(formula(clause))
      
      if (! solver.check())
        throw UnsatException
      
      // invariant: the model is consistent 
      for (d <- defaults) {
        solver.push();
        solver.assert(formula(d));
        if (! solver.check()) {
          solver.pop();
        }
      }
      assert(solver.check())
     
      debug(" *** SUCCESS SOLVING TIME: " + (System.currentTimeMillis - start) + " ms")
      
      // retrieve values for the variables      
      var result = env;
    
      for (v <- scope.vars; if (! env.has(v))) {
        val out = solver.eval(variable(v));
        // pattern ((v _))
        val value = out.substring(out.indexOf(" ") + 1, out.length() - 2)
        v match {
          case v: BoolVar => 
            result = result + (v -> value.toBoolean);
        }
      }

      Some(result);
    } catch {
      case UnsatException =>
        debug(" *** FAILED SOLVING TIME: " + (System.currentTimeMillis - start) + " ms")
        None;
      case e: Throwable => 
        throw e;
    } finally {
      solver.close()
    }
  }   
}
