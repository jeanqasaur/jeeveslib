package cap.jeeveslib.ast

import cap.jeeveslib.ast.Zeros._
import cap.jeeveslib.env._

/*
 * A DSL for logical constraints.
 * @author kuat
 */

case object Impossible extends RuntimeException
case object Unimplemented extends RuntimeException
case class Unexpected(msg: String) extends RuntimeException

/**
 * Expressions.
 */
sealed trait Expr[T] {
  def vars: Set[Var[_]]
  def eval(implicit env: VarEnv = EmptyEnv): T
  def ===(that: Expr[T]): Expr[Boolean]
  def default: T
  def show: T = throw Unexpected("show: " ++ this.toString)
}
sealed trait Ite[T] extends Expr[T] {
  def cond: Expr[Boolean]
  def thn: Expr[T]
  def els: Expr[T]
  def vars = cond.vars ++ thn.vars ++ els.vars
  def eval(implicit env: VarEnv)= if (cond.eval) thn.eval else els.eval
}
sealed trait Var[T] extends Expr[T] {
  def id: String
  def vars: Set[Var[_]] = Set(this)
  def eval(implicit env: VarEnv) = env(this)
}
object Var {
  private var COUNTER = 0
  private def inc() = {COUNTER = COUNTER + 1; COUNTER.toString}
  def makeBool = BoolVar(inc())
} 
sealed trait BinaryExpr[T <: Expr[_]] {
  assert (left != null)
  assert (right != null)
  def left: T
  def right: T
  def vars = left.vars ++ right.vars
}
sealed trait UnaryExpr[T <: Expr[_]] {
  assert (sub != null)
  def sub: T
  def vars = sub.vars
}
sealed trait Eq[T <: Expr[_]] extends Expr[Boolean] with BinaryExpr[T] {
  def eval(implicit env: VarEnv) = left.eval == right.eval 
}
sealed trait Constant[T] extends Expr[T] {
  protected def v: T
  def vars = Set()
  def eval(implicit env: VarEnv) = v
} 

/** 
 * Boolean expressions and algebra.
 */
sealed abstract class Formula extends Expr[Boolean] {
  def ===(that: Expr[Boolean]): Formula = 
    that match {
      case that: Formula => BoolEq(this, that)
    }
  def constant(t: Boolean) = BoolVal(t)
  def default = zero[Boolean]
  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  def ==> (that: Formula) = Not(this) || that
  def <==> (that: Formula) = ===(that)
  def unary_! = Not(this)
  def ?(thn: Formula) =
    new {def !(els: Formula) = BoolFacet(Formula.this, thn, els)}
  def ?(thn: IntExpr) = new {def !(els: IntExpr) =
    IntFacet(Formula.this, thn, els)}
  def ?[T >: Null <: Atom](thn: ObjectExpr[T]) =
    new {def !(els: ObjectExpr[T]) =
      ObjectFacet(Formula.this, thn, els)}
  // TODO: Define more function expressions...
  def ?[A, B](thn: FunctionExpr[A, B]) =
    new {def !(els: FunctionExpr[A, B]) =
      FunctionFacet(Formula.this, thn, els)}

  def clauses: List[Formula] = this match {
    case And(a,b) => a.clauses ++ b.clauses
    case _ => this :: Nil
  }
}
sealed abstract class BinaryFormula extends Formula with BinaryExpr[Formula] 
case class And(left: Formula, right: Formula) extends BinaryFormula {
  def eval(implicit env: VarEnv) = left.eval && right.eval
}
case class Or(left: Formula, right: Formula) extends BinaryFormula {
  def eval(implicit env: VarEnv) = left.eval || right.eval
}
case class BoolFacet(cond: Formula, thn: Formula, els: Formula) extends Formula with Ite[Boolean]
case class Not(sub: Formula) extends Formula with UnaryExpr[Formula] {
  def eval(implicit env: VarEnv) = ! sub.eval
}

/**
 * Atomic predicates.
 */
sealed abstract class IntFormula extends Formula with BinaryExpr[IntExpr] 
case class Leq(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: VarEnv) = left.eval <= right.eval
}
case class Geq(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: VarEnv) = left.eval >= right.eval
}
case class LT(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: VarEnv) = left.eval < right.eval
}
case class GT(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: VarEnv) = left.eval > right.eval
}
sealed abstract class RelFormula extends Formula with BinaryExpr[RelExpr]
case class RelSub(left: RelExpr, right: RelExpr) extends RelFormula {
  def eval(implicit env: VarEnv) = left.eval.subsetOf(right.eval)
}
case class BoolVal(v: Boolean) extends Formula with Constant[Boolean]
case class BoolVar(id: String) extends Formula with Var[Boolean] {
  override def toString = "b" + id
}
/** 
 * Equality atomic predicates.
 */
case class BoolEq(left: Formula, right: Formula) extends Formula with Eq[Formula]
case class IntEq(left: IntExpr, right: IntExpr) extends IntFormula with Eq[IntExpr] 
case class ObjectEq(left: ObjectExpr[Atom], right: ObjectExpr[Atom]) extends Formula with Eq[ObjectExpr[Atom]]
case class RelEq(left: RelExpr, right: RelExpr) extends RelFormula with Eq[RelExpr]

/**
 * Integer expression with Peano arithmetic.
 */
sealed abstract class IntExpr extends Expr[BigInt] {
  def ===(that: Expr[BigInt]): Formula = 
    that match {case that: IntExpr => IntEq(this, that)}
  def constant(t: BigInt) = IntVal(t)
  def default = zero[BigInt]
  def !==(that: IntExpr) = ! (this === that)
  def <=(that: IntExpr) = Leq(this, that)
  def >=(that: IntExpr) = Geq(this, that)
  def <(that: IntExpr) = LT(this, that)
  def >(that: IntExpr) = GT(this, that)
  def unary_- = Minus(IntVal(0), this)
  def +(that: IntExpr) = Plus(this, that)
  def -(that: IntExpr) = Minus(this, that)
  def *(that: IntExpr) = Times(this, that)
}
sealed abstract class BinaryIntExpr extends IntExpr with BinaryExpr[IntExpr]
case class Plus(left: IntExpr, right: IntExpr) extends BinaryIntExpr {
  def eval(implicit env: VarEnv) = left.eval + right.eval
}
case class Minus(left: IntExpr, right: IntExpr) extends BinaryIntExpr {
  def eval(implicit env: VarEnv) = left.eval - right.eval
}
case class Times(left: IntExpr, right: IntExpr) extends BinaryIntExpr {
  def eval(implicit env: VarEnv) = left.eval * right.eval
}
case class IntFacet(cond: Formula, thn: IntExpr, els: IntExpr)
extends IntExpr with Ite[BigInt]
case class IntVal(v: BigInt) extends IntExpr with Constant[BigInt]
case class ObjectIntField(root: ObjectExpr[Atom], f: FieldDesc[BigInt])
  extends IntExpr {
  def vars = root.vars // + IntVar("global" + f)
  def eval(implicit env: VarEnv) = f(root.eval).eval
}

sealed abstract class FunctionExpr[A, B] extends Expr[A => B] {
  def default: (A => B) = throw Impossible
  def ===(that: Expr[A => B]): Expr[Boolean] = BoolVal(this == that)
}
case class FunctionVal[A, B](v: A => B)
extends FunctionExpr[A, B] with Constant[A => B] {
  def app (arg: A): B = v (arg)
}
case class FunctionFacet[A, B](
  cond: Formula, thn: FunctionExpr[A,B], els: FunctionExpr[A,B])
extends FunctionExpr[A, B] with Ite[A => B]

/**
 * Object and field expressions.
 */

/* Atom is uniquely identified by its string representation.*/
trait Atom extends AnyRef
sealed abstract class ObjectExpr[+T >: Null <: Atom]
  extends Expr[Atom] with Dynamic { 
  def ===(that: Expr[Atom]): Formula = 
    that match {case that: ObjectExpr[_] => ObjectEq(this, that)}
  def !==(that: Expr[Atom]) = ! (this === that)
  def constant(t: Atom) = Object(t)
  def default = zero[Atom]

  def applyFunction(f: T => Formula): Formula
  def applyFunction(f: T => IntExpr): IntExpr
  def applyFunction[T2 >: Null <: Atom](f: T => ObjectExpr[T2]): ObjectExpr[T2]

  def ~(name: Symbol) = ObjectIntField(this, IntFieldDesc(name.name))
  def applyDynamic(name: String)(args: Any*) = {
    assert(args.length == 0)
    ObjectField(this, ObjectFieldDesc(name))
  }
}

case class ObjectFacet[+T >: Null <: Atom](
  cond: Formula, thn: ObjectExpr[T], els: ObjectExpr[T])
  extends ObjectExpr[T] with Ite[Atom]  {
    def applyFunction(f: T => Formula): Formula =
      BoolFacet(cond, thn.applyFunction(f), els.applyFunction(f))
    def applyFunction(f: T => IntExpr): IntExpr =
      IntFacet(cond, thn.applyFunction(f), els.applyFunction(f))
    def applyFunction[T2 >: Null <: Atom](f: T => ObjectExpr[T2])
      : ObjectExpr[T2] = {
      ObjectFacet(cond
        , if (thn == null) null else thn.applyFunction[T2](f)
        , if (els == null) null else els.applyFunction(f))
    }
    override def eval(implicit env: VarEnv): T =
      if (cond.eval) {
        if (thn == null) null else thn.eval.asInstanceOf[T]
      } else {
        if (els == null) null else els.eval.asInstanceOf[T]
      }
}
case class Object[+T >: Null <: Atom](v: T)
  extends ObjectExpr[T] with Constant[Atom]  {
  def applyFunction(f: T => Formula): Formula =
    if (v == null) null else f(v)
  def applyFunction(f: T => IntExpr): IntExpr = if (v == null) null else f(v)
  def applyFunction[T2 >: Null <: Atom](f: T => ObjectExpr[T2])
  : ObjectExpr[T2] = if (v == null) null else f(v)
}
case class ObjectField(root: ObjectExpr[Atom], f: FieldDesc[Atom])
  extends ObjectExpr[Atom] {
  def vars = root.vars // + ObjectVar[Atom]("global" + f)
  def eval(implicit env: VarEnv) = f(root.eval).eval

  def applyFunction(f: Atom => Formula): Formula =
    throw Unexpected ("applyFunction for ObjectField")
  def applyFunction(f: Atom => IntExpr): IntExpr =
    throw Unexpected ("applyFunction for ObjectField")
  def applyFunction[T2 >: Null <: Atom](f: Atom => ObjectExpr[T2])
    : ObjectExpr[T2] =
    throw Unexpected ("applyFunction for ObjectField")
}

/**
 * Object fields.
 */
sealed trait FieldDesc[T] {
  def id: String
  def apply(o: Atom): Expr[T]
  /** Read from the heap.*/
  def read(o: Atom): Option[AnyRef] = 
    if (o == null) None 
    else try {
      val fid = o.getClass.getDeclaredField(id);
      fid.setAccessible(true);
      try Some(fid.get(o))  
      finally fid.setAccessible(false);
    } catch {
      case _: NoSuchFieldException => None 
    }
}
case class ObjectFieldDesc(id: String) extends FieldDesc[Atom] {
  def apply(o: Atom) = read(o) match {
    case Some(t: Atom) => Object(t)
    case Some(o: ObjectExpr[_]) => o
    // Default value for objects is the null.
    case _ => Object(zero[Atom])
  }
  override def toString = "O" + id
}
case class IntFieldDesc(id: String) extends FieldDesc[BigInt] {
  def apply(o: Atom) = read(o) match {
    case Some(t: BigInt) => IntVal(t)
    case Some(e: IntExpr) => e
    // Default value for integers is 0.
    case _ => IntVal(zero[BigInt])
  }
  override def toString = "I" + id
}

/**
 * Relational algebra.
 */
sealed abstract class RelExpr extends Expr[Set[Atom]] with Dynamic {
  def ===(that: Expr[Set[Atom]]): Formula = 
    that match {case that: RelExpr => RelEq(this, that)}
  def constant(t: Set[Atom]) = ObjectSet(t)
  def default = zero[Set[Atom]]
  def in(that: RelExpr) = RelSub(this, that)
  def &(that: RelExpr) = Intersect(this, that)
  def ++(that: RelExpr) = Union(this, that)
  def --(that: RelExpr) = Diff(this, that)
  def applyDynamic(name: String)(args: Any*) = {
    assert(args.length == 0)
    RelJoin(this, ObjectFieldDesc(name))
  } 
}
case class Singleton(sub: ObjectExpr[Atom])
  extends RelExpr with UnaryExpr[ObjectExpr[Atom]] {
  def eval(implicit env: VarEnv) = Set(sub.eval)
}
case class ObjectSet(v: Set[Atom]) extends RelExpr with Constant[Set[Atom]] 
case class RelJoin(root: RelExpr, f: FieldDesc[Atom]) extends RelExpr {
  def vars = root.vars
  def eval(implicit env: VarEnv) = (for (o <- root.eval) yield f(o).eval)
}
sealed abstract class BinaryRelExpr extends RelExpr with BinaryExpr[RelExpr]
case class Union(left: RelExpr, right: RelExpr) extends BinaryRelExpr {
  def eval(implicit inv: VarEnv) = left.eval ++ right.eval
}
case class Diff(left: RelExpr, right: RelExpr) extends BinaryRelExpr {
  def eval(implicit inv: VarEnv) = left.eval -- right.eval
}
case class Intersect(left: RelExpr, right: RelExpr) extends BinaryRelExpr {
  def eval(implicit inv: VarEnv) = left.eval & right.eval
}

/**
 * Implicit conversions.
 */
object Expr {
  implicit def fromBigInt(i: BigInt) = IntVal(i)  
  implicit def fromInt(i: Int) = IntVal(i)
  implicit def fromBool(b: Boolean) = BoolVal(b)
  implicit def fromAtom[T >: Null <: Atom](o: T) = Object(o)
  implicit def fromIntExprs(vs: Traversable[IntExpr]) = IntExprs(vs)
  implicit def fromAtomExprs(vs: Traversable[ObjectExpr[Atom]]) = AtomExprs(vs)
  implicit def fromAtoms[T >: Null <: Atom](vs: Traversable[T]) = Atoms[T](vs)
  implicit def fromString(s: String) = Object(S(s))
}
object Formula {
  implicit def fromList(vs: Traversable[Formula]) = vs.foldLeft(true: Formula)(_ && _)
}
object RelExpr {
  implicit def fromRef(o: Atom) = Singleton(Object(o))
  implicit def fromSingleton(o: ObjectExpr[Atom]) = Singleton(o)
  implicit def fromRefSet(s: Traversable[_ <: Atom]) = ObjectSet(s.toSet)
}
object `package` {
  implicit def fromObjectField(f: ObjectField) = f match {
    case ObjectField(root, ObjectFieldDesc(name)) => ObjectIntField(root, IntFieldDesc(name))
  }
  def DISTINCT[T <% IntExpr](vs: Traversable[T]) = 
    for (vs1 <- vs; vs2 <- vs; if (vs1 != vs2)) yield ( ! (vs1 === vs2))
  def NULL = Object(null)
  def OR(vs: Traversable[Formula]) = vs.foldLeft(false: Formula)(_ || _)
  def AND(vs: Traversable[Formula]) = vs.foldLeft(true: Formula)(_ && _)
}


/** Lists of symbolic values. */
case class IntExprs[T >: Null <: IntExpr](vs: Traversable[T]) {
  def has(i: IntExpr): Formula = OR(for (v <- vs) yield i === v)
  def hasFormula(f: IntExpr => Formula): Formula =
    OR(for (v <- vs) yield f(v))
}
case class Atoms[T >: Null <: Atom](vs: Traversable[T]) extends Atom {
  def has(i: ObjectExpr[Atom]): Formula = OR(for (v <- vs) yield i === v)
  def hasFormula(f: ObjectExpr[Atom] => Formula): Formula =
    OR(for (v <- vs) yield f(v))
  def filter(f: T => Formula) = 
    vs.map(o => IF (f(o)) {o} ELSE {NULL})
}
case class AtomExprs(vs: Traversable[ObjectExpr[Atom]]) {
  def has(i: ObjectExpr[Atom]) = OR(for (v <- vs) yield i === v)
  def hasFormula(f: ObjectExpr[Atom] => Formula): Formula =
    OR(for (v <- vs) yield f(v))
  def filter(f: ObjectExpr[Atom] => Formula) = 
    vs.map(o => IF (f(o)) {o} ELSE {null})
}
/** Conditional expression constructors. */
case class IF(cond: Formula) {
  def apply(thn: IntExpr) = 
    new {def ELSE(els: IntExpr) = cond ? thn ! els}
  def apply(thn: ObjectExpr[Atom]) = 
    new {def ELSE(els: ObjectExpr[Atom]) = cond ? thn ! els}
}
/** String expression constructors. */
case class S(s: String) extends Atom
