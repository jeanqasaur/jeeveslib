package cap.scalasmt

sealed trait ValFacet[T, TS] {
  def valCons (v: T): TS
  def facetCons(c: Formula, t: TS, f: TS): TS
}
object ValFacet {
  implicit object BoolValFacet extends ValFacet[Boolean, Formula] {
  def valCons (b) = BoolVal(b)
  def facetCons (c: Formula, t: Formula, f: Formula): Formula =
    BoolConditional (c, t, f)
  }
  implicit object IntValFacet extends ValFacet[BigInt, IntExpr] {
    def valCons (i) = IntVal(i)
    def facetCons (c: Formula, t: IntExpr, f: IntExpr): IntExpr =
    IntFacet (c, t, f)
  }
  implicit object ObjectValFacet
  extends ValFacet[Atom, ObjectExpr[Atom]] {
    def valCons (o) = Object (o)
    def facetCons (c: Formula, t: ObjectExpr[Atom], f: ObjectExpr[Atom])
      : ObjectExpr[Atom] = ObjectFacet (c, t, f)
  }
}

sealed trait FacetCons[T] {
  def facetCons(c:Formula, t: T, f: T): T
}
object FacetCons {
  implicit object BoolFacetCons extends FacetCons[Formula] {
    def facetCons (c: Formula, t: Formula, f: Formula): Formula = {
      BoolConditional (c, t, f)
    }
  }
  implicit object IntFacetCons extends FacetCons[IntExpr] {
    def facetCons (c: Formula, t: IntExpr, f: IntExpr): IntExpr =
      IntFacet (c, t, f)
  }
  implicit object ObjectFacetCons extends FacetCons[ObjectExpr[Atom]] {
    def facetCons (c: Formula, t: ObjectExpr[Atom], f: ObjectExpr[Atom])
      : ObjectExpr[Atom] = ObjectFacet (c, t, f)
  }
}
