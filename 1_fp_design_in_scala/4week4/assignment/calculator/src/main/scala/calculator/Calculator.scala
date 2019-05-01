package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    var computedValues = scala.collection.mutable.Map[String, Signal[Double]]()
    namedExpressions.map(e => {
      val name = e._1
      val sigExpr = e._2
      val expr = sigExpr()

      val res: Signal[Double] = evaluateExpression(expr, namedExpressions - name)

      computedValues += (name -> res)
    })

    computedValues.toMap
  }

  private def evaluateExpression(expr: Expr,
                                 references: Map[String, Signal[Expr]]): Signal[Double] = eval(expr, references)

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Signal[Double] = expr match {
    case Literal(v) => Signal(v)

    case Ref(n) => eval(getReferenceExpr(n, references), references - n)

    case Plus(a, b) => Var(eval(a, references)() + eval(b, references)())

    case Minus(a, b) => Var(eval(a, references)() - eval(b, references)())

    case Times(a, b) => Var(eval(a, references)() * eval(b, references)())

    case Divide(a, b) => Var(eval(a, references)() / eval(b, references)())
  }

  /**
    * Get the Expr for a referenced variable.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]): Expr = {

    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
