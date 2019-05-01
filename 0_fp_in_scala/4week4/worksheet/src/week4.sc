/**
  *
  * Objects Everywhere
  *
  **/

/**
  * Implementing a pure object version of the boolean primitive
  */
object MyFalse extends MyBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = e
}

object MyTrue extends MyBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = t
}

abstract class MyBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def <(otherBool: MyBoolean): MyBoolean = ifThenElse(MyFalse, otherBool)
}


MyTrue.ifThenElse("a", 3)
MyFalse.ifThenElse("a", 3)


MyTrue < MyFalse
MyFalse < MyTrue

MyTrue < MyTrue
MyFalse < MyFalse


/**
  * Implementing a pure object version of Natural numbers
  */
abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  // Better implementation than the
  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  def toInt(): Int
}



object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new IllegalStateException

  // Marc's initial implementation
  //  override def successor: Nat = new Succ(Zero)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new IllegalStateException

  override def toInt(): Int = 0
}



class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  // Marc's initial implementation
  //  override def successor: Nat = new Succ(new Succ(n))

  override def +(that: Nat): Nat = {
    if (that.isZero) this
    else this.successor + that.predecessor
  }

  override def -(that: Nat): Nat = {
    if (that.isZero) this
    else this.predecessor - that.predecessor
  }

  override def toInt(): Int = 1 + predecessor.toInt()
}


val oneNat: Nat = Zero.successor
val two = oneNat.successor.toInt
val three = new Succ(new Succ(new Succ(Zero))).toInt
val four = new Succ(new Succ(new Succ(new Succ(Zero)))).toInt


val twoSucc = Zero.successor.successor
val oneSucc = Zero.successor
(oneSucc + twoSucc).toInt  // 3
(oneSucc + Zero).toInt     // 1

val threeSucc = Zero.successor.successor.successor
(twoSucc - Zero).toInt // 2
(threeSucc - twoSucc).toInt // 1









