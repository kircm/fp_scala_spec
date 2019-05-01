import java.util.NoSuchElementException

/**
  *
  * Class hierarchies
  *
  **/
abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  /** Implement union method **/
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  override def toString = "."

  /** Implement union method **/
  override def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{" + left + elem + right + "}"

  /** Implement union method **/
  override def union(other: IntSet): IntSet =
  ((left union right) union other) incl elem
}


val t1 = new NonEmpty(3, new Empty, new Empty)
val t2 = t1 incl 4


/**
  *
  * Polymorphism
  *
  **/
trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: Nothing = throw new NoSuchElementException
}


/** Implement ntgh function **/
def nthMarc[T](n: Int, l: List[T]): T = {
  def nthIdx[T](n: Int, l: List[T], i: Int): T = {
    if (l.isEmpty) throw new IndexOutOfBoundsException
    if (n == i) l.head
    else nthIdx(n, l.tail, i + 1)
  }

  nthIdx(n, l, 0)
}


val listMarc = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nthMarc(2, listMarc)


// Better (video):
object nth {
  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }


  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  nth(2, list)
}



