package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      n <- oneOf(const(empty), genHeap)
    } yield insert(x, n)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findmin1") = {
    findMin(insert(1, empty)) == 1
  }

  property("findmin12") = {
    findMin(insert(1, insert(2, empty))) == 1
  }

  property("findmin21") = {
    findMin(insert(2, insert(1, empty))) == 1
  }

  property("findmin213") = {
    findMin(insert(2, insert(1, insert(3, empty)))) == 1
  }

  property("delete") = {
    deleteMin(insert(3, empty)) == empty
  }

  property("delete2") = {
    deleteMin(insert(3, insert(1, empty))) == insert(3, empty)
  }

  property("size") = {
    def sizeH(currentHeap: H, count: Int): Int = {
      if (isEmpty(currentHeap)) count
      else {
        sizeH(deleteMin(currentHeap), count + 1)
      }
    }

    sizeH(insert(1, insert(1, empty)), 0) == 2
  }

  property("sequence") = forAll { (h: H) =>
    def sequence(currentHeap: H, listElements: List[Int]): List[Int] = {
      if (isEmpty(currentHeap)) listElements
      else {
        val min = findMin(currentHeap)
        sequence(deleteMin(currentHeap), min :: listElements)
      }
    }

    val sq = sequence(h, List.empty[Int])
    sq.sorted == sq.reverse
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) {
      isEmpty(h1) || isEmpty(h2)
    } else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)

      val melded = meld(h1, h2)
      val minMelded = findMin(melded)

      if (minMelded == min1) min2 >= min1
      else minMelded == min2 && min1 >= min2
    }
  }

  property("melding and deleting") = {
    val h1 = insert(5, insert(2, empty))
    val h2 = insert(9, insert(1, insert(3, empty)))
    val h3 = deleteMin(meld(h1, h2))

    findMin(h3) == 2
  }
}
