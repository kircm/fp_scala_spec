package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)

    val sxetTree =
      Fork(
        Fork(
          Fork(
            Leaf('e', 1),
            Leaf('t', 2),
            List('e', 't'),
            3),
          Leaf('x', 4),
          List('x', 'e', 't'),
          7),
        Leaf('s', 8),
        List('s', 'x', 'e', 't'),
        15)
    /*  F: fork   L: leaf

                                                     Fsxet(15)
                                                       |
                                             ---------------------
                                             |                   |
                                           Fxet(7)             Ls(8)
                                             |
                                   ---------------------
                                   |                   |
                                  Fet(3)              Lx(4)
                               ---------
                               |       |
                              Le(1)   Lt(2)
    */


  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of each char") {
    val result: List[(Char, Int)] = times(List('a', 'b', 'a', 'c', 'b', 'b'))
    print(result)
    assert(result.size == 3)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assert(makeOrderedLeafList(List(('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('x', 3)))
    assert(makeOrderedLeafList(List(('x', 3), ('t', 2), ('e', 1))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assert(makeOrderedLeafList(List(('c', 5), ('b', 3), ('a', 1), ('d', 6))) ===
      List(Leaf('a', 1), Leaf('b', 3), Leaf('c', 5), Leaf('d', 6)))
  }

  test("singleton") {
    assert(singleton(List(Leaf('a', 1))))
    assert(singleton(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))) == false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine a fork and some leaf list") {
    val treeList = List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))

    assert(combine(treeList) === List(
      Fork(
        Fork(// LEFT
          Leaf('e', 1), //left
          Leaf('t', 2), //right
          List('e', 't'), //chars
          3), //w
        Leaf('x', 4), // RIGHT
        List('x', 'e', 't'), // CHARS
        7))) // WEIGHT

    /*  F: fork   L: leaf
                                           Fxet(7)
                                             |
                                   ---------------------
                                   |                   |
                                  Fet(3)              Lx(4)
                               ---------
                               |       |
                              Le(1)   Lt(2)
    */
  }

  test("until") {
    new TestTrees {
      val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('s', 8))
      val result = until(singleton, combine)(leaflist)
      assert(result === sxetTree)
    }
  }

  test("create code tree") {
    new TestTrees {
      val chars = List('x', 'x', 's', 'x', 's', 's', 'x', 'e', 's', 't', 't', 's', 's', 's', 's')
      assert(createCodeTree(chars) === sxetTree)
    }
  }

  test("decode simple") {
    new TestTrees {
      // decoded: stst
      // encoded: 10011001
      val encoded: List[Bit] = List(1, 0, 0, 1, 1, 0, 0, 1)
      val decoded = decode(sxetTree, encoded)
      println(decoded)
      assert(decoded === List('s', 't', 's', 't'))
    }
  }

  test("decode") {
    new TestTrees {
      // decoded: ssssssexxxxtt
      // encoded: 11111100001010101001001
      val encoded: List[Bit] = List(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1)
      val decoded = decode(sxetTree, encoded)
      println(decoded)
      assert(decoded === List('s', 's', 's', 's', 's', 's', 'e', 'x', 'x', 'x', 'x', 't', 't'))
    }
  }

  test("decode secret") {
    println(decodedSecret)
  }

  test("encode simple") {
    new TestTrees {
      val expectedEncoded: List[Bit] = List(1, 0, 0, 1)
      val decoded = decode(sxetTree, expectedEncoded)
      println("decoded: " + decoded)

      val encoded = encode(sxetTree)(decoded)
      println("encoded: " + encoded)

      assert(encoded === expectedEncoded)
    }
  }

  test("encode") {
    new TestTrees {
      val expectedEncoded: List[Bit] = List(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1)
      val decoded = decode(sxetTree, expectedEncoded)
      println("decoded: " + decoded)

      val encoded = encode(sxetTree)(decoded)
      println("encoded: " + encoded)

      assert(encoded === expectedEncoded)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
