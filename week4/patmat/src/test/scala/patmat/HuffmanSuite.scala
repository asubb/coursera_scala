package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    val t1: List[(Char, Int)] = times(List('A', 'B', 'A', 'a', 'a', 'c', 'a'))
    assert(t1.contains(('A', 2)))
    assert(t1.contains(('B', 1)))
    assert(t1.contains(('c', 1)))
    assert(t1.contains(('a', 3)))
    val t2: List[(Char, Int)] = times(List())
    assert(t2.isEmpty)
    val t3: List[(Char, Int)] = times(List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'))
    assert(t3.contains(('a', 12)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val com1: List[CodeTree] = combine(leaflist)
    assert(com1 === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    val com2: List[CodeTree] = combine(com1)
    assert(com2 === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t', 'x'), 7)))
  }

  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val res = until(singleton, combine)(leaflist);
    assert(res === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t', 'x'), 7))
  }

  test("createCodeTree") {
    val res = createCodeTree(string2Chars("xetxxtx"))
    assert(res === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t', 'x'), 7))
  }

  test("decodeSecret") {
     val res = decodedSecret
    assert(res === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("encode") {
    val res = encode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
    println(res)
    println(secret)
    assert(res === secret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t1, encode(t1)("abbbbabababa".toList)) === "abbbbabababa".toList)
      assert(decode(t2, encode(t2)("abbabbadddbbdaabba".toList)) === "abbabbadddbbdaabba".toList)
    }
  }

  test("codeBits") {
    assert(codeBits(List(
      ('a', List(0)),
      ('b', List(1))
    ))('a') === List(0))
    assert(codeBits(List(
      ('a', List(0)),
      ('b', List(1))
    ))('b') === List(1))
    assert(codeBits(List(
      ('a', List(0, 0)),
      ('b', List(0, 1)),
      ('d', List(1))
    ))('b') === List(0, 1))
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) === List(
        ('a', List(0)),
        ('b', List(1))
      ))
      assert(convert(t2) === List(
        ('a', List(0, 0)),
        ('b', List(0, 1)),
        ('d', List(1))
      ))
    }
  }

  test("decode and quickEncode text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t1, quickEncode(t1)("abbbbabababa".toList)) === "abbbbabababa".toList)
      assert(decode(t2, quickEncode(t2)("abbabbadddbbdaabba".toList)) === "abbabbadddbbdaabba".toList)
      assert(quickEncode(frenchCode)(decode(frenchCode, secret)) === secret)
    }
  }

}
