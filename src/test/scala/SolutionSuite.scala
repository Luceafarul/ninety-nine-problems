class SolutionSuite extends munit.FunSuite {
  test("last") {
    val xs = List(1, 2, 3, 4, 5)
    assertEquals(last(xs), 5)
  }

  test("lastRec") {
    val xs = List(1, 2, 3, 4, 5)
    assertEquals(lastRec(xs), 5)
  }

  test("penultimate") {
    val xs = List(1, 2, 3, 4, 5)
    assertEquals(penultimate(xs), Some(4))
  }

  test("nth") {
    val xs = List(1, 2, 3, 4, 5)
    assertEquals(nth(2, xs), Some(3))
  }

  test("length") {
    val xs = List(1, 2, 3, 4, 5)
    assertEquals(length(xs), 5)
  }

  test("length 0") {
    assertEquals(length(Nil), 0)
  }

  test("reverse") {
    val xs = List(1, 2, 3, 4, 5)
    assertEquals(reverse(xs), List(5, 4, 3, 2, 1))
  }

  test("reverse empty") {
    assertEquals(reverse(Nil), Nil)
  }

  test("isPalindrome") {
    assertEquals(isPalindrome(List(1, 2, 3, 2, 1)), true)
  }

  test("flatten") {
    val xs = List(List(1, 1), 2, List(3, List(5, 8)))
    assertEquals(flatten(xs), List(1, 1, 2, 3, 5, 8))
  }

  test("compress") {
    val xs = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    assertEquals(compress(xs), List('a', 'b', 'c', 'd', 'e'))
  }

  test("pack") {
    val xs = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    val expected = List(
      List('a', 'a', 'a', 'a'),
      List('b'),
      List('c', 'c'),
      List('a', 'a'),
      List('d'),
      List('e', 'e', 'e', 'e')
    )
    assertEquals(pack(xs), expected)
  }

  test("encode") {
    val xs       = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    val expected = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
    assertEquals(encode(xs), expected)
  }

  test("encodeModified") {
    val xs       = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    val expected = List((4, 'a'), 'b', (2, 'c'), (2, 'a'), 'd', (4, 'e'))
    assertEquals(encodeModified(xs), expected)
  }

  test("decode") {
    val xs       = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
    val expected = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    assertEquals(decode(xs), expected)
  }
}
