package com.github.kmizu.macro_peg.ruby

import org.scalatest.funsuite.AnyFunSuite

class GeneratedRubyParserSmokeTest extends AnyFunSuite {
  private def accepts(input: String): Boolean = {
    val result = GeneratedRubyParser.parseAll(input)
    if (result.isLeft) println(s"FAILED '${input.take(40)}': ${result.left.toOption.get.take(80)}")
    result.isRight
  }

  test("parses simple assignment") {
    assert(accepts("x = 1"))
  }

  test("parses method call") {
    assert(accepts("puts 'hello'"))
  }

  test("parses class definition") {
    assert(accepts("class Foo\ndef bar\n42\nend\nend"))
  }

  test("parses if statement") {
    assert(accepts("if x > 0\nx\nend"))
  }

  test("parses while loop") {
    assert(accepts("while true\nbreak\nend"))
  }

  test("parses binary expressions") {
    assert(accepts("x = 1 + 2 * 3"))
  }

  test("parses string literal") {
    assert(accepts("""x = "hello world""""))
  }

  test("parses array literal") {
    assert(accepts("x = [1, 2, 3]"))
  }

  test("parses hash literal") {
    assert(accepts("h = {a: 1, b: 2}"))
  }

  test("parses def with args") {
    assert(accepts("def add(a, b)\na + b\nend"))
  }

  test("parses do/end block") {
    assert(accepts("[1,2,3].each do |x|\nputs x\nend"))
  }

  test("parses empty program") {
    assert(accepts(""))
  }
}
