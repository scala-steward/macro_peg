package com.github.kmizu.macro_peg

import com.github.kmizu.macro_peg.codegen.ParserGenerator
import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec

class ParserGeneratorSpec extends AnyFunSpec with Diagrams {
  describe("ParserGenerator") {
    it("generates parser code for first-order grammar") {
      val grammar = Parser.parse("S = \"a\" \"b\";")
      val generated = ParserGenerator.generate(grammar)
      assert(generated.isRight)
      val code = generated.toOption.get
      assert(code.contains("object GeneratedParser"))
      assert(code.contains("lazy val r_S"))
      assert(code.contains("def parseAll"))
    }

    it("generates interpreter-backed code for grammars with macro parameters") {
      val grammar = Parser.parse("S = F(\"a\"); F(x) = x;")
      val generated = ParserGenerator.generate(grammar)
      assert(generated.isRight)
      val code = generated.toOption.get
      assert(code.contains("Interpreter.fromSourceEither"))
      assert(code.contains("def evaluate"))
      assert(code.contains("strategy: EvaluationStrategy"))
      assert(code.contains("F(x) = x;"))
    }

    it("generates code where :ign removes element from ~ tuple (right side ignored)") {
      // "hello":ign "world" — action sees only "world", not ("hello" ~ "world")
      val source = """
%object TestIgn;
%start foo;
foo = "hello":ign "world" => { w => w } ;
"""
      val result = ParserGenerator.generateFromSource(source)
      assert(result.isRight, result.left.toOption.getOrElse(""))
      val code = result.toOption.get
      // Should use projection (map with _ for ignored side), NOT new ~
      assert(!code.contains("new ~(_r"), "ignored element should not appear in ~ pair")
    }

    it("generates code where :ign removes element from ~ tuple (left side ignored)") {
      // "open":ign content "close":ign — action sees only content
      val source = """
%object TestIgn2;
%start bar;
bar = "(" :ign [a-z]+ ")" :ign => { cs => cs } ;
"""
      val result = ParserGenerator.generateFromSource(source)
      assert(result.isRight, result.left.toOption.getOrElse(""))
    }

    it("generates interpreter-backed code for lambda style higher-order grammar") {
      val source = """|
        |S = Double((x -> x x), "aa") !.;
        |Double(f: ?, s: ?) = f(f(s));
        |""".stripMargin
      val generated = ParserGenerator.generateFromSource(source)
      assert(generated.isRight)
      val code = generated.toOption.get
      assert(code.contains("Interpreter.fromSourceEither"))
      assert(code.contains("Double((x -> x x), \\\"aa\\\")"))
    }
  }
}
