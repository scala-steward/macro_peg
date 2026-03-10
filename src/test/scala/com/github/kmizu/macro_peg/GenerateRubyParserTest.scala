package com.github.kmizu.macro_peg

import org.scalatest.funsuite.AnyFunSuite
import com.github.kmizu.macro_peg.codegen.ParserGenerator

class GenerateRubyParserTest extends AnyFunSuite {
  test("generate Ruby parser from ruby.mpeg") {
    val source = scala.io.Source.fromResource("ruby.mpeg").mkString
    val result = ParserGenerator.generateFromSource(
      source,
      "GeneratedRubyParser",
      Some("com.github.kmizu.macro_peg.ruby"),
      Symbol("program")
    )
    result match {
      case Right(code) =>
        println(s"Generated ${code.length} chars of code")
        val outFile = new java.io.File("/tmp/GeneratedRubyParser.scala")
        val pw = new java.io.PrintWriter(outFile)
        pw.write(code)
        pw.close()
        println(s"Written to ${outFile.getAbsolutePath}")
        assert(code.contains("def parseProgram"))
      case Left(err) =>
        fail(s"Code generation failed: $err")
    }
  }
}
