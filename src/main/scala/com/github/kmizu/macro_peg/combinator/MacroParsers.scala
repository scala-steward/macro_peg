package com.github.kmizu.macro_peg.combinator

import scala.collection.mutable.ListBuffer

object MacroParsers {
  type Input = String
  abstract sealed class ParseResult[+T] {
    /**
      * Drop extra information from this ParseResult.
      * If it was ParseSuccess, result is converted to None,
      * else, message is converted to ""
      * Mainly, this method is used for testing
 *
      * @return
      */
    def drop: ParseResult[Any] = this match {
      case ParseSuccess(_, next) => ParseSuccess(None, next)
      case ParseFailure(_, next) => ParseFailure("", next)
    }
  }
  final case class ParseSuccess[+T](result: T, next: Input) extends ParseResult[T]
  abstract sealed class AbstractFailure extends ParseResult[Nothing] {
    val message: String
  }
  final case class ParseFailure(val message: String, next: Input) extends AbstractFailure

  abstract sealed class MacroParser[+T] {
    def apply(input: Input): ParseResult[T]
    def |[U >: T](b: MacroParser[U]): MacroParser[U] = Alternation(this, b)
    def /[U >: T](b: MacroParser[U]): MacroParser[U] = this | b
    def ~[U](b: MacroParser[U]): MacroParser[(T, U)] = Sequence(this, b)
    def ? : MacroParser[Option[T]] = OptionParser(this)
    def * : MacroParser[List[T]] = RepeatParser(this)
    def + : MacroParser[List[T]] = Repeat1Parser(this)
    def unary_! : MacroParser[Any] = NotParser(this)
    def and: MacroParser[Any] = AndParser(this)
  }
  type P[+T] = MacroParser[T]
  def any: AnyParser.type = AnyParser
  def string(literal: Input): StringParser = StringParser(literal)
  implicit def stringToParser(literal: Input): StringParser = StringParser(literal)
  def range(ranges: Seq[Char]*): RangedParser = RangedParser(ranges:_*)
  implicit def characterRangesToParser(ranges: Seq[Seq[Char]]): RangedParser = range(ranges:_*)
  def refer[T](parser: => MacroParser[T]): ReferenceParser[T] = ReferenceParser(() => parser)
  def rewritable[T](parser: MacroParser[T]): RewritableParser[T] = RewritableParser(parser)
  final case class StringParser(literal: String) extends MacroParser[String] {
    override def apply(input: Input): ParseResult[String] = {
      if(input.startsWith(literal)) ParseSuccess(literal, input.substring(literal.length))
      else ParseFailure(s"expected $literal", input)
    }
  }
  final case class RangedParser(ranges: Seq[Char]*) extends MacroParser[String] {
    override def apply(input: Input): ParseResult[String] = {
      if(input.length == 0)
        ParseFailure(s"find EOF, but expeted following characters: $ranges", input)
      else if(ranges.exists(_.find{_ == input.charAt(0)}.isDefined))
        ParseSuccess(input.substring(0, 1), input.substring(1))
      else
        ParseFailure(s"find ${input.substring(0, 1)}, but expected following characters: $ranges", input)
    }
  }
  final case class OptionParser[T](parser: MacroParser[T]) extends MacroParser[Option[T]] {
    override def apply(input: Input): ParseResult[Option[T]] = {
      parser(input) match {
        case ParseSuccess(result, next) => ParseSuccess(Some(result), next)
        case ParseFailure(message, next) => ParseSuccess(None, next)
      }
    }
  }
  final case class RepeatParser[T](parser: MacroParser[T]) extends MacroParser[List[T]] {
    override def apply(input: Input): ParseResult[List[T]] = {
      var rest = input
      val total = ListBuffer[T]()
      while(true) {
        parser(rest) match {
          case ParseSuccess(result, next) =>
            total += result
            rest = next
          case ParseFailure(message, next) =>
            return ParseSuccess(total.toList, rest)
        }
      }
      throw new RuntimeException("unreachable code")
    }
  }
  final case class Repeat1Parser[T](parser: MacroParser[T]) extends MacroParser[List[T]] {
    override def apply(input: Input): ParseResult[List[T]] = {
      var rest = input
      val total = ListBuffer[T]()
      parser(rest) match {
        case ParseSuccess(result, next) =>
          total += result
          rest = next
          while(true) {
            parser(rest) match {
              case ParseSuccess(result, next) =>
                total += result
                rest = next
              case ParseFailure(message, next) =>
                return ParseSuccess(total.toList, rest)
            }
          }
          throw new RuntimeException("unreachable code")
        case ParseFailure(message, next) =>
          ParseFailure(message, next)
      }
      while(true) {
        parser(rest) match {
          case ParseSuccess(result, next) =>
            total += result
            rest = next
          case ParseFailure(message, next) =>
            return ParseSuccess(total.toList, rest)
        }
      }
      throw new RuntimeException("unreachable code")
    }
  }
  final case class Alternation[T, U >: T](a: MacroParser[T], b: MacroParser[U]) extends MacroParser[U] {
    override def apply(input: Input): ParseResult[U] = a(input) match {
      case ParseSuccess(result, next) => ParseSuccess(result, next)
      case ParseFailure(message, next) => b(input)
    }
  }
  final case class Sequence[T, U](a: MacroParser[T], b: MacroParser[U]) extends MacroParser[(T, U)] {
    override def apply(input: Input): ParseResult[(T, U)] = a(input) match {
      case ParseSuccess(result1, next1) =>
        b(next1) match {
          case ParseSuccess(result2, next2) =>
            ParseSuccess((result1, result2), next2)
          case ParseFailure(message, next2) =>
            ParseFailure(message, next2)
        }
      case ParseFailure(message, next) =>
        ParseFailure(message, next)
    }
  }
  final case class ReferenceParser[T](delayedParser: () => MacroParser[T]) extends MacroParser[T] {
    override def apply(input: Input): ParseResult[T] = reference(input)
    lazy val reference: MacroParser[T] = delayedParser()
  }
  final case object AnyParser extends MacroParser[String] {
    override def apply(input: Input): ParseResult[String] = {
      if(input.length >= 1) ParseSuccess(input.substring(0, 1), input.substring(1)) else ParseFailure("EOF", input)
    }
  }
  final case class AndParser[T](p: MacroParser[T]) extends MacroParser[Any] {
    override def apply(input: Input): ParseResult[Any] = p(input) match {
      case ParseSuccess(result, next) => ParseSuccess((), input)
      case ParseFailure(message, next) => ParseFailure(message, input)
    }
  }
  final case class NotParser[T](p: MacroParser[T]) extends MacroParser[Any] {
    override def apply(input: Input): ParseResult[Any] = p(input) match {
      case ParseSuccess(result, next) => ParseFailure("", input)
      case ParseFailure(message, next) => ParseSuccess("", input)
    }
  }
  final case class RewritableParser[T](var p: MacroParser[T]) extends MacroParser[T] {
    override def apply(input: Input): ParseResult[T] = p(input)
  }
}
