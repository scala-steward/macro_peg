package com.github.kmizu.macro_peg.ruby

import com.github.kmizu.macro_peg.ruby.RubyAst._
import scala.util.Try

object GeneratedRubyParser {
  case class ~[+A, +B](_1: A, _2: B) {
    override def toString: String = s"($_1 ~ $_2)"
  }
  private def _applyAction(f: Any => Any, v: Any): Any = f(v)

  private val _MEMO_CLASSVARNAME = 13
  private val _MEMO_LOCALVAREXPR = 22
  private val _MEMO_IDENTSTART = 5
  private val _MEMO_SHIFTEXPR = 36
  private val _MEMO_NEWLINECHAR = 1
  private val _MEMO_ANDEXPR = 42
  private val _MEMO_VARIABLE = 26
  private val _MEMO_IDENTCONT = 6
  private val _MEMO_SPACING = 3
  private val _MEMO_INSTANCEVARNAME = 12
  private val _MEMO_STRINGLITERAL = 18
  private val _MEMO_CONSTREF = 27
  private val _MEMO_IDENTIFIERRAW = 7
  private val _MEMO_FLOATLITERAL = 16
  private val _MEMO_RESERVEDWORD = 8
  private val _MEMO_IDENTIFIER = 9
  private val _MEMO_RANGEEXPR = 41
  private val _MEMO_INLINESPACING = 2
  private val _MEMO_UNARYEXPR = 33
  private val _MEMO_EQUALITYEXPR = 40
  private val _MEMO_CONSTNAME = 11
  private val _MEMO_SINGLEQUOTEDSTRINGLITERAL = 19
  private val _MEMO_BITANDEXPR = 37
  private val _MEMO_GLOBALVAREXPR = 25
  private val _MEMO_POWEREXPR = 32
  private val _MEMO_CLASSVAREXPR = 24
  private val _MEMO_CONDITIONALEXPR = 44
  private val _MEMO_STATEMENT = 47
  private val _MEMO_EXPR = 46
  private val _MEMO_CONSTNAMENOSPACE = 10
  private val _MEMO_INSTANCEVAREXPR = 23
  private val _MEMO_PRIMARYEXPR = 30
  private val _MEMO_INTEGERLITERAL = 17
  private val _MEMO_GLOBALVARNAME = 14
  private val _MEMO_POSTFIXEXPR = 31
  private val _MEMO_SELFEXPR = 21
  private val _MEMO_SYMBOLLITERAL = 20
  private val _MEMO_MULDIVEXPR = 34
  private val _MEMO_BITOREXPR = 38
  private val _MEMO_HORIZONTALSPACECHAR = 0
  private val _MEMO_ADDSUBEXPR = 35
  private val _MEMO_CHAINEDASSIGNRHS = 45
  private val _MEMO_HASHLITERAL = 29
  private val _MEMO_ARRAYLITERAL = 28
  private val _MEMO_STATEMENTSEP = 4
  private val _MEMO_RELATIONALEXPR = 39
  private val _MEMO_OREXPR = 43
  private val _MEMO_CONSTPATH = 15
  private val _memoStore = new ThreadLocal[java.util.HashMap[(Int,Int),AnyRef]] {
    override def initialValue() = new java.util.HashMap[(Int,Int),AnyRef]()
  }
  private def _withMemo(id: Int, pos: Int)(f: => Option[(Any,Int)]): Option[(Any,Int)] = {
    val key = (id, pos)
    _memoStore.get().get(key) match {
      case null =>
        _memoStore.get().put(key, (None: Option[(Any,Int)]).asInstanceOf[AnyRef])
        val r = f; _memoStore.get().put(key, r.asInstanceOf[AnyRef]); r
      case v => v.asInstanceOf[Option[(Any,Int)]]
    }
  }
  def resetMemo(): Unit = _memoStore.get().clear()
  
  private def _scanBalancedBody(input: String, pos: Int, open: Char, close: Char): Option[(Any, Int)] = {
    if (pos >= input.length || input.charAt(pos) != open) return None
    var p = pos + 1; var depth = 1
    while (p < input.length && depth > 0) {
      val c = input.charAt(p)
      if (c == '\\' && p + 1 < input.length) p += 2
      else if (c == open) { depth += 1; p += 1 }
      else if (c == close) { depth -= 1; p += 1 }
      else p += 1
    }
    if (depth == 0) Some(("", p)) else None
  }
  private def _scanSimpleBody(input: String, pos: Int, delim: Char): Option[(Any, Int)] = {
    if (pos >= input.length || input.charAt(pos) != delim) return None
    var p = pos + 1
    while (p < input.length && input.charAt(p) != delim) {
      if (input.charAt(p) == '\\' && p + 1 < input.length) p += 2
      else p += 1
    }
    if (p < input.length) Some(("", p + 1)) else None
  }



  def parseHeredocLiteral(input: String, pos: Int): Option[(Any, Int)] = {

    // Parse a Ruby heredoc literal: <<[-~]?(quote?DELIM quote?)
    // Consumes the start line (<<DELIM + rest of line), heredoc body lines, and terminator line.
    // Returns ("", posAfterTerminator) on success, None on failure.
    var p = pos
    if (p + 2 > input.length || input.charAt(p) != '<' || input.charAt(p + 1) != '<') return None
    p += 2
    // Optional flag: - or ~
    val indented = if (p < input.length && (input.charAt(p) == '-' || input.charAt(p) == '~')) { p += 1; true } else false
    // Optional quote character
    val qc: Char = if (p < input.length && (input.charAt(p) == '"' || input.charAt(p) == '\'' || input.charAt(p) == '`')) {
      val c = input.charAt(p); p += 1; c
    } else '\u0000'
    // Read delimiter
    val delimStart = p
    if (qc != '\u0000') {
      // Quoted: read until matching close quote (or newline = error)
      while (p < input.length && input.charAt(p) != qc && input.charAt(p) != '\n') p += 1
      if (p >= input.length || input.charAt(p) != qc) return None
    } else {
      // Unquoted: identifier characters
      while (p < input.length && (input.charAt(p).isLetterOrDigit || input.charAt(p) == '_')) p += 1
    }
    val delim = input.substring(delimStart, p)
    if (delim.isEmpty) return None
    if (qc != '\u0000') p += 1 // consume close quote
    // Skip rest of start line (including the terminating \n)
    while (p < input.length && input.charAt(p) != '\n' && input.charAt(p) != '\r') p += 1
    if (p < input.length && input.charAt(p) == '\r') p += 1
    if (p < input.length && input.charAt(p) == '\n') p += 1
    // Scan subsequent lines for the terminator
    while (p < input.length) {
      val lineStart = p
      var ts = p
      if (indented) while (ts < input.length && (input.charAt(ts) == ' ' || input.charAt(ts) == '\t')) ts += 1
      if (input.startsWith(delim, ts)) {
        val after = ts + delim.length
        if (after >= input.length || input.charAt(after) == '\n' || input.charAt(after) == '\r' || input.charAt(after) == ';') {
          var end = after
          if (end < input.length && input.charAt(end) == '\r') end += 1
          if (end < input.length && input.charAt(end) == '\n') end += 1
          return Some((StringLiteral(""), end))
        }
      }
      // Advance to next line
      while (p < input.length && input.charAt(p) != '\n') p += 1
      if (p < input.length) p += 1
      if (p == lineStart) return None // guard: no progress (EOF edge case)
    }
    None

  }

  def parsePercentBodyBraces(input: String, pos: Int): Option[(Any, Int)] = {
 _scanBalancedBody(input, pos, '{', '}') 
  }

  def parsePercentBodyParens(input: String, pos: Int): Option[(Any, Int)] = {
 _scanBalancedBody(input, pos, '(', ')') 
  }

  def parsePercentBodyBrackets(input: String, pos: Int): Option[(Any, Int)] = {
 _scanBalancedBody(input, pos, '[', ']') 
  }

  def parsePercentBodyAngles(input: String, pos: Int): Option[(Any, Int)] = {
 _scanBalancedBody(input, pos, '<', '>') 
  }

  def parsePercentBodySimplePipe(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '|') 
  }

  def parsePercentBodySimplePercent(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '%') 
  }

  def parsePercentBodySimpleDoubleQuote(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '"') 
  }

  def parsePercentBodySimpleSingleQuote(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '\'') 
  }

  def parsePercentBodySimpleSlash(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '/') 
  }

  def parsePercentBodySimpleColon(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, ':') 
  }

  def parsePercentBodySimpleExcl(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '!') 
  }

  def parsePercentBodySimpleComma(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, ',') 
  }

  def parsePercentBodySimpleSemicolon(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, ';') 
  }

  def parsePercentBodySimpleDash(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '-') 
  }

  def parsePercentBodySimpleUnderscore(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '_') 
  }

  def parsePercentBodySimpleEquals(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '=') 
  }

  def parsePercentBodySimplePlus(input: String, pos: Int): Option[(Any, Int)] = {
 _scanSimpleBody(input, pos, '+') 
  }

  def parseHorizontalSpaceChar(input: String, pos: Int): Option[(Any, Int)] = _withMemo(0, pos) {
    (((if (input.startsWith(" ", pos)) Some((" ", pos + 1)) else None)).orElse((if (input.startsWith("\t", pos)) Some(("\t", pos + 1)) else None))).orElse((if (input.startsWith("\r", pos)) Some(("\r", pos + 1)) else None)).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }
  }

  def parseNewlineChar(input: String, pos: Int): Option[(Any, Int)] = _withMemo(1, pos) {
    (if (input.startsWith("\n", pos)) Some(("\n", pos + 1)) else None).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }
  }

  def parseLineContinuation(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("\\\n", pos)) Some(("\\\n", pos + 2)) else None).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseComment(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("#", pos)) Some(("#", pos + 1)) else None).flatMap { case (_r1, _p2) => {
  var _rs5: List[Any] = Nil; var _cp6: Int = _p2; var _go9 = true
  while (_go9) { (if ((if (input.startsWith("\n", _cp6)) Some(("\n", _cp6 + 1)) else None).isEmpty) Some(((), _cp6)) else None).flatMap { case (_r10, _p11) => (if (_p11 < input.length) Some((input.charAt(_p11).toString, _p11 + 1)) else None).map { case (_r12, _p13) => (new ~(_r10, _r12), _p13) } } match {
    case Some((_st7, _np8)) => _rs5 = _rs5 :+ _st7; _cp6 = _np8
    case None => _go9 = false } }
  Some((_rs5, _cp6)) }.map { case (_r3, _p4) => (new ~(_r1, _r3), _p4) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseBlockComment(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("=begin", pos)) Some(("=begin", pos + 6)) else None).flatMap { case (_r18, _p19) => {
  var _rs22: List[Any] = Nil; var _cp23: Int = _p19; var _go26 = true
  while (_go26) { (if ((if (input.startsWith("=end", _cp23)) Some(("=end", _cp23 + 4)) else None).isEmpty) Some(((), _cp23)) else None).flatMap { case (_r27, _p28) => (if (_p28 < input.length) Some((input.charAt(_p28).toString, _p28 + 1)) else None).map { case (_r29, _p30) => (new ~(_r27, _r29), _p30) } } match {
    case Some((_st24, _np25)) => _rs22 = _rs22 :+ _st24; _cp23 = _np25
    case None => _go26 = false } }
  Some((_rs22, _cp23)) }.map { case (_r20, _p21) => (new ~(_r18, _r20), _p21) } }.flatMap { case (_r14, _p15) => (if (input.startsWith("=end", _p15)) Some(("=end", _p15 + 4)) else None).map { case (_r16, _p17) => (new ~(_r14, _r16), _p17) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseInlineSpacing(input: String, pos: Int): Option[(Any, Int)] = _withMemo(2, pos) {
    {
  var _rs31: List[Any] = Nil; var _cp32: Int = pos; var _go35 = true
  while (_go35) { (((parseHorizontalSpaceChar(input, _cp32)).orElse(parseLineContinuation(input, _cp32))).orElse(parseComment(input, _cp32))).orElse(parseBlockComment(input, _cp32)) match {
    case Some((_st33, _np34)) => _rs31 = _rs31 :+ _st33; _cp32 = _np34
    case None => _go35 = false } }
  Some((_rs31, _cp32)) }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }
  }

  def parseSpacing(input: String, pos: Int): Option[(Any, Int)] = _withMemo(3, pos) {
    {
  var _rs36: List[Any] = Nil; var _cp37: Int = pos; var _go40 = true
  while (_go40) { ((((parseHorizontalSpaceChar(input, _cp37)).orElse(parseNewlineChar(input, _cp37))).orElse(parseLineContinuation(input, _cp37))).orElse(parseComment(input, _cp37))).orElse(parseBlockComment(input, _cp37)) match {
    case Some((_st38, _np39)) => _rs36 = _rs36 :+ _st38; _cp37 = _np39
    case None => _go40 = false } }
  Some((_rs36, _cp37)) }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }
  }

  def parseSpacing1(input: String, pos: Int): Option[(Any, Int)] = {
  ((parseHorizontalSpaceChar(input, pos)).orElse(parseComment(input, pos))).orElse(parseBlockComment(input, pos)) match {
    case None => None
    case Some((_fs41, _fp42)) =>
      var _rs43: List[Any] = List(_fs41); var _cp44: Int = _fp42; var _go47 = true
      while (_go47) { ((parseHorizontalSpaceChar(input, _cp44)).orElse(parseComment(input, _cp44))).orElse(parseBlockComment(input, _cp44)) match {
        case Some((_st45, _np46)) => _rs43 = _rs43 :+ _st45; _cp44 = _np46
        case None => _go47 = false } }
      Some((_rs43, _cp44)) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseLineBreak(input: String, pos: Int): Option[(Any, Int)] = parseInlineSpacing(input, pos).flatMap { case (_r52, _p53) => (if (input.startsWith("\n", _p53)) Some(("\n", _p53 + 1)) else None).map { case (_r54, _p55) => (new ~(_r52, _r54), _p55) } }.flatMap { case (_r48, _p49) => parseInlineSpacing(input, _p49).map { case (_r50, _p51) => (new ~(_r48, _r50), _p51) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseStatementSep(input: String, pos: Int): Option[(Any, Int)] = _withMemo(4, pos) {
    parseInlineSpacing(input, pos).flatMap { case (_r60, _p61) => ((if (input.startsWith(";", _p61)) Some((";", _p61 + 1)) else None)).orElse((if (input.startsWith("\n", _p61)) Some(("\n", _p61 + 1)) else None)).map { case (_r62, _p63) => (new ~(_r60, _r62), _p63) } }.flatMap { case (_r56, _p57) => parseSpacing(input, _p57).map { case (_r58, _p59) => (new ~(_r56, _r58), _p59) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }
  }

  def parseIdentStart(input: String, pos: Int): Option[(Any, Int)] = _withMemo(5, pos) {
    (if (pos < input.length && { val _c = input.charAt(pos); (_c >= 'a' && _c <= 'z') || (_c >= 'A' && _c <= 'Z') || (_c == '_') }) Some((input.charAt(pos).toString, pos + 1)) else None).map { case (r, p) => (_applyAction({  c => c.toString  }, r), p) }
  }

  def parseIdentCont(input: String, pos: Int): Option[(Any, Int)] = _withMemo(6, pos) {
    (if (pos < input.length && { val _c = input.charAt(pos); (_c >= 'a' && _c <= 'z') || (_c >= 'A' && _c <= 'Z') || (_c >= '0' && _c <= '9') || (_c == '_') }) Some((input.charAt(pos).toString, pos + 1)) else None).map { case (r, p) => (_applyAction({  c => c.toString  }, r), p) }
  }

  def parseIdentifierRaw(input: String, pos: Int): Option[(Any, Int)] = _withMemo(7, pos) {
    parseIdentStart(input, pos).flatMap { case (_r64, _p65) => {
  var _rs68: List[Any] = Nil; var _cp69: Int = _p65; var _go72 = true
  while (_go72) { parseIdentCont(input, _cp69) match {
    case Some((_st70, _np71)) => _rs68 = _rs68 :+ _st70; _cp69 = _np71
    case None => _go72 = false } }
  Some((_rs68, _cp69)) }.map { case (_r66, _p67) => (new ~(_r64, _r66), _p67) } }.map { case (r, p) => (_applyAction({  case s ~ cs => s.toString + cs.asInstanceOf[List[Any]].mkString  }, r), p) }
  }

  def parseReservedWord(input: String, pos: Int): Option[(Any, Int)] = _withMemo(8, pos) {
    (((((((((((((((((((((((((((((((((if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None)).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None))).orElse((if (input.startsWith("do", pos)) Some(("do", pos + 2)) else None))).orElse((if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None))).orElse((if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None))).orElse((if (input.startsWith("while", pos)) Some(("while", pos + 5)) else None))).orElse((if (input.startsWith("until", pos)) Some(("until", pos + 5)) else None))).orElse((if (input.startsWith("for", pos)) Some(("for", pos + 3)) else None))).orElse((if (input.startsWith("in", pos)) Some(("in", pos + 2)) else None))).orElse((if (input.startsWith("return", pos)) Some(("return", pos + 6)) else None))).orElse((if (input.startsWith("yield", pos)) Some(("yield", pos + 5)) else None))).orElse((if (input.startsWith("and", pos)) Some(("and", pos + 3)) else None))).orElse((if (input.startsWith("or", pos)) Some(("or", pos + 2)) else None))).orElse((if (input.startsWith("not", pos)) Some(("not", pos + 3)) else None))).orElse((if (input.startsWith("true", pos)) Some(("true", pos + 4)) else None))).orElse((if (input.startsWith("false", pos)) Some(("false", pos + 5)) else None))).orElse((if (input.startsWith("nil", pos)) Some(("nil", pos + 3)) else None))).orElse((if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None))).orElse((if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None))).orElse((if (input.startsWith("module", pos)) Some(("module", pos + 6)) else None))).orElse((if (input.startsWith("def", pos)) Some(("def", pos + 3)) else None))).orElse((if (input.startsWith("then", pos)) Some(("then", pos + 4)) else None))).orElse((if (input.startsWith("elsif", pos)) Some(("elsif", pos + 5)) else None))).orElse((if (input.startsWith("else", pos)) Some(("else", pos + 4)) else None))).orElse((if (input.startsWith("case", pos)) Some(("case", pos + 4)) else None))).orElse((if (input.startsWith("when", pos)) Some(("when", pos + 4)) else None))).orElse((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None))).orElse((if (input.startsWith("ensure", pos)) Some(("ensure", pos + 6)) else None))).orElse((if (input.startsWith("retry", pos)) Some(("retry", pos + 5)) else None))).orElse((if (input.startsWith("break", pos)) Some(("break", pos + 5)) else None))).orElse((if (input.startsWith("next", pos)) Some(("next", pos + 4)) else None))).orElse((if (input.startsWith("alias", pos)) Some(("alias", pos + 5)) else None))).orElse((if (input.startsWith("defined?", pos)) Some(("defined?", pos + 8)) else None)).flatMap { case (_r73, _p74) => (if (parseIdentCont(input, _p74).isEmpty) Some(((), _p74)) else None).map { case (_r75, _p76) => (new ~(_r73, _r75), _p76) } }.map { case (r, p) => (_applyAction({  case kw ~ _ => kw  }, r), p) }
  }

  def parseIdentifierNoSpace(input: String, pos: Int): Option[(Any, Int)] = (if (parseReservedWord(input, pos).isEmpty) Some(((), pos)) else None).flatMap { case (_r77, _p78) => parseIdentifierRaw(input, _p78).map { case (_r79, _p80) => (new ~(_r77, _r79), _p80) } }.map { case (r, p) => (_applyAction({  case _ ~ name => name.asInstanceOf[String]  }, r), p) }

  def parseIdentifier(input: String, pos: Int): Option[(Any, Int)] = _withMemo(9, pos) {
    parseIdentifierNoSpace(input, pos).flatMap { case (_r81, _p82) => parseInlineSpacing(input, _p82).map { case (_r83, _p84) => (new ~(_r81, _r83), _p84) } }.map { case (r, p) => (_applyAction({  case name ~ _ => name.asInstanceOf[String]  }, r), p) }
  }

  def parseConstNameNoSpace(input: String, pos: Int): Option[(Any, Int)] = _withMemo(10, pos) {
    (if (pos < input.length && { val _c = input.charAt(pos); (_c >= 'A' && _c <= 'Z') }) Some((input.charAt(pos).toString, pos + 1)) else None).flatMap { case (_r85, _p86) => {
  var _rs89: List[Any] = Nil; var _cp90: Int = _p86; var _go93 = true
  while (_go93) { parseIdentCont(input, _cp90) match {
    case Some((_st91, _np92)) => _rs89 = _rs89 :+ _st91; _cp90 = _np92
    case None => _go93 = false } }
  Some((_rs89, _cp90)) }.map { case (_r87, _p88) => (new ~(_r85, _r87), _p88) } }.map { case (r, p) => (_applyAction({  case h ~ t => h.toString + t.asInstanceOf[List[Any]].mkString  }, r), p) }
  }

  def parseConstName(input: String, pos: Int): Option[(Any, Int)] = _withMemo(11, pos) {
    parseConstNameNoSpace(input, pos).flatMap { case (_r94, _p95) => parseInlineSpacing(input, _p95).map { case (_r96, _p97) => (new ~(_r94, _r96), _p97) } }.map { case (r, p) => (_applyAction({  case name ~ _ => name.asInstanceOf[String]  }, r), p) }
  }

  def parseInstanceVarNameRaw(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("@", pos)) Some(("@", pos + 1)) else None).flatMap { case (_r98, _p99) => parseIdentifierRaw(input, _p99).map { case (_r100, _p101) => (new ~(_r98, _r100), _p101) } }.map { case (r, p) => (_applyAction({  case _ ~ name => "@" + name.asInstanceOf[String]  }, r), p) }

  def parseClassVarNameRaw(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("@@", pos)) Some(("@@", pos + 2)) else None).flatMap { case (_r102, _p103) => parseIdentifierRaw(input, _p103).map { case (_r104, _p105) => (new ~(_r102, _r104), _p105) } }.map { case (r, p) => (_applyAction({  case _ ~ name => "@@" + name.asInstanceOf[String]  }, r), p) }

  def parseInstanceVarName(input: String, pos: Int): Option[(Any, Int)] = _withMemo(12, pos) {
    parseInstanceVarNameRaw(input, pos).flatMap { case (_r106, _p107) => parseInlineSpacing(input, _p107).map { case (_r108, _p109) => (new ~(_r106, _r108), _p109) } }.map { case (r, p) => (_applyAction({  case name ~ _ => name.asInstanceOf[String]  }, r), p) }
  }

  def parseClassVarName(input: String, pos: Int): Option[(Any, Int)] = _withMemo(13, pos) {
    parseClassVarNameRaw(input, pos).flatMap { case (_r110, _p111) => parseInlineSpacing(input, _p111).map { case (_r112, _p113) => (new ~(_r110, _r112), _p113) } }.map { case (r, p) => (_applyAction({  case name ~ _ => name.asInstanceOf[String]  }, r), p) }
  }

  def parseGlobalVarSpecial(input: String, pos: Int): Option[(Any, Int)] = ((((((((((((((((((((if (input.startsWith("$!", pos)) Some(("$!", pos + 2)) else None)).orElse((if (input.startsWith("$?", pos)) Some(("$?", pos + 2)) else None))).orElse((if (input.startsWith("$@", pos)) Some(("$@", pos + 2)) else None))).orElse((if (input.startsWith("$&", pos)) Some(("$&", pos + 2)) else None))).orElse((if (input.startsWith("$`", pos)) Some(("$`", pos + 2)) else None))).orElse((if (input.startsWith("$'", pos)) Some(("$'", pos + 2)) else None))).orElse((if (input.startsWith("$+", pos)) Some(("$+", pos + 2)) else None))).orElse((if (input.startsWith("$~", pos)) Some(("$~", pos + 2)) else None))).orElse((if (input.startsWith("$/", pos)) Some(("$/", pos + 2)) else None))).orElse((if (input.startsWith("$\\", pos)) Some(("$\\", pos + 2)) else None))).orElse((if (input.startsWith("$,", pos)) Some(("$,", pos + 2)) else None))).orElse((if (input.startsWith("$.", pos)) Some(("$.", pos + 2)) else None))).orElse((if (input.startsWith("$;", pos)) Some(("$;", pos + 2)) else None))).orElse((if (input.startsWith("$<", pos)) Some(("$<", pos + 2)) else None))).orElse((if (input.startsWith("$>", pos)) Some(("$>", pos + 2)) else None))).orElse((if (input.startsWith("$_", pos)) Some(("$_", pos + 2)) else None))).orElse((if (input.startsWith("$:", pos)) Some(("$:", pos + 2)) else None))).orElse((if (input.startsWith("$=", pos)) Some(("$=", pos + 2)) else None))).orElse((if (input.startsWith("$*", pos)) Some(("$*", pos + 2)) else None))).orElse((if (input.startsWith("$$", pos)) Some(("$$", pos + 2)) else None)).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseGlobalVarDash(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("$-", pos)) Some(("$-", pos + 2)) else None).flatMap { case (_r114, _p115) => (if (_p115 < input.length && { val _c = input.charAt(_p115); (_c >= 'a' && _c <= 'z') || (_c >= 'A' && _c <= 'Z') || (_c >= '0' && _c <= '9') || (_c == '_') }) Some((input.charAt(_p115).toString, _p115 + 1)) else None).map { case (_r116, _p117) => (new ~(_r114, _r116), _p117) } }.map { case (r, p) => (_applyAction({  case _ ~ c => "$-" + c.toString  }, r), p) }

  def parseGlobalVarNumeric(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("$", pos)) Some(("$", pos + 1)) else None).flatMap { case (_r118, _p119) => {
  (if (_p119 < input.length && { val _c = input.charAt(_p119); (_c >= '0' && _c <= '9') }) Some((input.charAt(_p119).toString, _p119 + 1)) else None) match {
    case None => None
    case Some((_fs122, _fp123)) =>
      var _rs124: List[Any] = List(_fs122); var _cp125: Int = _fp123; var _go128 = true
      while (_go128) { (if (_cp125 < input.length && { val _c = input.charAt(_cp125); (_c >= '0' && _c <= '9') }) Some((input.charAt(_cp125).toString, _cp125 + 1)) else None) match {
        case Some((_st126, _np127)) => _rs124 = _rs124 :+ _st126; _cp125 = _np127
        case None => _go128 = false } }
      Some((_rs124, _cp125)) } }.map { case (_r120, _p121) => (new ~(_r118, _r120), _p121) } }.map { case (r, p) => (_applyAction({  case _ ~ ds => "$" + ds.asInstanceOf[List[Any]].mkString  }, r), p) }

  def parseGlobalVarNamed(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("$", pos)) Some(("$", pos + 1)) else None).flatMap { case (_r129, _p130) => parseIdentifierRaw(input, _p130).map { case (_r131, _p132) => (new ~(_r129, _r131), _p132) } }.map { case (r, p) => (_applyAction({  case _ ~ name => "$" + name.asInstanceOf[String]  }, r), p) }

  def parseGlobalVarName(input: String, pos: Int): Option[(Any, Int)] = _withMemo(14, pos) {
    (((parseGlobalVarSpecial(input, pos)).orElse(parseGlobalVarDash(input, pos))).orElse(parseGlobalVarNumeric(input, pos))).orElse(parseGlobalVarNamed(input, pos)).flatMap { case (_r133, _p134) => parseInlineSpacing(input, _p134).map { case (_r135, _p136) => (new ~(_r133, _r135), _p136) } }.map { case (r, p) => (_applyAction({  case n ~ _ => n.asInstanceOf[String]  }, r), p) }
  }

  def parseConstPathSegment(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("::", pos)) Some(("::", pos + 2)) else None).flatMap { case (_r137, _p138) => parseConstNameNoSpace(input, _p138).map { case (_r139, _p140) => (new ~(_r137, _r139), _p140) } }.map { case (r, p) => (_applyAction({  case _ ~ n => n.asInstanceOf[String]  }, r), p) }

  def parseConstPathNoSpace(input: String, pos: Int): Option[(Any, Int)] = parseConstNameNoSpace(input, pos).flatMap { case (_r141, _p142) => {
  var _rs145: List[Any] = Nil; var _cp146: Int = _p142; var _go149 = true
  while (_go149) { parseConstPathSegment(input, _cp146) match {
    case Some((_st147, _np148)) => _rs145 = _rs145 :+ _st147; _cp146 = _np148
    case None => _go149 = false } }
  Some((_rs145, _cp146)) }.map { case (_r143, _p144) => (new ~(_r141, _r143), _p144) } }.map { case (r, p) => (_applyAction({  case h ~ t => h.asInstanceOf[String] :: t.asInstanceOf[List[Any]].map(_.asInstanceOf[String])  }, r), p) }

  def parseConstPathWithRoot(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("::", pos)) Some(("::", pos + 2)) else None).flatMap { case (_r154, _p155) => parseConstNameNoSpace(input, _p155).map { case (_r156, _p157) => (new ~(_r154, _r156), _p157) } }.flatMap { case (_r150, _p151) => {
  var _rs158: List[Any] = Nil; var _cp159: Int = _p151; var _go162 = true
  while (_go162) { (if (input.startsWith("::", _cp159)) Some(("::", _cp159 + 2)) else None).flatMap { case (_r163, _p164) => parseConstNameNoSpace(input, _p164).map { case (_r165, _p166) => (new ~(_r163, _r165), _p166) } } match {
    case Some((_st160, _np161)) => _rs158 = _rs158 :+ _st160; _cp159 = _np161
    case None => _go162 = false } }
  Some((_rs158, _cp159)) }.map { case (_r152, _p153) => (new ~(_r150, _r152), _p153) } }.map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseConstPath(input: String, pos: Int): Option[(Any, Int)] = _withMemo(15, pos) {
    (parseConstPathNoSpace(input, pos)).orElse(parseConstPathWithRoot(input, pos)).flatMap { case (_r167, _p168) => parseInlineSpacing(input, _p168).map { case (_r169, _p170) => (new ~(_r167, _r169), _p170) } }.map { case (r, p) => (_applyAction({  case p ~ _ => p  }, r), p) }
  }

  def parseSymbolOperatorName(input: String, pos: Int): Option[(Any, Int)] = ((((((((((((((((((((((((((((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None)).orElse((if (input.startsWith("===", pos)) Some(("===", pos + 3)) else None))).orElse((if (input.startsWith("<=>", pos)) Some(("<=>", pos + 3)) else None))).orElse((if (input.startsWith("=~", pos)) Some(("=~", pos + 2)) else None))).orElse((if (input.startsWith("!~", pos)) Some(("!~", pos + 2)) else None))).orElse((if (input.startsWith("!=", pos)) Some(("!=", pos + 2)) else None))).orElse((if (input.startsWith("==", pos)) Some(("==", pos + 2)) else None))).orElse((if (input.startsWith("<<", pos)) Some(("<<", pos + 2)) else None))).orElse((if (input.startsWith(">>", pos)) Some((">>", pos + 2)) else None))).orElse((if (input.startsWith("[]=", pos)) Some(("[]=", pos + 3)) else None))).orElse((if (input.startsWith("[]", pos)) Some(("[]", pos + 2)) else None))).orElse((if (input.startsWith("<=", pos)) Some(("<=", pos + 2)) else None))).orElse((if (input.startsWith(">=", pos)) Some((">=", pos + 2)) else None))).orElse((if (input.startsWith("<", pos)) Some(("<", pos + 1)) else None))).orElse((if (input.startsWith(">", pos)) Some((">", pos + 1)) else None))).orElse((if (input.startsWith("+@", pos)) Some(("+@", pos + 2)) else None))).orElse((if (input.startsWith("-@", pos)) Some(("-@", pos + 2)) else None))).orElse((if (input.startsWith("+", pos)) Some(("+", pos + 1)) else None))).orElse((if (input.startsWith("-", pos)) Some(("-", pos + 1)) else None))).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None))).orElse((if (input.startsWith("/", pos)) Some(("/", pos + 1)) else None))).orElse((if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None))).orElse((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None))).orElse((if (input.startsWith("|", pos)) Some(("|", pos + 1)) else None))).orElse((if (input.startsWith("^", pos)) Some(("^", pos + 1)) else None))).orElse((if (input.startsWith("~", pos)) Some(("~", pos + 1)) else None))).orElse((if (input.startsWith("!", pos)) Some(("!", pos + 1)) else None))).orElse((if (input.startsWith("`", pos)) Some(("`", pos + 1)) else None)).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseMethodSuffixChar(input: String, pos: Int): Option[(Any, Int)] = (((if (input.startsWith("?", pos)) Some(("?", pos + 1)) else None)).orElse((if (input.startsWith("!", pos)) Some(("!", pos + 1)) else None).flatMap { case (_r171, _p172) => (if (((if (input.startsWith("=", _p172)) Some(("=", _p172 + 1)) else None)).orElse((if (input.startsWith("~", _p172)) Some(("~", _p172 + 1)) else None)).isEmpty) Some(((), _p172)) else None).map { case (_r173, _p174) => (new ~(_r171, _r173), _p174) } })).orElse((if (input.startsWith("=", pos)) Some(("=", pos + 1)) else None).flatMap { case (_r175, _p176) => (if ((((if (input.startsWith("=", _p176)) Some(("=", _p176 + 1)) else None)).orElse((if (input.startsWith(">", _p176)) Some((">", _p176 + 1)) else None))).orElse((if (input.startsWith("~", _p176)) Some(("~", _p176 + 1)) else None)).isEmpty) Some(((), _p176)) else None).map { case (_r177, _p178) => (new ~(_r175, _r177), _p178) } }).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseMethodIdentifierRaw(input: String, pos: Int): Option[(Any, Int)] = (parseIdentifierRaw(input, pos).flatMap { case (_r179, _p180) => parseMethodSuffixChar(input, _p180).map { case (_r181, _p182) => (new ~(_r179, _r181), _p182) } }).orElse((if (parseReservedWord(input, pos).isEmpty) Some(((), pos)) else None).flatMap { case (_r183, _p184) => parseIdentifierRaw(input, _p184).map { case (_r185, _p186) => (new ~(_r183, _r185), _p186) } }).map { case (r, p) => (_applyAction({  v => v  }, r), p) }

  def parseAssignEq(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("=", pos)) Some(("=", pos + 1)) else None).flatMap { case (_r191, _p192) => (if ((((if (input.startsWith("=", _p192)) Some(("=", _p192 + 1)) else None)).orElse((if (input.startsWith(">", _p192)) Some((">", _p192 + 1)) else None))).orElse((if (input.startsWith("~", _p192)) Some(("~", _p192 + 1)) else None)).isEmpty) Some(((), _p192)) else None).map { case (_r193, _p194) => (new ~(_r191, _r193), _p194) } }.flatMap { case (_r187, _p188) => parseInlineSpacing(input, _p188).map { case (_r189, _p190) => (new ~(_r187, _r189), _p190) } }.map { case (r, p) => (_applyAction({  _ => "="  }, r), p) }

  def parseLabelColon(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith(":", pos)) Some((":", pos + 1)) else None).flatMap { case (_r199, _p200) => (if ((if (input.startsWith(":", _p200)) Some((":", _p200 + 1)) else None).isEmpty) Some(((), _p200)) else None).map { case (_r201, _p202) => (new ~(_r199, _r201), _p202) } }.flatMap { case (_r195, _p196) => parseInlineSpacing(input, _p196).map { case (_r197, _p198) => (new ~(_r195, _r197), _p198) } }.map { case (r, p) => (_applyAction({  _ => ":"  }, r), p) }

  def parseDecimalDigitsRaw(input: String, pos: Int): Option[(Any, Int)] = (if (pos < input.length && { val _c = input.charAt(pos); (_c >= '0' && _c <= '9') }) Some((input.charAt(pos).toString, pos + 1)) else None).flatMap { case (_r203, _p204) => {
  var _rs207: List[Any] = Nil; var _cp208: Int = _p204; var _go211 = true
  while (_go211) { (if (_cp208 < input.length && { val _c = input.charAt(_cp208); (_c >= '0' && _c <= '9') || (_c == '_') }) Some((input.charAt(_cp208).toString, _cp208 + 1)) else None) match {
    case Some((_st209, _np210)) => _rs207 = _rs207 :+ _st209; _cp208 = _np210
    case None => _go211 = false } }
  Some((_rs207, _cp208)) }.map { case (_r205, _p206) => (new ~(_r203, _r205), _p206) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseBinaryDigitsRaw(input: String, pos: Int): Option[(Any, Int)] = (if (pos < input.length && { val _c = input.charAt(pos); (_c == '0') || (_c == '1') }) Some((input.charAt(pos).toString, pos + 1)) else None).flatMap { case (_r212, _p213) => {
  var _rs216: List[Any] = Nil; var _cp217: Int = _p213; var _go220 = true
  while (_go220) { (if (_cp217 < input.length && { val _c = input.charAt(_cp217); (_c == '0') || (_c == '1') || (_c == '_') }) Some((input.charAt(_cp217).toString, _cp217 + 1)) else None) match {
    case Some((_st218, _np219)) => _rs216 = _rs216 :+ _st218; _cp217 = _np219
    case None => _go220 = false } }
  Some((_rs216, _cp217)) }.map { case (_r214, _p215) => (new ~(_r212, _r214), _p215) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseOctalDigitsRaw(input: String, pos: Int): Option[(Any, Int)] = (if (pos < input.length && { val _c = input.charAt(pos); (_c >= '0' && _c <= '7') }) Some((input.charAt(pos).toString, pos + 1)) else None).flatMap { case (_r221, _p222) => {
  var _rs225: List[Any] = Nil; var _cp226: Int = _p222; var _go229 = true
  while (_go229) { (if (_cp226 < input.length && { val _c = input.charAt(_cp226); (_c >= '0' && _c <= '7') || (_c == '_') }) Some((input.charAt(_cp226).toString, _cp226 + 1)) else None) match {
    case Some((_st227, _np228)) => _rs225 = _rs225 :+ _st227; _cp226 = _np228
    case None => _go229 = false } }
  Some((_rs225, _cp226)) }.map { case (_r223, _p224) => (new ~(_r221, _r223), _p224) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseHexDigitsRaw(input: String, pos: Int): Option[(Any, Int)] = (if (pos < input.length && { val _c = input.charAt(pos); (_c >= '0' && _c <= '9') || (_c >= 'a' && _c <= 'f') || (_c >= 'A' && _c <= 'F') }) Some((input.charAt(pos).toString, pos + 1)) else None).flatMap { case (_r230, _p231) => {
  var _rs234: List[Any] = Nil; var _cp235: Int = _p231; var _go238 = true
  while (_go238) { (if (_cp235 < input.length && { val _c = input.charAt(_cp235); (_c >= '0' && _c <= '9') || (_c >= 'a' && _c <= 'f') || (_c >= 'A' && _c <= 'F') || (_c == '_') }) Some((input.charAt(_cp235).toString, _cp235 + 1)) else None) match {
    case Some((_st236, _np237)) => _rs234 = _rs234 :+ _st236; _cp235 = _np237
    case None => _go238 = false } }
  Some((_rs234, _cp235)) }.map { case (_r232, _p233) => (new ~(_r230, _r232), _p233) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseIntegerLiteralCore(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("0", pos)) Some(("0", pos + 1)) else None).flatMap { case (_r243, _p244) => (if (_p244 < input.length && { val _c = input.charAt(_p244); (_c == 'b') || (_c == 'B') }) Some((input.charAt(_p244).toString, _p244 + 1)) else None).map { case (_r245, _p246) => (new ~(_r243, _r245), _p246) } }.flatMap { case (_r239, _p240) => parseBinaryDigitsRaw(input, _p240).map { case (_r241, _p242) => (new ~(_r239, _r241), _p242) } }).orElse((if (input.startsWith("0", pos)) Some(("0", pos + 1)) else None).flatMap { case (_r251, _p252) => (if (_p252 < input.length && { val _c = input.charAt(_p252); (_c == 'o') || (_c == 'O') }) Some((input.charAt(_p252).toString, _p252 + 1)) else None).map { case (_r253, _p254) => (new ~(_r251, _r253), _p254) } }.flatMap { case (_r247, _p248) => parseOctalDigitsRaw(input, _p248).map { case (_r249, _p250) => (new ~(_r247, _r249), _p250) } })).orElse((if (input.startsWith("0", pos)) Some(("0", pos + 1)) else None).flatMap { case (_r259, _p260) => (if (_p260 < input.length && { val _c = input.charAt(_p260); (_c == 'd') || (_c == 'D') }) Some((input.charAt(_p260).toString, _p260 + 1)) else None).map { case (_r261, _p262) => (new ~(_r259, _r261), _p262) } }.flatMap { case (_r255, _p256) => parseDecimalDigitsRaw(input, _p256).map { case (_r257, _p258) => (new ~(_r255, _r257), _p258) } })).orElse((if (input.startsWith("0", pos)) Some(("0", pos + 1)) else None).flatMap { case (_r267, _p268) => (if (_p268 < input.length && { val _c = input.charAt(_p268); (_c == 'x') || (_c == 'X') }) Some((input.charAt(_p268).toString, _p268 + 1)) else None).map { case (_r269, _p270) => (new ~(_r267, _r269), _p270) } }.flatMap { case (_r263, _p264) => parseHexDigitsRaw(input, _p264).map { case (_r265, _p266) => (new ~(_r263, _r265), _p266) } })).orElse(parseDecimalDigitsRaw(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseNumericSuffix(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("ri", pos)) Some(("ri", pos + 2)) else None)).orElse((if (input.startsWith("ir", pos)) Some(("ir", pos + 2)) else None))).orElse((if (input.startsWith("r", pos)) Some(("r", pos + 1)) else None))).orElse((if (input.startsWith("i", pos)) Some(("i", pos + 1)) else None)).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseFloatExponent(input: String, pos: Int): Option[(Any, Int)] = (if (pos < input.length && { val _c = input.charAt(pos); (_c == 'e') || (_c == 'E') }) Some((input.charAt(pos).toString, pos + 1)) else None).flatMap { case (_r275, _p276) => (((if (input.startsWith("+", _p276)) Some(("+", _p276 + 1)) else None)).orElse((if (input.startsWith("-", _p276)) Some(("-", _p276 + 1)) else None)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p276)))).map { case (_r277, _p278) => (new ~(_r275, _r277), _p278) } }.flatMap { case (_r271, _p272) => {
  (if (_p272 < input.length && { val _c = input.charAt(_p272); (_c >= '0' && _c <= '9') }) Some((input.charAt(_p272).toString, _p272 + 1)) else None) match {
    case None => None
    case Some((_fs279, _fp280)) =>
      var _rs281: List[Any] = List(_fs279); var _cp282: Int = _fp280; var _go285 = true
      while (_go285) { (if (_cp282 < input.length && { val _c = input.charAt(_cp282); (_c >= '0' && _c <= '9') }) Some((input.charAt(_cp282).toString, _cp282 + 1)) else None) match {
        case Some((_st283, _np284)) => _rs281 = _rs281 :+ _st283; _cp282 = _np284
        case None => _go285 = false } }
      Some((_rs281, _cp282)) } }.map { case (_r273, _p274) => (new ~(_r271, _r273), _p274) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseFloatLiteralCore(input: String, pos: Int): Option[(Any, Int)] = (parseDecimalDigitsRaw(input, pos).flatMap { case (_r294, _p295) => (if (input.startsWith(".", _p295)) Some((".", _p295 + 1)) else None).map { case (_r296, _p297) => (new ~(_r294, _r296), _p297) } }.flatMap { case (_r290, _p291) => parseDecimalDigitsRaw(input, _p291).map { case (_r292, _p293) => (new ~(_r290, _r292), _p293) } }.flatMap { case (_r286, _p287) => (parseFloatExponent(input, _p287).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p287)))).map { case (_r288, _p289) => (new ~(_r286, _r288), _p289) } }).orElse(parseDecimalDigitsRaw(input, pos).flatMap { case (_r298, _p299) => parseFloatExponent(input, _p299).map { case (_r300, _p301) => (new ~(_r298, _r300), _p301) } }).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseFloatLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(16, pos) {
    parseFloatLiteralCore(input, pos).flatMap { case (_r310, _p311) => (parseNumericSuffix(input, _p311).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p311)))).map { case (_r312, _p313) => (new ~(_r310, _r312), _p313) } }.flatMap { case (_r306, _p307) => (if (parseIdentCont(input, _p307).isEmpty) Some(((), _p307)) else None).map { case (_r308, _p309) => (new ~(_r306, _r308), _p309) } }.flatMap { case (_r302, _p303) => parseInlineSpacing(input, _p303).map { case (_r304, _p305) => (new ~(_r302, _r304), _p305) } }.map { case (r, p) => (_applyAction({  _ => FloatLiteral(0.0)  }, r), p) }
  }

  def parseIntegerLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(17, pos) {
    parseIntegerLiteralCore(input, pos).flatMap { case (_r326, _p327) => (if ((if (input.startsWith(".", _p327)) Some((".", _p327 + 1)) else None).flatMap { case (_r330, _p331) => (if (_p331 < input.length && { val _c = input.charAt(_p331); (_c >= '0' && _c <= '9') }) Some((input.charAt(_p331).toString, _p331 + 1)) else None).map { case (_r332, _p333) => (new ~(_r330, _r332), _p333) } }.isEmpty) Some(((), _p327)) else None).map { case (_r328, _p329) => (new ~(_r326, _r328), _p329) } }.flatMap { case (_r322, _p323) => (parseNumericSuffix(input, _p323).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p323)))).map { case (_r324, _p325) => (new ~(_r322, _r324), _p325) } }.flatMap { case (_r318, _p319) => (if (parseIdentCont(input, _p319).isEmpty) Some(((), _p319)) else None).map { case (_r320, _p321) => (new ~(_r318, _r320), _p321) } }.flatMap { case (_r314, _p315) => parseInlineSpacing(input, _p315).map { case (_r316, _p317) => (new ~(_r314, _r316), _p317) } }.map { case (r, p) => (_applyAction({  _ => IntLiteral(BigInt(0))  }, r), p) }
  }

  def parseCharLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("?", pos)) Some(("?", pos + 1)) else None).flatMap { case (_r342, _p343) => (if ((((if (input.startsWith(" ", _p343)) Some((" ", _p343 + 1)) else None)).orElse((if (input.startsWith("\t", _p343)) Some(("\t", _p343 + 1)) else None))).orElse((if (input.startsWith("\n", _p343)) Some(("\n", _p343 + 1)) else None)).isEmpty) Some(((), _p343)) else None).map { case (_r344, _p345) => (new ~(_r342, _r344), _p345) } }.flatMap { case (_r338, _p339) => ((if (input.startsWith("\\", _p339)) Some(("\\", _p339 + 1)) else None).flatMap { case (_r346, _p347) => (if (_p347 < input.length) Some((input.charAt(_p347).toString, _p347 + 1)) else None).map { case (_r348, _p349) => (new ~(_r346, _r348), _p349) } }).orElse((if (_p339 < input.length) Some((input.charAt(_p339).toString, _p339 + 1)) else None)).map { case (_r340, _p341) => (new ~(_r338, _r340), _p341) } }.flatMap { case (_r334, _p335) => parseInlineSpacing(input, _p335).map { case (_r336, _p337) => (new ~(_r334, _r336), _p337) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parseEscapedChar(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None).flatMap { case (_r350, _p351) => (if (_p351 < input.length) Some((input.charAt(_p351).toString, _p351 + 1)) else None).map { case (_r352, _p353) => (new ~(_r350, _r352), _p353) } }.map { case (r, p) => (_applyAction({  case _ ~ c => c.toString  }, r), p) }

  def parseInterpolationChunkInner(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("\"", pos)) Some(("\"", pos + 1)) else None).flatMap { case (_r358, _p359) => {
  var _rs362: List[Any] = Nil; var _cp363: Int = _p359; var _go366 = true
  while (_go366) { (if ((if (input.startsWith("\"", _cp363)) Some(("\"", _cp363 + 1)) else None).isEmpty) Some(((), _cp363)) else None).flatMap { case (_r367, _p368) => (if (_p368 < input.length) Some((input.charAt(_p368).toString, _p368 + 1)) else None).map { case (_r369, _p370) => (new ~(_r367, _r369), _p370) } } match {
    case Some((_st364, _np365)) => _rs362 = _rs362 :+ _st364; _cp363 = _np365
    case None => _go366 = false } }
  Some((_rs362, _cp363)) }.map { case (_r360, _p361) => (new ~(_r358, _r360), _p361) } }.flatMap { case (_r354, _p355) => (if (input.startsWith("\"", _p355)) Some(("\"", _p355 + 1)) else None).map { case (_r356, _p357) => (new ~(_r354, _r356), _p357) } }).orElse((if (input.startsWith("'", pos)) Some(("'", pos + 1)) else None).flatMap { case (_r375, _p376) => {
  var _rs379: List[Any] = Nil; var _cp380: Int = _p376; var _go383 = true
  while (_go383) { (if ((if (input.startsWith("'", _cp380)) Some(("'", _cp380 + 1)) else None).isEmpty) Some(((), _cp380)) else None).flatMap { case (_r384, _p385) => (if (_p385 < input.length) Some((input.charAt(_p385).toString, _p385 + 1)) else None).map { case (_r386, _p387) => (new ~(_r384, _r386), _p387) } } match {
    case Some((_st381, _np382)) => _rs379 = _rs379 :+ _st381; _cp380 = _np382
    case None => _go383 = false } }
  Some((_rs379, _cp380)) }.map { case (_r377, _p378) => (new ~(_r375, _r377), _p378) } }.flatMap { case (_r371, _p372) => (if (input.startsWith("'", _p372)) Some(("'", _p372 + 1)) else None).map { case (_r373, _p374) => (new ~(_r371, _r373), _p374) } })).orElse((if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r392, _p393) => {
  var _rs396: List[Any] = Nil; var _cp397: Int = _p393; var _go400 = true
  while (_go400) { parseInterpolationChunkInner(input, _cp397) match {
    case Some((_st398, _np399)) => _rs396 = _rs396 :+ _st398; _cp397 = _np399
    case None => _go400 = false } }
  Some((_rs396, _cp397)) }.map { case (_r394, _p395) => (new ~(_r392, _r394), _p395) } }.flatMap { case (_r388, _p389) => (if (input.startsWith("}", _p389)) Some(("}", _p389 + 1)) else None).map { case (_r390, _p391) => (new ~(_r388, _r390), _p391) } })).orElse((if ((if (input.startsWith("}", pos)) Some(("}", pos + 1)) else None).isEmpty) Some(((), pos)) else None).flatMap { case (_r401, _p402) => (if (_p402 < input.length) Some((input.charAt(_p402).toString, _p402 + 1)) else None).map { case (_r403, _p404) => (new ~(_r401, _r403), _p404) } }).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseInterpolationSegment(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("#{", pos)) Some(("#{", pos + 2)) else None).flatMap { case (_r409, _p410) => {
  var _rs413: List[Any] = Nil; var _cp414: Int = _p410; var _go417 = true
  while (_go417) { parseInterpolationChunkInner(input, _cp414) match {
    case Some((_st415, _np416)) => _rs413 = _rs413 :+ _st415; _cp414 = _np416
    case None => _go417 = false } }
  Some((_rs413, _cp414)) }.map { case (_r411, _p412) => (new ~(_r409, _r411), _p412) } }.flatMap { case (_r405, _p406) => (if (input.startsWith("}", _p406)) Some(("}", _p406 + 1)) else None).map { case (_r407, _p408) => (new ~(_r405, _r407), _p408) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDqStringChar(input: String, pos: Int): Option[(Any, Int)] = (if ((((if (input.startsWith("\"", pos)) Some(("\"", pos + 1)) else None)).orElse((if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None))).orElse((if (input.startsWith("#{", pos)) Some(("#{", pos + 2)) else None)).isEmpty) Some(((), pos)) else None).flatMap { case (_r418, _p419) => (if (_p419 < input.length) Some((input.charAt(_p419).toString, _p419 + 1)) else None).map { case (_r420, _p421) => (new ~(_r418, _r420), _p421) } }.map { case (r, p) => (_applyAction({  c => c.toString  }, r), p) }

  def parseStringLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(18, pos) {
    (if (input.startsWith("\"", pos)) Some(("\"", pos + 1)) else None).flatMap { case (_r430, _p431) => {
  var _rs434: List[Any] = Nil; var _cp435: Int = _p431; var _go438 = true
  while (_go438) { ((parseEscapedChar(input, _cp435)).orElse(parseInterpolationSegment(input, _cp435))).orElse(parseDqStringChar(input, _cp435)) match {
    case Some((_st436, _np437)) => _rs434 = _rs434 :+ _st436; _cp435 = _np437
    case None => _go438 = false } }
  Some((_rs434, _cp435)) }.map { case (_r432, _p433) => (new ~(_r430, _r432), _p433) } }.flatMap { case (_r426, _p427) => (if (input.startsWith("\"", _p427)) Some(("\"", _p427 + 1)) else None).map { case (_r428, _p429) => (new ~(_r426, _r428), _p429) } }.flatMap { case (_r422, _p423) => parseInlineSpacing(input, _p423).map { case (_r424, _p425) => (new ~(_r422, _r424), _p425) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }
  }

  def parseSqStringChar(input: String, pos: Int): Option[(Any, Int)] = (if (((if (input.startsWith("'", pos)) Some(("'", pos + 1)) else None)).orElse((if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None)).isEmpty) Some(((), pos)) else None).flatMap { case (_r439, _p440) => (if (_p440 < input.length) Some((input.charAt(_p440).toString, _p440 + 1)) else None).map { case (_r441, _p442) => (new ~(_r439, _r441), _p442) } }.map { case (r, p) => (_applyAction({  c => c.toString  }, r), p) }

  def parseEscapedSqChar(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None).flatMap { case (_r443, _p444) => ((if (input.startsWith("'", _p444)) Some(("'", _p444 + 1)) else None)).orElse((if (input.startsWith("\\", _p444)) Some(("\\", _p444 + 1)) else None)).map { case (_r445, _p446) => (new ~(_r443, _r445), _p446) } }.map { case (r, p) => (_applyAction({  case _ ~ c => c.toString  }, r), p) }

  def parseSingleQuotedStringLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(19, pos) {
    (if (input.startsWith("'", pos)) Some(("'", pos + 1)) else None).flatMap { case (_r455, _p456) => {
  var _rs459: List[Any] = Nil; var _cp460: Int = _p456; var _go463 = true
  while (_go463) { (parseEscapedSqChar(input, _cp460)).orElse(parseSqStringChar(input, _cp460)) match {
    case Some((_st461, _np462)) => _rs459 = _rs459 :+ _st461; _cp460 = _np462
    case None => _go463 = false } }
  Some((_rs459, _cp460)) }.map { case (_r457, _p458) => (new ~(_r455, _r457), _p458) } }.flatMap { case (_r451, _p452) => (if (input.startsWith("'", _p452)) Some(("'", _p452 + 1)) else None).map { case (_r453, _p454) => (new ~(_r451, _r453), _p454) } }.flatMap { case (_r447, _p448) => parseInlineSpacing(input, _p448).map { case (_r449, _p450) => (new ~(_r447, _r449), _p450) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }
  }

  def parseBacktickChar(input: String, pos: Int): Option[(Any, Int)] = (if ((((if (input.startsWith("`", pos)) Some(("`", pos + 1)) else None)).orElse((if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None))).orElse((if (input.startsWith("#{", pos)) Some(("#{", pos + 2)) else None)).isEmpty) Some(((), pos)) else None).flatMap { case (_r464, _p465) => (if (_p465 < input.length) Some((input.charAt(_p465).toString, _p465 + 1)) else None).map { case (_r466, _p467) => (new ~(_r464, _r466), _p467) } }.map { case (r, p) => (_applyAction({  c => c.toString  }, r), p) }

  def parseBacktickLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("`", pos)) Some(("`", pos + 1)) else None).flatMap { case (_r476, _p477) => {
  var _rs480: List[Any] = Nil; var _cp481: Int = _p477; var _go484 = true
  while (_go484) { ((parseEscapedChar(input, _cp481)).orElse(parseInterpolationSegment(input, _cp481))).orElse(parseBacktickChar(input, _cp481)) match {
    case Some((_st482, _np483)) => _rs480 = _rs480 :+ _st482; _cp481 = _np483
    case None => _go484 = false } }
  Some((_rs480, _cp481)) }.map { case (_r478, _p479) => (new ~(_r476, _r478), _p479) } }.flatMap { case (_r472, _p473) => (if (input.startsWith("`", _p473)) Some(("`", _p473 + 1)) else None).map { case (_r474, _p475) => (new ~(_r472, _r474), _p475) } }.flatMap { case (_r468, _p469) => parseInlineSpacing(input, _p469).map { case (_r470, _p471) => (new ~(_r468, _r470), _p471) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parsePercentBody(input: String, pos: Int): Option[(Any, Int)] = ((((((((((((((((parsePercentBodyBraces(input, pos)).orElse(parsePercentBodyParens(input, pos))).orElse(parsePercentBodyBrackets(input, pos))).orElse(parsePercentBodyAngles(input, pos))).orElse(parsePercentBodySimplePipe(input, pos))).orElse(parsePercentBodySimplePercent(input, pos))).orElse(parsePercentBodySimpleDoubleQuote(input, pos))).orElse(parsePercentBodySimpleSingleQuote(input, pos))).orElse(parsePercentBodySimpleSlash(input, pos))).orElse(parsePercentBodySimpleColon(input, pos))).orElse(parsePercentBodySimpleExcl(input, pos))).orElse(parsePercentBodySimpleComma(input, pos))).orElse(parsePercentBodySimpleSemicolon(input, pos))).orElse(parsePercentBodySimpleDash(input, pos))).orElse(parsePercentBodySimpleUnderscore(input, pos))).orElse(parsePercentBodySimpleEquals(input, pos))).orElse(parsePercentBodySimplePlus(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentQuotedStringLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None).flatMap { case (_r497, _p498) => (if ((((((((if (input.startsWith("r", _p498)) Some(("r", _p498 + 1)) else None)).orElse((if (input.startsWith("w", _p498)) Some(("w", _p498 + 1)) else None))).orElse((if (input.startsWith("W", _p498)) Some(("W", _p498 + 1)) else None))).orElse((if (input.startsWith("i", _p498)) Some(("i", _p498 + 1)) else None))).orElse((if (input.startsWith("I", _p498)) Some(("I", _p498 + 1)) else None))).orElse((if (input.startsWith("s", _p498)) Some(("s", _p498 + 1)) else None))).orElse((if (input.startsWith("x", _p498)) Some(("x", _p498 + 1)) else None)).isEmpty) Some(((), _p498)) else None).map { case (_r499, _p500) => (new ~(_r497, _r499), _p500) } }.flatMap { case (_r493, _p494) => (((if (input.startsWith("q", _p494)) Some(("q", _p494 + 1)) else None)).orElse((if (input.startsWith("Q", _p494)) Some(("Q", _p494 + 1)) else None)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p494)))).map { case (_r495, _p496) => (new ~(_r493, _r495), _p496) } }.flatMap { case (_r489, _p490) => parsePercentBody(input, _p490).map { case (_r491, _p492) => (new ~(_r489, _r491), _p492) } }.flatMap { case (_r485, _p486) => parseInlineSpacing(input, _p486).map { case (_r487, _p488) => (new ~(_r485, _r487), _p488) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parsePercentWordArray(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None).flatMap { case (_r509, _p510) => ((if (input.startsWith("w", _p510)) Some(("w", _p510 + 1)) else None)).orElse((if (input.startsWith("W", _p510)) Some(("W", _p510 + 1)) else None)).map { case (_r511, _p512) => (new ~(_r509, _r511), _p512) } }.flatMap { case (_r505, _p506) => parsePercentBody(input, _p506).map { case (_r507, _p508) => (new ~(_r505, _r507), _p508) } }.flatMap { case (_r501, _p502) => parseInlineSpacing(input, _p502).map { case (_r503, _p504) => (new ~(_r501, _r503), _p504) } }.map { case (r, p) => (_applyAction({  _ => ArrayLiteral(List.empty)  }, r), p) }

  def parsePercentSymbolArray(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None).flatMap { case (_r521, _p522) => ((if (input.startsWith("i", _p522)) Some(("i", _p522 + 1)) else None)).orElse((if (input.startsWith("I", _p522)) Some(("I", _p522 + 1)) else None)).map { case (_r523, _p524) => (new ~(_r521, _r523), _p524) } }.flatMap { case (_r517, _p518) => parsePercentBody(input, _p518).map { case (_r519, _p520) => (new ~(_r517, _r519), _p520) } }.flatMap { case (_r513, _p514) => parseInlineSpacing(input, _p514).map { case (_r515, _p516) => (new ~(_r513, _r515), _p516) } }.map { case (r, p) => (_applyAction({  _ => ArrayLiteral(List.empty)  }, r), p) }

  def parsePercentSymbolLiteralExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%s", pos)) Some(("%s", pos + 2)) else None).flatMap { case (_r529, _p530) => parsePercentBody(input, _p530).map { case (_r531, _p532) => (new ~(_r529, _r531), _p532) } }.flatMap { case (_r525, _p526) => parseInlineSpacing(input, _p526).map { case (_r527, _p528) => (new ~(_r525, _r527), _p528) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parsePercentRegex(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%r", pos)) Some(("%r", pos + 2)) else None).flatMap { case (_r541, _p542) => parsePercentBody(input, _p542).map { case (_r543, _p544) => (new ~(_r541, _r543), _p544) } }.flatMap { case (_r537, _p538) => {
  var _rs545: List[Any] = Nil; var _cp546: Int = _p538; var _go549 = true
  while (_go549) { (if (_cp546 < input.length && { val _c = input.charAt(_cp546); (_c >= 'a' && _c <= 'z') || (_c >= 'A' && _c <= 'Z') }) Some((input.charAt(_cp546).toString, _cp546 + 1)) else None) match {
    case Some((_st547, _np548)) => _rs545 = _rs545 :+ _st547; _cp546 = _np548
    case None => _go549 = false } }
  Some((_rs545, _cp546)) }.map { case (_r539, _p540) => (new ~(_r537, _r539), _p540) } }.flatMap { case (_r533, _p534) => parseInlineSpacing(input, _p534).map { case (_r535, _p536) => (new ~(_r533, _r535), _p536) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parsePercentCommand(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%x", pos)) Some(("%x", pos + 2)) else None).flatMap { case (_r554, _p555) => parsePercentBody(input, _p555).map { case (_r556, _p557) => (new ~(_r554, _r556), _p557) } }.flatMap { case (_r550, _p551) => parseInlineSpacing(input, _p551).map { case (_r552, _p553) => (new ~(_r550, _r552), _p553) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parseEscapedRegexChar(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None).flatMap { case (_r558, _p559) => (if (_p559 < input.length) Some((input.charAt(_p559).toString, _p559 + 1)) else None).map { case (_r560, _p561) => (new ~(_r558, _r560), _p561) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePlainRegexChar(input: String, pos: Int): Option[(Any, Int)] = (if ((((if (input.startsWith("/", pos)) Some(("/", pos + 1)) else None)).orElse((if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None))).orElse((if (input.startsWith("#{", pos)) Some(("#{", pos + 2)) else None)).isEmpty) Some(((), pos)) else None).flatMap { case (_r562, _p563) => (if (_p563 < input.length) Some((input.charAt(_p563).toString, _p563 + 1)) else None).map { case (_r564, _p565) => (new ~(_r562, _r564), _p565) } }.map { case (r, p) => (_applyAction({  c => c.toString  }, r), p) }

  def parseRegexBodyChars(input: String, pos: Int): Option[(Any, Int)] = {
  var _rs566: List[Any] = Nil; var _cp567: Int = pos; var _go570 = true
  while (_go570) { ((parseEscapedRegexChar(input, _cp567)).orElse(parseInterpolationSegment(input, _cp567))).orElse(parsePlainRegexChar(input, _cp567)) match {
    case Some((_st568, _np569)) => _rs566 = _rs566 :+ _st568; _cp567 = _np569
    case None => _go570 = false } }
  Some((_rs566, _cp567)) }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseRegexLiteral(input: String, pos: Int): Option[(Any, Int)] = (parsePercentRegex(input, pos)).orElse((if (input.startsWith("/", pos)) Some(("/", pos + 1)) else None).flatMap { case (_r587, _p588) => (if (((if (input.startsWith(" ", _p588)) Some((" ", _p588 + 1)) else None)).orElse((if (input.startsWith("\t", _p588)) Some(("\t", _p588 + 1)) else None)).isEmpty) Some(((), _p588)) else None).map { case (_r589, _p590) => (new ~(_r587, _r589), _p590) } }.flatMap { case (_r583, _p584) => parseRegexBodyChars(input, _p584).map { case (_r585, _p586) => (new ~(_r583, _r585), _p586) } }.flatMap { case (_r579, _p580) => (if (input.startsWith("/", _p580)) Some(("/", _p580 + 1)) else None).map { case (_r581, _p582) => (new ~(_r579, _r581), _p582) } }.flatMap { case (_r575, _p576) => {
  var _rs591: List[Any] = Nil; var _cp592: Int = _p576; var _go595 = true
  while (_go595) { (if (_cp592 < input.length && { val _c = input.charAt(_cp592); (_c >= 'a' && _c <= 'z') || (_c >= 'A' && _c <= 'Z') }) Some((input.charAt(_cp592).toString, _cp592 + 1)) else None) match {
    case Some((_st593, _np594)) => _rs591 = _rs591 :+ _st593; _cp592 = _np594
    case None => _go595 = false } }
  Some((_rs591, _cp592)) }.map { case (_r577, _p578) => (new ~(_r575, _r577), _p578) } }.flatMap { case (_r571, _p572) => parseInlineSpacing(input, _p572).map { case (_r573, _p574) => (new ~(_r571, _r573), _p574) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) })

  def parseSymbolNamePart(input: String, pos: Int): Option[(Any, Int)] = ((((((parseSymbolOperatorName(input, pos)).orElse(parseClassVarNameRaw(input, pos))).orElse(parseInstanceVarNameRaw(input, pos))).orElse(parseGlobalVarNamed(input, pos))).orElse(parseMethodIdentifierRaw(input, pos))).orElse(parseConstNameNoSpace(input, pos))).orElse(parseReservedWord(input, pos).map { case (r, p) => (_applyAction({  r => r  }, r), p) }).map { case (r, p) => (_applyAction({  v => v  }, r), p) }

  def parseSymbolLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(20, pos) {
    (if (input.startsWith(":", pos)) Some((":", pos + 1)) else None).flatMap { case (_r604, _p605) => (if ((if (input.startsWith(":", _p605)) Some((":", _p605 + 1)) else None).isEmpty) Some(((), _p605)) else None).map { case (_r606, _p607) => (new ~(_r604, _r606), _p607) } }.flatMap { case (_r600, _p601) => (((if (input.startsWith("\"", _p601)) Some(("\"", _p601 + 1)) else None).flatMap { case (_r612, _p613) => {
  var _rs616: List[Any] = Nil; var _cp617: Int = _p613; var _go620 = true
  while (_go620) { ((parseEscapedChar(input, _cp617)).orElse(parseInterpolationSegment(input, _cp617))).orElse(parseDqStringChar(input, _cp617)) match {
    case Some((_st618, _np619)) => _rs616 = _rs616 :+ _st618; _cp617 = _np619
    case None => _go620 = false } }
  Some((_rs616, _cp617)) }.map { case (_r614, _p615) => (new ~(_r612, _r614), _p615) } }.flatMap { case (_r608, _p609) => (if (input.startsWith("\"", _p609)) Some(("\"", _p609 + 1)) else None).map { case (_r610, _p611) => (new ~(_r608, _r610), _p611) } }).orElse((if (input.startsWith("'", _p601)) Some(("'", _p601 + 1)) else None).flatMap { case (_r625, _p626) => {
  var _rs629: List[Any] = Nil; var _cp630: Int = _p626; var _go633 = true
  while (_go633) { (parseEscapedSqChar(input, _cp630)).orElse(parseSqStringChar(input, _cp630)) match {
    case Some((_st631, _np632)) => _rs629 = _rs629 :+ _st631; _cp630 = _np632
    case None => _go633 = false } }
  Some((_rs629, _cp630)) }.map { case (_r627, _p628) => (new ~(_r625, _r627), _p628) } }.flatMap { case (_r621, _p622) => (if (input.startsWith("'", _p622)) Some(("'", _p622 + 1)) else None).map { case (_r623, _p624) => (new ~(_r621, _r623), _p624) } })).orElse(parseSymbolNamePart(input, _p601)).map { case (_r602, _p603) => (new ~(_r600, _r602), _p603) } }.flatMap { case (_r596, _p597) => parseInlineSpacing(input, _p597).map { case (_r598, _p599) => (new ~(_r596, _r598), _p599) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }
  }

  def parseLabelNameNoSpace(input: String, pos: Int): Option[(Any, Int)] = ((if (parseReservedWord(input, pos).isEmpty) Some(((), pos)) else None).flatMap { case (_r634, _p635) => parseIdentifierRaw(input, _p635).map { case (_r636, _p637) => (new ~(_r634, _r636), _p637) } }).orElse(parseReservedWord(input, pos)).map { case (r, p) => (_applyAction({  v => v  }, r), p) }

  def parseLabelSymbol(input: String, pos: Int): Option[(Any, Int)] = parseLabelNameNoSpace(input, pos).flatMap { case (_r646, _p647) => (if (input.startsWith(":", _p647)) Some((":", _p647 + 1)) else None).map { case (_r648, _p649) => (new ~(_r646, _r648), _p649) } }.flatMap { case (_r642, _p643) => (if ((if (input.startsWith(":", _p643)) Some((":", _p643 + 1)) else None).isEmpty) Some(((), _p643)) else None).map { case (_r644, _p645) => (new ~(_r642, _r644), _p645) } }.flatMap { case (_r638, _p639) => parseInlineSpacing(input, _p639).map { case (_r640, _p641) => (new ~(_r638, _r640), _p641) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parseQuotedLabelSymbol(input: String, pos: Int): Option[(Any, Int)] = (parseStringLiteral(input, pos)).orElse(parseSymbolLiteral(input, pos)).flatMap { case (_r654, _p655) => parseLabelColon(input, _p655).map { case (_r656, _p657) => (new ~(_r654, _r656), _p657) } }.flatMap { case (_r650, _p651) => parseSpacing(input, _p651).map { case (_r652, _p653) => (new ~(_r650, _r652), _p653) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parseSelfExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(21, pos) {
    (if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r662, _p663) => (if (parseIdentCont(input, _p663).isEmpty) Some(((), _p663)) else None).map { case (_r664, _p665) => (new ~(_r662, _r664), _p665) } }.flatMap { case (_r658, _p659) => parseInlineSpacing(input, _p659).map { case (_r660, _p661) => (new ~(_r658, _r660), _p661) } }.map { case (r, p) => (_applyAction({  _ => SelfExpr()  }, r), p) }
  }

  def parseBoolLiteral(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("true", pos)) Some(("true", pos + 4)) else None).flatMap { case (_r670, _p671) => (if (parseIdentCont(input, _p671).isEmpty) Some(((), _p671)) else None).map { case (_r672, _p673) => (new ~(_r670, _r672), _p673) } }.flatMap { case (_r666, _p667) => parseInlineSpacing(input, _p667).map { case (_r668, _p669) => (new ~(_r666, _r668), _p669) } }).orElse((if (input.startsWith("false", pos)) Some(("false", pos + 5)) else None).flatMap { case (_r678, _p679) => (if (parseIdentCont(input, _p679).isEmpty) Some(((), _p679)) else None).map { case (_r680, _p681) => (new ~(_r678, _r680), _p681) } }.flatMap { case (_r674, _p675) => parseInlineSpacing(input, _p675).map { case (_r676, _p677) => (new ~(_r674, _r676), _p677) } }).map { case (r, p) => (_applyAction({  _ => BoolLiteral(true)  }, r), p) }

  def parseNilLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("nil", pos)) Some(("nil", pos + 3)) else None).flatMap { case (_r686, _p687) => (if (parseIdentCont(input, _p687).isEmpty) Some(((), _p687)) else None).map { case (_r688, _p689) => (new ~(_r686, _r688), _p689) } }.flatMap { case (_r682, _p683) => parseInlineSpacing(input, _p683).map { case (_r684, _p685) => (new ~(_r682, _r684), _p685) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseLocalVarExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(22, pos) {
    parseIdentifier(input, pos).map { case (r, p) => (_applyAction({  name => LocalVar(name.asInstanceOf[String])  }, r), p) }
  }

  def parseInstanceVarExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(23, pos) {
    parseInstanceVarName(input, pos).map { case (r, p) => (_applyAction({  name => InstanceVar(name.asInstanceOf[String])  }, r), p) }
  }

  def parseClassVarExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(24, pos) {
    parseClassVarName(input, pos).map { case (r, p) => (_applyAction({  name => ClassVar(name.asInstanceOf[String])  }, r), p) }
  }

  def parseGlobalVarExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(25, pos) {
    parseGlobalVarName(input, pos).map { case (r, p) => (_applyAction({  name => GlobalVar(name.asInstanceOf[String])  }, r), p) }
  }

  def parseVariable(input: String, pos: Int): Option[(Any, Int)] = _withMemo(26, pos) {
    (((parseInstanceVarExpr(input, pos)).orElse(parseClassVarExpr(input, pos))).orElse(parseGlobalVarExpr(input, pos))).orElse(parseLocalVarExpr(input, pos))
  }

  def parseConstRef(input: String, pos: Int): Option[(Any, Int)] = _withMemo(27, pos) {
    parseConstPath(input, pos).map { case (r, p) => (_applyAction({  path => ConstRef(path.asInstanceOf[List[String]])  }, r), p) }
  }

  def parseParenExprInner(input: String, pos: Int): Option[(Any, Int)] = {
  var _rs698: List[Any] = Nil; var _cp699: Int = pos; var _go702 = true
  while (_go702) { parseStatementSep(input, _cp699) match {
    case Some((_st700, _np701)) => _rs698 = _rs698 :+ _st700; _cp699 = _np701
    case None => _go702 = false } }
  Some((_rs698, _cp699)) }.flatMap { case (_r694, _p695) => (parseExpr(input, _p695).flatMap { case (_r703, _p704) => {
  var _rs707: List[Any] = Nil; var _cp708: Int = _p704; var _go711 = true
  while (_go711) { {
  parseStatementSep(input, _cp708) match {
    case None => None
    case Some((_fs716, _fp717)) =>
      var _rs718: List[Any] = List(_fs716); var _cp719: Int = _fp717; var _go722 = true
      while (_go722) { parseStatementSep(input, _cp719) match {
        case Some((_st720, _np721)) => _rs718 = _rs718 :+ _st720; _cp719 = _np721
        case None => _go722 = false } }
      Some((_rs718, _cp719)) } }.flatMap { case (_r712, _p713) => parseExpr(input, _p713).map { case (_r714, _p715) => (new ~(_r712, _r714), _p715) } } match {
    case Some((_st709, _np710)) => _rs707 = _rs707 :+ _st709; _cp708 = _np710
    case None => _go711 = false } }
  Some((_rs707, _cp708)) }.map { case (_r705, _p706) => (new ~(_r703, _r705), _p706) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p695)))).map { case (_r696, _p697) => (new ~(_r694, _r696), _p697) } }.flatMap { case (_r690, _p691) => {
  var _rs723: List[Any] = Nil; var _cp724: Int = _p691; var _go727 = true
  while (_go727) { parseStatementSep(input, _cp724) match {
    case Some((_st725, _np726)) => _rs723 = _rs723 :+ _st725; _cp724 = _np726
    case None => _go727 = false } }
  Some((_rs723, _cp724)) }.map { case (_r692, _p693) => (new ~(_r690, _r692), _p693) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseParenExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r740, _p741) => parseSpacing(input, _p741).map { case (_r742, _p743) => (new ~(_r740, _r742), _p743) } }.flatMap { case (_r736, _p737) => parseParenExprInner(input, _p737).map { case (_r738, _p739) => (new ~(_r736, _r738), _p739) } }.flatMap { case (_r732, _p733) => (if (input.startsWith(")", _p733)) Some((")", _p733 + 1)) else None).map { case (_r734, _p735) => (new ~(_r732, _r734), _p735) } }.flatMap { case (_r728, _p729) => parseInlineSpacing(input, _p729).map { case (_r730, _p731) => (new ~(_r728, _r730), _p731) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseArrayElem(input: String, pos: Int): Option[(Any, Int)] = ((((((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r748, _p749) => parseSpacing(input, _p749).map { case (_r750, _p751) => (new ~(_r748, _r750), _p751) } }.flatMap { case (_r744, _p745) => parseExpr(input, _p745).map { case (_r746, _p747) => (new ~(_r744, _r746), _p747) } }).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r756, _p757) => parseSpacing(input, _p757).map { case (_r758, _p759) => (new ~(_r756, _r758), _p759) } }.flatMap { case (_r752, _p753) => parseExpr(input, _p753).map { case (_r754, _p755) => (new ~(_r752, _r754), _p755) } })).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r764, _p765) => parseSpacing(input, _p765).map { case (_r766, _p767) => (new ~(_r764, _r766), _p767) } }.flatMap { case (_r760, _p761) => parseExpr(input, _p761).map { case (_r762, _p763) => (new ~(_r760, _r762), _p763) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r772, _p773) => parseSpacing(input, _p773).map { case (_r774, _p775) => (new ~(_r772, _r774), _p775) } }.flatMap { case (_r768, _p769) => parseExpr(input, _p769).map { case (_r770, _p771) => (new ~(_r768, _r770), _p771) } })).orElse(parseExpr(input, pos).flatMap { case (_r788, _p789) => parseSpacing(input, _p789).map { case (_r790, _p791) => (new ~(_r788, _r790), _p791) } }.flatMap { case (_r784, _p785) => (if (input.startsWith("=>", _p785)) Some(("=>", _p785 + 2)) else None).map { case (_r786, _p787) => (new ~(_r784, _r786), _p787) } }.flatMap { case (_r780, _p781) => parseSpacing(input, _p781).map { case (_r782, _p783) => (new ~(_r780, _r782), _p783) } }.flatMap { case (_r776, _p777) => parseExpr(input, _p777).map { case (_r778, _p779) => (new ~(_r776, _r778), _p779) } })).orElse(parseExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseArrayLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(28, pos) {
    (if (input.startsWith("[", pos)) Some(("[", pos + 1)) else None).flatMap { case (_r804, _p805) => parseSpacing(input, _p805).map { case (_r806, _p807) => (new ~(_r804, _r806), _p807) } }.flatMap { case (_r800, _p801) => (parseArrayElem(input, _p801).flatMap { case (_r816, _p817) => {
  var _rs820: List[Any] = Nil; var _cp821: Int = _p817; var _go824 = true
  while (_go824) { parseSpacing(input, _cp821).flatMap { case (_r833, _p834) => (if (input.startsWith(",", _p834)) Some((",", _p834 + 1)) else None).map { case (_r835, _p836) => (new ~(_r833, _r835), _p836) } }.flatMap { case (_r829, _p830) => parseSpacing(input, _p830).map { case (_r831, _p832) => (new ~(_r829, _r831), _p832) } }.flatMap { case (_r825, _p826) => parseArrayElem(input, _p826).map { case (_r827, _p828) => (new ~(_r825, _r827), _p828) } } match {
    case Some((_st822, _np823)) => _rs820 = _rs820 :+ _st822; _cp821 = _np823
    case None => _go824 = false } }
  Some((_rs820, _cp821)) }.map { case (_r818, _p819) => (new ~(_r816, _r818), _p819) } }.flatMap { case (_r812, _p813) => (parseSpacing(input, _p813).flatMap { case (_r837, _p838) => (if (input.startsWith(",", _p838)) Some((",", _p838 + 1)) else None).map { case (_r839, _p840) => (new ~(_r837, _r839), _p840) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p813)))).map { case (_r814, _p815) => (new ~(_r812, _r814), _p815) } }.flatMap { case (_r808, _p809) => parseSpacing(input, _p809).map { case (_r810, _p811) => (new ~(_r808, _r810), _p811) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p801)))).map { case (_r802, _p803) => (new ~(_r800, _r802), _p803) } }.flatMap { case (_r796, _p797) => (if (input.startsWith("]", _p797)) Some(("]", _p797 + 1)) else None).map { case (_r798, _p799) => (new ~(_r796, _r798), _p799) } }.flatMap { case (_r792, _p793) => parseInlineSpacing(input, _p793).map { case (_r794, _p795) => (new ~(_r792, _r794), _p795) } }.map { case (r, p) => (_applyAction({  _ => ArrayLiteral(List.empty)  }, r), p) }
  }

  def parseHashEntry(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r845, _p846) => parseSpacing(input, _p846).map { case (_r847, _p848) => (new ~(_r845, _r847), _p848) } }.flatMap { case (_r841, _p842) => parseExpr(input, _p842).map { case (_r843, _p844) => (new ~(_r841, _r843), _p844) } }).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r853, _p854) => parseSpacing(input, _p854).map { case (_r855, _p856) => (new ~(_r853, _r855), _p856) } }.flatMap { case (_r849, _p850) => parseExpr(input, _p850).map { case (_r851, _p852) => (new ~(_r849, _r851), _p852) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r861, _p862) => parseSpacing(input, _p862).map { case (_r863, _p864) => (new ~(_r861, _r863), _p864) } }.flatMap { case (_r857, _p858) => parseExpr(input, _p858).map { case (_r859, _p860) => (new ~(_r857, _r859), _p860) } })).orElse(parseExpr(input, pos).flatMap { case (_r877, _p878) => parseSpacing(input, _p878).map { case (_r879, _p880) => (new ~(_r877, _r879), _p880) } }.flatMap { case (_r873, _p874) => (if (input.startsWith("=>", _p874)) Some(("=>", _p874 + 2)) else None).map { case (_r875, _p876) => (new ~(_r873, _r875), _p876) } }.flatMap { case (_r869, _p870) => parseSpacing(input, _p870).map { case (_r871, _p872) => (new ~(_r869, _r871), _p872) } }.flatMap { case (_r865, _p866) => parseExpr(input, _p866).map { case (_r867, _p868) => (new ~(_r865, _r867), _p868) } }).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseHashLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(29, pos) {
    (if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r893, _p894) => parseSpacing(input, _p894).map { case (_r895, _p896) => (new ~(_r893, _r895), _p896) } }.flatMap { case (_r889, _p890) => (parseHashEntry(input, _p890).flatMap { case (_r905, _p906) => {
  var _rs909: List[Any] = Nil; var _cp910: Int = _p906; var _go913 = true
  while (_go913) { parseSpacing(input, _cp910).flatMap { case (_r922, _p923) => (if (input.startsWith(",", _p923)) Some((",", _p923 + 1)) else None).map { case (_r924, _p925) => (new ~(_r922, _r924), _p925) } }.flatMap { case (_r918, _p919) => parseSpacing(input, _p919).map { case (_r920, _p921) => (new ~(_r918, _r920), _p921) } }.flatMap { case (_r914, _p915) => parseHashEntry(input, _p915).map { case (_r916, _p917) => (new ~(_r914, _r916), _p917) } } match {
    case Some((_st911, _np912)) => _rs909 = _rs909 :+ _st911; _cp910 = _np912
    case None => _go913 = false } }
  Some((_rs909, _cp910)) }.map { case (_r907, _p908) => (new ~(_r905, _r907), _p908) } }.flatMap { case (_r901, _p902) => (parseSpacing(input, _p902).flatMap { case (_r926, _p927) => (if (input.startsWith(",", _p927)) Some((",", _p927 + 1)) else None).map { case (_r928, _p929) => (new ~(_r926, _r928), _p929) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p902)))).map { case (_r903, _p904) => (new ~(_r901, _r903), _p904) } }.flatMap { case (_r897, _p898) => parseSpacing(input, _p898).map { case (_r899, _p900) => (new ~(_r897, _r899), _p900) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p890)))).map { case (_r891, _p892) => (new ~(_r889, _r891), _p892) } }.flatMap { case (_r885, _p886) => (if (input.startsWith("}", _p886)) Some(("}", _p886 + 1)) else None).map { case (_r887, _p888) => (new ~(_r885, _r887), _p888) } }.flatMap { case (_r881, _p882) => parseInlineSpacing(input, _p882).map { case (_r883, _p884) => (new ~(_r881, _r883), _p884) } }.map { case (r, p) => (_applyAction({  _ => HashLiteral(List.empty)  }, r), p) }
  }

  def parseKeywordParam(input: String, pos: Int): Option[(Any, Int)] = parseLabelNameNoSpace(input, pos).flatMap { case (_r942, _p943) => (if (input.startsWith(":", _p943)) Some((":", _p943 + 1)) else None).map { case (_r944, _p945) => (new ~(_r942, _r944), _p945) } }.flatMap { case (_r938, _p939) => (if ((if (input.startsWith(":", _p939)) Some((":", _p939 + 1)) else None).isEmpty) Some(((), _p939)) else None).map { case (_r940, _p941) => (new ~(_r938, _r940), _p941) } }.flatMap { case (_r934, _p935) => parseSpacing(input, _p935).map { case (_r936, _p937) => (new ~(_r934, _r936), _p937) } }.flatMap { case (_r930, _p931) => (parseExpr(input, _p931).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p931)))).map { case (_r932, _p933) => (new ~(_r930, _r932), _p933) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDestructuredParam(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r966, _p967) => parseSpacing(input, _p967).map { case (_r968, _p969) => (new ~(_r966, _r968), _p969) } }.flatMap { case (_r962, _p963) => parseFormalParam(input, _p963).map { case (_r964, _p965) => (new ~(_r962, _r964), _p965) } }.flatMap { case (_r958, _p959) => {
  var _rs970: List[Any] = Nil; var _cp971: Int = _p959; var _go974 = true
  while (_go974) { parseSpacing(input, _cp971).flatMap { case (_r983, _p984) => (if (input.startsWith(",", _p984)) Some((",", _p984 + 1)) else None).map { case (_r985, _p986) => (new ~(_r983, _r985), _p986) } }.flatMap { case (_r979, _p980) => parseSpacing(input, _p980).map { case (_r981, _p982) => (new ~(_r979, _r981), _p982) } }.flatMap { case (_r975, _p976) => parseFormalParam(input, _p976).map { case (_r977, _p978) => (new ~(_r975, _r977), _p978) } } match {
    case Some((_st972, _np973)) => _rs970 = _rs970 :+ _st972; _cp971 = _np973
    case None => _go974 = false } }
  Some((_rs970, _cp971)) }.map { case (_r960, _p961) => (new ~(_r958, _r960), _p961) } }.flatMap { case (_r954, _p955) => (parseSpacing(input, _p955).flatMap { case (_r987, _p988) => (if (input.startsWith(",", _p988)) Some((",", _p988 + 1)) else None).map { case (_r989, _p990) => (new ~(_r987, _r989), _p990) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p955)))).map { case (_r956, _p957) => (new ~(_r954, _r956), _p957) } }.flatMap { case (_r950, _p951) => parseSpacing(input, _p951).map { case (_r952, _p953) => (new ~(_r950, _r952), _p953) } }.flatMap { case (_r946, _p947) => (if (input.startsWith(")", _p947)) Some((")", _p947 + 1)) else None).map { case (_r948, _p949) => (new ~(_r946, _r948), _p949) } }.map { case (r, p) => (_applyAction({  _ => "()"  }, r), p) }

  def parseFormalParam(input: String, pos: Int): Option[(Any, Int)] = (((((((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r995, _p996) => parseSpacing(input, _p996).map { case (_r997, _p998) => (new ~(_r995, _r997), _p998) } }.flatMap { case (_r991, _p992) => (((if (input.startsWith("nil", _p992)) Some(("nil", _p992 + 3)) else None).flatMap { case (_r999, _p1000) => (if (parseIdentCont(input, _p1000).isEmpty) Some(((), _p1000)) else None).map { case (_r1001, _p1002) => (new ~(_r999, _r1001), _p1002) } }).orElse(parseIdentifier(input, _p992)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p992)))).map { case (_r993, _p994) => (new ~(_r991, _r993), _p994) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r1007, _p1008) => parseSpacing(input, _p1008).map { case (_r1009, _p1010) => (new ~(_r1007, _r1009), _p1010) } }.flatMap { case (_r1003, _p1004) => (((if (input.startsWith("nil", _p1004)) Some(("nil", _p1004 + 3)) else None).flatMap { case (_r1011, _p1012) => (if (parseIdentCont(input, _p1012).isEmpty) Some(((), _p1012)) else None).map { case (_r1013, _p1014) => (new ~(_r1011, _r1013), _p1014) } }).orElse(parseIdentifier(input, _p1004)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1004)))).map { case (_r1005, _p1006) => (new ~(_r1003, _r1005), _p1006) } })).orElse((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r1019, _p1020) => parseSpacing(input, _p1020).map { case (_r1021, _p1022) => (new ~(_r1019, _r1021), _p1022) } }.flatMap { case (_r1015, _p1016) => (((if (input.startsWith("nil", _p1016)) Some(("nil", _p1016 + 3)) else None).flatMap { case (_r1023, _p1024) => (if (parseIdentCont(input, _p1024).isEmpty) Some(((), _p1024)) else None).map { case (_r1025, _p1026) => (new ~(_r1023, _r1025), _p1026) } }).orElse(parseIdentifier(input, _p1016)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1016)))).map { case (_r1017, _p1018) => (new ~(_r1015, _r1017), _p1018) } })).orElse(parseKeywordParam(input, pos))).orElse(parseDestructuredParam(input, pos))).orElse(parseIdentifier(input, pos).flatMap { case (_r1027, _p1028) => (parseSpacing(input, _p1028).flatMap { case (_r1039, _p1040) => (if (input.startsWith("=", _p1040)) Some(("=", _p1040 + 1)) else None).map { case (_r1041, _p1042) => (new ~(_r1039, _r1041), _p1042) } }.flatMap { case (_r1035, _p1036) => parseSpacing(input, _p1036).map { case (_r1037, _p1038) => (new ~(_r1035, _r1037), _p1038) } }.flatMap { case (_r1031, _p1032) => parseExpr(input, _p1032).map { case (_r1033, _p1034) => (new ~(_r1031, _r1033), _p1034) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1028)))).map { case (_r1029, _p1030) => (new ~(_r1027, _r1029), _p1030) } }).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseParams(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r1055, _p1056) => parseSpacing(input, _p1056).map { case (_r1057, _p1058) => (new ~(_r1055, _r1057), _p1058) } }.flatMap { case (_r1051, _p1052) => (parseFormalParam(input, _p1052).flatMap { case (_r1067, _p1068) => {
  var _rs1071: List[Any] = Nil; var _cp1072: Int = _p1068; var _go1075 = true
  while (_go1075) { parseSpacing(input, _cp1072).flatMap { case (_r1084, _p1085) => (if (input.startsWith(",", _p1085)) Some((",", _p1085 + 1)) else None).map { case (_r1086, _p1087) => (new ~(_r1084, _r1086), _p1087) } }.flatMap { case (_r1080, _p1081) => parseSpacing(input, _p1081).map { case (_r1082, _p1083) => (new ~(_r1080, _r1082), _p1083) } }.flatMap { case (_r1076, _p1077) => parseFormalParam(input, _p1077).map { case (_r1078, _p1079) => (new ~(_r1076, _r1078), _p1079) } } match {
    case Some((_st1073, _np1074)) => _rs1071 = _rs1071 :+ _st1073; _cp1072 = _np1074
    case None => _go1075 = false } }
  Some((_rs1071, _cp1072)) }.map { case (_r1069, _p1070) => (new ~(_r1067, _r1069), _p1070) } }.flatMap { case (_r1063, _p1064) => (parseSpacing(input, _p1064).flatMap { case (_r1088, _p1089) => (if (input.startsWith(",", _p1089)) Some((",", _p1089 + 1)) else None).map { case (_r1090, _p1091) => (new ~(_r1088, _r1090), _p1091) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1064)))).map { case (_r1065, _p1066) => (new ~(_r1063, _r1065), _p1066) } }.flatMap { case (_r1059, _p1060) => parseSpacing(input, _p1060).map { case (_r1061, _p1062) => (new ~(_r1059, _r1061), _p1062) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1052)))).map { case (_r1053, _p1054) => (new ~(_r1051, _r1053), _p1054) } }.flatMap { case (_r1047, _p1048) => (if (input.startsWith(")", _p1048)) Some((")", _p1048 + 1)) else None).map { case (_r1049, _p1050) => (new ~(_r1047, _r1049), _p1050) } }.flatMap { case (_r1043, _p1044) => parseInlineSpacing(input, _p1044).map { case (_r1045, _p1046) => (new ~(_r1043, _r1045), _p1046) } }.map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseBareParams(input: String, pos: Int): Option[(Any, Int)] = parseFormalParam(input, pos).flatMap { case (_r1092, _p1093) => {
  parseInlineSpacing(input, _p1093).flatMap { case (_r1111, _p1112) => (if (input.startsWith(",", _p1112)) Some((",", _p1112 + 1)) else None).map { case (_r1113, _p1114) => (new ~(_r1111, _r1113), _p1114) } }.flatMap { case (_r1107, _p1108) => parseInlineSpacing(input, _p1108).map { case (_r1109, _p1110) => (new ~(_r1107, _r1109), _p1110) } }.flatMap { case (_r1103, _p1104) => parseFormalParam(input, _p1104).map { case (_r1105, _p1106) => (new ~(_r1103, _r1105), _p1106) } } match {
    case None => None
    case Some((_fs1096, _fp1097)) =>
      var _rs1098: List[Any] = List(_fs1096); var _cp1099: Int = _fp1097; var _go1102 = true
      while (_go1102) { parseInlineSpacing(input, _cp1099).flatMap { case (_r1123, _p1124) => (if (input.startsWith(",", _p1124)) Some((",", _p1124 + 1)) else None).map { case (_r1125, _p1126) => (new ~(_r1123, _r1125), _p1126) } }.flatMap { case (_r1119, _p1120) => parseInlineSpacing(input, _p1120).map { case (_r1121, _p1122) => (new ~(_r1119, _r1121), _p1122) } }.flatMap { case (_r1115, _p1116) => parseFormalParam(input, _p1116).map { case (_r1117, _p1118) => (new ~(_r1115, _r1117), _p1118) } } match {
        case Some((_st1100, _np1101)) => _rs1098 = _rs1098 :+ _st1100; _cp1099 = _np1101
        case None => _go1102 = false } }
      Some((_rs1098, _cp1099)) } }.map { case (_r1094, _p1095) => (new ~(_r1092, _r1094), _p1095) } }.map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseBlockFormalParam(input: String, pos: Int): Option[(Any, Int)] = (((((((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r1131, _p1132) => parseSpacing(input, _p1132).map { case (_r1133, _p1134) => (new ~(_r1131, _r1133), _p1134) } }.flatMap { case (_r1127, _p1128) => (((if (input.startsWith("nil", _p1128)) Some(("nil", _p1128 + 3)) else None).flatMap { case (_r1135, _p1136) => (if (parseIdentCont(input, _p1136).isEmpty) Some(((), _p1136)) else None).map { case (_r1137, _p1138) => (new ~(_r1135, _r1137), _p1138) } }).orElse(parseIdentifier(input, _p1128)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1128)))).map { case (_r1129, _p1130) => (new ~(_r1127, _r1129), _p1130) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r1143, _p1144) => parseSpacing(input, _p1144).map { case (_r1145, _p1146) => (new ~(_r1143, _r1145), _p1146) } }.flatMap { case (_r1139, _p1140) => (((if (input.startsWith("nil", _p1140)) Some(("nil", _p1140 + 3)) else None).flatMap { case (_r1147, _p1148) => (if (parseIdentCont(input, _p1148).isEmpty) Some(((), _p1148)) else None).map { case (_r1149, _p1150) => (new ~(_r1147, _r1149), _p1150) } }).orElse(parseIdentifier(input, _p1140)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1140)))).map { case (_r1141, _p1142) => (new ~(_r1139, _r1141), _p1142) } })).orElse((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r1155, _p1156) => parseSpacing(input, _p1156).map { case (_r1157, _p1158) => (new ~(_r1155, _r1157), _p1158) } }.flatMap { case (_r1151, _p1152) => (((if (input.startsWith("nil", _p1152)) Some(("nil", _p1152 + 3)) else None).flatMap { case (_r1159, _p1160) => (if (parseIdentCont(input, _p1160).isEmpty) Some(((), _p1160)) else None).map { case (_r1161, _p1162) => (new ~(_r1159, _r1161), _p1162) } }).orElse(parseIdentifier(input, _p1152)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1152)))).map { case (_r1153, _p1154) => (new ~(_r1151, _r1153), _p1154) } })).orElse(parseKeywordParam(input, pos))).orElse(parseDestructuredParam(input, pos))).orElse(parseIdentifier(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseBlockLocalVar(input: String, pos: Int): Option[(Any, Int)] = parseIdentifier(input, pos).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseBlockParams(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("|", pos)) Some(("|", pos + 1)) else None).flatMap { case (_r1183, _p1184) => parseSpacing(input, _p1184).map { case (_r1185, _p1186) => (new ~(_r1183, _r1185), _p1186) } }.flatMap { case (_r1179, _p1180) => (parseBlockFormalParam(input, _p1180).flatMap { case (_r1195, _p1196) => {
  var _rs1199: List[Any] = Nil; var _cp1200: Int = _p1196; var _go1203 = true
  while (_go1203) { parseSpacing(input, _cp1200).flatMap { case (_r1212, _p1213) => (if (input.startsWith(",", _p1213)) Some((",", _p1213 + 1)) else None).map { case (_r1214, _p1215) => (new ~(_r1212, _r1214), _p1215) } }.flatMap { case (_r1208, _p1209) => parseSpacing(input, _p1209).map { case (_r1210, _p1211) => (new ~(_r1208, _r1210), _p1211) } }.flatMap { case (_r1204, _p1205) => parseBlockFormalParam(input, _p1205).map { case (_r1206, _p1207) => (new ~(_r1204, _r1206), _p1207) } } match {
    case Some((_st1201, _np1202)) => _rs1199 = _rs1199 :+ _st1201; _cp1200 = _np1202
    case None => _go1203 = false } }
  Some((_rs1199, _cp1200)) }.map { case (_r1197, _p1198) => (new ~(_r1195, _r1197), _p1198) } }.flatMap { case (_r1191, _p1192) => (parseSpacing(input, _p1192).flatMap { case (_r1216, _p1217) => (if (input.startsWith(",", _p1217)) Some((",", _p1217 + 1)) else None).map { case (_r1218, _p1219) => (new ~(_r1216, _r1218), _p1219) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1192)))).map { case (_r1193, _p1194) => (new ~(_r1191, _r1193), _p1194) } }.flatMap { case (_r1187, _p1188) => parseSpacing(input, _p1188).map { case (_r1189, _p1190) => (new ~(_r1187, _r1189), _p1190) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1180)))).map { case (_r1181, _p1182) => (new ~(_r1179, _r1181), _p1182) } }.flatMap { case (_r1175, _p1176) => ((if (input.startsWith(";", _p1176)) Some((";", _p1176 + 1)) else None).flatMap { case (_r1228, _p1229) => parseSpacing(input, _p1229).map { case (_r1230, _p1231) => (new ~(_r1228, _r1230), _p1231) } }.flatMap { case (_r1224, _p1225) => parseBlockLocalVar(input, _p1225).map { case (_r1226, _p1227) => (new ~(_r1224, _r1226), _p1227) } }.flatMap { case (_r1220, _p1221) => {
  var _rs1232: List[Any] = Nil; var _cp1233: Int = _p1221; var _go1236 = true
  while (_go1236) { parseSpacing(input, _cp1233).flatMap { case (_r1245, _p1246) => (if (input.startsWith(",", _p1246)) Some((",", _p1246 + 1)) else None).map { case (_r1247, _p1248) => (new ~(_r1245, _r1247), _p1248) } }.flatMap { case (_r1241, _p1242) => parseSpacing(input, _p1242).map { case (_r1243, _p1244) => (new ~(_r1241, _r1243), _p1244) } }.flatMap { case (_r1237, _p1238) => parseBlockLocalVar(input, _p1238).map { case (_r1239, _p1240) => (new ~(_r1237, _r1239), _p1240) } } match {
    case Some((_st1234, _np1235)) => _rs1232 = _rs1232 :+ _st1234; _cp1233 = _np1235
    case None => _go1236 = false } }
  Some((_rs1232, _cp1233)) }.map { case (_r1222, _p1223) => (new ~(_r1220, _r1222), _p1223) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1176)))).map { case (_r1177, _p1178) => (new ~(_r1175, _r1177), _p1178) } }.flatMap { case (_r1171, _p1172) => parseSpacing(input, _p1172).map { case (_r1173, _p1174) => (new ~(_r1171, _r1173), _p1174) } }.flatMap { case (_r1167, _p1168) => (if (input.startsWith("|", _p1168)) Some(("|", _p1168 + 1)) else None).map { case (_r1169, _p1170) => (new ~(_r1167, _r1169), _p1170) } }.flatMap { case (_r1163, _p1164) => parseInlineSpacing(input, _p1164).map { case (_r1165, _p1166) => (new ~(_r1163, _r1165), _p1166) } }.map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseRescueStop(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r1253, _p1254) => (if (parseIdentCont(input, _p1254).isEmpty) Some(((), _p1254)) else None).map { case (_r1255, _p1256) => (new ~(_r1253, _r1255), _p1256) } }.flatMap { case (_r1249, _p1250) => parseSpacing(input, _p1250).map { case (_r1251, _p1252) => (new ~(_r1249, _r1251), _p1252) } }).orElse((if (input.startsWith("else", pos)) Some(("else", pos + 4)) else None).flatMap { case (_r1261, _p1262) => (if (parseIdentCont(input, _p1262).isEmpty) Some(((), _p1262)) else None).map { case (_r1263, _p1264) => (new ~(_r1261, _r1263), _p1264) } }.flatMap { case (_r1257, _p1258) => parseSpacing(input, _p1258).map { case (_r1259, _p1260) => (new ~(_r1257, _r1259), _p1260) } })).orElse((if (input.startsWith("ensure", pos)) Some(("ensure", pos + 6)) else None).flatMap { case (_r1269, _p1270) => (if (parseIdentCont(input, _p1270).isEmpty) Some(((), _p1270)) else None).map { case (_r1271, _p1272) => (new ~(_r1269, _r1271), _p1272) } }.flatMap { case (_r1265, _p1266) => parseSpacing(input, _p1266).map { case (_r1267, _p1268) => (new ~(_r1265, _r1267), _p1268) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r1277, _p1278) => (if (parseIdentCont(input, _p1278).isEmpty) Some(((), _p1278)) else None).map { case (_r1279, _p1280) => (new ~(_r1277, _r1279), _p1280) } }.flatMap { case (_r1273, _p1274) => parseSpacing(input, _p1274).map { case (_r1275, _p1276) => (new ~(_r1273, _r1275), _p1276) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseDoBlockStop(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r1285, _p1286) => (if (parseIdentCont(input, _p1286).isEmpty) Some(((), _p1286)) else None).map { case (_r1287, _p1288) => (new ~(_r1285, _r1287), _p1288) } }.flatMap { case (_r1281, _p1282) => parseSpacing(input, _p1282).map { case (_r1283, _p1284) => (new ~(_r1281, _r1283), _p1284) } }).orElse((if (input.startsWith("else", pos)) Some(("else", pos + 4)) else None).flatMap { case (_r1293, _p1294) => (if (parseIdentCont(input, _p1294).isEmpty) Some(((), _p1294)) else None).map { case (_r1295, _p1296) => (new ~(_r1293, _r1295), _p1296) } }.flatMap { case (_r1289, _p1290) => parseSpacing(input, _p1290).map { case (_r1291, _p1292) => (new ~(_r1289, _r1291), _p1292) } })).orElse((if (input.startsWith("ensure", pos)) Some(("ensure", pos + 6)) else None).flatMap { case (_r1301, _p1302) => (if (parseIdentCont(input, _p1302).isEmpty) Some(((), _p1302)) else None).map { case (_r1303, _p1304) => (new ~(_r1301, _r1303), _p1304) } }.flatMap { case (_r1297, _p1298) => parseSpacing(input, _p1298).map { case (_r1299, _p1300) => (new ~(_r1297, _r1299), _p1300) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r1309, _p1310) => (if (parseIdentCont(input, _p1310).isEmpty) Some(((), _p1310)) else None).map { case (_r1311, _p1312) => (new ~(_r1309, _r1311), _p1312) } }.flatMap { case (_r1305, _p1306) => parseSpacing(input, _p1306).map { case (_r1307, _p1308) => (new ~(_r1305, _r1307), _p1308) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseEndKeyword(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r1317, _p1318) => (if (parseIdentCont(input, _p1318).isEmpty) Some(((), _p1318)) else None).map { case (_r1319, _p1320) => (new ~(_r1317, _r1319), _p1320) } }.flatMap { case (_r1313, _p1314) => parseSpacing(input, _p1314).map { case (_r1315, _p1316) => (new ~(_r1313, _r1315), _p1316) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseRescueClause(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r1345, _p1346) => (if (parseIdentCont(input, _p1346).isEmpty) Some(((), _p1346)) else None).map { case (_r1347, _p1348) => (new ~(_r1345, _r1347), _p1348) } }.flatMap { case (_r1341, _p1342) => parseSpacing(input, _p1342).map { case (_r1343, _p1344) => (new ~(_r1341, _r1343), _p1344) } }.flatMap { case (_r1337, _p1338) => (parseExpr(input, _p1338).flatMap { case (_r1349, _p1350) => {
  var _rs1353: List[Any] = Nil; var _cp1354: Int = _p1350; var _go1357 = true
  while (_go1357) { parseSpacing(input, _cp1354).flatMap { case (_r1366, _p1367) => (if (input.startsWith(",", _p1367)) Some((",", _p1367 + 1)) else None).map { case (_r1368, _p1369) => (new ~(_r1366, _r1368), _p1369) } }.flatMap { case (_r1362, _p1363) => parseSpacing(input, _p1363).map { case (_r1364, _p1365) => (new ~(_r1362, _r1364), _p1365) } }.flatMap { case (_r1358, _p1359) => parseExpr(input, _p1359).map { case (_r1360, _p1361) => (new ~(_r1358, _r1360), _p1361) } } match {
    case Some((_st1355, _np1356)) => _rs1353 = _rs1353 :+ _st1355; _cp1354 = _np1356
    case None => _go1357 = false } }
  Some((_rs1353, _cp1354)) }.map { case (_r1351, _p1352) => (new ~(_r1349, _r1351), _p1352) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1338)))).map { case (_r1339, _p1340) => (new ~(_r1337, _r1339), _p1340) } }.flatMap { case (_r1333, _p1334) => ((if (input.startsWith("=>", _p1334)) Some(("=>", _p1334 + 2)) else None).flatMap { case (_r1378, _p1379) => parseSpacing(input, _p1379).map { case (_r1380, _p1381) => (new ~(_r1378, _r1380), _p1381) } }.flatMap { case (_r1374, _p1375) => parseVariable(input, _p1375).map { case (_r1376, _p1377) => (new ~(_r1374, _r1376), _p1377) } }.flatMap { case (_r1370, _p1371) => parseSpacing(input, _p1371).map { case (_r1372, _p1373) => (new ~(_r1370, _r1372), _p1373) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1334)))).map { case (_r1335, _p1336) => (new ~(_r1333, _r1335), _p1336) } }.flatMap { case (_r1329, _p1330) => {
  var _rs1382: List[Any] = Nil; var _cp1383: Int = _p1330; var _go1386 = true
  while (_go1386) { parseStatementSep(input, _cp1383) match {
    case Some((_st1384, _np1385)) => _rs1382 = _rs1382 :+ _st1384; _cp1383 = _np1385
    case None => _go1386 = false } }
  Some((_rs1382, _cp1383)) }.map { case (_r1331, _p1332) => (new ~(_r1329, _r1331), _p1332) } }.flatMap { case (_r1325, _p1326) => {
  var _rs1387: List[Any] = Nil; var _cp1388: Int = _p1326; var _go1391 = true
  while (_go1391) { (if (parseRescueStop(input, _cp1388).isEmpty) Some(((), _cp1388)) else None).flatMap { case (_r1396, _p1397) => parseStatement(input, _p1397).map { case (_r1398, _p1399) => (new ~(_r1396, _r1398), _p1399) } }.flatMap { case (_r1392, _p1393) => {
  var _rs1400: List[Any] = Nil; var _cp1401: Int = _p1393; var _go1404 = true
  while (_go1404) { parseStatementSep(input, _cp1401) match {
    case Some((_st1402, _np1403)) => _rs1400 = _rs1400 :+ _st1402; _cp1401 = _np1403
    case None => _go1404 = false } }
  Some((_rs1400, _cp1401)) }.map { case (_r1394, _p1395) => (new ~(_r1392, _r1394), _p1395) } } match {
    case Some((_st1389, _np1390)) => _rs1387 = _rs1387 :+ _st1389; _cp1388 = _np1390
    case None => _go1391 = false } }
  Some((_rs1387, _cp1388)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r1327, _p1328) => (new ~(_r1325, _r1327), _p1328) } }.flatMap { case (_r1321, _p1322) => {
  var _rs1405: List[Any] = Nil; var _cp1406: Int = _p1322; var _go1409 = true
  while (_go1409) { parseStatementSep(input, _cp1406) match {
    case Some((_st1407, _np1408)) => _rs1405 = _rs1405 :+ _st1407; _cp1406 = _np1408
    case None => _go1409 = false } }
  Some((_rs1405, _cp1406)) }.map { case (_r1323, _p1324) => (new ~(_r1321, _r1323), _p1324) } }.map { case (r, p) => (_applyAction({  _ => RescueClause(List.empty, None, List.empty)  }, r), p) }

  def parseLambdaLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("->", pos)) Some(("->", pos + 2)) else None).flatMap { case (_r1422, _p1423) => parseInlineSpacing(input, _p1423).map { case (_r1424, _p1425) => (new ~(_r1422, _r1424), _p1425) } }.flatMap { case (_r1418, _p1419) => ((parseParams(input, _p1419)).orElse(parseParenExpr(input, _p1419).map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1419)))).map { case (_r1420, _p1421) => (new ~(_r1418, _r1420), _p1421) } }.flatMap { case (_r1414, _p1415) => parseSpacing(input, _p1415).map { case (_r1416, _p1417) => (new ~(_r1414, _r1416), _p1417) } }.flatMap { case (_r1410, _p1411) => parseBlockLiteral(input, _p1411).map { case (_r1412, _p1413) => (new ~(_r1410, _r1412), _p1413) } }.map { case (r, p) => (_applyAction({  _ => LambdaLiteral(List.empty, List.empty)  }, r), p) }

  def parseDoBlock(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("do", pos)) Some(("do", pos + 2)) else None).flatMap { case (_r1470, _p1471) => (if (parseIdentCont(input, _p1471).isEmpty) Some(((), _p1471)) else None).map { case (_r1472, _p1473) => (new ~(_r1470, _r1472), _p1473) } }.flatMap { case (_r1466, _p1467) => parseSpacing(input, _p1467).map { case (_r1468, _p1469) => (new ~(_r1466, _r1468), _p1469) } }.flatMap { case (_r1462, _p1463) => {
  var _rs1474: List[Any] = Nil; var _cp1475: Int = _p1463; var _go1478 = true
  while (_go1478) { parseStatementSep(input, _cp1475) match {
    case Some((_st1476, _np1477)) => _rs1474 = _rs1474 :+ _st1476; _cp1475 = _np1477
    case None => _go1478 = false } }
  Some((_rs1474, _cp1475)) }.map { case (_r1464, _p1465) => (new ~(_r1462, _r1464), _p1465) } }.flatMap { case (_r1458, _p1459) => (parseBlockParams(input, _p1459).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1459)))).map { case (_r1460, _p1461) => (new ~(_r1458, _r1460), _p1461) } }.flatMap { case (_r1454, _p1455) => {
  var _rs1479: List[Any] = Nil; var _cp1480: Int = _p1455; var _go1483 = true
  while (_go1483) { parseStatementSep(input, _cp1480) match {
    case Some((_st1481, _np1482)) => _rs1479 = _rs1479 :+ _st1481; _cp1480 = _np1482
    case None => _go1483 = false } }
  Some((_rs1479, _cp1480)) }.map { case (_r1456, _p1457) => (new ~(_r1454, _r1456), _p1457) } }.flatMap { case (_r1450, _p1451) => {
  var _rs1484: List[Any] = Nil; var _cp1485: Int = _p1451; var _go1488 = true
  while (_go1488) { (if (parseDoBlockStop(input, _cp1485).isEmpty) Some(((), _cp1485)) else None).flatMap { case (_r1493, _p1494) => parseStatement(input, _p1494).map { case (_r1495, _p1496) => (new ~(_r1493, _r1495), _p1496) } }.flatMap { case (_r1489, _p1490) => {
  var _rs1497: List[Any] = Nil; var _cp1498: Int = _p1490; var _go1501 = true
  while (_go1501) { parseStatementSep(input, _cp1498) match {
    case Some((_st1499, _np1500)) => _rs1497 = _rs1497 :+ _st1499; _cp1498 = _np1500
    case None => _go1501 = false } }
  Some((_rs1497, _cp1498)) }.map { case (_r1491, _p1492) => (new ~(_r1489, _r1491), _p1492) } } match {
    case Some((_st1486, _np1487)) => _rs1484 = _rs1484 :+ _st1486; _cp1485 = _np1487
    case None => _go1488 = false } }
  Some((_rs1484, _cp1485)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r1452, _p1453) => (new ~(_r1450, _r1452), _p1453) } }.flatMap { case (_r1446, _p1447) => {
  var _rs1502: List[Any] = Nil; var _cp1503: Int = _p1447; var _go1506 = true
  while (_go1506) { parseStatementSep(input, _cp1503) match {
    case Some((_st1504, _np1505)) => _rs1502 = _rs1502 :+ _st1504; _cp1503 = _np1505
    case None => _go1506 = false } }
  Some((_rs1502, _cp1503)) }.map { case (_r1448, _p1449) => (new ~(_r1446, _r1448), _p1449) } }.flatMap { case (_r1442, _p1443) => {
  var _rs1507: List[Any] = Nil; var _cp1508: Int = _p1443; var _go1511 = true
  while (_go1511) { parseRescueClause(input, _cp1508) match {
    case Some((_st1509, _np1510)) => _rs1507 = _rs1507 :+ _st1509; _cp1508 = _np1510
    case None => _go1511 = false } }
  Some((_rs1507, _cp1508)) }.map { case (_r1444, _p1445) => (new ~(_r1442, _r1444), _p1445) } }.flatMap { case (_r1438, _p1439) => {
  var _rs1512: List[Any] = Nil; var _cp1513: Int = _p1439; var _go1516 = true
  while (_go1516) { parseStatementSep(input, _cp1513) match {
    case Some((_st1514, _np1515)) => _rs1512 = _rs1512 :+ _st1514; _cp1513 = _np1515
    case None => _go1516 = false } }
  Some((_rs1512, _cp1513)) }.map { case (_r1440, _p1441) => (new ~(_r1438, _r1440), _p1441) } }.flatMap { case (_r1434, _p1435) => ((if (input.startsWith("else", _p1435)) Some(("else", _p1435 + 4)) else None).flatMap { case (_r1533, _p1534) => (if (parseIdentCont(input, _p1534).isEmpty) Some(((), _p1534)) else None).map { case (_r1535, _p1536) => (new ~(_r1533, _r1535), _p1536) } }.flatMap { case (_r1529, _p1530) => parseSpacing(input, _p1530).map { case (_r1531, _p1532) => (new ~(_r1529, _r1531), _p1532) } }.flatMap { case (_r1525, _p1526) => {
  var _rs1537: List[Any] = Nil; var _cp1538: Int = _p1526; var _go1541 = true
  while (_go1541) { parseStatementSep(input, _cp1538) match {
    case Some((_st1539, _np1540)) => _rs1537 = _rs1537 :+ _st1539; _cp1538 = _np1540
    case None => _go1541 = false } }
  Some((_rs1537, _cp1538)) }.map { case (_r1527, _p1528) => (new ~(_r1525, _r1527), _p1528) } }.flatMap { case (_r1521, _p1522) => {
  var _rs1542: List[Any] = Nil; var _cp1543: Int = _p1522; var _go1546 = true
  while (_go1546) { (if (parseDoBlockStop(input, _cp1543).isEmpty) Some(((), _cp1543)) else None).flatMap { case (_r1551, _p1552) => parseStatement(input, _p1552).map { case (_r1553, _p1554) => (new ~(_r1551, _r1553), _p1554) } }.flatMap { case (_r1547, _p1548) => {
  var _rs1555: List[Any] = Nil; var _cp1556: Int = _p1548; var _go1559 = true
  while (_go1559) { parseStatementSep(input, _cp1556) match {
    case Some((_st1557, _np1558)) => _rs1555 = _rs1555 :+ _st1557; _cp1556 = _np1558
    case None => _go1559 = false } }
  Some((_rs1555, _cp1556)) }.map { case (_r1549, _p1550) => (new ~(_r1547, _r1549), _p1550) } } match {
    case Some((_st1544, _np1545)) => _rs1542 = _rs1542 :+ _st1544; _cp1543 = _np1545
    case None => _go1546 = false } }
  Some((_rs1542, _cp1543)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r1523, _p1524) => (new ~(_r1521, _r1523), _p1524) } }.flatMap { case (_r1517, _p1518) => {
  var _rs1560: List[Any] = Nil; var _cp1561: Int = _p1518; var _go1564 = true
  while (_go1564) { parseStatementSep(input, _cp1561) match {
    case Some((_st1562, _np1563)) => _rs1560 = _rs1560 :+ _st1562; _cp1561 = _np1563
    case None => _go1564 = false } }
  Some((_rs1560, _cp1561)) }.map { case (_r1519, _p1520) => (new ~(_r1517, _r1519), _p1520) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1435)))).map { case (_r1436, _p1437) => (new ~(_r1434, _r1436), _p1437) } }.flatMap { case (_r1430, _p1431) => ((if (input.startsWith("ensure", _p1431)) Some(("ensure", _p1431 + 6)) else None).flatMap { case (_r1581, _p1582) => (if (parseIdentCont(input, _p1582).isEmpty) Some(((), _p1582)) else None).map { case (_r1583, _p1584) => (new ~(_r1581, _r1583), _p1584) } }.flatMap { case (_r1577, _p1578) => parseSpacing(input, _p1578).map { case (_r1579, _p1580) => (new ~(_r1577, _r1579), _p1580) } }.flatMap { case (_r1573, _p1574) => {
  var _rs1585: List[Any] = Nil; var _cp1586: Int = _p1574; var _go1589 = true
  while (_go1589) { parseStatementSep(input, _cp1586) match {
    case Some((_st1587, _np1588)) => _rs1585 = _rs1585 :+ _st1587; _cp1586 = _np1588
    case None => _go1589 = false } }
  Some((_rs1585, _cp1586)) }.map { case (_r1575, _p1576) => (new ~(_r1573, _r1575), _p1576) } }.flatMap { case (_r1569, _p1570) => {
  var _rs1590: List[Any] = Nil; var _cp1591: Int = _p1570; var _go1594 = true
  while (_go1594) { (if (parseEndKeyword(input, _cp1591).isEmpty) Some(((), _cp1591)) else None).flatMap { case (_r1599, _p1600) => parseStatement(input, _p1600).map { case (_r1601, _p1602) => (new ~(_r1599, _r1601), _p1602) } }.flatMap { case (_r1595, _p1596) => {
  var _rs1603: List[Any] = Nil; var _cp1604: Int = _p1596; var _go1607 = true
  while (_go1607) { parseStatementSep(input, _cp1604) match {
    case Some((_st1605, _np1606)) => _rs1603 = _rs1603 :+ _st1605; _cp1604 = _np1606
    case None => _go1607 = false } }
  Some((_rs1603, _cp1604)) }.map { case (_r1597, _p1598) => (new ~(_r1595, _r1597), _p1598) } } match {
    case Some((_st1592, _np1593)) => _rs1590 = _rs1590 :+ _st1592; _cp1591 = _np1593
    case None => _go1594 = false } }
  Some((_rs1590, _cp1591)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r1571, _p1572) => (new ~(_r1569, _r1571), _p1572) } }.flatMap { case (_r1565, _p1566) => {
  var _rs1608: List[Any] = Nil; var _cp1609: Int = _p1566; var _go1612 = true
  while (_go1612) { parseStatementSep(input, _cp1609) match {
    case Some((_st1610, _np1611)) => _rs1608 = _rs1608 :+ _st1610; _cp1609 = _np1611
    case None => _go1612 = false } }
  Some((_rs1608, _cp1609)) }.map { case (_r1567, _p1568) => (new ~(_r1565, _r1567), _p1568) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1431)))).map { case (_r1432, _p1433) => (new ~(_r1430, _r1432), _p1433) } }.flatMap { case (_r1426, _p1427) => parseEndKeyword(input, _p1427).map { case (_r1428, _p1429) => (new ~(_r1426, _r1428), _p1429) } }.map { case (r, p) => (_applyAction({  _ => Block(List.empty, List.empty)  }, r), p) }

  def parseBraceBlockStop(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("}", pos)) Some(("}", pos + 1)) else None).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseBraceBlock(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r1645, _p1646) => parseInlineSpacing(input, _p1646).map { case (_r1647, _p1648) => (new ~(_r1645, _r1647), _p1648) } }.flatMap { case (_r1641, _p1642) => {
  var _rs1649: List[Any] = Nil; var _cp1650: Int = _p1642; var _go1653 = true
  while (_go1653) { parseStatementSep(input, _cp1650) match {
    case Some((_st1651, _np1652)) => _rs1649 = _rs1649 :+ _st1651; _cp1650 = _np1652
    case None => _go1653 = false } }
  Some((_rs1649, _cp1650)) }.map { case (_r1643, _p1644) => (new ~(_r1641, _r1643), _p1644) } }.flatMap { case (_r1637, _p1638) => (parseBlockParams(input, _p1638).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1638)))).map { case (_r1639, _p1640) => (new ~(_r1637, _r1639), _p1640) } }.flatMap { case (_r1633, _p1634) => {
  var _rs1654: List[Any] = Nil; var _cp1655: Int = _p1634; var _go1658 = true
  while (_go1658) { parseStatementSep(input, _cp1655) match {
    case Some((_st1656, _np1657)) => _rs1654 = _rs1654 :+ _st1656; _cp1655 = _np1657
    case None => _go1658 = false } }
  Some((_rs1654, _cp1655)) }.map { case (_r1635, _p1636) => (new ~(_r1633, _r1635), _p1636) } }.flatMap { case (_r1629, _p1630) => {
  var _rs1659: List[Any] = Nil; var _cp1660: Int = _p1630; var _go1663 = true
  while (_go1663) { (if (parseBraceBlockStop(input, _cp1660).isEmpty) Some(((), _cp1660)) else None).flatMap { case (_r1668, _p1669) => parseStatement(input, _p1669).map { case (_r1670, _p1671) => (new ~(_r1668, _r1670), _p1671) } }.flatMap { case (_r1664, _p1665) => {
  var _rs1672: List[Any] = Nil; var _cp1673: Int = _p1665; var _go1676 = true
  while (_go1676) { parseStatementSep(input, _cp1673) match {
    case Some((_st1674, _np1675)) => _rs1672 = _rs1672 :+ _st1674; _cp1673 = _np1675
    case None => _go1676 = false } }
  Some((_rs1672, _cp1673)) }.map { case (_r1666, _p1667) => (new ~(_r1664, _r1666), _p1667) } } match {
    case Some((_st1661, _np1662)) => _rs1659 = _rs1659 :+ _st1661; _cp1660 = _np1662
    case None => _go1663 = false } }
  Some((_rs1659, _cp1660)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r1631, _p1632) => (new ~(_r1629, _r1631), _p1632) } }.flatMap { case (_r1625, _p1626) => {
  var _rs1677: List[Any] = Nil; var _cp1678: Int = _p1626; var _go1681 = true
  while (_go1681) { parseStatementSep(input, _cp1678) match {
    case Some((_st1679, _np1680)) => _rs1677 = _rs1677 :+ _st1679; _cp1678 = _np1680
    case None => _go1681 = false } }
  Some((_rs1677, _cp1678)) }.map { case (_r1627, _p1628) => (new ~(_r1625, _r1627), _p1628) } }.flatMap { case (_r1621, _p1622) => parseSpacing(input, _p1622).map { case (_r1623, _p1624) => (new ~(_r1621, _r1623), _p1624) } }.flatMap { case (_r1617, _p1618) => (if (input.startsWith("}", _p1618)) Some(("}", _p1618 + 1)) else None).map { case (_r1619, _p1620) => (new ~(_r1617, _r1619), _p1620) } }.flatMap { case (_r1613, _p1614) => parseInlineSpacing(input, _p1614).map { case (_r1615, _p1616) => (new ~(_r1613, _r1615), _p1616) } }.map { case (r, p) => (_applyAction({  _ => Block(List.empty, List.empty)  }, r), p) }

  def parseBlockLiteral(input: String, pos: Int): Option[(Any, Int)] = (parseDoBlock(input, pos)).orElse(parseBraceBlock(input, pos))

  def parseCallArgItem(input: String, pos: Int): Option[(Any, Int)] = ((((((((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r1686, _p1687) => parseSpacing(input, _p1687).map { case (_r1688, _p1689) => (new ~(_r1686, _r1688), _p1689) } }.flatMap { case (_r1682, _p1683) => (parseSymbolLiteral(input, _p1683)).orElse(parseExpr(input, _p1683)).map { case (_r1684, _p1685) => (new ~(_r1682, _r1684), _p1685) } })).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r1694, _p1695) => parseSpacing(input, _p1695).map { case (_r1696, _p1697) => (new ~(_r1694, _r1696), _p1697) } }.flatMap { case (_r1690, _p1691) => parseExpr(input, _p1691).map { case (_r1692, _p1693) => (new ~(_r1690, _r1692), _p1693) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r1702, _p1703) => parseSpacing(input, _p1703).map { case (_r1704, _p1705) => (new ~(_r1702, _r1704), _p1705) } }.flatMap { case (_r1698, _p1699) => parseExpr(input, _p1699).map { case (_r1700, _p1701) => (new ~(_r1698, _r1700), _p1701) } })).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r1710, _p1711) => parseSpacing(input, _p1711).map { case (_r1712, _p1713) => (new ~(_r1710, _r1712), _p1713) } }.flatMap { case (_r1706, _p1707) => parseExpr(input, _p1707).map { case (_r1708, _p1709) => (new ~(_r1706, _r1708), _p1709) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r1718, _p1719) => parseSpacing(input, _p1719).map { case (_r1720, _p1721) => (new ~(_r1718, _r1720), _p1721) } }.flatMap { case (_r1714, _p1715) => parseExpr(input, _p1715).map { case (_r1716, _p1717) => (new ~(_r1714, _r1716), _p1717) } })).orElse(parseExpr(input, pos).flatMap { case (_r1734, _p1735) => parseSpacing(input, _p1735).map { case (_r1736, _p1737) => (new ~(_r1734, _r1736), _p1737) } }.flatMap { case (_r1730, _p1731) => (if (input.startsWith("=>", _p1731)) Some(("=>", _p1731 + 2)) else None).map { case (_r1732, _p1733) => (new ~(_r1730, _r1732), _p1733) } }.flatMap { case (_r1726, _p1727) => parseSpacing(input, _p1727).map { case (_r1728, _p1729) => (new ~(_r1726, _r1728), _p1729) } }.flatMap { case (_r1722, _p1723) => parseExpr(input, _p1723).map { case (_r1724, _p1725) => (new ~(_r1722, _r1724), _p1725) } })).orElse(parseExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseCallArgs(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r1750, _p1751) => parseSpacing(input, _p1751).map { case (_r1752, _p1753) => (new ~(_r1750, _r1752), _p1753) } }.flatMap { case (_r1746, _p1747) => (parseCallArgItem(input, _p1747).flatMap { case (_r1762, _p1763) => {
  var _rs1766: List[Any] = Nil; var _cp1767: Int = _p1763; var _go1770 = true
  while (_go1770) { parseSpacing(input, _cp1767).flatMap { case (_r1779, _p1780) => (if (input.startsWith(",", _p1780)) Some((",", _p1780 + 1)) else None).map { case (_r1781, _p1782) => (new ~(_r1779, _r1781), _p1782) } }.flatMap { case (_r1775, _p1776) => parseSpacing(input, _p1776).map { case (_r1777, _p1778) => (new ~(_r1775, _r1777), _p1778) } }.flatMap { case (_r1771, _p1772) => parseCallArgItem(input, _p1772).map { case (_r1773, _p1774) => (new ~(_r1771, _r1773), _p1774) } } match {
    case Some((_st1768, _np1769)) => _rs1766 = _rs1766 :+ _st1768; _cp1767 = _np1769
    case None => _go1770 = false } }
  Some((_rs1766, _cp1767)) }.map { case (_r1764, _p1765) => (new ~(_r1762, _r1764), _p1765) } }.flatMap { case (_r1758, _p1759) => (parseSpacing(input, _p1759).flatMap { case (_r1783, _p1784) => (if (input.startsWith(",", _p1784)) Some((",", _p1784 + 1)) else None).map { case (_r1785, _p1786) => (new ~(_r1783, _r1785), _p1786) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1759)))).map { case (_r1760, _p1761) => (new ~(_r1758, _r1760), _p1761) } }.flatMap { case (_r1754, _p1755) => parseSpacing(input, _p1755).map { case (_r1756, _p1757) => (new ~(_r1754, _r1756), _p1757) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1747)))).map { case (_r1748, _p1749) => (new ~(_r1746, _r1748), _p1749) } }.flatMap { case (_r1742, _p1743) => (if (input.startsWith(")", _p1743)) Some((")", _p1743 + 1)) else None).map { case (_r1744, _p1745) => (new ~(_r1742, _r1744), _p1745) } }.flatMap { case (_r1738, _p1739) => parseInlineSpacing(input, _p1739).map { case (_r1740, _p1741) => (new ~(_r1738, _r1740), _p1741) } }.map { case (r, p) => (_applyAction({  _ => List.empty[Expr]  }, r), p) }

  def parseCommandArgStop(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r1787, _p1788) => (if (parseIdentCont(input, _p1788).isEmpty) Some(((), _p1788)) else None).map { case (_r1789, _p1790) => (new ~(_r1787, _r1789), _p1790) } }).orElse((if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r1791, _p1792) => (if (parseIdentCont(input, _p1792).isEmpty) Some(((), _p1792)) else None).map { case (_r1793, _p1794) => (new ~(_r1791, _r1793), _p1794) } })).orElse((if (input.startsWith("while", pos)) Some(("while", pos + 5)) else None).flatMap { case (_r1795, _p1796) => (if (parseIdentCont(input, _p1796).isEmpty) Some(((), _p1796)) else None).map { case (_r1797, _p1798) => (new ~(_r1795, _r1797), _p1798) } })).orElse((if (input.startsWith("until", pos)) Some(("until", pos + 5)) else None).flatMap { case (_r1799, _p1800) => (if (parseIdentCont(input, _p1800).isEmpty) Some(((), _p1800)) else None).map { case (_r1801, _p1802) => (new ~(_r1799, _r1801), _p1802) } })).orElse((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r1803, _p1804) => (if (parseIdentCont(input, _p1804).isEmpty) Some(((), _p1804)) else None).map { case (_r1805, _p1806) => (new ~(_r1803, _r1805), _p1806) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseCommandArgItem(input: String, pos: Int): Option[(Any, Int)] = (((((((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r1811, _p1812) => parseSpacing(input, _p1812).map { case (_r1813, _p1814) => (new ~(_r1811, _r1813), _p1814) } }.flatMap { case (_r1807, _p1808) => (parseSymbolLiteral(input, _p1808)).orElse(parseExpr(input, _p1808)).map { case (_r1809, _p1810) => (new ~(_r1807, _r1809), _p1810) } }).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r1819, _p1820) => parseSpacing(input, _p1820).map { case (_r1821, _p1822) => (new ~(_r1819, _r1821), _p1822) } }.flatMap { case (_r1815, _p1816) => parseExpr(input, _p1816).map { case (_r1817, _p1818) => (new ~(_r1815, _r1817), _p1818) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r1827, _p1828) => parseSpacing(input, _p1828).map { case (_r1829, _p1830) => (new ~(_r1827, _r1829), _p1830) } }.flatMap { case (_r1823, _p1824) => parseExpr(input, _p1824).map { case (_r1825, _p1826) => (new ~(_r1823, _r1825), _p1826) } })).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r1835, _p1836) => parseSpacing(input, _p1836).map { case (_r1837, _p1838) => (new ~(_r1835, _r1837), _p1838) } }.flatMap { case (_r1831, _p1832) => parseExpr(input, _p1832).map { case (_r1833, _p1834) => (new ~(_r1831, _r1833), _p1834) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r1843, _p1844) => parseSpacing(input, _p1844).map { case (_r1845, _p1846) => (new ~(_r1843, _r1845), _p1846) } }.flatMap { case (_r1839, _p1840) => parseExpr(input, _p1840).map { case (_r1841, _p1842) => (new ~(_r1839, _r1841), _p1842) } })).orElse(parseExpr(input, pos).flatMap { case (_r1859, _p1860) => parseSpacing(input, _p1860).map { case (_r1861, _p1862) => (new ~(_r1859, _r1861), _p1862) } }.flatMap { case (_r1855, _p1856) => (if (input.startsWith("=>", _p1856)) Some(("=>", _p1856 + 2)) else None).map { case (_r1857, _p1858) => (new ~(_r1855, _r1857), _p1858) } }.flatMap { case (_r1851, _p1852) => parseSpacing(input, _p1852).map { case (_r1853, _p1854) => (new ~(_r1851, _r1853), _p1854) } }.flatMap { case (_r1847, _p1848) => parseExpr(input, _p1848).map { case (_r1849, _p1850) => (new ~(_r1847, _r1849), _p1850) } })).orElse(parseExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseCommandArgs(input: String, pos: Int): Option[(Any, Int)] = (if (parseCommandArgStop(input, pos).isEmpty) Some(((), pos)) else None).flatMap { case (_r1867, _p1868) => parseCommandArgItem(input, _p1868).map { case (_r1869, _p1870) => (new ~(_r1867, _r1869), _p1870) } }.flatMap { case (_r1863, _p1864) => {
  var _rs1871: List[Any] = Nil; var _cp1872: Int = _p1864; var _go1875 = true
  while (_go1875) { parseInlineSpacing(input, _cp1872).flatMap { case (_r1884, _p1885) => (if (input.startsWith(",", _p1885)) Some((",", _p1885 + 1)) else None).map { case (_r1886, _p1887) => (new ~(_r1884, _r1886), _p1887) } }.flatMap { case (_r1880, _p1881) => parseSpacing(input, _p1881).map { case (_r1882, _p1883) => (new ~(_r1880, _r1882), _p1883) } }.flatMap { case (_r1876, _p1877) => parseCommandArgItem(input, _p1877).map { case (_r1878, _p1879) => (new ~(_r1876, _r1878), _p1879) } } match {
    case Some((_st1873, _np1874)) => _rs1871 = _rs1871 :+ _st1873; _cp1872 = _np1874
    case None => _go1875 = false } }
  Some((_rs1871, _cp1872)) }.map { case (_r1865, _p1866) => (new ~(_r1863, _r1865), _p1866) } }.map { case (r, p) => (_applyAction({  _ => List.empty[Expr]  }, r), p) }

  def parseSubscriptArgs(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("[", pos)) Some(("[", pos + 1)) else None).flatMap { case (_r1900, _p1901) => parseSpacing(input, _p1901).map { case (_r1902, _p1903) => (new ~(_r1900, _r1902), _p1903) } }.flatMap { case (_r1896, _p1897) => (parseCallArgItem(input, _p1897).flatMap { case (_r1912, _p1913) => {
  var _rs1916: List[Any] = Nil; var _cp1917: Int = _p1913; var _go1920 = true
  while (_go1920) { parseSpacing(input, _cp1917).flatMap { case (_r1929, _p1930) => (if (input.startsWith(",", _p1930)) Some((",", _p1930 + 1)) else None).map { case (_r1931, _p1932) => (new ~(_r1929, _r1931), _p1932) } }.flatMap { case (_r1925, _p1926) => parseSpacing(input, _p1926).map { case (_r1927, _p1928) => (new ~(_r1925, _r1927), _p1928) } }.flatMap { case (_r1921, _p1922) => parseCallArgItem(input, _p1922).map { case (_r1923, _p1924) => (new ~(_r1921, _r1923), _p1924) } } match {
    case Some((_st1918, _np1919)) => _rs1916 = _rs1916 :+ _st1918; _cp1917 = _np1919
    case None => _go1920 = false } }
  Some((_rs1916, _cp1917)) }.map { case (_r1914, _p1915) => (new ~(_r1912, _r1914), _p1915) } }.flatMap { case (_r1908, _p1909) => (parseSpacing(input, _p1909).flatMap { case (_r1933, _p1934) => (if (input.startsWith(",", _p1934)) Some((",", _p1934 + 1)) else None).map { case (_r1935, _p1936) => (new ~(_r1933, _r1935), _p1936) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1909)))).map { case (_r1910, _p1911) => (new ~(_r1908, _r1910), _p1911) } }.flatMap { case (_r1904, _p1905) => parseSpacing(input, _p1905).map { case (_r1906, _p1907) => (new ~(_r1904, _r1906), _p1907) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1897)))).map { case (_r1898, _p1899) => (new ~(_r1896, _r1898), _p1899) } }.flatMap { case (_r1892, _p1893) => (if (input.startsWith("]", _p1893)) Some(("]", _p1893 + 1)) else None).map { case (_r1894, _p1895) => (new ~(_r1892, _r1894), _p1895) } }.flatMap { case (_r1888, _p1889) => parseInlineSpacing(input, _p1889).map { case (_r1890, _p1891) => (new ~(_r1888, _r1890), _p1891) } }.map { case (r, p) => (_applyAction({  _ => List.empty[Expr]  }, r), p) }

  def parseDotSep(input: String, pos: Int): Option[(Any, Int)] = (parseInlineSpacing(input, pos).flatMap { case (_r1941, _p1942) => (((if (input.startsWith("&.", _p1942)) Some(("&.", _p1942 + 2)) else None)).orElse((if (input.startsWith("::", _p1942)) Some(("::", _p1942 + 2)) else None))).orElse((if (input.startsWith(".", _p1942)) Some((".", _p1942 + 1)) else None)).map { case (_r1943, _p1944) => (new ~(_r1941, _r1943), _p1944) } }.flatMap { case (_r1937, _p1938) => parseInlineSpacing(input, _p1938).map { case (_r1939, _p1940) => (new ~(_r1937, _r1939), _p1940) } }).orElse(parseInlineSpacing(input, pos).flatMap { case (_r1957, _p1958) => (if (input.startsWith("\n", _p1958)) Some(("\n", _p1958 + 1)) else None).map { case (_r1959, _p1960) => (new ~(_r1957, _r1959), _p1960) } }.flatMap { case (_r1953, _p1954) => parseInlineSpacing(input, _p1954).map { case (_r1955, _p1956) => (new ~(_r1953, _r1955), _p1956) } }.flatMap { case (_r1949, _p1950) => (((if (input.startsWith("&.", _p1950)) Some(("&.", _p1950 + 2)) else None)).orElse((if (input.startsWith("::", _p1950)) Some(("::", _p1950 + 2)) else None))).orElse((if (input.startsWith(".", _p1950)) Some((".", _p1950 + 1)) else None)).map { case (_r1951, _p1952) => (new ~(_r1949, _r1951), _p1952) } }.flatMap { case (_r1945, _p1946) => parseInlineSpacing(input, _p1946).map { case (_r1947, _p1948) => (new ~(_r1945, _r1947), _p1948) } }).map { case (r, p) => (_applyAction({  _ => "."  }, r), p) }

  def parsePrimaryExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(30, pos) {
    ((((((((((((((((((((((((((((((parseLambdaLiteral(input, pos)).orElse(parseSingletonClassExpr(input, pos))).orElse(parseBeginExpr(input, pos))).orElse(parseDefExprForm(input, pos))).orElse(parseReturnExpr(input, pos))).orElse(parseYieldExpr(input, pos))).orElse(parseIfExpr(input, pos))).orElse(parseUnlessExpr(input, pos))).orElse(parseCaseExpr(input, pos))).orElse(parseSelfExpr(input, pos))).orElse(parseBoolLiteral(input, pos))).orElse(parseNilLiteral(input, pos))).orElse(parseConstRef(input, pos))).orElse(parseVariable(input, pos))).orElse(parseFloatLiteral(input, pos))).orElse(parseIntegerLiteral(input, pos))).orElse(parseCharLiteral(input, pos))).orElse(parseStringLiteral(input, pos))).orElse(parseSingleQuotedStringLiteral(input, pos))).orElse(parseBacktickLiteral(input, pos))).orElse(parsePercentCommand(input, pos))).orElse(parsePercentQuotedStringLiteral(input, pos))).orElse(parsePercentSymbolLiteralExpr(input, pos))).orElse(parsePercentWordArray(input, pos))).orElse(parsePercentSymbolArray(input, pos))).orElse(parseRegexLiteral(input, pos))).orElse(parseSymbolLiteral(input, pos))).orElse(parseArrayLiteral(input, pos))).orElse(parseHeredocLiteral(input, pos))).orElse(parseHashLiteral(input, pos))).orElse(parseParenExpr(input, pos))
  }

  def parseMethodName(input: String, pos: Int): Option[(Any, Int)] = (((parseSymbolOperatorName(input, pos)).orElse(parseConstNameNoSpace(input, pos))).orElse(parseMethodIdentifierRaw(input, pos))).orElse(((((((((((if (input.startsWith("private", pos)) Some(("private", pos + 7)) else None).flatMap { case (_r1965, _p1966) => (if (parseIdentCont(input, _p1966).isEmpty) Some(((), _p1966)) else None).map { case (_r1967, _p1968) => (new ~(_r1965, _r1967), _p1968) } }).orElse((if (input.startsWith("public", pos)) Some(("public", pos + 6)) else None).flatMap { case (_r1969, _p1970) => (if (parseIdentCont(input, _p1970).isEmpty) Some(((), _p1970)) else None).map { case (_r1971, _p1972) => (new ~(_r1969, _r1971), _p1972) } })).orElse((if (input.startsWith("protected", pos)) Some(("protected", pos + 9)) else None).flatMap { case (_r1973, _p1974) => (if (parseIdentCont(input, _p1974).isEmpty) Some(((), _p1974)) else None).map { case (_r1975, _p1976) => (new ~(_r1973, _r1975), _p1976) } })).orElse((if (input.startsWith("ruby2_keywords", pos)) Some(("ruby2_keywords", pos + 14)) else None).flatMap { case (_r1977, _p1978) => (if (parseIdentCont(input, _p1978).isEmpty) Some(((), _p1978)) else None).map { case (_r1979, _p1980) => (new ~(_r1977, _r1979), _p1980) } })).orElse((if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r1981, _p1982) => (if (parseIdentCont(input, _p1982).isEmpty) Some(((), _p1982)) else None).map { case (_r1983, _p1984) => (new ~(_r1981, _r1983), _p1984) } })).orElse((if (input.startsWith("def", pos)) Some(("def", pos + 3)) else None).flatMap { case (_r1985, _p1986) => (if (parseIdentCont(input, _p1986).isEmpty) Some(((), _p1986)) else None).map { case (_r1987, _p1988) => (new ~(_r1985, _r1987), _p1988) } })).orElse((if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r1989, _p1990) => (if (parseIdentCont(input, _p1990).isEmpty) Some(((), _p1990)) else None).map { case (_r1991, _p1992) => (new ~(_r1989, _r1991), _p1992) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r1993, _p1994) => (if (parseIdentCont(input, _p1994).isEmpty) Some(((), _p1994)) else None).map { case (_r1995, _p1996) => (new ~(_r1993, _r1995), _p1996) } })).orElse((if (input.startsWith("for", pos)) Some(("for", pos + 3)) else None).flatMap { case (_r1997, _p1998) => (if (parseIdentCont(input, _p1998).isEmpty) Some(((), _p1998)) else None).map { case (_r1999, _p2000) => (new ~(_r1997, _r1999), _p2000) } })).orElse((if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r2001, _p2002) => (if (parseIdentCont(input, _p2002).isEmpty) Some(((), _p2002)) else None).map { case (_r2003, _p2004) => (new ~(_r2001, _r2003), _p2004) } })).flatMap { case (_r1961, _p1962) => parseInlineSpacing(input, _p1962).map { case (_r1963, _p1964) => (new ~(_r1961, _r1963), _p1964) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseCallSuffix(input: String, pos: Int): Option[(Any, Int)] = (((parseDotSep(input, pos).flatMap { case (_r2009, _p2010) => parseMethodName(input, _p2010).map { case (_r2011, _p2012) => (new ~(_r2009, _r2011), _p2012) } }.flatMap { case (_r2005, _p2006) => (parseCallArgs(input, _p2006).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2006)))).map { case (_r2007, _p2008) => (new ~(_r2005, _r2007), _p2008) } }).orElse(parseDotSep(input, pos).flatMap { case (_r2013, _p2014) => parseCallArgs(input, _p2014).map { case (_r2015, _p2016) => (new ~(_r2013, _r2015), _p2016) } })).orElse(parseSubscriptArgs(input, pos))).orElse(parseInlineSpacing(input, pos).flatMap { case (_r2017, _p2018) => parseBlockLiteral(input, _p2018).map { case (_r2019, _p2020) => (new ~(_r2017, _r2019), _p2020) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseFunctionCallExpr(input: String, pos: Int): Option[(Any, Int)] = parseMethodIdentifierRaw(input, pos).flatMap { case (_r2025, _p2026) => parseInlineSpacing(input, _p2026).map { case (_r2027, _p2028) => (new ~(_r2025, _r2027), _p2028) } }.flatMap { case (_r2021, _p2022) => parseCallArgs(input, _p2022).map { case (_r2023, _p2024) => (new ~(_r2021, _r2023), _p2024) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseCommandCallExpr(input: String, pos: Int): Option[(Any, Int)] = parseMethodIdentifierRaw(input, pos).flatMap { case (_r2033, _p2034) => parseSpacing1(input, _p2034).map { case (_r2035, _p2036) => (new ~(_r2033, _r2035), _p2036) } }.flatMap { case (_r2029, _p2030) => parseCommandArgs(input, _p2030).map { case (_r2031, _p2032) => (new ~(_r2029, _r2031), _p2032) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parsePostfixExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(31, pos) {
    ((parseFunctionCallExpr(input, pos)).orElse(parseCommandCallExpr(input, pos))).orElse(parsePrimaryExpr(input, pos)).flatMap { case (_r2037, _p2038) => {
  var _rs2041: List[Any] = Nil; var _cp2042: Int = _p2038; var _go2045 = true
  while (_go2045) { parseCallSuffix(input, _cp2042) match {
    case Some((_st2043, _np2044)) => _rs2041 = _rs2041 :+ _st2043; _cp2042 = _np2044
    case None => _go2045 = false } }
  Some((_rs2041, _cp2042)) }.map { case (_r2039, _p2040) => (new ~(_r2037, _r2039), _p2040) } }.map { case (r, p) => (_applyAction({  case base ~ _ => base.asInstanceOf[Expr]  }, r), p) }
  }

  def parsePowerExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(32, pos) {
    parsePostfixExpr(input, pos).flatMap { case (_r2046, _p2047) => ((if (input.startsWith("**", _p2047)) Some(("**", _p2047 + 2)) else None).flatMap { case (_r2054, _p2055) => parseSpacing(input, _p2055).map { case (_r2056, _p2057) => (new ~(_r2054, _r2056), _p2057) } }.flatMap { case (_r2050, _p2051) => parsePowerExpr(input, _p2051).map { case (_r2052, _p2053) => (new ~(_r2050, _r2052), _p2053) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2047)))).map { case (_r2048, _p2049) => (new ~(_r2046, _r2048), _p2049) } }.map { case (r, p) => (_applyAction({  case base ~ opt => opt.asInstanceOf[Option[Any]] match { case None => base.asInstanceOf[Expr]; case Some(_ ~ rhs) => BinaryOp(base.asInstanceOf[Expr], "**", rhs.asInstanceOf[Expr]) }  }, r), p) }
  }

  def parseUnaryOpSym(input: String, pos: Int): Option[(Any, Int)] = ((((((if (input.startsWith("not", pos)) Some(("not", pos + 3)) else None).flatMap { case (_r2062, _p2063) => (if (parseIdentCont(input, _p2063).isEmpty) Some(((), _p2063)) else None).map { case (_r2064, _p2065) => (new ~(_r2062, _r2064), _p2065) } }.flatMap { case (_r2058, _p2059) => parseSpacing(input, _p2059).map { case (_r2060, _p2061) => (new ~(_r2058, _r2060), _p2061) } }).orElse((if (input.startsWith("!", pos)) Some(("!", pos + 1)) else None))).orElse((if (input.startsWith("~", pos)) Some(("~", pos + 1)) else None))).orElse((if (input.startsWith("-", pos)) Some(("-", pos + 1)) else None))).orElse((if (input.startsWith("+", pos)) Some(("+", pos + 1)) else None))).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None)).map { case (r, p) => (_applyAction({  v => v match { case (op ~ _) ~ _ => op.asInstanceOf[String]; case op => op.asInstanceOf[String] }  }, r), p) }

  def parseUnaryExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(33, pos) {
    (parseUnaryOpSym(input, pos).flatMap { case (_r2070, _p2071) => parseSpacing(input, _p2071).map { case (_r2072, _p2073) => (new ~(_r2070, _r2072), _p2073) } }.flatMap { case (_r2066, _p2067) => parseUnaryExpr(input, _p2067).map { case (_r2068, _p2069) => (new ~(_r2066, _r2068), _p2069) } }).orElse(parsePowerExpr(input, pos)).map { case (r, p) => (_applyAction({  v => v match { case (op ~ _) ~ e => UnaryOp(op.asInstanceOf[String], e.asInstanceOf[Expr]); case e => e.asInstanceOf[Expr] }  }, r), p) }
  }

  def parseMulDivExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(34, pos) {
    parseUnaryExpr(input, pos).flatMap { case (_r2074, _p2075) => {
  var _rs2078: List[Any] = Nil; var _cp2079: Int = _p2075; var _go2082 = true
  while (_go2082) { (((if (input.startsWith("*", _cp2079)) Some(("*", _cp2079 + 1)) else None).flatMap { case (_r2091, _p2092) => (if ((if (input.startsWith("=", _p2092)) Some(("=", _p2092 + 1)) else None).isEmpty) Some(((), _p2092)) else None).map { case (_r2093, _p2094) => (new ~(_r2091, _r2093), _p2094) } }).orElse((if (input.startsWith("/", _cp2079)) Some(("/", _cp2079 + 1)) else None).flatMap { case (_r2095, _p2096) => (if (((if (input.startsWith("=", _p2096)) Some(("=", _p2096 + 1)) else None)).orElse((if (input.startsWith("}", _p2096)) Some(("}", _p2096 + 1)) else None)).isEmpty) Some(((), _p2096)) else None).map { case (_r2097, _p2098) => (new ~(_r2095, _r2097), _p2098) } })).orElse((if (input.startsWith("%", _cp2079)) Some(("%", _cp2079 + 1)) else None).flatMap { case (_r2099, _p2100) => (if ((if (input.startsWith("=", _p2100)) Some(("=", _p2100 + 1)) else None).isEmpty) Some(((), _p2100)) else None).map { case (_r2101, _p2102) => (new ~(_r2099, _r2101), _p2102) } }).flatMap { case (_r2087, _p2088) => parseSpacing(input, _p2088).map { case (_r2089, _p2090) => (new ~(_r2087, _r2089), _p2090) } }.flatMap { case (_r2083, _p2084) => parseUnaryExpr(input, _p2084).map { case (_r2085, _p2086) => (new ~(_r2083, _r2085), _p2086) } } match {
    case Some((_st2080, _np2081)) => _rs2078 = _rs2078 :+ _st2080; _cp2079 = _np2081
    case None => _go2082 = false } }
  Some((_rs2078, _cp2079)) }.map { case (_r2076, _p2077) => (new ~(_r2074, _r2076), _p2077) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val ((op ~ _) ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseAddSubExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(35, pos) {
    parseMulDivExpr(input, pos).flatMap { case (_r2103, _p2104) => {
  var _rs2107: List[Any] = Nil; var _cp2108: Int = _p2104; var _go2111 = true
  while (_go2111) { ((if (input.startsWith("+", _cp2108)) Some(("+", _cp2108 + 1)) else None).flatMap { case (_r2120, _p2121) => (if (((if (input.startsWith("=", _p2121)) Some(("=", _p2121 + 1)) else None)).orElse((if (input.startsWith("@", _p2121)) Some(("@", _p2121 + 1)) else None)).isEmpty) Some(((), _p2121)) else None).map { case (_r2122, _p2123) => (new ~(_r2120, _r2122), _p2123) } }).orElse((if (input.startsWith("-", _cp2108)) Some(("-", _cp2108 + 1)) else None).flatMap { case (_r2124, _p2125) => (if ((((if (input.startsWith("=", _p2125)) Some(("=", _p2125 + 1)) else None)).orElse((if (input.startsWith("@", _p2125)) Some(("@", _p2125 + 1)) else None))).orElse((if (input.startsWith(">", _p2125)) Some((">", _p2125 + 1)) else None)).isEmpty) Some(((), _p2125)) else None).map { case (_r2126, _p2127) => (new ~(_r2124, _r2126), _p2127) } }).flatMap { case (_r2116, _p2117) => parseSpacing(input, _p2117).map { case (_r2118, _p2119) => (new ~(_r2116, _r2118), _p2119) } }.flatMap { case (_r2112, _p2113) => parseMulDivExpr(input, _p2113).map { case (_r2114, _p2115) => (new ~(_r2112, _r2114), _p2115) } } match {
    case Some((_st2109, _np2110)) => _rs2107 = _rs2107 :+ _st2109; _cp2108 = _np2110
    case None => _go2111 = false } }
  Some((_rs2107, _cp2108)) }.map { case (_r2105, _p2106) => (new ~(_r2103, _r2105), _p2106) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val ((op ~ _) ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseShiftExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(36, pos) {
    parseAddSubExpr(input, pos).flatMap { case (_r2128, _p2129) => {
  var _rs2132: List[Any] = Nil; var _cp2133: Int = _p2129; var _go2136 = true
  while (_go2136) { ((if (input.startsWith("<<", _cp2133)) Some(("<<", _cp2133 + 2)) else None)).orElse((if (input.startsWith(">>", _cp2133)) Some((">>", _cp2133 + 2)) else None)).flatMap { case (_r2145, _p2146) => (if ((if (input.startsWith("=", _p2146)) Some(("=", _p2146 + 1)) else None).isEmpty) Some(((), _p2146)) else None).map { case (_r2147, _p2148) => (new ~(_r2145, _r2147), _p2148) } }.flatMap { case (_r2141, _p2142) => parseSpacing(input, _p2142).map { case (_r2143, _p2144) => (new ~(_r2141, _r2143), _p2144) } }.flatMap { case (_r2137, _p2138) => parseAddSubExpr(input, _p2138).map { case (_r2139, _p2140) => (new ~(_r2137, _r2139), _p2140) } } match {
    case Some((_st2134, _np2135)) => _rs2132 = _rs2132 :+ _st2134; _cp2133 = _np2135
    case None => _go2136 = false } }
  Some((_rs2132, _cp2133)) }.map { case (_r2130, _p2131) => (new ~(_r2128, _r2130), _p2131) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val ((op ~ _) ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseBitAndExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(37, pos) {
    parseShiftExpr(input, pos).flatMap { case (_r2149, _p2150) => {
  var _rs2153: List[Any] = Nil; var _cp2154: Int = _p2150; var _go2157 = true
  while (_go2157) { (if (input.startsWith("&", _cp2154)) Some(("&", _cp2154 + 1)) else None).flatMap { case (_r2166, _p2167) => (if (((if (input.startsWith("&", _p2167)) Some(("&", _p2167 + 1)) else None)).orElse((if (input.startsWith("=", _p2167)) Some(("=", _p2167 + 1)) else None)).isEmpty) Some(((), _p2167)) else None).map { case (_r2168, _p2169) => (new ~(_r2166, _r2168), _p2169) } }.flatMap { case (_r2162, _p2163) => parseSpacing(input, _p2163).map { case (_r2164, _p2165) => (new ~(_r2162, _r2164), _p2165) } }.flatMap { case (_r2158, _p2159) => parseShiftExpr(input, _p2159).map { case (_r2160, _p2161) => (new ~(_r2158, _r2160), _p2161) } } match {
    case Some((_st2155, _np2156)) => _rs2153 = _rs2153 :+ _st2155; _cp2154 = _np2156
    case None => _go2157 = false } }
  Some((_rs2153, _cp2154)) }.map { case (_r2151, _p2152) => (new ~(_r2149, _r2151), _p2152) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val ((op ~ _) ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseBitOrExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(38, pos) {
    parseBitAndExpr(input, pos).flatMap { case (_r2170, _p2171) => {
  var _rs2174: List[Any] = Nil; var _cp2175: Int = _p2171; var _go2178 = true
  while (_go2178) { ((if (input.startsWith("|", _cp2175)) Some(("|", _cp2175 + 1)) else None).flatMap { case (_r2187, _p2188) => (if (((if (input.startsWith("=", _p2188)) Some(("=", _p2188 + 1)) else None)).orElse((if (input.startsWith("|", _p2188)) Some(("|", _p2188 + 1)) else None)).isEmpty) Some(((), _p2188)) else None).map { case (_r2189, _p2190) => (new ~(_r2187, _r2189), _p2190) } }).orElse((if (input.startsWith("^", _cp2175)) Some(("^", _cp2175 + 1)) else None).flatMap { case (_r2191, _p2192) => (if ((if (input.startsWith("=", _p2192)) Some(("=", _p2192 + 1)) else None).isEmpty) Some(((), _p2192)) else None).map { case (_r2193, _p2194) => (new ~(_r2191, _r2193), _p2194) } }).flatMap { case (_r2183, _p2184) => parseSpacing(input, _p2184).map { case (_r2185, _p2186) => (new ~(_r2183, _r2185), _p2186) } }.flatMap { case (_r2179, _p2180) => parseBitAndExpr(input, _p2180).map { case (_r2181, _p2182) => (new ~(_r2179, _r2181), _p2182) } } match {
    case Some((_st2176, _np2177)) => _rs2174 = _rs2174 :+ _st2176; _cp2175 = _np2177
    case None => _go2178 = false } }
  Some((_rs2174, _cp2175)) }.map { case (_r2172, _p2173) => (new ~(_r2170, _r2172), _p2173) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val ((op ~ _) ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseRelationalExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(39, pos) {
    parseBitOrExpr(input, pos).flatMap { case (_r2195, _p2196) => {
  var _rs2199: List[Any] = Nil; var _cp2200: Int = _p2196; var _go2203 = true
  while (_go2203) { ((((if (input.startsWith("<=", _cp2200)) Some(("<=", _cp2200 + 2)) else None)).orElse((if (input.startsWith(">=", _cp2200)) Some((">=", _cp2200 + 2)) else None))).orElse((if (input.startsWith("<", _cp2200)) Some(("<", _cp2200 + 1)) else None).flatMap { case (_r2212, _p2213) => (if (((if (input.startsWith("<", _p2213)) Some(("<", _p2213 + 1)) else None)).orElse((if (input.startsWith("=", _p2213)) Some(("=", _p2213 + 1)) else None)).isEmpty) Some(((), _p2213)) else None).map { case (_r2214, _p2215) => (new ~(_r2212, _r2214), _p2215) } })).orElse((if (input.startsWith(">", _cp2200)) Some((">", _cp2200 + 1)) else None).flatMap { case (_r2216, _p2217) => (if (((if (input.startsWith(">", _p2217)) Some((">", _p2217 + 1)) else None)).orElse((if (input.startsWith("=", _p2217)) Some(("=", _p2217 + 1)) else None)).isEmpty) Some(((), _p2217)) else None).map { case (_r2218, _p2219) => (new ~(_r2216, _r2218), _p2219) } }).flatMap { case (_r2208, _p2209) => parseSpacing(input, _p2209).map { case (_r2210, _p2211) => (new ~(_r2208, _r2210), _p2211) } }.flatMap { case (_r2204, _p2205) => parseBitOrExpr(input, _p2205).map { case (_r2206, _p2207) => (new ~(_r2204, _r2206), _p2207) } } match {
    case Some((_st2201, _np2202)) => _rs2199 = _rs2199 :+ _st2201; _cp2200 = _np2202
    case None => _go2203 = false } }
  Some((_rs2199, _cp2200)) }.map { case (_r2197, _p2198) => (new ~(_r2195, _r2197), _p2198) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => item match { case ((op ~ _) ~ _) ~ rhs => BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]); case (op ~ _) ~ rhs => BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseEqualityExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(40, pos) {
    parseRelationalExpr(input, pos).flatMap { case (_r2220, _p2221) => {
  var _rs2224: List[Any] = Nil; var _cp2225: Int = _p2221; var _go2228 = true
  while (_go2228) { ((((((if (input.startsWith("===", _cp2225)) Some(("===", _cp2225 + 3)) else None)).orElse((if (input.startsWith("<=>", _cp2225)) Some(("<=>", _cp2225 + 3)) else None))).orElse((if (input.startsWith("=~", _cp2225)) Some(("=~", _cp2225 + 2)) else None))).orElse((if (input.startsWith("!~", _cp2225)) Some(("!~", _cp2225 + 2)) else None))).orElse((if (input.startsWith("!=", _cp2225)) Some(("!=", _cp2225 + 2)) else None))).orElse((if (input.startsWith("==", _cp2225)) Some(("==", _cp2225 + 2)) else None)).flatMap { case (_r2233, _p2234) => parseSpacing(input, _p2234).map { case (_r2235, _p2236) => (new ~(_r2233, _r2235), _p2236) } }.flatMap { case (_r2229, _p2230) => parseRelationalExpr(input, _p2230).map { case (_r2231, _p2232) => (new ~(_r2229, _r2231), _p2232) } } match {
    case Some((_st2226, _np2227)) => _rs2224 = _rs2224 :+ _st2226; _cp2225 = _np2227
    case None => _go2228 = false } }
  Some((_rs2224, _cp2225)) }.map { case (_r2222, _p2223) => (new ~(_r2220, _r2222), _p2223) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val (op ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseRangeOp(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("..", pos)) Some(("..", pos + 2)) else None)).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseRangeExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(41, pos) {
    (parseRangeOp(input, pos).flatMap { case (_r2241, _p2242) => parseSpacing(input, _p2242).map { case (_r2243, _p2244) => (new ~(_r2241, _r2243), _p2244) } }.flatMap { case (_r2237, _p2238) => parseEqualityExpr(input, _p2238).map { case (_r2239, _p2240) => (new ~(_r2237, _r2239), _p2240) } }).orElse(parseEqualityExpr(input, pos).flatMap { case (_r2245, _p2246) => (parseRangeOp(input, _p2246).flatMap { case (_r2253, _p2254) => parseSpacing(input, _p2254).map { case (_r2255, _p2256) => (new ~(_r2253, _r2255), _p2256) } }.flatMap { case (_r2249, _p2250) => (parseEqualityExpr(input, _p2250).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2250)))).map { case (_r2251, _p2252) => (new ~(_r2249, _r2251), _p2252) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2246)))).map { case (_r2247, _p2248) => (new ~(_r2245, _r2247), _p2248) } }).map { case (r, p) => (_applyAction({  v => v match { case (op ~ _) ~ rhs => RangeExpr(NilLiteral(), rhs.asInstanceOf[Expr], op == "..."); case base ~ None => base.asInstanceOf[Expr]; case base ~ Some(v2) => { val (op ~ _) ~ endOpt = v2; RangeExpr(base.asInstanceOf[Expr], endOpt.asInstanceOf[Option[Any]].map(_.asInstanceOf[Expr]).getOrElse(NilLiteral()), op == "...") } }  }, r), p) }
  }

  def parseAndExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(42, pos) {
    parseRangeExpr(input, pos).flatMap { case (_r2257, _p2258) => {
  var _rs2261: List[Any] = Nil; var _cp2262: Int = _p2258; var _go2265 = true
  while (_go2265) { (if (input.startsWith("&&", _cp2262)) Some(("&&", _cp2262 + 2)) else None).flatMap { case (_r2274, _p2275) => (if ((if (input.startsWith("=", _p2275)) Some(("=", _p2275 + 1)) else None).isEmpty) Some(((), _p2275)) else None).map { case (_r2276, _p2277) => (new ~(_r2274, _r2276), _p2277) } }.flatMap { case (_r2270, _p2271) => parseSpacing(input, _p2271).map { case (_r2272, _p2273) => (new ~(_r2270, _r2272), _p2273) } }.flatMap { case (_r2266, _p2267) => parseRangeExpr(input, _p2267).map { case (_r2268, _p2269) => (new ~(_r2266, _r2268), _p2269) } } match {
    case Some((_st2263, _np2264)) => _rs2261 = _rs2261 :+ _st2263; _cp2262 = _np2264
    case None => _go2265 = false } }
  Some((_rs2261, _cp2262)) }.map { case (_r2259, _p2260) => (new ~(_r2257, _r2259), _p2260) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val ((op ~ _) ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseOrExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(43, pos) {
    parseAndExpr(input, pos).flatMap { case (_r2278, _p2279) => {
  var _rs2282: List[Any] = Nil; var _cp2283: Int = _p2279; var _go2286 = true
  while (_go2286) { (if (input.startsWith("||", _cp2283)) Some(("||", _cp2283 + 2)) else None).flatMap { case (_r2295, _p2296) => (if ((if (input.startsWith("=", _p2296)) Some(("=", _p2296 + 1)) else None).isEmpty) Some(((), _p2296)) else None).map { case (_r2297, _p2298) => (new ~(_r2295, _r2297), _p2298) } }.flatMap { case (_r2291, _p2292) => parseSpacing(input, _p2292).map { case (_r2293, _p2294) => (new ~(_r2291, _r2293), _p2294) } }.flatMap { case (_r2287, _p2288) => parseAndExpr(input, _p2288).map { case (_r2289, _p2290) => (new ~(_r2287, _r2289), _p2290) } } match {
    case Some((_st2284, _np2285)) => _rs2282 = _rs2282 :+ _st2284; _cp2283 = _np2285
    case None => _go2286 = false } }
  Some((_rs2282, _cp2283)) }.map { case (_r2280, _p2281) => (new ~(_r2278, _r2280), _p2281) } }.map { case (r, p) => (_applyAction({  case base ~ ops => ops.asInstanceOf[List[Any]].foldLeft(base.asInstanceOf[Expr]) { (acc, item) => { val ((op ~ _) ~ _) ~ rhs = item; BinaryOp(acc, op.asInstanceOf[String], rhs.asInstanceOf[Expr]) } }  }, r), p) }
  }

  def parseInPatternPrimary(input: String, pos: Int): Option[(Any, Int)] = ((((((((((((((parseConstRef(input, pos)).orElse(parseVariable(input, pos))).orElse(parseSymbolLiteral(input, pos))).orElse(parseStringLiteral(input, pos))).orElse(parseSingleQuotedStringLiteral(input, pos))).orElse(parseNilLiteral(input, pos))).orElse(parseBoolLiteral(input, pos))).orElse(parseIntegerLiteral(input, pos))).orElse(parseFloatLiteral(input, pos))).orElse((if (input.startsWith("^", pos)) Some(("^", pos + 1)) else None).flatMap { case (_r2303, _p2304) => parseSpacing(input, _p2304).map { case (_r2305, _p2306) => (new ~(_r2303, _r2305), _p2306) } }.flatMap { case (_r2299, _p2300) => parsePostfixExpr(input, _p2300).map { case (_r2301, _p2302) => (new ~(_r2299, _r2301), _p2302) } })).orElse(parseParenExpr(input, pos))).orElse((if (input.startsWith("[", pos)) Some(("[", pos + 1)) else None).flatMap { case (_r2319, _p2320) => parseSpacing(input, _p2320).map { case (_r2321, _p2322) => (new ~(_r2319, _r2321), _p2322) } }.flatMap { case (_r2315, _p2316) => (parseInPatternListElem(input, _p2316).flatMap { case (_r2331, _p2332) => {
  var _rs2335: List[Any] = Nil; var _cp2336: Int = _p2332; var _go2339 = true
  while (_go2339) { parseSpacing(input, _cp2336).flatMap { case (_r2348, _p2349) => (if (input.startsWith(",", _p2349)) Some((",", _p2349 + 1)) else None).map { case (_r2350, _p2351) => (new ~(_r2348, _r2350), _p2351) } }.flatMap { case (_r2344, _p2345) => parseSpacing(input, _p2345).map { case (_r2346, _p2347) => (new ~(_r2344, _r2346), _p2347) } }.flatMap { case (_r2340, _p2341) => parseInPatternListElem(input, _p2341).map { case (_r2342, _p2343) => (new ~(_r2340, _r2342), _p2343) } } match {
    case Some((_st2337, _np2338)) => _rs2335 = _rs2335 :+ _st2337; _cp2336 = _np2338
    case None => _go2339 = false } }
  Some((_rs2335, _cp2336)) }.map { case (_r2333, _p2334) => (new ~(_r2331, _r2333), _p2334) } }.flatMap { case (_r2327, _p2328) => (parseSpacing(input, _p2328).flatMap { case (_r2352, _p2353) => (if (input.startsWith(",", _p2353)) Some((",", _p2353 + 1)) else None).map { case (_r2354, _p2355) => (new ~(_r2352, _r2354), _p2355) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2328)))).map { case (_r2329, _p2330) => (new ~(_r2327, _r2329), _p2330) } }.flatMap { case (_r2323, _p2324) => parseSpacing(input, _p2324).map { case (_r2325, _p2326) => (new ~(_r2323, _r2325), _p2326) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2316)))).map { case (_r2317, _p2318) => (new ~(_r2315, _r2317), _p2318) } }.flatMap { case (_r2311, _p2312) => (if (input.startsWith("]", _p2312)) Some(("]", _p2312 + 1)) else None).map { case (_r2313, _p2314) => (new ~(_r2311, _r2313), _p2314) } }.flatMap { case (_r2307, _p2308) => parseInlineSpacing(input, _p2308).map { case (_r2309, _p2310) => (new ~(_r2307, _r2309), _p2310) } })).orElse((if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r2368, _p2369) => parseSpacing(input, _p2369).map { case (_r2370, _p2371) => (new ~(_r2368, _r2370), _p2371) } }.flatMap { case (_r2364, _p2365) => (parseInPatternHashEntry(input, _p2365).flatMap { case (_r2380, _p2381) => {
  var _rs2384: List[Any] = Nil; var _cp2385: Int = _p2381; var _go2388 = true
  while (_go2388) { parseSpacing(input, _cp2385).flatMap { case (_r2397, _p2398) => (if (input.startsWith(",", _p2398)) Some((",", _p2398 + 1)) else None).map { case (_r2399, _p2400) => (new ~(_r2397, _r2399), _p2400) } }.flatMap { case (_r2393, _p2394) => parseSpacing(input, _p2394).map { case (_r2395, _p2396) => (new ~(_r2393, _r2395), _p2396) } }.flatMap { case (_r2389, _p2390) => parseInPatternHashEntry(input, _p2390).map { case (_r2391, _p2392) => (new ~(_r2389, _r2391), _p2392) } } match {
    case Some((_st2386, _np2387)) => _rs2384 = _rs2384 :+ _st2386; _cp2385 = _np2387
    case None => _go2388 = false } }
  Some((_rs2384, _cp2385)) }.map { case (_r2382, _p2383) => (new ~(_r2380, _r2382), _p2383) } }.flatMap { case (_r2376, _p2377) => (parseSpacing(input, _p2377).flatMap { case (_r2401, _p2402) => (if (input.startsWith(",", _p2402)) Some((",", _p2402 + 1)) else None).map { case (_r2403, _p2404) => (new ~(_r2401, _r2403), _p2404) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2377)))).map { case (_r2378, _p2379) => (new ~(_r2376, _r2378), _p2379) } }.flatMap { case (_r2372, _p2373) => parseSpacing(input, _p2373).map { case (_r2374, _p2375) => (new ~(_r2372, _r2374), _p2375) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2365)))).map { case (_r2366, _p2367) => (new ~(_r2364, _r2366), _p2367) } }.flatMap { case (_r2360, _p2361) => (if (input.startsWith("}", _p2361)) Some(("}", _p2361 + 1)) else None).map { case (_r2362, _p2363) => (new ~(_r2360, _r2362), _p2363) } }.flatMap { case (_r2356, _p2357) => parseInlineSpacing(input, _p2357).map { case (_r2358, _p2359) => (new ~(_r2356, _r2358), _p2359) } })).orElse(parseConstRef(input, pos).flatMap { case (_r2421, _p2422) => (if (input.startsWith("(", _p2422)) Some(("(", _p2422 + 1)) else None).map { case (_r2423, _p2424) => (new ~(_r2421, _r2423), _p2424) } }.flatMap { case (_r2417, _p2418) => parseSpacing(input, _p2418).map { case (_r2419, _p2420) => (new ~(_r2417, _r2419), _p2420) } }.flatMap { case (_r2413, _p2414) => (parseInPatternListElem(input, _p2414).flatMap { case (_r2433, _p2434) => {
  var _rs2437: List[Any] = Nil; var _cp2438: Int = _p2434; var _go2441 = true
  while (_go2441) { parseSpacing(input, _cp2438).flatMap { case (_r2450, _p2451) => (if (input.startsWith(",", _p2451)) Some((",", _p2451 + 1)) else None).map { case (_r2452, _p2453) => (new ~(_r2450, _r2452), _p2453) } }.flatMap { case (_r2446, _p2447) => parseSpacing(input, _p2447).map { case (_r2448, _p2449) => (new ~(_r2446, _r2448), _p2449) } }.flatMap { case (_r2442, _p2443) => parseInPatternListElem(input, _p2443).map { case (_r2444, _p2445) => (new ~(_r2442, _r2444), _p2445) } } match {
    case Some((_st2439, _np2440)) => _rs2437 = _rs2437 :+ _st2439; _cp2438 = _np2440
    case None => _go2441 = false } }
  Some((_rs2437, _cp2438)) }.map { case (_r2435, _p2436) => (new ~(_r2433, _r2435), _p2436) } }.flatMap { case (_r2429, _p2430) => (parseSpacing(input, _p2430).flatMap { case (_r2454, _p2455) => (if (input.startsWith(",", _p2455)) Some((",", _p2455 + 1)) else None).map { case (_r2456, _p2457) => (new ~(_r2454, _r2456), _p2457) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2430)))).map { case (_r2431, _p2432) => (new ~(_r2429, _r2431), _p2432) } }.flatMap { case (_r2425, _p2426) => parseSpacing(input, _p2426).map { case (_r2427, _p2428) => (new ~(_r2425, _r2427), _p2428) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2414)))).map { case (_r2415, _p2416) => (new ~(_r2413, _r2415), _p2416) } }.flatMap { case (_r2409, _p2410) => (if (input.startsWith(")", _p2410)) Some((")", _p2410 + 1)) else None).map { case (_r2411, _p2412) => (new ~(_r2409, _r2411), _p2412) } }.flatMap { case (_r2405, _p2406) => parseInlineSpacing(input, _p2406).map { case (_r2407, _p2408) => (new ~(_r2405, _r2407), _p2408) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2462, _p2463) => parseSpacing(input, _p2463).map { case (_r2464, _p2465) => (new ~(_r2462, _r2464), _p2465) } }.flatMap { case (_r2458, _p2459) => (((if (input.startsWith("nil", _p2459)) Some(("nil", _p2459 + 3)) else None).flatMap { case (_r2470, _p2471) => (if (parseIdentCont(input, _p2471).isEmpty) Some(((), _p2471)) else None).map { case (_r2472, _p2473) => (new ~(_r2470, _r2472), _p2473) } }.flatMap { case (_r2466, _p2467) => parseInlineSpacing(input, _p2467).map { case (_r2468, _p2469) => (new ~(_r2466, _r2468), _p2469) } }).orElse(parseVariable(input, _p2459)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2459)))).map { case (_r2460, _p2461) => (new ~(_r2458, _r2460), _p2461) } }).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternHashEntry(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r2478, _p2479) => parseSpacing(input, _p2479).map { case (_r2480, _p2481) => (new ~(_r2478, _r2480), _p2481) } }.flatMap { case (_r2474, _p2475) => (((if (input.startsWith("nil", _p2475)) Some(("nil", _p2475 + 3)) else None).flatMap { case (_r2486, _p2487) => (if (parseIdentCont(input, _p2487).isEmpty) Some(((), _p2487)) else None).map { case (_r2488, _p2489) => (new ~(_r2486, _r2488), _p2489) } }.flatMap { case (_r2482, _p2483) => parseInlineSpacing(input, _p2483).map { case (_r2484, _p2485) => (new ~(_r2482, _r2484), _p2485) } }).orElse(parseVariable(input, _p2475)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2475)))).map { case (_r2476, _p2477) => (new ~(_r2474, _r2476), _p2477) } }).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r2494, _p2495) => parseSpacing(input, _p2495).map { case (_r2496, _p2497) => (new ~(_r2494, _r2496), _p2497) } }.flatMap { case (_r2490, _p2491) => parseInPatternPrimary(input, _p2491).map { case (_r2492, _p2493) => (new ~(_r2490, _r2492), _p2493) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r2502, _p2503) => parseSpacing(input, _p2503).map { case (_r2504, _p2505) => (new ~(_r2502, _r2504), _p2505) } }.flatMap { case (_r2498, _p2499) => parseInPatternPrimary(input, _p2499).map { case (_r2500, _p2501) => (new ~(_r2498, _r2500), _p2501) } })).orElse(parseLabelSymbol(input, pos))).orElse(parseExpr(input, pos).flatMap { case (_r2518, _p2519) => parseSpacing(input, _p2519).map { case (_r2520, _p2521) => (new ~(_r2518, _r2520), _p2521) } }.flatMap { case (_r2514, _p2515) => (if (input.startsWith("=>", _p2515)) Some(("=>", _p2515 + 2)) else None).map { case (_r2516, _p2517) => (new ~(_r2514, _r2516), _p2517) } }.flatMap { case (_r2510, _p2511) => parseSpacing(input, _p2511).map { case (_r2512, _p2513) => (new ~(_r2510, _r2512), _p2513) } }.flatMap { case (_r2506, _p2507) => parseInPatternPrimary(input, _p2507).map { case (_r2508, _p2509) => (new ~(_r2506, _r2508), _p2509) } }).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternListElem(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2526, _p2527) => parseSpacing(input, _p2527).map { case (_r2528, _p2529) => (new ~(_r2526, _r2528), _p2529) } }.flatMap { case (_r2522, _p2523) => (((if (input.startsWith("nil", _p2523)) Some(("nil", _p2523 + 3)) else None).flatMap { case (_r2534, _p2535) => (if (parseIdentCont(input, _p2535).isEmpty) Some(((), _p2535)) else None).map { case (_r2536, _p2537) => (new ~(_r2534, _r2536), _p2537) } }.flatMap { case (_r2530, _p2531) => parseInlineSpacing(input, _p2531).map { case (_r2532, _p2533) => (new ~(_r2530, _r2532), _p2533) } }).orElse(parseVariable(input, _p2523)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2523)))).map { case (_r2524, _p2525) => (new ~(_r2522, _r2524), _p2525) } }).orElse(parseInPatternOrExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternOrExpr(input: String, pos: Int): Option[(Any, Int)] = parseInPatternPrimary(input, pos).flatMap { case (_r2538, _p2539) => {
  var _rs2542: List[Any] = Nil; var _cp2543: Int = _p2539; var _go2546 = true
  while (_go2546) { parseSpacing(input, _cp2543).flatMap { case (_r2555, _p2556) => (if (input.startsWith("|", _p2556)) Some(("|", _p2556 + 1)) else None).map { case (_r2557, _p2558) => (new ~(_r2555, _r2557), _p2558) } }.flatMap { case (_r2551, _p2552) => parseSpacing(input, _p2552).map { case (_r2553, _p2554) => (new ~(_r2551, _r2553), _p2554) } }.flatMap { case (_r2547, _p2548) => parseInPatternPrimary(input, _p2548).map { case (_r2549, _p2550) => (new ~(_r2547, _r2549), _p2550) } } match {
    case Some((_st2544, _np2545)) => _rs2542 = _rs2542 :+ _st2544; _cp2543 = _np2545
    case None => _go2546 = false } }
  Some((_rs2542, _cp2543)) }.map { case (_r2540, _p2541) => (new ~(_r2538, _r2540), _p2541) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternExpr(input: String, pos: Int): Option[(Any, Int)] = parseInPatternOrExpr(input, pos).flatMap { case (_r2563, _p2564) => (parseSpacing(input, _p2564).flatMap { case (_r2575, _p2576) => (if (input.startsWith("=>", _p2576)) Some(("=>", _p2576 + 2)) else None).map { case (_r2577, _p2578) => (new ~(_r2575, _r2577), _p2578) } }.flatMap { case (_r2571, _p2572) => parseSpacing(input, _p2572).map { case (_r2573, _p2574) => (new ~(_r2571, _r2573), _p2574) } }.flatMap { case (_r2567, _p2568) => parseVariable(input, _p2568).map { case (_r2569, _p2570) => (new ~(_r2567, _r2569), _p2570) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2564)))).map { case (_r2565, _p2566) => (new ~(_r2563, _r2565), _p2566) } }.flatMap { case (_r2559, _p2560) => (parseSpacing(input, _p2560).flatMap { case (_r2587, _p2588) => ((if (input.startsWith("if", _p2588)) Some(("if", _p2588 + 2)) else None).flatMap { case (_r2591, _p2592) => (if (parseIdentCont(input, _p2592).isEmpty) Some(((), _p2592)) else None).map { case (_r2593, _p2594) => (new ~(_r2591, _r2593), _p2594) } }).orElse((if (input.startsWith("unless", _p2588)) Some(("unless", _p2588 + 6)) else None).flatMap { case (_r2595, _p2596) => (if (parseIdentCont(input, _p2596).isEmpty) Some(((), _p2596)) else None).map { case (_r2597, _p2598) => (new ~(_r2595, _r2597), _p2598) } }).map { case (_r2589, _p2590) => (new ~(_r2587, _r2589), _p2590) } }.flatMap { case (_r2583, _p2584) => parseSpacing(input, _p2584).map { case (_r2585, _p2586) => (new ~(_r2583, _r2585), _p2586) } }.flatMap { case (_r2579, _p2580) => parseExpr(input, _p2580).map { case (_r2581, _p2582) => (new ~(_r2579, _r2581), _p2582) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2560)))).map { case (_r2561, _p2562) => (new ~(_r2559, _r2561), _p2562) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInMatchExpr(input: String, pos: Int): Option[(Any, Int)] = parseOrExpr(input, pos).flatMap { case (_r2599, _p2600) => ((if (input.startsWith("in", _p2600)) Some(("in", _p2600 + 2)) else None).flatMap { case (_r2611, _p2612) => (if (parseIdentCont(input, _p2612).isEmpty) Some(((), _p2612)) else None).map { case (_r2613, _p2614) => (new ~(_r2611, _r2613), _p2614) } }.flatMap { case (_r2607, _p2608) => parseSpacing(input, _p2608).map { case (_r2609, _p2610) => (new ~(_r2607, _r2609), _p2610) } }.flatMap { case (_r2603, _p2604) => parseInPatternExpr(input, _p2604).map { case (_r2605, _p2606) => (new ~(_r2603, _r2605), _p2606) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2600)))).map { case (_r2601, _p2602) => (new ~(_r2599, _r2601), _p2602) } }.map { case (r, p) => (_applyAction({  case base ~ _ => base.asInstanceOf[Expr]  }, r), p) }

  def parseConditionalExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(44, pos) {
    parseInMatchExpr(input, pos).flatMap { case (_r2615, _p2616) => (parseSpacing(input, _p2616).flatMap { case (_r2647, _p2648) => (if (input.startsWith("?", _p2648)) Some(("?", _p2648 + 1)) else None).map { case (_r2649, _p2650) => (new ~(_r2647, _r2649), _p2650) } }.flatMap { case (_r2643, _p2644) => parseSpacing(input, _p2644).map { case (_r2645, _p2646) => (new ~(_r2643, _r2645), _p2646) } }.flatMap { case (_r2639, _p2640) => parseConditionalExpr(input, _p2640).map { case (_r2641, _p2642) => (new ~(_r2639, _r2641), _p2642) } }.flatMap { case (_r2635, _p2636) => parseSpacing(input, _p2636).map { case (_r2637, _p2638) => (new ~(_r2635, _r2637), _p2638) } }.flatMap { case (_r2631, _p2632) => (if (input.startsWith(":", _p2632)) Some((":", _p2632 + 1)) else None).map { case (_r2633, _p2634) => (new ~(_r2631, _r2633), _p2634) } }.flatMap { case (_r2627, _p2628) => (if ((if (input.startsWith(":", _p2628)) Some((":", _p2628 + 1)) else None).isEmpty) Some(((), _p2628)) else None).map { case (_r2629, _p2630) => (new ~(_r2627, _r2629), _p2630) } }.flatMap { case (_r2623, _p2624) => parseSpacing(input, _p2624).map { case (_r2625, _p2626) => (new ~(_r2623, _r2625), _p2626) } }.flatMap { case (_r2619, _p2620) => parseConditionalExpr(input, _p2620).map { case (_r2621, _p2622) => (new ~(_r2619, _r2621), _p2622) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2616)))).map { case (_r2617, _p2618) => (new ~(_r2615, _r2617), _p2618) } }.map { case (r, p) => (_applyAction({  case base ~ _ => base.asInstanceOf[Expr]  }, r), p) }
  }

  def parseCompoundAssignOp(input: String, pos: Int): Option[(Any, Int)] = (((((((((((if (input.startsWith("<<=", pos)) Some(("<<=", pos + 3)) else None)).orElse((if (input.startsWith(">>=", pos)) Some((">>=", pos + 3)) else None))).orElse((if (input.startsWith("+=", pos)) Some(("+=", pos + 2)) else None))).orElse((if (input.startsWith("-=", pos)) Some(("-=", pos + 2)) else None))).orElse((if (input.startsWith("*=", pos)) Some(("*=", pos + 2)) else None))).orElse((if (input.startsWith("/=", pos)) Some(("/=", pos + 2)) else None))).orElse((if (input.startsWith("%=", pos)) Some(("%=", pos + 2)) else None))).orElse((if (input.startsWith("&=", pos)) Some(("&=", pos + 2)) else None))).orElse((if (input.startsWith("|=", pos)) Some(("|=", pos + 2)) else None))).orElse((if (input.startsWith("^=", pos)) Some(("^=", pos + 2)) else None))).orElse((if (input.startsWith("**=", pos)) Some(("**=", pos + 3)) else None)).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseAssignableTarget(input: String, pos: Int): Option[(Any, Int)] = ((((parseInstanceVarExpr(input, pos)).orElse(parseClassVarExpr(input, pos))).orElse(parseGlobalVarExpr(input, pos))).orElse(parseConstRef(input, pos))).orElse(parseLocalVarExpr(input, pos)).map { case (r, p) => (_applyAction({  v => v.asInstanceOf[Expr]  }, r), p) }

  def parseAssignValueExpr(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2659, _p2660) => parseSpacing(input, _p2660).map { case (_r2661, _p2662) => (new ~(_r2659, _r2661), _p2662) } }.flatMap { case (_r2655, _p2656) => parseConditionalExpr(input, _p2656).map { case (_r2657, _p2658) => (new ~(_r2655, _r2657), _p2658) } }).orElse(parseConditionalExpr(input, pos)).flatMap { case (_r2651, _p2652) => {
  var _rs2663: List[Any] = Nil; var _cp2664: Int = _p2652; var _go2667 = true
  while (_go2667) { parseSpacing(input, _cp2664).flatMap { case (_r2676, _p2677) => (if (input.startsWith(",", _p2677)) Some((",", _p2677 + 1)) else None).map { case (_r2678, _p2679) => (new ~(_r2676, _r2678), _p2679) } }.flatMap { case (_r2672, _p2673) => parseSpacing(input, _p2673).map { case (_r2674, _p2675) => (new ~(_r2672, _r2674), _p2675) } }.flatMap { case (_r2668, _p2669) => ((if (input.startsWith("*", _p2669)) Some(("*", _p2669 + 1)) else None).flatMap { case (_r2684, _p2685) => parseSpacing(input, _p2685).map { case (_r2686, _p2687) => (new ~(_r2684, _r2686), _p2687) } }.flatMap { case (_r2680, _p2681) => parseConditionalExpr(input, _p2681).map { case (_r2682, _p2683) => (new ~(_r2680, _r2682), _p2683) } }).orElse(parseConditionalExpr(input, _p2669)).map { case (_r2670, _p2671) => (new ~(_r2668, _r2670), _p2671) } } match {
    case Some((_st2665, _np2666)) => _rs2663 = _rs2663 :+ _st2665; _cp2664 = _np2666
    case None => _go2667 = false } }
  Some((_rs2663, _cp2664)) }.map { case (_r2653, _p2654) => (new ~(_r2651, _r2653), _p2654) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseChainedAssignRhs(input: String, pos: Int): Option[(Any, Int)] = _withMemo(45, pos) {
    (((((((((parsePostfixExpr(input, pos).flatMap { case (_r2704, _p2705) => parseDotSep(input, _p2705).map { case (_r2706, _p2707) => (new ~(_r2704, _r2706), _p2707) } }.flatMap { case (_r2700, _p2701) => parseMethodName(input, _p2701).map { case (_r2702, _p2703) => (new ~(_r2700, _r2702), _p2703) } }.flatMap { case (_r2696, _p2697) => parseAssignEq(input, _p2697).map { case (_r2698, _p2699) => (new ~(_r2696, _r2698), _p2699) } }.flatMap { case (_r2692, _p2693) => parseSpacing(input, _p2693).map { case (_r2694, _p2695) => (new ~(_r2692, _r2694), _p2695) } }.flatMap { case (_r2688, _p2689) => parseChainedAssignRhs(input, _p2689).map { case (_r2690, _p2691) => (new ~(_r2688, _r2690), _p2691) } }).orElse(parsePostfixExpr(input, pos).flatMap { case (_r2724, _p2725) => parseDotSep(input, _p2725).map { case (_r2726, _p2727) => (new ~(_r2724, _r2726), _p2727) } }.flatMap { case (_r2720, _p2721) => parseMethodName(input, _p2721).map { case (_r2722, _p2723) => (new ~(_r2720, _r2722), _p2723) } }.flatMap { case (_r2716, _p2717) => parseCompoundAssignOp(input, _p2717).map { case (_r2718, _p2719) => (new ~(_r2716, _r2718), _p2719) } }.flatMap { case (_r2712, _p2713) => parseSpacing(input, _p2713).map { case (_r2714, _p2715) => (new ~(_r2712, _r2714), _p2715) } }.flatMap { case (_r2708, _p2709) => parseChainedAssignRhs(input, _p2709).map { case (_r2710, _p2711) => (new ~(_r2708, _r2710), _p2711) } })).orElse(parsePostfixExpr(input, pos).flatMap { case (_r2744, _p2745) => parseDotSep(input, _p2745).map { case (_r2746, _p2747) => (new ~(_r2744, _r2746), _p2747) } }.flatMap { case (_r2740, _p2741) => parseMethodName(input, _p2741).map { case (_r2742, _p2743) => (new ~(_r2740, _r2742), _p2743) } }.flatMap { case (_r2736, _p2737) => ((if (input.startsWith("||=", _p2737)) Some(("||=", _p2737 + 3)) else None)).orElse((if (input.startsWith("&&=", _p2737)) Some(("&&=", _p2737 + 3)) else None)).map { case (_r2738, _p2739) => (new ~(_r2736, _r2738), _p2739) } }.flatMap { case (_r2732, _p2733) => parseSpacing(input, _p2733).map { case (_r2734, _p2735) => (new ~(_r2732, _r2734), _p2735) } }.flatMap { case (_r2728, _p2729) => parseChainedAssignRhs(input, _p2729).map { case (_r2730, _p2731) => (new ~(_r2728, _r2730), _p2731) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r2760, _p2761) => parseSubscriptArgs(input, _p2761).map { case (_r2762, _p2763) => (new ~(_r2760, _r2762), _p2763) } }.flatMap { case (_r2756, _p2757) => parseAssignEq(input, _p2757).map { case (_r2758, _p2759) => (new ~(_r2756, _r2758), _p2759) } }.flatMap { case (_r2752, _p2753) => parseSpacing(input, _p2753).map { case (_r2754, _p2755) => (new ~(_r2752, _r2754), _p2755) } }.flatMap { case (_r2748, _p2749) => parseChainedAssignRhs(input, _p2749).map { case (_r2750, _p2751) => (new ~(_r2748, _r2750), _p2751) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r2776, _p2777) => parseSubscriptArgs(input, _p2777).map { case (_r2778, _p2779) => (new ~(_r2776, _r2778), _p2779) } }.flatMap { case (_r2772, _p2773) => parseCompoundAssignOp(input, _p2773).map { case (_r2774, _p2775) => (new ~(_r2772, _r2774), _p2775) } }.flatMap { case (_r2768, _p2769) => parseSpacing(input, _p2769).map { case (_r2770, _p2771) => (new ~(_r2768, _r2770), _p2771) } }.flatMap { case (_r2764, _p2765) => parseChainedAssignRhs(input, _p2765).map { case (_r2766, _p2767) => (new ~(_r2764, _r2766), _p2767) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r2792, _p2793) => parseSubscriptArgs(input, _p2793).map { case (_r2794, _p2795) => (new ~(_r2792, _r2794), _p2795) } }.flatMap { case (_r2788, _p2789) => ((if (input.startsWith("||=", _p2789)) Some(("||=", _p2789 + 3)) else None)).orElse((if (input.startsWith("&&=", _p2789)) Some(("&&=", _p2789 + 3)) else None)).map { case (_r2790, _p2791) => (new ~(_r2788, _r2790), _p2791) } }.flatMap { case (_r2784, _p2785) => parseSpacing(input, _p2785).map { case (_r2786, _p2787) => (new ~(_r2784, _r2786), _p2787) } }.flatMap { case (_r2780, _p2781) => parseChainedAssignRhs(input, _p2781).map { case (_r2782, _p2783) => (new ~(_r2780, _r2782), _p2783) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r2804, _p2805) => parseCompoundAssignOp(input, _p2805).map { case (_r2806, _p2807) => (new ~(_r2804, _r2806), _p2807) } }.flatMap { case (_r2800, _p2801) => parseSpacing(input, _p2801).map { case (_r2802, _p2803) => (new ~(_r2800, _r2802), _p2803) } }.flatMap { case (_r2796, _p2797) => parseChainedAssignRhs(input, _p2797).map { case (_r2798, _p2799) => (new ~(_r2796, _r2798), _p2799) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r2816, _p2817) => ((if (input.startsWith("||=", _p2817)) Some(("||=", _p2817 + 3)) else None)).orElse((if (input.startsWith("&&=", _p2817)) Some(("&&=", _p2817 + 3)) else None)).map { case (_r2818, _p2819) => (new ~(_r2816, _r2818), _p2819) } }.flatMap { case (_r2812, _p2813) => parseSpacing(input, _p2813).map { case (_r2814, _p2815) => (new ~(_r2812, _r2814), _p2815) } }.flatMap { case (_r2808, _p2809) => parseChainedAssignRhs(input, _p2809).map { case (_r2810, _p2811) => (new ~(_r2808, _r2810), _p2811) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r2828, _p2829) => parseAssignEq(input, _p2829).map { case (_r2830, _p2831) => (new ~(_r2828, _r2830), _p2831) } }.flatMap { case (_r2824, _p2825) => parseSpacing(input, _p2825).map { case (_r2826, _p2827) => (new ~(_r2824, _r2826), _p2827) } }.flatMap { case (_r2820, _p2821) => parseChainedAssignRhs(input, _p2821).map { case (_r2822, _p2823) => (new ~(_r2820, _r2822), _p2823) } })).orElse(parseConditionalExpr(input, pos)).map { case (r, p) => (_applyAction({  v => if (v.isInstanceOf[Expr]) v.asInstanceOf[Expr] else NilLiteral()  }, r), p) }
  }

  def parseMultiAssignElem(input: String, pos: Int): Option[(Any, Int)] = (((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2836, _p2837) => parseSpacing(input, _p2837).map { case (_r2838, _p2839) => (new ~(_r2836, _r2838), _p2839) } }.flatMap { case (_r2832, _p2833) => (parseAssignableTarget(input, _p2833).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2833)))).map { case (_r2834, _p2835) => (new ~(_r2832, _r2834), _p2835) } }).orElse((if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r2860, _p2861) => parseSpacing(input, _p2861).map { case (_r2862, _p2863) => (new ~(_r2860, _r2862), _p2863) } }.flatMap { case (_r2856, _p2857) => parseMultiAssignElem(input, _p2857).map { case (_r2858, _p2859) => (new ~(_r2856, _r2858), _p2859) } }.flatMap { case (_r2852, _p2853) => {
  var _rs2864: List[Any] = Nil; var _cp2865: Int = _p2853; var _go2868 = true
  while (_go2868) { parseSpacing(input, _cp2865).flatMap { case (_r2877, _p2878) => (if (input.startsWith(",", _p2878)) Some((",", _p2878 + 1)) else None).map { case (_r2879, _p2880) => (new ~(_r2877, _r2879), _p2880) } }.flatMap { case (_r2873, _p2874) => parseSpacing(input, _p2874).map { case (_r2875, _p2876) => (new ~(_r2873, _r2875), _p2876) } }.flatMap { case (_r2869, _p2870) => parseMultiAssignElem(input, _p2870).map { case (_r2871, _p2872) => (new ~(_r2869, _r2871), _p2872) } } match {
    case Some((_st2866, _np2867)) => _rs2864 = _rs2864 :+ _st2866; _cp2865 = _np2867
    case None => _go2868 = false } }
  Some((_rs2864, _cp2865)) }.map { case (_r2854, _p2855) => (new ~(_r2852, _r2854), _p2855) } }.flatMap { case (_r2848, _p2849) => (parseSpacing(input, _p2849).flatMap { case (_r2881, _p2882) => (if (input.startsWith(",", _p2882)) Some((",", _p2882 + 1)) else None).map { case (_r2883, _p2884) => (new ~(_r2881, _r2883), _p2884) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2849)))).map { case (_r2850, _p2851) => (new ~(_r2848, _r2850), _p2851) } }.flatMap { case (_r2844, _p2845) => parseSpacing(input, _p2845).map { case (_r2846, _p2847) => (new ~(_r2844, _r2846), _p2847) } }.flatMap { case (_r2840, _p2841) => (if (input.startsWith(")", _p2841)) Some((")", _p2841 + 1)) else None).map { case (_r2842, _p2843) => (new ~(_r2840, _r2842), _p2843) } })).orElse(parseAssignableTarget(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseMultiAssignTargets(input: String, pos: Int): Option[(Any, Int)] = ((parseMultiAssignElem(input, pos).flatMap { case (_r2889, _p2890) => {
  parseSpacing(input, _p2890).flatMap { case (_r2908, _p2909) => (if (input.startsWith(",", _p2909)) Some((",", _p2909 + 1)) else None).map { case (_r2910, _p2911) => (new ~(_r2908, _r2910), _p2911) } }.flatMap { case (_r2904, _p2905) => parseSpacing(input, _p2905).map { case (_r2906, _p2907) => (new ~(_r2904, _r2906), _p2907) } }.flatMap { case (_r2900, _p2901) => parseMultiAssignElem(input, _p2901).map { case (_r2902, _p2903) => (new ~(_r2900, _r2902), _p2903) } } match {
    case None => None
    case Some((_fs2893, _fp2894)) =>
      var _rs2895: List[Any] = List(_fs2893); var _cp2896: Int = _fp2894; var _go2899 = true
      while (_go2899) { parseSpacing(input, _cp2896).flatMap { case (_r2920, _p2921) => (if (input.startsWith(",", _p2921)) Some((",", _p2921 + 1)) else None).map { case (_r2922, _p2923) => (new ~(_r2920, _r2922), _p2923) } }.flatMap { case (_r2916, _p2917) => parseSpacing(input, _p2917).map { case (_r2918, _p2919) => (new ~(_r2916, _r2918), _p2919) } }.flatMap { case (_r2912, _p2913) => parseMultiAssignElem(input, _p2913).map { case (_r2914, _p2915) => (new ~(_r2912, _r2914), _p2915) } } match {
        case Some((_st2897, _np2898)) => _rs2895 = _rs2895 :+ _st2897; _cp2896 = _np2898
        case None => _go2899 = false } }
      Some((_rs2895, _cp2896)) } }.map { case (_r2891, _p2892) => (new ~(_r2889, _r2891), _p2892) } }.flatMap { case (_r2885, _p2886) => (parseSpacing(input, _p2886).flatMap { case (_r2924, _p2925) => (if (input.startsWith(",", _p2925)) Some((",", _p2925 + 1)) else None).map { case (_r2926, _p2927) => (new ~(_r2924, _r2926), _p2927) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2886)))).map { case (_r2887, _p2888) => (new ~(_r2885, _r2887), _p2888) } }).orElse(parseMultiAssignElem(input, pos).flatMap { case (_r2932, _p2933) => parseSpacing(input, _p2933).map { case (_r2934, _p2935) => (new ~(_r2932, _r2934), _p2935) } }.flatMap { case (_r2928, _p2929) => (if (input.startsWith(",", _p2929)) Some((",", _p2929 + 1)) else None).map { case (_r2930, _p2931) => (new ~(_r2928, _r2930), _p2931) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2940, _p2941) => parseSpacing(input, _p2941).map { case (_r2942, _p2943) => (new ~(_r2940, _r2942), _p2943) } }.flatMap { case (_r2936, _p2937) => (parseAssignableTarget(input, _p2937).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2937)))).map { case (_r2938, _p2939) => (new ~(_r2936, _r2938), _p2939) } }).map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseAssignmentExpr(input: String, pos: Int): Option[(Any, Int)] = ((((((((((parsePostfixExpr(input, pos).flatMap { case (_r2960, _p2961) => parseDotSep(input, _p2961).map { case (_r2962, _p2963) => (new ~(_r2960, _r2962), _p2963) } }.flatMap { case (_r2956, _p2957) => parseMethodName(input, _p2957).map { case (_r2958, _p2959) => (new ~(_r2956, _r2958), _p2959) } }.flatMap { case (_r2952, _p2953) => parseAssignEq(input, _p2953).map { case (_r2954, _p2955) => (new ~(_r2952, _r2954), _p2955) } }.flatMap { case (_r2948, _p2949) => parseSpacing(input, _p2949).map { case (_r2950, _p2951) => (new ~(_r2948, _r2950), _p2951) } }.flatMap { case (_r2944, _p2945) => parseAssignValueExpr(input, _p2945).map { case (_r2946, _p2947) => (new ~(_r2944, _r2946), _p2947) } }).orElse(parsePostfixExpr(input, pos).flatMap { case (_r2980, _p2981) => parseDotSep(input, _p2981).map { case (_r2982, _p2983) => (new ~(_r2980, _r2982), _p2983) } }.flatMap { case (_r2976, _p2977) => parseMethodName(input, _p2977).map { case (_r2978, _p2979) => (new ~(_r2976, _r2978), _p2979) } }.flatMap { case (_r2972, _p2973) => parseCompoundAssignOp(input, _p2973).map { case (_r2974, _p2975) => (new ~(_r2972, _r2974), _p2975) } }.flatMap { case (_r2968, _p2969) => parseSpacing(input, _p2969).map { case (_r2970, _p2971) => (new ~(_r2968, _r2970), _p2971) } }.flatMap { case (_r2964, _p2965) => parseChainedAssignRhs(input, _p2965).map { case (_r2966, _p2967) => (new ~(_r2964, _r2966), _p2967) } })).orElse(parsePostfixExpr(input, pos).flatMap { case (_r3000, _p3001) => parseDotSep(input, _p3001).map { case (_r3002, _p3003) => (new ~(_r3000, _r3002), _p3003) } }.flatMap { case (_r2996, _p2997) => parseMethodName(input, _p2997).map { case (_r2998, _p2999) => (new ~(_r2996, _r2998), _p2999) } }.flatMap { case (_r2992, _p2993) => ((if (input.startsWith("||=", _p2993)) Some(("||=", _p2993 + 3)) else None)).orElse((if (input.startsWith("&&=", _p2993)) Some(("&&=", _p2993 + 3)) else None)).map { case (_r2994, _p2995) => (new ~(_r2992, _r2994), _p2995) } }.flatMap { case (_r2988, _p2989) => parseSpacing(input, _p2989).map { case (_r2990, _p2991) => (new ~(_r2988, _r2990), _p2991) } }.flatMap { case (_r2984, _p2985) => parseChainedAssignRhs(input, _p2985).map { case (_r2986, _p2987) => (new ~(_r2984, _r2986), _p2987) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3016, _p3017) => parseSubscriptArgs(input, _p3017).map { case (_r3018, _p3019) => (new ~(_r3016, _r3018), _p3019) } }.flatMap { case (_r3012, _p3013) => parseAssignEq(input, _p3013).map { case (_r3014, _p3015) => (new ~(_r3012, _r3014), _p3015) } }.flatMap { case (_r3008, _p3009) => parseSpacing(input, _p3009).map { case (_r3010, _p3011) => (new ~(_r3008, _r3010), _p3011) } }.flatMap { case (_r3004, _p3005) => parseAssignValueExpr(input, _p3005).map { case (_r3006, _p3007) => (new ~(_r3004, _r3006), _p3007) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3032, _p3033) => parseSubscriptArgs(input, _p3033).map { case (_r3034, _p3035) => (new ~(_r3032, _r3034), _p3035) } }.flatMap { case (_r3028, _p3029) => parseCompoundAssignOp(input, _p3029).map { case (_r3030, _p3031) => (new ~(_r3028, _r3030), _p3031) } }.flatMap { case (_r3024, _p3025) => parseSpacing(input, _p3025).map { case (_r3026, _p3027) => (new ~(_r3024, _r3026), _p3027) } }.flatMap { case (_r3020, _p3021) => parseChainedAssignRhs(input, _p3021).map { case (_r3022, _p3023) => (new ~(_r3020, _r3022), _p3023) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3048, _p3049) => parseSubscriptArgs(input, _p3049).map { case (_r3050, _p3051) => (new ~(_r3048, _r3050), _p3051) } }.flatMap { case (_r3044, _p3045) => ((if (input.startsWith("||=", _p3045)) Some(("||=", _p3045 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3045)) Some(("&&=", _p3045 + 3)) else None)).map { case (_r3046, _p3047) => (new ~(_r3044, _r3046), _p3047) } }.flatMap { case (_r3040, _p3041) => parseSpacing(input, _p3041).map { case (_r3042, _p3043) => (new ~(_r3040, _r3042), _p3043) } }.flatMap { case (_r3036, _p3037) => parseChainedAssignRhs(input, _p3037).map { case (_r3038, _p3039) => (new ~(_r3036, _r3038), _p3039) } })).orElse(parseMultiAssignTargets(input, pos).flatMap { case (_r3060, _p3061) => parseAssignEq(input, _p3061).map { case (_r3062, _p3063) => (new ~(_r3060, _r3062), _p3063) } }.flatMap { case (_r3056, _p3057) => parseSpacing(input, _p3057).map { case (_r3058, _p3059) => (new ~(_r3056, _r3058), _p3059) } }.flatMap { case (_r3052, _p3053) => parseAssignValueExpr(input, _p3053).map { case (_r3054, _p3055) => (new ~(_r3052, _r3054), _p3055) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3072, _p3073) => parseCompoundAssignOp(input, _p3073).map { case (_r3074, _p3075) => (new ~(_r3072, _r3074), _p3075) } }.flatMap { case (_r3068, _p3069) => parseSpacing(input, _p3069).map { case (_r3070, _p3071) => (new ~(_r3068, _r3070), _p3071) } }.flatMap { case (_r3064, _p3065) => parseChainedAssignRhs(input, _p3065).map { case (_r3066, _p3067) => (new ~(_r3064, _r3066), _p3067) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3084, _p3085) => ((if (input.startsWith("||=", _p3085)) Some(("||=", _p3085 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3085)) Some(("&&=", _p3085 + 3)) else None)).map { case (_r3086, _p3087) => (new ~(_r3084, _r3086), _p3087) } }.flatMap { case (_r3080, _p3081) => parseSpacing(input, _p3081).map { case (_r3082, _p3083) => (new ~(_r3080, _r3082), _p3083) } }.flatMap { case (_r3076, _p3077) => parseChainedAssignRhs(input, _p3077).map { case (_r3078, _p3079) => (new ~(_r3076, _r3078), _p3079) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3096, _p3097) => parseAssignEq(input, _p3097).map { case (_r3098, _p3099) => (new ~(_r3096, _r3098), _p3099) } }.flatMap { case (_r3092, _p3093) => parseSpacing(input, _p3093).map { case (_r3094, _p3095) => (new ~(_r3092, _r3094), _p3095) } }.flatMap { case (_r3088, _p3089) => parseAssignValueExpr(input, _p3089).map { case (_r3090, _p3091) => (new ~(_r3088, _r3090), _p3091) } })).orElse(parseConditionalExpr(input, pos)).map { case (r, p) => (_applyAction({  v => if (v.isInstanceOf[Expr]) v.asInstanceOf[Expr] else NilLiteral()  }, r), p) }

  def parseExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(46, pos) {
    parseAssignmentExpr(input, pos).flatMap { case (_r3100, _p3101) => {
  var _rs3104: List[Any] = Nil; var _cp3105: Int = _p3101; var _go3108 = true
  while (_go3108) { ((if (input.startsWith("and", _cp3105)) Some(("and", _cp3105 + 3)) else None).flatMap { case (_r3117, _p3118) => (if (parseIdentCont(input, _p3118).isEmpty) Some(((), _p3118)) else None).map { case (_r3119, _p3120) => (new ~(_r3117, _r3119), _p3120) } }).orElse((if (input.startsWith("or", _cp3105)) Some(("or", _cp3105 + 2)) else None).flatMap { case (_r3121, _p3122) => (if (parseIdentCont(input, _p3122).isEmpty) Some(((), _p3122)) else None).map { case (_r3123, _p3124) => (new ~(_r3121, _r3123), _p3124) } }).flatMap { case (_r3113, _p3114) => parseSpacing(input, _p3114).map { case (_r3115, _p3116) => (new ~(_r3113, _r3115), _p3116) } }.flatMap { case (_r3109, _p3110) => parseAssignmentExpr(input, _p3110).map { case (_r3111, _p3112) => (new ~(_r3109, _r3111), _p3112) } } match {
    case Some((_st3106, _np3107)) => _rs3104 = _rs3104 :+ _st3106; _cp3105 = _np3107
    case None => _go3108 = false } }
  Some((_rs3104, _cp3105)) }.map { case (_r3102, _p3103) => (new ~(_r3100, _r3102), _p3103) } }.map { case (r, p) => (_applyAction({  case base ~ _ => base.asInstanceOf[Expr]  }, r), p) }
  }

  def parseBeginExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r3161, _p3162) => (if (parseIdentCont(input, _p3162).isEmpty) Some(((), _p3162)) else None).map { case (_r3163, _p3164) => (new ~(_r3161, _r3163), _p3164) } }.flatMap { case (_r3157, _p3158) => parseSpacing(input, _p3158).map { case (_r3159, _p3160) => (new ~(_r3157, _r3159), _p3160) } }.flatMap { case (_r3153, _p3154) => {
  var _rs3165: List[Any] = Nil; var _cp3166: Int = _p3154; var _go3169 = true
  while (_go3169) { parseStatementSep(input, _cp3166) match {
    case Some((_st3167, _np3168)) => _rs3165 = _rs3165 :+ _st3167; _cp3166 = _np3168
    case None => _go3169 = false } }
  Some((_rs3165, _cp3166)) }.map { case (_r3155, _p3156) => (new ~(_r3153, _r3155), _p3156) } }.flatMap { case (_r3149, _p3150) => {
  var _rs3170: List[Any] = Nil; var _cp3171: Int = _p3150; var _go3174 = true
  while (_go3174) { (if ((parseRescueStop(input, _cp3171)).orElse(parseEndKeyword(input, _cp3171)).isEmpty) Some(((), _cp3171)) else None).flatMap { case (_r3179, _p3180) => parseStatement(input, _p3180).map { case (_r3181, _p3182) => (new ~(_r3179, _r3181), _p3182) } }.flatMap { case (_r3175, _p3176) => {
  var _rs3183: List[Any] = Nil; var _cp3184: Int = _p3176; var _go3187 = true
  while (_go3187) { parseStatementSep(input, _cp3184) match {
    case Some((_st3185, _np3186)) => _rs3183 = _rs3183 :+ _st3185; _cp3184 = _np3186
    case None => _go3187 = false } }
  Some((_rs3183, _cp3184)) }.map { case (_r3177, _p3178) => (new ~(_r3175, _r3177), _p3178) } } match {
    case Some((_st3172, _np3173)) => _rs3170 = _rs3170 :+ _st3172; _cp3171 = _np3173
    case None => _go3174 = false } }
  Some((_rs3170, _cp3171)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3151, _p3152) => (new ~(_r3149, _r3151), _p3152) } }.flatMap { case (_r3145, _p3146) => {
  var _rs3188: List[Any] = Nil; var _cp3189: Int = _p3146; var _go3192 = true
  while (_go3192) { parseStatementSep(input, _cp3189) match {
    case Some((_st3190, _np3191)) => _rs3188 = _rs3188 :+ _st3190; _cp3189 = _np3191
    case None => _go3192 = false } }
  Some((_rs3188, _cp3189)) }.map { case (_r3147, _p3148) => (new ~(_r3145, _r3147), _p3148) } }.flatMap { case (_r3141, _p3142) => {
  var _rs3193: List[Any] = Nil; var _cp3194: Int = _p3142; var _go3197 = true
  while (_go3197) { parseRescueClause(input, _cp3194) match {
    case Some((_st3195, _np3196)) => _rs3193 = _rs3193 :+ _st3195; _cp3194 = _np3196
    case None => _go3197 = false } }
  Some((_rs3193, _cp3194)) }.map { case (_r3143, _p3144) => (new ~(_r3141, _r3143), _p3144) } }.flatMap { case (_r3137, _p3138) => {
  var _rs3198: List[Any] = Nil; var _cp3199: Int = _p3138; var _go3202 = true
  while (_go3202) { parseStatementSep(input, _cp3199) match {
    case Some((_st3200, _np3201)) => _rs3198 = _rs3198 :+ _st3200; _cp3199 = _np3201
    case None => _go3202 = false } }
  Some((_rs3198, _cp3199)) }.map { case (_r3139, _p3140) => (new ~(_r3137, _r3139), _p3140) } }.flatMap { case (_r3133, _p3134) => ((if (input.startsWith("else", _p3134)) Some(("else", _p3134 + 4)) else None).flatMap { case (_r3219, _p3220) => (if (parseIdentCont(input, _p3220).isEmpty) Some(((), _p3220)) else None).map { case (_r3221, _p3222) => (new ~(_r3219, _r3221), _p3222) } }.flatMap { case (_r3215, _p3216) => parseSpacing(input, _p3216).map { case (_r3217, _p3218) => (new ~(_r3215, _r3217), _p3218) } }.flatMap { case (_r3211, _p3212) => {
  var _rs3223: List[Any] = Nil; var _cp3224: Int = _p3212; var _go3227 = true
  while (_go3227) { parseStatementSep(input, _cp3224) match {
    case Some((_st3225, _np3226)) => _rs3223 = _rs3223 :+ _st3225; _cp3224 = _np3226
    case None => _go3227 = false } }
  Some((_rs3223, _cp3224)) }.map { case (_r3213, _p3214) => (new ~(_r3211, _r3213), _p3214) } }.flatMap { case (_r3207, _p3208) => {
  var _rs3228: List[Any] = Nil; var _cp3229: Int = _p3208; var _go3232 = true
  while (_go3232) { (if (parseDoBlockStop(input, _cp3229).isEmpty) Some(((), _cp3229)) else None).flatMap { case (_r3237, _p3238) => parseStatement(input, _p3238).map { case (_r3239, _p3240) => (new ~(_r3237, _r3239), _p3240) } }.flatMap { case (_r3233, _p3234) => {
  var _rs3241: List[Any] = Nil; var _cp3242: Int = _p3234; var _go3245 = true
  while (_go3245) { parseStatementSep(input, _cp3242) match {
    case Some((_st3243, _np3244)) => _rs3241 = _rs3241 :+ _st3243; _cp3242 = _np3244
    case None => _go3245 = false } }
  Some((_rs3241, _cp3242)) }.map { case (_r3235, _p3236) => (new ~(_r3233, _r3235), _p3236) } } match {
    case Some((_st3230, _np3231)) => _rs3228 = _rs3228 :+ _st3230; _cp3229 = _np3231
    case None => _go3232 = false } }
  Some((_rs3228, _cp3229)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3209, _p3210) => (new ~(_r3207, _r3209), _p3210) } }.flatMap { case (_r3203, _p3204) => {
  var _rs3246: List[Any] = Nil; var _cp3247: Int = _p3204; var _go3250 = true
  while (_go3250) { parseStatementSep(input, _cp3247) match {
    case Some((_st3248, _np3249)) => _rs3246 = _rs3246 :+ _st3248; _cp3247 = _np3249
    case None => _go3250 = false } }
  Some((_rs3246, _cp3247)) }.map { case (_r3205, _p3206) => (new ~(_r3203, _r3205), _p3206) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3134)))).map { case (_r3135, _p3136) => (new ~(_r3133, _r3135), _p3136) } }.flatMap { case (_r3129, _p3130) => ((if (input.startsWith("ensure", _p3130)) Some(("ensure", _p3130 + 6)) else None).flatMap { case (_r3267, _p3268) => (if (parseIdentCont(input, _p3268).isEmpty) Some(((), _p3268)) else None).map { case (_r3269, _p3270) => (new ~(_r3267, _r3269), _p3270) } }.flatMap { case (_r3263, _p3264) => parseSpacing(input, _p3264).map { case (_r3265, _p3266) => (new ~(_r3263, _r3265), _p3266) } }.flatMap { case (_r3259, _p3260) => {
  var _rs3271: List[Any] = Nil; var _cp3272: Int = _p3260; var _go3275 = true
  while (_go3275) { parseStatementSep(input, _cp3272) match {
    case Some((_st3273, _np3274)) => _rs3271 = _rs3271 :+ _st3273; _cp3272 = _np3274
    case None => _go3275 = false } }
  Some((_rs3271, _cp3272)) }.map { case (_r3261, _p3262) => (new ~(_r3259, _r3261), _p3262) } }.flatMap { case (_r3255, _p3256) => {
  var _rs3276: List[Any] = Nil; var _cp3277: Int = _p3256; var _go3280 = true
  while (_go3280) { (if (parseEndKeyword(input, _cp3277).isEmpty) Some(((), _cp3277)) else None).flatMap { case (_r3285, _p3286) => parseStatement(input, _p3286).map { case (_r3287, _p3288) => (new ~(_r3285, _r3287), _p3288) } }.flatMap { case (_r3281, _p3282) => {
  var _rs3289: List[Any] = Nil; var _cp3290: Int = _p3282; var _go3293 = true
  while (_go3293) { parseStatementSep(input, _cp3290) match {
    case Some((_st3291, _np3292)) => _rs3289 = _rs3289 :+ _st3291; _cp3290 = _np3292
    case None => _go3293 = false } }
  Some((_rs3289, _cp3290)) }.map { case (_r3283, _p3284) => (new ~(_r3281, _r3283), _p3284) } } match {
    case Some((_st3278, _np3279)) => _rs3276 = _rs3276 :+ _st3278; _cp3277 = _np3279
    case None => _go3280 = false } }
  Some((_rs3276, _cp3277)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3257, _p3258) => (new ~(_r3255, _r3257), _p3258) } }.flatMap { case (_r3251, _p3252) => {
  var _rs3294: List[Any] = Nil; var _cp3295: Int = _p3252; var _go3298 = true
  while (_go3298) { parseStatementSep(input, _cp3295) match {
    case Some((_st3296, _np3297)) => _rs3294 = _rs3294 :+ _st3296; _cp3295 = _np3297
    case None => _go3298 = false } }
  Some((_rs3294, _cp3295)) }.map { case (_r3253, _p3254) => (new ~(_r3251, _r3253), _p3254) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3130)))).map { case (_r3131, _p3132) => (new ~(_r3129, _r3131), _p3132) } }.flatMap { case (_r3125, _p3126) => parseEndKeyword(input, _p3126).map { case (_r3127, _p3128) => (new ~(_r3125, _r3127), _p3128) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseDefExprForm(input: String, pos: Int): Option[(Any, Int)] = parseDefStmt(input, pos).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseReturnExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("return", pos)) Some(("return", pos + 6)) else None).flatMap { case (_r3307, _p3308) => (if (parseIdentCont(input, _p3308).isEmpty) Some(((), _p3308)) else None).map { case (_r3309, _p3310) => (new ~(_r3307, _r3309), _p3310) } }.flatMap { case (_r3303, _p3304) => parseInlineSpacing(input, _p3304).map { case (_r3305, _p3306) => (new ~(_r3303, _r3305), _p3306) } }.flatMap { case (_r3299, _p3300) => ((if (parseCommandArgStop(input, _p3300).isEmpty) Some(((), _p3300)) else None).flatMap { case (_r3315, _p3316) => parseExpr(input, _p3316).map { case (_r3317, _p3318) => (new ~(_r3315, _r3317), _p3318) } }.flatMap { case (_r3311, _p3312) => {
  var _rs3319: List[Any] = Nil; var _cp3320: Int = _p3312; var _go3323 = true
  while (_go3323) { parseSpacing(input, _cp3320).flatMap { case (_r3332, _p3333) => (if (input.startsWith(",", _p3333)) Some((",", _p3333 + 1)) else None).map { case (_r3334, _p3335) => (new ~(_r3332, _r3334), _p3335) } }.flatMap { case (_r3328, _p3329) => parseSpacing(input, _p3329).map { case (_r3330, _p3331) => (new ~(_r3328, _r3330), _p3331) } }.flatMap { case (_r3324, _p3325) => parseExpr(input, _p3325).map { case (_r3326, _p3327) => (new ~(_r3324, _r3326), _p3327) } } match {
    case Some((_st3321, _np3322)) => _rs3319 = _rs3319 :+ _st3321; _cp3320 = _np3322
    case None => _go3323 = false } }
  Some((_rs3319, _cp3320)) }.map { case (_r3313, _p3314) => (new ~(_r3311, _r3313), _p3314) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3300)))).map { case (_r3301, _p3302) => (new ~(_r3299, _r3301), _p3302) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseYieldExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("yield", pos)) Some(("yield", pos + 5)) else None).flatMap { case (_r3344, _p3345) => (if (parseIdentCont(input, _p3345).isEmpty) Some(((), _p3345)) else None).map { case (_r3346, _p3347) => (new ~(_r3344, _r3346), _p3347) } }.flatMap { case (_r3340, _p3341) => parseInlineSpacing(input, _p3341).map { case (_r3342, _p3343) => (new ~(_r3340, _r3342), _p3343) } }.flatMap { case (_r3336, _p3337) => (parseCallArgs(input, _p3337).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3337)))).map { case (_r3338, _p3339) => (new ~(_r3336, _r3338), _p3339) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseSingletonClassExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r3380, _p3381) => (if (parseIdentCont(input, _p3381).isEmpty) Some(((), _p3381)) else None).map { case (_r3382, _p3383) => (new ~(_r3380, _r3382), _p3383) } }.flatMap { case (_r3376, _p3377) => parseSpacing(input, _p3377).map { case (_r3378, _p3379) => (new ~(_r3376, _r3378), _p3379) } }.flatMap { case (_r3372, _p3373) => (if (input.startsWith("<<", _p3373)) Some(("<<", _p3373 + 2)) else None).map { case (_r3374, _p3375) => (new ~(_r3372, _r3374), _p3375) } }.flatMap { case (_r3368, _p3369) => parseSpacing(input, _p3369).map { case (_r3370, _p3371) => (new ~(_r3368, _r3370), _p3371) } }.flatMap { case (_r3364, _p3365) => parseExpr(input, _p3365).map { case (_r3366, _p3367) => (new ~(_r3364, _r3366), _p3367) } }.flatMap { case (_r3360, _p3361) => {
  var _rs3384: List[Any] = Nil; var _cp3385: Int = _p3361; var _go3388 = true
  while (_go3388) { parseStatementSep(input, _cp3385) match {
    case Some((_st3386, _np3387)) => _rs3384 = _rs3384 :+ _st3386; _cp3385 = _np3387
    case None => _go3388 = false } }
  Some((_rs3384, _cp3385)) }.map { case (_r3362, _p3363) => (new ~(_r3360, _r3362), _p3363) } }.flatMap { case (_r3356, _p3357) => {
  var _rs3389: List[Any] = Nil; var _cp3390: Int = _p3357; var _go3393 = true
  while (_go3393) { (if (parseEndKeyword(input, _cp3390).isEmpty) Some(((), _cp3390)) else None).flatMap { case (_r3398, _p3399) => parseStatement(input, _p3399).map { case (_r3400, _p3401) => (new ~(_r3398, _r3400), _p3401) } }.flatMap { case (_r3394, _p3395) => {
  var _rs3402: List[Any] = Nil; var _cp3403: Int = _p3395; var _go3406 = true
  while (_go3406) { parseStatementSep(input, _cp3403) match {
    case Some((_st3404, _np3405)) => _rs3402 = _rs3402 :+ _st3404; _cp3403 = _np3405
    case None => _go3406 = false } }
  Some((_rs3402, _cp3403)) }.map { case (_r3396, _p3397) => (new ~(_r3394, _r3396), _p3397) } } match {
    case Some((_st3391, _np3392)) => _rs3389 = _rs3389 :+ _st3391; _cp3390 = _np3392
    case None => _go3393 = false } }
  Some((_rs3389, _cp3390)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3358, _p3359) => (new ~(_r3356, _r3358), _p3359) } }.flatMap { case (_r3352, _p3353) => {
  var _rs3407: List[Any] = Nil; var _cp3408: Int = _p3353; var _go3411 = true
  while (_go3411) { parseStatementSep(input, _cp3408) match {
    case Some((_st3409, _np3410)) => _rs3407 = _rs3407 :+ _st3409; _cp3408 = _np3410
    case None => _go3411 = false } }
  Some((_rs3407, _cp3408)) }.map { case (_r3354, _p3355) => (new ~(_r3352, _r3354), _p3355) } }.flatMap { case (_r3348, _p3349) => parseEndKeyword(input, _p3349).map { case (_r3350, _p3351) => (new ~(_r3348, _r3350), _p3351) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseIfElsifElse(input: String, pos: Int): Option[(Any, Int)] = (((if (input.startsWith("elsif", pos)) Some(("elsif", pos + 5)) else None).flatMap { case (_r3440, _p3441) => (if (parseIdentCont(input, _p3441).isEmpty) Some(((), _p3441)) else None).map { case (_r3442, _p3443) => (new ~(_r3440, _r3442), _p3443) } }.flatMap { case (_r3436, _p3437) => parseSpacing(input, _p3437).map { case (_r3438, _p3439) => (new ~(_r3436, _r3438), _p3439) } }.flatMap { case (_r3432, _p3433) => parseExpr(input, _p3433).map { case (_r3434, _p3435) => (new ~(_r3432, _r3434), _p3435) } }.flatMap { case (_r3428, _p3429) => ((if (input.startsWith("then", _p3429)) Some(("then", _p3429 + 4)) else None).flatMap { case (_r3448, _p3449) => (if (parseIdentCont(input, _p3449).isEmpty) Some(((), _p3449)) else None).map { case (_r3450, _p3451) => (new ~(_r3448, _r3450), _p3451) } }.flatMap { case (_r3444, _p3445) => parseSpacing(input, _p3445).map { case (_r3446, _p3447) => (new ~(_r3444, _r3446), _p3447) } }).orElse(parseInlineSpacing(input, _p3429).flatMap { case (_r3452, _p3453) => {
  parseStatementSep(input, _p3453) match {
    case None => None
    case Some((_fs3456, _fp3457)) =>
      var _rs3458: List[Any] = List(_fs3456); var _cp3459: Int = _fp3457; var _go3462 = true
      while (_go3462) { parseStatementSep(input, _cp3459) match {
        case Some((_st3460, _np3461)) => _rs3458 = _rs3458 :+ _st3460; _cp3459 = _np3461
        case None => _go3462 = false } }
      Some((_rs3458, _cp3459)) } }.map { case (_r3454, _p3455) => (new ~(_r3452, _r3454), _p3455) } }).map { case (_r3430, _p3431) => (new ~(_r3428, _r3430), _p3431) } }.flatMap { case (_r3424, _p3425) => {
  var _rs3463: List[Any] = Nil; var _cp3464: Int = _p3425; var _go3467 = true
  while (_go3467) { parseStatementSep(input, _cp3464) match {
    case Some((_st3465, _np3466)) => _rs3463 = _rs3463 :+ _st3465; _cp3464 = _np3466
    case None => _go3467 = false } }
  Some((_rs3463, _cp3464)) }.map { case (_r3426, _p3427) => (new ~(_r3424, _r3426), _p3427) } }.flatMap { case (_r3420, _p3421) => {
  var _rs3468: List[Any] = Nil; var _cp3469: Int = _p3421; var _go3472 = true
  while (_go3472) { (if ((((if (input.startsWith("elsif", _cp3469)) Some(("elsif", _cp3469 + 5)) else None).flatMap { case (_r3485, _p3486) => (if (parseIdentCont(input, _p3486).isEmpty) Some(((), _p3486)) else None).map { case (_r3487, _p3488) => (new ~(_r3485, _r3487), _p3488) } }.flatMap { case (_r3481, _p3482) => parseSpacing(input, _p3482).map { case (_r3483, _p3484) => (new ~(_r3481, _r3483), _p3484) } }).orElse((if (input.startsWith("else", _cp3469)) Some(("else", _cp3469 + 4)) else None).flatMap { case (_r3493, _p3494) => (if (parseIdentCont(input, _p3494).isEmpty) Some(((), _p3494)) else None).map { case (_r3495, _p3496) => (new ~(_r3493, _r3495), _p3496) } }.flatMap { case (_r3489, _p3490) => parseSpacing(input, _p3490).map { case (_r3491, _p3492) => (new ~(_r3489, _r3491), _p3492) } })).orElse(parseEndKeyword(input, _cp3469)).isEmpty) Some(((), _cp3469)) else None).flatMap { case (_r3477, _p3478) => parseStatement(input, _p3478).map { case (_r3479, _p3480) => (new ~(_r3477, _r3479), _p3480) } }.flatMap { case (_r3473, _p3474) => {
  var _rs3497: List[Any] = Nil; var _cp3498: Int = _p3474; var _go3501 = true
  while (_go3501) { parseStatementSep(input, _cp3498) match {
    case Some((_st3499, _np3500)) => _rs3497 = _rs3497 :+ _st3499; _cp3498 = _np3500
    case None => _go3501 = false } }
  Some((_rs3497, _cp3498)) }.map { case (_r3475, _p3476) => (new ~(_r3473, _r3475), _p3476) } } match {
    case Some((_st3470, _np3471)) => _rs3468 = _rs3468 :+ _st3470; _cp3469 = _np3471
    case None => _go3472 = false } }
  Some((_rs3468, _cp3469)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3422, _p3423) => (new ~(_r3420, _r3422), _p3423) } }.flatMap { case (_r3416, _p3417) => {
  var _rs3502: List[Any] = Nil; var _cp3503: Int = _p3417; var _go3506 = true
  while (_go3506) { parseStatementSep(input, _cp3503) match {
    case Some((_st3504, _np3505)) => _rs3502 = _rs3502 :+ _st3504; _cp3503 = _np3505
    case None => _go3506 = false } }
  Some((_rs3502, _cp3503)) }.map { case (_r3418, _p3419) => (new ~(_r3416, _r3418), _p3419) } }.flatMap { case (_r3412, _p3413) => parseIfElsifElse(input, _p3413).map { case (_r3414, _p3415) => (new ~(_r3412, _r3414), _p3415) } }).orElse((if (input.startsWith("else", pos)) Some(("else", pos + 4)) else None).flatMap { case (_r3523, _p3524) => (if (parseIdentCont(input, _p3524).isEmpty) Some(((), _p3524)) else None).map { case (_r3525, _p3526) => (new ~(_r3523, _r3525), _p3526) } }.flatMap { case (_r3519, _p3520) => parseSpacing(input, _p3520).map { case (_r3521, _p3522) => (new ~(_r3519, _r3521), _p3522) } }.flatMap { case (_r3515, _p3516) => {
  var _rs3527: List[Any] = Nil; var _cp3528: Int = _p3516; var _go3531 = true
  while (_go3531) { parseStatementSep(input, _cp3528) match {
    case Some((_st3529, _np3530)) => _rs3527 = _rs3527 :+ _st3529; _cp3528 = _np3530
    case None => _go3531 = false } }
  Some((_rs3527, _cp3528)) }.map { case (_r3517, _p3518) => (new ~(_r3515, _r3517), _p3518) } }.flatMap { case (_r3511, _p3512) => {
  var _rs3532: List[Any] = Nil; var _cp3533: Int = _p3512; var _go3536 = true
  while (_go3536) { (if (parseEndKeyword(input, _cp3533).isEmpty) Some(((), _cp3533)) else None).flatMap { case (_r3541, _p3542) => parseStatement(input, _p3542).map { case (_r3543, _p3544) => (new ~(_r3541, _r3543), _p3544) } }.flatMap { case (_r3537, _p3538) => {
  var _rs3545: List[Any] = Nil; var _cp3546: Int = _p3538; var _go3549 = true
  while (_go3549) { parseStatementSep(input, _cp3546) match {
    case Some((_st3547, _np3548)) => _rs3545 = _rs3545 :+ _st3547; _cp3546 = _np3548
    case None => _go3549 = false } }
  Some((_rs3545, _cp3546)) }.map { case (_r3539, _p3540) => (new ~(_r3537, _r3539), _p3540) } } match {
    case Some((_st3534, _np3535)) => _rs3532 = _rs3532 :+ _st3534; _cp3533 = _np3535
    case None => _go3536 = false } }
  Some((_rs3532, _cp3533)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3513, _p3514) => (new ~(_r3511, _r3513), _p3514) } }.flatMap { case (_r3507, _p3508) => {
  var _rs3550: List[Any] = Nil; var _cp3551: Int = _p3508; var _go3554 = true
  while (_go3554) { parseStatementSep(input, _cp3551) match {
    case Some((_st3552, _np3553)) => _rs3550 = _rs3550 :+ _st3552; _cp3551 = _np3553
    case None => _go3554 = false } }
  Some((_rs3550, _cp3551)) }.map { case (_r3509, _p3510) => (new ~(_r3507, _r3509), _p3510) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, pos)))).map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }

  def parseIfExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r3587, _p3588) => (if (parseIdentCont(input, _p3588).isEmpty) Some(((), _p3588)) else None).map { case (_r3589, _p3590) => (new ~(_r3587, _r3589), _p3590) } }.flatMap { case (_r3583, _p3584) => parseSpacing(input, _p3584).map { case (_r3585, _p3586) => (new ~(_r3583, _r3585), _p3586) } }.flatMap { case (_r3579, _p3580) => parseExpr(input, _p3580).map { case (_r3581, _p3582) => (new ~(_r3579, _r3581), _p3582) } }.flatMap { case (_r3575, _p3576) => (((if (input.startsWith("then", _p3576)) Some(("then", _p3576 + 4)) else None).flatMap { case (_r3595, _p3596) => (if (parseIdentCont(input, _p3596).isEmpty) Some(((), _p3596)) else None).map { case (_r3597, _p3598) => (new ~(_r3595, _r3597), _p3598) } }.flatMap { case (_r3591, _p3592) => parseSpacing(input, _p3592).map { case (_r3593, _p3594) => (new ~(_r3591, _r3593), _p3594) } }).orElse(parseInlineSpacing(input, _p3576).flatMap { case (_r3599, _p3600) => {
  parseStatementSep(input, _p3600) match {
    case None => None
    case Some((_fs3603, _fp3604)) =>
      var _rs3605: List[Any] = List(_fs3603); var _cp3606: Int = _fp3604; var _go3609 = true
      while (_go3609) { parseStatementSep(input, _cp3606) match {
        case Some((_st3607, _np3608)) => _rs3605 = _rs3605 :+ _st3607; _cp3606 = _np3608
        case None => _go3609 = false } }
      Some((_rs3605, _cp3606)) } }.map { case (_r3601, _p3602) => (new ~(_r3599, _r3601), _p3602) } })).orElse(parseInlineSpacing(input, _p3576).flatMap { case (_r3610, _p3611) => (if (input.startsWith(";", _p3611)) Some((";", _p3611 + 1)) else None).map { case (_r3612, _p3613) => (new ~(_r3610, _r3612), _p3613) } }).map { case (_r3577, _p3578) => (new ~(_r3575, _r3577), _p3578) } }.flatMap { case (_r3571, _p3572) => {
  var _rs3614: List[Any] = Nil; var _cp3615: Int = _p3572; var _go3618 = true
  while (_go3618) { parseStatementSep(input, _cp3615) match {
    case Some((_st3616, _np3617)) => _rs3614 = _rs3614 :+ _st3616; _cp3615 = _np3617
    case None => _go3618 = false } }
  Some((_rs3614, _cp3615)) }.map { case (_r3573, _p3574) => (new ~(_r3571, _r3573), _p3574) } }.flatMap { case (_r3567, _p3568) => {
  var _rs3619: List[Any] = Nil; var _cp3620: Int = _p3568; var _go3623 = true
  while (_go3623) { (if ((((if (input.startsWith("elsif", _cp3620)) Some(("elsif", _cp3620 + 5)) else None).flatMap { case (_r3636, _p3637) => (if (parseIdentCont(input, _p3637).isEmpty) Some(((), _p3637)) else None).map { case (_r3638, _p3639) => (new ~(_r3636, _r3638), _p3639) } }.flatMap { case (_r3632, _p3633) => parseSpacing(input, _p3633).map { case (_r3634, _p3635) => (new ~(_r3632, _r3634), _p3635) } }).orElse((if (input.startsWith("else", _cp3620)) Some(("else", _cp3620 + 4)) else None).flatMap { case (_r3644, _p3645) => (if (parseIdentCont(input, _p3645).isEmpty) Some(((), _p3645)) else None).map { case (_r3646, _p3647) => (new ~(_r3644, _r3646), _p3647) } }.flatMap { case (_r3640, _p3641) => parseSpacing(input, _p3641).map { case (_r3642, _p3643) => (new ~(_r3640, _r3642), _p3643) } })).orElse(parseEndKeyword(input, _cp3620)).isEmpty) Some(((), _cp3620)) else None).flatMap { case (_r3628, _p3629) => parseStatement(input, _p3629).map { case (_r3630, _p3631) => (new ~(_r3628, _r3630), _p3631) } }.flatMap { case (_r3624, _p3625) => {
  var _rs3648: List[Any] = Nil; var _cp3649: Int = _p3625; var _go3652 = true
  while (_go3652) { parseStatementSep(input, _cp3649) match {
    case Some((_st3650, _np3651)) => _rs3648 = _rs3648 :+ _st3650; _cp3649 = _np3651
    case None => _go3652 = false } }
  Some((_rs3648, _cp3649)) }.map { case (_r3626, _p3627) => (new ~(_r3624, _r3626), _p3627) } } match {
    case Some((_st3621, _np3622)) => _rs3619 = _rs3619 :+ _st3621; _cp3620 = _np3622
    case None => _go3623 = false } }
  Some((_rs3619, _cp3620)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3569, _p3570) => (new ~(_r3567, _r3569), _p3570) } }.flatMap { case (_r3563, _p3564) => {
  var _rs3653: List[Any] = Nil; var _cp3654: Int = _p3564; var _go3657 = true
  while (_go3657) { parseStatementSep(input, _cp3654) match {
    case Some((_st3655, _np3656)) => _rs3653 = _rs3653 :+ _st3655; _cp3654 = _np3656
    case None => _go3657 = false } }
  Some((_rs3653, _cp3654)) }.map { case (_r3565, _p3566) => (new ~(_r3563, _r3565), _p3566) } }.flatMap { case (_r3559, _p3560) => parseIfElsifElse(input, _p3560).map { case (_r3561, _p3562) => (new ~(_r3559, _r3561), _p3562) } }.flatMap { case (_r3555, _p3556) => parseEndKeyword(input, _p3556).map { case (_r3557, _p3558) => (new ~(_r3555, _r3557), _p3558) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseUnlessExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r3690, _p3691) => (if (parseIdentCont(input, _p3691).isEmpty) Some(((), _p3691)) else None).map { case (_r3692, _p3693) => (new ~(_r3690, _r3692), _p3693) } }.flatMap { case (_r3686, _p3687) => parseSpacing(input, _p3687).map { case (_r3688, _p3689) => (new ~(_r3686, _r3688), _p3689) } }.flatMap { case (_r3682, _p3683) => parseExpr(input, _p3683).map { case (_r3684, _p3685) => (new ~(_r3682, _r3684), _p3685) } }.flatMap { case (_r3678, _p3679) => (((if (input.startsWith("then", _p3679)) Some(("then", _p3679 + 4)) else None).flatMap { case (_r3698, _p3699) => (if (parseIdentCont(input, _p3699).isEmpty) Some(((), _p3699)) else None).map { case (_r3700, _p3701) => (new ~(_r3698, _r3700), _p3701) } }.flatMap { case (_r3694, _p3695) => parseSpacing(input, _p3695).map { case (_r3696, _p3697) => (new ~(_r3694, _r3696), _p3697) } }).orElse(parseInlineSpacing(input, _p3679).flatMap { case (_r3702, _p3703) => {
  parseStatementSep(input, _p3703) match {
    case None => None
    case Some((_fs3706, _fp3707)) =>
      var _rs3708: List[Any] = List(_fs3706); var _cp3709: Int = _fp3707; var _go3712 = true
      while (_go3712) { parseStatementSep(input, _cp3709) match {
        case Some((_st3710, _np3711)) => _rs3708 = _rs3708 :+ _st3710; _cp3709 = _np3711
        case None => _go3712 = false } }
      Some((_rs3708, _cp3709)) } }.map { case (_r3704, _p3705) => (new ~(_r3702, _r3704), _p3705) } })).orElse(parseInlineSpacing(input, _p3679).flatMap { case (_r3713, _p3714) => (if (input.startsWith(";", _p3714)) Some((";", _p3714 + 1)) else None).map { case (_r3715, _p3716) => (new ~(_r3713, _r3715), _p3716) } }).map { case (_r3680, _p3681) => (new ~(_r3678, _r3680), _p3681) } }.flatMap { case (_r3674, _p3675) => {
  var _rs3717: List[Any] = Nil; var _cp3718: Int = _p3675; var _go3721 = true
  while (_go3721) { parseStatementSep(input, _cp3718) match {
    case Some((_st3719, _np3720)) => _rs3717 = _rs3717 :+ _st3719; _cp3718 = _np3720
    case None => _go3721 = false } }
  Some((_rs3717, _cp3718)) }.map { case (_r3676, _p3677) => (new ~(_r3674, _r3676), _p3677) } }.flatMap { case (_r3670, _p3671) => {
  var _rs3722: List[Any] = Nil; var _cp3723: Int = _p3671; var _go3726 = true
  while (_go3726) { (if (((if (input.startsWith("else", _cp3723)) Some(("else", _cp3723 + 4)) else None).flatMap { case (_r3739, _p3740) => (if (parseIdentCont(input, _p3740).isEmpty) Some(((), _p3740)) else None).map { case (_r3741, _p3742) => (new ~(_r3739, _r3741), _p3742) } }.flatMap { case (_r3735, _p3736) => parseSpacing(input, _p3736).map { case (_r3737, _p3738) => (new ~(_r3735, _r3737), _p3738) } }).orElse(parseEndKeyword(input, _cp3723)).isEmpty) Some(((), _cp3723)) else None).flatMap { case (_r3731, _p3732) => parseStatement(input, _p3732).map { case (_r3733, _p3734) => (new ~(_r3731, _r3733), _p3734) } }.flatMap { case (_r3727, _p3728) => {
  var _rs3743: List[Any] = Nil; var _cp3744: Int = _p3728; var _go3747 = true
  while (_go3747) { parseStatementSep(input, _cp3744) match {
    case Some((_st3745, _np3746)) => _rs3743 = _rs3743 :+ _st3745; _cp3744 = _np3746
    case None => _go3747 = false } }
  Some((_rs3743, _cp3744)) }.map { case (_r3729, _p3730) => (new ~(_r3727, _r3729), _p3730) } } match {
    case Some((_st3724, _np3725)) => _rs3722 = _rs3722 :+ _st3724; _cp3723 = _np3725
    case None => _go3726 = false } }
  Some((_rs3722, _cp3723)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3672, _p3673) => (new ~(_r3670, _r3672), _p3673) } }.flatMap { case (_r3666, _p3667) => {
  var _rs3748: List[Any] = Nil; var _cp3749: Int = _p3667; var _go3752 = true
  while (_go3752) { parseStatementSep(input, _cp3749) match {
    case Some((_st3750, _np3751)) => _rs3748 = _rs3748 :+ _st3750; _cp3749 = _np3751
    case None => _go3752 = false } }
  Some((_rs3748, _cp3749)) }.map { case (_r3668, _p3669) => (new ~(_r3666, _r3668), _p3669) } }.flatMap { case (_r3662, _p3663) => ((if (input.startsWith("else", _p3663)) Some(("else", _p3663 + 4)) else None).flatMap { case (_r3769, _p3770) => (if (parseIdentCont(input, _p3770).isEmpty) Some(((), _p3770)) else None).map { case (_r3771, _p3772) => (new ~(_r3769, _r3771), _p3772) } }.flatMap { case (_r3765, _p3766) => parseSpacing(input, _p3766).map { case (_r3767, _p3768) => (new ~(_r3765, _r3767), _p3768) } }.flatMap { case (_r3761, _p3762) => {
  var _rs3773: List[Any] = Nil; var _cp3774: Int = _p3762; var _go3777 = true
  while (_go3777) { parseStatementSep(input, _cp3774) match {
    case Some((_st3775, _np3776)) => _rs3773 = _rs3773 :+ _st3775; _cp3774 = _np3776
    case None => _go3777 = false } }
  Some((_rs3773, _cp3774)) }.map { case (_r3763, _p3764) => (new ~(_r3761, _r3763), _p3764) } }.flatMap { case (_r3757, _p3758) => {
  var _rs3778: List[Any] = Nil; var _cp3779: Int = _p3758; var _go3782 = true
  while (_go3782) { (if (parseEndKeyword(input, _cp3779).isEmpty) Some(((), _cp3779)) else None).flatMap { case (_r3787, _p3788) => parseStatement(input, _p3788).map { case (_r3789, _p3790) => (new ~(_r3787, _r3789), _p3790) } }.flatMap { case (_r3783, _p3784) => {
  var _rs3791: List[Any] = Nil; var _cp3792: Int = _p3784; var _go3795 = true
  while (_go3795) { parseStatementSep(input, _cp3792) match {
    case Some((_st3793, _np3794)) => _rs3791 = _rs3791 :+ _st3793; _cp3792 = _np3794
    case None => _go3795 = false } }
  Some((_rs3791, _cp3792)) }.map { case (_r3785, _p3786) => (new ~(_r3783, _r3785), _p3786) } } match {
    case Some((_st3780, _np3781)) => _rs3778 = _rs3778 :+ _st3780; _cp3779 = _np3781
    case None => _go3782 = false } }
  Some((_rs3778, _cp3779)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3759, _p3760) => (new ~(_r3757, _r3759), _p3760) } }.flatMap { case (_r3753, _p3754) => {
  var _rs3796: List[Any] = Nil; var _cp3797: Int = _p3754; var _go3800 = true
  while (_go3800) { parseStatementSep(input, _cp3797) match {
    case Some((_st3798, _np3799)) => _rs3796 = _rs3796 :+ _st3798; _cp3797 = _np3799
    case None => _go3800 = false } }
  Some((_rs3796, _cp3797)) }.map { case (_r3755, _p3756) => (new ~(_r3753, _r3755), _p3756) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3663)))).map { case (_r3664, _p3665) => (new ~(_r3662, _r3664), _p3665) } }.flatMap { case (_r3658, _p3659) => parseEndKeyword(input, _p3659).map { case (_r3660, _p3661) => (new ~(_r3658, _r3660), _p3661) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseWhenClause(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("when", pos)) Some(("when", pos + 4)) else None).flatMap { case (_r3829, _p3830) => (if (parseIdentCont(input, _p3830).isEmpty) Some(((), _p3830)) else None).map { case (_r3831, _p3832) => (new ~(_r3829, _r3831), _p3832) } }).orElse((if (input.startsWith("in", pos)) Some(("in", pos + 2)) else None).flatMap { case (_r3833, _p3834) => (if (parseIdentCont(input, _p3834).isEmpty) Some(((), _p3834)) else None).map { case (_r3835, _p3836) => (new ~(_r3833, _r3835), _p3836) } }).flatMap { case (_r3825, _p3826) => parseSpacing(input, _p3826).map { case (_r3827, _p3828) => (new ~(_r3825, _r3827), _p3828) } }.flatMap { case (_r3821, _p3822) => parseInPatternExpr(input, _p3822).map { case (_r3823, _p3824) => (new ~(_r3821, _r3823), _p3824) } }.flatMap { case (_r3817, _p3818) => {
  var _rs3837: List[Any] = Nil; var _cp3838: Int = _p3818; var _go3841 = true
  while (_go3841) { parseSpacing(input, _cp3838).flatMap { case (_r3850, _p3851) => (if (input.startsWith(",", _p3851)) Some((",", _p3851 + 1)) else None).map { case (_r3852, _p3853) => (new ~(_r3850, _r3852), _p3853) } }.flatMap { case (_r3846, _p3847) => parseSpacing(input, _p3847).map { case (_r3848, _p3849) => (new ~(_r3846, _r3848), _p3849) } }.flatMap { case (_r3842, _p3843) => parseInPatternExpr(input, _p3843).map { case (_r3844, _p3845) => (new ~(_r3842, _r3844), _p3845) } } match {
    case Some((_st3839, _np3840)) => _rs3837 = _rs3837 :+ _st3839; _cp3838 = _np3840
    case None => _go3841 = false } }
  Some((_rs3837, _cp3838)) }.map { case (_r3819, _p3820) => (new ~(_r3817, _r3819), _p3820) } }.flatMap { case (_r3813, _p3814) => (parseInlineSpacing(input, _p3814).flatMap { case (_r3858, _p3859) => ((if (input.startsWith(";", _p3859)) Some((";", _p3859 + 1)) else None)).orElse((if (input.startsWith("then", _p3859)) Some(("then", _p3859 + 4)) else None).flatMap { case (_r3862, _p3863) => (if (parseIdentCont(input, _p3863).isEmpty) Some(((), _p3863)) else None).map { case (_r3864, _p3865) => (new ~(_r3862, _r3864), _p3865) } }).map { case (_r3860, _p3861) => (new ~(_r3858, _r3860), _p3861) } }.flatMap { case (_r3854, _p3855) => parseSpacing(input, _p3855).map { case (_r3856, _p3857) => (new ~(_r3854, _r3856), _p3857) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3814)))).map { case (_r3815, _p3816) => (new ~(_r3813, _r3815), _p3816) } }.flatMap { case (_r3809, _p3810) => {
  var _rs3866: List[Any] = Nil; var _cp3867: Int = _p3810; var _go3870 = true
  while (_go3870) { parseStatementSep(input, _cp3867) match {
    case Some((_st3868, _np3869)) => _rs3866 = _rs3866 :+ _st3868; _cp3867 = _np3869
    case None => _go3870 = false } }
  Some((_rs3866, _cp3867)) }.map { case (_r3811, _p3812) => (new ~(_r3809, _r3811), _p3812) } }.flatMap { case (_r3805, _p3806) => {
  var _rs3871: List[Any] = Nil; var _cp3872: Int = _p3806; var _go3875 = true
  while (_go3875) { (if (((((if (input.startsWith("when", _cp3872)) Some(("when", _cp3872 + 4)) else None).flatMap { case (_r3888, _p3889) => (if (parseIdentCont(input, _p3889).isEmpty) Some(((), _p3889)) else None).map { case (_r3890, _p3891) => (new ~(_r3888, _r3890), _p3891) } }.flatMap { case (_r3884, _p3885) => parseSpacing(input, _p3885).map { case (_r3886, _p3887) => (new ~(_r3884, _r3886), _p3887) } }).orElse((if (input.startsWith("in", _cp3872)) Some(("in", _cp3872 + 2)) else None).flatMap { case (_r3896, _p3897) => (if (parseIdentCont(input, _p3897).isEmpty) Some(((), _p3897)) else None).map { case (_r3898, _p3899) => (new ~(_r3896, _r3898), _p3899) } }.flatMap { case (_r3892, _p3893) => parseSpacing(input, _p3893).map { case (_r3894, _p3895) => (new ~(_r3892, _r3894), _p3895) } })).orElse((if (input.startsWith("else", _cp3872)) Some(("else", _cp3872 + 4)) else None).flatMap { case (_r3904, _p3905) => (if (parseIdentCont(input, _p3905).isEmpty) Some(((), _p3905)) else None).map { case (_r3906, _p3907) => (new ~(_r3904, _r3906), _p3907) } }.flatMap { case (_r3900, _p3901) => parseSpacing(input, _p3901).map { case (_r3902, _p3903) => (new ~(_r3900, _r3902), _p3903) } })).orElse(parseEndKeyword(input, _cp3872)).isEmpty) Some(((), _cp3872)) else None).flatMap { case (_r3880, _p3881) => parseStatement(input, _p3881).map { case (_r3882, _p3883) => (new ~(_r3880, _r3882), _p3883) } }.flatMap { case (_r3876, _p3877) => {
  var _rs3908: List[Any] = Nil; var _cp3909: Int = _p3877; var _go3912 = true
  while (_go3912) { parseStatementSep(input, _cp3909) match {
    case Some((_st3910, _np3911)) => _rs3908 = _rs3908 :+ _st3910; _cp3909 = _np3911
    case None => _go3912 = false } }
  Some((_rs3908, _cp3909)) }.map { case (_r3878, _p3879) => (new ~(_r3876, _r3878), _p3879) } } match {
    case Some((_st3873, _np3874)) => _rs3871 = _rs3871 :+ _st3873; _cp3872 = _np3874
    case None => _go3875 = false } }
  Some((_rs3871, _cp3872)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3807, _p3808) => (new ~(_r3805, _r3807), _p3808) } }.flatMap { case (_r3801, _p3802) => {
  var _rs3913: List[Any] = Nil; var _cp3914: Int = _p3802; var _go3917 = true
  while (_go3917) { parseStatementSep(input, _cp3914) match {
    case Some((_st3915, _np3916)) => _rs3913 = _rs3913 :+ _st3915; _cp3914 = _np3916
    case None => _go3917 = false } }
  Some((_rs3913, _cp3914)) }.map { case (_r3803, _p3804) => (new ~(_r3801, _r3803), _p3804) } }.map { case (r, p) => (_applyAction({  _ => WhenClause(List.empty, List.empty)  }, r), p) }

  def parseCaseExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("case", pos)) Some(("case", pos + 4)) else None).flatMap { case (_r3942, _p3943) => (if (parseIdentCont(input, _p3943).isEmpty) Some(((), _p3943)) else None).map { case (_r3944, _p3945) => (new ~(_r3942, _r3944), _p3945) } }.flatMap { case (_r3938, _p3939) => parseSpacing(input, _p3939).map { case (_r3940, _p3941) => (new ~(_r3938, _r3940), _p3941) } }.flatMap { case (_r3934, _p3935) => (parseExpr(input, _p3935).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3935)))).map { case (_r3936, _p3937) => (new ~(_r3934, _r3936), _p3937) } }.flatMap { case (_r3930, _p3931) => {
  var _rs3946: List[Any] = Nil; var _cp3947: Int = _p3931; var _go3950 = true
  while (_go3950) { parseStatementSep(input, _cp3947) match {
    case Some((_st3948, _np3949)) => _rs3946 = _rs3946 :+ _st3948; _cp3947 = _np3949
    case None => _go3950 = false } }
  Some((_rs3946, _cp3947)) }.map { case (_r3932, _p3933) => (new ~(_r3930, _r3932), _p3933) } }.flatMap { case (_r3926, _p3927) => {
  parseWhenClause(input, _p3927) match {
    case None => None
    case Some((_fs3951, _fp3952)) =>
      var _rs3953: List[Any] = List(_fs3951); var _cp3954: Int = _fp3952; var _go3957 = true
      while (_go3957) { parseWhenClause(input, _cp3954) match {
        case Some((_st3955, _np3956)) => _rs3953 = _rs3953 :+ _st3955; _cp3954 = _np3956
        case None => _go3957 = false } }
      Some((_rs3953, _cp3954)) } }.map { case (_r3928, _p3929) => (new ~(_r3926, _r3928), _p3929) } }.flatMap { case (_r3922, _p3923) => ((if (input.startsWith("else", _p3923)) Some(("else", _p3923 + 4)) else None).flatMap { case (_r3974, _p3975) => (if (parseIdentCont(input, _p3975).isEmpty) Some(((), _p3975)) else None).map { case (_r3976, _p3977) => (new ~(_r3974, _r3976), _p3977) } }.flatMap { case (_r3970, _p3971) => parseSpacing(input, _p3971).map { case (_r3972, _p3973) => (new ~(_r3970, _r3972), _p3973) } }.flatMap { case (_r3966, _p3967) => {
  var _rs3978: List[Any] = Nil; var _cp3979: Int = _p3967; var _go3982 = true
  while (_go3982) { parseStatementSep(input, _cp3979) match {
    case Some((_st3980, _np3981)) => _rs3978 = _rs3978 :+ _st3980; _cp3979 = _np3981
    case None => _go3982 = false } }
  Some((_rs3978, _cp3979)) }.map { case (_r3968, _p3969) => (new ~(_r3966, _r3968), _p3969) } }.flatMap { case (_r3962, _p3963) => {
  var _rs3983: List[Any] = Nil; var _cp3984: Int = _p3963; var _go3987 = true
  while (_go3987) { (if (parseEndKeyword(input, _cp3984).isEmpty) Some(((), _cp3984)) else None).flatMap { case (_r3992, _p3993) => parseStatement(input, _p3993).map { case (_r3994, _p3995) => (new ~(_r3992, _r3994), _p3995) } }.flatMap { case (_r3988, _p3989) => {
  var _rs3996: List[Any] = Nil; var _cp3997: Int = _p3989; var _go4000 = true
  while (_go4000) { parseStatementSep(input, _cp3997) match {
    case Some((_st3998, _np3999)) => _rs3996 = _rs3996 :+ _st3998; _cp3997 = _np3999
    case None => _go4000 = false } }
  Some((_rs3996, _cp3997)) }.map { case (_r3990, _p3991) => (new ~(_r3988, _r3990), _p3991) } } match {
    case Some((_st3985, _np3986)) => _rs3983 = _rs3983 :+ _st3985; _cp3984 = _np3986
    case None => _go3987 = false } }
  Some((_rs3983, _cp3984)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r3964, _p3965) => (new ~(_r3962, _r3964), _p3965) } }.flatMap { case (_r3958, _p3959) => {
  var _rs4001: List[Any] = Nil; var _cp4002: Int = _p3959; var _go4005 = true
  while (_go4005) { parseStatementSep(input, _cp4002) match {
    case Some((_st4003, _np4004)) => _rs4001 = _rs4001 :+ _st4003; _cp4002 = _np4004
    case None => _go4005 = false } }
  Some((_rs4001, _cp4002)) }.map { case (_r3960, _p3961) => (new ~(_r3958, _r3960), _p3961) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3923)))).map { case (_r3924, _p3925) => (new ~(_r3922, _r3924), _p3925) } }.flatMap { case (_r3918, _p3919) => parseEndKeyword(input, _p3919).map { case (_r3920, _p3921) => (new ~(_r3918, _r3920), _p3921) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseModifierSuffix(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r4014, _p4015) => (if (parseIdentCont(input, _p4015).isEmpty) Some(((), _p4015)) else None).map { case (_r4016, _p4017) => (new ~(_r4014, _r4016), _p4017) } }.flatMap { case (_r4010, _p4011) => parseSpacing(input, _p4011).map { case (_r4012, _p4013) => (new ~(_r4010, _r4012), _p4013) } }.flatMap { case (_r4006, _p4007) => parseExpr(input, _p4007).map { case (_r4008, _p4009) => (new ~(_r4006, _r4008), _p4009) } }).orElse((if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r4026, _p4027) => (if (parseIdentCont(input, _p4027).isEmpty) Some(((), _p4027)) else None).map { case (_r4028, _p4029) => (new ~(_r4026, _r4028), _p4029) } }.flatMap { case (_r4022, _p4023) => parseSpacing(input, _p4023).map { case (_r4024, _p4025) => (new ~(_r4022, _r4024), _p4025) } }.flatMap { case (_r4018, _p4019) => parseExpr(input, _p4019).map { case (_r4020, _p4021) => (new ~(_r4018, _r4020), _p4021) } })).orElse((if (input.startsWith("while", pos)) Some(("while", pos + 5)) else None).flatMap { case (_r4038, _p4039) => (if (parseIdentCont(input, _p4039).isEmpty) Some(((), _p4039)) else None).map { case (_r4040, _p4041) => (new ~(_r4038, _r4040), _p4041) } }.flatMap { case (_r4034, _p4035) => parseSpacing(input, _p4035).map { case (_r4036, _p4037) => (new ~(_r4034, _r4036), _p4037) } }.flatMap { case (_r4030, _p4031) => parseExpr(input, _p4031).map { case (_r4032, _p4033) => (new ~(_r4030, _r4032), _p4033) } })).orElse((if (input.startsWith("until", pos)) Some(("until", pos + 5)) else None).flatMap { case (_r4050, _p4051) => (if (parseIdentCont(input, _p4051).isEmpty) Some(((), _p4051)) else None).map { case (_r4052, _p4053) => (new ~(_r4050, _r4052), _p4053) } }.flatMap { case (_r4046, _p4047) => parseSpacing(input, _p4047).map { case (_r4048, _p4049) => (new ~(_r4046, _r4048), _p4049) } }.flatMap { case (_r4042, _p4043) => parseExpr(input, _p4043).map { case (_r4044, _p4045) => (new ~(_r4042, _r4044), _p4045) } })).orElse((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r4062, _p4063) => (if (parseIdentCont(input, _p4063).isEmpty) Some(((), _p4063)) else None).map { case (_r4064, _p4065) => (new ~(_r4062, _r4064), _p4065) } }.flatMap { case (_r4058, _p4059) => parseSpacing(input, _p4059).map { case (_r4060, _p4061) => (new ~(_r4058, _r4060), _p4061) } }.flatMap { case (_r4054, _p4055) => parseExpr(input, _p4055).map { case (_r4056, _p4057) => (new ~(_r4054, _r4056), _p4057) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseReturnStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("return", pos)) Some(("return", pos + 6)) else None).flatMap { case (_r4074, _p4075) => (if (parseIdentCont(input, _p4075).isEmpty) Some(((), _p4075)) else None).map { case (_r4076, _p4077) => (new ~(_r4074, _r4076), _p4077) } }.flatMap { case (_r4070, _p4071) => parseInlineSpacing(input, _p4071).map { case (_r4072, _p4073) => (new ~(_r4070, _r4072), _p4073) } }.flatMap { case (_r4066, _p4067) => ((if (parseCommandArgStop(input, _p4067).isEmpty) Some(((), _p4067)) else None).flatMap { case (_r4082, _p4083) => parseExpr(input, _p4083).map { case (_r4084, _p4085) => (new ~(_r4082, _r4084), _p4085) } }.flatMap { case (_r4078, _p4079) => {
  var _rs4086: List[Any] = Nil; var _cp4087: Int = _p4079; var _go4090 = true
  while (_go4090) { parseSpacing(input, _cp4087).flatMap { case (_r4099, _p4100) => (if (input.startsWith(",", _p4100)) Some((",", _p4100 + 1)) else None).map { case (_r4101, _p4102) => (new ~(_r4099, _r4101), _p4102) } }.flatMap { case (_r4095, _p4096) => parseSpacing(input, _p4096).map { case (_r4097, _p4098) => (new ~(_r4095, _r4097), _p4098) } }.flatMap { case (_r4091, _p4092) => parseExpr(input, _p4092).map { case (_r4093, _p4094) => (new ~(_r4091, _r4093), _p4094) } } match {
    case Some((_st4088, _np4089)) => _rs4086 = _rs4086 :+ _st4088; _cp4087 = _np4089
    case None => _go4090 = false } }
  Some((_rs4086, _cp4087)) }.map { case (_r4080, _p4081) => (new ~(_r4078, _r4080), _p4081) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4067)))).map { case (_r4068, _p4069) => (new ~(_r4066, _r4068), _p4069) } }.map { case (r, p) => (_applyAction({  _ => Return(None)  }, r), p) }

  def parseRetryStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("retry", pos)) Some(("retry", pos + 5)) else None).flatMap { case (_r4107, _p4108) => (if (parseIdentCont(input, _p4108).isEmpty) Some(((), _p4108)) else None).map { case (_r4109, _p4110) => (new ~(_r4107, _r4109), _p4110) } }.flatMap { case (_r4103, _p4104) => parseInlineSpacing(input, _p4104).map { case (_r4105, _p4106) => (new ~(_r4103, _r4105), _p4106) } }.map { case (r, p) => (_applyAction({  _ => Retry()  }, r), p) }

  def parseBreakStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("break", pos)) Some(("break", pos + 5)) else None).flatMap { case (_r4119, _p4120) => (if (parseIdentCont(input, _p4120).isEmpty) Some(((), _p4120)) else None).map { case (_r4121, _p4122) => (new ~(_r4119, _r4121), _p4122) } }.flatMap { case (_r4115, _p4116) => parseInlineSpacing(input, _p4116).map { case (_r4117, _p4118) => (new ~(_r4115, _r4117), _p4118) } }.flatMap { case (_r4111, _p4112) => ((if (parseCommandArgStop(input, _p4112).isEmpty) Some(((), _p4112)) else None).flatMap { case (_r4127, _p4128) => parseExpr(input, _p4128).map { case (_r4129, _p4130) => (new ~(_r4127, _r4129), _p4130) } }.flatMap { case (_r4123, _p4124) => {
  var _rs4131: List[Any] = Nil; var _cp4132: Int = _p4124; var _go4135 = true
  while (_go4135) { parseSpacing(input, _cp4132).flatMap { case (_r4144, _p4145) => (if (input.startsWith(",", _p4145)) Some((",", _p4145 + 1)) else None).map { case (_r4146, _p4147) => (new ~(_r4144, _r4146), _p4147) } }.flatMap { case (_r4140, _p4141) => parseSpacing(input, _p4141).map { case (_r4142, _p4143) => (new ~(_r4140, _r4142), _p4143) } }.flatMap { case (_r4136, _p4137) => parseExpr(input, _p4137).map { case (_r4138, _p4139) => (new ~(_r4136, _r4138), _p4139) } } match {
    case Some((_st4133, _np4134)) => _rs4131 = _rs4131 :+ _st4133; _cp4132 = _np4134
    case None => _go4135 = false } }
  Some((_rs4131, _cp4132)) }.map { case (_r4125, _p4126) => (new ~(_r4123, _r4125), _p4126) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4112)))).map { case (_r4113, _p4114) => (new ~(_r4111, _r4113), _p4114) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseNextStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("next", pos)) Some(("next", pos + 4)) else None).flatMap { case (_r4156, _p4157) => (if (parseIdentCont(input, _p4157).isEmpty) Some(((), _p4157)) else None).map { case (_r4158, _p4159) => (new ~(_r4156, _r4158), _p4159) } }.flatMap { case (_r4152, _p4153) => parseInlineSpacing(input, _p4153).map { case (_r4154, _p4155) => (new ~(_r4152, _r4154), _p4155) } }.flatMap { case (_r4148, _p4149) => ((if (parseCommandArgStop(input, _p4149).isEmpty) Some(((), _p4149)) else None).flatMap { case (_r4164, _p4165) => parseExpr(input, _p4165).map { case (_r4166, _p4167) => (new ~(_r4164, _r4166), _p4167) } }.flatMap { case (_r4160, _p4161) => {
  var _rs4168: List[Any] = Nil; var _cp4169: Int = _p4161; var _go4172 = true
  while (_go4172) { parseSpacing(input, _cp4169).flatMap { case (_r4181, _p4182) => (if (input.startsWith(",", _p4182)) Some((",", _p4182 + 1)) else None).map { case (_r4183, _p4184) => (new ~(_r4181, _r4183), _p4184) } }.flatMap { case (_r4177, _p4178) => parseSpacing(input, _p4178).map { case (_r4179, _p4180) => (new ~(_r4177, _r4179), _p4180) } }.flatMap { case (_r4173, _p4174) => parseExpr(input, _p4174).map { case (_r4175, _p4176) => (new ~(_r4173, _r4175), _p4176) } } match {
    case Some((_st4170, _np4171)) => _rs4168 = _rs4168 :+ _st4170; _cp4169 = _np4171
    case None => _go4172 = false } }
  Some((_rs4168, _cp4169)) }.map { case (_r4162, _p4163) => (new ~(_r4160, _r4162), _p4163) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4149)))).map { case (_r4150, _p4151) => (new ~(_r4148, _r4150), _p4151) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseYieldStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("yield", pos)) Some(("yield", pos + 5)) else None).flatMap { case (_r4193, _p4194) => (if (parseIdentCont(input, _p4194).isEmpty) Some(((), _p4194)) else None).map { case (_r4195, _p4196) => (new ~(_r4193, _r4195), _p4196) } }.flatMap { case (_r4189, _p4190) => parseInlineSpacing(input, _p4190).map { case (_r4191, _p4192) => (new ~(_r4189, _r4191), _p4192) } }.flatMap { case (_r4185, _p4186) => (parseCallArgs(input, _p4186).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4186)))).map { case (_r4187, _p4188) => (new ~(_r4185, _r4187), _p4188) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseAliasNamePart(input: String, pos: Int): Option[(Any, Int)] = (((parseSymbolLiteral(input, pos)).orElse(parseSymbolOperatorName(input, pos).flatMap { case (_r4197, _p4198) => parseInlineSpacing(input, _p4198).map { case (_r4199, _p4200) => (new ~(_r4197, _r4199), _p4200) } })).orElse(parseMethodIdentifierRaw(input, pos).flatMap { case (_r4201, _p4202) => parseInlineSpacing(input, _p4202).map { case (_r4203, _p4204) => (new ~(_r4201, _r4203), _p4204) } })).orElse(parseGlobalVarName(input, pos)).map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parseAliasStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("alias", pos)) Some(("alias", pos + 5)) else None).flatMap { case (_r4221, _p4222) => (if (parseIdentCont(input, _p4222).isEmpty) Some(((), _p4222)) else None).map { case (_r4223, _p4224) => (new ~(_r4221, _r4223), _p4224) } }.flatMap { case (_r4217, _p4218) => parseSpacing(input, _p4218).map { case (_r4219, _p4220) => (new ~(_r4217, _r4219), _p4220) } }.flatMap { case (_r4213, _p4214) => parseAliasNamePart(input, _p4214).map { case (_r4215, _p4216) => (new ~(_r4213, _r4215), _p4216) } }.flatMap { case (_r4209, _p4210) => parseSpacing(input, _p4210).map { case (_r4211, _p4212) => (new ~(_r4209, _r4211), _p4212) } }.flatMap { case (_r4205, _p4206) => parseAliasNamePart(input, _p4206).map { case (_r4207, _p4208) => (new ~(_r4205, _r4207), _p4208) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseDefReceiverPart(input: String, pos: Int): Option[(Any, Int)] = ((((((((((if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r4237, _p4238) => parseSpacing(input, _p4238).map { case (_r4239, _p4240) => (new ~(_r4237, _r4239), _p4240) } }.flatMap { case (_r4233, _p4234) => parseExpr(input, _p4234).map { case (_r4235, _p4236) => (new ~(_r4233, _r4235), _p4236) } }.flatMap { case (_r4229, _p4230) => parseSpacing(input, _p4230).map { case (_r4231, _p4232) => (new ~(_r4229, _r4231), _p4232) } }.flatMap { case (_r4225, _p4226) => (if (input.startsWith(")", _p4226)) Some((")", _p4226 + 1)) else None).map { case (_r4227, _p4228) => (new ~(_r4225, _r4227), _p4228) } }).orElse(parseConstPathNoSpace(input, pos).flatMap { case (_r4241, _p4242) => parseInlineSpacing(input, _p4242).map { case (_r4243, _p4244) => (new ~(_r4241, _r4243), _p4244) } })).orElse((if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r4249, _p4250) => (if (parseIdentCont(input, _p4250).isEmpty) Some(((), _p4250)) else None).map { case (_r4251, _p4252) => (new ~(_r4249, _r4251), _p4252) } }.flatMap { case (_r4245, _p4246) => parseInlineSpacing(input, _p4246).map { case (_r4247, _p4248) => (new ~(_r4245, _r4247), _p4248) } })).orElse((if (input.startsWith("nil", pos)) Some(("nil", pos + 3)) else None).flatMap { case (_r4257, _p4258) => (if (parseIdentCont(input, _p4258).isEmpty) Some(((), _p4258)) else None).map { case (_r4259, _p4260) => (new ~(_r4257, _r4259), _p4260) } }.flatMap { case (_r4253, _p4254) => parseInlineSpacing(input, _p4254).map { case (_r4255, _p4256) => (new ~(_r4253, _r4255), _p4256) } })).orElse((if (input.startsWith("true", pos)) Some(("true", pos + 4)) else None).flatMap { case (_r4265, _p4266) => (if (parseIdentCont(input, _p4266).isEmpty) Some(((), _p4266)) else None).map { case (_r4267, _p4268) => (new ~(_r4265, _r4267), _p4268) } }.flatMap { case (_r4261, _p4262) => parseInlineSpacing(input, _p4262).map { case (_r4263, _p4264) => (new ~(_r4261, _r4263), _p4264) } })).orElse((if (input.startsWith("false", pos)) Some(("false", pos + 5)) else None).flatMap { case (_r4273, _p4274) => (if (parseIdentCont(input, _p4274).isEmpty) Some(((), _p4274)) else None).map { case (_r4275, _p4276) => (new ~(_r4273, _r4275), _p4276) } }.flatMap { case (_r4269, _p4270) => parseInlineSpacing(input, _p4270).map { case (_r4271, _p4272) => (new ~(_r4269, _r4271), _p4272) } })).orElse(parseInstanceVarName(input, pos))).orElse(parseClassVarName(input, pos))).orElse(parseGlobalVarName(input, pos))).orElse(parseIdentifier(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDefMethodNamePart(input: String, pos: Int): Option[(Any, Int)] = (((parseSymbolOperatorName(input, pos)).orElse((if (input.startsWith("`", pos)) Some(("`", pos + 1)) else None).flatMap { case (_r4285, _p4286) => {
  var _rs4289: List[Any] = Nil; var _cp4290: Int = _p4286; var _go4293 = true
  while (_go4293) { (if ((if (input.startsWith("`", _cp4290)) Some(("`", _cp4290 + 1)) else None).isEmpty) Some(((), _cp4290)) else None).flatMap { case (_r4294, _p4295) => (if (_p4295 < input.length) Some((input.charAt(_p4295).toString, _p4295 + 1)) else None).map { case (_r4296, _p4297) => (new ~(_r4294, _r4296), _p4297) } } match {
    case Some((_st4291, _np4292)) => _rs4289 = _rs4289 :+ _st4291; _cp4290 = _np4292
    case None => _go4293 = false } }
  Some((_rs4289, _cp4290)) }.map { case (_r4287, _p4288) => (new ~(_r4285, _r4287), _p4288) } }.flatMap { case (_r4281, _p4282) => (if (input.startsWith("`", _p4282)) Some(("`", _p4282 + 1)) else None).map { case (_r4283, _p4284) => (new ~(_r4281, _r4283), _p4284) } })).orElse(parseMethodIdentifierRaw(input, pos))).orElse(((((((((((if (input.startsWith("private", pos)) Some(("private", pos + 7)) else None).flatMap { case (_r4298, _p4299) => (if (parseIdentCont(input, _p4299).isEmpty) Some(((), _p4299)) else None).map { case (_r4300, _p4301) => (new ~(_r4298, _r4300), _p4301) } }).orElse((if (input.startsWith("public", pos)) Some(("public", pos + 6)) else None).flatMap { case (_r4302, _p4303) => (if (parseIdentCont(input, _p4303).isEmpty) Some(((), _p4303)) else None).map { case (_r4304, _p4305) => (new ~(_r4302, _r4304), _p4305) } })).orElse((if (input.startsWith("protected", pos)) Some(("protected", pos + 9)) else None).flatMap { case (_r4306, _p4307) => (if (parseIdentCont(input, _p4307).isEmpty) Some(((), _p4307)) else None).map { case (_r4308, _p4309) => (new ~(_r4306, _r4308), _p4309) } })).orElse((if (input.startsWith("ruby2_keywords", pos)) Some(("ruby2_keywords", pos + 14)) else None).flatMap { case (_r4310, _p4311) => (if (parseIdentCont(input, _p4311).isEmpty) Some(((), _p4311)) else None).map { case (_r4312, _p4313) => (new ~(_r4310, _r4312), _p4313) } })).orElse((if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r4314, _p4315) => (if (parseIdentCont(input, _p4315).isEmpty) Some(((), _p4315)) else None).map { case (_r4316, _p4317) => (new ~(_r4314, _r4316), _p4317) } })).orElse((if (input.startsWith("def", pos)) Some(("def", pos + 3)) else None).flatMap { case (_r4318, _p4319) => (if (parseIdentCont(input, _p4319).isEmpty) Some(((), _p4319)) else None).map { case (_r4320, _p4321) => (new ~(_r4318, _r4320), _p4321) } })).orElse((if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r4322, _p4323) => (if (parseIdentCont(input, _p4323).isEmpty) Some(((), _p4323)) else None).map { case (_r4324, _p4325) => (new ~(_r4322, _r4324), _p4325) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r4326, _p4327) => (if (parseIdentCont(input, _p4327).isEmpty) Some(((), _p4327)) else None).map { case (_r4328, _p4329) => (new ~(_r4326, _r4328), _p4329) } })).orElse((if (input.startsWith("for", pos)) Some(("for", pos + 3)) else None).flatMap { case (_r4330, _p4331) => (if (parseIdentCont(input, _p4331).isEmpty) Some(((), _p4331)) else None).map { case (_r4332, _p4333) => (new ~(_r4330, _r4332), _p4333) } })).orElse((if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r4334, _p4335) => (if (parseIdentCont(input, _p4335).isEmpty) Some(((), _p4335)) else None).map { case (_r4336, _p4337) => (new ~(_r4334, _r4336), _p4337) } })).flatMap { case (_r4277, _p4278) => parseInlineSpacing(input, _p4278).map { case (_r4279, _p4280) => (new ~(_r4277, _r4279), _p4280) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDefNamePart(input: String, pos: Int): Option[(Any, Int)] = (parseDefReceiverPart(input, pos).flatMap { case (_r4346, _p4347) => (if (input.startsWith(".", _p4347)) Some((".", _p4347 + 1)) else None).map { case (_r4348, _p4349) => (new ~(_r4346, _r4348), _p4349) } }.flatMap { case (_r4342, _p4343) => parseSpacing(input, _p4343).map { case (_r4344, _p4345) => (new ~(_r4342, _r4344), _p4345) } }.flatMap { case (_r4338, _p4339) => parseDefMethodNamePart(input, _p4339).map { case (_r4340, _p4341) => (new ~(_r4338, _r4340), _p4341) } }).orElse(parseDefMethodNamePart(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDefStmt(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("private", pos)) Some(("private", pos + 7)) else None).flatMap { case (_r4382, _p4383) => (if (parseIdentCont(input, _p4383).isEmpty) Some(((), _p4383)) else None).map { case (_r4384, _p4385) => (new ~(_r4382, _r4384), _p4385) } }.flatMap { case (_r4378, _p4379) => parseSpacing(input, _p4379).map { case (_r4380, _p4381) => (new ~(_r4378, _r4380), _p4381) } }).orElse((if (input.startsWith("public", pos)) Some(("public", pos + 6)) else None).flatMap { case (_r4390, _p4391) => (if (parseIdentCont(input, _p4391).isEmpty) Some(((), _p4391)) else None).map { case (_r4392, _p4393) => (new ~(_r4390, _r4392), _p4393) } }.flatMap { case (_r4386, _p4387) => parseSpacing(input, _p4387).map { case (_r4388, _p4389) => (new ~(_r4386, _r4388), _p4389) } })).orElse((if (input.startsWith("protected", pos)) Some(("protected", pos + 9)) else None).flatMap { case (_r4398, _p4399) => (if (parseIdentCont(input, _p4399).isEmpty) Some(((), _p4399)) else None).map { case (_r4400, _p4401) => (new ~(_r4398, _r4400), _p4401) } }.flatMap { case (_r4394, _p4395) => parseSpacing(input, _p4395).map { case (_r4396, _p4397) => (new ~(_r4394, _r4396), _p4397) } })).orElse((if (input.startsWith("ruby2_keywords", pos)) Some(("ruby2_keywords", pos + 14)) else None).flatMap { case (_r4406, _p4407) => (if (parseIdentCont(input, _p4407).isEmpty) Some(((), _p4407)) else None).map { case (_r4408, _p4409) => (new ~(_r4406, _r4408), _p4409) } }.flatMap { case (_r4402, _p4403) => parseSpacing(input, _p4403).map { case (_r4404, _p4405) => (new ~(_r4402, _r4404), _p4405) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, pos)))).flatMap { case (_r4374, _p4375) => (if (input.startsWith("def", _p4375)) Some(("def", _p4375 + 3)) else None).map { case (_r4376, _p4377) => (new ~(_r4374, _r4376), _p4377) } }.flatMap { case (_r4370, _p4371) => (if (parseIdentCont(input, _p4371).isEmpty) Some(((), _p4371)) else None).map { case (_r4372, _p4373) => (new ~(_r4370, _r4372), _p4373) } }.flatMap { case (_r4366, _p4367) => parseSpacing(input, _p4367).map { case (_r4368, _p4369) => (new ~(_r4366, _r4368), _p4369) } }.flatMap { case (_r4362, _p4363) => parseDefNamePart(input, _p4363).map { case (_r4364, _p4365) => (new ~(_r4362, _r4364), _p4365) } }.flatMap { case (_r4358, _p4359) => ((parseParams(input, _p4359)).orElse(parseSpacing1(input, _p4359).flatMap { case (_r4410, _p4411) => parseBareParams(input, _p4411).map { case (_r4412, _p4413) => (new ~(_r4410, _r4412), _p4413) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4359)))).map { case (_r4360, _p4361) => (new ~(_r4358, _r4360), _p4361) } }.flatMap { case (_r4354, _p4355) => parseSpacing(input, _p4355).map { case (_r4356, _p4357) => (new ~(_r4354, _r4356), _p4357) } }.flatMap { case (_r4350, _p4351) => (parseAssignEq(input, _p4351).flatMap { case (_r4418, _p4419) => parseSpacing(input, _p4419).map { case (_r4420, _p4421) => (new ~(_r4418, _r4420), _p4421) } }.flatMap { case (_r4414, _p4415) => parseExpr(input, _p4415).map { case (_r4416, _p4417) => (new ~(_r4414, _r4416), _p4417) } }).orElse({
  var _rs4450: List[Any] = Nil; var _cp4451: Int = _p4351; var _go4454 = true
  while (_go4454) { parseStatementSep(input, _cp4451) match {
    case Some((_st4452, _np4453)) => _rs4450 = _rs4450 :+ _st4452; _cp4451 = _np4453
    case None => _go4454 = false } }
  Some((_rs4450, _cp4451)) }.flatMap { case (_r4446, _p4447) => {
  var _rs4455: List[Any] = Nil; var _cp4456: Int = _p4447; var _go4459 = true
  while (_go4459) { (if ((parseRescueStop(input, _cp4456)).orElse(parseEndKeyword(input, _cp4456)).isEmpty) Some(((), _cp4456)) else None).flatMap { case (_r4464, _p4465) => parseStatement(input, _p4465).map { case (_r4466, _p4467) => (new ~(_r4464, _r4466), _p4467) } }.flatMap { case (_r4460, _p4461) => {
  var _rs4468: List[Any] = Nil; var _cp4469: Int = _p4461; var _go4472 = true
  while (_go4472) { parseStatementSep(input, _cp4469) match {
    case Some((_st4470, _np4471)) => _rs4468 = _rs4468 :+ _st4470; _cp4469 = _np4471
    case None => _go4472 = false } }
  Some((_rs4468, _cp4469)) }.map { case (_r4462, _p4463) => (new ~(_r4460, _r4462), _p4463) } } match {
    case Some((_st4457, _np4458)) => _rs4455 = _rs4455 :+ _st4457; _cp4456 = _np4458
    case None => _go4459 = false } }
  Some((_rs4455, _cp4456)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4448, _p4449) => (new ~(_r4446, _r4448), _p4449) } }.flatMap { case (_r4442, _p4443) => {
  var _rs4473: List[Any] = Nil; var _cp4474: Int = _p4443; var _go4477 = true
  while (_go4477) { parseStatementSep(input, _cp4474) match {
    case Some((_st4475, _np4476)) => _rs4473 = _rs4473 :+ _st4475; _cp4474 = _np4476
    case None => _go4477 = false } }
  Some((_rs4473, _cp4474)) }.map { case (_r4444, _p4445) => (new ~(_r4442, _r4444), _p4445) } }.flatMap { case (_r4438, _p4439) => {
  var _rs4478: List[Any] = Nil; var _cp4479: Int = _p4439; var _go4482 = true
  while (_go4482) { parseRescueClause(input, _cp4479) match {
    case Some((_st4480, _np4481)) => _rs4478 = _rs4478 :+ _st4480; _cp4479 = _np4481
    case None => _go4482 = false } }
  Some((_rs4478, _cp4479)) }.map { case (_r4440, _p4441) => (new ~(_r4438, _r4440), _p4441) } }.flatMap { case (_r4434, _p4435) => {
  var _rs4483: List[Any] = Nil; var _cp4484: Int = _p4435; var _go4487 = true
  while (_go4487) { parseStatementSep(input, _cp4484) match {
    case Some((_st4485, _np4486)) => _rs4483 = _rs4483 :+ _st4485; _cp4484 = _np4486
    case None => _go4487 = false } }
  Some((_rs4483, _cp4484)) }.map { case (_r4436, _p4437) => (new ~(_r4434, _r4436), _p4437) } }.flatMap { case (_r4430, _p4431) => ((if (input.startsWith("else", _p4431)) Some(("else", _p4431 + 4)) else None).flatMap { case (_r4504, _p4505) => (if (parseIdentCont(input, _p4505).isEmpty) Some(((), _p4505)) else None).map { case (_r4506, _p4507) => (new ~(_r4504, _r4506), _p4507) } }.flatMap { case (_r4500, _p4501) => parseSpacing(input, _p4501).map { case (_r4502, _p4503) => (new ~(_r4500, _r4502), _p4503) } }.flatMap { case (_r4496, _p4497) => {
  var _rs4508: List[Any] = Nil; var _cp4509: Int = _p4497; var _go4512 = true
  while (_go4512) { parseStatementSep(input, _cp4509) match {
    case Some((_st4510, _np4511)) => _rs4508 = _rs4508 :+ _st4510; _cp4509 = _np4511
    case None => _go4512 = false } }
  Some((_rs4508, _cp4509)) }.map { case (_r4498, _p4499) => (new ~(_r4496, _r4498), _p4499) } }.flatMap { case (_r4492, _p4493) => {
  var _rs4513: List[Any] = Nil; var _cp4514: Int = _p4493; var _go4517 = true
  while (_go4517) { (if (parseDoBlockStop(input, _cp4514).isEmpty) Some(((), _cp4514)) else None).flatMap { case (_r4522, _p4523) => parseStatement(input, _p4523).map { case (_r4524, _p4525) => (new ~(_r4522, _r4524), _p4525) } }.flatMap { case (_r4518, _p4519) => {
  var _rs4526: List[Any] = Nil; var _cp4527: Int = _p4519; var _go4530 = true
  while (_go4530) { parseStatementSep(input, _cp4527) match {
    case Some((_st4528, _np4529)) => _rs4526 = _rs4526 :+ _st4528; _cp4527 = _np4529
    case None => _go4530 = false } }
  Some((_rs4526, _cp4527)) }.map { case (_r4520, _p4521) => (new ~(_r4518, _r4520), _p4521) } } match {
    case Some((_st4515, _np4516)) => _rs4513 = _rs4513 :+ _st4515; _cp4514 = _np4516
    case None => _go4517 = false } }
  Some((_rs4513, _cp4514)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4494, _p4495) => (new ~(_r4492, _r4494), _p4495) } }.flatMap { case (_r4488, _p4489) => {
  var _rs4531: List[Any] = Nil; var _cp4532: Int = _p4489; var _go4535 = true
  while (_go4535) { parseStatementSep(input, _cp4532) match {
    case Some((_st4533, _np4534)) => _rs4531 = _rs4531 :+ _st4533; _cp4532 = _np4534
    case None => _go4535 = false } }
  Some((_rs4531, _cp4532)) }.map { case (_r4490, _p4491) => (new ~(_r4488, _r4490), _p4491) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4431)))).map { case (_r4432, _p4433) => (new ~(_r4430, _r4432), _p4433) } }.flatMap { case (_r4426, _p4427) => ((if (input.startsWith("ensure", _p4427)) Some(("ensure", _p4427 + 6)) else None).flatMap { case (_r4552, _p4553) => (if (parseIdentCont(input, _p4553).isEmpty) Some(((), _p4553)) else None).map { case (_r4554, _p4555) => (new ~(_r4552, _r4554), _p4555) } }.flatMap { case (_r4548, _p4549) => parseSpacing(input, _p4549).map { case (_r4550, _p4551) => (new ~(_r4548, _r4550), _p4551) } }.flatMap { case (_r4544, _p4545) => {
  var _rs4556: List[Any] = Nil; var _cp4557: Int = _p4545; var _go4560 = true
  while (_go4560) { parseStatementSep(input, _cp4557) match {
    case Some((_st4558, _np4559)) => _rs4556 = _rs4556 :+ _st4558; _cp4557 = _np4559
    case None => _go4560 = false } }
  Some((_rs4556, _cp4557)) }.map { case (_r4546, _p4547) => (new ~(_r4544, _r4546), _p4547) } }.flatMap { case (_r4540, _p4541) => {
  var _rs4561: List[Any] = Nil; var _cp4562: Int = _p4541; var _go4565 = true
  while (_go4565) { (if (parseEndKeyword(input, _cp4562).isEmpty) Some(((), _cp4562)) else None).flatMap { case (_r4570, _p4571) => parseStatement(input, _p4571).map { case (_r4572, _p4573) => (new ~(_r4570, _r4572), _p4573) } }.flatMap { case (_r4566, _p4567) => {
  var _rs4574: List[Any] = Nil; var _cp4575: Int = _p4567; var _go4578 = true
  while (_go4578) { parseStatementSep(input, _cp4575) match {
    case Some((_st4576, _np4577)) => _rs4574 = _rs4574 :+ _st4576; _cp4575 = _np4577
    case None => _go4578 = false } }
  Some((_rs4574, _cp4575)) }.map { case (_r4568, _p4569) => (new ~(_r4566, _r4568), _p4569) } } match {
    case Some((_st4563, _np4564)) => _rs4561 = _rs4561 :+ _st4563; _cp4562 = _np4564
    case None => _go4565 = false } }
  Some((_rs4561, _cp4562)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4542, _p4543) => (new ~(_r4540, _r4542), _p4543) } }.flatMap { case (_r4536, _p4537) => {
  var _rs4579: List[Any] = Nil; var _cp4580: Int = _p4537; var _go4583 = true
  while (_go4583) { parseStatementSep(input, _cp4580) match {
    case Some((_st4581, _np4582)) => _rs4579 = _rs4579 :+ _st4581; _cp4580 = _np4582
    case None => _go4583 = false } }
  Some((_rs4579, _cp4580)) }.map { case (_r4538, _p4539) => (new ~(_r4536, _r4538), _p4539) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4427)))).map { case (_r4428, _p4429) => (new ~(_r4426, _r4428), _p4429) } }.flatMap { case (_r4422, _p4423) => parseEndKeyword(input, _p4423).map { case (_r4424, _p4425) => (new ~(_r4422, _r4424), _p4425) } }).map { case (_r4352, _p4353) => (new ~(_r4350, _r4352), _p4353) } }.map { case (r, p) => (_applyAction({  _ => Def("", List.empty, List.empty)  }, r), p) }

  def parseSingletonClassStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r4620, _p4621) => (if (parseIdentCont(input, _p4621).isEmpty) Some(((), _p4621)) else None).map { case (_r4622, _p4623) => (new ~(_r4620, _r4622), _p4623) } }.flatMap { case (_r4616, _p4617) => parseSpacing(input, _p4617).map { case (_r4618, _p4619) => (new ~(_r4616, _r4618), _p4619) } }.flatMap { case (_r4612, _p4613) => (if (input.startsWith("<<", _p4613)) Some(("<<", _p4613 + 2)) else None).map { case (_r4614, _p4615) => (new ~(_r4612, _r4614), _p4615) } }.flatMap { case (_r4608, _p4609) => parseSpacing(input, _p4609).map { case (_r4610, _p4611) => (new ~(_r4608, _r4610), _p4611) } }.flatMap { case (_r4604, _p4605) => parseExpr(input, _p4605).map { case (_r4606, _p4607) => (new ~(_r4604, _r4606), _p4607) } }.flatMap { case (_r4600, _p4601) => {
  var _rs4624: List[Any] = Nil; var _cp4625: Int = _p4601; var _go4628 = true
  while (_go4628) { parseStatementSep(input, _cp4625) match {
    case Some((_st4626, _np4627)) => _rs4624 = _rs4624 :+ _st4626; _cp4625 = _np4627
    case None => _go4628 = false } }
  Some((_rs4624, _cp4625)) }.map { case (_r4602, _p4603) => (new ~(_r4600, _r4602), _p4603) } }.flatMap { case (_r4596, _p4597) => {
  var _rs4629: List[Any] = Nil; var _cp4630: Int = _p4597; var _go4633 = true
  while (_go4633) { (if (parseEndKeyword(input, _cp4630).isEmpty) Some(((), _cp4630)) else None).flatMap { case (_r4638, _p4639) => parseStatement(input, _p4639).map { case (_r4640, _p4641) => (new ~(_r4638, _r4640), _p4641) } }.flatMap { case (_r4634, _p4635) => {
  var _rs4642: List[Any] = Nil; var _cp4643: Int = _p4635; var _go4646 = true
  while (_go4646) { parseStatementSep(input, _cp4643) match {
    case Some((_st4644, _np4645)) => _rs4642 = _rs4642 :+ _st4644; _cp4643 = _np4645
    case None => _go4646 = false } }
  Some((_rs4642, _cp4643)) }.map { case (_r4636, _p4637) => (new ~(_r4634, _r4636), _p4637) } } match {
    case Some((_st4631, _np4632)) => _rs4629 = _rs4629 :+ _st4631; _cp4630 = _np4632
    case None => _go4633 = false } }
  Some((_rs4629, _cp4630)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4598, _p4599) => (new ~(_r4596, _r4598), _p4599) } }.flatMap { case (_r4592, _p4593) => {
  var _rs4647: List[Any] = Nil; var _cp4648: Int = _p4593; var _go4651 = true
  while (_go4651) { parseStatementSep(input, _cp4648) match {
    case Some((_st4649, _np4650)) => _rs4647 = _rs4647 :+ _st4649; _cp4648 = _np4650
    case None => _go4651 = false } }
  Some((_rs4647, _cp4648)) }.map { case (_r4594, _p4595) => (new ~(_r4592, _r4594), _p4595) } }.flatMap { case (_r4588, _p4589) => parseEndKeyword(input, _p4589).map { case (_r4590, _p4591) => (new ~(_r4588, _r4590), _p4591) } }.flatMap { case (_r4584, _p4585) => {
  var _rs4652: List[Any] = Nil; var _cp4653: Int = _p4585; var _go4656 = true
  while (_go4656) { parseCallSuffix(input, _cp4653) match {
    case Some((_st4654, _np4655)) => _rs4652 = _rs4652 :+ _st4654; _cp4653 = _np4655
    case None => _go4656 = false } }
  Some((_rs4652, _cp4653)) }.map { case (_r4586, _p4587) => (new ~(_r4584, _r4586), _p4587) } }.map { case (r, p) => (_applyAction({  _ => SingletonClassDef(NilLiteral(), List.empty)  }, r), p) }

  def parseClassStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r4685, _p4686) => (if (parseIdentCont(input, _p4686).isEmpty) Some(((), _p4686)) else None).map { case (_r4687, _p4688) => (new ~(_r4685, _r4687), _p4688) } }.flatMap { case (_r4681, _p4682) => parseSpacing(input, _p4682).map { case (_r4683, _p4684) => (new ~(_r4681, _r4683), _p4684) } }.flatMap { case (_r4677, _p4678) => parseConstPath(input, _p4678).map { case (_r4679, _p4680) => (new ~(_r4677, _r4679), _p4680) } }.flatMap { case (_r4673, _p4674) => ((if (input.startsWith("<", _p4674)) Some(("<", _p4674 + 1)) else None).flatMap { case (_r4697, _p4698) => (if ((if (input.startsWith("<", _p4698)) Some(("<", _p4698 + 1)) else None).isEmpty) Some(((), _p4698)) else None).map { case (_r4699, _p4700) => (new ~(_r4697, _r4699), _p4700) } }.flatMap { case (_r4693, _p4694) => parseSpacing(input, _p4694).map { case (_r4695, _p4696) => (new ~(_r4693, _r4695), _p4696) } }.flatMap { case (_r4689, _p4690) => parseExpr(input, _p4690).map { case (_r4691, _p4692) => (new ~(_r4689, _r4691), _p4692) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4674)))).map { case (_r4675, _p4676) => (new ~(_r4673, _r4675), _p4676) } }.flatMap { case (_r4669, _p4670) => {
  var _rs4701: List[Any] = Nil; var _cp4702: Int = _p4670; var _go4705 = true
  while (_go4705) { parseStatementSep(input, _cp4702) match {
    case Some((_st4703, _np4704)) => _rs4701 = _rs4701 :+ _st4703; _cp4702 = _np4704
    case None => _go4705 = false } }
  Some((_rs4701, _cp4702)) }.map { case (_r4671, _p4672) => (new ~(_r4669, _r4671), _p4672) } }.flatMap { case (_r4665, _p4666) => {
  var _rs4706: List[Any] = Nil; var _cp4707: Int = _p4666; var _go4710 = true
  while (_go4710) { (if (parseEndKeyword(input, _cp4707).isEmpty) Some(((), _cp4707)) else None).flatMap { case (_r4715, _p4716) => parseStatement(input, _p4716).map { case (_r4717, _p4718) => (new ~(_r4715, _r4717), _p4718) } }.flatMap { case (_r4711, _p4712) => {
  var _rs4719: List[Any] = Nil; var _cp4720: Int = _p4712; var _go4723 = true
  while (_go4723) { parseStatementSep(input, _cp4720) match {
    case Some((_st4721, _np4722)) => _rs4719 = _rs4719 :+ _st4721; _cp4720 = _np4722
    case None => _go4723 = false } }
  Some((_rs4719, _cp4720)) }.map { case (_r4713, _p4714) => (new ~(_r4711, _r4713), _p4714) } } match {
    case Some((_st4708, _np4709)) => _rs4706 = _rs4706 :+ _st4708; _cp4707 = _np4709
    case None => _go4710 = false } }
  Some((_rs4706, _cp4707)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4667, _p4668) => (new ~(_r4665, _r4667), _p4668) } }.flatMap { case (_r4661, _p4662) => {
  var _rs4724: List[Any] = Nil; var _cp4725: Int = _p4662; var _go4728 = true
  while (_go4728) { parseStatementSep(input, _cp4725) match {
    case Some((_st4726, _np4727)) => _rs4724 = _rs4724 :+ _st4726; _cp4725 = _np4727
    case None => _go4728 = false } }
  Some((_rs4724, _cp4725)) }.map { case (_r4663, _p4664) => (new ~(_r4661, _r4663), _p4664) } }.flatMap { case (_r4657, _p4658) => parseEndKeyword(input, _p4658).map { case (_r4659, _p4660) => (new ~(_r4657, _r4659), _p4660) } }.map { case (r, p) => (_applyAction({  _ => ClassDef("", List.empty)  }, r), p) }

  def parseModuleStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("module", pos)) Some(("module", pos + 6)) else None).flatMap { case (_r4753, _p4754) => (if (parseIdentCont(input, _p4754).isEmpty) Some(((), _p4754)) else None).map { case (_r4755, _p4756) => (new ~(_r4753, _r4755), _p4756) } }.flatMap { case (_r4749, _p4750) => parseSpacing(input, _p4750).map { case (_r4751, _p4752) => (new ~(_r4749, _r4751), _p4752) } }.flatMap { case (_r4745, _p4746) => parseConstPath(input, _p4746).map { case (_r4747, _p4748) => (new ~(_r4745, _r4747), _p4748) } }.flatMap { case (_r4741, _p4742) => {
  var _rs4757: List[Any] = Nil; var _cp4758: Int = _p4742; var _go4761 = true
  while (_go4761) { parseStatementSep(input, _cp4758) match {
    case Some((_st4759, _np4760)) => _rs4757 = _rs4757 :+ _st4759; _cp4758 = _np4760
    case None => _go4761 = false } }
  Some((_rs4757, _cp4758)) }.map { case (_r4743, _p4744) => (new ~(_r4741, _r4743), _p4744) } }.flatMap { case (_r4737, _p4738) => {
  var _rs4762: List[Any] = Nil; var _cp4763: Int = _p4738; var _go4766 = true
  while (_go4766) { (if (parseEndKeyword(input, _cp4763).isEmpty) Some(((), _cp4763)) else None).flatMap { case (_r4771, _p4772) => parseStatement(input, _p4772).map { case (_r4773, _p4774) => (new ~(_r4771, _r4773), _p4774) } }.flatMap { case (_r4767, _p4768) => {
  var _rs4775: List[Any] = Nil; var _cp4776: Int = _p4768; var _go4779 = true
  while (_go4779) { parseStatementSep(input, _cp4776) match {
    case Some((_st4777, _np4778)) => _rs4775 = _rs4775 :+ _st4777; _cp4776 = _np4778
    case None => _go4779 = false } }
  Some((_rs4775, _cp4776)) }.map { case (_r4769, _p4770) => (new ~(_r4767, _r4769), _p4770) } } match {
    case Some((_st4764, _np4765)) => _rs4762 = _rs4762 :+ _st4764; _cp4763 = _np4765
    case None => _go4766 = false } }
  Some((_rs4762, _cp4763)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4739, _p4740) => (new ~(_r4737, _r4739), _p4740) } }.flatMap { case (_r4733, _p4734) => {
  var _rs4780: List[Any] = Nil; var _cp4781: Int = _p4734; var _go4784 = true
  while (_go4784) { parseStatementSep(input, _cp4781) match {
    case Some((_st4782, _np4783)) => _rs4780 = _rs4780 :+ _st4782; _cp4781 = _np4783
    case None => _go4784 = false } }
  Some((_rs4780, _cp4781)) }.map { case (_r4735, _p4736) => (new ~(_r4733, _r4735), _p4736) } }.flatMap { case (_r4729, _p4730) => parseEndKeyword(input, _p4730).map { case (_r4731, _p4732) => (new ~(_r4729, _r4731), _p4732) } }.map { case (r, p) => (_applyAction({  _ => ModuleDef("", List.empty)  }, r), p) }

  def parseWhileStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("while", pos)) Some(("while", pos + 5)) else None).flatMap { case (_r4813, _p4814) => (if (parseIdentCont(input, _p4814).isEmpty) Some(((), _p4814)) else None).map { case (_r4815, _p4816) => (new ~(_r4813, _r4815), _p4816) } }.flatMap { case (_r4809, _p4810) => parseSpacing(input, _p4810).map { case (_r4811, _p4812) => (new ~(_r4809, _r4811), _p4812) } }.flatMap { case (_r4805, _p4806) => parseExpr(input, _p4806).map { case (_r4807, _p4808) => (new ~(_r4805, _r4807), _p4808) } }.flatMap { case (_r4801, _p4802) => ((((if (input.startsWith("do", _p4802)) Some(("do", _p4802 + 2)) else None).flatMap { case (_r4821, _p4822) => (if (parseIdentCont(input, _p4822).isEmpty) Some(((), _p4822)) else None).map { case (_r4823, _p4824) => (new ~(_r4821, _r4823), _p4824) } }).orElse((if (input.startsWith("then", _p4802)) Some(("then", _p4802 + 4)) else None).flatMap { case (_r4825, _p4826) => (if (parseIdentCont(input, _p4826).isEmpty) Some(((), _p4826)) else None).map { case (_r4827, _p4828) => (new ~(_r4825, _r4827), _p4828) } }).flatMap { case (_r4817, _p4818) => parseSpacing(input, _p4818).map { case (_r4819, _p4820) => (new ~(_r4817, _r4819), _p4820) } }).orElse(parseInlineSpacing(input, _p4802).flatMap { case (_r4829, _p4830) => {
  parseStatementSep(input, _p4830) match {
    case None => None
    case Some((_fs4833, _fp4834)) =>
      var _rs4835: List[Any] = List(_fs4833); var _cp4836: Int = _fp4834; var _go4839 = true
      while (_go4839) { parseStatementSep(input, _cp4836) match {
        case Some((_st4837, _np4838)) => _rs4835 = _rs4835 :+ _st4837; _cp4836 = _np4838
        case None => _go4839 = false } }
      Some((_rs4835, _cp4836)) } }.map { case (_r4831, _p4832) => (new ~(_r4829, _r4831), _p4832) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4802)))).map { case (_r4803, _p4804) => (new ~(_r4801, _r4803), _p4804) } }.flatMap { case (_r4797, _p4798) => {
  var _rs4840: List[Any] = Nil; var _cp4841: Int = _p4798; var _go4844 = true
  while (_go4844) { parseStatementSep(input, _cp4841) match {
    case Some((_st4842, _np4843)) => _rs4840 = _rs4840 :+ _st4842; _cp4841 = _np4843
    case None => _go4844 = false } }
  Some((_rs4840, _cp4841)) }.map { case (_r4799, _p4800) => (new ~(_r4797, _r4799), _p4800) } }.flatMap { case (_r4793, _p4794) => {
  var _rs4845: List[Any] = Nil; var _cp4846: Int = _p4794; var _go4849 = true
  while (_go4849) { (if (parseEndKeyword(input, _cp4846).isEmpty) Some(((), _cp4846)) else None).flatMap { case (_r4854, _p4855) => parseStatement(input, _p4855).map { case (_r4856, _p4857) => (new ~(_r4854, _r4856), _p4857) } }.flatMap { case (_r4850, _p4851) => {
  var _rs4858: List[Any] = Nil; var _cp4859: Int = _p4851; var _go4862 = true
  while (_go4862) { parseStatementSep(input, _cp4859) match {
    case Some((_st4860, _np4861)) => _rs4858 = _rs4858 :+ _st4860; _cp4859 = _np4861
    case None => _go4862 = false } }
  Some((_rs4858, _cp4859)) }.map { case (_r4852, _p4853) => (new ~(_r4850, _r4852), _p4853) } } match {
    case Some((_st4847, _np4848)) => _rs4845 = _rs4845 :+ _st4847; _cp4846 = _np4848
    case None => _go4849 = false } }
  Some((_rs4845, _cp4846)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4795, _p4796) => (new ~(_r4793, _r4795), _p4796) } }.flatMap { case (_r4789, _p4790) => {
  var _rs4863: List[Any] = Nil; var _cp4864: Int = _p4790; var _go4867 = true
  while (_go4867) { parseStatementSep(input, _cp4864) match {
    case Some((_st4865, _np4866)) => _rs4863 = _rs4863 :+ _st4865; _cp4864 = _np4866
    case None => _go4867 = false } }
  Some((_rs4863, _cp4864)) }.map { case (_r4791, _p4792) => (new ~(_r4789, _r4791), _p4792) } }.flatMap { case (_r4785, _p4786) => parseEndKeyword(input, _p4786).map { case (_r4787, _p4788) => (new ~(_r4785, _r4787), _p4788) } }.map { case (r, p) => (_applyAction({  _ => WhileExpr(NilLiteral(), List.empty)  }, r), p) }

  def parseUntilStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("until", pos)) Some(("until", pos + 5)) else None).flatMap { case (_r4896, _p4897) => (if (parseIdentCont(input, _p4897).isEmpty) Some(((), _p4897)) else None).map { case (_r4898, _p4899) => (new ~(_r4896, _r4898), _p4899) } }.flatMap { case (_r4892, _p4893) => parseSpacing(input, _p4893).map { case (_r4894, _p4895) => (new ~(_r4892, _r4894), _p4895) } }.flatMap { case (_r4888, _p4889) => parseExpr(input, _p4889).map { case (_r4890, _p4891) => (new ~(_r4888, _r4890), _p4891) } }.flatMap { case (_r4884, _p4885) => ((((if (input.startsWith("do", _p4885)) Some(("do", _p4885 + 2)) else None).flatMap { case (_r4904, _p4905) => (if (parseIdentCont(input, _p4905).isEmpty) Some(((), _p4905)) else None).map { case (_r4906, _p4907) => (new ~(_r4904, _r4906), _p4907) } }).orElse((if (input.startsWith("then", _p4885)) Some(("then", _p4885 + 4)) else None).flatMap { case (_r4908, _p4909) => (if (parseIdentCont(input, _p4909).isEmpty) Some(((), _p4909)) else None).map { case (_r4910, _p4911) => (new ~(_r4908, _r4910), _p4911) } }).flatMap { case (_r4900, _p4901) => parseSpacing(input, _p4901).map { case (_r4902, _p4903) => (new ~(_r4900, _r4902), _p4903) } }).orElse(parseInlineSpacing(input, _p4885).flatMap { case (_r4912, _p4913) => {
  parseStatementSep(input, _p4913) match {
    case None => None
    case Some((_fs4916, _fp4917)) =>
      var _rs4918: List[Any] = List(_fs4916); var _cp4919: Int = _fp4917; var _go4922 = true
      while (_go4922) { parseStatementSep(input, _cp4919) match {
        case Some((_st4920, _np4921)) => _rs4918 = _rs4918 :+ _st4920; _cp4919 = _np4921
        case None => _go4922 = false } }
      Some((_rs4918, _cp4919)) } }.map { case (_r4914, _p4915) => (new ~(_r4912, _r4914), _p4915) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4885)))).map { case (_r4886, _p4887) => (new ~(_r4884, _r4886), _p4887) } }.flatMap { case (_r4880, _p4881) => {
  var _rs4923: List[Any] = Nil; var _cp4924: Int = _p4881; var _go4927 = true
  while (_go4927) { parseStatementSep(input, _cp4924) match {
    case Some((_st4925, _np4926)) => _rs4923 = _rs4923 :+ _st4925; _cp4924 = _np4926
    case None => _go4927 = false } }
  Some((_rs4923, _cp4924)) }.map { case (_r4882, _p4883) => (new ~(_r4880, _r4882), _p4883) } }.flatMap { case (_r4876, _p4877) => {
  var _rs4928: List[Any] = Nil; var _cp4929: Int = _p4877; var _go4932 = true
  while (_go4932) { (if (parseEndKeyword(input, _cp4929).isEmpty) Some(((), _cp4929)) else None).flatMap { case (_r4937, _p4938) => parseStatement(input, _p4938).map { case (_r4939, _p4940) => (new ~(_r4937, _r4939), _p4940) } }.flatMap { case (_r4933, _p4934) => {
  var _rs4941: List[Any] = Nil; var _cp4942: Int = _p4934; var _go4945 = true
  while (_go4945) { parseStatementSep(input, _cp4942) match {
    case Some((_st4943, _np4944)) => _rs4941 = _rs4941 :+ _st4943; _cp4942 = _np4944
    case None => _go4945 = false } }
  Some((_rs4941, _cp4942)) }.map { case (_r4935, _p4936) => (new ~(_r4933, _r4935), _p4936) } } match {
    case Some((_st4930, _np4931)) => _rs4928 = _rs4928 :+ _st4930; _cp4929 = _np4931
    case None => _go4932 = false } }
  Some((_rs4928, _cp4929)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4878, _p4879) => (new ~(_r4876, _r4878), _p4879) } }.flatMap { case (_r4872, _p4873) => {
  var _rs4946: List[Any] = Nil; var _cp4947: Int = _p4873; var _go4950 = true
  while (_go4950) { parseStatementSep(input, _cp4947) match {
    case Some((_st4948, _np4949)) => _rs4946 = _rs4946 :+ _st4948; _cp4947 = _np4949
    case None => _go4950 = false } }
  Some((_rs4946, _cp4947)) }.map { case (_r4874, _p4875) => (new ~(_r4872, _r4874), _p4875) } }.flatMap { case (_r4868, _p4869) => parseEndKeyword(input, _p4869).map { case (_r4870, _p4871) => (new ~(_r4868, _r4870), _p4871) } }.map { case (r, p) => (_applyAction({  _ => UntilExpr(NilLiteral(), List.empty)  }, r), p) }

  def parseForBindingTarget(input: String, pos: Int): Option[(Any, Int)] = (parseMultiAssignTargets(input, pos)).orElse(parseAssignableTarget(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseForStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("for", pos)) Some(("for", pos + 3)) else None).flatMap { case (_r4999, _p5000) => (if (parseIdentCont(input, _p5000).isEmpty) Some(((), _p5000)) else None).map { case (_r5001, _p5002) => (new ~(_r4999, _r5001), _p5002) } }.flatMap { case (_r4995, _p4996) => parseSpacing(input, _p4996).map { case (_r4997, _p4998) => (new ~(_r4995, _r4997), _p4998) } }.flatMap { case (_r4991, _p4992) => parseForBindingTarget(input, _p4992).map { case (_r4993, _p4994) => (new ~(_r4991, _r4993), _p4994) } }.flatMap { case (_r4987, _p4988) => parseSpacing(input, _p4988).map { case (_r4989, _p4990) => (new ~(_r4987, _r4989), _p4990) } }.flatMap { case (_r4983, _p4984) => (if (input.startsWith("in", _p4984)) Some(("in", _p4984 + 2)) else None).map { case (_r4985, _p4986) => (new ~(_r4983, _r4985), _p4986) } }.flatMap { case (_r4979, _p4980) => (if (parseIdentCont(input, _p4980).isEmpty) Some(((), _p4980)) else None).map { case (_r4981, _p4982) => (new ~(_r4979, _r4981), _p4982) } }.flatMap { case (_r4975, _p4976) => parseSpacing(input, _p4976).map { case (_r4977, _p4978) => (new ~(_r4975, _r4977), _p4978) } }.flatMap { case (_r4971, _p4972) => parseExpr(input, _p4972).map { case (_r4973, _p4974) => (new ~(_r4971, _r4973), _p4974) } }.flatMap { case (_r4967, _p4968) => ((((if (input.startsWith("do", _p4968)) Some(("do", _p4968 + 2)) else None).flatMap { case (_r5007, _p5008) => (if (parseIdentCont(input, _p5008).isEmpty) Some(((), _p5008)) else None).map { case (_r5009, _p5010) => (new ~(_r5007, _r5009), _p5010) } }).orElse((if (input.startsWith("then", _p4968)) Some(("then", _p4968 + 4)) else None).flatMap { case (_r5011, _p5012) => (if (parseIdentCont(input, _p5012).isEmpty) Some(((), _p5012)) else None).map { case (_r5013, _p5014) => (new ~(_r5011, _r5013), _p5014) } }).flatMap { case (_r5003, _p5004) => parseSpacing(input, _p5004).map { case (_r5005, _p5006) => (new ~(_r5003, _r5005), _p5006) } }).orElse(parseInlineSpacing(input, _p4968).flatMap { case (_r5015, _p5016) => {
  parseStatementSep(input, _p5016) match {
    case None => None
    case Some((_fs5019, _fp5020)) =>
      var _rs5021: List[Any] = List(_fs5019); var _cp5022: Int = _fp5020; var _go5025 = true
      while (_go5025) { parseStatementSep(input, _cp5022) match {
        case Some((_st5023, _np5024)) => _rs5021 = _rs5021 :+ _st5023; _cp5022 = _np5024
        case None => _go5025 = false } }
      Some((_rs5021, _cp5022)) } }.map { case (_r5017, _p5018) => (new ~(_r5015, _r5017), _p5018) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4968)))).map { case (_r4969, _p4970) => (new ~(_r4967, _r4969), _p4970) } }.flatMap { case (_r4963, _p4964) => {
  var _rs5026: List[Any] = Nil; var _cp5027: Int = _p4964; var _go5030 = true
  while (_go5030) { parseStatementSep(input, _cp5027) match {
    case Some((_st5028, _np5029)) => _rs5026 = _rs5026 :+ _st5028; _cp5027 = _np5029
    case None => _go5030 = false } }
  Some((_rs5026, _cp5027)) }.map { case (_r4965, _p4966) => (new ~(_r4963, _r4965), _p4966) } }.flatMap { case (_r4959, _p4960) => {
  var _rs5031: List[Any] = Nil; var _cp5032: Int = _p4960; var _go5035 = true
  while (_go5035) { (if (parseEndKeyword(input, _cp5032).isEmpty) Some(((), _cp5032)) else None).flatMap { case (_r5040, _p5041) => parseStatement(input, _p5041).map { case (_r5042, _p5043) => (new ~(_r5040, _r5042), _p5043) } }.flatMap { case (_r5036, _p5037) => {
  var _rs5044: List[Any] = Nil; var _cp5045: Int = _p5037; var _go5048 = true
  while (_go5048) { parseStatementSep(input, _cp5045) match {
    case Some((_st5046, _np5047)) => _rs5044 = _rs5044 :+ _st5046; _cp5045 = _np5047
    case None => _go5048 = false } }
  Some((_rs5044, _cp5045)) }.map { case (_r5038, _p5039) => (new ~(_r5036, _r5038), _p5039) } } match {
    case Some((_st5033, _np5034)) => _rs5031 = _rs5031 :+ _st5033; _cp5032 = _np5034
    case None => _go5035 = false } }
  Some((_rs5031, _cp5032)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r4961, _p4962) => (new ~(_r4959, _r4961), _p4962) } }.flatMap { case (_r4955, _p4956) => {
  var _rs5049: List[Any] = Nil; var _cp5050: Int = _p4956; var _go5053 = true
  while (_go5053) { parseStatementSep(input, _cp5050) match {
    case Some((_st5051, _np5052)) => _rs5049 = _rs5049 :+ _st5051; _cp5050 = _np5052
    case None => _go5053 = false } }
  Some((_rs5049, _cp5050)) }.map { case (_r4957, _p4958) => (new ~(_r4955, _r4957), _p4958) } }.flatMap { case (_r4951, _p4952) => parseEndKeyword(input, _p4952).map { case (_r4953, _p4954) => (new ~(_r4951, _r4953), _p4954) } }.map { case (r, p) => (_applyAction({  _ => ForIn("", NilLiteral(), List.empty)  }, r), p) }

  def parseBeginStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r5090, _p5091) => (if (parseIdentCont(input, _p5091).isEmpty) Some(((), _p5091)) else None).map { case (_r5092, _p5093) => (new ~(_r5090, _r5092), _p5093) } }.flatMap { case (_r5086, _p5087) => parseSpacing(input, _p5087).map { case (_r5088, _p5089) => (new ~(_r5086, _r5088), _p5089) } }.flatMap { case (_r5082, _p5083) => {
  var _rs5094: List[Any] = Nil; var _cp5095: Int = _p5083; var _go5098 = true
  while (_go5098) { parseStatementSep(input, _cp5095) match {
    case Some((_st5096, _np5097)) => _rs5094 = _rs5094 :+ _st5096; _cp5095 = _np5097
    case None => _go5098 = false } }
  Some((_rs5094, _cp5095)) }.map { case (_r5084, _p5085) => (new ~(_r5082, _r5084), _p5085) } }.flatMap { case (_r5078, _p5079) => {
  var _rs5099: List[Any] = Nil; var _cp5100: Int = _p5079; var _go5103 = true
  while (_go5103) { (if ((parseRescueStop(input, _cp5100)).orElse(parseEndKeyword(input, _cp5100)).isEmpty) Some(((), _cp5100)) else None).flatMap { case (_r5108, _p5109) => parseStatement(input, _p5109).map { case (_r5110, _p5111) => (new ~(_r5108, _r5110), _p5111) } }.flatMap { case (_r5104, _p5105) => {
  var _rs5112: List[Any] = Nil; var _cp5113: Int = _p5105; var _go5116 = true
  while (_go5116) { parseStatementSep(input, _cp5113) match {
    case Some((_st5114, _np5115)) => _rs5112 = _rs5112 :+ _st5114; _cp5113 = _np5115
    case None => _go5116 = false } }
  Some((_rs5112, _cp5113)) }.map { case (_r5106, _p5107) => (new ~(_r5104, _r5106), _p5107) } } match {
    case Some((_st5101, _np5102)) => _rs5099 = _rs5099 :+ _st5101; _cp5100 = _np5102
    case None => _go5103 = false } }
  Some((_rs5099, _cp5100)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r5080, _p5081) => (new ~(_r5078, _r5080), _p5081) } }.flatMap { case (_r5074, _p5075) => {
  var _rs5117: List[Any] = Nil; var _cp5118: Int = _p5075; var _go5121 = true
  while (_go5121) { parseStatementSep(input, _cp5118) match {
    case Some((_st5119, _np5120)) => _rs5117 = _rs5117 :+ _st5119; _cp5118 = _np5120
    case None => _go5121 = false } }
  Some((_rs5117, _cp5118)) }.map { case (_r5076, _p5077) => (new ~(_r5074, _r5076), _p5077) } }.flatMap { case (_r5070, _p5071) => {
  var _rs5122: List[Any] = Nil; var _cp5123: Int = _p5071; var _go5126 = true
  while (_go5126) { parseRescueClause(input, _cp5123) match {
    case Some((_st5124, _np5125)) => _rs5122 = _rs5122 :+ _st5124; _cp5123 = _np5125
    case None => _go5126 = false } }
  Some((_rs5122, _cp5123)) }.map { case (_r5072, _p5073) => (new ~(_r5070, _r5072), _p5073) } }.flatMap { case (_r5066, _p5067) => {
  var _rs5127: List[Any] = Nil; var _cp5128: Int = _p5067; var _go5131 = true
  while (_go5131) { parseStatementSep(input, _cp5128) match {
    case Some((_st5129, _np5130)) => _rs5127 = _rs5127 :+ _st5129; _cp5128 = _np5130
    case None => _go5131 = false } }
  Some((_rs5127, _cp5128)) }.map { case (_r5068, _p5069) => (new ~(_r5066, _r5068), _p5069) } }.flatMap { case (_r5062, _p5063) => ((if (input.startsWith("else", _p5063)) Some(("else", _p5063 + 4)) else None).flatMap { case (_r5148, _p5149) => (if (parseIdentCont(input, _p5149).isEmpty) Some(((), _p5149)) else None).map { case (_r5150, _p5151) => (new ~(_r5148, _r5150), _p5151) } }.flatMap { case (_r5144, _p5145) => parseSpacing(input, _p5145).map { case (_r5146, _p5147) => (new ~(_r5144, _r5146), _p5147) } }.flatMap { case (_r5140, _p5141) => {
  var _rs5152: List[Any] = Nil; var _cp5153: Int = _p5141; var _go5156 = true
  while (_go5156) { parseStatementSep(input, _cp5153) match {
    case Some((_st5154, _np5155)) => _rs5152 = _rs5152 :+ _st5154; _cp5153 = _np5155
    case None => _go5156 = false } }
  Some((_rs5152, _cp5153)) }.map { case (_r5142, _p5143) => (new ~(_r5140, _r5142), _p5143) } }.flatMap { case (_r5136, _p5137) => {
  var _rs5157: List[Any] = Nil; var _cp5158: Int = _p5137; var _go5161 = true
  while (_go5161) { (if (parseDoBlockStop(input, _cp5158).isEmpty) Some(((), _cp5158)) else None).flatMap { case (_r5166, _p5167) => parseStatement(input, _p5167).map { case (_r5168, _p5169) => (new ~(_r5166, _r5168), _p5169) } }.flatMap { case (_r5162, _p5163) => {
  var _rs5170: List[Any] = Nil; var _cp5171: Int = _p5163; var _go5174 = true
  while (_go5174) { parseStatementSep(input, _cp5171) match {
    case Some((_st5172, _np5173)) => _rs5170 = _rs5170 :+ _st5172; _cp5171 = _np5173
    case None => _go5174 = false } }
  Some((_rs5170, _cp5171)) }.map { case (_r5164, _p5165) => (new ~(_r5162, _r5164), _p5165) } } match {
    case Some((_st5159, _np5160)) => _rs5157 = _rs5157 :+ _st5159; _cp5158 = _np5160
    case None => _go5161 = false } }
  Some((_rs5157, _cp5158)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r5138, _p5139) => (new ~(_r5136, _r5138), _p5139) } }.flatMap { case (_r5132, _p5133) => {
  var _rs5175: List[Any] = Nil; var _cp5176: Int = _p5133; var _go5179 = true
  while (_go5179) { parseStatementSep(input, _cp5176) match {
    case Some((_st5177, _np5178)) => _rs5175 = _rs5175 :+ _st5177; _cp5176 = _np5178
    case None => _go5179 = false } }
  Some((_rs5175, _cp5176)) }.map { case (_r5134, _p5135) => (new ~(_r5132, _r5134), _p5135) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5063)))).map { case (_r5064, _p5065) => (new ~(_r5062, _r5064), _p5065) } }.flatMap { case (_r5058, _p5059) => ((if (input.startsWith("ensure", _p5059)) Some(("ensure", _p5059 + 6)) else None).flatMap { case (_r5196, _p5197) => (if (parseIdentCont(input, _p5197).isEmpty) Some(((), _p5197)) else None).map { case (_r5198, _p5199) => (new ~(_r5196, _r5198), _p5199) } }.flatMap { case (_r5192, _p5193) => parseSpacing(input, _p5193).map { case (_r5194, _p5195) => (new ~(_r5192, _r5194), _p5195) } }.flatMap { case (_r5188, _p5189) => {
  var _rs5200: List[Any] = Nil; var _cp5201: Int = _p5189; var _go5204 = true
  while (_go5204) { parseStatementSep(input, _cp5201) match {
    case Some((_st5202, _np5203)) => _rs5200 = _rs5200 :+ _st5202; _cp5201 = _np5203
    case None => _go5204 = false } }
  Some((_rs5200, _cp5201)) }.map { case (_r5190, _p5191) => (new ~(_r5188, _r5190), _p5191) } }.flatMap { case (_r5184, _p5185) => {
  var _rs5205: List[Any] = Nil; var _cp5206: Int = _p5185; var _go5209 = true
  while (_go5209) { (if (parseEndKeyword(input, _cp5206).isEmpty) Some(((), _cp5206)) else None).flatMap { case (_r5214, _p5215) => parseStatement(input, _p5215).map { case (_r5216, _p5217) => (new ~(_r5214, _r5216), _p5217) } }.flatMap { case (_r5210, _p5211) => {
  var _rs5218: List[Any] = Nil; var _cp5219: Int = _p5211; var _go5222 = true
  while (_go5222) { parseStatementSep(input, _cp5219) match {
    case Some((_st5220, _np5221)) => _rs5218 = _rs5218 :+ _st5220; _cp5219 = _np5221
    case None => _go5222 = false } }
  Some((_rs5218, _cp5219)) }.map { case (_r5212, _p5213) => (new ~(_r5210, _r5212), _p5213) } } match {
    case Some((_st5207, _np5208)) => _rs5205 = _rs5205 :+ _st5207; _cp5206 = _np5208
    case None => _go5209 = false } }
  Some((_rs5205, _cp5206)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r5186, _p5187) => (new ~(_r5184, _r5186), _p5187) } }.flatMap { case (_r5180, _p5181) => {
  var _rs5223: List[Any] = Nil; var _cp5224: Int = _p5181; var _go5227 = true
  while (_go5227) { parseStatementSep(input, _cp5224) match {
    case Some((_st5225, _np5226)) => _rs5223 = _rs5223 :+ _st5225; _cp5224 = _np5226
    case None => _go5227 = false } }
  Some((_rs5223, _cp5224)) }.map { case (_r5182, _p5183) => (new ~(_r5180, _r5182), _p5183) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5059)))).map { case (_r5060, _p5061) => (new ~(_r5058, _r5060), _p5061) } }.flatMap { case (_r5054, _p5055) => parseEndKeyword(input, _p5055).map { case (_r5056, _p5057) => (new ~(_r5054, _r5056), _p5057) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseIfStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r5260, _p5261) => (if (parseIdentCont(input, _p5261).isEmpty) Some(((), _p5261)) else None).map { case (_r5262, _p5263) => (new ~(_r5260, _r5262), _p5263) } }.flatMap { case (_r5256, _p5257) => parseSpacing(input, _p5257).map { case (_r5258, _p5259) => (new ~(_r5256, _r5258), _p5259) } }.flatMap { case (_r5252, _p5253) => parseExpr(input, _p5253).map { case (_r5254, _p5255) => (new ~(_r5252, _r5254), _p5255) } }.flatMap { case (_r5248, _p5249) => (((if (input.startsWith("then", _p5249)) Some(("then", _p5249 + 4)) else None).flatMap { case (_r5268, _p5269) => (if (parseIdentCont(input, _p5269).isEmpty) Some(((), _p5269)) else None).map { case (_r5270, _p5271) => (new ~(_r5268, _r5270), _p5271) } }.flatMap { case (_r5264, _p5265) => parseSpacing(input, _p5265).map { case (_r5266, _p5267) => (new ~(_r5264, _r5266), _p5267) } }).orElse(parseInlineSpacing(input, _p5249).flatMap { case (_r5272, _p5273) => {
  parseStatementSep(input, _p5273) match {
    case None => None
    case Some((_fs5276, _fp5277)) =>
      var _rs5278: List[Any] = List(_fs5276); var _cp5279: Int = _fp5277; var _go5282 = true
      while (_go5282) { parseStatementSep(input, _cp5279) match {
        case Some((_st5280, _np5281)) => _rs5278 = _rs5278 :+ _st5280; _cp5279 = _np5281
        case None => _go5282 = false } }
      Some((_rs5278, _cp5279)) } }.map { case (_r5274, _p5275) => (new ~(_r5272, _r5274), _p5275) } })).orElse(parseInlineSpacing(input, _p5249).flatMap { case (_r5283, _p5284) => (if (input.startsWith(";", _p5284)) Some((";", _p5284 + 1)) else None).map { case (_r5285, _p5286) => (new ~(_r5283, _r5285), _p5286) } }).map { case (_r5250, _p5251) => (new ~(_r5248, _r5250), _p5251) } }.flatMap { case (_r5244, _p5245) => {
  var _rs5287: List[Any] = Nil; var _cp5288: Int = _p5245; var _go5291 = true
  while (_go5291) { parseStatementSep(input, _cp5288) match {
    case Some((_st5289, _np5290)) => _rs5287 = _rs5287 :+ _st5289; _cp5288 = _np5290
    case None => _go5291 = false } }
  Some((_rs5287, _cp5288)) }.map { case (_r5246, _p5247) => (new ~(_r5244, _r5246), _p5247) } }.flatMap { case (_r5240, _p5241) => {
  var _rs5292: List[Any] = Nil; var _cp5293: Int = _p5241; var _go5296 = true
  while (_go5296) { (if ((((if (input.startsWith("elsif", _cp5293)) Some(("elsif", _cp5293 + 5)) else None).flatMap { case (_r5309, _p5310) => (if (parseIdentCont(input, _p5310).isEmpty) Some(((), _p5310)) else None).map { case (_r5311, _p5312) => (new ~(_r5309, _r5311), _p5312) } }.flatMap { case (_r5305, _p5306) => parseSpacing(input, _p5306).map { case (_r5307, _p5308) => (new ~(_r5305, _r5307), _p5308) } }).orElse((if (input.startsWith("else", _cp5293)) Some(("else", _cp5293 + 4)) else None).flatMap { case (_r5317, _p5318) => (if (parseIdentCont(input, _p5318).isEmpty) Some(((), _p5318)) else None).map { case (_r5319, _p5320) => (new ~(_r5317, _r5319), _p5320) } }.flatMap { case (_r5313, _p5314) => parseSpacing(input, _p5314).map { case (_r5315, _p5316) => (new ~(_r5313, _r5315), _p5316) } })).orElse(parseEndKeyword(input, _cp5293)).isEmpty) Some(((), _cp5293)) else None).flatMap { case (_r5301, _p5302) => parseStatement(input, _p5302).map { case (_r5303, _p5304) => (new ~(_r5301, _r5303), _p5304) } }.flatMap { case (_r5297, _p5298) => {
  var _rs5321: List[Any] = Nil; var _cp5322: Int = _p5298; var _go5325 = true
  while (_go5325) { parseStatementSep(input, _cp5322) match {
    case Some((_st5323, _np5324)) => _rs5321 = _rs5321 :+ _st5323; _cp5322 = _np5324
    case None => _go5325 = false } }
  Some((_rs5321, _cp5322)) }.map { case (_r5299, _p5300) => (new ~(_r5297, _r5299), _p5300) } } match {
    case Some((_st5294, _np5295)) => _rs5292 = _rs5292 :+ _st5294; _cp5293 = _np5295
    case None => _go5296 = false } }
  Some((_rs5292, _cp5293)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r5242, _p5243) => (new ~(_r5240, _r5242), _p5243) } }.flatMap { case (_r5236, _p5237) => {
  var _rs5326: List[Any] = Nil; var _cp5327: Int = _p5237; var _go5330 = true
  while (_go5330) { parseStatementSep(input, _cp5327) match {
    case Some((_st5328, _np5329)) => _rs5326 = _rs5326 :+ _st5328; _cp5327 = _np5329
    case None => _go5330 = false } }
  Some((_rs5326, _cp5327)) }.map { case (_r5238, _p5239) => (new ~(_r5236, _r5238), _p5239) } }.flatMap { case (_r5232, _p5233) => parseIfElsifElse(input, _p5233).map { case (_r5234, _p5235) => (new ~(_r5232, _r5234), _p5235) } }.flatMap { case (_r5228, _p5229) => parseEndKeyword(input, _p5229).map { case (_r5230, _p5231) => (new ~(_r5228, _r5230), _p5231) } }.map { case (r, p) => (_applyAction({  _ => IfExpr(NilLiteral(), List.empty, List.empty)  }, r), p) }

  def parseUnlessStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r5363, _p5364) => (if (parseIdentCont(input, _p5364).isEmpty) Some(((), _p5364)) else None).map { case (_r5365, _p5366) => (new ~(_r5363, _r5365), _p5366) } }.flatMap { case (_r5359, _p5360) => parseSpacing(input, _p5360).map { case (_r5361, _p5362) => (new ~(_r5359, _r5361), _p5362) } }.flatMap { case (_r5355, _p5356) => parseExpr(input, _p5356).map { case (_r5357, _p5358) => (new ~(_r5355, _r5357), _p5358) } }.flatMap { case (_r5351, _p5352) => (((if (input.startsWith("then", _p5352)) Some(("then", _p5352 + 4)) else None).flatMap { case (_r5371, _p5372) => (if (parseIdentCont(input, _p5372).isEmpty) Some(((), _p5372)) else None).map { case (_r5373, _p5374) => (new ~(_r5371, _r5373), _p5374) } }.flatMap { case (_r5367, _p5368) => parseSpacing(input, _p5368).map { case (_r5369, _p5370) => (new ~(_r5367, _r5369), _p5370) } }).orElse(parseInlineSpacing(input, _p5352).flatMap { case (_r5375, _p5376) => {
  parseStatementSep(input, _p5376) match {
    case None => None
    case Some((_fs5379, _fp5380)) =>
      var _rs5381: List[Any] = List(_fs5379); var _cp5382: Int = _fp5380; var _go5385 = true
      while (_go5385) { parseStatementSep(input, _cp5382) match {
        case Some((_st5383, _np5384)) => _rs5381 = _rs5381 :+ _st5383; _cp5382 = _np5384
        case None => _go5385 = false } }
      Some((_rs5381, _cp5382)) } }.map { case (_r5377, _p5378) => (new ~(_r5375, _r5377), _p5378) } })).orElse(parseInlineSpacing(input, _p5352).flatMap { case (_r5386, _p5387) => (if (input.startsWith(";", _p5387)) Some((";", _p5387 + 1)) else None).map { case (_r5388, _p5389) => (new ~(_r5386, _r5388), _p5389) } }).map { case (_r5353, _p5354) => (new ~(_r5351, _r5353), _p5354) } }.flatMap { case (_r5347, _p5348) => {
  var _rs5390: List[Any] = Nil; var _cp5391: Int = _p5348; var _go5394 = true
  while (_go5394) { parseStatementSep(input, _cp5391) match {
    case Some((_st5392, _np5393)) => _rs5390 = _rs5390 :+ _st5392; _cp5391 = _np5393
    case None => _go5394 = false } }
  Some((_rs5390, _cp5391)) }.map { case (_r5349, _p5350) => (new ~(_r5347, _r5349), _p5350) } }.flatMap { case (_r5343, _p5344) => {
  var _rs5395: List[Any] = Nil; var _cp5396: Int = _p5344; var _go5399 = true
  while (_go5399) { (if (((if (input.startsWith("else", _cp5396)) Some(("else", _cp5396 + 4)) else None).flatMap { case (_r5412, _p5413) => (if (parseIdentCont(input, _p5413).isEmpty) Some(((), _p5413)) else None).map { case (_r5414, _p5415) => (new ~(_r5412, _r5414), _p5415) } }.flatMap { case (_r5408, _p5409) => parseSpacing(input, _p5409).map { case (_r5410, _p5411) => (new ~(_r5408, _r5410), _p5411) } }).orElse(parseEndKeyword(input, _cp5396)).isEmpty) Some(((), _cp5396)) else None).flatMap { case (_r5404, _p5405) => parseStatement(input, _p5405).map { case (_r5406, _p5407) => (new ~(_r5404, _r5406), _p5407) } }.flatMap { case (_r5400, _p5401) => {
  var _rs5416: List[Any] = Nil; var _cp5417: Int = _p5401; var _go5420 = true
  while (_go5420) { parseStatementSep(input, _cp5417) match {
    case Some((_st5418, _np5419)) => _rs5416 = _rs5416 :+ _st5418; _cp5417 = _np5419
    case None => _go5420 = false } }
  Some((_rs5416, _cp5417)) }.map { case (_r5402, _p5403) => (new ~(_r5400, _r5402), _p5403) } } match {
    case Some((_st5397, _np5398)) => _rs5395 = _rs5395 :+ _st5397; _cp5396 = _np5398
    case None => _go5399 = false } }
  Some((_rs5395, _cp5396)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r5345, _p5346) => (new ~(_r5343, _r5345), _p5346) } }.flatMap { case (_r5339, _p5340) => {
  var _rs5421: List[Any] = Nil; var _cp5422: Int = _p5340; var _go5425 = true
  while (_go5425) { parseStatementSep(input, _cp5422) match {
    case Some((_st5423, _np5424)) => _rs5421 = _rs5421 :+ _st5423; _cp5422 = _np5424
    case None => _go5425 = false } }
  Some((_rs5421, _cp5422)) }.map { case (_r5341, _p5342) => (new ~(_r5339, _r5341), _p5342) } }.flatMap { case (_r5335, _p5336) => ((if (input.startsWith("else", _p5336)) Some(("else", _p5336 + 4)) else None).flatMap { case (_r5442, _p5443) => (if (parseIdentCont(input, _p5443).isEmpty) Some(((), _p5443)) else None).map { case (_r5444, _p5445) => (new ~(_r5442, _r5444), _p5445) } }.flatMap { case (_r5438, _p5439) => parseSpacing(input, _p5439).map { case (_r5440, _p5441) => (new ~(_r5438, _r5440), _p5441) } }.flatMap { case (_r5434, _p5435) => {
  var _rs5446: List[Any] = Nil; var _cp5447: Int = _p5435; var _go5450 = true
  while (_go5450) { parseStatementSep(input, _cp5447) match {
    case Some((_st5448, _np5449)) => _rs5446 = _rs5446 :+ _st5448; _cp5447 = _np5449
    case None => _go5450 = false } }
  Some((_rs5446, _cp5447)) }.map { case (_r5436, _p5437) => (new ~(_r5434, _r5436), _p5437) } }.flatMap { case (_r5430, _p5431) => {
  var _rs5451: List[Any] = Nil; var _cp5452: Int = _p5431; var _go5455 = true
  while (_go5455) { (if (parseEndKeyword(input, _cp5452).isEmpty) Some(((), _cp5452)) else None).flatMap { case (_r5460, _p5461) => parseStatement(input, _p5461).map { case (_r5462, _p5463) => (new ~(_r5460, _r5462), _p5463) } }.flatMap { case (_r5456, _p5457) => {
  var _rs5464: List[Any] = Nil; var _cp5465: Int = _p5457; var _go5468 = true
  while (_go5468) { parseStatementSep(input, _cp5465) match {
    case Some((_st5466, _np5467)) => _rs5464 = _rs5464 :+ _st5466; _cp5465 = _np5467
    case None => _go5468 = false } }
  Some((_rs5464, _cp5465)) }.map { case (_r5458, _p5459) => (new ~(_r5456, _r5458), _p5459) } } match {
    case Some((_st5453, _np5454)) => _rs5451 = _rs5451 :+ _st5453; _cp5452 = _np5454
    case None => _go5455 = false } }
  Some((_rs5451, _cp5452)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r5432, _p5433) => (new ~(_r5430, _r5432), _p5433) } }.flatMap { case (_r5426, _p5427) => {
  var _rs5469: List[Any] = Nil; var _cp5470: Int = _p5427; var _go5473 = true
  while (_go5473) { parseStatementSep(input, _cp5470) match {
    case Some((_st5471, _np5472)) => _rs5469 = _rs5469 :+ _st5471; _cp5470 = _np5472
    case None => _go5473 = false } }
  Some((_rs5469, _cp5470)) }.map { case (_r5428, _p5429) => (new ~(_r5426, _r5428), _p5429) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5336)))).map { case (_r5337, _p5338) => (new ~(_r5335, _r5337), _p5338) } }.flatMap { case (_r5331, _p5332) => parseEndKeyword(input, _p5332).map { case (_r5333, _p5334) => (new ~(_r5331, _r5333), _p5334) } }.map { case (r, p) => (_applyAction({  _ => UnlessExpr(NilLiteral(), List.empty, List.empty)  }, r), p) }

  def parseCaseStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("case", pos)) Some(("case", pos + 4)) else None).flatMap { case (_r5498, _p5499) => (if (parseIdentCont(input, _p5499).isEmpty) Some(((), _p5499)) else None).map { case (_r5500, _p5501) => (new ~(_r5498, _r5500), _p5501) } }.flatMap { case (_r5494, _p5495) => parseSpacing(input, _p5495).map { case (_r5496, _p5497) => (new ~(_r5494, _r5496), _p5497) } }.flatMap { case (_r5490, _p5491) => (parseExpr(input, _p5491).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5491)))).map { case (_r5492, _p5493) => (new ~(_r5490, _r5492), _p5493) } }.flatMap { case (_r5486, _p5487) => {
  var _rs5502: List[Any] = Nil; var _cp5503: Int = _p5487; var _go5506 = true
  while (_go5506) { parseStatementSep(input, _cp5503) match {
    case Some((_st5504, _np5505)) => _rs5502 = _rs5502 :+ _st5504; _cp5503 = _np5505
    case None => _go5506 = false } }
  Some((_rs5502, _cp5503)) }.map { case (_r5488, _p5489) => (new ~(_r5486, _r5488), _p5489) } }.flatMap { case (_r5482, _p5483) => {
  parseWhenClause(input, _p5483) match {
    case None => None
    case Some((_fs5507, _fp5508)) =>
      var _rs5509: List[Any] = List(_fs5507); var _cp5510: Int = _fp5508; var _go5513 = true
      while (_go5513) { parseWhenClause(input, _cp5510) match {
        case Some((_st5511, _np5512)) => _rs5509 = _rs5509 :+ _st5511; _cp5510 = _np5512
        case None => _go5513 = false } }
      Some((_rs5509, _cp5510)) } }.map { case (_r5484, _p5485) => (new ~(_r5482, _r5484), _p5485) } }.flatMap { case (_r5478, _p5479) => ((if (input.startsWith("else", _p5479)) Some(("else", _p5479 + 4)) else None).flatMap { case (_r5530, _p5531) => (if (parseIdentCont(input, _p5531).isEmpty) Some(((), _p5531)) else None).map { case (_r5532, _p5533) => (new ~(_r5530, _r5532), _p5533) } }.flatMap { case (_r5526, _p5527) => parseSpacing(input, _p5527).map { case (_r5528, _p5529) => (new ~(_r5526, _r5528), _p5529) } }.flatMap { case (_r5522, _p5523) => {
  var _rs5534: List[Any] = Nil; var _cp5535: Int = _p5523; var _go5538 = true
  while (_go5538) { parseStatementSep(input, _cp5535) match {
    case Some((_st5536, _np5537)) => _rs5534 = _rs5534 :+ _st5536; _cp5535 = _np5537
    case None => _go5538 = false } }
  Some((_rs5534, _cp5535)) }.map { case (_r5524, _p5525) => (new ~(_r5522, _r5524), _p5525) } }.flatMap { case (_r5518, _p5519) => {
  var _rs5539: List[Any] = Nil; var _cp5540: Int = _p5519; var _go5543 = true
  while (_go5543) { (if (parseEndKeyword(input, _cp5540).isEmpty) Some(((), _cp5540)) else None).flatMap { case (_r5548, _p5549) => parseStatement(input, _p5549).map { case (_r5550, _p5551) => (new ~(_r5548, _r5550), _p5551) } }.flatMap { case (_r5544, _p5545) => {
  var _rs5552: List[Any] = Nil; var _cp5553: Int = _p5545; var _go5556 = true
  while (_go5556) { parseStatementSep(input, _cp5553) match {
    case Some((_st5554, _np5555)) => _rs5552 = _rs5552 :+ _st5554; _cp5553 = _np5555
    case None => _go5556 = false } }
  Some((_rs5552, _cp5553)) }.map { case (_r5546, _p5547) => (new ~(_r5544, _r5546), _p5547) } } match {
    case Some((_st5541, _np5542)) => _rs5539 = _rs5539 :+ _st5541; _cp5540 = _np5542
    case None => _go5543 = false } }
  Some((_rs5539, _cp5540)) }.map { case (r, p) => (_applyAction({  stmts => stmts.asInstanceOf[List[Any]].map { case _ ~ stmt ~ _ => stmt.asInstanceOf[Statement] }  }, r), p) }.map { case (_r5520, _p5521) => (new ~(_r5518, _r5520), _p5521) } }.flatMap { case (_r5514, _p5515) => {
  var _rs5557: List[Any] = Nil; var _cp5558: Int = _p5515; var _go5561 = true
  while (_go5561) { parseStatementSep(input, _cp5558) match {
    case Some((_st5559, _np5560)) => _rs5557 = _rs5557 :+ _st5559; _cp5558 = _np5560
    case None => _go5561 = false } }
  Some((_rs5557, _cp5558)) }.map { case (_r5516, _p5517) => (new ~(_r5514, _r5516), _p5517) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5479)))).map { case (_r5480, _p5481) => (new ~(_r5478, _r5480), _p5481) } }.flatMap { case (_r5474, _p5475) => parseEndKeyword(input, _p5475).map { case (_r5476, _p5477) => (new ~(_r5474, _r5476), _p5477) } }.map { case (r, p) => (_applyAction({  _ => CaseExpr(None, List.empty, List.empty)  }, r), p) }

  def parseRightwardAssignStmt(input: String, pos: Int): Option[(Any, Int)] = parseConditionalExpr(input, pos).flatMap { case (_r5574, _p5575) => parseSpacing(input, _p5575).map { case (_r5576, _p5577) => (new ~(_r5574, _r5576), _p5577) } }.flatMap { case (_r5570, _p5571) => (if (input.startsWith("=>", _p5571)) Some(("=>", _p5571 + 2)) else None).map { case (_r5572, _p5573) => (new ~(_r5570, _r5572), _p5573) } }.flatMap { case (_r5566, _p5567) => parseSpacing(input, _p5567).map { case (_r5568, _p5569) => (new ~(_r5566, _r5568), _p5569) } }.flatMap { case (_r5562, _p5563) => parseInPatternExpr(input, _p5563).map { case (_r5564, _p5565) => (new ~(_r5562, _r5564), _p5565) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseExprStatement(input: String, pos: Int): Option[(Any, Int)] = parseExpr(input, pos).flatMap { case (_r5582, _p5583) => (parseSpacing1(input, _p5583).flatMap { case (_r5586, _p5587) => parseCommandArgs(input, _p5587).map { case (_r5588, _p5589) => (new ~(_r5586, _r5588), _p5589) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5583)))).map { case (_r5584, _p5585) => (new ~(_r5582, _r5584), _p5585) } }.flatMap { case (_r5578, _p5579) => parseInlineSpacing(input, _p5579).map { case (_r5580, _p5581) => (new ~(_r5578, _r5580), _p5581) } }.map { case (r, p) => (_applyAction({  case (e ~ _) ~ _ => ExprStmt(e.asInstanceOf[Expr])  }, r), p) }

  def parseStatement(input: String, pos: Int): Option[(Any, Int)] = _withMemo(47, pos) {
    ((((((((((((((((((parseReturnStmt(input, pos)).orElse(parseRetryStmt(input, pos))).orElse(parseBreakStmt(input, pos))).orElse(parseNextStmt(input, pos))).orElse(parseYieldStmt(input, pos))).orElse(parseAliasStmt(input, pos))).orElse(parseBeginStmt(input, pos))).orElse(parseWhileStmt(input, pos))).orElse(parseUntilStmt(input, pos))).orElse(parseForStmt(input, pos))).orElse(parseCaseStmt(input, pos))).orElse(parseSingletonClassStmt(input, pos))).orElse(parseClassStmt(input, pos))).orElse(parseModuleStmt(input, pos))).orElse(parseDefStmt(input, pos))).orElse(parseIfStmt(input, pos))).orElse(parseUnlessStmt(input, pos))).orElse(parseRightwardAssignStmt(input, pos))).orElse(parseExprStatement(input, pos)).flatMap { case (_r5590, _p5591) => {
  var _rs5594: List[Any] = Nil; var _cp5595: Int = _p5591; var _go5598 = true
  while (_go5598) { parseInlineSpacing(input, _cp5595).flatMap { case (_r5599, _p5600) => parseModifierSuffix(input, _p5600).map { case (_r5601, _p5602) => (new ~(_r5599, _r5601), _p5602) } } match {
    case Some((_st5596, _np5597)) => _rs5594 = _rs5594 :+ _st5596; _cp5595 = _np5597
    case None => _go5598 = false } }
  Some((_rs5594, _cp5595)) }.map { case (_r5592, _p5593) => (new ~(_r5590, _r5592), _p5593) } }.map { case (r, p) => (_applyAction({  case stmt ~ _ => stmt.asInstanceOf[Statement]  }, r), p) }
  }

  def parseEndDataSection(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("__END__", pos)) Some(("__END__", pos + 7)) else None).flatMap { case (_r5607, _p5608) => (if (parseIdentCont(input, _p5608).isEmpty) Some(((), _p5608)) else None).map { case (_r5609, _p5610) => (new ~(_r5607, _r5609), _p5610) } }.flatMap { case (_r5603, _p5604) => {
  var _rs5611: List[Any] = Nil; var _cp5612: Int = _p5604; var _go5615 = true
  while (_go5615) { (if (_cp5612 < input.length) Some((input.charAt(_cp5612).toString, _cp5612 + 1)) else None) match {
    case Some((_st5613, _np5614)) => _rs5611 = _rs5611 :+ _st5613; _cp5612 = _np5614
    case None => _go5615 = false } }
  Some((_rs5611, _cp5612)) }.map { case (_r5605, _p5606) => (new ~(_r5603, _r5605), _p5606) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseTopLevelStatements(input: String, pos: Int): Option[(Any, Int)] = {
  var _rs5624: List[Any] = Nil; var _cp5625: Int = pos; var _go5628 = true
  while (_go5628) { parseStatementSep(input, _cp5625) match {
    case Some((_st5626, _np5627)) => _rs5624 = _rs5624 :+ _st5626; _cp5625 = _np5627
    case None => _go5628 = false } }
  Some((_rs5624, _cp5625)) }.flatMap { case (_r5620, _p5621) => ((if (parseEndDataSection(input, _p5621).isEmpty) Some(((), _p5621)) else None).flatMap { case (_r5633, _p5634) => parseStatement(input, _p5634).map { case (_r5635, _p5636) => (new ~(_r5633, _r5635), _p5636) } }.flatMap { case (_r5629, _p5630) => {
  var _rs5637: List[Any] = Nil; var _cp5638: Int = _p5630; var _go5641 = true
  while (_go5641) { {
  parseStatementSep(input, _cp5638) match {
    case None => None
    case Some((_fs5650, _fp5651)) =>
      var _rs5652: List[Any] = List(_fs5650); var _cp5653: Int = _fp5651; var _go5656 = true
      while (_go5656) { parseStatementSep(input, _cp5653) match {
        case Some((_st5654, _np5655)) => _rs5652 = _rs5652 :+ _st5654; _cp5653 = _np5655
        case None => _go5656 = false } }
      Some((_rs5652, _cp5653)) } }.flatMap { case (_r5646, _p5647) => (if (parseEndDataSection(input, _p5647).isEmpty) Some(((), _p5647)) else None).map { case (_r5648, _p5649) => (new ~(_r5646, _r5648), _p5649) } }.flatMap { case (_r5642, _p5643) => parseStatement(input, _p5643).map { case (_r5644, _p5645) => (new ~(_r5642, _r5644), _p5645) } } match {
    case Some((_st5639, _np5640)) => _rs5637 = _rs5637 :+ _st5639; _cp5638 = _np5640
    case None => _go5641 = false } }
  Some((_rs5637, _cp5638)) }.map { case (_r5631, _p5632) => (new ~(_r5629, _r5631), _p5632) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5621)))).map { case (_r5622, _p5623) => (new ~(_r5620, _r5622), _p5623) } }.flatMap { case (_r5616, _p5617) => {
  var _rs5657: List[Any] = Nil; var _cp5658: Int = _p5617; var _go5661 = true
  while (_go5661) { parseStatementSep(input, _cp5658) match {
    case Some((_st5659, _np5660)) => _rs5657 = _rs5657 :+ _st5659; _cp5658 = _np5660
    case None => _go5661 = false } }
  Some((_rs5657, _cp5658)) }.map { case (_r5618, _p5619) => (new ~(_r5616, _r5618), _p5619) } }.map { case (r, p) => (_applyAction({  v => { val (_ ~ opt) ~ _ = v; opt.asInstanceOf[Option[Any]] match { case None => List.empty[Statement]; case Some(inner) => { val (_ ~ first) ~ rest = inner; first.asInstanceOf[Statement] :: rest.asInstanceOf[List[Any]].map { case _ ~ stmt => stmt.asInstanceOf[Statement] } } } }  }, r), p) }

  def parseProgram(input: String, pos: Int): Option[(Any, Int)] = parseSpacing(input, pos).flatMap { case (_, _p5668) => parseTopLevelStatements(input, _p5668) }.flatMap { case (_r5665, _p5666) => parseSpacing(input, _p5666).map { case (_, _p5667) => (_r5665, _p5667) } }.flatMap { case (_r5662, _p5663) => (parseEndDataSection(input, _p5663).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5663)))).map { case (_, _p5664) => (_r5662, _p5664) } }.map { case (r, p) => (_applyAction({  stmts => Program(stmts.asInstanceOf[List[Statement]])  }, r), p) }

  def parse(input: String): Option[(Any, Int)] = {
    resetMemo()
    val _in = input
    parseProgram(_in, 0)
  }

  def parseAll(input: String): Either[String, Any] = {
    resetMemo()
    val _in = input
    parseProgram(_in, 0) match {
      case Some((result, pos)) if pos == _in.length => Right(result)
      case Some((_, pos)) => Left(s"Unconsumed input at position $pos")
      case None => Left("Parse failed")
    }
  }
}
