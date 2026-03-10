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
          return Some(("", end))
        }
      }
      // Advance to next line
      while (p < input.length && input.charAt(p) != '\n') p += 1
      if (p < input.length) p += 1
      if (p == lineStart) return None // guard: no progress (EOF edge case)
    }
    None

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

  def parsePercentBodyBraces(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r489, _p490) => {
  var _rs493: List[Any] = Nil; var _cp494: Int = _p490; var _go497 = true
  while (_go497) { (((if (input.startsWith("\\", _cp494)) Some(("\\", _cp494 + 1)) else None).flatMap { case (_r498, _p499) => (if (_p499 < input.length) Some((input.charAt(_p499).toString, _p499 + 1)) else None).map { case (_r500, _p501) => (new ~(_r498, _r500), _p501) } }).orElse(parsePercentBodyBraces(input, _cp494))).orElse((if ((if (input.startsWith("}", _cp494)) Some(("}", _cp494 + 1)) else None).isEmpty) Some(((), _cp494)) else None).flatMap { case (_r502, _p503) => (if (_p503 < input.length) Some((input.charAt(_p503).toString, _p503 + 1)) else None).map { case (_r504, _p505) => (new ~(_r502, _r504), _p505) } }) match {
    case Some((_st495, _np496)) => _rs493 = _rs493 :+ _st495; _cp494 = _np496
    case None => _go497 = false } }
  Some((_rs493, _cp494)) }.map { case (_r491, _p492) => (new ~(_r489, _r491), _p492) } }.flatMap { case (_r485, _p486) => (if (input.startsWith("}", _p486)) Some(("}", _p486 + 1)) else None).map { case (_r487, _p488) => (new ~(_r485, _r487), _p488) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodyParens(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r510, _p511) => {
  var _rs514: List[Any] = Nil; var _cp515: Int = _p511; var _go518 = true
  while (_go518) { (((if (input.startsWith("\\", _cp515)) Some(("\\", _cp515 + 1)) else None).flatMap { case (_r519, _p520) => (if (_p520 < input.length) Some((input.charAt(_p520).toString, _p520 + 1)) else None).map { case (_r521, _p522) => (new ~(_r519, _r521), _p522) } }).orElse(parsePercentBodyParens(input, _cp515))).orElse((if ((if (input.startsWith(")", _cp515)) Some((")", _cp515 + 1)) else None).isEmpty) Some(((), _cp515)) else None).flatMap { case (_r523, _p524) => (if (_p524 < input.length) Some((input.charAt(_p524).toString, _p524 + 1)) else None).map { case (_r525, _p526) => (new ~(_r523, _r525), _p526) } }) match {
    case Some((_st516, _np517)) => _rs514 = _rs514 :+ _st516; _cp515 = _np517
    case None => _go518 = false } }
  Some((_rs514, _cp515)) }.map { case (_r512, _p513) => (new ~(_r510, _r512), _p513) } }.flatMap { case (_r506, _p507) => (if (input.startsWith(")", _p507)) Some((")", _p507 + 1)) else None).map { case (_r508, _p509) => (new ~(_r506, _r508), _p509) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodyBrackets(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("[", pos)) Some(("[", pos + 1)) else None).flatMap { case (_r531, _p532) => {
  var _rs535: List[Any] = Nil; var _cp536: Int = _p532; var _go539 = true
  while (_go539) { (((if (input.startsWith("\\", _cp536)) Some(("\\", _cp536 + 1)) else None).flatMap { case (_r540, _p541) => (if (_p541 < input.length) Some((input.charAt(_p541).toString, _p541 + 1)) else None).map { case (_r542, _p543) => (new ~(_r540, _r542), _p543) } }).orElse(parsePercentBodyBrackets(input, _cp536))).orElse((if ((if (input.startsWith("]", _cp536)) Some(("]", _cp536 + 1)) else None).isEmpty) Some(((), _cp536)) else None).flatMap { case (_r544, _p545) => (if (_p545 < input.length) Some((input.charAt(_p545).toString, _p545 + 1)) else None).map { case (_r546, _p547) => (new ~(_r544, _r546), _p547) } }) match {
    case Some((_st537, _np538)) => _rs535 = _rs535 :+ _st537; _cp536 = _np538
    case None => _go539 = false } }
  Some((_rs535, _cp536)) }.map { case (_r533, _p534) => (new ~(_r531, _r533), _p534) } }.flatMap { case (_r527, _p528) => (if (input.startsWith("]", _p528)) Some(("]", _p528 + 1)) else None).map { case (_r529, _p530) => (new ~(_r527, _r529), _p530) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodyAngles(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("<", pos)) Some(("<", pos + 1)) else None).flatMap { case (_r552, _p553) => {
  var _rs556: List[Any] = Nil; var _cp557: Int = _p553; var _go560 = true
  while (_go560) { (((if (input.startsWith("\\", _cp557)) Some(("\\", _cp557 + 1)) else None).flatMap { case (_r561, _p562) => (if (_p562 < input.length) Some((input.charAt(_p562).toString, _p562 + 1)) else None).map { case (_r563, _p564) => (new ~(_r561, _r563), _p564) } }).orElse(parsePercentBodyAngles(input, _cp557))).orElse((if ((if (input.startsWith(">", _cp557)) Some((">", _cp557 + 1)) else None).isEmpty) Some(((), _cp557)) else None).flatMap { case (_r565, _p566) => (if (_p566 < input.length) Some((input.charAt(_p566).toString, _p566 + 1)) else None).map { case (_r567, _p568) => (new ~(_r565, _r567), _p568) } }) match {
    case Some((_st558, _np559)) => _rs556 = _rs556 :+ _st558; _cp557 = _np559
    case None => _go560 = false } }
  Some((_rs556, _cp557)) }.map { case (_r554, _p555) => (new ~(_r552, _r554), _p555) } }.flatMap { case (_r548, _p549) => (if (input.startsWith(">", _p549)) Some((">", _p549 + 1)) else None).map { case (_r550, _p551) => (new ~(_r548, _r550), _p551) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimplePipe(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("|", pos)) Some(("|", pos + 1)) else None).flatMap { case (_r573, _p574) => {
  var _rs577: List[Any] = Nil; var _cp578: Int = _p574; var _go581 = true
  while (_go581) { ((if (input.startsWith("\\", _cp578)) Some(("\\", _cp578 + 1)) else None).flatMap { case (_r582, _p583) => (if (_p583 < input.length) Some((input.charAt(_p583).toString, _p583 + 1)) else None).map { case (_r584, _p585) => (new ~(_r582, _r584), _p585) } }).orElse((if ((if (input.startsWith("|", _cp578)) Some(("|", _cp578 + 1)) else None).isEmpty) Some(((), _cp578)) else None).flatMap { case (_r586, _p587) => (if (_p587 < input.length) Some((input.charAt(_p587).toString, _p587 + 1)) else None).map { case (_r588, _p589) => (new ~(_r586, _r588), _p589) } }) match {
    case Some((_st579, _np580)) => _rs577 = _rs577 :+ _st579; _cp578 = _np580
    case None => _go581 = false } }
  Some((_rs577, _cp578)) }.map { case (_r575, _p576) => (new ~(_r573, _r575), _p576) } }.flatMap { case (_r569, _p570) => (if (input.startsWith("|", _p570)) Some(("|", _p570 + 1)) else None).map { case (_r571, _p572) => (new ~(_r569, _r571), _p572) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimplePercent(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None).flatMap { case (_r594, _p595) => {
  var _rs598: List[Any] = Nil; var _cp599: Int = _p595; var _go602 = true
  while (_go602) { ((if (input.startsWith("\\", _cp599)) Some(("\\", _cp599 + 1)) else None).flatMap { case (_r603, _p604) => (if (_p604 < input.length) Some((input.charAt(_p604).toString, _p604 + 1)) else None).map { case (_r605, _p606) => (new ~(_r603, _r605), _p606) } }).orElse((if ((if (input.startsWith("%", _cp599)) Some(("%", _cp599 + 1)) else None).isEmpty) Some(((), _cp599)) else None).flatMap { case (_r607, _p608) => (if (_p608 < input.length) Some((input.charAt(_p608).toString, _p608 + 1)) else None).map { case (_r609, _p610) => (new ~(_r607, _r609), _p610) } }) match {
    case Some((_st600, _np601)) => _rs598 = _rs598 :+ _st600; _cp599 = _np601
    case None => _go602 = false } }
  Some((_rs598, _cp599)) }.map { case (_r596, _p597) => (new ~(_r594, _r596), _p597) } }.flatMap { case (_r590, _p591) => (if (input.startsWith("%", _p591)) Some(("%", _p591 + 1)) else None).map { case (_r592, _p593) => (new ~(_r590, _r592), _p593) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleDoubleQuote(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("\"", pos)) Some(("\"", pos + 1)) else None).flatMap { case (_r615, _p616) => {
  var _rs619: List[Any] = Nil; var _cp620: Int = _p616; var _go623 = true
  while (_go623) { ((if (input.startsWith("\\", _cp620)) Some(("\\", _cp620 + 1)) else None).flatMap { case (_r624, _p625) => (if (_p625 < input.length) Some((input.charAt(_p625).toString, _p625 + 1)) else None).map { case (_r626, _p627) => (new ~(_r624, _r626), _p627) } }).orElse((if ((if (input.startsWith("\"", _cp620)) Some(("\"", _cp620 + 1)) else None).isEmpty) Some(((), _cp620)) else None).flatMap { case (_r628, _p629) => (if (_p629 < input.length) Some((input.charAt(_p629).toString, _p629 + 1)) else None).map { case (_r630, _p631) => (new ~(_r628, _r630), _p631) } }) match {
    case Some((_st621, _np622)) => _rs619 = _rs619 :+ _st621; _cp620 = _np622
    case None => _go623 = false } }
  Some((_rs619, _cp620)) }.map { case (_r617, _p618) => (new ~(_r615, _r617), _p618) } }.flatMap { case (_r611, _p612) => (if (input.startsWith("\"", _p612)) Some(("\"", _p612 + 1)) else None).map { case (_r613, _p614) => (new ~(_r611, _r613), _p614) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleSingleQuote(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("'", pos)) Some(("'", pos + 1)) else None).flatMap { case (_r636, _p637) => {
  var _rs640: List[Any] = Nil; var _cp641: Int = _p637; var _go644 = true
  while (_go644) { ((if (input.startsWith("\\", _cp641)) Some(("\\", _cp641 + 1)) else None).flatMap { case (_r645, _p646) => (if (_p646 < input.length) Some((input.charAt(_p646).toString, _p646 + 1)) else None).map { case (_r647, _p648) => (new ~(_r645, _r647), _p648) } }).orElse((if ((if (input.startsWith("'", _cp641)) Some(("'", _cp641 + 1)) else None).isEmpty) Some(((), _cp641)) else None).flatMap { case (_r649, _p650) => (if (_p650 < input.length) Some((input.charAt(_p650).toString, _p650 + 1)) else None).map { case (_r651, _p652) => (new ~(_r649, _r651), _p652) } }) match {
    case Some((_st642, _np643)) => _rs640 = _rs640 :+ _st642; _cp641 = _np643
    case None => _go644 = false } }
  Some((_rs640, _cp641)) }.map { case (_r638, _p639) => (new ~(_r636, _r638), _p639) } }.flatMap { case (_r632, _p633) => (if (input.startsWith("'", _p633)) Some(("'", _p633 + 1)) else None).map { case (_r634, _p635) => (new ~(_r632, _r634), _p635) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleSlash(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("/", pos)) Some(("/", pos + 1)) else None).flatMap { case (_r657, _p658) => {
  var _rs661: List[Any] = Nil; var _cp662: Int = _p658; var _go665 = true
  while (_go665) { ((if (input.startsWith("\\", _cp662)) Some(("\\", _cp662 + 1)) else None).flatMap { case (_r666, _p667) => (if (_p667 < input.length) Some((input.charAt(_p667).toString, _p667 + 1)) else None).map { case (_r668, _p669) => (new ~(_r666, _r668), _p669) } }).orElse((if ((if (input.startsWith("/", _cp662)) Some(("/", _cp662 + 1)) else None).isEmpty) Some(((), _cp662)) else None).flatMap { case (_r670, _p671) => (if (_p671 < input.length) Some((input.charAt(_p671).toString, _p671 + 1)) else None).map { case (_r672, _p673) => (new ~(_r670, _r672), _p673) } }) match {
    case Some((_st663, _np664)) => _rs661 = _rs661 :+ _st663; _cp662 = _np664
    case None => _go665 = false } }
  Some((_rs661, _cp662)) }.map { case (_r659, _p660) => (new ~(_r657, _r659), _p660) } }.flatMap { case (_r653, _p654) => (if (input.startsWith("/", _p654)) Some(("/", _p654 + 1)) else None).map { case (_r655, _p656) => (new ~(_r653, _r655), _p656) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleColon(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith(":", pos)) Some((":", pos + 1)) else None).flatMap { case (_r678, _p679) => {
  var _rs682: List[Any] = Nil; var _cp683: Int = _p679; var _go686 = true
  while (_go686) { ((if (input.startsWith("\\", _cp683)) Some(("\\", _cp683 + 1)) else None).flatMap { case (_r687, _p688) => (if (_p688 < input.length) Some((input.charAt(_p688).toString, _p688 + 1)) else None).map { case (_r689, _p690) => (new ~(_r687, _r689), _p690) } }).orElse((if ((if (input.startsWith(":", _cp683)) Some((":", _cp683 + 1)) else None).isEmpty) Some(((), _cp683)) else None).flatMap { case (_r691, _p692) => (if (_p692 < input.length) Some((input.charAt(_p692).toString, _p692 + 1)) else None).map { case (_r693, _p694) => (new ~(_r691, _r693), _p694) } }) match {
    case Some((_st684, _np685)) => _rs682 = _rs682 :+ _st684; _cp683 = _np685
    case None => _go686 = false } }
  Some((_rs682, _cp683)) }.map { case (_r680, _p681) => (new ~(_r678, _r680), _p681) } }.flatMap { case (_r674, _p675) => (if (input.startsWith(":", _p675)) Some((":", _p675 + 1)) else None).map { case (_r676, _p677) => (new ~(_r674, _r676), _p677) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleExcl(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("!", pos)) Some(("!", pos + 1)) else None).flatMap { case (_r699, _p700) => {
  var _rs703: List[Any] = Nil; var _cp704: Int = _p700; var _go707 = true
  while (_go707) { ((if (input.startsWith("\\", _cp704)) Some(("\\", _cp704 + 1)) else None).flatMap { case (_r708, _p709) => (if (_p709 < input.length) Some((input.charAt(_p709).toString, _p709 + 1)) else None).map { case (_r710, _p711) => (new ~(_r708, _r710), _p711) } }).orElse((if ((if (input.startsWith("!", _cp704)) Some(("!", _cp704 + 1)) else None).isEmpty) Some(((), _cp704)) else None).flatMap { case (_r712, _p713) => (if (_p713 < input.length) Some((input.charAt(_p713).toString, _p713 + 1)) else None).map { case (_r714, _p715) => (new ~(_r712, _r714), _p715) } }) match {
    case Some((_st705, _np706)) => _rs703 = _rs703 :+ _st705; _cp704 = _np706
    case None => _go707 = false } }
  Some((_rs703, _cp704)) }.map { case (_r701, _p702) => (new ~(_r699, _r701), _p702) } }.flatMap { case (_r695, _p696) => (if (input.startsWith("!", _p696)) Some(("!", _p696 + 1)) else None).map { case (_r697, _p698) => (new ~(_r695, _r697), _p698) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleComma(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith(",", pos)) Some((",", pos + 1)) else None).flatMap { case (_r720, _p721) => {
  var _rs724: List[Any] = Nil; var _cp725: Int = _p721; var _go728 = true
  while (_go728) { ((if (input.startsWith("\\", _cp725)) Some(("\\", _cp725 + 1)) else None).flatMap { case (_r729, _p730) => (if (_p730 < input.length) Some((input.charAt(_p730).toString, _p730 + 1)) else None).map { case (_r731, _p732) => (new ~(_r729, _r731), _p732) } }).orElse((if ((if (input.startsWith(",", _cp725)) Some((",", _cp725 + 1)) else None).isEmpty) Some(((), _cp725)) else None).flatMap { case (_r733, _p734) => (if (_p734 < input.length) Some((input.charAt(_p734).toString, _p734 + 1)) else None).map { case (_r735, _p736) => (new ~(_r733, _r735), _p736) } }) match {
    case Some((_st726, _np727)) => _rs724 = _rs724 :+ _st726; _cp725 = _np727
    case None => _go728 = false } }
  Some((_rs724, _cp725)) }.map { case (_r722, _p723) => (new ~(_r720, _r722), _p723) } }.flatMap { case (_r716, _p717) => (if (input.startsWith(",", _p717)) Some((",", _p717 + 1)) else None).map { case (_r718, _p719) => (new ~(_r716, _r718), _p719) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleSemicolon(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith(";", pos)) Some((";", pos + 1)) else None).flatMap { case (_r741, _p742) => {
  var _rs745: List[Any] = Nil; var _cp746: Int = _p742; var _go749 = true
  while (_go749) { ((if (input.startsWith("\\", _cp746)) Some(("\\", _cp746 + 1)) else None).flatMap { case (_r750, _p751) => (if (_p751 < input.length) Some((input.charAt(_p751).toString, _p751 + 1)) else None).map { case (_r752, _p753) => (new ~(_r750, _r752), _p753) } }).orElse((if ((if (input.startsWith(";", _cp746)) Some((";", _cp746 + 1)) else None).isEmpty) Some(((), _cp746)) else None).flatMap { case (_r754, _p755) => (if (_p755 < input.length) Some((input.charAt(_p755).toString, _p755 + 1)) else None).map { case (_r756, _p757) => (new ~(_r754, _r756), _p757) } }) match {
    case Some((_st747, _np748)) => _rs745 = _rs745 :+ _st747; _cp746 = _np748
    case None => _go749 = false } }
  Some((_rs745, _cp746)) }.map { case (_r743, _p744) => (new ~(_r741, _r743), _p744) } }.flatMap { case (_r737, _p738) => (if (input.startsWith(";", _p738)) Some((";", _p738 + 1)) else None).map { case (_r739, _p740) => (new ~(_r737, _r739), _p740) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleDash(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("-", pos)) Some(("-", pos + 1)) else None).flatMap { case (_r762, _p763) => {
  var _rs766: List[Any] = Nil; var _cp767: Int = _p763; var _go770 = true
  while (_go770) { ((if (input.startsWith("\\", _cp767)) Some(("\\", _cp767 + 1)) else None).flatMap { case (_r771, _p772) => (if (_p772 < input.length) Some((input.charAt(_p772).toString, _p772 + 1)) else None).map { case (_r773, _p774) => (new ~(_r771, _r773), _p774) } }).orElse((if ((if (input.startsWith("-", _cp767)) Some(("-", _cp767 + 1)) else None).isEmpty) Some(((), _cp767)) else None).flatMap { case (_r775, _p776) => (if (_p776 < input.length) Some((input.charAt(_p776).toString, _p776 + 1)) else None).map { case (_r777, _p778) => (new ~(_r775, _r777), _p778) } }) match {
    case Some((_st768, _np769)) => _rs766 = _rs766 :+ _st768; _cp767 = _np769
    case None => _go770 = false } }
  Some((_rs766, _cp767)) }.map { case (_r764, _p765) => (new ~(_r762, _r764), _p765) } }.flatMap { case (_r758, _p759) => (if (input.startsWith("-", _p759)) Some(("-", _p759 + 1)) else None).map { case (_r760, _p761) => (new ~(_r758, _r760), _p761) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleUnderscore(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("_", pos)) Some(("_", pos + 1)) else None).flatMap { case (_r783, _p784) => {
  var _rs787: List[Any] = Nil; var _cp788: Int = _p784; var _go791 = true
  while (_go791) { ((if (input.startsWith("\\", _cp788)) Some(("\\", _cp788 + 1)) else None).flatMap { case (_r792, _p793) => (if (_p793 < input.length) Some((input.charAt(_p793).toString, _p793 + 1)) else None).map { case (_r794, _p795) => (new ~(_r792, _r794), _p795) } }).orElse((if ((if (input.startsWith("_", _cp788)) Some(("_", _cp788 + 1)) else None).isEmpty) Some(((), _cp788)) else None).flatMap { case (_r796, _p797) => (if (_p797 < input.length) Some((input.charAt(_p797).toString, _p797 + 1)) else None).map { case (_r798, _p799) => (new ~(_r796, _r798), _p799) } }) match {
    case Some((_st789, _np790)) => _rs787 = _rs787 :+ _st789; _cp788 = _np790
    case None => _go791 = false } }
  Some((_rs787, _cp788)) }.map { case (_r785, _p786) => (new ~(_r783, _r785), _p786) } }.flatMap { case (_r779, _p780) => (if (input.startsWith("_", _p780)) Some(("_", _p780 + 1)) else None).map { case (_r781, _p782) => (new ~(_r779, _r781), _p782) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimpleEquals(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("=", pos)) Some(("=", pos + 1)) else None).flatMap { case (_r804, _p805) => {
  var _rs808: List[Any] = Nil; var _cp809: Int = _p805; var _go812 = true
  while (_go812) { ((if (input.startsWith("\\", _cp809)) Some(("\\", _cp809 + 1)) else None).flatMap { case (_r813, _p814) => (if (_p814 < input.length) Some((input.charAt(_p814).toString, _p814 + 1)) else None).map { case (_r815, _p816) => (new ~(_r813, _r815), _p816) } }).orElse((if ((if (input.startsWith("=", _cp809)) Some(("=", _cp809 + 1)) else None).isEmpty) Some(((), _cp809)) else None).flatMap { case (_r817, _p818) => (if (_p818 < input.length) Some((input.charAt(_p818).toString, _p818 + 1)) else None).map { case (_r819, _p820) => (new ~(_r817, _r819), _p820) } }) match {
    case Some((_st810, _np811)) => _rs808 = _rs808 :+ _st810; _cp809 = _np811
    case None => _go812 = false } }
  Some((_rs808, _cp809)) }.map { case (_r806, _p807) => (new ~(_r804, _r806), _p807) } }.flatMap { case (_r800, _p801) => (if (input.startsWith("=", _p801)) Some(("=", _p801 + 1)) else None).map { case (_r802, _p803) => (new ~(_r800, _r802), _p803) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBodySimplePlus(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("+", pos)) Some(("+", pos + 1)) else None).flatMap { case (_r825, _p826) => {
  var _rs829: List[Any] = Nil; var _cp830: Int = _p826; var _go833 = true
  while (_go833) { ((if (input.startsWith("\\", _cp830)) Some(("\\", _cp830 + 1)) else None).flatMap { case (_r834, _p835) => (if (_p835 < input.length) Some((input.charAt(_p835).toString, _p835 + 1)) else None).map { case (_r836, _p837) => (new ~(_r834, _r836), _p837) } }).orElse((if ((if (input.startsWith("+", _cp830)) Some(("+", _cp830 + 1)) else None).isEmpty) Some(((), _cp830)) else None).flatMap { case (_r838, _p839) => (if (_p839 < input.length) Some((input.charAt(_p839).toString, _p839 + 1)) else None).map { case (_r840, _p841) => (new ~(_r838, _r840), _p841) } }) match {
    case Some((_st831, _np832)) => _rs829 = _rs829 :+ _st831; _cp830 = _np832
    case None => _go833 = false } }
  Some((_rs829, _cp830)) }.map { case (_r827, _p828) => (new ~(_r825, _r827), _p828) } }.flatMap { case (_r821, _p822) => (if (input.startsWith("+", _p822)) Some(("+", _p822 + 1)) else None).map { case (_r823, _p824) => (new ~(_r821, _r823), _p824) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentBody(input: String, pos: Int): Option[(Any, Int)] = ((((((((((((((((parsePercentBodyBraces(input, pos)).orElse(parsePercentBodyParens(input, pos))).orElse(parsePercentBodyBrackets(input, pos))).orElse(parsePercentBodyAngles(input, pos))).orElse(parsePercentBodySimplePipe(input, pos))).orElse(parsePercentBodySimplePercent(input, pos))).orElse(parsePercentBodySimpleDoubleQuote(input, pos))).orElse(parsePercentBodySimpleSingleQuote(input, pos))).orElse(parsePercentBodySimpleSlash(input, pos))).orElse(parsePercentBodySimpleColon(input, pos))).orElse(parsePercentBodySimpleExcl(input, pos))).orElse(parsePercentBodySimpleComma(input, pos))).orElse(parsePercentBodySimpleSemicolon(input, pos))).orElse(parsePercentBodySimpleDash(input, pos))).orElse(parsePercentBodySimpleUnderscore(input, pos))).orElse(parsePercentBodySimpleEquals(input, pos))).orElse(parsePercentBodySimplePlus(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePercentQuotedStringLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None).flatMap { case (_r854, _p855) => (if ((((((((if (input.startsWith("r", _p855)) Some(("r", _p855 + 1)) else None)).orElse((if (input.startsWith("w", _p855)) Some(("w", _p855 + 1)) else None))).orElse((if (input.startsWith("W", _p855)) Some(("W", _p855 + 1)) else None))).orElse((if (input.startsWith("i", _p855)) Some(("i", _p855 + 1)) else None))).orElse((if (input.startsWith("I", _p855)) Some(("I", _p855 + 1)) else None))).orElse((if (input.startsWith("s", _p855)) Some(("s", _p855 + 1)) else None))).orElse((if (input.startsWith("x", _p855)) Some(("x", _p855 + 1)) else None)).isEmpty) Some(((), _p855)) else None).map { case (_r856, _p857) => (new ~(_r854, _r856), _p857) } }.flatMap { case (_r850, _p851) => (((if (input.startsWith("q", _p851)) Some(("q", _p851 + 1)) else None)).orElse((if (input.startsWith("Q", _p851)) Some(("Q", _p851 + 1)) else None)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p851)))).map { case (_r852, _p853) => (new ~(_r850, _r852), _p853) } }.flatMap { case (_r846, _p847) => parsePercentBody(input, _p847).map { case (_r848, _p849) => (new ~(_r846, _r848), _p849) } }.flatMap { case (_r842, _p843) => parseInlineSpacing(input, _p843).map { case (_r844, _p845) => (new ~(_r842, _r844), _p845) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parsePercentWordArray(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None).flatMap { case (_r866, _p867) => ((if (input.startsWith("w", _p867)) Some(("w", _p867 + 1)) else None)).orElse((if (input.startsWith("W", _p867)) Some(("W", _p867 + 1)) else None)).map { case (_r868, _p869) => (new ~(_r866, _r868), _p869) } }.flatMap { case (_r862, _p863) => parsePercentBody(input, _p863).map { case (_r864, _p865) => (new ~(_r862, _r864), _p865) } }.flatMap { case (_r858, _p859) => parseInlineSpacing(input, _p859).map { case (_r860, _p861) => (new ~(_r858, _r860), _p861) } }.map { case (r, p) => (_applyAction({  _ => ArrayLiteral(List.empty)  }, r), p) }

  def parsePercentSymbolArray(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%", pos)) Some(("%", pos + 1)) else None).flatMap { case (_r878, _p879) => ((if (input.startsWith("i", _p879)) Some(("i", _p879 + 1)) else None)).orElse((if (input.startsWith("I", _p879)) Some(("I", _p879 + 1)) else None)).map { case (_r880, _p881) => (new ~(_r878, _r880), _p881) } }.flatMap { case (_r874, _p875) => parsePercentBody(input, _p875).map { case (_r876, _p877) => (new ~(_r874, _r876), _p877) } }.flatMap { case (_r870, _p871) => parseInlineSpacing(input, _p871).map { case (_r872, _p873) => (new ~(_r870, _r872), _p873) } }.map { case (r, p) => (_applyAction({  _ => ArrayLiteral(List.empty)  }, r), p) }

  def parsePercentSymbolLiteralExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%s", pos)) Some(("%s", pos + 2)) else None).flatMap { case (_r886, _p887) => parsePercentBody(input, _p887).map { case (_r888, _p889) => (new ~(_r886, _r888), _p889) } }.flatMap { case (_r882, _p883) => parseInlineSpacing(input, _p883).map { case (_r884, _p885) => (new ~(_r882, _r884), _p885) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parsePercentRegex(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%r", pos)) Some(("%r", pos + 2)) else None).flatMap { case (_r898, _p899) => parsePercentBody(input, _p899).map { case (_r900, _p901) => (new ~(_r898, _r900), _p901) } }.flatMap { case (_r894, _p895) => {
  var _rs902: List[Any] = Nil; var _cp903: Int = _p895; var _go906 = true
  while (_go906) { (if (_cp903 < input.length && { val _c = input.charAt(_cp903); (_c >= 'a' && _c <= 'z') || (_c >= 'A' && _c <= 'Z') }) Some((input.charAt(_cp903).toString, _cp903 + 1)) else None) match {
    case Some((_st904, _np905)) => _rs902 = _rs902 :+ _st904; _cp903 = _np905
    case None => _go906 = false } }
  Some((_rs902, _cp903)) }.map { case (_r896, _p897) => (new ~(_r894, _r896), _p897) } }.flatMap { case (_r890, _p891) => parseInlineSpacing(input, _p891).map { case (_r892, _p893) => (new ~(_r890, _r892), _p893) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parsePercentCommand(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("%x", pos)) Some(("%x", pos + 2)) else None).flatMap { case (_r911, _p912) => parsePercentBody(input, _p912).map { case (_r913, _p914) => (new ~(_r911, _r913), _p914) } }.flatMap { case (_r907, _p908) => parseInlineSpacing(input, _p908).map { case (_r909, _p910) => (new ~(_r907, _r909), _p910) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) }

  def parseEscapedRegexChar(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None).flatMap { case (_r915, _p916) => (if (_p916 < input.length) Some((input.charAt(_p916).toString, _p916 + 1)) else None).map { case (_r917, _p918) => (new ~(_r915, _r917), _p918) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parsePlainRegexChar(input: String, pos: Int): Option[(Any, Int)] = (if ((((if (input.startsWith("/", pos)) Some(("/", pos + 1)) else None)).orElse((if (input.startsWith("\\", pos)) Some(("\\", pos + 1)) else None))).orElse((if (input.startsWith("#{", pos)) Some(("#{", pos + 2)) else None)).isEmpty) Some(((), pos)) else None).flatMap { case (_r919, _p920) => (if (_p920 < input.length) Some((input.charAt(_p920).toString, _p920 + 1)) else None).map { case (_r921, _p922) => (new ~(_r919, _r921), _p922) } }.map { case (r, p) => (_applyAction({  c => c.toString  }, r), p) }

  def parseRegexBodyChars(input: String, pos: Int): Option[(Any, Int)] = {
  var _rs923: List[Any] = Nil; var _cp924: Int = pos; var _go927 = true
  while (_go927) { ((parseEscapedRegexChar(input, _cp924)).orElse(parseInterpolationSegment(input, _cp924))).orElse(parsePlainRegexChar(input, _cp924)) match {
    case Some((_st925, _np926)) => _rs923 = _rs923 :+ _st925; _cp924 = _np926
    case None => _go927 = false } }
  Some((_rs923, _cp924)) }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseRegexLiteral(input: String, pos: Int): Option[(Any, Int)] = (parsePercentRegex(input, pos)).orElse((if (input.startsWith("/", pos)) Some(("/", pos + 1)) else None).flatMap { case (_r944, _p945) => (if (((if (input.startsWith(" ", _p945)) Some((" ", _p945 + 1)) else None)).orElse((if (input.startsWith("\t", _p945)) Some(("\t", _p945 + 1)) else None)).isEmpty) Some(((), _p945)) else None).map { case (_r946, _p947) => (new ~(_r944, _r946), _p947) } }.flatMap { case (_r940, _p941) => parseRegexBodyChars(input, _p941).map { case (_r942, _p943) => (new ~(_r940, _r942), _p943) } }.flatMap { case (_r936, _p937) => (if (input.startsWith("/", _p937)) Some(("/", _p937 + 1)) else None).map { case (_r938, _p939) => (new ~(_r936, _r938), _p939) } }.flatMap { case (_r932, _p933) => {
  var _rs948: List[Any] = Nil; var _cp949: Int = _p933; var _go952 = true
  while (_go952) { (if (_cp949 < input.length && { val _c = input.charAt(_cp949); (_c >= 'a' && _c <= 'z') || (_c >= 'A' && _c <= 'Z') }) Some((input.charAt(_cp949).toString, _cp949 + 1)) else None) match {
    case Some((_st950, _np951)) => _rs948 = _rs948 :+ _st950; _cp949 = _np951
    case None => _go952 = false } }
  Some((_rs948, _cp949)) }.map { case (_r934, _p935) => (new ~(_r932, _r934), _p935) } }.flatMap { case (_r928, _p929) => parseInlineSpacing(input, _p929).map { case (_r930, _p931) => (new ~(_r928, _r930), _p931) } }.map { case (r, p) => (_applyAction({  _ => StringLiteral("")  }, r), p) })

  def parseSymbolNamePart(input: String, pos: Int): Option[(Any, Int)] = ((((((parseSymbolOperatorName(input, pos)).orElse(parseClassVarNameRaw(input, pos))).orElse(parseInstanceVarNameRaw(input, pos))).orElse(parseGlobalVarNamed(input, pos))).orElse(parseMethodIdentifierRaw(input, pos))).orElse(parseConstNameNoSpace(input, pos))).orElse(parseReservedWord(input, pos).map { case (r, p) => (_applyAction({  r => r  }, r), p) }).map { case (r, p) => (_applyAction({  v => v  }, r), p) }

  def parseSymbolLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(20, pos) {
    (if (input.startsWith(":", pos)) Some((":", pos + 1)) else None).flatMap { case (_r961, _p962) => (if ((if (input.startsWith(":", _p962)) Some((":", _p962 + 1)) else None).isEmpty) Some(((), _p962)) else None).map { case (_r963, _p964) => (new ~(_r961, _r963), _p964) } }.flatMap { case (_r957, _p958) => (((if (input.startsWith("\"", _p958)) Some(("\"", _p958 + 1)) else None).flatMap { case (_r969, _p970) => {
  var _rs973: List[Any] = Nil; var _cp974: Int = _p970; var _go977 = true
  while (_go977) { ((parseEscapedChar(input, _cp974)).orElse(parseInterpolationSegment(input, _cp974))).orElse(parseDqStringChar(input, _cp974)) match {
    case Some((_st975, _np976)) => _rs973 = _rs973 :+ _st975; _cp974 = _np976
    case None => _go977 = false } }
  Some((_rs973, _cp974)) }.map { case (_r971, _p972) => (new ~(_r969, _r971), _p972) } }.flatMap { case (_r965, _p966) => (if (input.startsWith("\"", _p966)) Some(("\"", _p966 + 1)) else None).map { case (_r967, _p968) => (new ~(_r965, _r967), _p968) } }).orElse((if (input.startsWith("'", _p958)) Some(("'", _p958 + 1)) else None).flatMap { case (_r982, _p983) => {
  var _rs986: List[Any] = Nil; var _cp987: Int = _p983; var _go990 = true
  while (_go990) { (parseEscapedSqChar(input, _cp987)).orElse(parseSqStringChar(input, _cp987)) match {
    case Some((_st988, _np989)) => _rs986 = _rs986 :+ _st988; _cp987 = _np989
    case None => _go990 = false } }
  Some((_rs986, _cp987)) }.map { case (_r984, _p985) => (new ~(_r982, _r984), _p985) } }.flatMap { case (_r978, _p979) => (if (input.startsWith("'", _p979)) Some(("'", _p979 + 1)) else None).map { case (_r980, _p981) => (new ~(_r978, _r980), _p981) } })).orElse(parseSymbolNamePart(input, _p958)).map { case (_r959, _p960) => (new ~(_r957, _r959), _p960) } }.flatMap { case (_r953, _p954) => parseInlineSpacing(input, _p954).map { case (_r955, _p956) => (new ~(_r953, _r955), _p956) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }
  }

  def parseLabelNameNoSpace(input: String, pos: Int): Option[(Any, Int)] = ((if (parseReservedWord(input, pos).isEmpty) Some(((), pos)) else None).flatMap { case (_r991, _p992) => parseIdentifierRaw(input, _p992).map { case (_r993, _p994) => (new ~(_r991, _r993), _p994) } }).orElse(parseReservedWord(input, pos)).map { case (r, p) => (_applyAction({  v => v  }, r), p) }

  def parseLabelSymbol(input: String, pos: Int): Option[(Any, Int)] = parseLabelNameNoSpace(input, pos).flatMap { case (_r1003, _p1004) => (if (input.startsWith(":", _p1004)) Some((":", _p1004 + 1)) else None).map { case (_r1005, _p1006) => (new ~(_r1003, _r1005), _p1006) } }.flatMap { case (_r999, _p1000) => (if ((if (input.startsWith(":", _p1000)) Some((":", _p1000 + 1)) else None).isEmpty) Some(((), _p1000)) else None).map { case (_r1001, _p1002) => (new ~(_r999, _r1001), _p1002) } }.flatMap { case (_r995, _p996) => parseInlineSpacing(input, _p996).map { case (_r997, _p998) => (new ~(_r995, _r997), _p998) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parseQuotedLabelSymbol(input: String, pos: Int): Option[(Any, Int)] = (parseStringLiteral(input, pos)).orElse(parseSymbolLiteral(input, pos)).flatMap { case (_r1011, _p1012) => parseLabelColon(input, _p1012).map { case (_r1013, _p1014) => (new ~(_r1011, _r1013), _p1014) } }.flatMap { case (_r1007, _p1008) => parseSpacing(input, _p1008).map { case (_r1009, _p1010) => (new ~(_r1007, _r1009), _p1010) } }.map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parseSelfExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(21, pos) {
    (if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r1019, _p1020) => (if (parseIdentCont(input, _p1020).isEmpty) Some(((), _p1020)) else None).map { case (_r1021, _p1022) => (new ~(_r1019, _r1021), _p1022) } }.flatMap { case (_r1015, _p1016) => parseInlineSpacing(input, _p1016).map { case (_r1017, _p1018) => (new ~(_r1015, _r1017), _p1018) } }.map { case (r, p) => (_applyAction({  _ => SelfExpr()  }, r), p) }
  }

  def parseBoolLiteral(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("true", pos)) Some(("true", pos + 4)) else None).flatMap { case (_r1027, _p1028) => (if (parseIdentCont(input, _p1028).isEmpty) Some(((), _p1028)) else None).map { case (_r1029, _p1030) => (new ~(_r1027, _r1029), _p1030) } }.flatMap { case (_r1023, _p1024) => parseInlineSpacing(input, _p1024).map { case (_r1025, _p1026) => (new ~(_r1023, _r1025), _p1026) } }).orElse((if (input.startsWith("false", pos)) Some(("false", pos + 5)) else None).flatMap { case (_r1035, _p1036) => (if (parseIdentCont(input, _p1036).isEmpty) Some(((), _p1036)) else None).map { case (_r1037, _p1038) => (new ~(_r1035, _r1037), _p1038) } }.flatMap { case (_r1031, _p1032) => parseInlineSpacing(input, _p1032).map { case (_r1033, _p1034) => (new ~(_r1031, _r1033), _p1034) } }).map { case (r, p) => (_applyAction({  _ => BoolLiteral(true)  }, r), p) }

  def parseNilLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("nil", pos)) Some(("nil", pos + 3)) else None).flatMap { case (_r1043, _p1044) => (if (parseIdentCont(input, _p1044).isEmpty) Some(((), _p1044)) else None).map { case (_r1045, _p1046) => (new ~(_r1043, _r1045), _p1046) } }.flatMap { case (_r1039, _p1040) => parseInlineSpacing(input, _p1040).map { case (_r1041, _p1042) => (new ~(_r1039, _r1041), _p1042) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

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
  var _rs1055: List[Any] = Nil; var _cp1056: Int = pos; var _go1059 = true
  while (_go1059) { parseStatementSep(input, _cp1056) match {
    case Some((_st1057, _np1058)) => _rs1055 = _rs1055 :+ _st1057; _cp1056 = _np1058
    case None => _go1059 = false } }
  Some((_rs1055, _cp1056)) }.flatMap { case (_r1051, _p1052) => (parseExpr(input, _p1052).flatMap { case (_r1060, _p1061) => {
  var _rs1064: List[Any] = Nil; var _cp1065: Int = _p1061; var _go1068 = true
  while (_go1068) { {
  parseStatementSep(input, _cp1065) match {
    case None => None
    case Some((_fs1073, _fp1074)) =>
      var _rs1075: List[Any] = List(_fs1073); var _cp1076: Int = _fp1074; var _go1079 = true
      while (_go1079) { parseStatementSep(input, _cp1076) match {
        case Some((_st1077, _np1078)) => _rs1075 = _rs1075 :+ _st1077; _cp1076 = _np1078
        case None => _go1079 = false } }
      Some((_rs1075, _cp1076)) } }.flatMap { case (_r1069, _p1070) => parseExpr(input, _p1070).map { case (_r1071, _p1072) => (new ~(_r1069, _r1071), _p1072) } } match {
    case Some((_st1066, _np1067)) => _rs1064 = _rs1064 :+ _st1066; _cp1065 = _np1067
    case None => _go1068 = false } }
  Some((_rs1064, _cp1065)) }.map { case (_r1062, _p1063) => (new ~(_r1060, _r1062), _p1063) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1052)))).map { case (_r1053, _p1054) => (new ~(_r1051, _r1053), _p1054) } }.flatMap { case (_r1047, _p1048) => {
  var _rs1080: List[Any] = Nil; var _cp1081: Int = _p1048; var _go1084 = true
  while (_go1084) { parseStatementSep(input, _cp1081) match {
    case Some((_st1082, _np1083)) => _rs1080 = _rs1080 :+ _st1082; _cp1081 = _np1083
    case None => _go1084 = false } }
  Some((_rs1080, _cp1081)) }.map { case (_r1049, _p1050) => (new ~(_r1047, _r1049), _p1050) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseParenExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r1097, _p1098) => parseSpacing(input, _p1098).map { case (_r1099, _p1100) => (new ~(_r1097, _r1099), _p1100) } }.flatMap { case (_r1093, _p1094) => parseParenExprInner(input, _p1094).map { case (_r1095, _p1096) => (new ~(_r1093, _r1095), _p1096) } }.flatMap { case (_r1089, _p1090) => (if (input.startsWith(")", _p1090)) Some((")", _p1090 + 1)) else None).map { case (_r1091, _p1092) => (new ~(_r1089, _r1091), _p1092) } }.flatMap { case (_r1085, _p1086) => parseInlineSpacing(input, _p1086).map { case (_r1087, _p1088) => (new ~(_r1085, _r1087), _p1088) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseArrayElem(input: String, pos: Int): Option[(Any, Int)] = ((((((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r1105, _p1106) => parseSpacing(input, _p1106).map { case (_r1107, _p1108) => (new ~(_r1105, _r1107), _p1108) } }.flatMap { case (_r1101, _p1102) => parseExpr(input, _p1102).map { case (_r1103, _p1104) => (new ~(_r1101, _r1103), _p1104) } }).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r1113, _p1114) => parseSpacing(input, _p1114).map { case (_r1115, _p1116) => (new ~(_r1113, _r1115), _p1116) } }.flatMap { case (_r1109, _p1110) => parseExpr(input, _p1110).map { case (_r1111, _p1112) => (new ~(_r1109, _r1111), _p1112) } })).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r1121, _p1122) => parseSpacing(input, _p1122).map { case (_r1123, _p1124) => (new ~(_r1121, _r1123), _p1124) } }.flatMap { case (_r1117, _p1118) => parseExpr(input, _p1118).map { case (_r1119, _p1120) => (new ~(_r1117, _r1119), _p1120) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r1129, _p1130) => parseSpacing(input, _p1130).map { case (_r1131, _p1132) => (new ~(_r1129, _r1131), _p1132) } }.flatMap { case (_r1125, _p1126) => parseExpr(input, _p1126).map { case (_r1127, _p1128) => (new ~(_r1125, _r1127), _p1128) } })).orElse(parseExpr(input, pos).flatMap { case (_r1145, _p1146) => parseSpacing(input, _p1146).map { case (_r1147, _p1148) => (new ~(_r1145, _r1147), _p1148) } }.flatMap { case (_r1141, _p1142) => (if (input.startsWith("=>", _p1142)) Some(("=>", _p1142 + 2)) else None).map { case (_r1143, _p1144) => (new ~(_r1141, _r1143), _p1144) } }.flatMap { case (_r1137, _p1138) => parseSpacing(input, _p1138).map { case (_r1139, _p1140) => (new ~(_r1137, _r1139), _p1140) } }.flatMap { case (_r1133, _p1134) => parseExpr(input, _p1134).map { case (_r1135, _p1136) => (new ~(_r1133, _r1135), _p1136) } })).orElse(parseExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseArrayLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(28, pos) {
    (if (input.startsWith("[", pos)) Some(("[", pos + 1)) else None).flatMap { case (_r1161, _p1162) => parseSpacing(input, _p1162).map { case (_r1163, _p1164) => (new ~(_r1161, _r1163), _p1164) } }.flatMap { case (_r1157, _p1158) => (parseArrayElem(input, _p1158).flatMap { case (_r1173, _p1174) => {
  var _rs1177: List[Any] = Nil; var _cp1178: Int = _p1174; var _go1181 = true
  while (_go1181) { parseSpacing(input, _cp1178).flatMap { case (_r1190, _p1191) => (if (input.startsWith(",", _p1191)) Some((",", _p1191 + 1)) else None).map { case (_r1192, _p1193) => (new ~(_r1190, _r1192), _p1193) } }.flatMap { case (_r1186, _p1187) => parseSpacing(input, _p1187).map { case (_r1188, _p1189) => (new ~(_r1186, _r1188), _p1189) } }.flatMap { case (_r1182, _p1183) => parseArrayElem(input, _p1183).map { case (_r1184, _p1185) => (new ~(_r1182, _r1184), _p1185) } } match {
    case Some((_st1179, _np1180)) => _rs1177 = _rs1177 :+ _st1179; _cp1178 = _np1180
    case None => _go1181 = false } }
  Some((_rs1177, _cp1178)) }.map { case (_r1175, _p1176) => (new ~(_r1173, _r1175), _p1176) } }.flatMap { case (_r1169, _p1170) => (parseSpacing(input, _p1170).flatMap { case (_r1194, _p1195) => (if (input.startsWith(",", _p1195)) Some((",", _p1195 + 1)) else None).map { case (_r1196, _p1197) => (new ~(_r1194, _r1196), _p1197) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1170)))).map { case (_r1171, _p1172) => (new ~(_r1169, _r1171), _p1172) } }.flatMap { case (_r1165, _p1166) => parseSpacing(input, _p1166).map { case (_r1167, _p1168) => (new ~(_r1165, _r1167), _p1168) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1158)))).map { case (_r1159, _p1160) => (new ~(_r1157, _r1159), _p1160) } }.flatMap { case (_r1153, _p1154) => (if (input.startsWith("]", _p1154)) Some(("]", _p1154 + 1)) else None).map { case (_r1155, _p1156) => (new ~(_r1153, _r1155), _p1156) } }.flatMap { case (_r1149, _p1150) => parseInlineSpacing(input, _p1150).map { case (_r1151, _p1152) => (new ~(_r1149, _r1151), _p1152) } }.map { case (r, p) => (_applyAction({  _ => ArrayLiteral(List.empty)  }, r), p) }
  }

  def parseHashEntry(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r1202, _p1203) => parseSpacing(input, _p1203).map { case (_r1204, _p1205) => (new ~(_r1202, _r1204), _p1205) } }.flatMap { case (_r1198, _p1199) => parseExpr(input, _p1199).map { case (_r1200, _p1201) => (new ~(_r1198, _r1200), _p1201) } }).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r1210, _p1211) => parseSpacing(input, _p1211).map { case (_r1212, _p1213) => (new ~(_r1210, _r1212), _p1213) } }.flatMap { case (_r1206, _p1207) => parseExpr(input, _p1207).map { case (_r1208, _p1209) => (new ~(_r1206, _r1208), _p1209) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r1218, _p1219) => parseSpacing(input, _p1219).map { case (_r1220, _p1221) => (new ~(_r1218, _r1220), _p1221) } }.flatMap { case (_r1214, _p1215) => parseExpr(input, _p1215).map { case (_r1216, _p1217) => (new ~(_r1214, _r1216), _p1217) } })).orElse(parseExpr(input, pos).flatMap { case (_r1234, _p1235) => parseSpacing(input, _p1235).map { case (_r1236, _p1237) => (new ~(_r1234, _r1236), _p1237) } }.flatMap { case (_r1230, _p1231) => (if (input.startsWith("=>", _p1231)) Some(("=>", _p1231 + 2)) else None).map { case (_r1232, _p1233) => (new ~(_r1230, _r1232), _p1233) } }.flatMap { case (_r1226, _p1227) => parseSpacing(input, _p1227).map { case (_r1228, _p1229) => (new ~(_r1226, _r1228), _p1229) } }.flatMap { case (_r1222, _p1223) => parseExpr(input, _p1223).map { case (_r1224, _p1225) => (new ~(_r1222, _r1224), _p1225) } }).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseHashLiteral(input: String, pos: Int): Option[(Any, Int)] = _withMemo(29, pos) {
    (if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r1250, _p1251) => parseSpacing(input, _p1251).map { case (_r1252, _p1253) => (new ~(_r1250, _r1252), _p1253) } }.flatMap { case (_r1246, _p1247) => (parseHashEntry(input, _p1247).flatMap { case (_r1262, _p1263) => {
  var _rs1266: List[Any] = Nil; var _cp1267: Int = _p1263; var _go1270 = true
  while (_go1270) { parseSpacing(input, _cp1267).flatMap { case (_r1279, _p1280) => (if (input.startsWith(",", _p1280)) Some((",", _p1280 + 1)) else None).map { case (_r1281, _p1282) => (new ~(_r1279, _r1281), _p1282) } }.flatMap { case (_r1275, _p1276) => parseSpacing(input, _p1276).map { case (_r1277, _p1278) => (new ~(_r1275, _r1277), _p1278) } }.flatMap { case (_r1271, _p1272) => parseHashEntry(input, _p1272).map { case (_r1273, _p1274) => (new ~(_r1271, _r1273), _p1274) } } match {
    case Some((_st1268, _np1269)) => _rs1266 = _rs1266 :+ _st1268; _cp1267 = _np1269
    case None => _go1270 = false } }
  Some((_rs1266, _cp1267)) }.map { case (_r1264, _p1265) => (new ~(_r1262, _r1264), _p1265) } }.flatMap { case (_r1258, _p1259) => (parseSpacing(input, _p1259).flatMap { case (_r1283, _p1284) => (if (input.startsWith(",", _p1284)) Some((",", _p1284 + 1)) else None).map { case (_r1285, _p1286) => (new ~(_r1283, _r1285), _p1286) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1259)))).map { case (_r1260, _p1261) => (new ~(_r1258, _r1260), _p1261) } }.flatMap { case (_r1254, _p1255) => parseSpacing(input, _p1255).map { case (_r1256, _p1257) => (new ~(_r1254, _r1256), _p1257) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1247)))).map { case (_r1248, _p1249) => (new ~(_r1246, _r1248), _p1249) } }.flatMap { case (_r1242, _p1243) => (if (input.startsWith("}", _p1243)) Some(("}", _p1243 + 1)) else None).map { case (_r1244, _p1245) => (new ~(_r1242, _r1244), _p1245) } }.flatMap { case (_r1238, _p1239) => parseInlineSpacing(input, _p1239).map { case (_r1240, _p1241) => (new ~(_r1238, _r1240), _p1241) } }.map { case (r, p) => (_applyAction({  _ => HashLiteral(List.empty)  }, r), p) }
  }

  def parseKeywordParam(input: String, pos: Int): Option[(Any, Int)] = parseLabelNameNoSpace(input, pos).flatMap { case (_r1299, _p1300) => (if (input.startsWith(":", _p1300)) Some((":", _p1300 + 1)) else None).map { case (_r1301, _p1302) => (new ~(_r1299, _r1301), _p1302) } }.flatMap { case (_r1295, _p1296) => (if ((if (input.startsWith(":", _p1296)) Some((":", _p1296 + 1)) else None).isEmpty) Some(((), _p1296)) else None).map { case (_r1297, _p1298) => (new ~(_r1295, _r1297), _p1298) } }.flatMap { case (_r1291, _p1292) => parseSpacing(input, _p1292).map { case (_r1293, _p1294) => (new ~(_r1291, _r1293), _p1294) } }.flatMap { case (_r1287, _p1288) => (parseExpr(input, _p1288).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1288)))).map { case (_r1289, _p1290) => (new ~(_r1287, _r1289), _p1290) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDestructuredParam(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r1323, _p1324) => parseSpacing(input, _p1324).map { case (_r1325, _p1326) => (new ~(_r1323, _r1325), _p1326) } }.flatMap { case (_r1319, _p1320) => parseFormalParam(input, _p1320).map { case (_r1321, _p1322) => (new ~(_r1319, _r1321), _p1322) } }.flatMap { case (_r1315, _p1316) => {
  var _rs1327: List[Any] = Nil; var _cp1328: Int = _p1316; var _go1331 = true
  while (_go1331) { parseSpacing(input, _cp1328).flatMap { case (_r1340, _p1341) => (if (input.startsWith(",", _p1341)) Some((",", _p1341 + 1)) else None).map { case (_r1342, _p1343) => (new ~(_r1340, _r1342), _p1343) } }.flatMap { case (_r1336, _p1337) => parseSpacing(input, _p1337).map { case (_r1338, _p1339) => (new ~(_r1336, _r1338), _p1339) } }.flatMap { case (_r1332, _p1333) => parseFormalParam(input, _p1333).map { case (_r1334, _p1335) => (new ~(_r1332, _r1334), _p1335) } } match {
    case Some((_st1329, _np1330)) => _rs1327 = _rs1327 :+ _st1329; _cp1328 = _np1330
    case None => _go1331 = false } }
  Some((_rs1327, _cp1328)) }.map { case (_r1317, _p1318) => (new ~(_r1315, _r1317), _p1318) } }.flatMap { case (_r1311, _p1312) => (parseSpacing(input, _p1312).flatMap { case (_r1344, _p1345) => (if (input.startsWith(",", _p1345)) Some((",", _p1345 + 1)) else None).map { case (_r1346, _p1347) => (new ~(_r1344, _r1346), _p1347) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1312)))).map { case (_r1313, _p1314) => (new ~(_r1311, _r1313), _p1314) } }.flatMap { case (_r1307, _p1308) => parseSpacing(input, _p1308).map { case (_r1309, _p1310) => (new ~(_r1307, _r1309), _p1310) } }.flatMap { case (_r1303, _p1304) => (if (input.startsWith(")", _p1304)) Some((")", _p1304 + 1)) else None).map { case (_r1305, _p1306) => (new ~(_r1303, _r1305), _p1306) } }.map { case (r, p) => (_applyAction({  _ => "()"  }, r), p) }

  def parseFormalParam(input: String, pos: Int): Option[(Any, Int)] = (((((((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r1352, _p1353) => parseSpacing(input, _p1353).map { case (_r1354, _p1355) => (new ~(_r1352, _r1354), _p1355) } }.flatMap { case (_r1348, _p1349) => (((if (input.startsWith("nil", _p1349)) Some(("nil", _p1349 + 3)) else None).flatMap { case (_r1356, _p1357) => (if (parseIdentCont(input, _p1357).isEmpty) Some(((), _p1357)) else None).map { case (_r1358, _p1359) => (new ~(_r1356, _r1358), _p1359) } }).orElse(parseIdentifier(input, _p1349)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1349)))).map { case (_r1350, _p1351) => (new ~(_r1348, _r1350), _p1351) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r1364, _p1365) => parseSpacing(input, _p1365).map { case (_r1366, _p1367) => (new ~(_r1364, _r1366), _p1367) } }.flatMap { case (_r1360, _p1361) => (((if (input.startsWith("nil", _p1361)) Some(("nil", _p1361 + 3)) else None).flatMap { case (_r1368, _p1369) => (if (parseIdentCont(input, _p1369).isEmpty) Some(((), _p1369)) else None).map { case (_r1370, _p1371) => (new ~(_r1368, _r1370), _p1371) } }).orElse(parseIdentifier(input, _p1361)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1361)))).map { case (_r1362, _p1363) => (new ~(_r1360, _r1362), _p1363) } })).orElse((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r1376, _p1377) => parseSpacing(input, _p1377).map { case (_r1378, _p1379) => (new ~(_r1376, _r1378), _p1379) } }.flatMap { case (_r1372, _p1373) => (((if (input.startsWith("nil", _p1373)) Some(("nil", _p1373 + 3)) else None).flatMap { case (_r1380, _p1381) => (if (parseIdentCont(input, _p1381).isEmpty) Some(((), _p1381)) else None).map { case (_r1382, _p1383) => (new ~(_r1380, _r1382), _p1383) } }).orElse(parseIdentifier(input, _p1373)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1373)))).map { case (_r1374, _p1375) => (new ~(_r1372, _r1374), _p1375) } })).orElse(parseKeywordParam(input, pos))).orElse(parseDestructuredParam(input, pos))).orElse(parseIdentifier(input, pos).flatMap { case (_r1384, _p1385) => (parseSpacing(input, _p1385).flatMap { case (_r1396, _p1397) => (if (input.startsWith("=", _p1397)) Some(("=", _p1397 + 1)) else None).map { case (_r1398, _p1399) => (new ~(_r1396, _r1398), _p1399) } }.flatMap { case (_r1392, _p1393) => parseSpacing(input, _p1393).map { case (_r1394, _p1395) => (new ~(_r1392, _r1394), _p1395) } }.flatMap { case (_r1388, _p1389) => parseExpr(input, _p1389).map { case (_r1390, _p1391) => (new ~(_r1388, _r1390), _p1391) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1385)))).map { case (_r1386, _p1387) => (new ~(_r1384, _r1386), _p1387) } }).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseParams(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r1412, _p1413) => parseSpacing(input, _p1413).map { case (_r1414, _p1415) => (new ~(_r1412, _r1414), _p1415) } }.flatMap { case (_r1408, _p1409) => (parseFormalParam(input, _p1409).flatMap { case (_r1424, _p1425) => {
  var _rs1428: List[Any] = Nil; var _cp1429: Int = _p1425; var _go1432 = true
  while (_go1432) { parseSpacing(input, _cp1429).flatMap { case (_r1441, _p1442) => (if (input.startsWith(",", _p1442)) Some((",", _p1442 + 1)) else None).map { case (_r1443, _p1444) => (new ~(_r1441, _r1443), _p1444) } }.flatMap { case (_r1437, _p1438) => parseSpacing(input, _p1438).map { case (_r1439, _p1440) => (new ~(_r1437, _r1439), _p1440) } }.flatMap { case (_r1433, _p1434) => parseFormalParam(input, _p1434).map { case (_r1435, _p1436) => (new ~(_r1433, _r1435), _p1436) } } match {
    case Some((_st1430, _np1431)) => _rs1428 = _rs1428 :+ _st1430; _cp1429 = _np1431
    case None => _go1432 = false } }
  Some((_rs1428, _cp1429)) }.map { case (_r1426, _p1427) => (new ~(_r1424, _r1426), _p1427) } }.flatMap { case (_r1420, _p1421) => (parseSpacing(input, _p1421).flatMap { case (_r1445, _p1446) => (if (input.startsWith(",", _p1446)) Some((",", _p1446 + 1)) else None).map { case (_r1447, _p1448) => (new ~(_r1445, _r1447), _p1448) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1421)))).map { case (_r1422, _p1423) => (new ~(_r1420, _r1422), _p1423) } }.flatMap { case (_r1416, _p1417) => parseSpacing(input, _p1417).map { case (_r1418, _p1419) => (new ~(_r1416, _r1418), _p1419) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1409)))).map { case (_r1410, _p1411) => (new ~(_r1408, _r1410), _p1411) } }.flatMap { case (_r1404, _p1405) => (if (input.startsWith(")", _p1405)) Some((")", _p1405 + 1)) else None).map { case (_r1406, _p1407) => (new ~(_r1404, _r1406), _p1407) } }.flatMap { case (_r1400, _p1401) => parseInlineSpacing(input, _p1401).map { case (_r1402, _p1403) => (new ~(_r1400, _r1402), _p1403) } }.map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseBareParams(input: String, pos: Int): Option[(Any, Int)] = parseFormalParam(input, pos).flatMap { case (_r1449, _p1450) => {
  parseInlineSpacing(input, _p1450).flatMap { case (_r1468, _p1469) => (if (input.startsWith(",", _p1469)) Some((",", _p1469 + 1)) else None).map { case (_r1470, _p1471) => (new ~(_r1468, _r1470), _p1471) } }.flatMap { case (_r1464, _p1465) => parseInlineSpacing(input, _p1465).map { case (_r1466, _p1467) => (new ~(_r1464, _r1466), _p1467) } }.flatMap { case (_r1460, _p1461) => parseFormalParam(input, _p1461).map { case (_r1462, _p1463) => (new ~(_r1460, _r1462), _p1463) } } match {
    case None => None
    case Some((_fs1453, _fp1454)) =>
      var _rs1455: List[Any] = List(_fs1453); var _cp1456: Int = _fp1454; var _go1459 = true
      while (_go1459) { parseInlineSpacing(input, _cp1456).flatMap { case (_r1480, _p1481) => (if (input.startsWith(",", _p1481)) Some((",", _p1481 + 1)) else None).map { case (_r1482, _p1483) => (new ~(_r1480, _r1482), _p1483) } }.flatMap { case (_r1476, _p1477) => parseInlineSpacing(input, _p1477).map { case (_r1478, _p1479) => (new ~(_r1476, _r1478), _p1479) } }.flatMap { case (_r1472, _p1473) => parseFormalParam(input, _p1473).map { case (_r1474, _p1475) => (new ~(_r1472, _r1474), _p1475) } } match {
        case Some((_st1457, _np1458)) => _rs1455 = _rs1455 :+ _st1457; _cp1456 = _np1458
        case None => _go1459 = false } }
      Some((_rs1455, _cp1456)) } }.map { case (_r1451, _p1452) => (new ~(_r1449, _r1451), _p1452) } }.map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseBlockFormalParam(input: String, pos: Int): Option[(Any, Int)] = (((((((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r1488, _p1489) => parseSpacing(input, _p1489).map { case (_r1490, _p1491) => (new ~(_r1488, _r1490), _p1491) } }.flatMap { case (_r1484, _p1485) => (((if (input.startsWith("nil", _p1485)) Some(("nil", _p1485 + 3)) else None).flatMap { case (_r1492, _p1493) => (if (parseIdentCont(input, _p1493).isEmpty) Some(((), _p1493)) else None).map { case (_r1494, _p1495) => (new ~(_r1492, _r1494), _p1495) } }).orElse(parseIdentifier(input, _p1485)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1485)))).map { case (_r1486, _p1487) => (new ~(_r1484, _r1486), _p1487) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r1500, _p1501) => parseSpacing(input, _p1501).map { case (_r1502, _p1503) => (new ~(_r1500, _r1502), _p1503) } }.flatMap { case (_r1496, _p1497) => (((if (input.startsWith("nil", _p1497)) Some(("nil", _p1497 + 3)) else None).flatMap { case (_r1504, _p1505) => (if (parseIdentCont(input, _p1505).isEmpty) Some(((), _p1505)) else None).map { case (_r1506, _p1507) => (new ~(_r1504, _r1506), _p1507) } }).orElse(parseIdentifier(input, _p1497)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1497)))).map { case (_r1498, _p1499) => (new ~(_r1496, _r1498), _p1499) } })).orElse((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r1512, _p1513) => parseSpacing(input, _p1513).map { case (_r1514, _p1515) => (new ~(_r1512, _r1514), _p1515) } }.flatMap { case (_r1508, _p1509) => (((if (input.startsWith("nil", _p1509)) Some(("nil", _p1509 + 3)) else None).flatMap { case (_r1516, _p1517) => (if (parseIdentCont(input, _p1517).isEmpty) Some(((), _p1517)) else None).map { case (_r1518, _p1519) => (new ~(_r1516, _r1518), _p1519) } }).orElse(parseIdentifier(input, _p1509)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1509)))).map { case (_r1510, _p1511) => (new ~(_r1508, _r1510), _p1511) } })).orElse(parseKeywordParam(input, pos))).orElse(parseDestructuredParam(input, pos))).orElse(parseIdentifier(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseBlockLocalVar(input: String, pos: Int): Option[(Any, Int)] = parseIdentifier(input, pos).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseBlockParams(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("|", pos)) Some(("|", pos + 1)) else None).flatMap { case (_r1540, _p1541) => parseSpacing(input, _p1541).map { case (_r1542, _p1543) => (new ~(_r1540, _r1542), _p1543) } }.flatMap { case (_r1536, _p1537) => (parseBlockFormalParam(input, _p1537).flatMap { case (_r1552, _p1553) => {
  var _rs1556: List[Any] = Nil; var _cp1557: Int = _p1553; var _go1560 = true
  while (_go1560) { parseSpacing(input, _cp1557).flatMap { case (_r1569, _p1570) => (if (input.startsWith(",", _p1570)) Some((",", _p1570 + 1)) else None).map { case (_r1571, _p1572) => (new ~(_r1569, _r1571), _p1572) } }.flatMap { case (_r1565, _p1566) => parseSpacing(input, _p1566).map { case (_r1567, _p1568) => (new ~(_r1565, _r1567), _p1568) } }.flatMap { case (_r1561, _p1562) => parseBlockFormalParam(input, _p1562).map { case (_r1563, _p1564) => (new ~(_r1561, _r1563), _p1564) } } match {
    case Some((_st1558, _np1559)) => _rs1556 = _rs1556 :+ _st1558; _cp1557 = _np1559
    case None => _go1560 = false } }
  Some((_rs1556, _cp1557)) }.map { case (_r1554, _p1555) => (new ~(_r1552, _r1554), _p1555) } }.flatMap { case (_r1548, _p1549) => (parseSpacing(input, _p1549).flatMap { case (_r1573, _p1574) => (if (input.startsWith(",", _p1574)) Some((",", _p1574 + 1)) else None).map { case (_r1575, _p1576) => (new ~(_r1573, _r1575), _p1576) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1549)))).map { case (_r1550, _p1551) => (new ~(_r1548, _r1550), _p1551) } }.flatMap { case (_r1544, _p1545) => parseSpacing(input, _p1545).map { case (_r1546, _p1547) => (new ~(_r1544, _r1546), _p1547) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1537)))).map { case (_r1538, _p1539) => (new ~(_r1536, _r1538), _p1539) } }.flatMap { case (_r1532, _p1533) => ((if (input.startsWith(";", _p1533)) Some((";", _p1533 + 1)) else None).flatMap { case (_r1585, _p1586) => parseSpacing(input, _p1586).map { case (_r1587, _p1588) => (new ~(_r1585, _r1587), _p1588) } }.flatMap { case (_r1581, _p1582) => parseBlockLocalVar(input, _p1582).map { case (_r1583, _p1584) => (new ~(_r1581, _r1583), _p1584) } }.flatMap { case (_r1577, _p1578) => {
  var _rs1589: List[Any] = Nil; var _cp1590: Int = _p1578; var _go1593 = true
  while (_go1593) { parseSpacing(input, _cp1590).flatMap { case (_r1602, _p1603) => (if (input.startsWith(",", _p1603)) Some((",", _p1603 + 1)) else None).map { case (_r1604, _p1605) => (new ~(_r1602, _r1604), _p1605) } }.flatMap { case (_r1598, _p1599) => parseSpacing(input, _p1599).map { case (_r1600, _p1601) => (new ~(_r1598, _r1600), _p1601) } }.flatMap { case (_r1594, _p1595) => parseBlockLocalVar(input, _p1595).map { case (_r1596, _p1597) => (new ~(_r1594, _r1596), _p1597) } } match {
    case Some((_st1591, _np1592)) => _rs1589 = _rs1589 :+ _st1591; _cp1590 = _np1592
    case None => _go1593 = false } }
  Some((_rs1589, _cp1590)) }.map { case (_r1579, _p1580) => (new ~(_r1577, _r1579), _p1580) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1533)))).map { case (_r1534, _p1535) => (new ~(_r1532, _r1534), _p1535) } }.flatMap { case (_r1528, _p1529) => parseSpacing(input, _p1529).map { case (_r1530, _p1531) => (new ~(_r1528, _r1530), _p1531) } }.flatMap { case (_r1524, _p1525) => (if (input.startsWith("|", _p1525)) Some(("|", _p1525 + 1)) else None).map { case (_r1526, _p1527) => (new ~(_r1524, _r1526), _p1527) } }.flatMap { case (_r1520, _p1521) => parseInlineSpacing(input, _p1521).map { case (_r1522, _p1523) => (new ~(_r1520, _r1522), _p1523) } }.map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseRescueStop(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r1610, _p1611) => (if (parseIdentCont(input, _p1611).isEmpty) Some(((), _p1611)) else None).map { case (_r1612, _p1613) => (new ~(_r1610, _r1612), _p1613) } }.flatMap { case (_r1606, _p1607) => parseSpacing(input, _p1607).map { case (_r1608, _p1609) => (new ~(_r1606, _r1608), _p1609) } }).orElse((if (input.startsWith("else", pos)) Some(("else", pos + 4)) else None).flatMap { case (_r1618, _p1619) => (if (parseIdentCont(input, _p1619).isEmpty) Some(((), _p1619)) else None).map { case (_r1620, _p1621) => (new ~(_r1618, _r1620), _p1621) } }.flatMap { case (_r1614, _p1615) => parseSpacing(input, _p1615).map { case (_r1616, _p1617) => (new ~(_r1614, _r1616), _p1617) } })).orElse((if (input.startsWith("ensure", pos)) Some(("ensure", pos + 6)) else None).flatMap { case (_r1626, _p1627) => (if (parseIdentCont(input, _p1627).isEmpty) Some(((), _p1627)) else None).map { case (_r1628, _p1629) => (new ~(_r1626, _r1628), _p1629) } }.flatMap { case (_r1622, _p1623) => parseSpacing(input, _p1623).map { case (_r1624, _p1625) => (new ~(_r1622, _r1624), _p1625) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r1634, _p1635) => (if (parseIdentCont(input, _p1635).isEmpty) Some(((), _p1635)) else None).map { case (_r1636, _p1637) => (new ~(_r1634, _r1636), _p1637) } }.flatMap { case (_r1630, _p1631) => parseSpacing(input, _p1631).map { case (_r1632, _p1633) => (new ~(_r1630, _r1632), _p1633) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseDoBlockStop(input: String, pos: Int): Option[(Any, Int)] = ((((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r1642, _p1643) => (if (parseIdentCont(input, _p1643).isEmpty) Some(((), _p1643)) else None).map { case (_r1644, _p1645) => (new ~(_r1642, _r1644), _p1645) } }.flatMap { case (_r1638, _p1639) => parseSpacing(input, _p1639).map { case (_r1640, _p1641) => (new ~(_r1638, _r1640), _p1641) } }).orElse((if (input.startsWith("else", pos)) Some(("else", pos + 4)) else None).flatMap { case (_r1650, _p1651) => (if (parseIdentCont(input, _p1651).isEmpty) Some(((), _p1651)) else None).map { case (_r1652, _p1653) => (new ~(_r1650, _r1652), _p1653) } }.flatMap { case (_r1646, _p1647) => parseSpacing(input, _p1647).map { case (_r1648, _p1649) => (new ~(_r1646, _r1648), _p1649) } })).orElse((if (input.startsWith("ensure", pos)) Some(("ensure", pos + 6)) else None).flatMap { case (_r1658, _p1659) => (if (parseIdentCont(input, _p1659).isEmpty) Some(((), _p1659)) else None).map { case (_r1660, _p1661) => (new ~(_r1658, _r1660), _p1661) } }.flatMap { case (_r1654, _p1655) => parseSpacing(input, _p1655).map { case (_r1656, _p1657) => (new ~(_r1654, _r1656), _p1657) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r1666, _p1667) => (if (parseIdentCont(input, _p1667).isEmpty) Some(((), _p1667)) else None).map { case (_r1668, _p1669) => (new ~(_r1666, _r1668), _p1669) } }.flatMap { case (_r1662, _p1663) => parseSpacing(input, _p1663).map { case (_r1664, _p1665) => (new ~(_r1662, _r1664), _p1665) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseEndKeyword(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r1674, _p1675) => (if (parseIdentCont(input, _p1675).isEmpty) Some(((), _p1675)) else None).map { case (_r1676, _p1677) => (new ~(_r1674, _r1676), _p1677) } }.flatMap { case (_r1670, _p1671) => parseSpacing(input, _p1671).map { case (_r1672, _p1673) => (new ~(_r1670, _r1672), _p1673) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseRescueClause(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r1702, _p1703) => (if (parseIdentCont(input, _p1703).isEmpty) Some(((), _p1703)) else None).map { case (_r1704, _p1705) => (new ~(_r1702, _r1704), _p1705) } }.flatMap { case (_r1698, _p1699) => parseSpacing(input, _p1699).map { case (_r1700, _p1701) => (new ~(_r1698, _r1700), _p1701) } }.flatMap { case (_r1694, _p1695) => (parseExpr(input, _p1695).flatMap { case (_r1706, _p1707) => {
  var _rs1710: List[Any] = Nil; var _cp1711: Int = _p1707; var _go1714 = true
  while (_go1714) { parseSpacing(input, _cp1711).flatMap { case (_r1723, _p1724) => (if (input.startsWith(",", _p1724)) Some((",", _p1724 + 1)) else None).map { case (_r1725, _p1726) => (new ~(_r1723, _r1725), _p1726) } }.flatMap { case (_r1719, _p1720) => parseSpacing(input, _p1720).map { case (_r1721, _p1722) => (new ~(_r1719, _r1721), _p1722) } }.flatMap { case (_r1715, _p1716) => parseExpr(input, _p1716).map { case (_r1717, _p1718) => (new ~(_r1715, _r1717), _p1718) } } match {
    case Some((_st1712, _np1713)) => _rs1710 = _rs1710 :+ _st1712; _cp1711 = _np1713
    case None => _go1714 = false } }
  Some((_rs1710, _cp1711)) }.map { case (_r1708, _p1709) => (new ~(_r1706, _r1708), _p1709) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1695)))).map { case (_r1696, _p1697) => (new ~(_r1694, _r1696), _p1697) } }.flatMap { case (_r1690, _p1691) => ((if (input.startsWith("=>", _p1691)) Some(("=>", _p1691 + 2)) else None).flatMap { case (_r1735, _p1736) => parseSpacing(input, _p1736).map { case (_r1737, _p1738) => (new ~(_r1735, _r1737), _p1738) } }.flatMap { case (_r1731, _p1732) => parseVariable(input, _p1732).map { case (_r1733, _p1734) => (new ~(_r1731, _r1733), _p1734) } }.flatMap { case (_r1727, _p1728) => parseSpacing(input, _p1728).map { case (_r1729, _p1730) => (new ~(_r1727, _r1729), _p1730) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1691)))).map { case (_r1692, _p1693) => (new ~(_r1690, _r1692), _p1693) } }.flatMap { case (_r1686, _p1687) => {
  var _rs1739: List[Any] = Nil; var _cp1740: Int = _p1687; var _go1743 = true
  while (_go1743) { parseStatementSep(input, _cp1740) match {
    case Some((_st1741, _np1742)) => _rs1739 = _rs1739 :+ _st1741; _cp1740 = _np1742
    case None => _go1743 = false } }
  Some((_rs1739, _cp1740)) }.map { case (_r1688, _p1689) => (new ~(_r1686, _r1688), _p1689) } }.flatMap { case (_r1682, _p1683) => {
  var _rs1744: List[Any] = Nil; var _cp1745: Int = _p1683; var _go1748 = true
  while (_go1748) { (if (parseRescueStop(input, _cp1745).isEmpty) Some(((), _cp1745)) else None).flatMap { case (_r1753, _p1754) => parseStatement(input, _p1754).map { case (_r1755, _p1756) => (new ~(_r1753, _r1755), _p1756) } }.flatMap { case (_r1749, _p1750) => {
  var _rs1757: List[Any] = Nil; var _cp1758: Int = _p1750; var _go1761 = true
  while (_go1761) { parseStatementSep(input, _cp1758) match {
    case Some((_st1759, _np1760)) => _rs1757 = _rs1757 :+ _st1759; _cp1758 = _np1760
    case None => _go1761 = false } }
  Some((_rs1757, _cp1758)) }.map { case (_r1751, _p1752) => (new ~(_r1749, _r1751), _p1752) } } match {
    case Some((_st1746, _np1747)) => _rs1744 = _rs1744 :+ _st1746; _cp1745 = _np1747
    case None => _go1748 = false } }
  Some((_rs1744, _cp1745)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r1684, _p1685) => (new ~(_r1682, _r1684), _p1685) } }.flatMap { case (_r1678, _p1679) => {
  var _rs1762: List[Any] = Nil; var _cp1763: Int = _p1679; var _go1766 = true
  while (_go1766) { parseStatementSep(input, _cp1763) match {
    case Some((_st1764, _np1765)) => _rs1762 = _rs1762 :+ _st1764; _cp1763 = _np1765
    case None => _go1766 = false } }
  Some((_rs1762, _cp1763)) }.map { case (_r1680, _p1681) => (new ~(_r1678, _r1680), _p1681) } }.map { case (r, p) => (_applyAction({  _ => RescueClause(List.empty, None, List.empty)  }, r), p) }

  def parseLambdaLiteral(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("->", pos)) Some(("->", pos + 2)) else None).flatMap { case (_r1779, _p1780) => parseInlineSpacing(input, _p1780).map { case (_r1781, _p1782) => (new ~(_r1779, _r1781), _p1782) } }.flatMap { case (_r1775, _p1776) => ((parseParams(input, _p1776)).orElse(parseParenExpr(input, _p1776).map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1776)))).map { case (_r1777, _p1778) => (new ~(_r1775, _r1777), _p1778) } }.flatMap { case (_r1771, _p1772) => parseSpacing(input, _p1772).map { case (_r1773, _p1774) => (new ~(_r1771, _r1773), _p1774) } }.flatMap { case (_r1767, _p1768) => parseBlockLiteral(input, _p1768).map { case (_r1769, _p1770) => (new ~(_r1767, _r1769), _p1770) } }.map { case (r, p) => (_applyAction({  _ => LambdaLiteral(List.empty, List.empty)  }, r), p) }

  def parseDoBlock(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("do", pos)) Some(("do", pos + 2)) else None).flatMap { case (_r1827, _p1828) => (if (parseIdentCont(input, _p1828).isEmpty) Some(((), _p1828)) else None).map { case (_r1829, _p1830) => (new ~(_r1827, _r1829), _p1830) } }.flatMap { case (_r1823, _p1824) => parseSpacing(input, _p1824).map { case (_r1825, _p1826) => (new ~(_r1823, _r1825), _p1826) } }.flatMap { case (_r1819, _p1820) => {
  var _rs1831: List[Any] = Nil; var _cp1832: Int = _p1820; var _go1835 = true
  while (_go1835) { parseStatementSep(input, _cp1832) match {
    case Some((_st1833, _np1834)) => _rs1831 = _rs1831 :+ _st1833; _cp1832 = _np1834
    case None => _go1835 = false } }
  Some((_rs1831, _cp1832)) }.map { case (_r1821, _p1822) => (new ~(_r1819, _r1821), _p1822) } }.flatMap { case (_r1815, _p1816) => (parseBlockParams(input, _p1816).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1816)))).map { case (_r1817, _p1818) => (new ~(_r1815, _r1817), _p1818) } }.flatMap { case (_r1811, _p1812) => {
  var _rs1836: List[Any] = Nil; var _cp1837: Int = _p1812; var _go1840 = true
  while (_go1840) { parseStatementSep(input, _cp1837) match {
    case Some((_st1838, _np1839)) => _rs1836 = _rs1836 :+ _st1838; _cp1837 = _np1839
    case None => _go1840 = false } }
  Some((_rs1836, _cp1837)) }.map { case (_r1813, _p1814) => (new ~(_r1811, _r1813), _p1814) } }.flatMap { case (_r1807, _p1808) => {
  var _rs1841: List[Any] = Nil; var _cp1842: Int = _p1808; var _go1845 = true
  while (_go1845) { (if (parseDoBlockStop(input, _cp1842).isEmpty) Some(((), _cp1842)) else None).flatMap { case (_r1850, _p1851) => parseStatement(input, _p1851).map { case (_r1852, _p1853) => (new ~(_r1850, _r1852), _p1853) } }.flatMap { case (_r1846, _p1847) => {
  var _rs1854: List[Any] = Nil; var _cp1855: Int = _p1847; var _go1858 = true
  while (_go1858) { parseStatementSep(input, _cp1855) match {
    case Some((_st1856, _np1857)) => _rs1854 = _rs1854 :+ _st1856; _cp1855 = _np1857
    case None => _go1858 = false } }
  Some((_rs1854, _cp1855)) }.map { case (_r1848, _p1849) => (new ~(_r1846, _r1848), _p1849) } } match {
    case Some((_st1843, _np1844)) => _rs1841 = _rs1841 :+ _st1843; _cp1842 = _np1844
    case None => _go1845 = false } }
  Some((_rs1841, _cp1842)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r1809, _p1810) => (new ~(_r1807, _r1809), _p1810) } }.flatMap { case (_r1803, _p1804) => {
  var _rs1859: List[Any] = Nil; var _cp1860: Int = _p1804; var _go1863 = true
  while (_go1863) { parseStatementSep(input, _cp1860) match {
    case Some((_st1861, _np1862)) => _rs1859 = _rs1859 :+ _st1861; _cp1860 = _np1862
    case None => _go1863 = false } }
  Some((_rs1859, _cp1860)) }.map { case (_r1805, _p1806) => (new ~(_r1803, _r1805), _p1806) } }.flatMap { case (_r1799, _p1800) => {
  var _rs1864: List[Any] = Nil; var _cp1865: Int = _p1800; var _go1868 = true
  while (_go1868) { parseRescueClause(input, _cp1865) match {
    case Some((_st1866, _np1867)) => _rs1864 = _rs1864 :+ _st1866; _cp1865 = _np1867
    case None => _go1868 = false } }
  Some((_rs1864, _cp1865)) }.map { case (_r1801, _p1802) => (new ~(_r1799, _r1801), _p1802) } }.flatMap { case (_r1795, _p1796) => {
  var _rs1869: List[Any] = Nil; var _cp1870: Int = _p1796; var _go1873 = true
  while (_go1873) { parseStatementSep(input, _cp1870) match {
    case Some((_st1871, _np1872)) => _rs1869 = _rs1869 :+ _st1871; _cp1870 = _np1872
    case None => _go1873 = false } }
  Some((_rs1869, _cp1870)) }.map { case (_r1797, _p1798) => (new ~(_r1795, _r1797), _p1798) } }.flatMap { case (_r1791, _p1792) => ((if (input.startsWith("else", _p1792)) Some(("else", _p1792 + 4)) else None).flatMap { case (_r1890, _p1891) => (if (parseIdentCont(input, _p1891).isEmpty) Some(((), _p1891)) else None).map { case (_r1892, _p1893) => (new ~(_r1890, _r1892), _p1893) } }.flatMap { case (_r1886, _p1887) => parseSpacing(input, _p1887).map { case (_r1888, _p1889) => (new ~(_r1886, _r1888), _p1889) } }.flatMap { case (_r1882, _p1883) => {
  var _rs1894: List[Any] = Nil; var _cp1895: Int = _p1883; var _go1898 = true
  while (_go1898) { parseStatementSep(input, _cp1895) match {
    case Some((_st1896, _np1897)) => _rs1894 = _rs1894 :+ _st1896; _cp1895 = _np1897
    case None => _go1898 = false } }
  Some((_rs1894, _cp1895)) }.map { case (_r1884, _p1885) => (new ~(_r1882, _r1884), _p1885) } }.flatMap { case (_r1878, _p1879) => {
  var _rs1899: List[Any] = Nil; var _cp1900: Int = _p1879; var _go1903 = true
  while (_go1903) { (if (parseDoBlockStop(input, _cp1900).isEmpty) Some(((), _cp1900)) else None).flatMap { case (_r1908, _p1909) => parseStatement(input, _p1909).map { case (_r1910, _p1911) => (new ~(_r1908, _r1910), _p1911) } }.flatMap { case (_r1904, _p1905) => {
  var _rs1912: List[Any] = Nil; var _cp1913: Int = _p1905; var _go1916 = true
  while (_go1916) { parseStatementSep(input, _cp1913) match {
    case Some((_st1914, _np1915)) => _rs1912 = _rs1912 :+ _st1914; _cp1913 = _np1915
    case None => _go1916 = false } }
  Some((_rs1912, _cp1913)) }.map { case (_r1906, _p1907) => (new ~(_r1904, _r1906), _p1907) } } match {
    case Some((_st1901, _np1902)) => _rs1899 = _rs1899 :+ _st1901; _cp1900 = _np1902
    case None => _go1903 = false } }
  Some((_rs1899, _cp1900)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r1880, _p1881) => (new ~(_r1878, _r1880), _p1881) } }.flatMap { case (_r1874, _p1875) => {
  var _rs1917: List[Any] = Nil; var _cp1918: Int = _p1875; var _go1921 = true
  while (_go1921) { parseStatementSep(input, _cp1918) match {
    case Some((_st1919, _np1920)) => _rs1917 = _rs1917 :+ _st1919; _cp1918 = _np1920
    case None => _go1921 = false } }
  Some((_rs1917, _cp1918)) }.map { case (_r1876, _p1877) => (new ~(_r1874, _r1876), _p1877) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1792)))).map { case (_r1793, _p1794) => (new ~(_r1791, _r1793), _p1794) } }.flatMap { case (_r1787, _p1788) => ((if (input.startsWith("ensure", _p1788)) Some(("ensure", _p1788 + 6)) else None).flatMap { case (_r1938, _p1939) => (if (parseIdentCont(input, _p1939).isEmpty) Some(((), _p1939)) else None).map { case (_r1940, _p1941) => (new ~(_r1938, _r1940), _p1941) } }.flatMap { case (_r1934, _p1935) => parseSpacing(input, _p1935).map { case (_r1936, _p1937) => (new ~(_r1934, _r1936), _p1937) } }.flatMap { case (_r1930, _p1931) => {
  var _rs1942: List[Any] = Nil; var _cp1943: Int = _p1931; var _go1946 = true
  while (_go1946) { parseStatementSep(input, _cp1943) match {
    case Some((_st1944, _np1945)) => _rs1942 = _rs1942 :+ _st1944; _cp1943 = _np1945
    case None => _go1946 = false } }
  Some((_rs1942, _cp1943)) }.map { case (_r1932, _p1933) => (new ~(_r1930, _r1932), _p1933) } }.flatMap { case (_r1926, _p1927) => {
  var _rs1947: List[Any] = Nil; var _cp1948: Int = _p1927; var _go1951 = true
  while (_go1951) { (if (parseEndKeyword(input, _cp1948).isEmpty) Some(((), _cp1948)) else None).flatMap { case (_r1956, _p1957) => parseStatement(input, _p1957).map { case (_r1958, _p1959) => (new ~(_r1956, _r1958), _p1959) } }.flatMap { case (_r1952, _p1953) => {
  var _rs1960: List[Any] = Nil; var _cp1961: Int = _p1953; var _go1964 = true
  while (_go1964) { parseStatementSep(input, _cp1961) match {
    case Some((_st1962, _np1963)) => _rs1960 = _rs1960 :+ _st1962; _cp1961 = _np1963
    case None => _go1964 = false } }
  Some((_rs1960, _cp1961)) }.map { case (_r1954, _p1955) => (new ~(_r1952, _r1954), _p1955) } } match {
    case Some((_st1949, _np1950)) => _rs1947 = _rs1947 :+ _st1949; _cp1948 = _np1950
    case None => _go1951 = false } }
  Some((_rs1947, _cp1948)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r1928, _p1929) => (new ~(_r1926, _r1928), _p1929) } }.flatMap { case (_r1922, _p1923) => {
  var _rs1965: List[Any] = Nil; var _cp1966: Int = _p1923; var _go1969 = true
  while (_go1969) { parseStatementSep(input, _cp1966) match {
    case Some((_st1967, _np1968)) => _rs1965 = _rs1965 :+ _st1967; _cp1966 = _np1968
    case None => _go1969 = false } }
  Some((_rs1965, _cp1966)) }.map { case (_r1924, _p1925) => (new ~(_r1922, _r1924), _p1925) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1788)))).map { case (_r1789, _p1790) => (new ~(_r1787, _r1789), _p1790) } }.flatMap { case (_r1783, _p1784) => parseEndKeyword(input, _p1784).map { case (_r1785, _p1786) => (new ~(_r1783, _r1785), _p1786) } }.map { case (r, p) => (_applyAction({  _ => Block(List.empty, List.empty)  }, r), p) }

  def parseBraceBlockStop(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("}", pos)) Some(("}", pos + 1)) else None).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseBraceBlock(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r2002, _p2003) => parseInlineSpacing(input, _p2003).map { case (_r2004, _p2005) => (new ~(_r2002, _r2004), _p2005) } }.flatMap { case (_r1998, _p1999) => {
  var _rs2006: List[Any] = Nil; var _cp2007: Int = _p1999; var _go2010 = true
  while (_go2010) { parseStatementSep(input, _cp2007) match {
    case Some((_st2008, _np2009)) => _rs2006 = _rs2006 :+ _st2008; _cp2007 = _np2009
    case None => _go2010 = false } }
  Some((_rs2006, _cp2007)) }.map { case (_r2000, _p2001) => (new ~(_r1998, _r2000), _p2001) } }.flatMap { case (_r1994, _p1995) => (parseBlockParams(input, _p1995).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p1995)))).map { case (_r1996, _p1997) => (new ~(_r1994, _r1996), _p1997) } }.flatMap { case (_r1990, _p1991) => {
  var _rs2011: List[Any] = Nil; var _cp2012: Int = _p1991; var _go2015 = true
  while (_go2015) { parseStatementSep(input, _cp2012) match {
    case Some((_st2013, _np2014)) => _rs2011 = _rs2011 :+ _st2013; _cp2012 = _np2014
    case None => _go2015 = false } }
  Some((_rs2011, _cp2012)) }.map { case (_r1992, _p1993) => (new ~(_r1990, _r1992), _p1993) } }.flatMap { case (_r1986, _p1987) => {
  var _rs2016: List[Any] = Nil; var _cp2017: Int = _p1987; var _go2020 = true
  while (_go2020) { (if (parseBraceBlockStop(input, _cp2017).isEmpty) Some(((), _cp2017)) else None).flatMap { case (_r2025, _p2026) => parseStatement(input, _p2026).map { case (_r2027, _p2028) => (new ~(_r2025, _r2027), _p2028) } }.flatMap { case (_r2021, _p2022) => {
  var _rs2029: List[Any] = Nil; var _cp2030: Int = _p2022; var _go2033 = true
  while (_go2033) { parseStatementSep(input, _cp2030) match {
    case Some((_st2031, _np2032)) => _rs2029 = _rs2029 :+ _st2031; _cp2030 = _np2032
    case None => _go2033 = false } }
  Some((_rs2029, _cp2030)) }.map { case (_r2023, _p2024) => (new ~(_r2021, _r2023), _p2024) } } match {
    case Some((_st2018, _np2019)) => _rs2016 = _rs2016 :+ _st2018; _cp2017 = _np2019
    case None => _go2020 = false } }
  Some((_rs2016, _cp2017)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r1988, _p1989) => (new ~(_r1986, _r1988), _p1989) } }.flatMap { case (_r1982, _p1983) => {
  var _rs2034: List[Any] = Nil; var _cp2035: Int = _p1983; var _go2038 = true
  while (_go2038) { parseStatementSep(input, _cp2035) match {
    case Some((_st2036, _np2037)) => _rs2034 = _rs2034 :+ _st2036; _cp2035 = _np2037
    case None => _go2038 = false } }
  Some((_rs2034, _cp2035)) }.map { case (_r1984, _p1985) => (new ~(_r1982, _r1984), _p1985) } }.flatMap { case (_r1978, _p1979) => parseSpacing(input, _p1979).map { case (_r1980, _p1981) => (new ~(_r1978, _r1980), _p1981) } }.flatMap { case (_r1974, _p1975) => (if (input.startsWith("}", _p1975)) Some(("}", _p1975 + 1)) else None).map { case (_r1976, _p1977) => (new ~(_r1974, _r1976), _p1977) } }.flatMap { case (_r1970, _p1971) => parseInlineSpacing(input, _p1971).map { case (_r1972, _p1973) => (new ~(_r1970, _r1972), _p1973) } }.map { case (r, p) => (_applyAction({  _ => Block(List.empty, List.empty)  }, r), p) }

  def parseBlockLiteral(input: String, pos: Int): Option[(Any, Int)] = (parseDoBlock(input, pos)).orElse(parseBraceBlock(input, pos))

  def parseCallArgItem(input: String, pos: Int): Option[(Any, Int)] = ((((((((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r2043, _p2044) => parseSpacing(input, _p2044).map { case (_r2045, _p2046) => (new ~(_r2043, _r2045), _p2046) } }.flatMap { case (_r2039, _p2040) => (parseSymbolLiteral(input, _p2040)).orElse(parseExpr(input, _p2040)).map { case (_r2041, _p2042) => (new ~(_r2039, _r2041), _p2042) } })).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r2051, _p2052) => parseSpacing(input, _p2052).map { case (_r2053, _p2054) => (new ~(_r2051, _r2053), _p2054) } }.flatMap { case (_r2047, _p2048) => parseExpr(input, _p2048).map { case (_r2049, _p2050) => (new ~(_r2047, _r2049), _p2050) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2059, _p2060) => parseSpacing(input, _p2060).map { case (_r2061, _p2062) => (new ~(_r2059, _r2061), _p2062) } }.flatMap { case (_r2055, _p2056) => parseExpr(input, _p2056).map { case (_r2057, _p2058) => (new ~(_r2055, _r2057), _p2058) } })).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r2067, _p2068) => parseSpacing(input, _p2068).map { case (_r2069, _p2070) => (new ~(_r2067, _r2069), _p2070) } }.flatMap { case (_r2063, _p2064) => parseExpr(input, _p2064).map { case (_r2065, _p2066) => (new ~(_r2063, _r2065), _p2066) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r2075, _p2076) => parseSpacing(input, _p2076).map { case (_r2077, _p2078) => (new ~(_r2075, _r2077), _p2078) } }.flatMap { case (_r2071, _p2072) => parseExpr(input, _p2072).map { case (_r2073, _p2074) => (new ~(_r2071, _r2073), _p2074) } })).orElse(parseExpr(input, pos).flatMap { case (_r2091, _p2092) => parseSpacing(input, _p2092).map { case (_r2093, _p2094) => (new ~(_r2091, _r2093), _p2094) } }.flatMap { case (_r2087, _p2088) => (if (input.startsWith("=>", _p2088)) Some(("=>", _p2088 + 2)) else None).map { case (_r2089, _p2090) => (new ~(_r2087, _r2089), _p2090) } }.flatMap { case (_r2083, _p2084) => parseSpacing(input, _p2084).map { case (_r2085, _p2086) => (new ~(_r2083, _r2085), _p2086) } }.flatMap { case (_r2079, _p2080) => parseExpr(input, _p2080).map { case (_r2081, _p2082) => (new ~(_r2079, _r2081), _p2082) } })).orElse(parseExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseCallArgs(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r2107, _p2108) => parseSpacing(input, _p2108).map { case (_r2109, _p2110) => (new ~(_r2107, _r2109), _p2110) } }.flatMap { case (_r2103, _p2104) => (parseCallArgItem(input, _p2104).flatMap { case (_r2119, _p2120) => {
  var _rs2123: List[Any] = Nil; var _cp2124: Int = _p2120; var _go2127 = true
  while (_go2127) { parseSpacing(input, _cp2124).flatMap { case (_r2136, _p2137) => (if (input.startsWith(",", _p2137)) Some((",", _p2137 + 1)) else None).map { case (_r2138, _p2139) => (new ~(_r2136, _r2138), _p2139) } }.flatMap { case (_r2132, _p2133) => parseSpacing(input, _p2133).map { case (_r2134, _p2135) => (new ~(_r2132, _r2134), _p2135) } }.flatMap { case (_r2128, _p2129) => parseCallArgItem(input, _p2129).map { case (_r2130, _p2131) => (new ~(_r2128, _r2130), _p2131) } } match {
    case Some((_st2125, _np2126)) => _rs2123 = _rs2123 :+ _st2125; _cp2124 = _np2126
    case None => _go2127 = false } }
  Some((_rs2123, _cp2124)) }.map { case (_r2121, _p2122) => (new ~(_r2119, _r2121), _p2122) } }.flatMap { case (_r2115, _p2116) => (parseSpacing(input, _p2116).flatMap { case (_r2140, _p2141) => (if (input.startsWith(",", _p2141)) Some((",", _p2141 + 1)) else None).map { case (_r2142, _p2143) => (new ~(_r2140, _r2142), _p2143) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2116)))).map { case (_r2117, _p2118) => (new ~(_r2115, _r2117), _p2118) } }.flatMap { case (_r2111, _p2112) => parseSpacing(input, _p2112).map { case (_r2113, _p2114) => (new ~(_r2111, _r2113), _p2114) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2104)))).map { case (_r2105, _p2106) => (new ~(_r2103, _r2105), _p2106) } }.flatMap { case (_r2099, _p2100) => (if (input.startsWith(")", _p2100)) Some((")", _p2100 + 1)) else None).map { case (_r2101, _p2102) => (new ~(_r2099, _r2101), _p2102) } }.flatMap { case (_r2095, _p2096) => parseInlineSpacing(input, _p2096).map { case (_r2097, _p2098) => (new ~(_r2095, _r2097), _p2098) } }.map { case (r, p) => (_applyAction({  _ => List.empty[Expr]  }, r), p) }

  def parseCommandArgStop(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r2144, _p2145) => (if (parseIdentCont(input, _p2145).isEmpty) Some(((), _p2145)) else None).map { case (_r2146, _p2147) => (new ~(_r2144, _r2146), _p2147) } }).orElse((if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r2148, _p2149) => (if (parseIdentCont(input, _p2149).isEmpty) Some(((), _p2149)) else None).map { case (_r2150, _p2151) => (new ~(_r2148, _r2150), _p2151) } })).orElse((if (input.startsWith("while", pos)) Some(("while", pos + 5)) else None).flatMap { case (_r2152, _p2153) => (if (parseIdentCont(input, _p2153).isEmpty) Some(((), _p2153)) else None).map { case (_r2154, _p2155) => (new ~(_r2152, _r2154), _p2155) } })).orElse((if (input.startsWith("until", pos)) Some(("until", pos + 5)) else None).flatMap { case (_r2156, _p2157) => (if (parseIdentCont(input, _p2157).isEmpty) Some(((), _p2157)) else None).map { case (_r2158, _p2159) => (new ~(_r2156, _r2158), _p2159) } })).orElse((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r2160, _p2161) => (if (parseIdentCont(input, _p2161).isEmpty) Some(((), _p2161)) else None).map { case (_r2162, _p2163) => (new ~(_r2160, _r2162), _p2163) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseCommandArgItem(input: String, pos: Int): Option[(Any, Int)] = (((((((if (input.startsWith("&", pos)) Some(("&", pos + 1)) else None).flatMap { case (_r2168, _p2169) => parseSpacing(input, _p2169).map { case (_r2170, _p2171) => (new ~(_r2168, _r2170), _p2171) } }.flatMap { case (_r2164, _p2165) => (parseSymbolLiteral(input, _p2165)).orElse(parseExpr(input, _p2165)).map { case (_r2166, _p2167) => (new ~(_r2164, _r2166), _p2167) } }).orElse((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r2176, _p2177) => parseSpacing(input, _p2177).map { case (_r2178, _p2179) => (new ~(_r2176, _r2178), _p2179) } }.flatMap { case (_r2172, _p2173) => parseExpr(input, _p2173).map { case (_r2174, _p2175) => (new ~(_r2172, _r2174), _p2175) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2184, _p2185) => parseSpacing(input, _p2185).map { case (_r2186, _p2187) => (new ~(_r2184, _r2186), _p2187) } }.flatMap { case (_r2180, _p2181) => parseExpr(input, _p2181).map { case (_r2182, _p2183) => (new ~(_r2180, _r2182), _p2183) } })).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r2192, _p2193) => parseSpacing(input, _p2193).map { case (_r2194, _p2195) => (new ~(_r2192, _r2194), _p2195) } }.flatMap { case (_r2188, _p2189) => parseExpr(input, _p2189).map { case (_r2190, _p2191) => (new ~(_r2188, _r2190), _p2191) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r2200, _p2201) => parseSpacing(input, _p2201).map { case (_r2202, _p2203) => (new ~(_r2200, _r2202), _p2203) } }.flatMap { case (_r2196, _p2197) => parseExpr(input, _p2197).map { case (_r2198, _p2199) => (new ~(_r2196, _r2198), _p2199) } })).orElse(parseExpr(input, pos).flatMap { case (_r2216, _p2217) => parseSpacing(input, _p2217).map { case (_r2218, _p2219) => (new ~(_r2216, _r2218), _p2219) } }.flatMap { case (_r2212, _p2213) => (if (input.startsWith("=>", _p2213)) Some(("=>", _p2213 + 2)) else None).map { case (_r2214, _p2215) => (new ~(_r2212, _r2214), _p2215) } }.flatMap { case (_r2208, _p2209) => parseSpacing(input, _p2209).map { case (_r2210, _p2211) => (new ~(_r2208, _r2210), _p2211) } }.flatMap { case (_r2204, _p2205) => parseExpr(input, _p2205).map { case (_r2206, _p2207) => (new ~(_r2204, _r2206), _p2207) } })).orElse(parseExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseCommandArgs(input: String, pos: Int): Option[(Any, Int)] = (if (parseCommandArgStop(input, pos).isEmpty) Some(((), pos)) else None).flatMap { case (_r2224, _p2225) => parseCommandArgItem(input, _p2225).map { case (_r2226, _p2227) => (new ~(_r2224, _r2226), _p2227) } }.flatMap { case (_r2220, _p2221) => {
  var _rs2228: List[Any] = Nil; var _cp2229: Int = _p2221; var _go2232 = true
  while (_go2232) { parseInlineSpacing(input, _cp2229).flatMap { case (_r2241, _p2242) => (if (input.startsWith(",", _p2242)) Some((",", _p2242 + 1)) else None).map { case (_r2243, _p2244) => (new ~(_r2241, _r2243), _p2244) } }.flatMap { case (_r2237, _p2238) => parseSpacing(input, _p2238).map { case (_r2239, _p2240) => (new ~(_r2237, _r2239), _p2240) } }.flatMap { case (_r2233, _p2234) => parseCommandArgItem(input, _p2234).map { case (_r2235, _p2236) => (new ~(_r2233, _r2235), _p2236) } } match {
    case Some((_st2230, _np2231)) => _rs2228 = _rs2228 :+ _st2230; _cp2229 = _np2231
    case None => _go2232 = false } }
  Some((_rs2228, _cp2229)) }.map { case (_r2222, _p2223) => (new ~(_r2220, _r2222), _p2223) } }.map { case (r, p) => (_applyAction({  _ => List.empty[Expr]  }, r), p) }

  def parseSubscriptArgs(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("[", pos)) Some(("[", pos + 1)) else None).flatMap { case (_r2257, _p2258) => parseSpacing(input, _p2258).map { case (_r2259, _p2260) => (new ~(_r2257, _r2259), _p2260) } }.flatMap { case (_r2253, _p2254) => (parseCallArgItem(input, _p2254).flatMap { case (_r2269, _p2270) => {
  var _rs2273: List[Any] = Nil; var _cp2274: Int = _p2270; var _go2277 = true
  while (_go2277) { parseSpacing(input, _cp2274).flatMap { case (_r2286, _p2287) => (if (input.startsWith(",", _p2287)) Some((",", _p2287 + 1)) else None).map { case (_r2288, _p2289) => (new ~(_r2286, _r2288), _p2289) } }.flatMap { case (_r2282, _p2283) => parseSpacing(input, _p2283).map { case (_r2284, _p2285) => (new ~(_r2282, _r2284), _p2285) } }.flatMap { case (_r2278, _p2279) => parseCallArgItem(input, _p2279).map { case (_r2280, _p2281) => (new ~(_r2278, _r2280), _p2281) } } match {
    case Some((_st2275, _np2276)) => _rs2273 = _rs2273 :+ _st2275; _cp2274 = _np2276
    case None => _go2277 = false } }
  Some((_rs2273, _cp2274)) }.map { case (_r2271, _p2272) => (new ~(_r2269, _r2271), _p2272) } }.flatMap { case (_r2265, _p2266) => (parseSpacing(input, _p2266).flatMap { case (_r2290, _p2291) => (if (input.startsWith(",", _p2291)) Some((",", _p2291 + 1)) else None).map { case (_r2292, _p2293) => (new ~(_r2290, _r2292), _p2293) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2266)))).map { case (_r2267, _p2268) => (new ~(_r2265, _r2267), _p2268) } }.flatMap { case (_r2261, _p2262) => parseSpacing(input, _p2262).map { case (_r2263, _p2264) => (new ~(_r2261, _r2263), _p2264) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2254)))).map { case (_r2255, _p2256) => (new ~(_r2253, _r2255), _p2256) } }.flatMap { case (_r2249, _p2250) => (if (input.startsWith("]", _p2250)) Some(("]", _p2250 + 1)) else None).map { case (_r2251, _p2252) => (new ~(_r2249, _r2251), _p2252) } }.flatMap { case (_r2245, _p2246) => parseInlineSpacing(input, _p2246).map { case (_r2247, _p2248) => (new ~(_r2245, _r2247), _p2248) } }.map { case (r, p) => (_applyAction({  _ => List.empty[Expr]  }, r), p) }

  def parseDotSep(input: String, pos: Int): Option[(Any, Int)] = (parseInlineSpacing(input, pos).flatMap { case (_r2298, _p2299) => (((if (input.startsWith("&.", _p2299)) Some(("&.", _p2299 + 2)) else None)).orElse((if (input.startsWith("::", _p2299)) Some(("::", _p2299 + 2)) else None))).orElse((if (input.startsWith(".", _p2299)) Some((".", _p2299 + 1)) else None)).map { case (_r2300, _p2301) => (new ~(_r2298, _r2300), _p2301) } }.flatMap { case (_r2294, _p2295) => parseInlineSpacing(input, _p2295).map { case (_r2296, _p2297) => (new ~(_r2294, _r2296), _p2297) } }).orElse(parseInlineSpacing(input, pos).flatMap { case (_r2314, _p2315) => (if (input.startsWith("\n", _p2315)) Some(("\n", _p2315 + 1)) else None).map { case (_r2316, _p2317) => (new ~(_r2314, _r2316), _p2317) } }.flatMap { case (_r2310, _p2311) => parseInlineSpacing(input, _p2311).map { case (_r2312, _p2313) => (new ~(_r2310, _r2312), _p2313) } }.flatMap { case (_r2306, _p2307) => (((if (input.startsWith("&.", _p2307)) Some(("&.", _p2307 + 2)) else None)).orElse((if (input.startsWith("::", _p2307)) Some(("::", _p2307 + 2)) else None))).orElse((if (input.startsWith(".", _p2307)) Some((".", _p2307 + 1)) else None)).map { case (_r2308, _p2309) => (new ~(_r2306, _r2308), _p2309) } }.flatMap { case (_r2302, _p2303) => parseInlineSpacing(input, _p2303).map { case (_r2304, _p2305) => (new ~(_r2302, _r2304), _p2305) } }).map { case (r, p) => (_applyAction({  _ => "."  }, r), p) }

  def parsePrimaryExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(30, pos) {
    ((((((((((((((((((((((((((((((parseLambdaLiteral(input, pos)).orElse(parseSingletonClassExpr(input, pos))).orElse(parseBeginExpr(input, pos))).orElse(parseDefExprForm(input, pos))).orElse(parseReturnExpr(input, pos))).orElse(parseYieldExpr(input, pos))).orElse(parseIfExpr(input, pos))).orElse(parseUnlessExpr(input, pos))).orElse(parseCaseExpr(input, pos))).orElse(parseSelfExpr(input, pos))).orElse(parseBoolLiteral(input, pos))).orElse(parseNilLiteral(input, pos))).orElse(parseConstRef(input, pos))).orElse(parseVariable(input, pos))).orElse(parseFloatLiteral(input, pos))).orElse(parseIntegerLiteral(input, pos))).orElse(parseCharLiteral(input, pos))).orElse(parseStringLiteral(input, pos))).orElse(parseSingleQuotedStringLiteral(input, pos))).orElse(parseBacktickLiteral(input, pos))).orElse(parsePercentCommand(input, pos))).orElse(parsePercentQuotedStringLiteral(input, pos))).orElse(parsePercentSymbolLiteralExpr(input, pos))).orElse(parsePercentWordArray(input, pos))).orElse(parsePercentSymbolArray(input, pos))).orElse(parseRegexLiteral(input, pos))).orElse(parseSymbolLiteral(input, pos))).orElse(parseArrayLiteral(input, pos))).orElse(parseHeredocLiteral(input, pos))).orElse(parseHashLiteral(input, pos))).orElse(parseParenExpr(input, pos))
  }

  def parseMethodName(input: String, pos: Int): Option[(Any, Int)] = (((parseSymbolOperatorName(input, pos)).orElse(parseConstNameNoSpace(input, pos))).orElse(parseMethodIdentifierRaw(input, pos))).orElse(((((((((((if (input.startsWith("private", pos)) Some(("private", pos + 7)) else None).flatMap { case (_r2322, _p2323) => (if (parseIdentCont(input, _p2323).isEmpty) Some(((), _p2323)) else None).map { case (_r2324, _p2325) => (new ~(_r2322, _r2324), _p2325) } }).orElse((if (input.startsWith("public", pos)) Some(("public", pos + 6)) else None).flatMap { case (_r2326, _p2327) => (if (parseIdentCont(input, _p2327).isEmpty) Some(((), _p2327)) else None).map { case (_r2328, _p2329) => (new ~(_r2326, _r2328), _p2329) } })).orElse((if (input.startsWith("protected", pos)) Some(("protected", pos + 9)) else None).flatMap { case (_r2330, _p2331) => (if (parseIdentCont(input, _p2331).isEmpty) Some(((), _p2331)) else None).map { case (_r2332, _p2333) => (new ~(_r2330, _r2332), _p2333) } })).orElse((if (input.startsWith("ruby2_keywords", pos)) Some(("ruby2_keywords", pos + 14)) else None).flatMap { case (_r2334, _p2335) => (if (parseIdentCont(input, _p2335).isEmpty) Some(((), _p2335)) else None).map { case (_r2336, _p2337) => (new ~(_r2334, _r2336), _p2337) } })).orElse((if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r2338, _p2339) => (if (parseIdentCont(input, _p2339).isEmpty) Some(((), _p2339)) else None).map { case (_r2340, _p2341) => (new ~(_r2338, _r2340), _p2341) } })).orElse((if (input.startsWith("def", pos)) Some(("def", pos + 3)) else None).flatMap { case (_r2342, _p2343) => (if (parseIdentCont(input, _p2343).isEmpty) Some(((), _p2343)) else None).map { case (_r2344, _p2345) => (new ~(_r2342, _r2344), _p2345) } })).orElse((if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r2346, _p2347) => (if (parseIdentCont(input, _p2347).isEmpty) Some(((), _p2347)) else None).map { case (_r2348, _p2349) => (new ~(_r2346, _r2348), _p2349) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r2350, _p2351) => (if (parseIdentCont(input, _p2351).isEmpty) Some(((), _p2351)) else None).map { case (_r2352, _p2353) => (new ~(_r2350, _r2352), _p2353) } })).orElse((if (input.startsWith("for", pos)) Some(("for", pos + 3)) else None).flatMap { case (_r2354, _p2355) => (if (parseIdentCont(input, _p2355).isEmpty) Some(((), _p2355)) else None).map { case (_r2356, _p2357) => (new ~(_r2354, _r2356), _p2357) } })).orElse((if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r2358, _p2359) => (if (parseIdentCont(input, _p2359).isEmpty) Some(((), _p2359)) else None).map { case (_r2360, _p2361) => (new ~(_r2358, _r2360), _p2361) } })).flatMap { case (_r2318, _p2319) => parseInlineSpacing(input, _p2319).map { case (_r2320, _p2321) => (new ~(_r2318, _r2320), _p2321) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseCallSuffix(input: String, pos: Int): Option[(Any, Int)] = (((parseDotSep(input, pos).flatMap { case (_r2366, _p2367) => parseMethodName(input, _p2367).map { case (_r2368, _p2369) => (new ~(_r2366, _r2368), _p2369) } }.flatMap { case (_r2362, _p2363) => (parseCallArgs(input, _p2363).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2363)))).map { case (_r2364, _p2365) => (new ~(_r2362, _r2364), _p2365) } }).orElse(parseDotSep(input, pos).flatMap { case (_r2370, _p2371) => parseCallArgs(input, _p2371).map { case (_r2372, _p2373) => (new ~(_r2370, _r2372), _p2373) } })).orElse(parseSubscriptArgs(input, pos))).orElse(parseInlineSpacing(input, pos).flatMap { case (_r2374, _p2375) => parseBlockLiteral(input, _p2375).map { case (_r2376, _p2377) => (new ~(_r2374, _r2376), _p2377) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseFunctionCallExpr(input: String, pos: Int): Option[(Any, Int)] = parseMethodIdentifierRaw(input, pos).flatMap { case (_r2382, _p2383) => parseInlineSpacing(input, _p2383).map { case (_r2384, _p2385) => (new ~(_r2382, _r2384), _p2385) } }.flatMap { case (_r2378, _p2379) => parseCallArgs(input, _p2379).map { case (_r2380, _p2381) => (new ~(_r2378, _r2380), _p2381) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseCommandCallExpr(input: String, pos: Int): Option[(Any, Int)] = parseMethodIdentifierRaw(input, pos).flatMap { case (_r2390, _p2391) => parseSpacing1(input, _p2391).map { case (_r2392, _p2393) => (new ~(_r2390, _r2392), _p2393) } }.flatMap { case (_r2386, _p2387) => parseCommandArgs(input, _p2387).map { case (_r2388, _p2389) => (new ~(_r2386, _r2388), _p2389) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parsePostfixExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(31, pos) {
    ((parseFunctionCallExpr(input, pos)).orElse(parseCommandCallExpr(input, pos))).orElse(parsePrimaryExpr(input, pos)).flatMap { case (_r2394, _p2395) => {
  var _rs2398: List[Any] = Nil; var _cp2399: Int = _p2395; var _go2402 = true
  while (_go2402) { parseCallSuffix(input, _cp2399) match {
    case Some((_st2400, _np2401)) => _rs2398 = _rs2398 :+ _st2400; _cp2399 = _np2401
    case None => _go2402 = false } }
  Some((_rs2398, _cp2399)) }.map { case (_r2396, _p2397) => (new ~(_r2394, _r2396), _p2397) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parsePowerExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(32, pos) {
    parsePostfixExpr(input, pos).flatMap { case (_r2403, _p2404) => ((if (input.startsWith("**", _p2404)) Some(("**", _p2404 + 2)) else None).flatMap { case (_r2411, _p2412) => parseSpacing(input, _p2412).map { case (_r2413, _p2414) => (new ~(_r2411, _r2413), _p2414) } }.flatMap { case (_r2407, _p2408) => parsePowerExpr(input, _p2408).map { case (_r2409, _p2410) => (new ~(_r2407, _r2409), _p2410) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2404)))).map { case (_r2405, _p2406) => (new ~(_r2403, _r2405), _p2406) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseUnaryOpSym(input: String, pos: Int): Option[(Any, Int)] = ((((((if (input.startsWith("not", pos)) Some(("not", pos + 3)) else None).flatMap { case (_r2419, _p2420) => (if (parseIdentCont(input, _p2420).isEmpty) Some(((), _p2420)) else None).map { case (_r2421, _p2422) => (new ~(_r2419, _r2421), _p2422) } }.flatMap { case (_r2415, _p2416) => parseSpacing(input, _p2416).map { case (_r2417, _p2418) => (new ~(_r2415, _r2417), _p2418) } }).orElse((if (input.startsWith("!", pos)) Some(("!", pos + 1)) else None))).orElse((if (input.startsWith("~", pos)) Some(("~", pos + 1)) else None))).orElse((if (input.startsWith("-", pos)) Some(("-", pos + 1)) else None))).orElse((if (input.startsWith("+", pos)) Some(("+", pos + 1)) else None))).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseUnaryExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(33, pos) {
    (parseUnaryOpSym(input, pos).flatMap { case (_r2427, _p2428) => parseSpacing(input, _p2428).map { case (_r2429, _p2430) => (new ~(_r2427, _r2429), _p2430) } }.flatMap { case (_r2423, _p2424) => parseUnaryExpr(input, _p2424).map { case (_r2425, _p2426) => (new ~(_r2423, _r2425), _p2426) } }).orElse(parsePowerExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseMulDivExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(34, pos) {
    parseUnaryExpr(input, pos).flatMap { case (_r2431, _p2432) => {
  var _rs2435: List[Any] = Nil; var _cp2436: Int = _p2432; var _go2439 = true
  while (_go2439) { (((if (input.startsWith("*", _cp2436)) Some(("*", _cp2436 + 1)) else None).flatMap { case (_r2448, _p2449) => (if ((if (input.startsWith("=", _p2449)) Some(("=", _p2449 + 1)) else None).isEmpty) Some(((), _p2449)) else None).map { case (_r2450, _p2451) => (new ~(_r2448, _r2450), _p2451) } }).orElse((if (input.startsWith("/", _cp2436)) Some(("/", _cp2436 + 1)) else None).flatMap { case (_r2452, _p2453) => (if (((if (input.startsWith("=", _p2453)) Some(("=", _p2453 + 1)) else None)).orElse((if (input.startsWith("}", _p2453)) Some(("}", _p2453 + 1)) else None)).isEmpty) Some(((), _p2453)) else None).map { case (_r2454, _p2455) => (new ~(_r2452, _r2454), _p2455) } })).orElse((if (input.startsWith("%", _cp2436)) Some(("%", _cp2436 + 1)) else None).flatMap { case (_r2456, _p2457) => (if ((if (input.startsWith("=", _p2457)) Some(("=", _p2457 + 1)) else None).isEmpty) Some(((), _p2457)) else None).map { case (_r2458, _p2459) => (new ~(_r2456, _r2458), _p2459) } }).flatMap { case (_r2444, _p2445) => parseSpacing(input, _p2445).map { case (_r2446, _p2447) => (new ~(_r2444, _r2446), _p2447) } }.flatMap { case (_r2440, _p2441) => parseUnaryExpr(input, _p2441).map { case (_r2442, _p2443) => (new ~(_r2440, _r2442), _p2443) } } match {
    case Some((_st2437, _np2438)) => _rs2435 = _rs2435 :+ _st2437; _cp2436 = _np2438
    case None => _go2439 = false } }
  Some((_rs2435, _cp2436)) }.map { case (_r2433, _p2434) => (new ~(_r2431, _r2433), _p2434) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseAddSubExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(35, pos) {
    parseMulDivExpr(input, pos).flatMap { case (_r2460, _p2461) => {
  var _rs2464: List[Any] = Nil; var _cp2465: Int = _p2461; var _go2468 = true
  while (_go2468) { ((if (input.startsWith("+", _cp2465)) Some(("+", _cp2465 + 1)) else None).flatMap { case (_r2477, _p2478) => (if (((if (input.startsWith("=", _p2478)) Some(("=", _p2478 + 1)) else None)).orElse((if (input.startsWith("@", _p2478)) Some(("@", _p2478 + 1)) else None)).isEmpty) Some(((), _p2478)) else None).map { case (_r2479, _p2480) => (new ~(_r2477, _r2479), _p2480) } }).orElse((if (input.startsWith("-", _cp2465)) Some(("-", _cp2465 + 1)) else None).flatMap { case (_r2481, _p2482) => (if ((((if (input.startsWith("=", _p2482)) Some(("=", _p2482 + 1)) else None)).orElse((if (input.startsWith("@", _p2482)) Some(("@", _p2482 + 1)) else None))).orElse((if (input.startsWith(">", _p2482)) Some((">", _p2482 + 1)) else None)).isEmpty) Some(((), _p2482)) else None).map { case (_r2483, _p2484) => (new ~(_r2481, _r2483), _p2484) } }).flatMap { case (_r2473, _p2474) => parseSpacing(input, _p2474).map { case (_r2475, _p2476) => (new ~(_r2473, _r2475), _p2476) } }.flatMap { case (_r2469, _p2470) => parseMulDivExpr(input, _p2470).map { case (_r2471, _p2472) => (new ~(_r2469, _r2471), _p2472) } } match {
    case Some((_st2466, _np2467)) => _rs2464 = _rs2464 :+ _st2466; _cp2465 = _np2467
    case None => _go2468 = false } }
  Some((_rs2464, _cp2465)) }.map { case (_r2462, _p2463) => (new ~(_r2460, _r2462), _p2463) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseShiftExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(36, pos) {
    parseAddSubExpr(input, pos).flatMap { case (_r2485, _p2486) => {
  var _rs2489: List[Any] = Nil; var _cp2490: Int = _p2486; var _go2493 = true
  while (_go2493) { ((if (input.startsWith("<<", _cp2490)) Some(("<<", _cp2490 + 2)) else None)).orElse((if (input.startsWith(">>", _cp2490)) Some((">>", _cp2490 + 2)) else None)).flatMap { case (_r2502, _p2503) => (if ((if (input.startsWith("=", _p2503)) Some(("=", _p2503 + 1)) else None).isEmpty) Some(((), _p2503)) else None).map { case (_r2504, _p2505) => (new ~(_r2502, _r2504), _p2505) } }.flatMap { case (_r2498, _p2499) => parseSpacing(input, _p2499).map { case (_r2500, _p2501) => (new ~(_r2498, _r2500), _p2501) } }.flatMap { case (_r2494, _p2495) => parseAddSubExpr(input, _p2495).map { case (_r2496, _p2497) => (new ~(_r2494, _r2496), _p2497) } } match {
    case Some((_st2491, _np2492)) => _rs2489 = _rs2489 :+ _st2491; _cp2490 = _np2492
    case None => _go2493 = false } }
  Some((_rs2489, _cp2490)) }.map { case (_r2487, _p2488) => (new ~(_r2485, _r2487), _p2488) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseBitAndExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(37, pos) {
    parseShiftExpr(input, pos).flatMap { case (_r2506, _p2507) => {
  var _rs2510: List[Any] = Nil; var _cp2511: Int = _p2507; var _go2514 = true
  while (_go2514) { (if (input.startsWith("&", _cp2511)) Some(("&", _cp2511 + 1)) else None).flatMap { case (_r2523, _p2524) => (if (((if (input.startsWith("&", _p2524)) Some(("&", _p2524 + 1)) else None)).orElse((if (input.startsWith("=", _p2524)) Some(("=", _p2524 + 1)) else None)).isEmpty) Some(((), _p2524)) else None).map { case (_r2525, _p2526) => (new ~(_r2523, _r2525), _p2526) } }.flatMap { case (_r2519, _p2520) => parseSpacing(input, _p2520).map { case (_r2521, _p2522) => (new ~(_r2519, _r2521), _p2522) } }.flatMap { case (_r2515, _p2516) => parseShiftExpr(input, _p2516).map { case (_r2517, _p2518) => (new ~(_r2515, _r2517), _p2518) } } match {
    case Some((_st2512, _np2513)) => _rs2510 = _rs2510 :+ _st2512; _cp2511 = _np2513
    case None => _go2514 = false } }
  Some((_rs2510, _cp2511)) }.map { case (_r2508, _p2509) => (new ~(_r2506, _r2508), _p2509) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseBitOrExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(38, pos) {
    parseBitAndExpr(input, pos).flatMap { case (_r2527, _p2528) => {
  var _rs2531: List[Any] = Nil; var _cp2532: Int = _p2528; var _go2535 = true
  while (_go2535) { ((if (input.startsWith("|", _cp2532)) Some(("|", _cp2532 + 1)) else None).flatMap { case (_r2544, _p2545) => (if (((if (input.startsWith("=", _p2545)) Some(("=", _p2545 + 1)) else None)).orElse((if (input.startsWith("|", _p2545)) Some(("|", _p2545 + 1)) else None)).isEmpty) Some(((), _p2545)) else None).map { case (_r2546, _p2547) => (new ~(_r2544, _r2546), _p2547) } }).orElse((if (input.startsWith("^", _cp2532)) Some(("^", _cp2532 + 1)) else None).flatMap { case (_r2548, _p2549) => (if ((if (input.startsWith("=", _p2549)) Some(("=", _p2549 + 1)) else None).isEmpty) Some(((), _p2549)) else None).map { case (_r2550, _p2551) => (new ~(_r2548, _r2550), _p2551) } }).flatMap { case (_r2540, _p2541) => parseSpacing(input, _p2541).map { case (_r2542, _p2543) => (new ~(_r2540, _r2542), _p2543) } }.flatMap { case (_r2536, _p2537) => parseBitAndExpr(input, _p2537).map { case (_r2538, _p2539) => (new ~(_r2536, _r2538), _p2539) } } match {
    case Some((_st2533, _np2534)) => _rs2531 = _rs2531 :+ _st2533; _cp2532 = _np2534
    case None => _go2535 = false } }
  Some((_rs2531, _cp2532)) }.map { case (_r2529, _p2530) => (new ~(_r2527, _r2529), _p2530) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseRelationalExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(39, pos) {
    parseBitOrExpr(input, pos).flatMap { case (_r2552, _p2553) => {
  var _rs2556: List[Any] = Nil; var _cp2557: Int = _p2553; var _go2560 = true
  while (_go2560) { ((((if (input.startsWith("<=", _cp2557)) Some(("<=", _cp2557 + 2)) else None)).orElse((if (input.startsWith(">=", _cp2557)) Some((">=", _cp2557 + 2)) else None))).orElse((if (input.startsWith("<", _cp2557)) Some(("<", _cp2557 + 1)) else None).flatMap { case (_r2569, _p2570) => (if (((if (input.startsWith("<", _p2570)) Some(("<", _p2570 + 1)) else None)).orElse((if (input.startsWith("=", _p2570)) Some(("=", _p2570 + 1)) else None)).isEmpty) Some(((), _p2570)) else None).map { case (_r2571, _p2572) => (new ~(_r2569, _r2571), _p2572) } })).orElse((if (input.startsWith(">", _cp2557)) Some((">", _cp2557 + 1)) else None).flatMap { case (_r2573, _p2574) => (if (((if (input.startsWith(">", _p2574)) Some((">", _p2574 + 1)) else None)).orElse((if (input.startsWith("=", _p2574)) Some(("=", _p2574 + 1)) else None)).isEmpty) Some(((), _p2574)) else None).map { case (_r2575, _p2576) => (new ~(_r2573, _r2575), _p2576) } }).flatMap { case (_r2565, _p2566) => parseSpacing(input, _p2566).map { case (_r2567, _p2568) => (new ~(_r2565, _r2567), _p2568) } }.flatMap { case (_r2561, _p2562) => parseBitOrExpr(input, _p2562).map { case (_r2563, _p2564) => (new ~(_r2561, _r2563), _p2564) } } match {
    case Some((_st2558, _np2559)) => _rs2556 = _rs2556 :+ _st2558; _cp2557 = _np2559
    case None => _go2560 = false } }
  Some((_rs2556, _cp2557)) }.map { case (_r2554, _p2555) => (new ~(_r2552, _r2554), _p2555) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseEqualityExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(40, pos) {
    parseRelationalExpr(input, pos).flatMap { case (_r2577, _p2578) => {
  var _rs2581: List[Any] = Nil; var _cp2582: Int = _p2578; var _go2585 = true
  while (_go2585) { ((((((if (input.startsWith("===", _cp2582)) Some(("===", _cp2582 + 3)) else None)).orElse((if (input.startsWith("<=>", _cp2582)) Some(("<=>", _cp2582 + 3)) else None))).orElse((if (input.startsWith("=~", _cp2582)) Some(("=~", _cp2582 + 2)) else None))).orElse((if (input.startsWith("!~", _cp2582)) Some(("!~", _cp2582 + 2)) else None))).orElse((if (input.startsWith("!=", _cp2582)) Some(("!=", _cp2582 + 2)) else None))).orElse((if (input.startsWith("==", _cp2582)) Some(("==", _cp2582 + 2)) else None)).flatMap { case (_r2590, _p2591) => parseSpacing(input, _p2591).map { case (_r2592, _p2593) => (new ~(_r2590, _r2592), _p2593) } }.flatMap { case (_r2586, _p2587) => parseRelationalExpr(input, _p2587).map { case (_r2588, _p2589) => (new ~(_r2586, _r2588), _p2589) } } match {
    case Some((_st2583, _np2584)) => _rs2581 = _rs2581 :+ _st2583; _cp2582 = _np2584
    case None => _go2585 = false } }
  Some((_rs2581, _cp2582)) }.map { case (_r2579, _p2580) => (new ~(_r2577, _r2579), _p2580) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseRangeOp(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("...", pos)) Some(("...", pos + 3)) else None)).orElse((if (input.startsWith("..", pos)) Some(("..", pos + 2)) else None)).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseRangeExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(41, pos) {
    (parseRangeOp(input, pos).flatMap { case (_r2598, _p2599) => parseSpacing(input, _p2599).map { case (_r2600, _p2601) => (new ~(_r2598, _r2600), _p2601) } }.flatMap { case (_r2594, _p2595) => parseEqualityExpr(input, _p2595).map { case (_r2596, _p2597) => (new ~(_r2594, _r2596), _p2597) } }).orElse(parseEqualityExpr(input, pos).flatMap { case (_r2602, _p2603) => (parseRangeOp(input, _p2603).flatMap { case (_r2610, _p2611) => parseSpacing(input, _p2611).map { case (_r2612, _p2613) => (new ~(_r2610, _r2612), _p2613) } }.flatMap { case (_r2606, _p2607) => (parseEqualityExpr(input, _p2607).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2607)))).map { case (_r2608, _p2609) => (new ~(_r2606, _r2608), _p2609) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2603)))).map { case (_r2604, _p2605) => (new ~(_r2602, _r2604), _p2605) } }).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseAndExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(42, pos) {
    parseRangeExpr(input, pos).flatMap { case (_r2614, _p2615) => {
  var _rs2618: List[Any] = Nil; var _cp2619: Int = _p2615; var _go2622 = true
  while (_go2622) { (if (input.startsWith("&&", _cp2619)) Some(("&&", _cp2619 + 2)) else None).flatMap { case (_r2631, _p2632) => (if ((if (input.startsWith("=", _p2632)) Some(("=", _p2632 + 1)) else None).isEmpty) Some(((), _p2632)) else None).map { case (_r2633, _p2634) => (new ~(_r2631, _r2633), _p2634) } }.flatMap { case (_r2627, _p2628) => parseSpacing(input, _p2628).map { case (_r2629, _p2630) => (new ~(_r2627, _r2629), _p2630) } }.flatMap { case (_r2623, _p2624) => parseRangeExpr(input, _p2624).map { case (_r2625, _p2626) => (new ~(_r2623, _r2625), _p2626) } } match {
    case Some((_st2620, _np2621)) => _rs2618 = _rs2618 :+ _st2620; _cp2619 = _np2621
    case None => _go2622 = false } }
  Some((_rs2618, _cp2619)) }.map { case (_r2616, _p2617) => (new ~(_r2614, _r2616), _p2617) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseOrExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(43, pos) {
    parseAndExpr(input, pos).flatMap { case (_r2635, _p2636) => {
  var _rs2639: List[Any] = Nil; var _cp2640: Int = _p2636; var _go2643 = true
  while (_go2643) { (if (input.startsWith("||", _cp2640)) Some(("||", _cp2640 + 2)) else None).flatMap { case (_r2652, _p2653) => (if ((if (input.startsWith("=", _p2653)) Some(("=", _p2653 + 1)) else None).isEmpty) Some(((), _p2653)) else None).map { case (_r2654, _p2655) => (new ~(_r2652, _r2654), _p2655) } }.flatMap { case (_r2648, _p2649) => parseSpacing(input, _p2649).map { case (_r2650, _p2651) => (new ~(_r2648, _r2650), _p2651) } }.flatMap { case (_r2644, _p2645) => parseAndExpr(input, _p2645).map { case (_r2646, _p2647) => (new ~(_r2644, _r2646), _p2647) } } match {
    case Some((_st2641, _np2642)) => _rs2639 = _rs2639 :+ _st2641; _cp2640 = _np2642
    case None => _go2643 = false } }
  Some((_rs2639, _cp2640)) }.map { case (_r2637, _p2638) => (new ~(_r2635, _r2637), _p2638) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseInPatternPrimary(input: String, pos: Int): Option[(Any, Int)] = ((((((((((((((parseConstRef(input, pos)).orElse(parseVariable(input, pos))).orElse(parseSymbolLiteral(input, pos))).orElse(parseStringLiteral(input, pos))).orElse(parseSingleQuotedStringLiteral(input, pos))).orElse(parseNilLiteral(input, pos))).orElse(parseBoolLiteral(input, pos))).orElse(parseIntegerLiteral(input, pos))).orElse(parseFloatLiteral(input, pos))).orElse((if (input.startsWith("^", pos)) Some(("^", pos + 1)) else None).flatMap { case (_r2660, _p2661) => parseSpacing(input, _p2661).map { case (_r2662, _p2663) => (new ~(_r2660, _r2662), _p2663) } }.flatMap { case (_r2656, _p2657) => parsePostfixExpr(input, _p2657).map { case (_r2658, _p2659) => (new ~(_r2656, _r2658), _p2659) } })).orElse(parseParenExpr(input, pos))).orElse((if (input.startsWith("[", pos)) Some(("[", pos + 1)) else None).flatMap { case (_r2676, _p2677) => parseSpacing(input, _p2677).map { case (_r2678, _p2679) => (new ~(_r2676, _r2678), _p2679) } }.flatMap { case (_r2672, _p2673) => (parseInPatternListElem(input, _p2673).flatMap { case (_r2688, _p2689) => {
  var _rs2692: List[Any] = Nil; var _cp2693: Int = _p2689; var _go2696 = true
  while (_go2696) { parseSpacing(input, _cp2693).flatMap { case (_r2705, _p2706) => (if (input.startsWith(",", _p2706)) Some((",", _p2706 + 1)) else None).map { case (_r2707, _p2708) => (new ~(_r2705, _r2707), _p2708) } }.flatMap { case (_r2701, _p2702) => parseSpacing(input, _p2702).map { case (_r2703, _p2704) => (new ~(_r2701, _r2703), _p2704) } }.flatMap { case (_r2697, _p2698) => parseInPatternListElem(input, _p2698).map { case (_r2699, _p2700) => (new ~(_r2697, _r2699), _p2700) } } match {
    case Some((_st2694, _np2695)) => _rs2692 = _rs2692 :+ _st2694; _cp2693 = _np2695
    case None => _go2696 = false } }
  Some((_rs2692, _cp2693)) }.map { case (_r2690, _p2691) => (new ~(_r2688, _r2690), _p2691) } }.flatMap { case (_r2684, _p2685) => (parseSpacing(input, _p2685).flatMap { case (_r2709, _p2710) => (if (input.startsWith(",", _p2710)) Some((",", _p2710 + 1)) else None).map { case (_r2711, _p2712) => (new ~(_r2709, _r2711), _p2712) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2685)))).map { case (_r2686, _p2687) => (new ~(_r2684, _r2686), _p2687) } }.flatMap { case (_r2680, _p2681) => parseSpacing(input, _p2681).map { case (_r2682, _p2683) => (new ~(_r2680, _r2682), _p2683) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2673)))).map { case (_r2674, _p2675) => (new ~(_r2672, _r2674), _p2675) } }.flatMap { case (_r2668, _p2669) => (if (input.startsWith("]", _p2669)) Some(("]", _p2669 + 1)) else None).map { case (_r2670, _p2671) => (new ~(_r2668, _r2670), _p2671) } }.flatMap { case (_r2664, _p2665) => parseInlineSpacing(input, _p2665).map { case (_r2666, _p2667) => (new ~(_r2664, _r2666), _p2667) } })).orElse((if (input.startsWith("{", pos)) Some(("{", pos + 1)) else None).flatMap { case (_r2725, _p2726) => parseSpacing(input, _p2726).map { case (_r2727, _p2728) => (new ~(_r2725, _r2727), _p2728) } }.flatMap { case (_r2721, _p2722) => (parseInPatternHashEntry(input, _p2722).flatMap { case (_r2737, _p2738) => {
  var _rs2741: List[Any] = Nil; var _cp2742: Int = _p2738; var _go2745 = true
  while (_go2745) { parseSpacing(input, _cp2742).flatMap { case (_r2754, _p2755) => (if (input.startsWith(",", _p2755)) Some((",", _p2755 + 1)) else None).map { case (_r2756, _p2757) => (new ~(_r2754, _r2756), _p2757) } }.flatMap { case (_r2750, _p2751) => parseSpacing(input, _p2751).map { case (_r2752, _p2753) => (new ~(_r2750, _r2752), _p2753) } }.flatMap { case (_r2746, _p2747) => parseInPatternHashEntry(input, _p2747).map { case (_r2748, _p2749) => (new ~(_r2746, _r2748), _p2749) } } match {
    case Some((_st2743, _np2744)) => _rs2741 = _rs2741 :+ _st2743; _cp2742 = _np2744
    case None => _go2745 = false } }
  Some((_rs2741, _cp2742)) }.map { case (_r2739, _p2740) => (new ~(_r2737, _r2739), _p2740) } }.flatMap { case (_r2733, _p2734) => (parseSpacing(input, _p2734).flatMap { case (_r2758, _p2759) => (if (input.startsWith(",", _p2759)) Some((",", _p2759 + 1)) else None).map { case (_r2760, _p2761) => (new ~(_r2758, _r2760), _p2761) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2734)))).map { case (_r2735, _p2736) => (new ~(_r2733, _r2735), _p2736) } }.flatMap { case (_r2729, _p2730) => parseSpacing(input, _p2730).map { case (_r2731, _p2732) => (new ~(_r2729, _r2731), _p2732) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2722)))).map { case (_r2723, _p2724) => (new ~(_r2721, _r2723), _p2724) } }.flatMap { case (_r2717, _p2718) => (if (input.startsWith("}", _p2718)) Some(("}", _p2718 + 1)) else None).map { case (_r2719, _p2720) => (new ~(_r2717, _r2719), _p2720) } }.flatMap { case (_r2713, _p2714) => parseInlineSpacing(input, _p2714).map { case (_r2715, _p2716) => (new ~(_r2713, _r2715), _p2716) } })).orElse(parseConstRef(input, pos).flatMap { case (_r2778, _p2779) => (if (input.startsWith("(", _p2779)) Some(("(", _p2779 + 1)) else None).map { case (_r2780, _p2781) => (new ~(_r2778, _r2780), _p2781) } }.flatMap { case (_r2774, _p2775) => parseSpacing(input, _p2775).map { case (_r2776, _p2777) => (new ~(_r2774, _r2776), _p2777) } }.flatMap { case (_r2770, _p2771) => (parseInPatternListElem(input, _p2771).flatMap { case (_r2790, _p2791) => {
  var _rs2794: List[Any] = Nil; var _cp2795: Int = _p2791; var _go2798 = true
  while (_go2798) { parseSpacing(input, _cp2795).flatMap { case (_r2807, _p2808) => (if (input.startsWith(",", _p2808)) Some((",", _p2808 + 1)) else None).map { case (_r2809, _p2810) => (new ~(_r2807, _r2809), _p2810) } }.flatMap { case (_r2803, _p2804) => parseSpacing(input, _p2804).map { case (_r2805, _p2806) => (new ~(_r2803, _r2805), _p2806) } }.flatMap { case (_r2799, _p2800) => parseInPatternListElem(input, _p2800).map { case (_r2801, _p2802) => (new ~(_r2799, _r2801), _p2802) } } match {
    case Some((_st2796, _np2797)) => _rs2794 = _rs2794 :+ _st2796; _cp2795 = _np2797
    case None => _go2798 = false } }
  Some((_rs2794, _cp2795)) }.map { case (_r2792, _p2793) => (new ~(_r2790, _r2792), _p2793) } }.flatMap { case (_r2786, _p2787) => (parseSpacing(input, _p2787).flatMap { case (_r2811, _p2812) => (if (input.startsWith(",", _p2812)) Some((",", _p2812 + 1)) else None).map { case (_r2813, _p2814) => (new ~(_r2811, _r2813), _p2814) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2787)))).map { case (_r2788, _p2789) => (new ~(_r2786, _r2788), _p2789) } }.flatMap { case (_r2782, _p2783) => parseSpacing(input, _p2783).map { case (_r2784, _p2785) => (new ~(_r2782, _r2784), _p2785) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2771)))).map { case (_r2772, _p2773) => (new ~(_r2770, _r2772), _p2773) } }.flatMap { case (_r2766, _p2767) => (if (input.startsWith(")", _p2767)) Some((")", _p2767 + 1)) else None).map { case (_r2768, _p2769) => (new ~(_r2766, _r2768), _p2769) } }.flatMap { case (_r2762, _p2763) => parseInlineSpacing(input, _p2763).map { case (_r2764, _p2765) => (new ~(_r2762, _r2764), _p2765) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2819, _p2820) => parseSpacing(input, _p2820).map { case (_r2821, _p2822) => (new ~(_r2819, _r2821), _p2822) } }.flatMap { case (_r2815, _p2816) => (((if (input.startsWith("nil", _p2816)) Some(("nil", _p2816 + 3)) else None).flatMap { case (_r2827, _p2828) => (if (parseIdentCont(input, _p2828).isEmpty) Some(((), _p2828)) else None).map { case (_r2829, _p2830) => (new ~(_r2827, _r2829), _p2830) } }.flatMap { case (_r2823, _p2824) => parseInlineSpacing(input, _p2824).map { case (_r2825, _p2826) => (new ~(_r2823, _r2825), _p2826) } }).orElse(parseVariable(input, _p2816)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2816)))).map { case (_r2817, _p2818) => (new ~(_r2815, _r2817), _p2818) } }).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternHashEntry(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("**", pos)) Some(("**", pos + 2)) else None).flatMap { case (_r2835, _p2836) => parseSpacing(input, _p2836).map { case (_r2837, _p2838) => (new ~(_r2835, _r2837), _p2838) } }.flatMap { case (_r2831, _p2832) => (((if (input.startsWith("nil", _p2832)) Some(("nil", _p2832 + 3)) else None).flatMap { case (_r2843, _p2844) => (if (parseIdentCont(input, _p2844).isEmpty) Some(((), _p2844)) else None).map { case (_r2845, _p2846) => (new ~(_r2843, _r2845), _p2846) } }.flatMap { case (_r2839, _p2840) => parseInlineSpacing(input, _p2840).map { case (_r2841, _p2842) => (new ~(_r2839, _r2841), _p2842) } }).orElse(parseVariable(input, _p2832)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2832)))).map { case (_r2833, _p2834) => (new ~(_r2831, _r2833), _p2834) } }).orElse(parseQuotedLabelSymbol(input, pos).flatMap { case (_r2851, _p2852) => parseSpacing(input, _p2852).map { case (_r2853, _p2854) => (new ~(_r2851, _r2853), _p2854) } }.flatMap { case (_r2847, _p2848) => parseInPatternPrimary(input, _p2848).map { case (_r2849, _p2850) => (new ~(_r2847, _r2849), _p2850) } })).orElse(parseLabelSymbol(input, pos).flatMap { case (_r2859, _p2860) => parseSpacing(input, _p2860).map { case (_r2861, _p2862) => (new ~(_r2859, _r2861), _p2862) } }.flatMap { case (_r2855, _p2856) => parseInPatternPrimary(input, _p2856).map { case (_r2857, _p2858) => (new ~(_r2855, _r2857), _p2858) } })).orElse(parseLabelSymbol(input, pos))).orElse(parseExpr(input, pos).flatMap { case (_r2875, _p2876) => parseSpacing(input, _p2876).map { case (_r2877, _p2878) => (new ~(_r2875, _r2877), _p2878) } }.flatMap { case (_r2871, _p2872) => (if (input.startsWith("=>", _p2872)) Some(("=>", _p2872 + 2)) else None).map { case (_r2873, _p2874) => (new ~(_r2871, _r2873), _p2874) } }.flatMap { case (_r2867, _p2868) => parseSpacing(input, _p2868).map { case (_r2869, _p2870) => (new ~(_r2867, _r2869), _p2870) } }.flatMap { case (_r2863, _p2864) => parseInPatternPrimary(input, _p2864).map { case (_r2865, _p2866) => (new ~(_r2863, _r2865), _p2866) } }).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternListElem(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r2883, _p2884) => parseSpacing(input, _p2884).map { case (_r2885, _p2886) => (new ~(_r2883, _r2885), _p2886) } }.flatMap { case (_r2879, _p2880) => (((if (input.startsWith("nil", _p2880)) Some(("nil", _p2880 + 3)) else None).flatMap { case (_r2891, _p2892) => (if (parseIdentCont(input, _p2892).isEmpty) Some(((), _p2892)) else None).map { case (_r2893, _p2894) => (new ~(_r2891, _r2893), _p2894) } }.flatMap { case (_r2887, _p2888) => parseInlineSpacing(input, _p2888).map { case (_r2889, _p2890) => (new ~(_r2887, _r2889), _p2890) } }).orElse(parseVariable(input, _p2880)).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2880)))).map { case (_r2881, _p2882) => (new ~(_r2879, _r2881), _p2882) } }).orElse(parseInPatternOrExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternOrExpr(input: String, pos: Int): Option[(Any, Int)] = parseInPatternPrimary(input, pos).flatMap { case (_r2895, _p2896) => {
  var _rs2899: List[Any] = Nil; var _cp2900: Int = _p2896; var _go2903 = true
  while (_go2903) { parseSpacing(input, _cp2900).flatMap { case (_r2912, _p2913) => (if (input.startsWith("|", _p2913)) Some(("|", _p2913 + 1)) else None).map { case (_r2914, _p2915) => (new ~(_r2912, _r2914), _p2915) } }.flatMap { case (_r2908, _p2909) => parseSpacing(input, _p2909).map { case (_r2910, _p2911) => (new ~(_r2908, _r2910), _p2911) } }.flatMap { case (_r2904, _p2905) => parseInPatternPrimary(input, _p2905).map { case (_r2906, _p2907) => (new ~(_r2904, _r2906), _p2907) } } match {
    case Some((_st2901, _np2902)) => _rs2899 = _rs2899 :+ _st2901; _cp2900 = _np2902
    case None => _go2903 = false } }
  Some((_rs2899, _cp2900)) }.map { case (_r2897, _p2898) => (new ~(_r2895, _r2897), _p2898) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInPatternExpr(input: String, pos: Int): Option[(Any, Int)] = parseInPatternOrExpr(input, pos).flatMap { case (_r2920, _p2921) => (parseSpacing(input, _p2921).flatMap { case (_r2932, _p2933) => (if (input.startsWith("=>", _p2933)) Some(("=>", _p2933 + 2)) else None).map { case (_r2934, _p2935) => (new ~(_r2932, _r2934), _p2935) } }.flatMap { case (_r2928, _p2929) => parseSpacing(input, _p2929).map { case (_r2930, _p2931) => (new ~(_r2928, _r2930), _p2931) } }.flatMap { case (_r2924, _p2925) => parseVariable(input, _p2925).map { case (_r2926, _p2927) => (new ~(_r2924, _r2926), _p2927) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2921)))).map { case (_r2922, _p2923) => (new ~(_r2920, _r2922), _p2923) } }.flatMap { case (_r2916, _p2917) => (parseSpacing(input, _p2917).flatMap { case (_r2944, _p2945) => ((if (input.startsWith("if", _p2945)) Some(("if", _p2945 + 2)) else None).flatMap { case (_r2948, _p2949) => (if (parseIdentCont(input, _p2949).isEmpty) Some(((), _p2949)) else None).map { case (_r2950, _p2951) => (new ~(_r2948, _r2950), _p2951) } }).orElse((if (input.startsWith("unless", _p2945)) Some(("unless", _p2945 + 6)) else None).flatMap { case (_r2952, _p2953) => (if (parseIdentCont(input, _p2953).isEmpty) Some(((), _p2953)) else None).map { case (_r2954, _p2955) => (new ~(_r2952, _r2954), _p2955) } }).map { case (_r2946, _p2947) => (new ~(_r2944, _r2946), _p2947) } }.flatMap { case (_r2940, _p2941) => parseSpacing(input, _p2941).map { case (_r2942, _p2943) => (new ~(_r2940, _r2942), _p2943) } }.flatMap { case (_r2936, _p2937) => parseExpr(input, _p2937).map { case (_r2938, _p2939) => (new ~(_r2936, _r2938), _p2939) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2917)))).map { case (_r2918, _p2919) => (new ~(_r2916, _r2918), _p2919) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseInMatchExpr(input: String, pos: Int): Option[(Any, Int)] = parseOrExpr(input, pos).flatMap { case (_r2956, _p2957) => ((if (input.startsWith("in", _p2957)) Some(("in", _p2957 + 2)) else None).flatMap { case (_r2968, _p2969) => (if (parseIdentCont(input, _p2969).isEmpty) Some(((), _p2969)) else None).map { case (_r2970, _p2971) => (new ~(_r2968, _r2970), _p2971) } }.flatMap { case (_r2964, _p2965) => parseSpacing(input, _p2965).map { case (_r2966, _p2967) => (new ~(_r2964, _r2966), _p2967) } }.flatMap { case (_r2960, _p2961) => parseInPatternExpr(input, _p2961).map { case (_r2962, _p2963) => (new ~(_r2960, _r2962), _p2963) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2957)))).map { case (_r2958, _p2959) => (new ~(_r2956, _r2958), _p2959) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseConditionalExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(44, pos) {
    parseInMatchExpr(input, pos).flatMap { case (_r2972, _p2973) => (parseSpacing(input, _p2973).flatMap { case (_r3004, _p3005) => (if (input.startsWith("?", _p3005)) Some(("?", _p3005 + 1)) else None).map { case (_r3006, _p3007) => (new ~(_r3004, _r3006), _p3007) } }.flatMap { case (_r3000, _p3001) => parseSpacing(input, _p3001).map { case (_r3002, _p3003) => (new ~(_r3000, _r3002), _p3003) } }.flatMap { case (_r2996, _p2997) => parseConditionalExpr(input, _p2997).map { case (_r2998, _p2999) => (new ~(_r2996, _r2998), _p2999) } }.flatMap { case (_r2992, _p2993) => parseSpacing(input, _p2993).map { case (_r2994, _p2995) => (new ~(_r2992, _r2994), _p2995) } }.flatMap { case (_r2988, _p2989) => (if (input.startsWith(":", _p2989)) Some((":", _p2989 + 1)) else None).map { case (_r2990, _p2991) => (new ~(_r2988, _r2990), _p2991) } }.flatMap { case (_r2984, _p2985) => (if ((if (input.startsWith(":", _p2985)) Some((":", _p2985 + 1)) else None).isEmpty) Some(((), _p2985)) else None).map { case (_r2986, _p2987) => (new ~(_r2984, _r2986), _p2987) } }.flatMap { case (_r2980, _p2981) => parseSpacing(input, _p2981).map { case (_r2982, _p2983) => (new ~(_r2980, _r2982), _p2983) } }.flatMap { case (_r2976, _p2977) => parseConditionalExpr(input, _p2977).map { case (_r2978, _p2979) => (new ~(_r2976, _r2978), _p2979) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p2973)))).map { case (_r2974, _p2975) => (new ~(_r2972, _r2974), _p2975) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseCompoundAssignOp(input: String, pos: Int): Option[(Any, Int)] = (((((((((((if (input.startsWith("<<=", pos)) Some(("<<=", pos + 3)) else None)).orElse((if (input.startsWith(">>=", pos)) Some((">>=", pos + 3)) else None))).orElse((if (input.startsWith("+=", pos)) Some(("+=", pos + 2)) else None))).orElse((if (input.startsWith("-=", pos)) Some(("-=", pos + 2)) else None))).orElse((if (input.startsWith("*=", pos)) Some(("*=", pos + 2)) else None))).orElse((if (input.startsWith("/=", pos)) Some(("/=", pos + 2)) else None))).orElse((if (input.startsWith("%=", pos)) Some(("%=", pos + 2)) else None))).orElse((if (input.startsWith("&=", pos)) Some(("&=", pos + 2)) else None))).orElse((if (input.startsWith("|=", pos)) Some(("|=", pos + 2)) else None))).orElse((if (input.startsWith("^=", pos)) Some(("^=", pos + 2)) else None))).orElse((if (input.startsWith("**=", pos)) Some(("**=", pos + 3)) else None)).map { case (r, p) => (_applyAction({  s => s  }, r), p) }

  def parseAssignableTarget(input: String, pos: Int): Option[(Any, Int)] = ((((parseInstanceVarExpr(input, pos)).orElse(parseClassVarExpr(input, pos))).orElse(parseGlobalVarExpr(input, pos))).orElse(parseConstRef(input, pos))).orElse(parseLocalVarExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseAssignValueExpr(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r3016, _p3017) => parseSpacing(input, _p3017).map { case (_r3018, _p3019) => (new ~(_r3016, _r3018), _p3019) } }.flatMap { case (_r3012, _p3013) => parseConditionalExpr(input, _p3013).map { case (_r3014, _p3015) => (new ~(_r3012, _r3014), _p3015) } }).orElse(parseConditionalExpr(input, pos)).flatMap { case (_r3008, _p3009) => {
  var _rs3020: List[Any] = Nil; var _cp3021: Int = _p3009; var _go3024 = true
  while (_go3024) { parseSpacing(input, _cp3021).flatMap { case (_r3033, _p3034) => (if (input.startsWith(",", _p3034)) Some((",", _p3034 + 1)) else None).map { case (_r3035, _p3036) => (new ~(_r3033, _r3035), _p3036) } }.flatMap { case (_r3029, _p3030) => parseSpacing(input, _p3030).map { case (_r3031, _p3032) => (new ~(_r3029, _r3031), _p3032) } }.flatMap { case (_r3025, _p3026) => ((if (input.startsWith("*", _p3026)) Some(("*", _p3026 + 1)) else None).flatMap { case (_r3041, _p3042) => parseSpacing(input, _p3042).map { case (_r3043, _p3044) => (new ~(_r3041, _r3043), _p3044) } }.flatMap { case (_r3037, _p3038) => parseConditionalExpr(input, _p3038).map { case (_r3039, _p3040) => (new ~(_r3037, _r3039), _p3040) } }).orElse(parseConditionalExpr(input, _p3026)).map { case (_r3027, _p3028) => (new ~(_r3025, _r3027), _p3028) } } match {
    case Some((_st3022, _np3023)) => _rs3020 = _rs3020 :+ _st3022; _cp3021 = _np3023
    case None => _go3024 = false } }
  Some((_rs3020, _cp3021)) }.map { case (_r3010, _p3011) => (new ~(_r3008, _r3010), _p3011) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseChainedAssignRhs(input: String, pos: Int): Option[(Any, Int)] = _withMemo(45, pos) {
    (((((((((parsePostfixExpr(input, pos).flatMap { case (_r3061, _p3062) => parseDotSep(input, _p3062).map { case (_r3063, _p3064) => (new ~(_r3061, _r3063), _p3064) } }.flatMap { case (_r3057, _p3058) => parseMethodName(input, _p3058).map { case (_r3059, _p3060) => (new ~(_r3057, _r3059), _p3060) } }.flatMap { case (_r3053, _p3054) => parseAssignEq(input, _p3054).map { case (_r3055, _p3056) => (new ~(_r3053, _r3055), _p3056) } }.flatMap { case (_r3049, _p3050) => parseSpacing(input, _p3050).map { case (_r3051, _p3052) => (new ~(_r3049, _r3051), _p3052) } }.flatMap { case (_r3045, _p3046) => parseChainedAssignRhs(input, _p3046).map { case (_r3047, _p3048) => (new ~(_r3045, _r3047), _p3048) } }).orElse(parsePostfixExpr(input, pos).flatMap { case (_r3081, _p3082) => parseDotSep(input, _p3082).map { case (_r3083, _p3084) => (new ~(_r3081, _r3083), _p3084) } }.flatMap { case (_r3077, _p3078) => parseMethodName(input, _p3078).map { case (_r3079, _p3080) => (new ~(_r3077, _r3079), _p3080) } }.flatMap { case (_r3073, _p3074) => parseCompoundAssignOp(input, _p3074).map { case (_r3075, _p3076) => (new ~(_r3073, _r3075), _p3076) } }.flatMap { case (_r3069, _p3070) => parseSpacing(input, _p3070).map { case (_r3071, _p3072) => (new ~(_r3069, _r3071), _p3072) } }.flatMap { case (_r3065, _p3066) => parseChainedAssignRhs(input, _p3066).map { case (_r3067, _p3068) => (new ~(_r3065, _r3067), _p3068) } })).orElse(parsePostfixExpr(input, pos).flatMap { case (_r3101, _p3102) => parseDotSep(input, _p3102).map { case (_r3103, _p3104) => (new ~(_r3101, _r3103), _p3104) } }.flatMap { case (_r3097, _p3098) => parseMethodName(input, _p3098).map { case (_r3099, _p3100) => (new ~(_r3097, _r3099), _p3100) } }.flatMap { case (_r3093, _p3094) => ((if (input.startsWith("||=", _p3094)) Some(("||=", _p3094 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3094)) Some(("&&=", _p3094 + 3)) else None)).map { case (_r3095, _p3096) => (new ~(_r3093, _r3095), _p3096) } }.flatMap { case (_r3089, _p3090) => parseSpacing(input, _p3090).map { case (_r3091, _p3092) => (new ~(_r3089, _r3091), _p3092) } }.flatMap { case (_r3085, _p3086) => parseChainedAssignRhs(input, _p3086).map { case (_r3087, _p3088) => (new ~(_r3085, _r3087), _p3088) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3117, _p3118) => parseSubscriptArgs(input, _p3118).map { case (_r3119, _p3120) => (new ~(_r3117, _r3119), _p3120) } }.flatMap { case (_r3113, _p3114) => parseAssignEq(input, _p3114).map { case (_r3115, _p3116) => (new ~(_r3113, _r3115), _p3116) } }.flatMap { case (_r3109, _p3110) => parseSpacing(input, _p3110).map { case (_r3111, _p3112) => (new ~(_r3109, _r3111), _p3112) } }.flatMap { case (_r3105, _p3106) => parseChainedAssignRhs(input, _p3106).map { case (_r3107, _p3108) => (new ~(_r3105, _r3107), _p3108) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3133, _p3134) => parseSubscriptArgs(input, _p3134).map { case (_r3135, _p3136) => (new ~(_r3133, _r3135), _p3136) } }.flatMap { case (_r3129, _p3130) => parseCompoundAssignOp(input, _p3130).map { case (_r3131, _p3132) => (new ~(_r3129, _r3131), _p3132) } }.flatMap { case (_r3125, _p3126) => parseSpacing(input, _p3126).map { case (_r3127, _p3128) => (new ~(_r3125, _r3127), _p3128) } }.flatMap { case (_r3121, _p3122) => parseChainedAssignRhs(input, _p3122).map { case (_r3123, _p3124) => (new ~(_r3121, _r3123), _p3124) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3149, _p3150) => parseSubscriptArgs(input, _p3150).map { case (_r3151, _p3152) => (new ~(_r3149, _r3151), _p3152) } }.flatMap { case (_r3145, _p3146) => ((if (input.startsWith("||=", _p3146)) Some(("||=", _p3146 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3146)) Some(("&&=", _p3146 + 3)) else None)).map { case (_r3147, _p3148) => (new ~(_r3145, _r3147), _p3148) } }.flatMap { case (_r3141, _p3142) => parseSpacing(input, _p3142).map { case (_r3143, _p3144) => (new ~(_r3141, _r3143), _p3144) } }.flatMap { case (_r3137, _p3138) => parseChainedAssignRhs(input, _p3138).map { case (_r3139, _p3140) => (new ~(_r3137, _r3139), _p3140) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3161, _p3162) => parseCompoundAssignOp(input, _p3162).map { case (_r3163, _p3164) => (new ~(_r3161, _r3163), _p3164) } }.flatMap { case (_r3157, _p3158) => parseSpacing(input, _p3158).map { case (_r3159, _p3160) => (new ~(_r3157, _r3159), _p3160) } }.flatMap { case (_r3153, _p3154) => parseChainedAssignRhs(input, _p3154).map { case (_r3155, _p3156) => (new ~(_r3153, _r3155), _p3156) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3173, _p3174) => ((if (input.startsWith("||=", _p3174)) Some(("||=", _p3174 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3174)) Some(("&&=", _p3174 + 3)) else None)).map { case (_r3175, _p3176) => (new ~(_r3173, _r3175), _p3176) } }.flatMap { case (_r3169, _p3170) => parseSpacing(input, _p3170).map { case (_r3171, _p3172) => (new ~(_r3169, _r3171), _p3172) } }.flatMap { case (_r3165, _p3166) => parseChainedAssignRhs(input, _p3166).map { case (_r3167, _p3168) => (new ~(_r3165, _r3167), _p3168) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3185, _p3186) => parseAssignEq(input, _p3186).map { case (_r3187, _p3188) => (new ~(_r3185, _r3187), _p3188) } }.flatMap { case (_r3181, _p3182) => parseSpacing(input, _p3182).map { case (_r3183, _p3184) => (new ~(_r3181, _r3183), _p3184) } }.flatMap { case (_r3177, _p3178) => parseChainedAssignRhs(input, _p3178).map { case (_r3179, _p3180) => (new ~(_r3177, _r3179), _p3180) } })).orElse(parseConditionalExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseMultiAssignElem(input: String, pos: Int): Option[(Any, Int)] = (((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r3193, _p3194) => parseSpacing(input, _p3194).map { case (_r3195, _p3196) => (new ~(_r3193, _r3195), _p3196) } }.flatMap { case (_r3189, _p3190) => (parseAssignableTarget(input, _p3190).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3190)))).map { case (_r3191, _p3192) => (new ~(_r3189, _r3191), _p3192) } }).orElse((if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r3217, _p3218) => parseSpacing(input, _p3218).map { case (_r3219, _p3220) => (new ~(_r3217, _r3219), _p3220) } }.flatMap { case (_r3213, _p3214) => parseMultiAssignElem(input, _p3214).map { case (_r3215, _p3216) => (new ~(_r3213, _r3215), _p3216) } }.flatMap { case (_r3209, _p3210) => {
  var _rs3221: List[Any] = Nil; var _cp3222: Int = _p3210; var _go3225 = true
  while (_go3225) { parseSpacing(input, _cp3222).flatMap { case (_r3234, _p3235) => (if (input.startsWith(",", _p3235)) Some((",", _p3235 + 1)) else None).map { case (_r3236, _p3237) => (new ~(_r3234, _r3236), _p3237) } }.flatMap { case (_r3230, _p3231) => parseSpacing(input, _p3231).map { case (_r3232, _p3233) => (new ~(_r3230, _r3232), _p3233) } }.flatMap { case (_r3226, _p3227) => parseMultiAssignElem(input, _p3227).map { case (_r3228, _p3229) => (new ~(_r3226, _r3228), _p3229) } } match {
    case Some((_st3223, _np3224)) => _rs3221 = _rs3221 :+ _st3223; _cp3222 = _np3224
    case None => _go3225 = false } }
  Some((_rs3221, _cp3222)) }.map { case (_r3211, _p3212) => (new ~(_r3209, _r3211), _p3212) } }.flatMap { case (_r3205, _p3206) => (parseSpacing(input, _p3206).flatMap { case (_r3238, _p3239) => (if (input.startsWith(",", _p3239)) Some((",", _p3239 + 1)) else None).map { case (_r3240, _p3241) => (new ~(_r3238, _r3240), _p3241) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3206)))).map { case (_r3207, _p3208) => (new ~(_r3205, _r3207), _p3208) } }.flatMap { case (_r3201, _p3202) => parseSpacing(input, _p3202).map { case (_r3203, _p3204) => (new ~(_r3201, _r3203), _p3204) } }.flatMap { case (_r3197, _p3198) => (if (input.startsWith(")", _p3198)) Some((")", _p3198 + 1)) else None).map { case (_r3199, _p3200) => (new ~(_r3197, _r3199), _p3200) } })).orElse(parseAssignableTarget(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseMultiAssignTargets(input: String, pos: Int): Option[(Any, Int)] = ((parseMultiAssignElem(input, pos).flatMap { case (_r3246, _p3247) => {
  parseSpacing(input, _p3247).flatMap { case (_r3265, _p3266) => (if (input.startsWith(",", _p3266)) Some((",", _p3266 + 1)) else None).map { case (_r3267, _p3268) => (new ~(_r3265, _r3267), _p3268) } }.flatMap { case (_r3261, _p3262) => parseSpacing(input, _p3262).map { case (_r3263, _p3264) => (new ~(_r3261, _r3263), _p3264) } }.flatMap { case (_r3257, _p3258) => parseMultiAssignElem(input, _p3258).map { case (_r3259, _p3260) => (new ~(_r3257, _r3259), _p3260) } } match {
    case None => None
    case Some((_fs3250, _fp3251)) =>
      var _rs3252: List[Any] = List(_fs3250); var _cp3253: Int = _fp3251; var _go3256 = true
      while (_go3256) { parseSpacing(input, _cp3253).flatMap { case (_r3277, _p3278) => (if (input.startsWith(",", _p3278)) Some((",", _p3278 + 1)) else None).map { case (_r3279, _p3280) => (new ~(_r3277, _r3279), _p3280) } }.flatMap { case (_r3273, _p3274) => parseSpacing(input, _p3274).map { case (_r3275, _p3276) => (new ~(_r3273, _r3275), _p3276) } }.flatMap { case (_r3269, _p3270) => parseMultiAssignElem(input, _p3270).map { case (_r3271, _p3272) => (new ~(_r3269, _r3271), _p3272) } } match {
        case Some((_st3254, _np3255)) => _rs3252 = _rs3252 :+ _st3254; _cp3253 = _np3255
        case None => _go3256 = false } }
      Some((_rs3252, _cp3253)) } }.map { case (_r3248, _p3249) => (new ~(_r3246, _r3248), _p3249) } }.flatMap { case (_r3242, _p3243) => (parseSpacing(input, _p3243).flatMap { case (_r3281, _p3282) => (if (input.startsWith(",", _p3282)) Some((",", _p3282 + 1)) else None).map { case (_r3283, _p3284) => (new ~(_r3281, _r3283), _p3284) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3243)))).map { case (_r3244, _p3245) => (new ~(_r3242, _r3244), _p3245) } }).orElse(parseMultiAssignElem(input, pos).flatMap { case (_r3289, _p3290) => parseSpacing(input, _p3290).map { case (_r3291, _p3292) => (new ~(_r3289, _r3291), _p3292) } }.flatMap { case (_r3285, _p3286) => (if (input.startsWith(",", _p3286)) Some((",", _p3286 + 1)) else None).map { case (_r3287, _p3288) => (new ~(_r3285, _r3287), _p3288) } })).orElse((if (input.startsWith("*", pos)) Some(("*", pos + 1)) else None).flatMap { case (_r3297, _p3298) => parseSpacing(input, _p3298).map { case (_r3299, _p3300) => (new ~(_r3297, _r3299), _p3300) } }.flatMap { case (_r3293, _p3294) => (parseAssignableTarget(input, _p3294).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3294)))).map { case (_r3295, _p3296) => (new ~(_r3293, _r3295), _p3296) } }).map { case (r, p) => (_applyAction({  _ => List.empty[String]  }, r), p) }

  def parseAssignmentExpr(input: String, pos: Int): Option[(Any, Int)] = ((((((((((parsePostfixExpr(input, pos).flatMap { case (_r3317, _p3318) => parseDotSep(input, _p3318).map { case (_r3319, _p3320) => (new ~(_r3317, _r3319), _p3320) } }.flatMap { case (_r3313, _p3314) => parseMethodName(input, _p3314).map { case (_r3315, _p3316) => (new ~(_r3313, _r3315), _p3316) } }.flatMap { case (_r3309, _p3310) => parseAssignEq(input, _p3310).map { case (_r3311, _p3312) => (new ~(_r3309, _r3311), _p3312) } }.flatMap { case (_r3305, _p3306) => parseSpacing(input, _p3306).map { case (_r3307, _p3308) => (new ~(_r3305, _r3307), _p3308) } }.flatMap { case (_r3301, _p3302) => parseAssignValueExpr(input, _p3302).map { case (_r3303, _p3304) => (new ~(_r3301, _r3303), _p3304) } }).orElse(parsePostfixExpr(input, pos).flatMap { case (_r3337, _p3338) => parseDotSep(input, _p3338).map { case (_r3339, _p3340) => (new ~(_r3337, _r3339), _p3340) } }.flatMap { case (_r3333, _p3334) => parseMethodName(input, _p3334).map { case (_r3335, _p3336) => (new ~(_r3333, _r3335), _p3336) } }.flatMap { case (_r3329, _p3330) => parseCompoundAssignOp(input, _p3330).map { case (_r3331, _p3332) => (new ~(_r3329, _r3331), _p3332) } }.flatMap { case (_r3325, _p3326) => parseSpacing(input, _p3326).map { case (_r3327, _p3328) => (new ~(_r3325, _r3327), _p3328) } }.flatMap { case (_r3321, _p3322) => parseChainedAssignRhs(input, _p3322).map { case (_r3323, _p3324) => (new ~(_r3321, _r3323), _p3324) } })).orElse(parsePostfixExpr(input, pos).flatMap { case (_r3357, _p3358) => parseDotSep(input, _p3358).map { case (_r3359, _p3360) => (new ~(_r3357, _r3359), _p3360) } }.flatMap { case (_r3353, _p3354) => parseMethodName(input, _p3354).map { case (_r3355, _p3356) => (new ~(_r3353, _r3355), _p3356) } }.flatMap { case (_r3349, _p3350) => ((if (input.startsWith("||=", _p3350)) Some(("||=", _p3350 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3350)) Some(("&&=", _p3350 + 3)) else None)).map { case (_r3351, _p3352) => (new ~(_r3349, _r3351), _p3352) } }.flatMap { case (_r3345, _p3346) => parseSpacing(input, _p3346).map { case (_r3347, _p3348) => (new ~(_r3345, _r3347), _p3348) } }.flatMap { case (_r3341, _p3342) => parseChainedAssignRhs(input, _p3342).map { case (_r3343, _p3344) => (new ~(_r3341, _r3343), _p3344) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3373, _p3374) => parseSubscriptArgs(input, _p3374).map { case (_r3375, _p3376) => (new ~(_r3373, _r3375), _p3376) } }.flatMap { case (_r3369, _p3370) => parseAssignEq(input, _p3370).map { case (_r3371, _p3372) => (new ~(_r3369, _r3371), _p3372) } }.flatMap { case (_r3365, _p3366) => parseSpacing(input, _p3366).map { case (_r3367, _p3368) => (new ~(_r3365, _r3367), _p3368) } }.flatMap { case (_r3361, _p3362) => parseAssignValueExpr(input, _p3362).map { case (_r3363, _p3364) => (new ~(_r3361, _r3363), _p3364) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3389, _p3390) => parseSubscriptArgs(input, _p3390).map { case (_r3391, _p3392) => (new ~(_r3389, _r3391), _p3392) } }.flatMap { case (_r3385, _p3386) => parseCompoundAssignOp(input, _p3386).map { case (_r3387, _p3388) => (new ~(_r3385, _r3387), _p3388) } }.flatMap { case (_r3381, _p3382) => parseSpacing(input, _p3382).map { case (_r3383, _p3384) => (new ~(_r3381, _r3383), _p3384) } }.flatMap { case (_r3377, _p3378) => parseChainedAssignRhs(input, _p3378).map { case (_r3379, _p3380) => (new ~(_r3377, _r3379), _p3380) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3405, _p3406) => parseSubscriptArgs(input, _p3406).map { case (_r3407, _p3408) => (new ~(_r3405, _r3407), _p3408) } }.flatMap { case (_r3401, _p3402) => ((if (input.startsWith("||=", _p3402)) Some(("||=", _p3402 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3402)) Some(("&&=", _p3402 + 3)) else None)).map { case (_r3403, _p3404) => (new ~(_r3401, _r3403), _p3404) } }.flatMap { case (_r3397, _p3398) => parseSpacing(input, _p3398).map { case (_r3399, _p3400) => (new ~(_r3397, _r3399), _p3400) } }.flatMap { case (_r3393, _p3394) => parseChainedAssignRhs(input, _p3394).map { case (_r3395, _p3396) => (new ~(_r3393, _r3395), _p3396) } })).orElse(parseMultiAssignTargets(input, pos).flatMap { case (_r3417, _p3418) => parseAssignEq(input, _p3418).map { case (_r3419, _p3420) => (new ~(_r3417, _r3419), _p3420) } }.flatMap { case (_r3413, _p3414) => parseSpacing(input, _p3414).map { case (_r3415, _p3416) => (new ~(_r3413, _r3415), _p3416) } }.flatMap { case (_r3409, _p3410) => parseAssignValueExpr(input, _p3410).map { case (_r3411, _p3412) => (new ~(_r3409, _r3411), _p3412) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3429, _p3430) => parseCompoundAssignOp(input, _p3430).map { case (_r3431, _p3432) => (new ~(_r3429, _r3431), _p3432) } }.flatMap { case (_r3425, _p3426) => parseSpacing(input, _p3426).map { case (_r3427, _p3428) => (new ~(_r3425, _r3427), _p3428) } }.flatMap { case (_r3421, _p3422) => parseChainedAssignRhs(input, _p3422).map { case (_r3423, _p3424) => (new ~(_r3421, _r3423), _p3424) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3441, _p3442) => ((if (input.startsWith("||=", _p3442)) Some(("||=", _p3442 + 3)) else None)).orElse((if (input.startsWith("&&=", _p3442)) Some(("&&=", _p3442 + 3)) else None)).map { case (_r3443, _p3444) => (new ~(_r3441, _r3443), _p3444) } }.flatMap { case (_r3437, _p3438) => parseSpacing(input, _p3438).map { case (_r3439, _p3440) => (new ~(_r3437, _r3439), _p3440) } }.flatMap { case (_r3433, _p3434) => parseChainedAssignRhs(input, _p3434).map { case (_r3435, _p3436) => (new ~(_r3433, _r3435), _p3436) } })).orElse(parseAssignableTarget(input, pos).flatMap { case (_r3453, _p3454) => parseAssignEq(input, _p3454).map { case (_r3455, _p3456) => (new ~(_r3453, _r3455), _p3456) } }.flatMap { case (_r3449, _p3450) => parseSpacing(input, _p3450).map { case (_r3451, _p3452) => (new ~(_r3449, _r3451), _p3452) } }.flatMap { case (_r3445, _p3446) => parseAssignValueExpr(input, _p3446).map { case (_r3447, _p3448) => (new ~(_r3445, _r3447), _p3448) } })).orElse(parseConditionalExpr(input, pos)).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseExpr(input: String, pos: Int): Option[(Any, Int)] = _withMemo(46, pos) {
    parseAssignmentExpr(input, pos).flatMap { case (_r3457, _p3458) => {
  var _rs3461: List[Any] = Nil; var _cp3462: Int = _p3458; var _go3465 = true
  while (_go3465) { ((if (input.startsWith("and", _cp3462)) Some(("and", _cp3462 + 3)) else None).flatMap { case (_r3474, _p3475) => (if (parseIdentCont(input, _p3475).isEmpty) Some(((), _p3475)) else None).map { case (_r3476, _p3477) => (new ~(_r3474, _r3476), _p3477) } }).orElse((if (input.startsWith("or", _cp3462)) Some(("or", _cp3462 + 2)) else None).flatMap { case (_r3478, _p3479) => (if (parseIdentCont(input, _p3479).isEmpty) Some(((), _p3479)) else None).map { case (_r3480, _p3481) => (new ~(_r3478, _r3480), _p3481) } }).flatMap { case (_r3470, _p3471) => parseSpacing(input, _p3471).map { case (_r3472, _p3473) => (new ~(_r3470, _r3472), _p3473) } }.flatMap { case (_r3466, _p3467) => parseAssignmentExpr(input, _p3467).map { case (_r3468, _p3469) => (new ~(_r3466, _r3468), _p3469) } } match {
    case Some((_st3463, _np3464)) => _rs3461 = _rs3461 :+ _st3463; _cp3462 = _np3464
    case None => _go3465 = false } }
  Some((_rs3461, _cp3462)) }.map { case (_r3459, _p3460) => (new ~(_r3457, _r3459), _p3460) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }
  }

  def parseBeginExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r3518, _p3519) => (if (parseIdentCont(input, _p3519).isEmpty) Some(((), _p3519)) else None).map { case (_r3520, _p3521) => (new ~(_r3518, _r3520), _p3521) } }.flatMap { case (_r3514, _p3515) => parseSpacing(input, _p3515).map { case (_r3516, _p3517) => (new ~(_r3514, _r3516), _p3517) } }.flatMap { case (_r3510, _p3511) => {
  var _rs3522: List[Any] = Nil; var _cp3523: Int = _p3511; var _go3526 = true
  while (_go3526) { parseStatementSep(input, _cp3523) match {
    case Some((_st3524, _np3525)) => _rs3522 = _rs3522 :+ _st3524; _cp3523 = _np3525
    case None => _go3526 = false } }
  Some((_rs3522, _cp3523)) }.map { case (_r3512, _p3513) => (new ~(_r3510, _r3512), _p3513) } }.flatMap { case (_r3506, _p3507) => {
  var _rs3527: List[Any] = Nil; var _cp3528: Int = _p3507; var _go3531 = true
  while (_go3531) { (if ((parseRescueStop(input, _cp3528)).orElse(parseEndKeyword(input, _cp3528)).isEmpty) Some(((), _cp3528)) else None).flatMap { case (_r3536, _p3537) => parseStatement(input, _p3537).map { case (_r3538, _p3539) => (new ~(_r3536, _r3538), _p3539) } }.flatMap { case (_r3532, _p3533) => {
  var _rs3540: List[Any] = Nil; var _cp3541: Int = _p3533; var _go3544 = true
  while (_go3544) { parseStatementSep(input, _cp3541) match {
    case Some((_st3542, _np3543)) => _rs3540 = _rs3540 :+ _st3542; _cp3541 = _np3543
    case None => _go3544 = false } }
  Some((_rs3540, _cp3541)) }.map { case (_r3534, _p3535) => (new ~(_r3532, _r3534), _p3535) } } match {
    case Some((_st3529, _np3530)) => _rs3527 = _rs3527 :+ _st3529; _cp3528 = _np3530
    case None => _go3531 = false } }
  Some((_rs3527, _cp3528)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r3508, _p3509) => (new ~(_r3506, _r3508), _p3509) } }.flatMap { case (_r3502, _p3503) => {
  var _rs3545: List[Any] = Nil; var _cp3546: Int = _p3503; var _go3549 = true
  while (_go3549) { parseStatementSep(input, _cp3546) match {
    case Some((_st3547, _np3548)) => _rs3545 = _rs3545 :+ _st3547; _cp3546 = _np3548
    case None => _go3549 = false } }
  Some((_rs3545, _cp3546)) }.map { case (_r3504, _p3505) => (new ~(_r3502, _r3504), _p3505) } }.flatMap { case (_r3498, _p3499) => {
  var _rs3550: List[Any] = Nil; var _cp3551: Int = _p3499; var _go3554 = true
  while (_go3554) { parseRescueClause(input, _cp3551) match {
    case Some((_st3552, _np3553)) => _rs3550 = _rs3550 :+ _st3552; _cp3551 = _np3553
    case None => _go3554 = false } }
  Some((_rs3550, _cp3551)) }.map { case (_r3500, _p3501) => (new ~(_r3498, _r3500), _p3501) } }.flatMap { case (_r3494, _p3495) => {
  var _rs3555: List[Any] = Nil; var _cp3556: Int = _p3495; var _go3559 = true
  while (_go3559) { parseStatementSep(input, _cp3556) match {
    case Some((_st3557, _np3558)) => _rs3555 = _rs3555 :+ _st3557; _cp3556 = _np3558
    case None => _go3559 = false } }
  Some((_rs3555, _cp3556)) }.map { case (_r3496, _p3497) => (new ~(_r3494, _r3496), _p3497) } }.flatMap { case (_r3490, _p3491) => ((if (input.startsWith("else", _p3491)) Some(("else", _p3491 + 4)) else None).flatMap { case (_r3576, _p3577) => (if (parseIdentCont(input, _p3577).isEmpty) Some(((), _p3577)) else None).map { case (_r3578, _p3579) => (new ~(_r3576, _r3578), _p3579) } }.flatMap { case (_r3572, _p3573) => parseSpacing(input, _p3573).map { case (_r3574, _p3575) => (new ~(_r3572, _r3574), _p3575) } }.flatMap { case (_r3568, _p3569) => {
  var _rs3580: List[Any] = Nil; var _cp3581: Int = _p3569; var _go3584 = true
  while (_go3584) { parseStatementSep(input, _cp3581) match {
    case Some((_st3582, _np3583)) => _rs3580 = _rs3580 :+ _st3582; _cp3581 = _np3583
    case None => _go3584 = false } }
  Some((_rs3580, _cp3581)) }.map { case (_r3570, _p3571) => (new ~(_r3568, _r3570), _p3571) } }.flatMap { case (_r3564, _p3565) => {
  var _rs3585: List[Any] = Nil; var _cp3586: Int = _p3565; var _go3589 = true
  while (_go3589) { (if (parseDoBlockStop(input, _cp3586).isEmpty) Some(((), _cp3586)) else None).flatMap { case (_r3594, _p3595) => parseStatement(input, _p3595).map { case (_r3596, _p3597) => (new ~(_r3594, _r3596), _p3597) } }.flatMap { case (_r3590, _p3591) => {
  var _rs3598: List[Any] = Nil; var _cp3599: Int = _p3591; var _go3602 = true
  while (_go3602) { parseStatementSep(input, _cp3599) match {
    case Some((_st3600, _np3601)) => _rs3598 = _rs3598 :+ _st3600; _cp3599 = _np3601
    case None => _go3602 = false } }
  Some((_rs3598, _cp3599)) }.map { case (_r3592, _p3593) => (new ~(_r3590, _r3592), _p3593) } } match {
    case Some((_st3587, _np3588)) => _rs3585 = _rs3585 :+ _st3587; _cp3586 = _np3588
    case None => _go3589 = false } }
  Some((_rs3585, _cp3586)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r3566, _p3567) => (new ~(_r3564, _r3566), _p3567) } }.flatMap { case (_r3560, _p3561) => {
  var _rs3603: List[Any] = Nil; var _cp3604: Int = _p3561; var _go3607 = true
  while (_go3607) { parseStatementSep(input, _cp3604) match {
    case Some((_st3605, _np3606)) => _rs3603 = _rs3603 :+ _st3605; _cp3604 = _np3606
    case None => _go3607 = false } }
  Some((_rs3603, _cp3604)) }.map { case (_r3562, _p3563) => (new ~(_r3560, _r3562), _p3563) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3491)))).map { case (_r3492, _p3493) => (new ~(_r3490, _r3492), _p3493) } }.flatMap { case (_r3486, _p3487) => ((if (input.startsWith("ensure", _p3487)) Some(("ensure", _p3487 + 6)) else None).flatMap { case (_r3624, _p3625) => (if (parseIdentCont(input, _p3625).isEmpty) Some(((), _p3625)) else None).map { case (_r3626, _p3627) => (new ~(_r3624, _r3626), _p3627) } }.flatMap { case (_r3620, _p3621) => parseSpacing(input, _p3621).map { case (_r3622, _p3623) => (new ~(_r3620, _r3622), _p3623) } }.flatMap { case (_r3616, _p3617) => {
  var _rs3628: List[Any] = Nil; var _cp3629: Int = _p3617; var _go3632 = true
  while (_go3632) { parseStatementSep(input, _cp3629) match {
    case Some((_st3630, _np3631)) => _rs3628 = _rs3628 :+ _st3630; _cp3629 = _np3631
    case None => _go3632 = false } }
  Some((_rs3628, _cp3629)) }.map { case (_r3618, _p3619) => (new ~(_r3616, _r3618), _p3619) } }.flatMap { case (_r3612, _p3613) => {
  var _rs3633: List[Any] = Nil; var _cp3634: Int = _p3613; var _go3637 = true
  while (_go3637) { (if (parseEndKeyword(input, _cp3634).isEmpty) Some(((), _cp3634)) else None).flatMap { case (_r3642, _p3643) => parseStatement(input, _p3643).map { case (_r3644, _p3645) => (new ~(_r3642, _r3644), _p3645) } }.flatMap { case (_r3638, _p3639) => {
  var _rs3646: List[Any] = Nil; var _cp3647: Int = _p3639; var _go3650 = true
  while (_go3650) { parseStatementSep(input, _cp3647) match {
    case Some((_st3648, _np3649)) => _rs3646 = _rs3646 :+ _st3648; _cp3647 = _np3649
    case None => _go3650 = false } }
  Some((_rs3646, _cp3647)) }.map { case (_r3640, _p3641) => (new ~(_r3638, _r3640), _p3641) } } match {
    case Some((_st3635, _np3636)) => _rs3633 = _rs3633 :+ _st3635; _cp3634 = _np3636
    case None => _go3637 = false } }
  Some((_rs3633, _cp3634)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r3614, _p3615) => (new ~(_r3612, _r3614), _p3615) } }.flatMap { case (_r3608, _p3609) => {
  var _rs3651: List[Any] = Nil; var _cp3652: Int = _p3609; var _go3655 = true
  while (_go3655) { parseStatementSep(input, _cp3652) match {
    case Some((_st3653, _np3654)) => _rs3651 = _rs3651 :+ _st3653; _cp3652 = _np3654
    case None => _go3655 = false } }
  Some((_rs3651, _cp3652)) }.map { case (_r3610, _p3611) => (new ~(_r3608, _r3610), _p3611) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3487)))).map { case (_r3488, _p3489) => (new ~(_r3486, _r3488), _p3489) } }.flatMap { case (_r3482, _p3483) => parseEndKeyword(input, _p3483).map { case (_r3484, _p3485) => (new ~(_r3482, _r3484), _p3485) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseDefExprForm(input: String, pos: Int): Option[(Any, Int)] = parseDefStmt(input, pos).map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseReturnExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("return", pos)) Some(("return", pos + 6)) else None).flatMap { case (_r3664, _p3665) => (if (parseIdentCont(input, _p3665).isEmpty) Some(((), _p3665)) else None).map { case (_r3666, _p3667) => (new ~(_r3664, _r3666), _p3667) } }.flatMap { case (_r3660, _p3661) => parseInlineSpacing(input, _p3661).map { case (_r3662, _p3663) => (new ~(_r3660, _r3662), _p3663) } }.flatMap { case (_r3656, _p3657) => ((if (parseCommandArgStop(input, _p3657).isEmpty) Some(((), _p3657)) else None).flatMap { case (_r3672, _p3673) => parseExpr(input, _p3673).map { case (_r3674, _p3675) => (new ~(_r3672, _r3674), _p3675) } }.flatMap { case (_r3668, _p3669) => {
  var _rs3676: List[Any] = Nil; var _cp3677: Int = _p3669; var _go3680 = true
  while (_go3680) { parseSpacing(input, _cp3677).flatMap { case (_r3689, _p3690) => (if (input.startsWith(",", _p3690)) Some((",", _p3690 + 1)) else None).map { case (_r3691, _p3692) => (new ~(_r3689, _r3691), _p3692) } }.flatMap { case (_r3685, _p3686) => parseSpacing(input, _p3686).map { case (_r3687, _p3688) => (new ~(_r3685, _r3687), _p3688) } }.flatMap { case (_r3681, _p3682) => parseExpr(input, _p3682).map { case (_r3683, _p3684) => (new ~(_r3681, _r3683), _p3684) } } match {
    case Some((_st3678, _np3679)) => _rs3676 = _rs3676 :+ _st3678; _cp3677 = _np3679
    case None => _go3680 = false } }
  Some((_rs3676, _cp3677)) }.map { case (_r3670, _p3671) => (new ~(_r3668, _r3670), _p3671) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3657)))).map { case (_r3658, _p3659) => (new ~(_r3656, _r3658), _p3659) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseYieldExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("yield", pos)) Some(("yield", pos + 5)) else None).flatMap { case (_r3701, _p3702) => (if (parseIdentCont(input, _p3702).isEmpty) Some(((), _p3702)) else None).map { case (_r3703, _p3704) => (new ~(_r3701, _r3703), _p3704) } }.flatMap { case (_r3697, _p3698) => parseInlineSpacing(input, _p3698).map { case (_r3699, _p3700) => (new ~(_r3697, _r3699), _p3700) } }.flatMap { case (_r3693, _p3694) => (parseCallArgs(input, _p3694).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p3694)))).map { case (_r3695, _p3696) => (new ~(_r3693, _r3695), _p3696) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseSingletonClassExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r3737, _p3738) => (if (parseIdentCont(input, _p3738).isEmpty) Some(((), _p3738)) else None).map { case (_r3739, _p3740) => (new ~(_r3737, _r3739), _p3740) } }.flatMap { case (_r3733, _p3734) => parseSpacing(input, _p3734).map { case (_r3735, _p3736) => (new ~(_r3733, _r3735), _p3736) } }.flatMap { case (_r3729, _p3730) => (if (input.startsWith("<<", _p3730)) Some(("<<", _p3730 + 2)) else None).map { case (_r3731, _p3732) => (new ~(_r3729, _r3731), _p3732) } }.flatMap { case (_r3725, _p3726) => parseSpacing(input, _p3726).map { case (_r3727, _p3728) => (new ~(_r3725, _r3727), _p3728) } }.flatMap { case (_r3721, _p3722) => parseExpr(input, _p3722).map { case (_r3723, _p3724) => (new ~(_r3721, _r3723), _p3724) } }.flatMap { case (_r3717, _p3718) => {
  var _rs3741: List[Any] = Nil; var _cp3742: Int = _p3718; var _go3745 = true
  while (_go3745) { parseStatementSep(input, _cp3742) match {
    case Some((_st3743, _np3744)) => _rs3741 = _rs3741 :+ _st3743; _cp3742 = _np3744
    case None => _go3745 = false } }
  Some((_rs3741, _cp3742)) }.map { case (_r3719, _p3720) => (new ~(_r3717, _r3719), _p3720) } }.flatMap { case (_r3713, _p3714) => {
  var _rs3746: List[Any] = Nil; var _cp3747: Int = _p3714; var _go3750 = true
  while (_go3750) { (if (parseEndKeyword(input, _cp3747).isEmpty) Some(((), _cp3747)) else None).flatMap { case (_r3755, _p3756) => parseStatement(input, _p3756).map { case (_r3757, _p3758) => (new ~(_r3755, _r3757), _p3758) } }.flatMap { case (_r3751, _p3752) => {
  var _rs3759: List[Any] = Nil; var _cp3760: Int = _p3752; var _go3763 = true
  while (_go3763) { parseStatementSep(input, _cp3760) match {
    case Some((_st3761, _np3762)) => _rs3759 = _rs3759 :+ _st3761; _cp3760 = _np3762
    case None => _go3763 = false } }
  Some((_rs3759, _cp3760)) }.map { case (_r3753, _p3754) => (new ~(_r3751, _r3753), _p3754) } } match {
    case Some((_st3748, _np3749)) => _rs3746 = _rs3746 :+ _st3748; _cp3747 = _np3749
    case None => _go3750 = false } }
  Some((_rs3746, _cp3747)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r3715, _p3716) => (new ~(_r3713, _r3715), _p3716) } }.flatMap { case (_r3709, _p3710) => {
  var _rs3764: List[Any] = Nil; var _cp3765: Int = _p3710; var _go3768 = true
  while (_go3768) { parseStatementSep(input, _cp3765) match {
    case Some((_st3766, _np3767)) => _rs3764 = _rs3764 :+ _st3766; _cp3765 = _np3767
    case None => _go3768 = false } }
  Some((_rs3764, _cp3765)) }.map { case (_r3711, _p3712) => (new ~(_r3709, _r3711), _p3712) } }.flatMap { case (_r3705, _p3706) => parseEndKeyword(input, _p3706).map { case (_r3707, _p3708) => (new ~(_r3705, _r3707), _p3708) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseIfElsifElse(input: String, pos: Int): Option[(Any, Int)] = (((if (input.startsWith("elsif", pos)) Some(("elsif", pos + 5)) else None).flatMap { case (_r3797, _p3798) => (if (parseIdentCont(input, _p3798).isEmpty) Some(((), _p3798)) else None).map { case (_r3799, _p3800) => (new ~(_r3797, _r3799), _p3800) } }.flatMap { case (_r3793, _p3794) => parseSpacing(input, _p3794).map { case (_r3795, _p3796) => (new ~(_r3793, _r3795), _p3796) } }.flatMap { case (_r3789, _p3790) => parseExpr(input, _p3790).map { case (_r3791, _p3792) => (new ~(_r3789, _r3791), _p3792) } }.flatMap { case (_r3785, _p3786) => ((if (input.startsWith("then", _p3786)) Some(("then", _p3786 + 4)) else None).flatMap { case (_r3805, _p3806) => (if (parseIdentCont(input, _p3806).isEmpty) Some(((), _p3806)) else None).map { case (_r3807, _p3808) => (new ~(_r3805, _r3807), _p3808) } }.flatMap { case (_r3801, _p3802) => parseSpacing(input, _p3802).map { case (_r3803, _p3804) => (new ~(_r3801, _r3803), _p3804) } }).orElse(parseInlineSpacing(input, _p3786).flatMap { case (_r3809, _p3810) => {
  parseStatementSep(input, _p3810) match {
    case None => None
    case Some((_fs3813, _fp3814)) =>
      var _rs3815: List[Any] = List(_fs3813); var _cp3816: Int = _fp3814; var _go3819 = true
      while (_go3819) { parseStatementSep(input, _cp3816) match {
        case Some((_st3817, _np3818)) => _rs3815 = _rs3815 :+ _st3817; _cp3816 = _np3818
        case None => _go3819 = false } }
      Some((_rs3815, _cp3816)) } }.map { case (_r3811, _p3812) => (new ~(_r3809, _r3811), _p3812) } }).map { case (_r3787, _p3788) => (new ~(_r3785, _r3787), _p3788) } }.flatMap { case (_r3781, _p3782) => {
  var _rs3820: List[Any] = Nil; var _cp3821: Int = _p3782; var _go3824 = true
  while (_go3824) { parseStatementSep(input, _cp3821) match {
    case Some((_st3822, _np3823)) => _rs3820 = _rs3820 :+ _st3822; _cp3821 = _np3823
    case None => _go3824 = false } }
  Some((_rs3820, _cp3821)) }.map { case (_r3783, _p3784) => (new ~(_r3781, _r3783), _p3784) } }.flatMap { case (_r3777, _p3778) => {
  var _rs3825: List[Any] = Nil; var _cp3826: Int = _p3778; var _go3829 = true
  while (_go3829) { (if ((((if (input.startsWith("elsif", _cp3826)) Some(("elsif", _cp3826 + 5)) else None).flatMap { case (_r3842, _p3843) => (if (parseIdentCont(input, _p3843).isEmpty) Some(((), _p3843)) else None).map { case (_r3844, _p3845) => (new ~(_r3842, _r3844), _p3845) } }.flatMap { case (_r3838, _p3839) => parseSpacing(input, _p3839).map { case (_r3840, _p3841) => (new ~(_r3838, _r3840), _p3841) } }).orElse((if (input.startsWith("else", _cp3826)) Some(("else", _cp3826 + 4)) else None).flatMap { case (_r3850, _p3851) => (if (parseIdentCont(input, _p3851).isEmpty) Some(((), _p3851)) else None).map { case (_r3852, _p3853) => (new ~(_r3850, _r3852), _p3853) } }.flatMap { case (_r3846, _p3847) => parseSpacing(input, _p3847).map { case (_r3848, _p3849) => (new ~(_r3846, _r3848), _p3849) } })).orElse(parseEndKeyword(input, _cp3826)).isEmpty) Some(((), _cp3826)) else None).flatMap { case (_r3834, _p3835) => parseStatement(input, _p3835).map { case (_r3836, _p3837) => (new ~(_r3834, _r3836), _p3837) } }.flatMap { case (_r3830, _p3831) => {
  var _rs3854: List[Any] = Nil; var _cp3855: Int = _p3831; var _go3858 = true
  while (_go3858) { parseStatementSep(input, _cp3855) match {
    case Some((_st3856, _np3857)) => _rs3854 = _rs3854 :+ _st3856; _cp3855 = _np3857
    case None => _go3858 = false } }
  Some((_rs3854, _cp3855)) }.map { case (_r3832, _p3833) => (new ~(_r3830, _r3832), _p3833) } } match {
    case Some((_st3827, _np3828)) => _rs3825 = _rs3825 :+ _st3827; _cp3826 = _np3828
    case None => _go3829 = false } }
  Some((_rs3825, _cp3826)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r3779, _p3780) => (new ~(_r3777, _r3779), _p3780) } }.flatMap { case (_r3773, _p3774) => {
  var _rs3859: List[Any] = Nil; var _cp3860: Int = _p3774; var _go3863 = true
  while (_go3863) { parseStatementSep(input, _cp3860) match {
    case Some((_st3861, _np3862)) => _rs3859 = _rs3859 :+ _st3861; _cp3860 = _np3862
    case None => _go3863 = false } }
  Some((_rs3859, _cp3860)) }.map { case (_r3775, _p3776) => (new ~(_r3773, _r3775), _p3776) } }.flatMap { case (_r3769, _p3770) => parseIfElsifElse(input, _p3770).map { case (_r3771, _p3772) => (new ~(_r3769, _r3771), _p3772) } }).orElse((if (input.startsWith("else", pos)) Some(("else", pos + 4)) else None).flatMap { case (_r3880, _p3881) => (if (parseIdentCont(input, _p3881).isEmpty) Some(((), _p3881)) else None).map { case (_r3882, _p3883) => (new ~(_r3880, _r3882), _p3883) } }.flatMap { case (_r3876, _p3877) => parseSpacing(input, _p3877).map { case (_r3878, _p3879) => (new ~(_r3876, _r3878), _p3879) } }.flatMap { case (_r3872, _p3873) => {
  var _rs3884: List[Any] = Nil; var _cp3885: Int = _p3873; var _go3888 = true
  while (_go3888) { parseStatementSep(input, _cp3885) match {
    case Some((_st3886, _np3887)) => _rs3884 = _rs3884 :+ _st3886; _cp3885 = _np3887
    case None => _go3888 = false } }
  Some((_rs3884, _cp3885)) }.map { case (_r3874, _p3875) => (new ~(_r3872, _r3874), _p3875) } }.flatMap { case (_r3868, _p3869) => {
  var _rs3889: List[Any] = Nil; var _cp3890: Int = _p3869; var _go3893 = true
  while (_go3893) { (if (parseEndKeyword(input, _cp3890).isEmpty) Some(((), _cp3890)) else None).flatMap { case (_r3898, _p3899) => parseStatement(input, _p3899).map { case (_r3900, _p3901) => (new ~(_r3898, _r3900), _p3901) } }.flatMap { case (_r3894, _p3895) => {
  var _rs3902: List[Any] = Nil; var _cp3903: Int = _p3895; var _go3906 = true
  while (_go3906) { parseStatementSep(input, _cp3903) match {
    case Some((_st3904, _np3905)) => _rs3902 = _rs3902 :+ _st3904; _cp3903 = _np3905
    case None => _go3906 = false } }
  Some((_rs3902, _cp3903)) }.map { case (_r3896, _p3897) => (new ~(_r3894, _r3896), _p3897) } } match {
    case Some((_st3891, _np3892)) => _rs3889 = _rs3889 :+ _st3891; _cp3890 = _np3892
    case None => _go3893 = false } }
  Some((_rs3889, _cp3890)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r3870, _p3871) => (new ~(_r3868, _r3870), _p3871) } }.flatMap { case (_r3864, _p3865) => {
  var _rs3907: List[Any] = Nil; var _cp3908: Int = _p3865; var _go3911 = true
  while (_go3911) { parseStatementSep(input, _cp3908) match {
    case Some((_st3909, _np3910)) => _rs3907 = _rs3907 :+ _st3909; _cp3908 = _np3910
    case None => _go3911 = false } }
  Some((_rs3907, _cp3908)) }.map { case (_r3866, _p3867) => (new ~(_r3864, _r3866), _p3867) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, pos)))).map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }

  def parseIfExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r3944, _p3945) => (if (parseIdentCont(input, _p3945).isEmpty) Some(((), _p3945)) else None).map { case (_r3946, _p3947) => (new ~(_r3944, _r3946), _p3947) } }.flatMap { case (_r3940, _p3941) => parseSpacing(input, _p3941).map { case (_r3942, _p3943) => (new ~(_r3940, _r3942), _p3943) } }.flatMap { case (_r3936, _p3937) => parseExpr(input, _p3937).map { case (_r3938, _p3939) => (new ~(_r3936, _r3938), _p3939) } }.flatMap { case (_r3932, _p3933) => (((if (input.startsWith("then", _p3933)) Some(("then", _p3933 + 4)) else None).flatMap { case (_r3952, _p3953) => (if (parseIdentCont(input, _p3953).isEmpty) Some(((), _p3953)) else None).map { case (_r3954, _p3955) => (new ~(_r3952, _r3954), _p3955) } }.flatMap { case (_r3948, _p3949) => parseSpacing(input, _p3949).map { case (_r3950, _p3951) => (new ~(_r3948, _r3950), _p3951) } }).orElse(parseInlineSpacing(input, _p3933).flatMap { case (_r3956, _p3957) => {
  parseStatementSep(input, _p3957) match {
    case None => None
    case Some((_fs3960, _fp3961)) =>
      var _rs3962: List[Any] = List(_fs3960); var _cp3963: Int = _fp3961; var _go3966 = true
      while (_go3966) { parseStatementSep(input, _cp3963) match {
        case Some((_st3964, _np3965)) => _rs3962 = _rs3962 :+ _st3964; _cp3963 = _np3965
        case None => _go3966 = false } }
      Some((_rs3962, _cp3963)) } }.map { case (_r3958, _p3959) => (new ~(_r3956, _r3958), _p3959) } })).orElse(parseInlineSpacing(input, _p3933).flatMap { case (_r3967, _p3968) => (if (input.startsWith(";", _p3968)) Some((";", _p3968 + 1)) else None).map { case (_r3969, _p3970) => (new ~(_r3967, _r3969), _p3970) } }).map { case (_r3934, _p3935) => (new ~(_r3932, _r3934), _p3935) } }.flatMap { case (_r3928, _p3929) => {
  var _rs3971: List[Any] = Nil; var _cp3972: Int = _p3929; var _go3975 = true
  while (_go3975) { parseStatementSep(input, _cp3972) match {
    case Some((_st3973, _np3974)) => _rs3971 = _rs3971 :+ _st3973; _cp3972 = _np3974
    case None => _go3975 = false } }
  Some((_rs3971, _cp3972)) }.map { case (_r3930, _p3931) => (new ~(_r3928, _r3930), _p3931) } }.flatMap { case (_r3924, _p3925) => {
  var _rs3976: List[Any] = Nil; var _cp3977: Int = _p3925; var _go3980 = true
  while (_go3980) { (if ((((if (input.startsWith("elsif", _cp3977)) Some(("elsif", _cp3977 + 5)) else None).flatMap { case (_r3993, _p3994) => (if (parseIdentCont(input, _p3994).isEmpty) Some(((), _p3994)) else None).map { case (_r3995, _p3996) => (new ~(_r3993, _r3995), _p3996) } }.flatMap { case (_r3989, _p3990) => parseSpacing(input, _p3990).map { case (_r3991, _p3992) => (new ~(_r3989, _r3991), _p3992) } }).orElse((if (input.startsWith("else", _cp3977)) Some(("else", _cp3977 + 4)) else None).flatMap { case (_r4001, _p4002) => (if (parseIdentCont(input, _p4002).isEmpty) Some(((), _p4002)) else None).map { case (_r4003, _p4004) => (new ~(_r4001, _r4003), _p4004) } }.flatMap { case (_r3997, _p3998) => parseSpacing(input, _p3998).map { case (_r3999, _p4000) => (new ~(_r3997, _r3999), _p4000) } })).orElse(parseEndKeyword(input, _cp3977)).isEmpty) Some(((), _cp3977)) else None).flatMap { case (_r3985, _p3986) => parseStatement(input, _p3986).map { case (_r3987, _p3988) => (new ~(_r3985, _r3987), _p3988) } }.flatMap { case (_r3981, _p3982) => {
  var _rs4005: List[Any] = Nil; var _cp4006: Int = _p3982; var _go4009 = true
  while (_go4009) { parseStatementSep(input, _cp4006) match {
    case Some((_st4007, _np4008)) => _rs4005 = _rs4005 :+ _st4007; _cp4006 = _np4008
    case None => _go4009 = false } }
  Some((_rs4005, _cp4006)) }.map { case (_r3983, _p3984) => (new ~(_r3981, _r3983), _p3984) } } match {
    case Some((_st3978, _np3979)) => _rs3976 = _rs3976 :+ _st3978; _cp3977 = _np3979
    case None => _go3980 = false } }
  Some((_rs3976, _cp3977)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r3926, _p3927) => (new ~(_r3924, _r3926), _p3927) } }.flatMap { case (_r3920, _p3921) => {
  var _rs4010: List[Any] = Nil; var _cp4011: Int = _p3921; var _go4014 = true
  while (_go4014) { parseStatementSep(input, _cp4011) match {
    case Some((_st4012, _np4013)) => _rs4010 = _rs4010 :+ _st4012; _cp4011 = _np4013
    case None => _go4014 = false } }
  Some((_rs4010, _cp4011)) }.map { case (_r3922, _p3923) => (new ~(_r3920, _r3922), _p3923) } }.flatMap { case (_r3916, _p3917) => parseIfElsifElse(input, _p3917).map { case (_r3918, _p3919) => (new ~(_r3916, _r3918), _p3919) } }.flatMap { case (_r3912, _p3913) => parseEndKeyword(input, _p3913).map { case (_r3914, _p3915) => (new ~(_r3912, _r3914), _p3915) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseUnlessExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r4047, _p4048) => (if (parseIdentCont(input, _p4048).isEmpty) Some(((), _p4048)) else None).map { case (_r4049, _p4050) => (new ~(_r4047, _r4049), _p4050) } }.flatMap { case (_r4043, _p4044) => parseSpacing(input, _p4044).map { case (_r4045, _p4046) => (new ~(_r4043, _r4045), _p4046) } }.flatMap { case (_r4039, _p4040) => parseExpr(input, _p4040).map { case (_r4041, _p4042) => (new ~(_r4039, _r4041), _p4042) } }.flatMap { case (_r4035, _p4036) => (((if (input.startsWith("then", _p4036)) Some(("then", _p4036 + 4)) else None).flatMap { case (_r4055, _p4056) => (if (parseIdentCont(input, _p4056).isEmpty) Some(((), _p4056)) else None).map { case (_r4057, _p4058) => (new ~(_r4055, _r4057), _p4058) } }.flatMap { case (_r4051, _p4052) => parseSpacing(input, _p4052).map { case (_r4053, _p4054) => (new ~(_r4051, _r4053), _p4054) } }).orElse(parseInlineSpacing(input, _p4036).flatMap { case (_r4059, _p4060) => {
  parseStatementSep(input, _p4060) match {
    case None => None
    case Some((_fs4063, _fp4064)) =>
      var _rs4065: List[Any] = List(_fs4063); var _cp4066: Int = _fp4064; var _go4069 = true
      while (_go4069) { parseStatementSep(input, _cp4066) match {
        case Some((_st4067, _np4068)) => _rs4065 = _rs4065 :+ _st4067; _cp4066 = _np4068
        case None => _go4069 = false } }
      Some((_rs4065, _cp4066)) } }.map { case (_r4061, _p4062) => (new ~(_r4059, _r4061), _p4062) } })).orElse(parseInlineSpacing(input, _p4036).flatMap { case (_r4070, _p4071) => (if (input.startsWith(";", _p4071)) Some((";", _p4071 + 1)) else None).map { case (_r4072, _p4073) => (new ~(_r4070, _r4072), _p4073) } }).map { case (_r4037, _p4038) => (new ~(_r4035, _r4037), _p4038) } }.flatMap { case (_r4031, _p4032) => {
  var _rs4074: List[Any] = Nil; var _cp4075: Int = _p4032; var _go4078 = true
  while (_go4078) { parseStatementSep(input, _cp4075) match {
    case Some((_st4076, _np4077)) => _rs4074 = _rs4074 :+ _st4076; _cp4075 = _np4077
    case None => _go4078 = false } }
  Some((_rs4074, _cp4075)) }.map { case (_r4033, _p4034) => (new ~(_r4031, _r4033), _p4034) } }.flatMap { case (_r4027, _p4028) => {
  var _rs4079: List[Any] = Nil; var _cp4080: Int = _p4028; var _go4083 = true
  while (_go4083) { (if (((if (input.startsWith("else", _cp4080)) Some(("else", _cp4080 + 4)) else None).flatMap { case (_r4096, _p4097) => (if (parseIdentCont(input, _p4097).isEmpty) Some(((), _p4097)) else None).map { case (_r4098, _p4099) => (new ~(_r4096, _r4098), _p4099) } }.flatMap { case (_r4092, _p4093) => parseSpacing(input, _p4093).map { case (_r4094, _p4095) => (new ~(_r4092, _r4094), _p4095) } }).orElse(parseEndKeyword(input, _cp4080)).isEmpty) Some(((), _cp4080)) else None).flatMap { case (_r4088, _p4089) => parseStatement(input, _p4089).map { case (_r4090, _p4091) => (new ~(_r4088, _r4090), _p4091) } }.flatMap { case (_r4084, _p4085) => {
  var _rs4100: List[Any] = Nil; var _cp4101: Int = _p4085; var _go4104 = true
  while (_go4104) { parseStatementSep(input, _cp4101) match {
    case Some((_st4102, _np4103)) => _rs4100 = _rs4100 :+ _st4102; _cp4101 = _np4103
    case None => _go4104 = false } }
  Some((_rs4100, _cp4101)) }.map { case (_r4086, _p4087) => (new ~(_r4084, _r4086), _p4087) } } match {
    case Some((_st4081, _np4082)) => _rs4079 = _rs4079 :+ _st4081; _cp4080 = _np4082
    case None => _go4083 = false } }
  Some((_rs4079, _cp4080)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4029, _p4030) => (new ~(_r4027, _r4029), _p4030) } }.flatMap { case (_r4023, _p4024) => {
  var _rs4105: List[Any] = Nil; var _cp4106: Int = _p4024; var _go4109 = true
  while (_go4109) { parseStatementSep(input, _cp4106) match {
    case Some((_st4107, _np4108)) => _rs4105 = _rs4105 :+ _st4107; _cp4106 = _np4108
    case None => _go4109 = false } }
  Some((_rs4105, _cp4106)) }.map { case (_r4025, _p4026) => (new ~(_r4023, _r4025), _p4026) } }.flatMap { case (_r4019, _p4020) => ((if (input.startsWith("else", _p4020)) Some(("else", _p4020 + 4)) else None).flatMap { case (_r4126, _p4127) => (if (parseIdentCont(input, _p4127).isEmpty) Some(((), _p4127)) else None).map { case (_r4128, _p4129) => (new ~(_r4126, _r4128), _p4129) } }.flatMap { case (_r4122, _p4123) => parseSpacing(input, _p4123).map { case (_r4124, _p4125) => (new ~(_r4122, _r4124), _p4125) } }.flatMap { case (_r4118, _p4119) => {
  var _rs4130: List[Any] = Nil; var _cp4131: Int = _p4119; var _go4134 = true
  while (_go4134) { parseStatementSep(input, _cp4131) match {
    case Some((_st4132, _np4133)) => _rs4130 = _rs4130 :+ _st4132; _cp4131 = _np4133
    case None => _go4134 = false } }
  Some((_rs4130, _cp4131)) }.map { case (_r4120, _p4121) => (new ~(_r4118, _r4120), _p4121) } }.flatMap { case (_r4114, _p4115) => {
  var _rs4135: List[Any] = Nil; var _cp4136: Int = _p4115; var _go4139 = true
  while (_go4139) { (if (parseEndKeyword(input, _cp4136).isEmpty) Some(((), _cp4136)) else None).flatMap { case (_r4144, _p4145) => parseStatement(input, _p4145).map { case (_r4146, _p4147) => (new ~(_r4144, _r4146), _p4147) } }.flatMap { case (_r4140, _p4141) => {
  var _rs4148: List[Any] = Nil; var _cp4149: Int = _p4141; var _go4152 = true
  while (_go4152) { parseStatementSep(input, _cp4149) match {
    case Some((_st4150, _np4151)) => _rs4148 = _rs4148 :+ _st4150; _cp4149 = _np4151
    case None => _go4152 = false } }
  Some((_rs4148, _cp4149)) }.map { case (_r4142, _p4143) => (new ~(_r4140, _r4142), _p4143) } } match {
    case Some((_st4137, _np4138)) => _rs4135 = _rs4135 :+ _st4137; _cp4136 = _np4138
    case None => _go4139 = false } }
  Some((_rs4135, _cp4136)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4116, _p4117) => (new ~(_r4114, _r4116), _p4117) } }.flatMap { case (_r4110, _p4111) => {
  var _rs4153: List[Any] = Nil; var _cp4154: Int = _p4111; var _go4157 = true
  while (_go4157) { parseStatementSep(input, _cp4154) match {
    case Some((_st4155, _np4156)) => _rs4153 = _rs4153 :+ _st4155; _cp4154 = _np4156
    case None => _go4157 = false } }
  Some((_rs4153, _cp4154)) }.map { case (_r4112, _p4113) => (new ~(_r4110, _r4112), _p4113) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4020)))).map { case (_r4021, _p4022) => (new ~(_r4019, _r4021), _p4022) } }.flatMap { case (_r4015, _p4016) => parseEndKeyword(input, _p4016).map { case (_r4017, _p4018) => (new ~(_r4015, _r4017), _p4018) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseWhenClause(input: String, pos: Int): Option[(Any, Int)] = ((if (input.startsWith("when", pos)) Some(("when", pos + 4)) else None).flatMap { case (_r4186, _p4187) => (if (parseIdentCont(input, _p4187).isEmpty) Some(((), _p4187)) else None).map { case (_r4188, _p4189) => (new ~(_r4186, _r4188), _p4189) } }).orElse((if (input.startsWith("in", pos)) Some(("in", pos + 2)) else None).flatMap { case (_r4190, _p4191) => (if (parseIdentCont(input, _p4191).isEmpty) Some(((), _p4191)) else None).map { case (_r4192, _p4193) => (new ~(_r4190, _r4192), _p4193) } }).flatMap { case (_r4182, _p4183) => parseSpacing(input, _p4183).map { case (_r4184, _p4185) => (new ~(_r4182, _r4184), _p4185) } }.flatMap { case (_r4178, _p4179) => parseInPatternExpr(input, _p4179).map { case (_r4180, _p4181) => (new ~(_r4178, _r4180), _p4181) } }.flatMap { case (_r4174, _p4175) => {
  var _rs4194: List[Any] = Nil; var _cp4195: Int = _p4175; var _go4198 = true
  while (_go4198) { parseSpacing(input, _cp4195).flatMap { case (_r4207, _p4208) => (if (input.startsWith(",", _p4208)) Some((",", _p4208 + 1)) else None).map { case (_r4209, _p4210) => (new ~(_r4207, _r4209), _p4210) } }.flatMap { case (_r4203, _p4204) => parseSpacing(input, _p4204).map { case (_r4205, _p4206) => (new ~(_r4203, _r4205), _p4206) } }.flatMap { case (_r4199, _p4200) => parseInPatternExpr(input, _p4200).map { case (_r4201, _p4202) => (new ~(_r4199, _r4201), _p4202) } } match {
    case Some((_st4196, _np4197)) => _rs4194 = _rs4194 :+ _st4196; _cp4195 = _np4197
    case None => _go4198 = false } }
  Some((_rs4194, _cp4195)) }.map { case (_r4176, _p4177) => (new ~(_r4174, _r4176), _p4177) } }.flatMap { case (_r4170, _p4171) => (parseInlineSpacing(input, _p4171).flatMap { case (_r4215, _p4216) => ((if (input.startsWith(";", _p4216)) Some((";", _p4216 + 1)) else None)).orElse((if (input.startsWith("then", _p4216)) Some(("then", _p4216 + 4)) else None).flatMap { case (_r4219, _p4220) => (if (parseIdentCont(input, _p4220).isEmpty) Some(((), _p4220)) else None).map { case (_r4221, _p4222) => (new ~(_r4219, _r4221), _p4222) } }).map { case (_r4217, _p4218) => (new ~(_r4215, _r4217), _p4218) } }.flatMap { case (_r4211, _p4212) => parseSpacing(input, _p4212).map { case (_r4213, _p4214) => (new ~(_r4211, _r4213), _p4214) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4171)))).map { case (_r4172, _p4173) => (new ~(_r4170, _r4172), _p4173) } }.flatMap { case (_r4166, _p4167) => {
  var _rs4223: List[Any] = Nil; var _cp4224: Int = _p4167; var _go4227 = true
  while (_go4227) { parseStatementSep(input, _cp4224) match {
    case Some((_st4225, _np4226)) => _rs4223 = _rs4223 :+ _st4225; _cp4224 = _np4226
    case None => _go4227 = false } }
  Some((_rs4223, _cp4224)) }.map { case (_r4168, _p4169) => (new ~(_r4166, _r4168), _p4169) } }.flatMap { case (_r4162, _p4163) => {
  var _rs4228: List[Any] = Nil; var _cp4229: Int = _p4163; var _go4232 = true
  while (_go4232) { (if (((((if (input.startsWith("when", _cp4229)) Some(("when", _cp4229 + 4)) else None).flatMap { case (_r4245, _p4246) => (if (parseIdentCont(input, _p4246).isEmpty) Some(((), _p4246)) else None).map { case (_r4247, _p4248) => (new ~(_r4245, _r4247), _p4248) } }.flatMap { case (_r4241, _p4242) => parseSpacing(input, _p4242).map { case (_r4243, _p4244) => (new ~(_r4241, _r4243), _p4244) } }).orElse((if (input.startsWith("in", _cp4229)) Some(("in", _cp4229 + 2)) else None).flatMap { case (_r4253, _p4254) => (if (parseIdentCont(input, _p4254).isEmpty) Some(((), _p4254)) else None).map { case (_r4255, _p4256) => (new ~(_r4253, _r4255), _p4256) } }.flatMap { case (_r4249, _p4250) => parseSpacing(input, _p4250).map { case (_r4251, _p4252) => (new ~(_r4249, _r4251), _p4252) } })).orElse((if (input.startsWith("else", _cp4229)) Some(("else", _cp4229 + 4)) else None).flatMap { case (_r4261, _p4262) => (if (parseIdentCont(input, _p4262).isEmpty) Some(((), _p4262)) else None).map { case (_r4263, _p4264) => (new ~(_r4261, _r4263), _p4264) } }.flatMap { case (_r4257, _p4258) => parseSpacing(input, _p4258).map { case (_r4259, _p4260) => (new ~(_r4257, _r4259), _p4260) } })).orElse(parseEndKeyword(input, _cp4229)).isEmpty) Some(((), _cp4229)) else None).flatMap { case (_r4237, _p4238) => parseStatement(input, _p4238).map { case (_r4239, _p4240) => (new ~(_r4237, _r4239), _p4240) } }.flatMap { case (_r4233, _p4234) => {
  var _rs4265: List[Any] = Nil; var _cp4266: Int = _p4234; var _go4269 = true
  while (_go4269) { parseStatementSep(input, _cp4266) match {
    case Some((_st4267, _np4268)) => _rs4265 = _rs4265 :+ _st4267; _cp4266 = _np4268
    case None => _go4269 = false } }
  Some((_rs4265, _cp4266)) }.map { case (_r4235, _p4236) => (new ~(_r4233, _r4235), _p4236) } } match {
    case Some((_st4230, _np4231)) => _rs4228 = _rs4228 :+ _st4230; _cp4229 = _np4231
    case None => _go4232 = false } }
  Some((_rs4228, _cp4229)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4164, _p4165) => (new ~(_r4162, _r4164), _p4165) } }.flatMap { case (_r4158, _p4159) => {
  var _rs4270: List[Any] = Nil; var _cp4271: Int = _p4159; var _go4274 = true
  while (_go4274) { parseStatementSep(input, _cp4271) match {
    case Some((_st4272, _np4273)) => _rs4270 = _rs4270 :+ _st4272; _cp4271 = _np4273
    case None => _go4274 = false } }
  Some((_rs4270, _cp4271)) }.map { case (_r4160, _p4161) => (new ~(_r4158, _r4160), _p4161) } }.map { case (r, p) => (_applyAction({  _ => WhenClause(List.empty, List.empty)  }, r), p) }

  def parseCaseExpr(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("case", pos)) Some(("case", pos + 4)) else None).flatMap { case (_r4299, _p4300) => (if (parseIdentCont(input, _p4300).isEmpty) Some(((), _p4300)) else None).map { case (_r4301, _p4302) => (new ~(_r4299, _r4301), _p4302) } }.flatMap { case (_r4295, _p4296) => parseSpacing(input, _p4296).map { case (_r4297, _p4298) => (new ~(_r4295, _r4297), _p4298) } }.flatMap { case (_r4291, _p4292) => (parseExpr(input, _p4292).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4292)))).map { case (_r4293, _p4294) => (new ~(_r4291, _r4293), _p4294) } }.flatMap { case (_r4287, _p4288) => {
  var _rs4303: List[Any] = Nil; var _cp4304: Int = _p4288; var _go4307 = true
  while (_go4307) { parseStatementSep(input, _cp4304) match {
    case Some((_st4305, _np4306)) => _rs4303 = _rs4303 :+ _st4305; _cp4304 = _np4306
    case None => _go4307 = false } }
  Some((_rs4303, _cp4304)) }.map { case (_r4289, _p4290) => (new ~(_r4287, _r4289), _p4290) } }.flatMap { case (_r4283, _p4284) => {
  parseWhenClause(input, _p4284) match {
    case None => None
    case Some((_fs4308, _fp4309)) =>
      var _rs4310: List[Any] = List(_fs4308); var _cp4311: Int = _fp4309; var _go4314 = true
      while (_go4314) { parseWhenClause(input, _cp4311) match {
        case Some((_st4312, _np4313)) => _rs4310 = _rs4310 :+ _st4312; _cp4311 = _np4313
        case None => _go4314 = false } }
      Some((_rs4310, _cp4311)) } }.map { case (_r4285, _p4286) => (new ~(_r4283, _r4285), _p4286) } }.flatMap { case (_r4279, _p4280) => ((if (input.startsWith("else", _p4280)) Some(("else", _p4280 + 4)) else None).flatMap { case (_r4331, _p4332) => (if (parseIdentCont(input, _p4332).isEmpty) Some(((), _p4332)) else None).map { case (_r4333, _p4334) => (new ~(_r4331, _r4333), _p4334) } }.flatMap { case (_r4327, _p4328) => parseSpacing(input, _p4328).map { case (_r4329, _p4330) => (new ~(_r4327, _r4329), _p4330) } }.flatMap { case (_r4323, _p4324) => {
  var _rs4335: List[Any] = Nil; var _cp4336: Int = _p4324; var _go4339 = true
  while (_go4339) { parseStatementSep(input, _cp4336) match {
    case Some((_st4337, _np4338)) => _rs4335 = _rs4335 :+ _st4337; _cp4336 = _np4338
    case None => _go4339 = false } }
  Some((_rs4335, _cp4336)) }.map { case (_r4325, _p4326) => (new ~(_r4323, _r4325), _p4326) } }.flatMap { case (_r4319, _p4320) => {
  var _rs4340: List[Any] = Nil; var _cp4341: Int = _p4320; var _go4344 = true
  while (_go4344) { (if (parseEndKeyword(input, _cp4341).isEmpty) Some(((), _cp4341)) else None).flatMap { case (_r4349, _p4350) => parseStatement(input, _p4350).map { case (_r4351, _p4352) => (new ~(_r4349, _r4351), _p4352) } }.flatMap { case (_r4345, _p4346) => {
  var _rs4353: List[Any] = Nil; var _cp4354: Int = _p4346; var _go4357 = true
  while (_go4357) { parseStatementSep(input, _cp4354) match {
    case Some((_st4355, _np4356)) => _rs4353 = _rs4353 :+ _st4355; _cp4354 = _np4356
    case None => _go4357 = false } }
  Some((_rs4353, _cp4354)) }.map { case (_r4347, _p4348) => (new ~(_r4345, _r4347), _p4348) } } match {
    case Some((_st4342, _np4343)) => _rs4340 = _rs4340 :+ _st4342; _cp4341 = _np4343
    case None => _go4344 = false } }
  Some((_rs4340, _cp4341)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4321, _p4322) => (new ~(_r4319, _r4321), _p4322) } }.flatMap { case (_r4315, _p4316) => {
  var _rs4358: List[Any] = Nil; var _cp4359: Int = _p4316; var _go4362 = true
  while (_go4362) { parseStatementSep(input, _cp4359) match {
    case Some((_st4360, _np4361)) => _rs4358 = _rs4358 :+ _st4360; _cp4359 = _np4361
    case None => _go4362 = false } }
  Some((_rs4358, _cp4359)) }.map { case (_r4317, _p4318) => (new ~(_r4315, _r4317), _p4318) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4280)))).map { case (_r4281, _p4282) => (new ~(_r4279, _r4281), _p4282) } }.flatMap { case (_r4275, _p4276) => parseEndKeyword(input, _p4276).map { case (_r4277, _p4278) => (new ~(_r4275, _r4277), _p4278) } }.map { case (r, p) => (_applyAction({  _ => NilLiteral()  }, r), p) }

  def parseModifierSuffix(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r4371, _p4372) => (if (parseIdentCont(input, _p4372).isEmpty) Some(((), _p4372)) else None).map { case (_r4373, _p4374) => (new ~(_r4371, _r4373), _p4374) } }.flatMap { case (_r4367, _p4368) => parseSpacing(input, _p4368).map { case (_r4369, _p4370) => (new ~(_r4367, _r4369), _p4370) } }.flatMap { case (_r4363, _p4364) => parseExpr(input, _p4364).map { case (_r4365, _p4366) => (new ~(_r4363, _r4365), _p4366) } }).orElse((if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r4383, _p4384) => (if (parseIdentCont(input, _p4384).isEmpty) Some(((), _p4384)) else None).map { case (_r4385, _p4386) => (new ~(_r4383, _r4385), _p4386) } }.flatMap { case (_r4379, _p4380) => parseSpacing(input, _p4380).map { case (_r4381, _p4382) => (new ~(_r4379, _r4381), _p4382) } }.flatMap { case (_r4375, _p4376) => parseExpr(input, _p4376).map { case (_r4377, _p4378) => (new ~(_r4375, _r4377), _p4378) } })).orElse((if (input.startsWith("while", pos)) Some(("while", pos + 5)) else None).flatMap { case (_r4395, _p4396) => (if (parseIdentCont(input, _p4396).isEmpty) Some(((), _p4396)) else None).map { case (_r4397, _p4398) => (new ~(_r4395, _r4397), _p4398) } }.flatMap { case (_r4391, _p4392) => parseSpacing(input, _p4392).map { case (_r4393, _p4394) => (new ~(_r4391, _r4393), _p4394) } }.flatMap { case (_r4387, _p4388) => parseExpr(input, _p4388).map { case (_r4389, _p4390) => (new ~(_r4387, _r4389), _p4390) } })).orElse((if (input.startsWith("until", pos)) Some(("until", pos + 5)) else None).flatMap { case (_r4407, _p4408) => (if (parseIdentCont(input, _p4408).isEmpty) Some(((), _p4408)) else None).map { case (_r4409, _p4410) => (new ~(_r4407, _r4409), _p4410) } }.flatMap { case (_r4403, _p4404) => parseSpacing(input, _p4404).map { case (_r4405, _p4406) => (new ~(_r4403, _r4405), _p4406) } }.flatMap { case (_r4399, _p4400) => parseExpr(input, _p4400).map { case (_r4401, _p4402) => (new ~(_r4399, _r4401), _p4402) } })).orElse((if (input.startsWith("rescue", pos)) Some(("rescue", pos + 6)) else None).flatMap { case (_r4419, _p4420) => (if (parseIdentCont(input, _p4420).isEmpty) Some(((), _p4420)) else None).map { case (_r4421, _p4422) => (new ~(_r4419, _r4421), _p4422) } }.flatMap { case (_r4415, _p4416) => parseSpacing(input, _p4416).map { case (_r4417, _p4418) => (new ~(_r4415, _r4417), _p4418) } }.flatMap { case (_r4411, _p4412) => parseExpr(input, _p4412).map { case (_r4413, _p4414) => (new ~(_r4411, _r4413), _p4414) } }).map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseReturnStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("return", pos)) Some(("return", pos + 6)) else None).flatMap { case (_r4431, _p4432) => (if (parseIdentCont(input, _p4432).isEmpty) Some(((), _p4432)) else None).map { case (_r4433, _p4434) => (new ~(_r4431, _r4433), _p4434) } }.flatMap { case (_r4427, _p4428) => parseInlineSpacing(input, _p4428).map { case (_r4429, _p4430) => (new ~(_r4427, _r4429), _p4430) } }.flatMap { case (_r4423, _p4424) => ((if (parseCommandArgStop(input, _p4424).isEmpty) Some(((), _p4424)) else None).flatMap { case (_r4439, _p4440) => parseExpr(input, _p4440).map { case (_r4441, _p4442) => (new ~(_r4439, _r4441), _p4442) } }.flatMap { case (_r4435, _p4436) => {
  var _rs4443: List[Any] = Nil; var _cp4444: Int = _p4436; var _go4447 = true
  while (_go4447) { parseSpacing(input, _cp4444).flatMap { case (_r4456, _p4457) => (if (input.startsWith(",", _p4457)) Some((",", _p4457 + 1)) else None).map { case (_r4458, _p4459) => (new ~(_r4456, _r4458), _p4459) } }.flatMap { case (_r4452, _p4453) => parseSpacing(input, _p4453).map { case (_r4454, _p4455) => (new ~(_r4452, _r4454), _p4455) } }.flatMap { case (_r4448, _p4449) => parseExpr(input, _p4449).map { case (_r4450, _p4451) => (new ~(_r4448, _r4450), _p4451) } } match {
    case Some((_st4445, _np4446)) => _rs4443 = _rs4443 :+ _st4445; _cp4444 = _np4446
    case None => _go4447 = false } }
  Some((_rs4443, _cp4444)) }.map { case (_r4437, _p4438) => (new ~(_r4435, _r4437), _p4438) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4424)))).map { case (_r4425, _p4426) => (new ~(_r4423, _r4425), _p4426) } }.map { case (r, p) => (_applyAction({  _ => Return(None)  }, r), p) }

  def parseRetryStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("retry", pos)) Some(("retry", pos + 5)) else None).flatMap { case (_r4464, _p4465) => (if (parseIdentCont(input, _p4465).isEmpty) Some(((), _p4465)) else None).map { case (_r4466, _p4467) => (new ~(_r4464, _r4466), _p4467) } }.flatMap { case (_r4460, _p4461) => parseInlineSpacing(input, _p4461).map { case (_r4462, _p4463) => (new ~(_r4460, _r4462), _p4463) } }.map { case (r, p) => (_applyAction({  _ => Retry()  }, r), p) }

  def parseBreakStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("break", pos)) Some(("break", pos + 5)) else None).flatMap { case (_r4476, _p4477) => (if (parseIdentCont(input, _p4477).isEmpty) Some(((), _p4477)) else None).map { case (_r4478, _p4479) => (new ~(_r4476, _r4478), _p4479) } }.flatMap { case (_r4472, _p4473) => parseInlineSpacing(input, _p4473).map { case (_r4474, _p4475) => (new ~(_r4472, _r4474), _p4475) } }.flatMap { case (_r4468, _p4469) => ((if (parseCommandArgStop(input, _p4469).isEmpty) Some(((), _p4469)) else None).flatMap { case (_r4484, _p4485) => parseExpr(input, _p4485).map { case (_r4486, _p4487) => (new ~(_r4484, _r4486), _p4487) } }.flatMap { case (_r4480, _p4481) => {
  var _rs4488: List[Any] = Nil; var _cp4489: Int = _p4481; var _go4492 = true
  while (_go4492) { parseSpacing(input, _cp4489).flatMap { case (_r4501, _p4502) => (if (input.startsWith(",", _p4502)) Some((",", _p4502 + 1)) else None).map { case (_r4503, _p4504) => (new ~(_r4501, _r4503), _p4504) } }.flatMap { case (_r4497, _p4498) => parseSpacing(input, _p4498).map { case (_r4499, _p4500) => (new ~(_r4497, _r4499), _p4500) } }.flatMap { case (_r4493, _p4494) => parseExpr(input, _p4494).map { case (_r4495, _p4496) => (new ~(_r4493, _r4495), _p4496) } } match {
    case Some((_st4490, _np4491)) => _rs4488 = _rs4488 :+ _st4490; _cp4489 = _np4491
    case None => _go4492 = false } }
  Some((_rs4488, _cp4489)) }.map { case (_r4482, _p4483) => (new ~(_r4480, _r4482), _p4483) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4469)))).map { case (_r4470, _p4471) => (new ~(_r4468, _r4470), _p4471) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseNextStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("next", pos)) Some(("next", pos + 4)) else None).flatMap { case (_r4513, _p4514) => (if (parseIdentCont(input, _p4514).isEmpty) Some(((), _p4514)) else None).map { case (_r4515, _p4516) => (new ~(_r4513, _r4515), _p4516) } }.flatMap { case (_r4509, _p4510) => parseInlineSpacing(input, _p4510).map { case (_r4511, _p4512) => (new ~(_r4509, _r4511), _p4512) } }.flatMap { case (_r4505, _p4506) => ((if (parseCommandArgStop(input, _p4506).isEmpty) Some(((), _p4506)) else None).flatMap { case (_r4521, _p4522) => parseExpr(input, _p4522).map { case (_r4523, _p4524) => (new ~(_r4521, _r4523), _p4524) } }.flatMap { case (_r4517, _p4518) => {
  var _rs4525: List[Any] = Nil; var _cp4526: Int = _p4518; var _go4529 = true
  while (_go4529) { parseSpacing(input, _cp4526).flatMap { case (_r4538, _p4539) => (if (input.startsWith(",", _p4539)) Some((",", _p4539 + 1)) else None).map { case (_r4540, _p4541) => (new ~(_r4538, _r4540), _p4541) } }.flatMap { case (_r4534, _p4535) => parseSpacing(input, _p4535).map { case (_r4536, _p4537) => (new ~(_r4534, _r4536), _p4537) } }.flatMap { case (_r4530, _p4531) => parseExpr(input, _p4531).map { case (_r4532, _p4533) => (new ~(_r4530, _r4532), _p4533) } } match {
    case Some((_st4527, _np4528)) => _rs4525 = _rs4525 :+ _st4527; _cp4526 = _np4528
    case None => _go4529 = false } }
  Some((_rs4525, _cp4526)) }.map { case (_r4519, _p4520) => (new ~(_r4517, _r4519), _p4520) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4506)))).map { case (_r4507, _p4508) => (new ~(_r4505, _r4507), _p4508) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseYieldStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("yield", pos)) Some(("yield", pos + 5)) else None).flatMap { case (_r4550, _p4551) => (if (parseIdentCont(input, _p4551).isEmpty) Some(((), _p4551)) else None).map { case (_r4552, _p4553) => (new ~(_r4550, _r4552), _p4553) } }.flatMap { case (_r4546, _p4547) => parseInlineSpacing(input, _p4547).map { case (_r4548, _p4549) => (new ~(_r4546, _r4548), _p4549) } }.flatMap { case (_r4542, _p4543) => (parseCallArgs(input, _p4543).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4543)))).map { case (_r4544, _p4545) => (new ~(_r4542, _r4544), _p4545) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseAliasNamePart(input: String, pos: Int): Option[(Any, Int)] = (((parseSymbolLiteral(input, pos)).orElse(parseSymbolOperatorName(input, pos).flatMap { case (_r4554, _p4555) => parseInlineSpacing(input, _p4555).map { case (_r4556, _p4557) => (new ~(_r4554, _r4556), _p4557) } })).orElse(parseMethodIdentifierRaw(input, pos).flatMap { case (_r4558, _p4559) => parseInlineSpacing(input, _p4559).map { case (_r4560, _p4561) => (new ~(_r4558, _r4560), _p4561) } })).orElse(parseGlobalVarName(input, pos)).map { case (r, p) => (_applyAction({  _ => SymbolLiteral("", UnknownSpan)  }, r), p) }

  def parseAliasStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("alias", pos)) Some(("alias", pos + 5)) else None).flatMap { case (_r4578, _p4579) => (if (parseIdentCont(input, _p4579).isEmpty) Some(((), _p4579)) else None).map { case (_r4580, _p4581) => (new ~(_r4578, _r4580), _p4581) } }.flatMap { case (_r4574, _p4575) => parseSpacing(input, _p4575).map { case (_r4576, _p4577) => (new ~(_r4574, _r4576), _p4577) } }.flatMap { case (_r4570, _p4571) => parseAliasNamePart(input, _p4571).map { case (_r4572, _p4573) => (new ~(_r4570, _r4572), _p4573) } }.flatMap { case (_r4566, _p4567) => parseSpacing(input, _p4567).map { case (_r4568, _p4569) => (new ~(_r4566, _r4568), _p4569) } }.flatMap { case (_r4562, _p4563) => parseAliasNamePart(input, _p4563).map { case (_r4564, _p4565) => (new ~(_r4562, _r4564), _p4565) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseDefReceiverPart(input: String, pos: Int): Option[(Any, Int)] = ((((((((((if (input.startsWith("(", pos)) Some(("(", pos + 1)) else None).flatMap { case (_r4594, _p4595) => parseSpacing(input, _p4595).map { case (_r4596, _p4597) => (new ~(_r4594, _r4596), _p4597) } }.flatMap { case (_r4590, _p4591) => parseExpr(input, _p4591).map { case (_r4592, _p4593) => (new ~(_r4590, _r4592), _p4593) } }.flatMap { case (_r4586, _p4587) => parseSpacing(input, _p4587).map { case (_r4588, _p4589) => (new ~(_r4586, _r4588), _p4589) } }.flatMap { case (_r4582, _p4583) => (if (input.startsWith(")", _p4583)) Some((")", _p4583 + 1)) else None).map { case (_r4584, _p4585) => (new ~(_r4582, _r4584), _p4585) } }).orElse(parseConstPathNoSpace(input, pos).flatMap { case (_r4598, _p4599) => parseInlineSpacing(input, _p4599).map { case (_r4600, _p4601) => (new ~(_r4598, _r4600), _p4601) } })).orElse((if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r4606, _p4607) => (if (parseIdentCont(input, _p4607).isEmpty) Some(((), _p4607)) else None).map { case (_r4608, _p4609) => (new ~(_r4606, _r4608), _p4609) } }.flatMap { case (_r4602, _p4603) => parseInlineSpacing(input, _p4603).map { case (_r4604, _p4605) => (new ~(_r4602, _r4604), _p4605) } })).orElse((if (input.startsWith("nil", pos)) Some(("nil", pos + 3)) else None).flatMap { case (_r4614, _p4615) => (if (parseIdentCont(input, _p4615).isEmpty) Some(((), _p4615)) else None).map { case (_r4616, _p4617) => (new ~(_r4614, _r4616), _p4617) } }.flatMap { case (_r4610, _p4611) => parseInlineSpacing(input, _p4611).map { case (_r4612, _p4613) => (new ~(_r4610, _r4612), _p4613) } })).orElse((if (input.startsWith("true", pos)) Some(("true", pos + 4)) else None).flatMap { case (_r4622, _p4623) => (if (parseIdentCont(input, _p4623).isEmpty) Some(((), _p4623)) else None).map { case (_r4624, _p4625) => (new ~(_r4622, _r4624), _p4625) } }.flatMap { case (_r4618, _p4619) => parseInlineSpacing(input, _p4619).map { case (_r4620, _p4621) => (new ~(_r4618, _r4620), _p4621) } })).orElse((if (input.startsWith("false", pos)) Some(("false", pos + 5)) else None).flatMap { case (_r4630, _p4631) => (if (parseIdentCont(input, _p4631).isEmpty) Some(((), _p4631)) else None).map { case (_r4632, _p4633) => (new ~(_r4630, _r4632), _p4633) } }.flatMap { case (_r4626, _p4627) => parseInlineSpacing(input, _p4627).map { case (_r4628, _p4629) => (new ~(_r4626, _r4628), _p4629) } })).orElse(parseInstanceVarName(input, pos))).orElse(parseClassVarName(input, pos))).orElse(parseGlobalVarName(input, pos))).orElse(parseIdentifier(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDefMethodNamePart(input: String, pos: Int): Option[(Any, Int)] = (((parseSymbolOperatorName(input, pos)).orElse((if (input.startsWith("`", pos)) Some(("`", pos + 1)) else None).flatMap { case (_r4642, _p4643) => {
  var _rs4646: List[Any] = Nil; var _cp4647: Int = _p4643; var _go4650 = true
  while (_go4650) { (if ((if (input.startsWith("`", _cp4647)) Some(("`", _cp4647 + 1)) else None).isEmpty) Some(((), _cp4647)) else None).flatMap { case (_r4651, _p4652) => (if (_p4652 < input.length) Some((input.charAt(_p4652).toString, _p4652 + 1)) else None).map { case (_r4653, _p4654) => (new ~(_r4651, _r4653), _p4654) } } match {
    case Some((_st4648, _np4649)) => _rs4646 = _rs4646 :+ _st4648; _cp4647 = _np4649
    case None => _go4650 = false } }
  Some((_rs4646, _cp4647)) }.map { case (_r4644, _p4645) => (new ~(_r4642, _r4644), _p4645) } }.flatMap { case (_r4638, _p4639) => (if (input.startsWith("`", _p4639)) Some(("`", _p4639 + 1)) else None).map { case (_r4640, _p4641) => (new ~(_r4638, _r4640), _p4641) } })).orElse(parseMethodIdentifierRaw(input, pos))).orElse(((((((((((if (input.startsWith("private", pos)) Some(("private", pos + 7)) else None).flatMap { case (_r4655, _p4656) => (if (parseIdentCont(input, _p4656).isEmpty) Some(((), _p4656)) else None).map { case (_r4657, _p4658) => (new ~(_r4655, _r4657), _p4658) } }).orElse((if (input.startsWith("public", pos)) Some(("public", pos + 6)) else None).flatMap { case (_r4659, _p4660) => (if (parseIdentCont(input, _p4660).isEmpty) Some(((), _p4660)) else None).map { case (_r4661, _p4662) => (new ~(_r4659, _r4661), _p4662) } })).orElse((if (input.startsWith("protected", pos)) Some(("protected", pos + 9)) else None).flatMap { case (_r4663, _p4664) => (if (parseIdentCont(input, _p4664).isEmpty) Some(((), _p4664)) else None).map { case (_r4665, _p4666) => (new ~(_r4663, _r4665), _p4666) } })).orElse((if (input.startsWith("ruby2_keywords", pos)) Some(("ruby2_keywords", pos + 14)) else None).flatMap { case (_r4667, _p4668) => (if (parseIdentCont(input, _p4668).isEmpty) Some(((), _p4668)) else None).map { case (_r4669, _p4670) => (new ~(_r4667, _r4669), _p4670) } })).orElse((if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r4671, _p4672) => (if (parseIdentCont(input, _p4672).isEmpty) Some(((), _p4672)) else None).map { case (_r4673, _p4674) => (new ~(_r4671, _r4673), _p4674) } })).orElse((if (input.startsWith("def", pos)) Some(("def", pos + 3)) else None).flatMap { case (_r4675, _p4676) => (if (parseIdentCont(input, _p4676).isEmpty) Some(((), _p4676)) else None).map { case (_r4677, _p4678) => (new ~(_r4675, _r4677), _p4678) } })).orElse((if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r4679, _p4680) => (if (parseIdentCont(input, _p4680).isEmpty) Some(((), _p4680)) else None).map { case (_r4681, _p4682) => (new ~(_r4679, _r4681), _p4682) } })).orElse((if (input.startsWith("end", pos)) Some(("end", pos + 3)) else None).flatMap { case (_r4683, _p4684) => (if (parseIdentCont(input, _p4684).isEmpty) Some(((), _p4684)) else None).map { case (_r4685, _p4686) => (new ~(_r4683, _r4685), _p4686) } })).orElse((if (input.startsWith("for", pos)) Some(("for", pos + 3)) else None).flatMap { case (_r4687, _p4688) => (if (parseIdentCont(input, _p4688).isEmpty) Some(((), _p4688)) else None).map { case (_r4689, _p4690) => (new ~(_r4687, _r4689), _p4690) } })).orElse((if (input.startsWith("self", pos)) Some(("self", pos + 4)) else None).flatMap { case (_r4691, _p4692) => (if (parseIdentCont(input, _p4692).isEmpty) Some(((), _p4692)) else None).map { case (_r4693, _p4694) => (new ~(_r4691, _r4693), _p4694) } })).flatMap { case (_r4634, _p4635) => parseInlineSpacing(input, _p4635).map { case (_r4636, _p4637) => (new ~(_r4634, _r4636), _p4637) } }.map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDefNamePart(input: String, pos: Int): Option[(Any, Int)] = (parseDefReceiverPart(input, pos).flatMap { case (_r4703, _p4704) => (if (input.startsWith(".", _p4704)) Some((".", _p4704 + 1)) else None).map { case (_r4705, _p4706) => (new ~(_r4703, _r4705), _p4706) } }.flatMap { case (_r4699, _p4700) => parseSpacing(input, _p4700).map { case (_r4701, _p4702) => (new ~(_r4699, _r4701), _p4702) } }.flatMap { case (_r4695, _p4696) => parseDefMethodNamePart(input, _p4696).map { case (_r4697, _p4698) => (new ~(_r4695, _r4697), _p4698) } }).orElse(parseDefMethodNamePart(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseDefStmt(input: String, pos: Int): Option[(Any, Int)] = (((((if (input.startsWith("private", pos)) Some(("private", pos + 7)) else None).flatMap { case (_r4739, _p4740) => (if (parseIdentCont(input, _p4740).isEmpty) Some(((), _p4740)) else None).map { case (_r4741, _p4742) => (new ~(_r4739, _r4741), _p4742) } }.flatMap { case (_r4735, _p4736) => parseSpacing(input, _p4736).map { case (_r4737, _p4738) => (new ~(_r4735, _r4737), _p4738) } }).orElse((if (input.startsWith("public", pos)) Some(("public", pos + 6)) else None).flatMap { case (_r4747, _p4748) => (if (parseIdentCont(input, _p4748).isEmpty) Some(((), _p4748)) else None).map { case (_r4749, _p4750) => (new ~(_r4747, _r4749), _p4750) } }.flatMap { case (_r4743, _p4744) => parseSpacing(input, _p4744).map { case (_r4745, _p4746) => (new ~(_r4743, _r4745), _p4746) } })).orElse((if (input.startsWith("protected", pos)) Some(("protected", pos + 9)) else None).flatMap { case (_r4755, _p4756) => (if (parseIdentCont(input, _p4756).isEmpty) Some(((), _p4756)) else None).map { case (_r4757, _p4758) => (new ~(_r4755, _r4757), _p4758) } }.flatMap { case (_r4751, _p4752) => parseSpacing(input, _p4752).map { case (_r4753, _p4754) => (new ~(_r4751, _r4753), _p4754) } })).orElse((if (input.startsWith("ruby2_keywords", pos)) Some(("ruby2_keywords", pos + 14)) else None).flatMap { case (_r4763, _p4764) => (if (parseIdentCont(input, _p4764).isEmpty) Some(((), _p4764)) else None).map { case (_r4765, _p4766) => (new ~(_r4763, _r4765), _p4766) } }.flatMap { case (_r4759, _p4760) => parseSpacing(input, _p4760).map { case (_r4761, _p4762) => (new ~(_r4759, _r4761), _p4762) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, pos)))).flatMap { case (_r4731, _p4732) => (if (input.startsWith("def", _p4732)) Some(("def", _p4732 + 3)) else None).map { case (_r4733, _p4734) => (new ~(_r4731, _r4733), _p4734) } }.flatMap { case (_r4727, _p4728) => (if (parseIdentCont(input, _p4728).isEmpty) Some(((), _p4728)) else None).map { case (_r4729, _p4730) => (new ~(_r4727, _r4729), _p4730) } }.flatMap { case (_r4723, _p4724) => parseSpacing(input, _p4724).map { case (_r4725, _p4726) => (new ~(_r4723, _r4725), _p4726) } }.flatMap { case (_r4719, _p4720) => parseDefNamePart(input, _p4720).map { case (_r4721, _p4722) => (new ~(_r4719, _r4721), _p4722) } }.flatMap { case (_r4715, _p4716) => ((parseParams(input, _p4716)).orElse(parseSpacing1(input, _p4716).flatMap { case (_r4767, _p4768) => parseBareParams(input, _p4768).map { case (_r4769, _p4770) => (new ~(_r4767, _r4769), _p4770) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4716)))).map { case (_r4717, _p4718) => (new ~(_r4715, _r4717), _p4718) } }.flatMap { case (_r4711, _p4712) => parseSpacing(input, _p4712).map { case (_r4713, _p4714) => (new ~(_r4711, _r4713), _p4714) } }.flatMap { case (_r4707, _p4708) => (parseAssignEq(input, _p4708).flatMap { case (_r4775, _p4776) => parseSpacing(input, _p4776).map { case (_r4777, _p4778) => (new ~(_r4775, _r4777), _p4778) } }.flatMap { case (_r4771, _p4772) => parseExpr(input, _p4772).map { case (_r4773, _p4774) => (new ~(_r4771, _r4773), _p4774) } }).orElse({
  var _rs4807: List[Any] = Nil; var _cp4808: Int = _p4708; var _go4811 = true
  while (_go4811) { parseStatementSep(input, _cp4808) match {
    case Some((_st4809, _np4810)) => _rs4807 = _rs4807 :+ _st4809; _cp4808 = _np4810
    case None => _go4811 = false } }
  Some((_rs4807, _cp4808)) }.flatMap { case (_r4803, _p4804) => {
  var _rs4812: List[Any] = Nil; var _cp4813: Int = _p4804; var _go4816 = true
  while (_go4816) { (if ((parseRescueStop(input, _cp4813)).orElse(parseEndKeyword(input, _cp4813)).isEmpty) Some(((), _cp4813)) else None).flatMap { case (_r4821, _p4822) => parseStatement(input, _p4822).map { case (_r4823, _p4824) => (new ~(_r4821, _r4823), _p4824) } }.flatMap { case (_r4817, _p4818) => {
  var _rs4825: List[Any] = Nil; var _cp4826: Int = _p4818; var _go4829 = true
  while (_go4829) { parseStatementSep(input, _cp4826) match {
    case Some((_st4827, _np4828)) => _rs4825 = _rs4825 :+ _st4827; _cp4826 = _np4828
    case None => _go4829 = false } }
  Some((_rs4825, _cp4826)) }.map { case (_r4819, _p4820) => (new ~(_r4817, _r4819), _p4820) } } match {
    case Some((_st4814, _np4815)) => _rs4812 = _rs4812 :+ _st4814; _cp4813 = _np4815
    case None => _go4816 = false } }
  Some((_rs4812, _cp4813)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4805, _p4806) => (new ~(_r4803, _r4805), _p4806) } }.flatMap { case (_r4799, _p4800) => {
  var _rs4830: List[Any] = Nil; var _cp4831: Int = _p4800; var _go4834 = true
  while (_go4834) { parseStatementSep(input, _cp4831) match {
    case Some((_st4832, _np4833)) => _rs4830 = _rs4830 :+ _st4832; _cp4831 = _np4833
    case None => _go4834 = false } }
  Some((_rs4830, _cp4831)) }.map { case (_r4801, _p4802) => (new ~(_r4799, _r4801), _p4802) } }.flatMap { case (_r4795, _p4796) => {
  var _rs4835: List[Any] = Nil; var _cp4836: Int = _p4796; var _go4839 = true
  while (_go4839) { parseRescueClause(input, _cp4836) match {
    case Some((_st4837, _np4838)) => _rs4835 = _rs4835 :+ _st4837; _cp4836 = _np4838
    case None => _go4839 = false } }
  Some((_rs4835, _cp4836)) }.map { case (_r4797, _p4798) => (new ~(_r4795, _r4797), _p4798) } }.flatMap { case (_r4791, _p4792) => {
  var _rs4840: List[Any] = Nil; var _cp4841: Int = _p4792; var _go4844 = true
  while (_go4844) { parseStatementSep(input, _cp4841) match {
    case Some((_st4842, _np4843)) => _rs4840 = _rs4840 :+ _st4842; _cp4841 = _np4843
    case None => _go4844 = false } }
  Some((_rs4840, _cp4841)) }.map { case (_r4793, _p4794) => (new ~(_r4791, _r4793), _p4794) } }.flatMap { case (_r4787, _p4788) => ((if (input.startsWith("else", _p4788)) Some(("else", _p4788 + 4)) else None).flatMap { case (_r4861, _p4862) => (if (parseIdentCont(input, _p4862).isEmpty) Some(((), _p4862)) else None).map { case (_r4863, _p4864) => (new ~(_r4861, _r4863), _p4864) } }.flatMap { case (_r4857, _p4858) => parseSpacing(input, _p4858).map { case (_r4859, _p4860) => (new ~(_r4857, _r4859), _p4860) } }.flatMap { case (_r4853, _p4854) => {
  var _rs4865: List[Any] = Nil; var _cp4866: Int = _p4854; var _go4869 = true
  while (_go4869) { parseStatementSep(input, _cp4866) match {
    case Some((_st4867, _np4868)) => _rs4865 = _rs4865 :+ _st4867; _cp4866 = _np4868
    case None => _go4869 = false } }
  Some((_rs4865, _cp4866)) }.map { case (_r4855, _p4856) => (new ~(_r4853, _r4855), _p4856) } }.flatMap { case (_r4849, _p4850) => {
  var _rs4870: List[Any] = Nil; var _cp4871: Int = _p4850; var _go4874 = true
  while (_go4874) { (if (parseDoBlockStop(input, _cp4871).isEmpty) Some(((), _cp4871)) else None).flatMap { case (_r4879, _p4880) => parseStatement(input, _p4880).map { case (_r4881, _p4882) => (new ~(_r4879, _r4881), _p4882) } }.flatMap { case (_r4875, _p4876) => {
  var _rs4883: List[Any] = Nil; var _cp4884: Int = _p4876; var _go4887 = true
  while (_go4887) { parseStatementSep(input, _cp4884) match {
    case Some((_st4885, _np4886)) => _rs4883 = _rs4883 :+ _st4885; _cp4884 = _np4886
    case None => _go4887 = false } }
  Some((_rs4883, _cp4884)) }.map { case (_r4877, _p4878) => (new ~(_r4875, _r4877), _p4878) } } match {
    case Some((_st4872, _np4873)) => _rs4870 = _rs4870 :+ _st4872; _cp4871 = _np4873
    case None => _go4874 = false } }
  Some((_rs4870, _cp4871)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4851, _p4852) => (new ~(_r4849, _r4851), _p4852) } }.flatMap { case (_r4845, _p4846) => {
  var _rs4888: List[Any] = Nil; var _cp4889: Int = _p4846; var _go4892 = true
  while (_go4892) { parseStatementSep(input, _cp4889) match {
    case Some((_st4890, _np4891)) => _rs4888 = _rs4888 :+ _st4890; _cp4889 = _np4891
    case None => _go4892 = false } }
  Some((_rs4888, _cp4889)) }.map { case (_r4847, _p4848) => (new ~(_r4845, _r4847), _p4848) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4788)))).map { case (_r4789, _p4790) => (new ~(_r4787, _r4789), _p4790) } }.flatMap { case (_r4783, _p4784) => ((if (input.startsWith("ensure", _p4784)) Some(("ensure", _p4784 + 6)) else None).flatMap { case (_r4909, _p4910) => (if (parseIdentCont(input, _p4910).isEmpty) Some(((), _p4910)) else None).map { case (_r4911, _p4912) => (new ~(_r4909, _r4911), _p4912) } }.flatMap { case (_r4905, _p4906) => parseSpacing(input, _p4906).map { case (_r4907, _p4908) => (new ~(_r4905, _r4907), _p4908) } }.flatMap { case (_r4901, _p4902) => {
  var _rs4913: List[Any] = Nil; var _cp4914: Int = _p4902; var _go4917 = true
  while (_go4917) { parseStatementSep(input, _cp4914) match {
    case Some((_st4915, _np4916)) => _rs4913 = _rs4913 :+ _st4915; _cp4914 = _np4916
    case None => _go4917 = false } }
  Some((_rs4913, _cp4914)) }.map { case (_r4903, _p4904) => (new ~(_r4901, _r4903), _p4904) } }.flatMap { case (_r4897, _p4898) => {
  var _rs4918: List[Any] = Nil; var _cp4919: Int = _p4898; var _go4922 = true
  while (_go4922) { (if (parseEndKeyword(input, _cp4919).isEmpty) Some(((), _cp4919)) else None).flatMap { case (_r4927, _p4928) => parseStatement(input, _p4928).map { case (_r4929, _p4930) => (new ~(_r4927, _r4929), _p4930) } }.flatMap { case (_r4923, _p4924) => {
  var _rs4931: List[Any] = Nil; var _cp4932: Int = _p4924; var _go4935 = true
  while (_go4935) { parseStatementSep(input, _cp4932) match {
    case Some((_st4933, _np4934)) => _rs4931 = _rs4931 :+ _st4933; _cp4932 = _np4934
    case None => _go4935 = false } }
  Some((_rs4931, _cp4932)) }.map { case (_r4925, _p4926) => (new ~(_r4923, _r4925), _p4926) } } match {
    case Some((_st4920, _np4921)) => _rs4918 = _rs4918 :+ _st4920; _cp4919 = _np4921
    case None => _go4922 = false } }
  Some((_rs4918, _cp4919)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4899, _p4900) => (new ~(_r4897, _r4899), _p4900) } }.flatMap { case (_r4893, _p4894) => {
  var _rs4936: List[Any] = Nil; var _cp4937: Int = _p4894; var _go4940 = true
  while (_go4940) { parseStatementSep(input, _cp4937) match {
    case Some((_st4938, _np4939)) => _rs4936 = _rs4936 :+ _st4938; _cp4937 = _np4939
    case None => _go4940 = false } }
  Some((_rs4936, _cp4937)) }.map { case (_r4895, _p4896) => (new ~(_r4893, _r4895), _p4896) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p4784)))).map { case (_r4785, _p4786) => (new ~(_r4783, _r4785), _p4786) } }.flatMap { case (_r4779, _p4780) => parseEndKeyword(input, _p4780).map { case (_r4781, _p4782) => (new ~(_r4779, _r4781), _p4782) } }).map { case (_r4709, _p4710) => (new ~(_r4707, _r4709), _p4710) } }.map { case (r, p) => (_applyAction({  _ => Def("", List.empty, List.empty)  }, r), p) }

  def parseSingletonClassStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r4977, _p4978) => (if (parseIdentCont(input, _p4978).isEmpty) Some(((), _p4978)) else None).map { case (_r4979, _p4980) => (new ~(_r4977, _r4979), _p4980) } }.flatMap { case (_r4973, _p4974) => parseSpacing(input, _p4974).map { case (_r4975, _p4976) => (new ~(_r4973, _r4975), _p4976) } }.flatMap { case (_r4969, _p4970) => (if (input.startsWith("<<", _p4970)) Some(("<<", _p4970 + 2)) else None).map { case (_r4971, _p4972) => (new ~(_r4969, _r4971), _p4972) } }.flatMap { case (_r4965, _p4966) => parseSpacing(input, _p4966).map { case (_r4967, _p4968) => (new ~(_r4965, _r4967), _p4968) } }.flatMap { case (_r4961, _p4962) => parseExpr(input, _p4962).map { case (_r4963, _p4964) => (new ~(_r4961, _r4963), _p4964) } }.flatMap { case (_r4957, _p4958) => {
  var _rs4981: List[Any] = Nil; var _cp4982: Int = _p4958; var _go4985 = true
  while (_go4985) { parseStatementSep(input, _cp4982) match {
    case Some((_st4983, _np4984)) => _rs4981 = _rs4981 :+ _st4983; _cp4982 = _np4984
    case None => _go4985 = false } }
  Some((_rs4981, _cp4982)) }.map { case (_r4959, _p4960) => (new ~(_r4957, _r4959), _p4960) } }.flatMap { case (_r4953, _p4954) => {
  var _rs4986: List[Any] = Nil; var _cp4987: Int = _p4954; var _go4990 = true
  while (_go4990) { (if (parseEndKeyword(input, _cp4987).isEmpty) Some(((), _cp4987)) else None).flatMap { case (_r4995, _p4996) => parseStatement(input, _p4996).map { case (_r4997, _p4998) => (new ~(_r4995, _r4997), _p4998) } }.flatMap { case (_r4991, _p4992) => {
  var _rs4999: List[Any] = Nil; var _cp5000: Int = _p4992; var _go5003 = true
  while (_go5003) { parseStatementSep(input, _cp5000) match {
    case Some((_st5001, _np5002)) => _rs4999 = _rs4999 :+ _st5001; _cp5000 = _np5002
    case None => _go5003 = false } }
  Some((_rs4999, _cp5000)) }.map { case (_r4993, _p4994) => (new ~(_r4991, _r4993), _p4994) } } match {
    case Some((_st4988, _np4989)) => _rs4986 = _rs4986 :+ _st4988; _cp4987 = _np4989
    case None => _go4990 = false } }
  Some((_rs4986, _cp4987)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r4955, _p4956) => (new ~(_r4953, _r4955), _p4956) } }.flatMap { case (_r4949, _p4950) => {
  var _rs5004: List[Any] = Nil; var _cp5005: Int = _p4950; var _go5008 = true
  while (_go5008) { parseStatementSep(input, _cp5005) match {
    case Some((_st5006, _np5007)) => _rs5004 = _rs5004 :+ _st5006; _cp5005 = _np5007
    case None => _go5008 = false } }
  Some((_rs5004, _cp5005)) }.map { case (_r4951, _p4952) => (new ~(_r4949, _r4951), _p4952) } }.flatMap { case (_r4945, _p4946) => parseEndKeyword(input, _p4946).map { case (_r4947, _p4948) => (new ~(_r4945, _r4947), _p4948) } }.flatMap { case (_r4941, _p4942) => {
  var _rs5009: List[Any] = Nil; var _cp5010: Int = _p4942; var _go5013 = true
  while (_go5013) { parseCallSuffix(input, _cp5010) match {
    case Some((_st5011, _np5012)) => _rs5009 = _rs5009 :+ _st5011; _cp5010 = _np5012
    case None => _go5013 = false } }
  Some((_rs5009, _cp5010)) }.map { case (_r4943, _p4944) => (new ~(_r4941, _r4943), _p4944) } }.map { case (r, p) => (_applyAction({  _ => SingletonClassDef(NilLiteral(), List.empty)  }, r), p) }

  def parseClassStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("class", pos)) Some(("class", pos + 5)) else None).flatMap { case (_r5042, _p5043) => (if (parseIdentCont(input, _p5043).isEmpty) Some(((), _p5043)) else None).map { case (_r5044, _p5045) => (new ~(_r5042, _r5044), _p5045) } }.flatMap { case (_r5038, _p5039) => parseSpacing(input, _p5039).map { case (_r5040, _p5041) => (new ~(_r5038, _r5040), _p5041) } }.flatMap { case (_r5034, _p5035) => parseConstPath(input, _p5035).map { case (_r5036, _p5037) => (new ~(_r5034, _r5036), _p5037) } }.flatMap { case (_r5030, _p5031) => ((if (input.startsWith("<", _p5031)) Some(("<", _p5031 + 1)) else None).flatMap { case (_r5054, _p5055) => (if ((if (input.startsWith("<", _p5055)) Some(("<", _p5055 + 1)) else None).isEmpty) Some(((), _p5055)) else None).map { case (_r5056, _p5057) => (new ~(_r5054, _r5056), _p5057) } }.flatMap { case (_r5050, _p5051) => parseSpacing(input, _p5051).map { case (_r5052, _p5053) => (new ~(_r5050, _r5052), _p5053) } }.flatMap { case (_r5046, _p5047) => parseExpr(input, _p5047).map { case (_r5048, _p5049) => (new ~(_r5046, _r5048), _p5049) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5031)))).map { case (_r5032, _p5033) => (new ~(_r5030, _r5032), _p5033) } }.flatMap { case (_r5026, _p5027) => {
  var _rs5058: List[Any] = Nil; var _cp5059: Int = _p5027; var _go5062 = true
  while (_go5062) { parseStatementSep(input, _cp5059) match {
    case Some((_st5060, _np5061)) => _rs5058 = _rs5058 :+ _st5060; _cp5059 = _np5061
    case None => _go5062 = false } }
  Some((_rs5058, _cp5059)) }.map { case (_r5028, _p5029) => (new ~(_r5026, _r5028), _p5029) } }.flatMap { case (_r5022, _p5023) => {
  var _rs5063: List[Any] = Nil; var _cp5064: Int = _p5023; var _go5067 = true
  while (_go5067) { (if (parseEndKeyword(input, _cp5064).isEmpty) Some(((), _cp5064)) else None).flatMap { case (_r5072, _p5073) => parseStatement(input, _p5073).map { case (_r5074, _p5075) => (new ~(_r5072, _r5074), _p5075) } }.flatMap { case (_r5068, _p5069) => {
  var _rs5076: List[Any] = Nil; var _cp5077: Int = _p5069; var _go5080 = true
  while (_go5080) { parseStatementSep(input, _cp5077) match {
    case Some((_st5078, _np5079)) => _rs5076 = _rs5076 :+ _st5078; _cp5077 = _np5079
    case None => _go5080 = false } }
  Some((_rs5076, _cp5077)) }.map { case (_r5070, _p5071) => (new ~(_r5068, _r5070), _p5071) } } match {
    case Some((_st5065, _np5066)) => _rs5063 = _rs5063 :+ _st5065; _cp5064 = _np5066
    case None => _go5067 = false } }
  Some((_rs5063, _cp5064)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5024, _p5025) => (new ~(_r5022, _r5024), _p5025) } }.flatMap { case (_r5018, _p5019) => {
  var _rs5081: List[Any] = Nil; var _cp5082: Int = _p5019; var _go5085 = true
  while (_go5085) { parseStatementSep(input, _cp5082) match {
    case Some((_st5083, _np5084)) => _rs5081 = _rs5081 :+ _st5083; _cp5082 = _np5084
    case None => _go5085 = false } }
  Some((_rs5081, _cp5082)) }.map { case (_r5020, _p5021) => (new ~(_r5018, _r5020), _p5021) } }.flatMap { case (_r5014, _p5015) => parseEndKeyword(input, _p5015).map { case (_r5016, _p5017) => (new ~(_r5014, _r5016), _p5017) } }.map { case (r, p) => (_applyAction({  _ => ClassDef("", List.empty)  }, r), p) }

  def parseModuleStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("module", pos)) Some(("module", pos + 6)) else None).flatMap { case (_r5110, _p5111) => (if (parseIdentCont(input, _p5111).isEmpty) Some(((), _p5111)) else None).map { case (_r5112, _p5113) => (new ~(_r5110, _r5112), _p5113) } }.flatMap { case (_r5106, _p5107) => parseSpacing(input, _p5107).map { case (_r5108, _p5109) => (new ~(_r5106, _r5108), _p5109) } }.flatMap { case (_r5102, _p5103) => parseConstPath(input, _p5103).map { case (_r5104, _p5105) => (new ~(_r5102, _r5104), _p5105) } }.flatMap { case (_r5098, _p5099) => {
  var _rs5114: List[Any] = Nil; var _cp5115: Int = _p5099; var _go5118 = true
  while (_go5118) { parseStatementSep(input, _cp5115) match {
    case Some((_st5116, _np5117)) => _rs5114 = _rs5114 :+ _st5116; _cp5115 = _np5117
    case None => _go5118 = false } }
  Some((_rs5114, _cp5115)) }.map { case (_r5100, _p5101) => (new ~(_r5098, _r5100), _p5101) } }.flatMap { case (_r5094, _p5095) => {
  var _rs5119: List[Any] = Nil; var _cp5120: Int = _p5095; var _go5123 = true
  while (_go5123) { (if (parseEndKeyword(input, _cp5120).isEmpty) Some(((), _cp5120)) else None).flatMap { case (_r5128, _p5129) => parseStatement(input, _p5129).map { case (_r5130, _p5131) => (new ~(_r5128, _r5130), _p5131) } }.flatMap { case (_r5124, _p5125) => {
  var _rs5132: List[Any] = Nil; var _cp5133: Int = _p5125; var _go5136 = true
  while (_go5136) { parseStatementSep(input, _cp5133) match {
    case Some((_st5134, _np5135)) => _rs5132 = _rs5132 :+ _st5134; _cp5133 = _np5135
    case None => _go5136 = false } }
  Some((_rs5132, _cp5133)) }.map { case (_r5126, _p5127) => (new ~(_r5124, _r5126), _p5127) } } match {
    case Some((_st5121, _np5122)) => _rs5119 = _rs5119 :+ _st5121; _cp5120 = _np5122
    case None => _go5123 = false } }
  Some((_rs5119, _cp5120)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5096, _p5097) => (new ~(_r5094, _r5096), _p5097) } }.flatMap { case (_r5090, _p5091) => {
  var _rs5137: List[Any] = Nil; var _cp5138: Int = _p5091; var _go5141 = true
  while (_go5141) { parseStatementSep(input, _cp5138) match {
    case Some((_st5139, _np5140)) => _rs5137 = _rs5137 :+ _st5139; _cp5138 = _np5140
    case None => _go5141 = false } }
  Some((_rs5137, _cp5138)) }.map { case (_r5092, _p5093) => (new ~(_r5090, _r5092), _p5093) } }.flatMap { case (_r5086, _p5087) => parseEndKeyword(input, _p5087).map { case (_r5088, _p5089) => (new ~(_r5086, _r5088), _p5089) } }.map { case (r, p) => (_applyAction({  _ => ModuleDef("", List.empty)  }, r), p) }

  def parseWhileStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("while", pos)) Some(("while", pos + 5)) else None).flatMap { case (_r5170, _p5171) => (if (parseIdentCont(input, _p5171).isEmpty) Some(((), _p5171)) else None).map { case (_r5172, _p5173) => (new ~(_r5170, _r5172), _p5173) } }.flatMap { case (_r5166, _p5167) => parseSpacing(input, _p5167).map { case (_r5168, _p5169) => (new ~(_r5166, _r5168), _p5169) } }.flatMap { case (_r5162, _p5163) => parseExpr(input, _p5163).map { case (_r5164, _p5165) => (new ~(_r5162, _r5164), _p5165) } }.flatMap { case (_r5158, _p5159) => ((((if (input.startsWith("do", _p5159)) Some(("do", _p5159 + 2)) else None).flatMap { case (_r5178, _p5179) => (if (parseIdentCont(input, _p5179).isEmpty) Some(((), _p5179)) else None).map { case (_r5180, _p5181) => (new ~(_r5178, _r5180), _p5181) } }).orElse((if (input.startsWith("then", _p5159)) Some(("then", _p5159 + 4)) else None).flatMap { case (_r5182, _p5183) => (if (parseIdentCont(input, _p5183).isEmpty) Some(((), _p5183)) else None).map { case (_r5184, _p5185) => (new ~(_r5182, _r5184), _p5185) } }).flatMap { case (_r5174, _p5175) => parseSpacing(input, _p5175).map { case (_r5176, _p5177) => (new ~(_r5174, _r5176), _p5177) } }).orElse(parseInlineSpacing(input, _p5159).flatMap { case (_r5186, _p5187) => {
  parseStatementSep(input, _p5187) match {
    case None => None
    case Some((_fs5190, _fp5191)) =>
      var _rs5192: List[Any] = List(_fs5190); var _cp5193: Int = _fp5191; var _go5196 = true
      while (_go5196) { parseStatementSep(input, _cp5193) match {
        case Some((_st5194, _np5195)) => _rs5192 = _rs5192 :+ _st5194; _cp5193 = _np5195
        case None => _go5196 = false } }
      Some((_rs5192, _cp5193)) } }.map { case (_r5188, _p5189) => (new ~(_r5186, _r5188), _p5189) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5159)))).map { case (_r5160, _p5161) => (new ~(_r5158, _r5160), _p5161) } }.flatMap { case (_r5154, _p5155) => {
  var _rs5197: List[Any] = Nil; var _cp5198: Int = _p5155; var _go5201 = true
  while (_go5201) { parseStatementSep(input, _cp5198) match {
    case Some((_st5199, _np5200)) => _rs5197 = _rs5197 :+ _st5199; _cp5198 = _np5200
    case None => _go5201 = false } }
  Some((_rs5197, _cp5198)) }.map { case (_r5156, _p5157) => (new ~(_r5154, _r5156), _p5157) } }.flatMap { case (_r5150, _p5151) => {
  var _rs5202: List[Any] = Nil; var _cp5203: Int = _p5151; var _go5206 = true
  while (_go5206) { (if (parseEndKeyword(input, _cp5203).isEmpty) Some(((), _cp5203)) else None).flatMap { case (_r5211, _p5212) => parseStatement(input, _p5212).map { case (_r5213, _p5214) => (new ~(_r5211, _r5213), _p5214) } }.flatMap { case (_r5207, _p5208) => {
  var _rs5215: List[Any] = Nil; var _cp5216: Int = _p5208; var _go5219 = true
  while (_go5219) { parseStatementSep(input, _cp5216) match {
    case Some((_st5217, _np5218)) => _rs5215 = _rs5215 :+ _st5217; _cp5216 = _np5218
    case None => _go5219 = false } }
  Some((_rs5215, _cp5216)) }.map { case (_r5209, _p5210) => (new ~(_r5207, _r5209), _p5210) } } match {
    case Some((_st5204, _np5205)) => _rs5202 = _rs5202 :+ _st5204; _cp5203 = _np5205
    case None => _go5206 = false } }
  Some((_rs5202, _cp5203)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5152, _p5153) => (new ~(_r5150, _r5152), _p5153) } }.flatMap { case (_r5146, _p5147) => {
  var _rs5220: List[Any] = Nil; var _cp5221: Int = _p5147; var _go5224 = true
  while (_go5224) { parseStatementSep(input, _cp5221) match {
    case Some((_st5222, _np5223)) => _rs5220 = _rs5220 :+ _st5222; _cp5221 = _np5223
    case None => _go5224 = false } }
  Some((_rs5220, _cp5221)) }.map { case (_r5148, _p5149) => (new ~(_r5146, _r5148), _p5149) } }.flatMap { case (_r5142, _p5143) => parseEndKeyword(input, _p5143).map { case (_r5144, _p5145) => (new ~(_r5142, _r5144), _p5145) } }.map { case (r, p) => (_applyAction({  _ => WhileExpr(NilLiteral(), List.empty)  }, r), p) }

  def parseUntilStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("until", pos)) Some(("until", pos + 5)) else None).flatMap { case (_r5253, _p5254) => (if (parseIdentCont(input, _p5254).isEmpty) Some(((), _p5254)) else None).map { case (_r5255, _p5256) => (new ~(_r5253, _r5255), _p5256) } }.flatMap { case (_r5249, _p5250) => parseSpacing(input, _p5250).map { case (_r5251, _p5252) => (new ~(_r5249, _r5251), _p5252) } }.flatMap { case (_r5245, _p5246) => parseExpr(input, _p5246).map { case (_r5247, _p5248) => (new ~(_r5245, _r5247), _p5248) } }.flatMap { case (_r5241, _p5242) => ((((if (input.startsWith("do", _p5242)) Some(("do", _p5242 + 2)) else None).flatMap { case (_r5261, _p5262) => (if (parseIdentCont(input, _p5262).isEmpty) Some(((), _p5262)) else None).map { case (_r5263, _p5264) => (new ~(_r5261, _r5263), _p5264) } }).orElse((if (input.startsWith("then", _p5242)) Some(("then", _p5242 + 4)) else None).flatMap { case (_r5265, _p5266) => (if (parseIdentCont(input, _p5266).isEmpty) Some(((), _p5266)) else None).map { case (_r5267, _p5268) => (new ~(_r5265, _r5267), _p5268) } }).flatMap { case (_r5257, _p5258) => parseSpacing(input, _p5258).map { case (_r5259, _p5260) => (new ~(_r5257, _r5259), _p5260) } }).orElse(parseInlineSpacing(input, _p5242).flatMap { case (_r5269, _p5270) => {
  parseStatementSep(input, _p5270) match {
    case None => None
    case Some((_fs5273, _fp5274)) =>
      var _rs5275: List[Any] = List(_fs5273); var _cp5276: Int = _fp5274; var _go5279 = true
      while (_go5279) { parseStatementSep(input, _cp5276) match {
        case Some((_st5277, _np5278)) => _rs5275 = _rs5275 :+ _st5277; _cp5276 = _np5278
        case None => _go5279 = false } }
      Some((_rs5275, _cp5276)) } }.map { case (_r5271, _p5272) => (new ~(_r5269, _r5271), _p5272) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5242)))).map { case (_r5243, _p5244) => (new ~(_r5241, _r5243), _p5244) } }.flatMap { case (_r5237, _p5238) => {
  var _rs5280: List[Any] = Nil; var _cp5281: Int = _p5238; var _go5284 = true
  while (_go5284) { parseStatementSep(input, _cp5281) match {
    case Some((_st5282, _np5283)) => _rs5280 = _rs5280 :+ _st5282; _cp5281 = _np5283
    case None => _go5284 = false } }
  Some((_rs5280, _cp5281)) }.map { case (_r5239, _p5240) => (new ~(_r5237, _r5239), _p5240) } }.flatMap { case (_r5233, _p5234) => {
  var _rs5285: List[Any] = Nil; var _cp5286: Int = _p5234; var _go5289 = true
  while (_go5289) { (if (parseEndKeyword(input, _cp5286).isEmpty) Some(((), _cp5286)) else None).flatMap { case (_r5294, _p5295) => parseStatement(input, _p5295).map { case (_r5296, _p5297) => (new ~(_r5294, _r5296), _p5297) } }.flatMap { case (_r5290, _p5291) => {
  var _rs5298: List[Any] = Nil; var _cp5299: Int = _p5291; var _go5302 = true
  while (_go5302) { parseStatementSep(input, _cp5299) match {
    case Some((_st5300, _np5301)) => _rs5298 = _rs5298 :+ _st5300; _cp5299 = _np5301
    case None => _go5302 = false } }
  Some((_rs5298, _cp5299)) }.map { case (_r5292, _p5293) => (new ~(_r5290, _r5292), _p5293) } } match {
    case Some((_st5287, _np5288)) => _rs5285 = _rs5285 :+ _st5287; _cp5286 = _np5288
    case None => _go5289 = false } }
  Some((_rs5285, _cp5286)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5235, _p5236) => (new ~(_r5233, _r5235), _p5236) } }.flatMap { case (_r5229, _p5230) => {
  var _rs5303: List[Any] = Nil; var _cp5304: Int = _p5230; var _go5307 = true
  while (_go5307) { parseStatementSep(input, _cp5304) match {
    case Some((_st5305, _np5306)) => _rs5303 = _rs5303 :+ _st5305; _cp5304 = _np5306
    case None => _go5307 = false } }
  Some((_rs5303, _cp5304)) }.map { case (_r5231, _p5232) => (new ~(_r5229, _r5231), _p5232) } }.flatMap { case (_r5225, _p5226) => parseEndKeyword(input, _p5226).map { case (_r5227, _p5228) => (new ~(_r5225, _r5227), _p5228) } }.map { case (r, p) => (_applyAction({  _ => UntilExpr(NilLiteral(), List.empty)  }, r), p) }

  def parseForBindingTarget(input: String, pos: Int): Option[(Any, Int)] = (parseMultiAssignTargets(input, pos)).orElse(parseAssignableTarget(input, pos)).map { case (r, p) => (_applyAction({  _ => ""  }, r), p) }

  def parseForStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("for", pos)) Some(("for", pos + 3)) else None).flatMap { case (_r5356, _p5357) => (if (parseIdentCont(input, _p5357).isEmpty) Some(((), _p5357)) else None).map { case (_r5358, _p5359) => (new ~(_r5356, _r5358), _p5359) } }.flatMap { case (_r5352, _p5353) => parseSpacing(input, _p5353).map { case (_r5354, _p5355) => (new ~(_r5352, _r5354), _p5355) } }.flatMap { case (_r5348, _p5349) => parseForBindingTarget(input, _p5349).map { case (_r5350, _p5351) => (new ~(_r5348, _r5350), _p5351) } }.flatMap { case (_r5344, _p5345) => parseSpacing(input, _p5345).map { case (_r5346, _p5347) => (new ~(_r5344, _r5346), _p5347) } }.flatMap { case (_r5340, _p5341) => (if (input.startsWith("in", _p5341)) Some(("in", _p5341 + 2)) else None).map { case (_r5342, _p5343) => (new ~(_r5340, _r5342), _p5343) } }.flatMap { case (_r5336, _p5337) => (if (parseIdentCont(input, _p5337).isEmpty) Some(((), _p5337)) else None).map { case (_r5338, _p5339) => (new ~(_r5336, _r5338), _p5339) } }.flatMap { case (_r5332, _p5333) => parseSpacing(input, _p5333).map { case (_r5334, _p5335) => (new ~(_r5332, _r5334), _p5335) } }.flatMap { case (_r5328, _p5329) => parseExpr(input, _p5329).map { case (_r5330, _p5331) => (new ~(_r5328, _r5330), _p5331) } }.flatMap { case (_r5324, _p5325) => ((((if (input.startsWith("do", _p5325)) Some(("do", _p5325 + 2)) else None).flatMap { case (_r5364, _p5365) => (if (parseIdentCont(input, _p5365).isEmpty) Some(((), _p5365)) else None).map { case (_r5366, _p5367) => (new ~(_r5364, _r5366), _p5367) } }).orElse((if (input.startsWith("then", _p5325)) Some(("then", _p5325 + 4)) else None).flatMap { case (_r5368, _p5369) => (if (parseIdentCont(input, _p5369).isEmpty) Some(((), _p5369)) else None).map { case (_r5370, _p5371) => (new ~(_r5368, _r5370), _p5371) } }).flatMap { case (_r5360, _p5361) => parseSpacing(input, _p5361).map { case (_r5362, _p5363) => (new ~(_r5360, _r5362), _p5363) } }).orElse(parseInlineSpacing(input, _p5325).flatMap { case (_r5372, _p5373) => {
  parseStatementSep(input, _p5373) match {
    case None => None
    case Some((_fs5376, _fp5377)) =>
      var _rs5378: List[Any] = List(_fs5376); var _cp5379: Int = _fp5377; var _go5382 = true
      while (_go5382) { parseStatementSep(input, _cp5379) match {
        case Some((_st5380, _np5381)) => _rs5378 = _rs5378 :+ _st5380; _cp5379 = _np5381
        case None => _go5382 = false } }
      Some((_rs5378, _cp5379)) } }.map { case (_r5374, _p5375) => (new ~(_r5372, _r5374), _p5375) } }).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5325)))).map { case (_r5326, _p5327) => (new ~(_r5324, _r5326), _p5327) } }.flatMap { case (_r5320, _p5321) => {
  var _rs5383: List[Any] = Nil; var _cp5384: Int = _p5321; var _go5387 = true
  while (_go5387) { parseStatementSep(input, _cp5384) match {
    case Some((_st5385, _np5386)) => _rs5383 = _rs5383 :+ _st5385; _cp5384 = _np5386
    case None => _go5387 = false } }
  Some((_rs5383, _cp5384)) }.map { case (_r5322, _p5323) => (new ~(_r5320, _r5322), _p5323) } }.flatMap { case (_r5316, _p5317) => {
  var _rs5388: List[Any] = Nil; var _cp5389: Int = _p5317; var _go5392 = true
  while (_go5392) { (if (parseEndKeyword(input, _cp5389).isEmpty) Some(((), _cp5389)) else None).flatMap { case (_r5397, _p5398) => parseStatement(input, _p5398).map { case (_r5399, _p5400) => (new ~(_r5397, _r5399), _p5400) } }.flatMap { case (_r5393, _p5394) => {
  var _rs5401: List[Any] = Nil; var _cp5402: Int = _p5394; var _go5405 = true
  while (_go5405) { parseStatementSep(input, _cp5402) match {
    case Some((_st5403, _np5404)) => _rs5401 = _rs5401 :+ _st5403; _cp5402 = _np5404
    case None => _go5405 = false } }
  Some((_rs5401, _cp5402)) }.map { case (_r5395, _p5396) => (new ~(_r5393, _r5395), _p5396) } } match {
    case Some((_st5390, _np5391)) => _rs5388 = _rs5388 :+ _st5390; _cp5389 = _np5391
    case None => _go5392 = false } }
  Some((_rs5388, _cp5389)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5318, _p5319) => (new ~(_r5316, _r5318), _p5319) } }.flatMap { case (_r5312, _p5313) => {
  var _rs5406: List[Any] = Nil; var _cp5407: Int = _p5313; var _go5410 = true
  while (_go5410) { parseStatementSep(input, _cp5407) match {
    case Some((_st5408, _np5409)) => _rs5406 = _rs5406 :+ _st5408; _cp5407 = _np5409
    case None => _go5410 = false } }
  Some((_rs5406, _cp5407)) }.map { case (_r5314, _p5315) => (new ~(_r5312, _r5314), _p5315) } }.flatMap { case (_r5308, _p5309) => parseEndKeyword(input, _p5309).map { case (_r5310, _p5311) => (new ~(_r5308, _r5310), _p5311) } }.map { case (r, p) => (_applyAction({  _ => ForIn("", NilLiteral(), List.empty)  }, r), p) }

  def parseBeginStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("begin", pos)) Some(("begin", pos + 5)) else None).flatMap { case (_r5447, _p5448) => (if (parseIdentCont(input, _p5448).isEmpty) Some(((), _p5448)) else None).map { case (_r5449, _p5450) => (new ~(_r5447, _r5449), _p5450) } }.flatMap { case (_r5443, _p5444) => parseSpacing(input, _p5444).map { case (_r5445, _p5446) => (new ~(_r5443, _r5445), _p5446) } }.flatMap { case (_r5439, _p5440) => {
  var _rs5451: List[Any] = Nil; var _cp5452: Int = _p5440; var _go5455 = true
  while (_go5455) { parseStatementSep(input, _cp5452) match {
    case Some((_st5453, _np5454)) => _rs5451 = _rs5451 :+ _st5453; _cp5452 = _np5454
    case None => _go5455 = false } }
  Some((_rs5451, _cp5452)) }.map { case (_r5441, _p5442) => (new ~(_r5439, _r5441), _p5442) } }.flatMap { case (_r5435, _p5436) => {
  var _rs5456: List[Any] = Nil; var _cp5457: Int = _p5436; var _go5460 = true
  while (_go5460) { (if ((parseRescueStop(input, _cp5457)).orElse(parseEndKeyword(input, _cp5457)).isEmpty) Some(((), _cp5457)) else None).flatMap { case (_r5465, _p5466) => parseStatement(input, _p5466).map { case (_r5467, _p5468) => (new ~(_r5465, _r5467), _p5468) } }.flatMap { case (_r5461, _p5462) => {
  var _rs5469: List[Any] = Nil; var _cp5470: Int = _p5462; var _go5473 = true
  while (_go5473) { parseStatementSep(input, _cp5470) match {
    case Some((_st5471, _np5472)) => _rs5469 = _rs5469 :+ _st5471; _cp5470 = _np5472
    case None => _go5473 = false } }
  Some((_rs5469, _cp5470)) }.map { case (_r5463, _p5464) => (new ~(_r5461, _r5463), _p5464) } } match {
    case Some((_st5458, _np5459)) => _rs5456 = _rs5456 :+ _st5458; _cp5457 = _np5459
    case None => _go5460 = false } }
  Some((_rs5456, _cp5457)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5437, _p5438) => (new ~(_r5435, _r5437), _p5438) } }.flatMap { case (_r5431, _p5432) => {
  var _rs5474: List[Any] = Nil; var _cp5475: Int = _p5432; var _go5478 = true
  while (_go5478) { parseStatementSep(input, _cp5475) match {
    case Some((_st5476, _np5477)) => _rs5474 = _rs5474 :+ _st5476; _cp5475 = _np5477
    case None => _go5478 = false } }
  Some((_rs5474, _cp5475)) }.map { case (_r5433, _p5434) => (new ~(_r5431, _r5433), _p5434) } }.flatMap { case (_r5427, _p5428) => {
  var _rs5479: List[Any] = Nil; var _cp5480: Int = _p5428; var _go5483 = true
  while (_go5483) { parseRescueClause(input, _cp5480) match {
    case Some((_st5481, _np5482)) => _rs5479 = _rs5479 :+ _st5481; _cp5480 = _np5482
    case None => _go5483 = false } }
  Some((_rs5479, _cp5480)) }.map { case (_r5429, _p5430) => (new ~(_r5427, _r5429), _p5430) } }.flatMap { case (_r5423, _p5424) => {
  var _rs5484: List[Any] = Nil; var _cp5485: Int = _p5424; var _go5488 = true
  while (_go5488) { parseStatementSep(input, _cp5485) match {
    case Some((_st5486, _np5487)) => _rs5484 = _rs5484 :+ _st5486; _cp5485 = _np5487
    case None => _go5488 = false } }
  Some((_rs5484, _cp5485)) }.map { case (_r5425, _p5426) => (new ~(_r5423, _r5425), _p5426) } }.flatMap { case (_r5419, _p5420) => ((if (input.startsWith("else", _p5420)) Some(("else", _p5420 + 4)) else None).flatMap { case (_r5505, _p5506) => (if (parseIdentCont(input, _p5506).isEmpty) Some(((), _p5506)) else None).map { case (_r5507, _p5508) => (new ~(_r5505, _r5507), _p5508) } }.flatMap { case (_r5501, _p5502) => parseSpacing(input, _p5502).map { case (_r5503, _p5504) => (new ~(_r5501, _r5503), _p5504) } }.flatMap { case (_r5497, _p5498) => {
  var _rs5509: List[Any] = Nil; var _cp5510: Int = _p5498; var _go5513 = true
  while (_go5513) { parseStatementSep(input, _cp5510) match {
    case Some((_st5511, _np5512)) => _rs5509 = _rs5509 :+ _st5511; _cp5510 = _np5512
    case None => _go5513 = false } }
  Some((_rs5509, _cp5510)) }.map { case (_r5499, _p5500) => (new ~(_r5497, _r5499), _p5500) } }.flatMap { case (_r5493, _p5494) => {
  var _rs5514: List[Any] = Nil; var _cp5515: Int = _p5494; var _go5518 = true
  while (_go5518) { (if (parseDoBlockStop(input, _cp5515).isEmpty) Some(((), _cp5515)) else None).flatMap { case (_r5523, _p5524) => parseStatement(input, _p5524).map { case (_r5525, _p5526) => (new ~(_r5523, _r5525), _p5526) } }.flatMap { case (_r5519, _p5520) => {
  var _rs5527: List[Any] = Nil; var _cp5528: Int = _p5520; var _go5531 = true
  while (_go5531) { parseStatementSep(input, _cp5528) match {
    case Some((_st5529, _np5530)) => _rs5527 = _rs5527 :+ _st5529; _cp5528 = _np5530
    case None => _go5531 = false } }
  Some((_rs5527, _cp5528)) }.map { case (_r5521, _p5522) => (new ~(_r5519, _r5521), _p5522) } } match {
    case Some((_st5516, _np5517)) => _rs5514 = _rs5514 :+ _st5516; _cp5515 = _np5517
    case None => _go5518 = false } }
  Some((_rs5514, _cp5515)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5495, _p5496) => (new ~(_r5493, _r5495), _p5496) } }.flatMap { case (_r5489, _p5490) => {
  var _rs5532: List[Any] = Nil; var _cp5533: Int = _p5490; var _go5536 = true
  while (_go5536) { parseStatementSep(input, _cp5533) match {
    case Some((_st5534, _np5535)) => _rs5532 = _rs5532 :+ _st5534; _cp5533 = _np5535
    case None => _go5536 = false } }
  Some((_rs5532, _cp5533)) }.map { case (_r5491, _p5492) => (new ~(_r5489, _r5491), _p5492) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5420)))).map { case (_r5421, _p5422) => (new ~(_r5419, _r5421), _p5422) } }.flatMap { case (_r5415, _p5416) => ((if (input.startsWith("ensure", _p5416)) Some(("ensure", _p5416 + 6)) else None).flatMap { case (_r5553, _p5554) => (if (parseIdentCont(input, _p5554).isEmpty) Some(((), _p5554)) else None).map { case (_r5555, _p5556) => (new ~(_r5553, _r5555), _p5556) } }.flatMap { case (_r5549, _p5550) => parseSpacing(input, _p5550).map { case (_r5551, _p5552) => (new ~(_r5549, _r5551), _p5552) } }.flatMap { case (_r5545, _p5546) => {
  var _rs5557: List[Any] = Nil; var _cp5558: Int = _p5546; var _go5561 = true
  while (_go5561) { parseStatementSep(input, _cp5558) match {
    case Some((_st5559, _np5560)) => _rs5557 = _rs5557 :+ _st5559; _cp5558 = _np5560
    case None => _go5561 = false } }
  Some((_rs5557, _cp5558)) }.map { case (_r5547, _p5548) => (new ~(_r5545, _r5547), _p5548) } }.flatMap { case (_r5541, _p5542) => {
  var _rs5562: List[Any] = Nil; var _cp5563: Int = _p5542; var _go5566 = true
  while (_go5566) { (if (parseEndKeyword(input, _cp5563).isEmpty) Some(((), _cp5563)) else None).flatMap { case (_r5571, _p5572) => parseStatement(input, _p5572).map { case (_r5573, _p5574) => (new ~(_r5571, _r5573), _p5574) } }.flatMap { case (_r5567, _p5568) => {
  var _rs5575: List[Any] = Nil; var _cp5576: Int = _p5568; var _go5579 = true
  while (_go5579) { parseStatementSep(input, _cp5576) match {
    case Some((_st5577, _np5578)) => _rs5575 = _rs5575 :+ _st5577; _cp5576 = _np5578
    case None => _go5579 = false } }
  Some((_rs5575, _cp5576)) }.map { case (_r5569, _p5570) => (new ~(_r5567, _r5569), _p5570) } } match {
    case Some((_st5564, _np5565)) => _rs5562 = _rs5562 :+ _st5564; _cp5563 = _np5565
    case None => _go5566 = false } }
  Some((_rs5562, _cp5563)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5543, _p5544) => (new ~(_r5541, _r5543), _p5544) } }.flatMap { case (_r5537, _p5538) => {
  var _rs5580: List[Any] = Nil; var _cp5581: Int = _p5538; var _go5584 = true
  while (_go5584) { parseStatementSep(input, _cp5581) match {
    case Some((_st5582, _np5583)) => _rs5580 = _rs5580 :+ _st5582; _cp5581 = _np5583
    case None => _go5584 = false } }
  Some((_rs5580, _cp5581)) }.map { case (_r5539, _p5540) => (new ~(_r5537, _r5539), _p5540) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5416)))).map { case (_r5417, _p5418) => (new ~(_r5415, _r5417), _p5418) } }.flatMap { case (_r5411, _p5412) => parseEndKeyword(input, _p5412).map { case (_r5413, _p5414) => (new ~(_r5411, _r5413), _p5414) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseIfStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("if", pos)) Some(("if", pos + 2)) else None).flatMap { case (_r5617, _p5618) => (if (parseIdentCont(input, _p5618).isEmpty) Some(((), _p5618)) else None).map { case (_r5619, _p5620) => (new ~(_r5617, _r5619), _p5620) } }.flatMap { case (_r5613, _p5614) => parseSpacing(input, _p5614).map { case (_r5615, _p5616) => (new ~(_r5613, _r5615), _p5616) } }.flatMap { case (_r5609, _p5610) => parseExpr(input, _p5610).map { case (_r5611, _p5612) => (new ~(_r5609, _r5611), _p5612) } }.flatMap { case (_r5605, _p5606) => (((if (input.startsWith("then", _p5606)) Some(("then", _p5606 + 4)) else None).flatMap { case (_r5625, _p5626) => (if (parseIdentCont(input, _p5626).isEmpty) Some(((), _p5626)) else None).map { case (_r5627, _p5628) => (new ~(_r5625, _r5627), _p5628) } }.flatMap { case (_r5621, _p5622) => parseSpacing(input, _p5622).map { case (_r5623, _p5624) => (new ~(_r5621, _r5623), _p5624) } }).orElse(parseInlineSpacing(input, _p5606).flatMap { case (_r5629, _p5630) => {
  parseStatementSep(input, _p5630) match {
    case None => None
    case Some((_fs5633, _fp5634)) =>
      var _rs5635: List[Any] = List(_fs5633); var _cp5636: Int = _fp5634; var _go5639 = true
      while (_go5639) { parseStatementSep(input, _cp5636) match {
        case Some((_st5637, _np5638)) => _rs5635 = _rs5635 :+ _st5637; _cp5636 = _np5638
        case None => _go5639 = false } }
      Some((_rs5635, _cp5636)) } }.map { case (_r5631, _p5632) => (new ~(_r5629, _r5631), _p5632) } })).orElse(parseInlineSpacing(input, _p5606).flatMap { case (_r5640, _p5641) => (if (input.startsWith(";", _p5641)) Some((";", _p5641 + 1)) else None).map { case (_r5642, _p5643) => (new ~(_r5640, _r5642), _p5643) } }).map { case (_r5607, _p5608) => (new ~(_r5605, _r5607), _p5608) } }.flatMap { case (_r5601, _p5602) => {
  var _rs5644: List[Any] = Nil; var _cp5645: Int = _p5602; var _go5648 = true
  while (_go5648) { parseStatementSep(input, _cp5645) match {
    case Some((_st5646, _np5647)) => _rs5644 = _rs5644 :+ _st5646; _cp5645 = _np5647
    case None => _go5648 = false } }
  Some((_rs5644, _cp5645)) }.map { case (_r5603, _p5604) => (new ~(_r5601, _r5603), _p5604) } }.flatMap { case (_r5597, _p5598) => {
  var _rs5649: List[Any] = Nil; var _cp5650: Int = _p5598; var _go5653 = true
  while (_go5653) { (if ((((if (input.startsWith("elsif", _cp5650)) Some(("elsif", _cp5650 + 5)) else None).flatMap { case (_r5666, _p5667) => (if (parseIdentCont(input, _p5667).isEmpty) Some(((), _p5667)) else None).map { case (_r5668, _p5669) => (new ~(_r5666, _r5668), _p5669) } }.flatMap { case (_r5662, _p5663) => parseSpacing(input, _p5663).map { case (_r5664, _p5665) => (new ~(_r5662, _r5664), _p5665) } }).orElse((if (input.startsWith("else", _cp5650)) Some(("else", _cp5650 + 4)) else None).flatMap { case (_r5674, _p5675) => (if (parseIdentCont(input, _p5675).isEmpty) Some(((), _p5675)) else None).map { case (_r5676, _p5677) => (new ~(_r5674, _r5676), _p5677) } }.flatMap { case (_r5670, _p5671) => parseSpacing(input, _p5671).map { case (_r5672, _p5673) => (new ~(_r5670, _r5672), _p5673) } })).orElse(parseEndKeyword(input, _cp5650)).isEmpty) Some(((), _cp5650)) else None).flatMap { case (_r5658, _p5659) => parseStatement(input, _p5659).map { case (_r5660, _p5661) => (new ~(_r5658, _r5660), _p5661) } }.flatMap { case (_r5654, _p5655) => {
  var _rs5678: List[Any] = Nil; var _cp5679: Int = _p5655; var _go5682 = true
  while (_go5682) { parseStatementSep(input, _cp5679) match {
    case Some((_st5680, _np5681)) => _rs5678 = _rs5678 :+ _st5680; _cp5679 = _np5681
    case None => _go5682 = false } }
  Some((_rs5678, _cp5679)) }.map { case (_r5656, _p5657) => (new ~(_r5654, _r5656), _p5657) } } match {
    case Some((_st5651, _np5652)) => _rs5649 = _rs5649 :+ _st5651; _cp5650 = _np5652
    case None => _go5653 = false } }
  Some((_rs5649, _cp5650)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5599, _p5600) => (new ~(_r5597, _r5599), _p5600) } }.flatMap { case (_r5593, _p5594) => {
  var _rs5683: List[Any] = Nil; var _cp5684: Int = _p5594; var _go5687 = true
  while (_go5687) { parseStatementSep(input, _cp5684) match {
    case Some((_st5685, _np5686)) => _rs5683 = _rs5683 :+ _st5685; _cp5684 = _np5686
    case None => _go5687 = false } }
  Some((_rs5683, _cp5684)) }.map { case (_r5595, _p5596) => (new ~(_r5593, _r5595), _p5596) } }.flatMap { case (_r5589, _p5590) => parseIfElsifElse(input, _p5590).map { case (_r5591, _p5592) => (new ~(_r5589, _r5591), _p5592) } }.flatMap { case (_r5585, _p5586) => parseEndKeyword(input, _p5586).map { case (_r5587, _p5588) => (new ~(_r5585, _r5587), _p5588) } }.map { case (r, p) => (_applyAction({  _ => IfExpr(NilLiteral(), List.empty, List.empty)  }, r), p) }

  def parseUnlessStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("unless", pos)) Some(("unless", pos + 6)) else None).flatMap { case (_r5720, _p5721) => (if (parseIdentCont(input, _p5721).isEmpty) Some(((), _p5721)) else None).map { case (_r5722, _p5723) => (new ~(_r5720, _r5722), _p5723) } }.flatMap { case (_r5716, _p5717) => parseSpacing(input, _p5717).map { case (_r5718, _p5719) => (new ~(_r5716, _r5718), _p5719) } }.flatMap { case (_r5712, _p5713) => parseExpr(input, _p5713).map { case (_r5714, _p5715) => (new ~(_r5712, _r5714), _p5715) } }.flatMap { case (_r5708, _p5709) => (((if (input.startsWith("then", _p5709)) Some(("then", _p5709 + 4)) else None).flatMap { case (_r5728, _p5729) => (if (parseIdentCont(input, _p5729).isEmpty) Some(((), _p5729)) else None).map { case (_r5730, _p5731) => (new ~(_r5728, _r5730), _p5731) } }.flatMap { case (_r5724, _p5725) => parseSpacing(input, _p5725).map { case (_r5726, _p5727) => (new ~(_r5724, _r5726), _p5727) } }).orElse(parseInlineSpacing(input, _p5709).flatMap { case (_r5732, _p5733) => {
  parseStatementSep(input, _p5733) match {
    case None => None
    case Some((_fs5736, _fp5737)) =>
      var _rs5738: List[Any] = List(_fs5736); var _cp5739: Int = _fp5737; var _go5742 = true
      while (_go5742) { parseStatementSep(input, _cp5739) match {
        case Some((_st5740, _np5741)) => _rs5738 = _rs5738 :+ _st5740; _cp5739 = _np5741
        case None => _go5742 = false } }
      Some((_rs5738, _cp5739)) } }.map { case (_r5734, _p5735) => (new ~(_r5732, _r5734), _p5735) } })).orElse(parseInlineSpacing(input, _p5709).flatMap { case (_r5743, _p5744) => (if (input.startsWith(";", _p5744)) Some((";", _p5744 + 1)) else None).map { case (_r5745, _p5746) => (new ~(_r5743, _r5745), _p5746) } }).map { case (_r5710, _p5711) => (new ~(_r5708, _r5710), _p5711) } }.flatMap { case (_r5704, _p5705) => {
  var _rs5747: List[Any] = Nil; var _cp5748: Int = _p5705; var _go5751 = true
  while (_go5751) { parseStatementSep(input, _cp5748) match {
    case Some((_st5749, _np5750)) => _rs5747 = _rs5747 :+ _st5749; _cp5748 = _np5750
    case None => _go5751 = false } }
  Some((_rs5747, _cp5748)) }.map { case (_r5706, _p5707) => (new ~(_r5704, _r5706), _p5707) } }.flatMap { case (_r5700, _p5701) => {
  var _rs5752: List[Any] = Nil; var _cp5753: Int = _p5701; var _go5756 = true
  while (_go5756) { (if (((if (input.startsWith("else", _cp5753)) Some(("else", _cp5753 + 4)) else None).flatMap { case (_r5769, _p5770) => (if (parseIdentCont(input, _p5770).isEmpty) Some(((), _p5770)) else None).map { case (_r5771, _p5772) => (new ~(_r5769, _r5771), _p5772) } }.flatMap { case (_r5765, _p5766) => parseSpacing(input, _p5766).map { case (_r5767, _p5768) => (new ~(_r5765, _r5767), _p5768) } }).orElse(parseEndKeyword(input, _cp5753)).isEmpty) Some(((), _cp5753)) else None).flatMap { case (_r5761, _p5762) => parseStatement(input, _p5762).map { case (_r5763, _p5764) => (new ~(_r5761, _r5763), _p5764) } }.flatMap { case (_r5757, _p5758) => {
  var _rs5773: List[Any] = Nil; var _cp5774: Int = _p5758; var _go5777 = true
  while (_go5777) { parseStatementSep(input, _cp5774) match {
    case Some((_st5775, _np5776)) => _rs5773 = _rs5773 :+ _st5775; _cp5774 = _np5776
    case None => _go5777 = false } }
  Some((_rs5773, _cp5774)) }.map { case (_r5759, _p5760) => (new ~(_r5757, _r5759), _p5760) } } match {
    case Some((_st5754, _np5755)) => _rs5752 = _rs5752 :+ _st5754; _cp5753 = _np5755
    case None => _go5756 = false } }
  Some((_rs5752, _cp5753)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5702, _p5703) => (new ~(_r5700, _r5702), _p5703) } }.flatMap { case (_r5696, _p5697) => {
  var _rs5778: List[Any] = Nil; var _cp5779: Int = _p5697; var _go5782 = true
  while (_go5782) { parseStatementSep(input, _cp5779) match {
    case Some((_st5780, _np5781)) => _rs5778 = _rs5778 :+ _st5780; _cp5779 = _np5781
    case None => _go5782 = false } }
  Some((_rs5778, _cp5779)) }.map { case (_r5698, _p5699) => (new ~(_r5696, _r5698), _p5699) } }.flatMap { case (_r5692, _p5693) => ((if (input.startsWith("else", _p5693)) Some(("else", _p5693 + 4)) else None).flatMap { case (_r5799, _p5800) => (if (parseIdentCont(input, _p5800).isEmpty) Some(((), _p5800)) else None).map { case (_r5801, _p5802) => (new ~(_r5799, _r5801), _p5802) } }.flatMap { case (_r5795, _p5796) => parseSpacing(input, _p5796).map { case (_r5797, _p5798) => (new ~(_r5795, _r5797), _p5798) } }.flatMap { case (_r5791, _p5792) => {
  var _rs5803: List[Any] = Nil; var _cp5804: Int = _p5792; var _go5807 = true
  while (_go5807) { parseStatementSep(input, _cp5804) match {
    case Some((_st5805, _np5806)) => _rs5803 = _rs5803 :+ _st5805; _cp5804 = _np5806
    case None => _go5807 = false } }
  Some((_rs5803, _cp5804)) }.map { case (_r5793, _p5794) => (new ~(_r5791, _r5793), _p5794) } }.flatMap { case (_r5787, _p5788) => {
  var _rs5808: List[Any] = Nil; var _cp5809: Int = _p5788; var _go5812 = true
  while (_go5812) { (if (parseEndKeyword(input, _cp5809).isEmpty) Some(((), _cp5809)) else None).flatMap { case (_r5817, _p5818) => parseStatement(input, _p5818).map { case (_r5819, _p5820) => (new ~(_r5817, _r5819), _p5820) } }.flatMap { case (_r5813, _p5814) => {
  var _rs5821: List[Any] = Nil; var _cp5822: Int = _p5814; var _go5825 = true
  while (_go5825) { parseStatementSep(input, _cp5822) match {
    case Some((_st5823, _np5824)) => _rs5821 = _rs5821 :+ _st5823; _cp5822 = _np5824
    case None => _go5825 = false } }
  Some((_rs5821, _cp5822)) }.map { case (_r5815, _p5816) => (new ~(_r5813, _r5815), _p5816) } } match {
    case Some((_st5810, _np5811)) => _rs5808 = _rs5808 :+ _st5810; _cp5809 = _np5811
    case None => _go5812 = false } }
  Some((_rs5808, _cp5809)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5789, _p5790) => (new ~(_r5787, _r5789), _p5790) } }.flatMap { case (_r5783, _p5784) => {
  var _rs5826: List[Any] = Nil; var _cp5827: Int = _p5784; var _go5830 = true
  while (_go5830) { parseStatementSep(input, _cp5827) match {
    case Some((_st5828, _np5829)) => _rs5826 = _rs5826 :+ _st5828; _cp5827 = _np5829
    case None => _go5830 = false } }
  Some((_rs5826, _cp5827)) }.map { case (_r5785, _p5786) => (new ~(_r5783, _r5785), _p5786) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5693)))).map { case (_r5694, _p5695) => (new ~(_r5692, _r5694), _p5695) } }.flatMap { case (_r5688, _p5689) => parseEndKeyword(input, _p5689).map { case (_r5690, _p5691) => (new ~(_r5688, _r5690), _p5691) } }.map { case (r, p) => (_applyAction({  _ => UnlessExpr(NilLiteral(), List.empty, List.empty)  }, r), p) }

  def parseCaseStmt(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("case", pos)) Some(("case", pos + 4)) else None).flatMap { case (_r5855, _p5856) => (if (parseIdentCont(input, _p5856).isEmpty) Some(((), _p5856)) else None).map { case (_r5857, _p5858) => (new ~(_r5855, _r5857), _p5858) } }.flatMap { case (_r5851, _p5852) => parseSpacing(input, _p5852).map { case (_r5853, _p5854) => (new ~(_r5851, _r5853), _p5854) } }.flatMap { case (_r5847, _p5848) => (parseExpr(input, _p5848).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5848)))).map { case (_r5849, _p5850) => (new ~(_r5847, _r5849), _p5850) } }.flatMap { case (_r5843, _p5844) => {
  var _rs5859: List[Any] = Nil; var _cp5860: Int = _p5844; var _go5863 = true
  while (_go5863) { parseStatementSep(input, _cp5860) match {
    case Some((_st5861, _np5862)) => _rs5859 = _rs5859 :+ _st5861; _cp5860 = _np5862
    case None => _go5863 = false } }
  Some((_rs5859, _cp5860)) }.map { case (_r5845, _p5846) => (new ~(_r5843, _r5845), _p5846) } }.flatMap { case (_r5839, _p5840) => {
  parseWhenClause(input, _p5840) match {
    case None => None
    case Some((_fs5864, _fp5865)) =>
      var _rs5866: List[Any] = List(_fs5864); var _cp5867: Int = _fp5865; var _go5870 = true
      while (_go5870) { parseWhenClause(input, _cp5867) match {
        case Some((_st5868, _np5869)) => _rs5866 = _rs5866 :+ _st5868; _cp5867 = _np5869
        case None => _go5870 = false } }
      Some((_rs5866, _cp5867)) } }.map { case (_r5841, _p5842) => (new ~(_r5839, _r5841), _p5842) } }.flatMap { case (_r5835, _p5836) => ((if (input.startsWith("else", _p5836)) Some(("else", _p5836 + 4)) else None).flatMap { case (_r5887, _p5888) => (if (parseIdentCont(input, _p5888).isEmpty) Some(((), _p5888)) else None).map { case (_r5889, _p5890) => (new ~(_r5887, _r5889), _p5890) } }.flatMap { case (_r5883, _p5884) => parseSpacing(input, _p5884).map { case (_r5885, _p5886) => (new ~(_r5883, _r5885), _p5886) } }.flatMap { case (_r5879, _p5880) => {
  var _rs5891: List[Any] = Nil; var _cp5892: Int = _p5880; var _go5895 = true
  while (_go5895) { parseStatementSep(input, _cp5892) match {
    case Some((_st5893, _np5894)) => _rs5891 = _rs5891 :+ _st5893; _cp5892 = _np5894
    case None => _go5895 = false } }
  Some((_rs5891, _cp5892)) }.map { case (_r5881, _p5882) => (new ~(_r5879, _r5881), _p5882) } }.flatMap { case (_r5875, _p5876) => {
  var _rs5896: List[Any] = Nil; var _cp5897: Int = _p5876; var _go5900 = true
  while (_go5900) { (if (parseEndKeyword(input, _cp5897).isEmpty) Some(((), _cp5897)) else None).flatMap { case (_r5905, _p5906) => parseStatement(input, _p5906).map { case (_r5907, _p5908) => (new ~(_r5905, _r5907), _p5908) } }.flatMap { case (_r5901, _p5902) => {
  var _rs5909: List[Any] = Nil; var _cp5910: Int = _p5902; var _go5913 = true
  while (_go5913) { parseStatementSep(input, _cp5910) match {
    case Some((_st5911, _np5912)) => _rs5909 = _rs5909 :+ _st5911; _cp5910 = _np5912
    case None => _go5913 = false } }
  Some((_rs5909, _cp5910)) }.map { case (_r5903, _p5904) => (new ~(_r5901, _r5903), _p5904) } } match {
    case Some((_st5898, _np5899)) => _rs5896 = _rs5896 :+ _st5898; _cp5897 = _np5899
    case None => _go5900 = false } }
  Some((_rs5896, _cp5897)) }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }.map { case (_r5877, _p5878) => (new ~(_r5875, _r5877), _p5878) } }.flatMap { case (_r5871, _p5872) => {
  var _rs5914: List[Any] = Nil; var _cp5915: Int = _p5872; var _go5918 = true
  while (_go5918) { parseStatementSep(input, _cp5915) match {
    case Some((_st5916, _np5917)) => _rs5914 = _rs5914 :+ _st5916; _cp5915 = _np5917
    case None => _go5918 = false } }
  Some((_rs5914, _cp5915)) }.map { case (_r5873, _p5874) => (new ~(_r5871, _r5873), _p5874) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5836)))).map { case (_r5837, _p5838) => (new ~(_r5835, _r5837), _p5838) } }.flatMap { case (_r5831, _p5832) => parseEndKeyword(input, _p5832).map { case (_r5833, _p5834) => (new ~(_r5831, _r5833), _p5834) } }.map { case (r, p) => (_applyAction({  _ => CaseExpr(None, List.empty, List.empty)  }, r), p) }

  def parseRightwardAssignStmt(input: String, pos: Int): Option[(Any, Int)] = parseConditionalExpr(input, pos).flatMap { case (_r5931, _p5932) => parseSpacing(input, _p5932).map { case (_r5933, _p5934) => (new ~(_r5931, _r5933), _p5934) } }.flatMap { case (_r5927, _p5928) => (if (input.startsWith("=>", _p5928)) Some(("=>", _p5928 + 2)) else None).map { case (_r5929, _p5930) => (new ~(_r5927, _r5929), _p5930) } }.flatMap { case (_r5923, _p5924) => parseSpacing(input, _p5924).map { case (_r5925, _p5926) => (new ~(_r5923, _r5925), _p5926) } }.flatMap { case (_r5919, _p5920) => parseInPatternExpr(input, _p5920).map { case (_r5921, _p5922) => (new ~(_r5919, _r5921), _p5922) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseExprStatement(input: String, pos: Int): Option[(Any, Int)] = parseExpr(input, pos).flatMap { case (_r5939, _p5940) => (parseSpacing1(input, _p5940).flatMap { case (_r5943, _p5944) => parseCommandArgs(input, _p5944).map { case (_r5945, _p5946) => (new ~(_r5943, _r5945), _p5946) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5940)))).map { case (_r5941, _p5942) => (new ~(_r5939, _r5941), _p5942) } }.flatMap { case (_r5935, _p5936) => parseInlineSpacing(input, _p5936).map { case (_r5937, _p5938) => (new ~(_r5935, _r5937), _p5938) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }

  def parseStatement(input: String, pos: Int): Option[(Any, Int)] = _withMemo(47, pos) {
    ((((((((((((((((((parseReturnStmt(input, pos)).orElse(parseRetryStmt(input, pos))).orElse(parseBreakStmt(input, pos))).orElse(parseNextStmt(input, pos))).orElse(parseYieldStmt(input, pos))).orElse(parseAliasStmt(input, pos))).orElse(parseBeginStmt(input, pos))).orElse(parseWhileStmt(input, pos))).orElse(parseUntilStmt(input, pos))).orElse(parseForStmt(input, pos))).orElse(parseCaseStmt(input, pos))).orElse(parseSingletonClassStmt(input, pos))).orElse(parseClassStmt(input, pos))).orElse(parseModuleStmt(input, pos))).orElse(parseDefStmt(input, pos))).orElse(parseIfStmt(input, pos))).orElse(parseUnlessStmt(input, pos))).orElse(parseRightwardAssignStmt(input, pos))).orElse(parseExprStatement(input, pos)).flatMap { case (_r5947, _p5948) => {
  var _rs5951: List[Any] = Nil; var _cp5952: Int = _p5948; var _go5955 = true
  while (_go5955) { parseInlineSpacing(input, _cp5952).flatMap { case (_r5956, _p5957) => parseModifierSuffix(input, _p5957).map { case (_r5958, _p5959) => (new ~(_r5956, _r5958), _p5959) } } match {
    case Some((_st5953, _np5954)) => _rs5951 = _rs5951 :+ _st5953; _cp5952 = _np5954
    case None => _go5955 = false } }
  Some((_rs5951, _cp5952)) }.map { case (_r5949, _p5950) => (new ~(_r5947, _r5949), _p5950) } }.map { case (r, p) => (_applyAction({  _ => ExprStmt(NilLiteral())  }, r), p) }
  }

  def parseEndDataSection(input: String, pos: Int): Option[(Any, Int)] = (if (input.startsWith("__END__", pos)) Some(("__END__", pos + 7)) else None).flatMap { case (_r5964, _p5965) => (if (parseIdentCont(input, _p5965).isEmpty) Some(((), _p5965)) else None).map { case (_r5966, _p5967) => (new ~(_r5964, _r5966), _p5967) } }.flatMap { case (_r5960, _p5961) => {
  var _rs5968: List[Any] = Nil; var _cp5969: Int = _p5961; var _go5972 = true
  while (_go5972) { (if (_cp5969 < input.length) Some((input.charAt(_cp5969).toString, _cp5969 + 1)) else None) match {
    case Some((_st5970, _np5971)) => _rs5968 = _rs5968 :+ _st5970; _cp5969 = _np5971
    case None => _go5972 = false } }
  Some((_rs5968, _cp5969)) }.map { case (_r5962, _p5963) => (new ~(_r5960, _r5962), _p5963) } }.map { case (r, p) => (_applyAction({  _ => ()  }, r), p) }

  def parseTopLevelStatements(input: String, pos: Int): Option[(Any, Int)] = {
  var _rs5981: List[Any] = Nil; var _cp5982: Int = pos; var _go5985 = true
  while (_go5985) { parseStatementSep(input, _cp5982) match {
    case Some((_st5983, _np5984)) => _rs5981 = _rs5981 :+ _st5983; _cp5982 = _np5984
    case None => _go5985 = false } }
  Some((_rs5981, _cp5982)) }.flatMap { case (_r5977, _p5978) => ((if (parseEndDataSection(input, _p5978).isEmpty) Some(((), _p5978)) else None).flatMap { case (_r5990, _p5991) => parseStatement(input, _p5991).map { case (_r5992, _p5993) => (new ~(_r5990, _r5992), _p5993) } }.flatMap { case (_r5986, _p5987) => {
  var _rs5994: List[Any] = Nil; var _cp5995: Int = _p5987; var _go5998 = true
  while (_go5998) { {
  parseStatementSep(input, _cp5995) match {
    case None => None
    case Some((_fs6007, _fp6008)) =>
      var _rs6009: List[Any] = List(_fs6007); var _cp6010: Int = _fp6008; var _go6013 = true
      while (_go6013) { parseStatementSep(input, _cp6010) match {
        case Some((_st6011, _np6012)) => _rs6009 = _rs6009 :+ _st6011; _cp6010 = _np6012
        case None => _go6013 = false } }
      Some((_rs6009, _cp6010)) } }.flatMap { case (_r6003, _p6004) => (if (parseEndDataSection(input, _p6004).isEmpty) Some(((), _p6004)) else None).map { case (_r6005, _p6006) => (new ~(_r6003, _r6005), _p6006) } }.flatMap { case (_r5999, _p6000) => parseStatement(input, _p6000).map { case (_r6001, _p6002) => (new ~(_r5999, _r6001), _p6002) } } match {
    case Some((_st5996, _np5997)) => _rs5994 = _rs5994 :+ _st5996; _cp5995 = _np5997
    case None => _go5998 = false } }
  Some((_rs5994, _cp5995)) }.map { case (_r5988, _p5989) => (new ~(_r5986, _r5988), _p5989) } }.map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p5978)))).map { case (_r5979, _p5980) => (new ~(_r5977, _r5979), _p5980) } }.flatMap { case (_r5973, _p5974) => {
  var _rs6014: List[Any] = Nil; var _cp6015: Int = _p5974; var _go6018 = true
  while (_go6018) { parseStatementSep(input, _cp6015) match {
    case Some((_st6016, _np6017)) => _rs6014 = _rs6014 :+ _st6016; _cp6015 = _np6017
    case None => _go6018 = false } }
  Some((_rs6014, _cp6015)) }.map { case (_r5975, _p5976) => (new ~(_r5973, _r5975), _p5976) } }.map { case (r, p) => (_applyAction({  _ => List.empty[Statement]  }, r), p) }

  def parseProgram(input: String, pos: Int): Option[(Any, Int)] = parseSpacing(input, pos).flatMap { case (_, _p6025) => parseTopLevelStatements(input, _p6025) }.flatMap { case (_r6022, _p6023) => parseSpacing(input, _p6023).map { case (_, _p6024) => (_r6022, _p6024) } }.flatMap { case (_r6019, _p6020) => (parseEndDataSection(input, _p6020).map { case (v, p) => (Some(v), p) }.orElse(Some((None, _p6020)))).map { case (_, _p6021) => (_r6019, _p6021) } }.map { case (r, p) => (_applyAction({  _ => Program(List.empty)  }, r), p) }

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
