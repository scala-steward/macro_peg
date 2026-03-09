package com.github.kmizu.macro_peg.codegen

import com.github.kmizu.macro_peg.Ast
import com.github.kmizu.macro_peg.Ast._
import com.github.kmizu.macro_peg._

case class GenerationError(pos: Position, message: String, hint: Option[String] = None)

object ParserGenerator {
  def generateFromSource(
    source: String,
    objectName: String = "GeneratedParser",
    packageName: Option[String] = None,
    startRule: Symbol = Symbol("S")
  ): Either[Diagnostic, String] = {
    val parsed = try {
      Right(Parser.parse(source))
    } catch {
      case Parser.ParseException(pos, msg) =>
        Left(Diagnostic(
          phase = DiagnosticPhase.Parse,
          message = msg,
          position = Some(pos),
          hint = Some("fix grammar syntax before code generation")
        ))
    }
    parsed.flatMap(grammar => generateWithSource(grammar, source, objectName, packageName, startRule))
  }

  def generate(
    grammar: Grammar,
    objectName: String = "GeneratedParser",
    packageName: Option[String] = None,
    startRule: Symbol = Symbol("S")
  ): Either[Diagnostic, String] = {
    val source = renderGrammar(grammar)
    generateWithSource(grammar, source, objectName, packageName, startRule)
  }

  private def generateWithSource(
    grammar: Grammar,
    source: String,
    objectName: String,
    packageName: Option[String],
    startRule: Symbol
  ): Either[Diagnostic, String] = {
    GrammarValidator.validate(grammar).left.map { err =>
      Diagnostic(
        phase = DiagnosticPhase.WellFormedness,
        message = err.message,
        position = Some(err.pos),
        hint = err.hint
      )
    }.flatMap { _ =>
      generateInternal(grammar, source, objectName, packageName, startRule).left.map { err =>
        Diagnostic(
          phase = DiagnosticPhase.Generation,
          message = err.message,
          position = Some(err.pos),
          hint = err.hint
        )
      }
    }
  }

  private def generateInternal(
    grammar: Grammar,
    source: String,
    objectName: String,
    packageName: Option[String],
    startRule: Symbol
  ): Either[GenerationError, String] = {
    val effectiveStart = grammar.directives.collectFirst { case StartDirective(r) => r }.getOrElse(startRule)
    if(!grammar.rules.exists(_.name == effectiveStart)) {
      return Left(GenerationError(Ast.DUMMY_POSITION, s"start rule `${effectiveStart.name}` is not defined", Some("choose an existing rule as startRule")))
    }

    if(usesNewFormat(grammar)) {
      generatePlainScalaBackend(grammar, objectName, packageName, effectiveStart)
    } else if(isFirstOrder(grammar)) {
      generateCombinatorBackend(grammar, objectName, packageName, effectiveStart)
    } else {
      Right(generateInterpreterBackend(source, objectName, packageName, effectiveStart))
    }
  }

  private def usesNewFormat(grammar: Grammar): Boolean =
    grammar.directives.nonEmpty ||
    grammar.rules.exists(r =>
      r.annotations.nonEmpty ||
      r.returnType.isDefined ||
      containsNewFeatures(r.body)
    )

  private def containsNewFeatures(exp: Expression): Boolean = exp match {
    case ActionBlock(_, _, _) => true
    case LeftProject(_, l, r) => containsNewFeatures(l) || containsNewFeatures(r)
    case RightProject(_, l, r) => containsNewFeatures(l) || containsNewFeatures(r)
    case Sequence(_, l, r) => containsNewFeatures(l) || containsNewFeatures(r)
    case Alternation(_, l, r) => containsNewFeatures(l) || containsNewFeatures(r)
    case Repeat0(_, b) => containsNewFeatures(b)
    case Repeat1(_, b) => containsNewFeatures(b)
    case Optional(_, b) => containsNewFeatures(b)
    case AndPredicate(_, b) => containsNewFeatures(b)
    case NotPredicate(_, b) => containsNewFeatures(b)
    case Debug(_, b) => containsNewFeatures(b)
    case Function(_, _, body) => containsNewFeatures(body)
    case Call(_, _, args) => args.exists(containsNewFeatures)
    case _ => false
  }

  private def generatePlainScalaBackend(
    grammar: Grammar,
    objectName: String,
    packageName: Option[String],
    startRule: Symbol
  ): Either[GenerationError, String] = {

    val ruleMap: Map[Symbol, Rule] = grammar.rules.map(r => r.name -> r).toMap
    val higherOrderSet: Set[Symbol] = grammar.rules.filter(_.args.nonEmpty).map(_.name).toSet
    val memoRuleList: List[Symbol] = grammar.rules.filter(_.annotations.exists(_.isInstanceOf[MemoAnnotation])).map(_.name)
    val memoIdMap: Map[Symbol, Int] = memoRuleList.zipWithIndex.toMap

    var counter = 0
    def fresh(prefix: String): String = { counter += 1; s"_${prefix}${counter}" }

    def parseMethodName(sym: Symbol): String = {
      val n = sym.name
      if (n.isEmpty) "parse" else s"parse${n.head.toUpper}${n.tail}"
    }

    def charLiteralS(ch: Char): String = ch match {
      case '\'' => "'\\''"
      case '\\' => "'\\\\'"
      case '\n' => "'\\n'"
      case '\r' => "'\\r'"
      case '\t' => "'\\t'"
      case c if c < 32 || c > 126 => s"'\\u${"%04x".format(c.toInt)}'"
      case c => s"'$c'"
    }

    def charClassPred(elems: List[CharClassElement]): String =
      elems.map {
        case CharRange(f, t) => s"(_c >= ${charLiteralS(f)} && _c <= ${charLiteralS(t)})"
        case OneChar(ch)     => s"(_c == ${charLiteralS(ch)})"
      }.mkString(" || ")

    def substitute(exp: Expression, env: Map[Symbol, Expression]): Expression = {
      if (env.isEmpty) return exp
      exp match {
        case Sequence(p, l, r)      => Sequence(p, substitute(l, env), substitute(r, env))
        case Alternation(p, l, r)   => Alternation(p, substitute(l, env), substitute(r, env))
        case Repeat0(p, b)          => Repeat0(p, substitute(b, env))
        case Repeat1(p, b)          => Repeat1(p, substitute(b, env))
        case Optional(p, b)         => Optional(p, substitute(b, env))
        case AndPredicate(p, b)     => AndPredicate(p, substitute(b, env))
        case NotPredicate(p, b)     => NotPredicate(p, substitute(b, env))
        case ActionBlock(p, b, c)   => ActionBlock(p, substitute(b, env), c)
        case LeftProject(p, l, r)   => LeftProject(p, substitute(l, env), substitute(r, env))
        case RightProject(p, l, r)  => RightProject(p, substitute(l, env), substitute(r, env))
        case Debug(p, b)            => Debug(p, substitute(b, env))
        case Identifier(_, name)    => env.getOrElse(name, exp)
        case Call(pos, name, args)  =>
          val sArgs = args.map(a => substitute(a, env))
          env.get(name) match {
            case Some(Function(_, params, body)) if params.length == sArgs.length =>
              substitute(body, params.zip(sArgs).toMap)
            case Some(other) if sArgs.isEmpty => other
            case _                            => Call(pos, name, sArgs)
          }
        case Function(p, params, body) =>
          Function(p, params, substitute(body, env -- params.toSet))
        case other => other
      }
    }

    def genExpr(exp: Expression, pos: String): String = exp match {
      case StringLiteral(_, s) =>
        val esc = escapeString(s)
        s"""(if (input.startsWith("$esc", $pos)) Some(("$esc", $pos + ${s.length})) else None)"""

      case Wildcard(_) =>
        s"(if ($pos < input.length) Some((input.charAt($pos).toString, $pos + 1)) else None)"

      case CharClass(_, positive, elems) =>
        val pred = charClassPred(elems)
        val check = if (positive) pred else s"!($pred)"
        s"(if ($pos < input.length && { val _c = input.charAt($pos); $check }) Some((input.charAt($pos).toString, $pos + 1)) else None)"

      case CharSet(_, positive, elems) =>
        val sortedPred = elems.toList.sorted.map(ch => s"(_c == ${charLiteralS(ch)})").mkString(" || ")
        val check = if (positive) sortedPred else s"!($sortedPred)"
        s"(if ($pos < input.length && { val _c = input.charAt($pos); $check }) Some((input.charAt($pos).toString, $pos + 1)) else None)"

      case Sequence(_, l, r) =>
        val (r1, p1, r2, p2) = (fresh("r"), fresh("p"), fresh("r"), fresh("p"))
        s"${genExpr(l, pos)}.flatMap { case ($r1, $p1) => ${genExpr(r, p1)}.map { case ($r2, $p2) => (new ~($r1, $r2), $p2) } }"

      case LeftProject(_, l, r) =>
        val (r1, p1, p2) = (fresh("r"), fresh("p"), fresh("p"))
        s"${genExpr(l, pos)}.flatMap { case ($r1, $p1) => ${genExpr(r, p1)}.map { case (_, $p2) => ($r1, $p2) } }"

      case RightProject(_, l, r) =>
        val p1 = fresh("p")
        s"${genExpr(l, pos)}.flatMap { case (_, $p1) => ${genExpr(r, p1)} }"

      case Alternation(_, l, r) =>
        s"(${genExpr(l, pos)}).orElse(${genExpr(r, pos)})"

      case Repeat0(_, b) =>
        val (rs, cp, st, np, go) = (fresh("rs"), fresh("cp"), fresh("st"), fresh("np"), fresh("go"))
        s"""{
          |  var $rs: List[Any] = Nil; var $cp: Int = $pos; var $go = true
          |  while ($go) { ${genExpr(b, cp)} match {
          |    case Some(($st, $np)) => $rs = $rs :+ $st; $cp = $np
          |    case None => $go = false } }
          |  Some(($rs, $cp)) }""".stripMargin

      case Repeat1(_, b) =>
        val (fs, fp, rs, cp, st, np, go) = (fresh("fs"), fresh("fp"), fresh("rs"), fresh("cp"), fresh("st"), fresh("np"), fresh("go"))
        s"""{
          |  ${genExpr(b, pos)} match {
          |    case None => None
          |    case Some(($fs, $fp)) =>
          |      var $rs: List[Any] = List($fs); var $cp: Int = $fp; var $go = true
          |      while ($go) { ${genExpr(b, cp)} match {
          |        case Some(($st, $np)) => $rs = $rs :+ $st; $cp = $np
          |        case None => $go = false } }
          |      Some(($rs, $cp)) } }""".stripMargin

      case Optional(_, b) =>
        s"(${genExpr(b, pos)}.map { case (v, p) => (Some(v), p) }.orElse(Some((None, $pos))))"

      case AndPredicate(_, b) =>
        s"(if (${genExpr(b, pos)}.isDefined) Some(((), $pos)) else None)"

      case NotPredicate(_, b) =>
        s"(if (${genExpr(b, pos)}.isEmpty) Some(((), $pos)) else None)"

      case ActionBlock(_, body, code) =>
        s"${genExpr(body, pos)}.map { case (r, p) => (({ $code })(r), p) }"

      case Debug(_, b) => genExpr(b, pos)

      case Identifier(_, name) =>
        s"${parseMethodName(name)}(input, $pos)"

      case Call(_, name, args) =>
        ruleMap.get(name) match {
          case Some(rule) if rule.args.nonEmpty =>
            if (args.length != rule.args.length)
              s"(None: Option[(Any, Int)]) /* arity mismatch for ${name.name} */"
            else
              genExpr(substitute(rule.body, rule.args.zip(args).toMap), pos)
          case _ =>
            s"${parseMethodName(name)}(input, $pos)"
        }

      case Function(_, _, _) =>
        "(None: Option[(Any, Int)]) /* unexpected Function node */"
    }

    def genRule(rule: Rule): String = {
      if (rule.args.nonEmpty) return ""
      val method = parseMethodName(rule.name)
      val bodyCode = genExpr(rule.body, "pos")
      val body = memoIdMap.get(rule.name) match {
        case Some(id) =>
          s"_withMemo($id, pos) {\n    $bodyCode\n  }"
        case None =>
          bodyCode
      }
      s"  def $method(input: String, pos: Int): Option[(Any, Int)] = $body"
    }

    val directives = grammar.directives
    val pkgName    = directives.collectFirst { case PackageDirective(n) => n }.orElse(packageName)
    val objName    = directives.collectFirst { case ObjectDirective(n) => n }.getOrElse(objectName)
    val imports    = directives.collect { case ImportDirective(p) => s"import $p" }
    val helpers    = directives.collect { case HelperDirective(c) => c }
    val preprocs   = directives.collect { case PreprocessDirective(c) => c }

    val packageLine  = pkgName.map(p => s"package $p\n\n").getOrElse("")
    val importSection = if (imports.nonEmpty) imports.mkString("\n") + "\n\n" else ""

    val memoInfra = if (memoIdMap.nonEmpty) {
      val idDecls = memoIdMap.map { case (sym, id) =>
        s"  private val _MEMO_${sanitizeIdentifier(sym.name).toUpperCase} = $id"
      }.mkString("\n")
      s"""$idDecls
         |  private val _memoStore = new ThreadLocal[java.util.HashMap[(Int,Int),AnyRef]] {
         |    override def initialValue() = new java.util.HashMap[(Int,Int),AnyRef]()
         |  }
         |  private def _withMemo(id: Int, pos: Int)(f: => Option[(Any,Int)]): Option[(Any,Int)] = {
         |    val key = (id, pos)
         |    _memoStore.get().get(key) match {
         |      case null =>
         |        _memoStore.get().put(key, (None: Option[(Any,Int)]).asInstanceOf[AnyRef])
         |        val r = f; _memoStore.get().put(key, r.asInstanceOf[AnyRef]); r
         |      case v => v.asInstanceOf[Option[(Any,Int)]]
         |    }
         |  }
         |  def resetMemo(): Unit = _memoStore.get().clear()""".stripMargin
    } else ""

    val helperSection  = helpers.map(h => s"  $h").mkString("\n")
    val preprocSection = preprocs.map(p => s"  $p").mkString("\n")
    val hasPreproc     = preprocs.nonEmpty
    val ruleDefs       = grammar.rules.filter(_.args.isEmpty).map(genRule).filter(_.nonEmpty).mkString("\n\n")
    val startMethod    = parseMethodName(startRule)

    // parse entry point: if %preprocess is defined, call _preprocess first
    val parseEntryInput = if (hasPreproc) "_preprocess(input)" else "input"

    Right(
      s"""${packageLine}${importSection}object $objName {
         |  case class ~[+A, +B](_1: A, _2: B) {
         |    override def toString: String = s"($$_1 ~ $$_2)"
         |  }
         |
         |$memoInfra
         |$helperSection
         |$preprocSection
         |
         |$ruleDefs
         |
         |  def parse(input: String): Option[(Any, Int)] =
         |    $startMethod($parseEntryInput, 0)
         |
         |  def parseAll(input: String): Either[String, Any] =
         |    $startMethod($parseEntryInput, 0) match {
         |      case Some((result, pos)) if pos == input.length => Right(result)
         |      case Some((_, pos)) => Left(s"Unconsumed input at position $$pos")
         |      case None => Left("Parse failed")
         |    }
         |}
         |""".stripMargin
    )
  }

  private def generateCombinatorBackend(
    grammar: Grammar,
    objectName: String,
    packageName: Option[String],
    startRule: Symbol
  ): Either[GenerationError, String] = {
    val ruleNameMap = buildRuleNameMap(grammar.rules)

    def emitExpression(exp: Expression): Either[GenerationError, String] = exp match {
      case Sequence(_, l, r) =>
        for(le <- emitExpression(l); re <- emitExpression(r)) yield s"($le ~ $re)"
      case Alternation(_, l, r) =>
        for(le <- emitExpression(l); re <- emitExpression(r)) yield s"($le / $re)"
      case Repeat0(_, b) =>
        emitExpression(b).map(be => s"($be).*")
      case Repeat1(_, b) =>
        emitExpression(b).map(be => s"($be).+")
      case Optional(_, b) =>
        emitExpression(b).map(be => s"($be).?")
      case AndPredicate(_, b) =>
        emitExpression(b).map(be => s"($be).and")
      case NotPredicate(_, b) =>
        emitExpression(b).map(be => s"!($be)")
      case StringLiteral(_, target) =>
        Right("\"" + escapeString(target) + "\".s")
      case Wildcard(_) =>
        Right("any")
      case CharClass(_, positive, elems) =>
        val encodedElems = elems.map {
          case CharRange(from, to) => s"${charLiteral(from)} to ${charLiteral(to)}"
          case OneChar(ch) => s"Seq(${charLiteral(ch)})"
        }.mkString(", ")
        if(positive) Right(s"range($encodedElems)")
        else Right(s"notIn(range($encodedElems))")
      case CharSet(_, positive, elems) =>
        val encodedElems = elems.toList.sorted.map(ch => s"Seq(${charLiteral(ch)})").mkString(", ")
        if(positive) Right(s"range($encodedElems)")
        else Right(s"notIn(range($encodedElems))")
      case Identifier(pos, name) =>
        ruleNameMap.get(name) match {
          case Some(scalaName) => Right(s"refer($scalaName)")
          case None => Left(GenerationError(pos, s"unknown identifier `${name.name}` during generation"))
        }
      case Call(pos, name, args) =>
        if(args.nonEmpty) {
          Left(GenerationError(pos, s"call `${name.name}` has arguments; first-order combinator backend cannot emit this", Some("use higher-order fallback backend")))
        } else {
          ruleNameMap.get(name) match {
            case Some(scalaName) => Right(s"refer($scalaName)")
            case None => Left(GenerationError(pos, s"unknown call target `${name.name}` during generation"))
          }
        }
      case Function(pos, _, _) =>
        Left(GenerationError(pos, "lambda/function expression is not supported by first-order combinator backend", Some("use higher-order fallback backend")))
      case Debug(_, b) =>
        emitExpression(b).map(be => s"($be).display")
      case ActionBlock(pos, _, _) =>
        Left(GenerationError(pos, "action blocks are not supported by the combinator backend", Some("use the plain Scala backend by adding a directive or annotation")))
      case LeftProject(pos, l, r) =>
        for(le <- emitExpression(l); re <- emitExpression(r)) yield s"($le << $re)"
      case RightProject(pos, l, r) =>
        for(le <- emitExpression(l); re <- emitExpression(r)) yield s"($le >> $re)"
    }

    val emittedRules = grammar.rules.foldLeft[Either[GenerationError, List[String]]](Right(Nil)) { (acc, rule) =>
      for {
        lines <- acc
        body <- emitExpression(rule.body)
      } yield lines :+ s"  lazy val ${ruleNameMap(rule.name)}: P[Any] = $body"
    }

    emittedRules.map { lines =>
      val packagePrefix = packageName.map(p => s"package $p\n\n").getOrElse("")
      val startRuleName = ruleNameMap(startRule)
      val ruleDefs = lines.mkString("\n")
      s"""${packagePrefix}import com.github.kmizu.macro_peg.combinator.MacroParsers._
         |
         |object $objectName {
         |  private def notIn[T](p: P[T]): P[String] =
         |    (!p ~ any).map { case _ ~ ch => ch }
         |
         |$ruleDefs
         |
         |  lazy val Start: P[Any] = refer($startRuleName) ~ !any
         |
         |  def parse(input: String): ParseResult[Any] =
         |    Start(input)
         |
         |  def parseAll(input: String): Either[String, Any] =
         |    com.github.kmizu.macro_peg.combinator.MacroParsers.parseAll(refer($startRuleName), input)
         |      .left.map(f => com.github.kmizu.macro_peg.combinator.MacroParsers.formatFailure(input, f))
         |}
         |""".stripMargin
    }
  }

  private def generateInterpreterBackend(
    source: String,
    objectName: String,
    packageName: Option[String],
    startRule: Symbol
  ): String = {
    val packagePrefix = packageName.map(p => s"package $p\n\n").getOrElse("")
    val start = escapeString(startRule.name)
    val sourceLiteral = "\"" + escapeString(source) + "\""
    s"""${packagePrefix}import com.github.kmizu.macro_peg._
       |
       |object $objectName {
       |  private val grammarSource: String = $sourceLiteral
       |  private val interpreterCache = scala.collection.mutable.Map.empty[EvaluationStrategy, Either[Diagnostic, Interpreter]]
       |
       |  private def interpreterEither(strategy: EvaluationStrategy): Either[Diagnostic, Interpreter] =
       |    interpreterCache.getOrElseUpdate(strategy, Interpreter.fromSourceEither(grammarSource, strategy))
       |
       |  def evaluate(
       |    input: String,
       |    start: Symbol = Symbol("$start"),
       |    strategy: EvaluationStrategy = EvaluationStrategy.CallByName
       |  ): Either[Diagnostic, EvaluationResult.Success] =
       |    interpreterEither(strategy).flatMap(_.evaluateEither(input, start))
       |
       |  def parse(
       |    input: String,
       |    start: Symbol = Symbol("$start"),
       |    strategy: EvaluationStrategy = EvaluationStrategy.CallByName
       |  ): Either[Diagnostic, EvaluationResult.Success] =
       |    evaluate(input, start, strategy)
       |
       |  def parseAll(
       |    input: String,
       |    start: Symbol = Symbol("$start"),
       |    strategy: EvaluationStrategy = EvaluationStrategy.CallByName
       |  ): Either[String, String] =
       |    evaluate(input, start, strategy).map(_.remained).left.map(_.format)
       |}
       |""".stripMargin
  }

  private def isFirstOrder(grammar: Grammar): Boolean = {
    grammar.rules.forall(r => r.args.isEmpty && !containsHigherOrder(r.body))
  }

  private def containsHigherOrder(exp: Expression): Boolean = exp match {
    case Sequence(_, l, r) => containsHigherOrder(l) || containsHigherOrder(r)
    case Alternation(_, l, r) => containsHigherOrder(l) || containsHigherOrder(r)
    case Repeat0(_, b) => containsHigherOrder(b)
    case Repeat1(_, b) => containsHigherOrder(b)
    case Optional(_, b) => containsHigherOrder(b)
    case AndPredicate(_, b) => containsHigherOrder(b)
    case NotPredicate(_, b) => containsHigherOrder(b)
    case Call(_, _, args) => args.nonEmpty || args.exists(containsHigherOrder)
    case Function(_, _, _) => true
    case Debug(_, b) => containsHigherOrder(b)
    case _ => false
  }

  private def buildRuleNameMap(rules: List[Rule]): Map[Symbol, String] = {
    val used = scala.collection.mutable.Set.empty[String]
    var nameMap = Map.empty[Symbol, String]

    rules.foreach { rule =>
      val base = "r_" + sanitizeIdentifier(rule.name.name)
      var candidate = base
      var index = 1
      while(used.contains(candidate)) {
        index += 1
        candidate = s"${base}_$index"
      }
      used += candidate
      nameMap += (rule.name -> candidate)
    }
    nameMap
  }

  private def sanitizeIdentifier(name: String): String = {
    val cleaned = name.map { ch =>
      if(ch.isLetterOrDigit || ch == '_') ch else '_'
    }
    if(cleaned.headOption.exists(_.isDigit)) "_" + cleaned else cleaned
  }

  private def renderGrammar(grammar: Grammar): String = {
    grammar.rules.map(renderRule).mkString("\n")
  }

  private def renderRule(rule: Rule): String = {
    val argsText =
      if(rule.args.isEmpty) ""
      else {
        val args = rule.args.zipWithIndex.map { case (argName, i) =>
          rule.argTypes.lift(i).flatten match {
            case Some(tpe) => s"${argName.name}: ${renderType(tpe)}"
            case None => argName.name
          }
        }.mkString(", ")
        s"($args)"
      }
    s"${rule.name.name}$argsText = ${renderExpression(rule.body)};"
  }

  private def renderType(tpe: Type): String = tpe match {
    case SimpleType(_) => "?"
    case RuleType(_, paramTypes, resultType) =>
      val params = paramTypes.map(renderType).mkString(", ")
      s"($params) -> ${renderType(resultType)}"
  }

  private def renderExpression(exp: Expression): String = exp match {
    case Sequence(_, l, r) => s"(${renderExpression(l)} ${renderExpression(r)})"
    case Alternation(_, l, r) => s"(${renderExpression(l)} / ${renderExpression(r)})"
    case Repeat0(_, b) => s"(${renderExpression(b)})*"
    case Repeat1(_, b) => s"(${renderExpression(b)})+"
    case Optional(_, b) => s"(${renderExpression(b)})?"
    case AndPredicate(_, b) => s"&(${renderExpression(b)})"
    case NotPredicate(_, b) => s"!(${renderExpression(b)})"
    case StringLiteral(_, target) => "\"" + escapeString(target) + "\""
    case Wildcard(_) => "."
    case CharClass(_, positive, elems) => renderCharClass(positive, elems)
    case CharSet(_, positive, elems) =>
      val sorted = elems.toList.sorted
      val body = sorted.map(ch => unicodeEscape(ch)).mkString
      if(positive) s"[$body]" else s"[^$body]"
    case Debug(_, b) => s"Debug(${renderExpression(b)})"
    case Identifier(_, name) => name.name
    case Call(_, name, args) =>
      s"${name.name}(${args.map(renderExpression).mkString(", ")})"
    case Function(_, args, body) =>
      s"(${args.map(_.name).mkString(", ")} -> ${renderExpression(body)})"
    case ActionBlock(_, b, code) => s"(${renderExpression(b)} => { $code })"
    case LeftProject(_, l, r) => s"(${renderExpression(l)} <~ ${renderExpression(r)})"
    case RightProject(_, l, r) => s"(${renderExpression(l)} ~> ${renderExpression(r)})"
  }

  private def renderCharClass(positive: Boolean, elems: List[CharClassElement]): String = {
    val body = elems.map {
      case CharRange(from, to) => s"${unicodeEscape(from)}-${unicodeEscape(to)}"
      case OneChar(ch) => unicodeEscape(ch)
    }.mkString
    if(positive) s"[$body]" else s"[^$body]"
  }

  private def unicodeEscape(ch: Char): String = "\\u%04x".format(ch.toInt)

  private def charLiteral(ch: Char): String = ch match {
    case '\n' => "'\\n'"
    case '\r' => "'\\r'"
    case '\t' => "'\\t'"
    case '\'' => "'\\''"
    case '\\' => "'\\\\'"
    case c if c.isControl => "'\\u%04x'".format(c.toInt)
    case c => "'" + c + "'"
  }

  private def escapeString(raw: String): String = {
    val builder = new StringBuilder
    raw.foreach {
      case '"' => builder.append("\\\"")
      case '\\' => builder.append("\\\\")
      case '\n' => builder.append("\\n")
      case '\r' => builder.append("\\r")
      case '\t' => builder.append("\\t")
      case c if c.isControl => builder.append("\\u%04x".format(c.toInt))
      case c => builder.append(c)
    }
    builder.toString()
  }
}
