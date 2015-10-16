/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @author Vamsi Thummala {vamsi@cs.duke.edu}, Copyright (C) 2013-2015
 *
 */



package safe.safelog
package parser

import scala.collection.mutable.{Set => MutableSet}

trait ParserImpl
  extends scala.util.parsing.combinator.JavaTokenParsers 
  with scala.util.parsing.combinator.PackratParsers 
  with com.typesafe.scalalogging.LazyLogging {
  parserImpl: ParserService =>

  // override whiteSpace to support C-style comments or (* comments *)
                             // space | single-line-comment | multi-line-comments-c-style | multi-line-comments-with-braces
  //protected override val whiteSpace = """(\s|(?m)/\*(\*(?!/)|[^*])*\*/|(?m)\(\*(\*(?!\))|[^*])*\*\))+""".r // ?m for multiline mode
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/|(?m)\(\*(\*(?!\))|[^*])*\*\))+""".r

  /*
  private var _saysOperator: Boolean = Config.config.saysOperator
  private var _self: String          = Config.config.self
  // _statementCache is accessed from Repl
  private[safelog] val _statementCache: MutableCache[Index, MutableSet[Statement]] = 
    new MutableCache[Index, MutableSet[Statement]]()
  */

  lazy val symbol = """[a-zA-Z][a-zA-Z_\d]*""".r
  lazy val identifier: PackratParser[String] = (
      "+"
    | "-"
    | "*"
    | "/"
    | "%"   // modulo operator
    | "!"
    | "not"
    | "<:<" // subset
    | "<="  // Note: order important here for parsing, i.e., <= and >= should come before < and > respectively
    | "<"
    | ">="  
    | ">"
    | "=:=" // compare opeartor; right side eval + left side eval + unify 
    | ":="  // is opeartor; right side eval + unify
    | "="   // unify opeartor
    | "compare"
    | "is"
    | "unify"
    | "max"
    | "min"
    | "range"
    | "subset"
    | "in"
    | "@"   // at operator
  )

  lazy val globalVariablePattern = """\$([a-zA-Z_\d]*)""".r
  lazy val variablePattern = """(^[\?_][a-zA-Z_\d]*)""".r
  lazy val anyVariablePattern = """([\$\?])(?:\()?([a-zA-Z_\d]*)(?:\))?""".r

  lazy val typeDelimiter: PackratParser[String]		   = ("#"   |  "type") // dom for domain
  lazy val logicalIf: PackratParser[String]		   = (":-"  |  "if")
  lazy val logicalAnd: PackratParser[String]		   = (","   |  "and")
  lazy val logicalOr: PackratParser[String]		   = (";"   |  "or")
  lazy val logicalEnd: PackratParser[String]		   = ("."   |  "end")
  lazy val logicalNegation: PackratParser[String]	   = ("!"   |  "not")
  lazy val attrMapDelimiter: PackratParser[String]	   = ("->"  |  "as") ^^ {case v => "as"}
  lazy val attrMapIndexDelimiter: PackratParser[String]    = ("->>" |  "keyAs") ^^ {case v => "keyAs"}

  def addStatement(index: Index, s: Statement): MutableSet[Statement] = {
    val stmts: MutableSet[Statement] = _statementCache.get(index) map {
      v => v +=s
    } getOrElse {
      val _newSet = MutableSet.empty[Statement] 
      _newSet += s
    }
    _statementCache.put(index, stmts)
    stmts
  }
  
  def addStatementSeq(index: Index, seq: Seq[Statement]): MutableSet[Statement] = {
    val stmts: MutableSet[Statement] = _statementCache.get(index) map {
      v => v ++= seq
    } getOrElse {
      val _newSet = MutableSet.empty[Statement]
      _newSet ++= seq
    }
    _statementCache.put(index, stmts)
    stmts
  }

  lazy val program: PackratParser[MutableCache[Index, MutableSet[Statement]]]  = rep(multiRule | statement) ^^ {
    case multiStatements =>
      _statementCache
  }

  lazy val statement: PackratParser[(Index, MutableSet[Statement])] = (query | assertion | retraction) ^^ {
    case s @ Assertion(terms) => terms.head match {
      case Structure(id, trs, _, _, _) if Config.config.reserved.contains(id) => 
        val expectedArity: Int = Config.config.reserved(id)
        val termsLength = if(saysOperator == true) {trs.length - 1} else {trs.length}
        //println(s"TERMS LENGTH: ${trs.length}; $terms; saysOp: ${saysOperator}")
        val res = if(termsLength != expectedArity) {
	  logger.warn(s"For metadata, the expected arity of $id is $expectedArity but ${trs.length} found")
          addStatement(StrLit(s"_${id.name}"), s)
        } else if(termsLength == expectedArity) addStatement(StrLit(s"_${id.name}"), s)
          else if(id.name == "name" && termsLength == 1) addStatement(StrLit(s"_${id.name}"), s)
          else addStatement(s.primaryIndex, s)
        (id, res)
      case _                  =>
        val res = addStatement(s.primaryIndex, s)
        (s.primaryIndex, res)
    }
    case s @ Retraction(x)    => 
      val res = addStatement(StrLit("_retraction"), s)
      (StrLit("_retraction"), res)
    case s @ Query(x)         => 
      val res = addStatement(StrLit("_query"), s)
      (StrLit("_query"), res)
    case s @ QueryAll(x)      => 
      val res = addStatement(StrLit("_query"), s)
      (StrLit("_query"), res)          // this is on purpose; for indexing we only care whether the statement is a query
    case _                    => throw new UnSafeException(s"Statement type not detected")
  }
  lazy val assertion: PackratParser[Statement] = 
    (("assert" ~> clause <~ "end") | (clause <~ logicalEnd)) ^^ {
    case trs => Assertion(trs)
  }
  lazy val retraction: PackratParser[Retraction] = 
    (("retract" ~> (predicateWithArity | clause) <~ logicalEnd) | ((predicateWithArity | clause) <~ "~")) ^^ {
    case trs => Retraction(trs)
  }

  lazy val predicateWithArity: PackratParser[Seq[Term]] = (constantString ~ "/" ~ integer) ^^ {
    case sym ~ slash ~ arity => Constant(StrLit("_withArity")) +: sym +: arity +: Nil
  }
  lazy val clause: PackratParser[Seq[Term]]  = (rule | groundFact)
  lazy val rule: PackratParser[Seq[Term]] = headLiteral ~ logicalIf ~ literals ^^ { // head :- body1; body2.
      // TODO
      // 1. check for safety: range restriction

      // 2. check for stratified logicalNegation and/or other valid rules (for e.g., guarded safelog)
    case head ~ lIf ~ body => 
      val (isSafe, unSafeVar) = rangeRestrictionCheck(head, body)
      if(!isSafe) {
        throw ParserException(s"""Unsound rule dectected. Check range restriction failed for ${unSafeVar.mkString(",")}""")
      }
      head +: body
  }
  lazy val multiRule: PackratParser[(Index, MutableSet[Statement])]  = (multiRuleAssertion | multiRuleRetraction) ^^ {
    case s @ Assertion(terms)  +: other    => terms.head match {
      case Structure(id, trs, _, _, _) if Config.config.reserved.contains(id) => 
        val expectedArity: Int = Config.config.reserved(id)
        val termsLength = if(saysOperator == true) {trs.length - 1} else {trs.length}
        //println(s"TERMS LENGTH: ${trs.length}; $terms; saysOp: ${saysOperator}")
        val out = if(termsLength != expectedArity) {
	  logger.warn(s"For metadata, the expected arity of $id is $expectedArity but ${trs.length} found")
          val res: MutableSet[Statement] = addStatementSeq(StrLit(s"_${id.name}"), s)
          (StrLit(s"_${id.name}"), res)
        } else if(termsLength == expectedArity) {
          val res: MutableSet[Statement] = addStatementSeq(StrLit(s"_${id.name}"), s)
          (StrLit(s"_${id.name}"), res)
        } else if(id.name == "name" && termsLength == 1) {
          val res: MutableSet[Statement] = addStatementSeq(StrLit(s"_${id.name}"), s)
          (StrLit(s"_${id.name}"), res)
        } else {
          val res: MutableSet[Statement] = addStatementSeq(s.head.primaryIndex, s)
          (s.head.primaryIndex, res)
        }
        out
      case _                  =>
        val res = addStatementSeq(s.head.primaryIndex, s)
        (s.head.primaryIndex, res)
      }
    case s @ Retraction(x) +: other    => 
      val res: MutableSet[Statement] = addStatementSeq(StrLit("_retraction"), s)
      (StrLit("_retraction"), res)
  }
  lazy val multiRuleAssertion: PackratParser[Seq[Statement]] = 
    (("assert" ~> headLiteral ~ logicalIf ~ repsep(literals, logicalOr) <~ "end") | (headLiteral ~ logicalIf ~ repsep(literals, logicalOr) <~ logicalEnd)) ^^ {
    case head ~ lIf ~ clauses => clauses.map{clause => 
      val (isSafe, unSafeVar) = rangeRestrictionCheck(head, clause)
      if(!isSafe) {
        throw ParserException(s"""Unsound rule dectected. Check range restriction failed for ${unSafeVar.mkString(",")}""")
      }
      Assertion(head +: clause)
    }
  }
  lazy val multiRuleRetraction: PackratParser[Seq[Statement]] = (("retract" ~> headLiteral ~ logicalIf ~ repsep(literals, logicalOr) <~ "end") | (headLiteral ~ logicalIf ~ repsep(literals, logicalOr) <~ "~")) ^^ {
    case head ~ lIf ~ clauses => clauses.map{clause => Retraction(head +: clause)}
  }

  lazy val groundFact: PackratParser[Seq[Term]] = headLiteral ^^ { 
    case head => 
      val (isSafe, unSafeVar) = rangeRestrictionCheck(head, Nil)
      if(!isSafe) {
        throw ParserException(s"""Unsound rule dectected. Check range restriction failed for ${unSafeVar.mkString(",")}""")
      }
      Seq(head) 
  }

  lazy val query: PackratParser[Statement] = (queryAll | queryOne)

  lazy val queryAll: PackratParser[QueryAll] = 
    (("queryAll" ~> literals <~ "end") | (opt(logicalIf) ~> literals <~ "??")) ^^ {case q => QueryAll(q)}

  lazy val queryOne: PackratParser[Query] = 
    (("query" ~> literals <~ "end") | (opt(logicalIf) ~> literals <~ "?")) ^^ {case q => Query(q)}

  lazy val literals: PackratParser[Seq[Term]] = repsep(literal, logicalAnd)
  lazy val headLiteral: PackratParser[Term] = (infixTerm | negatedAtom | structureTerm | atom)
  lazy val literal: PackratParser[Term] = (headLiteral | nilAtom)
  lazy val atoms: PackratParser[Seq[Term]] = repsep(atom, logicalAnd)

  lazy val nilAtom: PackratParser[Term] = opt(constant) ^^ {
    case None                                => Constant(StrLit("true"))
    case Some(c: Constant) if(c.id == StrLit("end"))  => c // the constant match should never occur; it is only for pattern matching purposes
    case other                               => throw ParserException(s"Statement not terminated properly: $other")
  }

  lazy val atom: PackratParser[Term] =  // Note: opt(typeDelimiter) is a hack to make #type -> value work
    opt((singleQuotedString | symbol | "?" | "_") <~ typeDelimiter) ~ opt(opt(typeDelimiter) ~ (symbol | singleQuotedString) ~ (attrMapIndexDelimiter | attrMapDelimiter)) ~ (variable | constant) ^^ {

    case None ~ None                           ~ Constant(x, cattrName, ctpe, ckey)            => 
      val (cctpe: StrLit, indexAndEncode: Encoding) = typeWithEncoding(ctpe, ckey, Encoding.Attr)
      Constant(x, cattrName, cctpe, indexAndEncode)

    case None ~ None                           ~ Variable(x, cattrName, ctpe, ckey)            => 
      val (cctpe: StrLit, indexAndEncode: Encoding) = typeWithEncoding(ctpe, ckey, Encoding.Attr)
      Variable(x, cattrName, cctpe, indexAndEncode)

    case None ~ None                           ~ Structure(x, xterms, cattrName, ctpe, ckey)   => // interpolation case
      val (cctpe: StrLit, indexAndEncode: Encoding) = typeWithEncoding(ctpe, ckey, Encoding.Attr)
      Structure(x, xterms, cattrName, cctpe, indexAndEncode)

    case None ~ Some(None ~ attrName ~ keyAttr)     ~ Constant(x, cattrName, ctpe, ckey)       => 
      val (cctpe: StrLit, indexAndEncode: Encoding) = 
        if (keyAttr == "as") typeWithEncoding(ctpe, ckey, Encoding.Attr)
        else typeWithEncoding(ctpe, ckey, Encoding.IndexAttr)
      Constant(x, StrLit(attrName.toString), cctpe, indexAndEncode)

    case None ~ Some(None ~ attrName ~ keyAttr)     ~ Variable(x, cattrName, ctpe, ckey)       => 
      val (cctpe: StrLit, indexAndEncode: Encoding) =
        if (keyAttr == "as") typeWithEncoding(ctpe, ckey, Encoding.Attr)
        else typeWithEncoding(ctpe, ckey, Encoding.IndexAttr)
      Variable(x, StrLit(attrName.toString), cctpe, indexAndEncode)

    case None ~ Some(dlm ~ tpe ~ keyAttr)      ~ Constant(x, cattrName, ctpe, ckey)            => 
      val (cctpe: StrLit, indexAndEncode: Encoding) =
        if (keyAttr == "as") typeWithEncoding(ctpe, ckey, Encoding.Attr)
        else typeWithEncoding(ctpe, ckey, Encoding.IndexAttr)
      Constant(x, cattrName, StrLit(tpe.toString), indexAndEncode)

    case None ~ Some(dlm ~ tpe ~ keyAttr)      ~ Variable(x, cattrName, ctpe, ckey)       => 
      val (cctpe: StrLit, indexAndEncode: Encoding) =
        if (keyAttr == "as") typeWithEncoding(ctpe, ckey, Encoding.Attr)
        else typeWithEncoding(ctpe, ckey, Encoding.IndexAttr)
      Variable(x, cattrName, StrLit(tpe.toString), indexAndEncode)

    case Some(attrName) ~ None                      ~ Constant(x, cattrName, ctpe, ckey)       => 
      val (cctpe: StrLit, indexAndEncode: Encoding) = typeWithEncoding(ctpe, ckey, Encoding.Attr)
      Constant(x, StrLit(attrName.toString), cctpe, indexAndEncode)

    case Some(attrName) ~ None                      ~ Variable(x, cattrName, ctpe, ckey)       => 
      val (cctpe: StrLit, indexAndEncode: Encoding) = typeWithEncoding(ctpe, ckey, Encoding.Attr)
      Variable(x, StrLit(attrName.toString), cctpe, indexAndEncode)

    case Some(attrName) ~ Some(dlm ~ tpe ~ keyAttr) ~ Constant(x, cattrName, ctpe, ckey)       => 
      val (cctpe: StrLit, indexAndEncode: Encoding) =
        if (keyAttr == "as") typeWithEncoding(ctpe, ckey, Encoding.Attr)
        else typeWithEncoding(ctpe, ckey, Encoding.IndexAttr)
      Constant(x, StrLit(attrName.toString), StrLit(tpe.toString), indexAndEncode)

    case Some(attrName) ~ Some(dlm ~ tpe ~ keyAttr) ~ Variable(x, cattrName, ctpe, ckey)       => 
      val (cctpe: StrLit, indexAndEncode: Encoding) =
        if (keyAttr == "as") typeWithEncoding(ctpe, ckey, Encoding.Attr)
        else typeWithEncoding(ctpe, ckey, Encoding.IndexAttr)
      Variable(x, StrLit(attrName.toString), StrLit(tpe.toString), indexAndEncode)
  }

  protected def typeWithEncoding(tpe: StrLit, enc: Encoding, indexAndEncode: Encoding): (StrLit, Encoding) = {
    if(enc == Encoding.AttrLiteral) (StrLit("StrLit"), Encoding(indexAndEncode.id | 2))
    else if(enc == Encoding.AttrHex) (StrLit("StrLit"), Encoding(indexAndEncode.id | 4))
    else if(enc == Encoding.AttrBase64) (StrLit("StrLit"), Encoding(indexAndEncode.id | 6))
    else (tpe, indexAndEncode) // enc == StrLit
  }

  // A rule is safe (range restricted) iff:
  //
  // 1. Each distinguished variable,             // Unsafe: s(X) :- r(Y)
  // 2. Each variable in an arithmetic subgoal,  // Unsafe: s(X) :- r(Y), X < Y 
  //     or contains an equality or is goal where X = Y, where Y is safe
  // 3. Each variable in a negated subgoal,      // Unsafe: s(X) :- r(Y), NOT r(X)
  //
  // also appears in a nonnegated relational subgoal.

  //@annotation.tailrec
  private def rangeRestrictionCheck(head: Term, body: Seq[Term]): Tuple2[Boolean, Set[Term]] = {

    // filter arithmetic literals and negated literals from body
    def filterVariablesInNegatedArithemeticLiterals(
        body: Seq[Term]
      , stdLiteralVariables: Set[Term] = Set.empty
      , negatedArithmeticLiteralVariables: Set[Term] = Set.empty
    ): (Set[Term], Set[Term]) = body match {

      case NegatedTerm(id, term, _, _, _) +: other => 
        filterVariablesInNegatedArithemeticLiterals(
            other
          , stdLiteralVariables
          , negatedArithmeticLiteralVariables ++ term.unboundVariables
        )
      case term @ Structure(equality, terms, _, _, _) +: other if Set(StrLit("_unify"), StrLit("_is")).contains(equality) => 
        val (isSubGoalSafe: Boolean, varSeq: Set[Term]) = terms.last match {
          case Constant(_, _, _, _)                                         => (true, Set.empty)
          case v @ Variable(_, _, _, _) if v.isEnvVariable()                => (true, Set.empty) // ignore env variables
          case Variable(_, _, _, _)                                         => rangeRestrictionCheck(head, terms)
          case Structure(_, xterms, _, _, _)                                => rangeRestrictionCheck(head, terms.head +: xterms)
        }
        if(isSubGoalSafe) {
	  filterVariablesInNegatedArithemeticLiterals(
	      other
	    , stdLiteralVariables ++ term.head.unboundVariables
	    , negatedArithmeticLiteralVariables
          )
        }
        else {
	  filterVariablesInNegatedArithemeticLiterals(
	      other
	    , stdLiteralVariables
	    , negatedArithmeticLiteralVariables ++ varSeq
          )
        }
      case term @ Structure(arithmetic, terms, _, _, _) +: other if arithmeticLiterals.contains(arithmetic) =>
        filterVariablesInNegatedArithemeticLiterals(
            other
          , stdLiteralVariables
          , negatedArithmeticLiteralVariables ++ term.head.unboundVariables
        )
      case stdLiteral +: other => 
        filterVariablesInNegatedArithemeticLiterals(
            other
          , stdLiteralVariables ++ stdLiteral.unboundVariables
          , negatedArithmeticLiteralVariables
        )
      case Nil => (stdLiteralVariables, negatedArithmeticLiteralVariables)
    }

    val (stdLiteralVariables, negatedArithmeticLiteralVariables) = filterVariablesInNegatedArithemeticLiterals(body)
    val headVariables = head.unboundVariables
    val variablesToCheck: Set[Term] = headVariables ++ negatedArithmeticLiteralVariables
    val diffVariables: Set[Term] = variablesToCheck.diff(stdLiteralVariables)
    if(diffVariables.isEmpty) (true, Set.empty) else (false, diffVariables)
  }

  lazy val negatedAtom: PackratParser[Term] = logicalNegation ~ opt("(") ~ (structureTerm | atom) ~ opt(")") ^^ {
    case neg ~ Some(_) ~ atm ~ Some(_) => NegatedTerm(StrLit("_not"), atm)
    case neg ~ None ~ atm ~ None => NegatedTerm(StrLit("_not"), atm)
  }

  lazy val infixTerm: PackratParser[Term] = (
      expr ~ "compare" ~ expr                     // eval both leftExpr and rightExpr and unify
    | expr ~ "notcompare" ~ expr                  // eval both leftExpr and rightExpr and unify
    | expr ~ "=:=" ~ expr                         // eval both leftExpr and rightExpr and unify
    | expr ~ "!=:=" ~ expr                        // eval both leftExpr and rightExpr and unify
    | expr ~ "is" ~ expr                          // eval rightExpr and unfiy
    | expr ~ ":=" ~ expr                          // eval rightExpr and unfiy
    | expr ~ "unify" ~ expr                       // unify 
    | expr ~ "notunify" ~ expr                    // unify 
    | expr ~ "=" ~ expr                           // unify 
    | expr ~ "!<:<" ~ expr                        // subset
    | expr ~ "<:<" ~ expr                         // subset
    | expr ~ "subset" ~ expr                      // subset
    | expr ~ "notsubset" ~ expr                   // subset
    | expr ~ "<:" ~ expr                          // in
    | expr ~ "in" ~ expr                          // in
    | expr ~ "!<:" ~ expr                         // in
    | expr ~ "notin" ~ expr                       // in
    | expr ~ "!=" ~ expr                          // not unify 
    | expr ~ "<=" ~ expr
    | expr ~ "!<=" ~ expr
    | expr ~ "<" ~ expr
    | expr ~ "!<" ~ expr
    | expr ~ ">=" ~ expr
    | expr ~ "!>=" ~ expr
    | expr ~ ">" ~ expr
    | expr ~ "!>" ~ expr
    | expr ~ "@" ~ expr                           // at operator
  ) ^^ {
    case leftTerm ~ operator ~ rightTerm => operator match {
      case "=:="  | "compare"     => Structure(StrLit("_compare"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!=:=" | "notcompare"  => NegatedTerm(StrLit("_not"), Structure(StrLit("_compare"), Seq(leftTerm) ++: Seq(rightTerm)))
      case ":="   | "is"          => Structure(StrLit("_is"), Seq(leftTerm) ++: Seq(rightTerm))
      case "="    | "unify"       => Structure(StrLit("_unify"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!="   | "notunify"    => NegatedTerm(StrLit("_not"), Structure(StrLit("_unify"), Seq(leftTerm) ++: Seq(rightTerm)))
      case "<:<"  | "subset"      => Structure(StrLit("_subset"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!<:<" | "notsubset"   => NegatedTerm(StrLit("_not"), Structure(StrLit("_subset"), Seq(leftTerm) ++: Seq(rightTerm)))
      case "<:"   | "in"          => Structure(StrLit("_in"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!<:<" | "notin"       => NegatedTerm(StrLit("_not"), Structure(StrLit("_in"), Seq(leftTerm) ++: Seq(rightTerm)))
      case "<"                    => Structure(StrLit("_lt"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!<"                   => NegatedTerm(StrLit("_not"), Structure(StrLit("_lt"), Seq(leftTerm) ++: Seq(rightTerm)))
      case "<="                   => Structure(StrLit("_lteq"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!<="                  => NegatedTerm(StrLit("_not"), Structure(StrLit("_lteq"), Seq(leftTerm) ++: Seq(rightTerm)))
      case ">"                    => Structure(StrLit("_gt"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!>"                   => NegatedTerm(StrLit("_not"), Structure(StrLit("_gt"), Seq(leftTerm) ++: Seq(rightTerm)))
      case ">="                   => Structure(StrLit("_gteq"), Seq(leftTerm) ++: Seq(rightTerm))
      case "!>="                  => NegatedTerm(StrLit("_not"), Structure(StrLit("_gteq"), Seq(leftTerm) ++: Seq(rightTerm)))
      case "@"                    => Structure(StrLit("_at"), Seq(leftTerm) ++: Seq(rightTerm))
      case _                      => Structure(operator, Seq(leftTerm) ++: Seq(rightTerm))
    }
  }

  lazy val expr: PackratParser[Term] = (structureTerm | variable | constant)

  lazy val arithmeticLiterals: Set[StrLit] = Set(
      StrLit("_plus")
    , StrLit("_minus")
    , StrLit("_times")
    , StrLit("_div")
    , StrLit("_rem")
    , StrLit("_lteq")
    , StrLit("_lt")
    , StrLit("_gteq")
    , StrLit("_gt")
    , StrLit("_max")
    , StrLit("_min")
  )

  lazy val functor: PackratParser[StrLit] = (identifier) ^^ {
    case "+"               => StrLit("_plus")
    case "-"               => StrLit("_minus")
    case "*"               => StrLit("_times")
    case "/"               => StrLit("_div")
    case "%"               => StrLit("_rem")     // modulo operator
    case "<:<" | "subset"  => StrLit("_subset")  // subset operator
    case "<:"  | "in"      => StrLit("_in")      // in operator
    case "<="              => StrLit("_lteq")
    case "<"               => StrLit("_lt")
    case ">="              => StrLit("_gteq")
    case ">"               => StrLit("_gt")
    case "=:=" | "compare" => StrLit("_compare") // compare opeartor; right side eval + left side eval + unify 
    case ":="  | "is"      => StrLit("_is")     // is opeartor; right side eval + unify
    case "="   | "unify"   => StrLit("_unify")
    case "!"   | "not"     => StrLit("_not")
    case "max"             => StrLit("_max")
    case "min"             => StrLit("_min")
    case "range"           => StrLit("_range")
    case "@"               => StrLit("_at")      // at operator
  }

  lazy val operatorTerm: PackratParser[Term] = opt(atom <~ ("says" | ":")) ~ functor ~ "(" ~ atoms <~ ")" ^^ {
    case Some(subject) ~ funct ~ lParen ~ trs => Structure(funct, trs) // No signing needed for functional operators
    case None ~ funct ~ lParen ~ trs => Structure(funct, trs)
  }

  lazy val symbolTerm: PackratParser[Term] = opt(atom <~ ("says" | ":")) ~ (constantString | singleQuotedString) ~ "(" ~ atoms <~ ")" ^^ {
    case Some(speaker) ~ funct ~ lParen ~ trs => Structure(funct.id, speaker +: trs)
    case None ~ funct ~ lParen ~ trs =>
      if(saysOperator == true && self != "Self") Structure(funct.id, Constant(StrLit(self), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64) +: trs)
      else if(saysOperator) Structure(funct.id, Variable("$" + s"${self}") +: trs)
      else Structure(funct.id, trs)
  }

  lazy val overrideOperatorTerm: PackratParser[Term] = opt(atom <~ ("says" | ":")) ~ identifier ~ "(" ~ atoms <~ ")" ^^ {
    case Some(subject) ~ funct ~ lParen ~ trs => Structure(funct, trs) // No signing needed for functional operators
    case None ~ funct ~ lParen ~ trs => Structure(funct, trs)
  }

  // override may be allowed indirectly from a higher layer (slang)
  lazy val overrideDefaultTerm = ".." ~> (overrideOperatorTerm | symbolTerm) ^^ {case x => x}

  lazy val structureTerm: PackratParser[Term] = opt((singleQuotedString | symbol | "?" | "_") <~ typeDelimiter) ~ opt(opt(typeDelimiter) ~ (symbol | singleQuotedString) <~ (attrMapIndexDelimiter | attrMapDelimiter)) ~ (overrideDefaultTerm | operatorTerm | symbolTerm) ^^ {
    // For Structure, attrMapIndexDelimiter does not matter since all predicates are indexed by default
    case None ~ None ~ Structure(funct, trs, _, _, _)                      => 
      Structure(funct, trs, StrLit("nil"), termType)
    case None ~ Some(None ~ attrName) ~ Structure(funct, trs, _, _, _)     => 
      Structure(funct, trs, StrLit(attrName.toString), termType)
    case None ~ Some(dlm ~ tpe) ~ Structure(funct, trs, _, _, _)           => 
      Structure(funct, trs, StrLit("nil"), StrLit(tpe.toString))
    case Some(attrName) ~ None ~ Structure(funct, trs, _, _, _)            => 
      Structure(funct, trs, StrLit(attrName.toString), termType)
    case Some(attrName) ~ Some(dlm ~ tpe) ~ Structure(funct, trs, _, _, _) => 
      Structure(funct, trs, StrLit(attrName.toString), StrLit(tpe.toString))
  }

  lazy val variable: PackratParser[Term] = (localVariable | globalVariable)

  lazy val localVariable: PackratParser[Term] = variablePattern ^^ {v => Variable(v)}
  lazy val globalVariable: PackratParser[Term] = "$" ~> symbol <~ opt("(" ~ ")") ^^ {v => Variable("$" + v)}

  lazy val constant: PackratParser[Term] = (doubleQuotedString | numeric | singleQuotedString | constantString)
  lazy val constantString: PackratParser[Term] = not("""end$""".r) ~> symbol  ^^ {
    case sym => Constant(sym)
  }
  lazy val doubleQuotedString: PackratParser[Term] = tripleDoubleQuotedString | doubleQuotedStringWithEscapeDelimitedMayBe

  private def parseDoubleQuotedString(str: String): Term = {
    var _mutableStr = str
      anyVariablePattern.findAllIn(str).matchData.map {
        case m if !m.group(2).isEmpty =>
          val enclosedVarMayBe = s"${m.group(1)}${m.group(2)}"
          _mutableStr = _mutableStr.replace(s"${m.group(1)}(${m.group(2)})", enclosedVarMayBe) // NOTE: m.group(0) would not work. e.g., pattern hello($World) will match the right most paren
          Variable(enclosedVarMayBe)
      }.toSeq match  {
	case Nil    => Constant(StrLit(Term.stripQuotes(_mutableStr.toString)), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral)
	case varSeq =>
          Structure(StrLit("_interpolate")
             , Constant(Term.stripQuotes(_mutableStr.toString)) 
            +: Constant(varSeq.mkString(","))
            +: varSeq
          )
      }
  }

  lazy val domainTerm: PackratParser[Term] = ("dn" ~ "|") ~> repsep(numeric | singleQuotedString | constantString, ".") ~ opt(".") ^^ {
    case cTerms ~ Some(root) => Structure(StrLit("_seq"), Constant(".") +: cTerms.reverse, termIndex, StrLit("Dn"))
    case cTerms ~ None       => Structure(StrLit("_seq"), Constant(".") +: cTerms.reverse, termIndex, StrLit("Dn"))
  }


  lazy val doubleQuotedStringWithEscapeDelimitedMayBe: PackratParser[Term] = opt(symbol) ~ "\"" ~ """([^"\\]*(?:\\.[^"\\]*)*)""".r <~ "\"" ^^ {
    case Some(tpe) ~ lquote ~ str if (tpe == "r" | tpe == "regex") =>
      Variable(s"^$str")
    case Some(tpe) ~ lquote ~ str                                  => parseDomainTerm(s"$tpe|$str")
    case None ~ lquote ~ str      => parseDoubleQuotedString(str)
  }
  lazy val tripleDoubleQuotedString: PackratParser[Term] = opt(symbol) ~ "\"\"\"" ~ """(((?s)(?!\"\"\").)*)""".r <~ "\"\"\"" ^^ {
    case Some(tpe) ~ lquote ~ str if (tpe == "r" | tpe == "regex") =>
      Variable(s"^$str")
    case Some(tpe) ~ lquote ~ str =>
      throw ParserException(s"Prefix type not recognized: $tpe")
    case None ~ lquote ~ str      => parseDoubleQuotedString(str)
  }

  lazy val singleQuotedString: PackratParser[Term] = tripleQuotedString | singleQuotedStringWithEscapeDelimitedMayBe

  // ((?!''')(.|\n))* --- negative lookup is very expensive resulting in stack overflow
  lazy val tripleQuotedString: PackratParser[Term] = opt(symbol) ~ "'''" ~ """(((?s)(?!''').)*)""".r <~ "'''" ^^ {
    case None ~ lquote ~ str      => Constant(StrLit(str.replaceAll("""\\'""", "'")), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral)
    case Some(tpe) ~ lquote ~ str => tpe match {
      case "u" => Constant(StrLit(str.replaceAll("""\\'""", "'")), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case "h" =>
	val hex = try {java.lang.Long.parseLong(str, 16)} catch {
	  case ex: NumberFormatException => throw NumericException(s"Invalid input for hex: $str")
	}
        Constant(StrLit(hex.toString), StrLit("nil"), StrLit("StrLit"), Encoding.AttrHex)
      case _ => throw ParserException(s"Unknown encoding detected: $tpe")
    }
  }

  // ([^'\\]*(?:\\.[^'\\]*)*) match anything other than ' or \; followed by \anything and then not ' or \
  lazy val singleQuotedStringWithEscapeDelimitedMayBe: PackratParser[Term] = opt(symbol) ~ "'" ~ """([^'\\]*(?:\\.[^'\\]*)*)""".r <~ "'" ^^ {
    case None ~ lquote ~ str      => Constant(StrLit(str.replaceAll("""\\'""", "'")), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral) // """[^']""".r good enough?
    case Some(tpe) ~ lquote ~ str => tpe match {
      case "u" => Constant(StrLit(str.replaceAll("""\\'""", "'")), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case "h" =>
	val hex = try {java.lang.Long.parseLong(str, 16)} catch {
	  case ex: NumberFormatException => throw NumericException(s"Invalid input for hex: $str")
	}
        Constant(StrLit(hex.toString), StrLit("nil"), StrLit("StrLit"), Encoding.AttrHex)
      case _ => throw ParserException(s"Unknown encoding detected: $tpe")
    }
  }

  lazy val numeric = (
      float              // 32 bit
    | double             // 64 bit
    | hexInteger         // 32 bit (same as long)
    | bigInteger         // string
    | long               // 64 bit
    | doubleInteger      // 64 bit
    | floatInteger       // 32 bit
    | short              // 16 bit (same as char)
    | byte               // 8 bit
    | integer            // 32 bit
  )

  lazy val float: PackratParser[Term] = """-?(\d+\.\d+)([eE][+-]?\d+)?""".r <~ """[fF]+""".r ^^ { c => Constant(c, StrLit("nil"), StrLit("Float")) }
  lazy val double: PackratParser[Term] = """-?(\d+\.\d+)""".r <~ opt("""[dD]?""".r) ^^ { c => Constant(c, StrLit("nil"), StrLit("Double")) } // restriction: .1 and 3. are not valid 
  lazy val hexInteger: PackratParser[Term] = """\-?0x[\da-fA-f]+""".r ^^ { c => 
    val hex = try {java.lang.Long.parseLong(c.substring(2), 16)} catch {
      case ex: NumberFormatException => throw NumericException(s"Invalid input for hex: $c")
    }
    Constant(StrLit(hex.toString), StrLit("nil"), StrLit("StrLit"), Encoding.AttrHex)
  }
  lazy val bigInteger: PackratParser[Term] = wholeNumber <~ """[zZ]""".r ^^ { c => Constant(c, StrLit("nil"), StrLit("BigInt")) }
  lazy val long: PackratParser[Term] = wholeNumber <~ """[lL]""".r ^^ { c => Constant(c, StrLit("nil"), StrLit("Long")) }
  lazy val doubleInteger: PackratParser[Term] = wholeNumber <~ """[dD]""".r ^^ { c => Constant(c, StrLit("nil"), StrLit("Double")) }
  lazy val floatInteger: PackratParser[Term] = wholeNumber <~ """[fF]""".r ^^ { c => Constant(c, StrLit("nil"), StrLit("Float")) }
  lazy val short: PackratParser[Term] = wholeNumber <~ """[sS]""".r ^^ { c => 
    try { c.toShort } catch {
      case ex: NumberFormatException => throw NumericException(s"Invalid input for short: $c: ")
    }
    Constant(c, StrLit("nil"), StrLit("Short")) 
  }
  lazy val byte: PackratParser[Term] = wholeNumber <~ """[bB]""".r ^^ { c => 
    try { c.toByte } catch {
      case ex: NumberFormatException => throw NumericException(s"Invalid input for byte: $c")
    }
    Constant(c, StrLit("nil"), StrLit("Byte"))
  }
  lazy val integer: PackratParser[Term] = wholeNumber ^^ {c => Constant(c, StrLit("nil"), StrLit("Int"))}

  /**
   *  source is provided by slang program
   */
  def parseSlog(source: String): ParseResult[MutableCache[Index, MutableSet[Statement]]] = {
    parseAll(program, source)
  }

  private[safelog] def parseDomainTerm(source: String): Term = {
    val res: Term = parseAll(domainTerm, source) match {
      case Success(_result, _) => _result
      case failure: NoSuccess => throw ParserException(s"Parse error: ${failure}")
    }
    res
  }
 

  override def parse(source: String): Map[Index, Set[Statement]] = {
    val res: Map[Index, Set[Statement]] = parseAll(program, source) match {
      case Success(_result, _) =>
        _result.map { kv => (kv._1, kv._2.toSet)}.toMap
      case failure: NoSuccess => throw ParserException(s"${failure.msg}")
    }
    res
  }

  override def parse(source: java.io.Reader): Map[Index, Set[Statement]] = {
    val res: Map[Index, Set[Statement]] = parseAll(program, source) match {
      case Success(_result, _) => 
        _result.map { kv => (kv._1, kv._2.toSet)}.toMap
      case failure: NoSuccess => throw ParserException(s"${failure.msg}")
    }
    res
  }

  override def parseFile(fileName: String): Map[Index, Set[Statement]] = {
    val source = new java.io.BufferedReader(new java.io.FileReader(fileName))
    val res = parse(source)
    source.close()
    res
  }

  /*
  def setSays(value: Boolean): Unit = {
    _saysOperator = value
  }
  def setSpeaker(speaker: String): Unit = {
    _self = speaker
  }
  def clearContextCached(): Unit = { // scala bug? seems clearContext() is defined somewhere in predef
    _statementCache.clear()
  }
  */

  override def parseFileFresh(speaker: String, fileName: String): Map[Index, Set[Statement]] = {
    //setSays(true)
    //setSpeaker(speaker)
    //clearContextCached()
    val source = new java.io.BufferedReader(new java.io.FileReader(fileName))
    val res = parse(source)
    source.close()
    res
  }

  private def parseAsSegmentsHelper(
    result: ParseResult[MutableCache[Index, MutableSet[Statement]]]
  ): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]] = result match {

    case Success(_result, _) =>
      val importSeq      = _result.get(StrLit("_import")).getOrElse(Nil).toSeq
      _result           -= StrLit("_import")
      val querySeq       = _result.get(StrLit("_query")).getOrElse(Nil).toSeq
      _result           -= StrLit("_query")
      val retractionSeq  = _result.get(StrLit("_retraction")).getOrElse(Nil).toSeq
      _result           -= StrLit("_retraction")
      Tuple4(_result.map {kv => (kv._1, kv._2.toSet)}.toMap, querySeq, importSeq, retractionSeq)
    case failure: NoSuccess => throw ParserException(s"${failure.msg}")
  }

  override def parseAsSegments(source: String): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]] = {
    parseAsSegmentsHelper(parseAll(program, source))
  }

  override def parseAsSegments(source: java.io.Reader): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]] = {
    parseAsSegmentsHelper(parseAll(program, source))
  }

  override def parseFileAsSegments(fileName: String): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]] = {
    val source = new java.io.BufferedReader(new java.io.FileReader(fileName))
    val res = parseAsSegments(source)
    source.close()
    res
  }

  lazy val startsWithComment = """(\s*//.*)+$""".r  // line starts with a comment
  lazy val pasteMode         = """(\s*p(aste)?(\(\))?\.)\s*$""".r  // line starts with a comment
  lazy val endOfSource       = """(.*)([.?~]|end)\s*(//.*)?$""".r
  lazy val quit              = """(\s*q(uit)?\s*(\(\))?\s*[.?]+\s*$)""".r // q. | quit. | q(). | quit(). | q? | quit? | ..
  lazy val pasteQuit         = """(.*)(\.q(uit)?\s*(\(\))?\s*[.?]+\s*$)""".r // q. | quit. | q(). | quit(). | q? | quit? | ..
  var _isPasteMode           = false

  private def handleCmdLine(source: String): Tuple2[Option[MutableCache[Index, MutableSet[Statement]]], Symbol] = {
    endOfSource.findFirstIn(source) match {
      case Some(_) => try {
	parseAll(program, source) match {
	  case Success(result, _) => 
	    (Some(result), 'success)
	  case Failure(msg, _)    => 
	    logger.error("Parse error: " + msg)
	    (None, 'failure)
	  case Error(msg, _)      => 
	    logger.error("Parse error: " + msg)
	    (None, 'error)
	}
      } catch {
	case ex: ParserException => 
	  logger.error("Parse error: " + ex)
	  (None, 'error)
	case ex: NumericException => 
	  logger.error("Parse error: " + ex)
	  (None, 'error)
      }
      case None => (None, 'continuation)
    }
  }

  override def parseCmdLine(source: String): Tuple2[Option[MutableCache[Index, MutableSet[Statement]]], Symbol] = source match {
    case startsWithComment(_) => (None, 'comment)
    case pasteMode(_, _, _)   => 
      _isPasteMode = true
      (None, 'paste)
    case quit(_, _, _) => (None, 'quit)
    case pasteQuit(_, _, _, _) if(_isPasteMode == false) => (None, 'quit)
    case pasteQuit(src, _, _, _) if(_isPasteMode == true)  =>
      _isPasteMode = false
      handleCmdLine(s"$src.")
    case _ if(_isPasteMode)     => (None, 'continuation)
    case _                      => handleCmdLine(source)
  }
}
