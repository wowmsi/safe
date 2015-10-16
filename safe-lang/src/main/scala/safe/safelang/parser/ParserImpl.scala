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



package safe.safelang
package parser

import scala.collection.mutable.{Set => MutableSet}

import safe.safelog.{
  Constant, Variable, Encoding, MutableCache, StrLit,
  Assertion, Retraction, Query, QueryAll, Term, Structure, Statement, 
  termType, ParserException, Index, Validity, UnSafeException
}
import model.{
  UnsignedCredential, UnsignedUnBoundCredential, SignedCredential, 
  SignedCredentialHolder, Status, SubjectStmt, SpeakerStmt, Subject
}
    
trait ParserImpl
  extends safe.safelog.parser.ParserImpl
  with scala.util.parsing.combinator.JavaTokenParsers
  with scala.util.parsing.combinator.PackratParsers
  with com.typesafe.scalalogging.LazyLogging {
  slangParser: ParserService =>

  lazy val reservedFunctions: Set[StrLit] = Set(StrLit("definit"), StrLit("defenv"), StrLit("defcon"), StrLit("defun"), StrLit("defetch"), StrLit("defpost"), StrLit("defguard"))

  /*
  override lazy val endOfSource =

  """([^"\\]*(?:\\.[^"\\]*)*)""".r     // double quotes
  """([^'\\]*(?:\\.[^'\\]*)*)""".r     // single quotes
  """[^}\\]*(?:\\.[^}\\]*)*""".r       // set term
  """[^`\\]*(?:\\.[^`\\]*)*""".r       // fun term
  """(.*)([.?~!]|end)\s*(//.*)?$""".r  // standard eos
  */

  def word(str: String): Parser[String] = regex(s"\\b$str\\b".r)

  override lazy val statement: PackratParser[(Index, MutableSet[Statement])] = (query | assertion | retraction) ^^ {
    case s @ Assertion(terms) => terms.head match {
      case Structure(StrLit("import"), trs +: Nil, _, _, _) =>
        val res = addStatement(StrLit("import"), s)
        (StrLit("import"), res)
      case c @ Constant(name, _, _, _) if reservedFunctions.contains(name) => // def* case
        buildIndexForDef(c.primaryIndex, s.terms.tail.head.primaryIndex, s.terms.tail)
      case x                  =>
        val res = addStatement(s.primaryIndex, s)
        (s.primaryIndex, res)
    }
    case s @ Retraction(x)    =>
      val res = addStatement(StrLit("_retraction"), s)
      (StrLit("_retraction"), res)
    case s @ Query(x)         =>
      val qIndex = s.terms.head.primaryIndex
      if(qIndex == StrLit("definit0")) {
        buildIndexForDef(qIndex, s.terms.tail.head.primaryIndex, s.terms.tail)
      } else {
        val res = addStatement(StrLit("_query"), s)
        (StrLit("_query"), res)
      }
    case s @ QueryAll(x)      =>
      val qIndex = s.terms.head.primaryIndex
      if(qIndex == StrLit("definit0")) {
        buildIndexForDef(qIndex, s.terms.tail.head.primaryIndex, s.terms.tail)
      } else {
        val res = addStatement(StrLit("_query"), s)
        (StrLit("_query"), res)          // this is on purpose; for indexing we only care whether the statement is a query
      }
    case other                => throw UnSafeException(s"Statement type not detected: $other")
  }
  
  private def buildIndexForDef(defIndex: StrLit, stmtIndex: StrLit, terms: Seq[Term]): Tuple2[Index, MutableSet[Statement]] = {
    val stmts = Assertion(terms)
    val resForDef = addStatement(defIndex, stmts)
    val res = addStatement(stmtIndex, stmts)
    (stmtIndex, res)
  }
  
  // Note: not("def.*".r) is to avoid the case definit ?Var := fun()
  /*
  override lazy val queryAll: PackratParser[QueryAll] =
    (("queryAll" ~ not("def.*".r) ~> qliterals <~ "end") | (opt(logicalIf) ~ not("def.*".r) ~> qliterals <~ "??")) ^^ {case q => QueryAll(q)}

  override lazy val queryOne: PackratParser[Query] =
    (("query" ~ not("def.*".r) ~> qliterals <~ "end") | (opt(logicalIf) ~ not("def.*".r) ~> qliterals <~ "?")) ^^ {case q => Query(q)}
  */
  override lazy val queryAll: PackratParser[QueryAll] =
    (("queryAll" ~ not("defenv") ~> qliterals <~ "end") | (opt(logicalIf) ~ not("defenv") ~> qliterals <~ "??")) ^^ {case q => QueryAll(q)}

  override lazy val queryOne: PackratParser[Query] =
    (("query" ~ not("defenv") ~> qliterals <~ "end") | (opt(logicalIf) ~ not("defenv") ~> qliterals <~ "?")) ^^ {case q => Query(q)}
 
  lazy val qliterals: PackratParser[Seq[Term]] = initFunction | literals

    // Not need for range detection check
  override lazy val rule: PackratParser[Seq[Term]] = headLiteral ~ logicalIf ~ literals ^^ { // head :- body1; body2.
    case head ~ lIf ~ body => head +: body
  }
  override  lazy val multiRuleAssertion: PackratParser[Seq[Statement]] =
    ((word("assert") ~> headLiteral ~ logicalIf ~ repsep(literals, ";") <~ opt(",") ~ word("end")) | (headLiteral ~ logicalIf ~ repsep(literals, ";") <~ logicalEnd)) ^^ {
    case head ~ lIf ~ clauses => clauses.map{clause => Assertion(head +: clause) }
  }
  // Not need for range detection check
  override lazy val groundFact: PackratParser[Seq[Term]] = headLiteral ^^ {
    case head => Seq(head)
  }

  override lazy val clause: PackratParser[Seq[Term]]  = (function | rule | groundFact)
  override lazy val headLiteral: PackratParser[Term] = (infixTerm | structureTerm | funTerm | setTerm | atom)
  override lazy val literal: PackratParser[Term] = (headLiteral | nilAtom)

  override lazy val atom: PackratParser[Term] = // Note: opt(typeDelimiter) is a hack to make #type -> value work
    opt((singleQuotedString | symbol | "?" | "_") <~ typeDelimiter) ~ opt(opt(typeDelimiter) ~ (symbol | singleQuotedString) ~ (attrMapIndexDelimiter | attrMapDelimiter)) ~ (sequence | structureTerm | variable | constant) ^^ { 

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

    case None ~ Some(dlm ~ tpe ~ keyAttr)      ~ Variable(x, cattrName, ctpe, ckey)            =>
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

  override lazy val operatorTerm: PackratParser[Term] = functor ~ "(" ~ atoms <~ ")" ^^ {
    case funct ~ lParen ~ trs => Structure(funct, trs)
  }

  override lazy val symbolTerm: PackratParser[Term] = (constantString | singleQuotedString) ~ "(" ~ atoms <~ ")" ^^ {
    case funct ~ lParen ~ trs => Structure(funct.id, trs)
  }

  lazy val dnTerm: PackratParser[Term] =
    (word("dn") ~ "|") ~> repsep(numeric | singleQuotedString | constantString, ".") ~ opt(".") ^^ {
    case cTerms ~ Some(root) => Structure(StrLit("_seq"), Constant(".") +: cTerms.reverse, StrLit("nil"), StrLit("Dn"))
    case cTerms ~ None       => Structure(StrLit("_seq"), Constant(".") +: cTerms.reverse, StrLit("nil"), StrLit("Dn"))
  }

  override lazy val structureTerm: PackratParser[Term] = opt((singleQuotedString | symbol | "?" | "_") <~ typeDelimiter) ~ opt(opt(typeDelimiter) ~ (symbol | singleQuotedString) <~ (attrMapIndexDelimiter | attrMapDelimiter)) ~ (operatorTerm | symbolTerm) ^^ {
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

  override lazy val expr: PackratParser[Term] = (sequence | structureTerm | variable | constant)

  /* Forms of list:
   * (1) [a, b, c, d] = [a | [b, c, d]] = [a, b | [c, d]] = [a, b, c | [d]] = [a, b, c, d | []].
   * (2) [a | [b]] = [a, b].
   * (3) [a, b, c, d] = [a | [b | X]] = [a, b|X].
   *     X = [c, d]
   * (4) [a, b, c, d] = [A, B|C] = [A | [B|C]].
   *     A = a,
   *     B = b,
   *     C = [c, d].
   * (5) [a, b, c, d] = [A|[B|C]].
   *     A = a,
   *     B = b,
   *     C = [c, d].
   */

  lazy val sequence: PackratParser[Term] = "[" ~> opt(literals) ~ opt("|" ~ (variable | sequence)) <~ "]" ^^ {
    case Some(head) ~ None              =>  
          if(head == Seq()) Structure(StrLit("_seq"), Nil) // TODO: why does the literals return empty seq? check nilAtom
      else head.foldRight(Structure(StrLit("_seq"), Nil)) { (x,y) => Structure(StrLit("_seq"), Seq(x) ++: Seq(y)) }

    case Some(head) ~ Some(cons ~ body) =>  head.foldRight(body) { (x,y) => Structure(StrLit("_seq"), Seq(x) ++: Seq(y)) }
    case None       ~ None              =>  Structure(StrLit("_seq"), Nil)
    case None       ~ Some(x)           =>  throw ParserException(s"Invalid sequence format")
  }

  //============== Functional forms ================================//
  lazy val function: PackratParser[Seq[Term]] = (setFunction | jvmFunction | guardFunction | postFunction | fetchFunction | envFunction | initFunction)

  def conAndGuardTerm(funType: StrLit, head: Term, body: Seq[Term]): Seq[Term] = body.last match {
    case t @ SetTerm(id: StrLit, argRefs, args, certificate, _, _, _) => 
      Constant(funType) +: head +: body.init :+ Structure(funType, head +: t +: Nil)
    case _ => throw ParserException(s"$funType rule should end with a set term")
  }
  /*
  lazy val guardFunction: PackratParser[Seq[Term]] = "defguard" ~ structureTerm ~ logicalIf ~ literals ^^ {
    case predef ~ head ~ logicalIf ~ body => body.last match {
      case t @ SetTerm(id: StrLit, argRefs, args, certificate, _, _, _) => 
        Constant(StrLit("defguard")) +: head +: body.init :+ Structure(StrLit("defguard"), head +: t +: Nil)
      case Constant(StrLit("true"), _, _, _)     => conAndGuardTerm(StrLit("defguard"), head, body.init)
      case Structure(StrLit("spec"), _, _, _, _) => conAndGuardTerm(StrLit("defguard"), head, body.init)
      case other @ _ => throw ParserException(s"defguard rule should end with a set term but $other found")
    }
  }
  */

  //====defguard is relaxed to support any method after query====//
  lazy val guardFunction: PackratParser[Seq[Term]] = "defguard" ~ structureTerm ~ logicalIf ~ literals ^^ {
    case predef ~ head ~ logicalIf ~ body => Constant(StrLit("defguard")) +: head +: body.map {
      case t @ SetTerm(id: StrLit, argRefs, args, certificate, _, _, _) => 
        Structure(StrLit("defguard"), head +: t +: Nil)
      case other: Term => other
    }
  }

  def fetchAndPostTerm(funType: StrLit, head: Term, body: Seq[Term]): Seq[Term] = body.last match {
    case t @ Structure(StrLit("_seq"), terms, _, _, _) =>
      Constant(funType) +: head +: body.init :+ Structure(funType, head +: t +: Nil)
    case other @ _ => throw ParserException(s"$funType rule should end with a sequence but $other found")
  }
  lazy val postFunction: PackratParser[Seq[Term]] = "defpost" ~ structureTerm ~ logicalIf ~ literals ^^ {
    case predef ~ head ~ logicalIf ~ body => body.last match {
      case t @ Structure(StrLit("_seq"), terms, _, _, _) =>
        Constant(StrLit("defpost")) +: head +: body.init :+ Structure(StrLit("defpost"), head +: t +: Nil)
      case Constant(StrLit("true"), _, _, _)     => fetchAndPostTerm(StrLit("defpost"), head, body.init)
      case Structure(StrLit("spec"), _, _, _, _) => fetchAndPostTerm(StrLit("defpost"), head, body.init)
      case other @ _ => throw ParserException(s"defpost rule should end with a sequence but $other found")
    }
  }
  lazy val fetchFunction: PackratParser[Seq[Term]] = "defetch" ~ structureTerm ~ logicalIf ~ literals ^^ {
    case predef ~ head ~ logicalIf ~ body => body.last match {
      case t @ Structure(StrLit("_seq"), terms, _, _, _) =>
        Constant(StrLit("defetch")) +: head +: body.init :+ Structure(StrLit("defetch"), head +: t +: Nil)
      case Constant(StrLit("true"), _, _, _)     => fetchAndPostTerm(StrLit("defetch"), head, body.init)
      case Structure(StrLit("spec"), _, _, _, _) => fetchAndPostTerm(StrLit("defetch"), head, body.init)
      case other @ _ => throw ParserException(s"defetch rule should end with a sequence but $other found")
    }
  }
  lazy val setFunction: PackratParser[Seq[Term]] = "defcon" ~ structureTerm ~ logicalIf ~ literals ^^ {
    case predef ~ head ~ logicalIf ~ body => body.last match {
      case t @ SetTerm(id: StrLit, argRefs, args, certificate, _, _, _) => 
        Constant(StrLit("defcon")) +: head +: body.init :+ Structure(StrLit("defcon"), head +: t +: Nil)
      case Constant(StrLit("true"), _, _, _)     => conAndGuardTerm(StrLit("defcon"), head, body.init)
      case Structure(StrLit("spec"), _, _, _, _) => conAndGuardTerm(StrLit("defcon"), head, body.init)
      case other @ _ => throw ParserException(s"defcon rule should end with a set term but $other found")
    }
  }

  def defunTerm(head: Term, body: Seq[Term]): Seq[Term] = body.last match {
    case t @ FunTerm(id: StrLit, code: String, terms: Seq[Term], compileRef: Class[_], _, _, _) => 
      Constant(StrLit("defun")) +: head +: body.init :+ Structure(StrLit("defun"), head +: t +: Nil)
    case other @ _ => throw ParserException(s"defun rule should end with a set term but $other found")
  }
  lazy val jvmFunction: PackratParser[Seq[Term]] = "defun" ~ structureTerm ~ logicalIf ~ literals ^^ {
    case predef ~ head ~ logicalIf ~ body => body.last match {
      case t @ FunTerm(id: StrLit, code: String, terms: Seq[Term], compileRef: Class[_], _, _, _) => 
        Constant(StrLit("defun")) +: head +: body.init :+ Structure(StrLit("defun"), head +: t +: Nil)
      case Constant(StrLit("true"), _, _, _)     => defunTerm(head, body.init)
      case Structure(StrLit("spec"), _, _, _, _) => defunTerm(head, body.init)
      case other @ _ => throw ParserException(s"defun rule should end with a functional term but $other found")
    }
  }
  lazy val initFunction: PackratParser[Seq[Term]] = "definit" ~ literals ^^ {
    case predef ~ body => Constant(StrLit("definit")) +: Constant(StrLit("query(_*)")) +: body // query(_*) can be anything and acts as a rule head
  }
  lazy val envFunction: PackratParser[Seq[Term]] = "defenv" ~ opt("?") ~> symbol ~ opt("(" ~ ")") ~ logicalIf ~ literals ^^ {
    case head ~ paren ~ logicalIf ~ body => body.last match {
      case Constant(StrLit("true"), _, _, _)     =>
        Constant(StrLit("defenv")) +: Variable(head) +: body.init.init :+ Structure(StrLit("defenv"), Variable(head) +: body.init.last +: Nil)
      case Structure(StrLit("spec"), _, _, _, _) =>
        Constant(StrLit("defenv")) +: Variable(head) +: body.init.init :+ Structure(StrLit("defenv"), Variable(head) +: body.init.last +: Nil)
      case _                            =>
        Constant(StrLit("defenv")) +: Variable(head) +: body.init :+ Structure(StrLit("defenv"), Variable(head) +: body.last +: Nil)
    }
  }

  //lazy val funTerm: PackratParser[Term] = opt("java" | "scala") ~ "`" ~ """[^`\\]*(?:\\.[^`\\]*)*""".r <~ "`" ^^ {
  lazy val funTerm: PackratParser[Term] = opt(symbolConstant) ~ "`" ~ """[^`\\]*(?:\\.[^`\\]*)*""".r <~ "`" ^^ {
    case None ~ lParen ~ body                 => funTermHelper(body)
    case Some(jvmInterpreter) ~ lParen ~ body if((jvmInterpreter == "java") | (jvmInterpreter == "scala")) => 
      funTermHelper(body, s"${jvmInterpreter}")
    case Some(jvmInterpreter) ~ lParen ~ body => throw ParserException(s"Unsupported JVM interpreter language: $jvmInterpreter")
  }

  import java.util.regex.Pattern.quote
  def stripMultiComments(str: String, s: String = "/*", e: String = "*/") = str.replaceAll("(?s)" + quote(s)+".*?" + quote(e), "")
  def stripSingleComments(str: String, markers: String = "//") = str.takeWhile(!markers.contains(_)).trim
  
  @inline
  def funTermHelper(bodyWithComments: String, lang: String = "scala"): FunTerm = {
    // remove duplicates
    // group(1) matches with the Variable without dollar
    val body = stripMultiComments(stripMultiComments(bodyWithComments), "//", "\n")
    val args: Seq[String] = globalVariablePattern.findAllIn(body).matchData.collect {
      case m if !m.group(1).isEmpty => m.group(1) // to handle cases where println(s"$$Self") inside ``.
    }.toSeq.distinct
    val argsWithDollar = args.map(arg => "$" + arg).toArray
    val argsWithQ      = args.map(arg => Variable("?" + arg))
    //val argsWithDollar = args.map(_.toString).toArray
    val compiledRef = jvmContext(lang).compile(body, argsWithDollar)
    FunTerm(StrLit(s"_${lang}"), body, argsWithQ, compiledRef)(jvmContext(lang))
  }

  //============== Set form ================================//
  lazy val setTerm: PackratParser[Term] = immutableSetTerm | mutableSetTerm

  lazy val immutableSetTerm: PackratParser[Term] = "{" ~> "{" ~ opt("""[^}\\]*(?:\\.[^}\\]*)*""".r) ~ "}" <~ "}" ^^ {
    case lparen2 ~ None ~ rparen1                   =>
      Constant(StrLit("nil"))
    case lparen2 ~ Some(slogSource) ~ rparen1       =>
      setTermHelper(slogSource, None, immutable = true)
  }

  lazy val mutableSetTerm: PackratParser[Term] = opt(symbolConstant | variable) ~ "{" ~ opt("""[^}\\]*(?:\\.[^}\\]*)*""".r) <~ "}" ^^ {
    case None ~ lparen ~ None                    =>
      Constant(StrLit("nil"))
    case None ~ lparen ~ Some(slogSource)        =>
      setTermHelper(slogSource, None)
    case Some(name) ~ lparen ~ None             =>
      Constant(StrLit("nil"))        // TODO: is this okay? Under what conditions, we do need to post an empty set?
    case Some(name) ~ lparen ~ Some(slogSource) =>
      setTermHelper(slogSource, Some(name))
  }

  private def setTermHelper(slogSourceWithComments: String, name: Option[Term], immutable: Boolean = false): SetTerm = {
    //val slogSource = stripMultiComments(stripMultiComments(slogSourceWithComments), "//", "\n")
    val slogSource = slogSourceWithComments

    val nameMayBe: Option[Term] = if(immutable) Some(Variable("%ImmutableHash")) else name
    // group(1) matches the variable without dollar
    val args: Seq[String] = {globalVariablePattern.findAllIn(slogSource).matchData.collect {
      case m if !m.group(1).isEmpty => m.group(1)   // to handle cases where println(s"$$Self") inside ``.
    }.toSeq :+ "Self" :+ "SelfKey"}.distinct

    val argsWithQ: Seq[Term] = args.map{x => Variable("?" + x)}
    val argsMap: Map[StrLit, Term] = argsWithQ.map {term => (term.id, term)}.toMap

    val unsignedUnBoundCredential: UnsignedUnBoundCredential = {
      SafelogParserContext("Self").parseUnsignedCredential(slogSource, argsMap, nameMayBe, immutable)
    }

    // setName is a hash of its contents -- helping for testing
    val setName = safe.safelang.model.Identity.base64EncodeURLSafe(
      safe.safelang.model.Identity.hash(slogSourceWithComments.getBytes("UTF-8"), "MD5")
    )
    SetTerm(setName, args.map(x => StrLit(x)), argsWithQ, unsignedUnBoundCredential)(safelogContext)
  }

  /*
  lazy val payload: PackratParser[Term] = encryptedTerms | signedData

  lazy val encryptedTerms: PackratParser[Term] = repsep(encryptedTerm, logicalAnd) ^^ {
    case terms => Structure(StrLit("encrypted"), terms)
  }

  lazy val encryptedTerm: PackratParser[Term] = "recipientId" ~> "(" ~ symbolConstant ~ ")" ~ logicalAnd ~ "encryptedData" ~ "(" ~ (variable | symbolConstant) <~ ")" ^^ {
    case lParen ~ recipientIdValue ~ rParen ~ lAnd ~ eData ~ lParen2 ~ encryptedDataValue =>
      Structure(StrLit("recipientId"), recipientIdValue +: encryptedDataValue +: Nil)
  }

  lazy val signedData: PackratParser[Term] =
    "signedData" ~ "(" ~ repsep(setTerm | structureTerm | variable | constant, logicalAnd) ~ ")" ~ logicalAnd ~ "signature" ~ "(" ~ (variable | symbolConstant) <~ ")" ^^ {

    case funct ~ lParen ~ terms ~ rParen ~ lAnd ~ sig ~ lParen2 ~ sigValue =>
      Structure(StrLit("signedData"), terms +: Structure(StrLit("signature"), sigValue +: Nil) +: Nil)
  }
  */

  lazy val symbolConstant: PackratParser[Term] = singleQuotedString | constantString | doubleQuotedString

  lazy val certificate: PackratParser[SetTerm] = opt((singleQuotedString | symbol | "?" | "_") <~ typeDelimiter) ~ opt(opt(typeDelimiter) ~ (symbol | singleQuotedString) <~ attrMapDelimiter) ~ (signedCertificateTerm) ^^ {
    case None ~ None ~ SetTerm(id, argRefs, args, signedCred, _, _, _)                 => 
      SetTerm(id, argRefs, args, signedCred, StrLit("nil"), termType)(safelogContext)
    case None ~ Some(None ~ attrName) ~ SetTerm(id, argRefs, args, signedCred, _, _, _)     => 
      SetTerm(id, argRefs, args, signedCred, StrLit(attrName.toString), termType)(safelogContext)
    case None ~ Some(dlm ~ tpe) ~ SetTerm(id, argRefs, args, signedCred, _, _, _)      => 
      SetTerm(id, argRefs, args, signedCred, StrLit("nil"), StrLit(tpe.toString))(safelogContext)
    case Some(attrName) ~ None ~ SetTerm(id, argRefs, args, signedCred, _, _, _)            => 
      SetTerm(id, argRefs, args, signedCred, StrLit(attrName.toString), termType)(safelogContext)
    case Some(attrName) ~ Some(dlm ~ tpe) ~ SetTerm(id, argRefs, args, signedCred, _, _, _) => 
      SetTerm(id, argRefs, args, signedCred, StrLit(attrName.toString), StrLit(tpe.toString))(safelogContext)
  }

  lazy val signedCertificateTerm: PackratParser[SetTerm] = symbolConstant ~ "(" ~ "signedData" ~ "(" ~ version ~ logicalAnd ~ speakerStmt ~ logicalAnd ~ subjectStmt ~ logicalAnd ~ validity ~ logicalAnd ~ credential ~ logicalAnd ~ signatureAlgorithm ~ ")" ~ logicalAnd ~ signature ~ ")" ~ "." ^^ {

    case id ~ lParen1 ~ signedDat ~ lParen2 ~ version_ ~ and01 ~ speakerStmt_ ~ and3 ~ subjectStmt_ ~ and4 ~ validity_ ~ and6 ~ signedSetTerm_ ~ and7 ~ sigAlg ~ rParen1 ~ and8 ~ signature_ ~ rParen2 ~ dot =>

     val setTerm = if(signedSetTerm_._3 == "?Encrypted") { // if name matches the built-in name
         /*
         val (name: String, setTermStr: String, immutable: Boolean) = decryptSetTerm(
             subjectStmt_._1
           , subjectStmt_._3
           , signedSetTerm_._4
           , subjectStmt_._4
         )
         parseSignedCredential(s"${speakerStmt_._1}", name, setTermStr)
         */

         throw new UnSafeException(s"Not yet implemented")
       } else {
         // TODO: XXX Note -- ignoring the slog type for now
         val name: String = (signedSetTerm_._1, signedSetTerm_._2, signedSetTerm_._3) match {
           case ("nil", "nil", "nil")            => ""
           case ("nil", "nil", x)                => x
           case ("nil", tpe, x) if !x.isEmpty    => s"#${tpe} -> '$x'"
           case (idToken, tpe, x) if !x.isEmpty  => s"${idToken} #${tpe} -> '$x'"
           case ("nil", tpe, x)                  => s"#${tpe} -> "
           case (idToken, tpe, x)                => s"${idToken} #${tpe} -> "
         }
         //parseSignedCredential(s"'${speakerStmt_._1}'", name, signedSetTerm_._4)
         //parseSignedCredential(s"${speakerStmt_._1}", name, signedSetTerm_._4)
         safe.safelang.Parser.parseSignedCredential(s"${subjectStmt_._1}", name, signedSetTerm_._4)
       }

      val principalSubject = setTerm.principal match {
        case None     => None
        case Some(p)  => Some(Subject(p))
      }

      val signedCredential = SignedCredential(
          id.id.name
        , version_
        , SpeakerStmt(speakerStmt_._1, speakerStmt_._2, speakerStmt_._3)
        , SubjectStmt(subjectStmt_._1, subjectStmt_._2)
        , Validity(validity_._1, validity_._2)
        , setTerm.name
        , setTerm.assertions
        , sigAlg
        , signature_
        , setTerm.queries
        , setTerm.links
        , principalSubject
        , setTerm.speaksFor
        , setTerm.speaksForOn
        , setTerm.setData
        , signedSetTerm_._5
      )

      SetTerm(id.toString, signedCredential)(safelogContext)
  }

  lazy val version: PackratParser[String] = "version" ~ "(" ~ symbolConstant ~ ")" ^^ {
    case enc ~ lParen ~ value ~ rParen => value.id.name
  }
  lazy val speakerStmt: PackratParser[Tuple3[String, String, String]] = "speaker" ~ "(" ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ ")" ^^ {
    case enc ~ lParen ~ speaker ~ land ~ principal ~ land2 ~ speaksForRef ~ rParen => 
      (speaker.id.name, principal.id.name, speaksForRef.id.name)
  }
  lazy val subjectStmt: PackratParser[Tuple2[String, String]] = "subject" ~ "(" ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ ")" ^^ {
    case enc ~ lParen ~ subject ~ land2 ~ principal ~ rParen => 
      (subject.id.name, principal.id.name)
  }
  lazy val recipientStmt: PackratParser[Tuple4[String, String, String, String]] = "recipient" ~ "(" ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ ")" ^^ {
    case enc ~ lParen ~ subject ~ land2 ~ principal ~ land3 ~ sharedEncryptedKey ~ land4 ~ encryptionAlgorithm ~ rParen => 
      (subject.id.name, principal.id.name, sharedEncryptedKey.id.name, encryptionAlgorithm.id.name)
  }
  lazy val validity: PackratParser[Tuple3[String, String, String]] = "validity" ~ "(" ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ logicalAnd ~ symbolConstant ~ ")" ^^ {
    case enc ~ lParen ~ notBefore ~ land ~ notAfter ~ rand ~ refresh ~ rParen => (notBefore.id.name, notAfter.id.name, refresh.id.name)
  }
  lazy val signatureAlgorithm: PackratParser[String] = "signatureAlgorithm" ~ "(" ~ symbolConstant ~ ")" ^^ {
    case enc ~ lParen ~ value ~ rParen => value.id.name
  }
  lazy val signature: PackratParser[String] = "signature" ~ "(" ~ symbolConstant ~ ")" ^^ {
    case enc ~ lParen ~ value ~ rParen => value.id.name
  }

  lazy val credential: Parser[(String, String, String, String, Boolean)] = 
    opt((singleQuotedString | symbol | "?" | "_") <~ typeDelimiter) ~ opt(opt(typeDelimiter) ~ (symbol | singleQuotedString) <~ attrMapDelimiter) ~ (credentialTerm) ^^ {

    case None ~ None ~ setContents                   => 
      ("nil", termType.toString, setContents._1, setContents._2, setContents._3)
    case None ~ Some(None ~ attrName) ~ setContents       => 
      (attrName.toString, termType.toString, setContents._1, setContents._2, setContents._3)
    case None ~ Some(dlm ~ tpe) ~ setContents        => 
      ("nil", tpe.toString, setContents._1, setContents._2, setContents._3)
    case Some(attrName) ~ None ~ setContents              => 
      (attrName.toString, termType.toString, setContents._1, setContents._2, setContents._3)
    case Some(attrName) ~ Some(dlm ~ tpe) ~ setContents   => 
      (attrName.toString, tpe.toString, setContents._1, setContents._2, setContents._3)
  }

  lazy val credentialTerm: Parser[(String, String, Boolean)] = signedSetTerm | encryptedSetTerm

  lazy val signedSetTerm: Parser[(String, String, Boolean)] = signedImmutableSetTerm | signedMutableSetTerm

  lazy val signedImmutableSetTerm: PackratParser[(String, String, Boolean)] = 
    opt(symbolConstant) ~ "{" ~ "{" ~ opt("""[^}\\]*(?:\\.[^}\\]*)*""".r) ~ "}" <~ "}" ^^ {

    case Some(setName) ~ lparen1 ~ lparen2 ~ None        ~ rparen1 => (setName.id.name, "", true)
    case Some(setName) ~ lparen1 ~ lparen2 ~ Some(stmts) ~ rparen1 => (setName.id.name, stmts, true)
    case None          ~ lparen1 ~ lparen2 ~ Some(stmts) ~ rparen1 => ("", stmts, true)
    case None          ~ lparen1 ~ lparen2 ~ None        ~ rparen1 => throw ParserException(s"set cannot be empty")
  }

  lazy val signedMutableSetTerm: PackratParser[(String, String, Boolean)] = 
    opt(symbolConstant) ~ "{" ~ opt("""[^}\\]*(?:\\.[^}\\]*)*""".r) <~ "}" ^^ {

    case Some(setName) ~ lparen ~ None        => (setName.id.name, "", false)
    case Some(setName) ~ lparen ~ Some(stmts) => (setName.id.name, stmts, false)
    case None          ~ lparen ~ Some(stmts) => ("", stmts, false)
    case None          ~ lparen ~ None        => throw ParserException(s"set cannot be empty")
  }

  lazy val encryptedSetTerm: Parser[(String, String, Boolean)] = (symbolConstant) ^^ {
    case encStr => ("?Encrypted", encStr.id.name, false)
  }

  def decryptSetTerm(
      recipientId: String
    , encryptedSharedKey: String
    , encryptedSetTerm: String
    , encryptionAlgorithm: String
  ): Tuple3[String, String, Boolean] = {

    /** envContext passed for Parser?
    val principal: model.Principal = safe.safelang.InferenceService.envContext.get(StrLit("Selfie")) match {
      case Some(p: model.Principal) => p
      case _                        => throw ParserException(s"cannot sign since principal (Selfie) is undefined")
    }
    */

    //=============== TODO: TEMP ================//
    val principal: model.Principal = if(encryptionAlgorithm != "true") {
      throw ParserException(s"Not implemented; envContext import issue needs to be resolved first")
    } else throw ParserException(s"Not implemented; envContext import issue needs to be resolved first")
    //=============== TEMP ================//

    val signedData: String = SafelogParserContext(principal.subject.toString).parseEncryptedSignedCredential(
        principal
      , recipientId
      , encryptedSharedKey
      , encryptedSetTerm
      , encryptionAlgorithm
    )
    logger.debug(s"SignedData: $signedData")
    parseSignedSetTerm(signedData)
  }

  /*
  def parseSignedCredential(speaker: String, name: String, stmts: String): SignedCredentialHolder = (name, stmts) match {
    case ("", "") => throw ParserException(s"identity set cannot be empty")
    case ("", _) => SafelogParserContext(speaker).parseSignedCredential(speaker, "", stmts)
    case (_, "") => SignedCredentialHolder(name, "", Map.empty[Index, Set[Statement]], Nil, Nil, None, Nil, Nil)
    case _       => SafelogParserContext(speaker).parseSignedCredential(speaker, name, stmts)
  }
  */

  //=========== Parsing certificate from safesets =====//
  private[this] def parseSignedSetTerm(source: String): Tuple3[String, String, Boolean] = {
    val res = parseAll(signedSetTerm, source) match {
      case Success(result, _) =>  // result: Term
        result 
      case failure: NoSuccess => throw ParserException(s"${failure.msg}")
    }
    res
  }

  private[safe] def parseCertificate(source: String): SetTerm = {
    val res = parseAll(certificate, source) match {
      case Success(result, _) =>  // result: Term
        result 
      case failure: NoSuccess => throw ParserException(s"${failure.msg}")
    }
    res
  }

  private[safe] def parseCertificate(source: java.io.Reader): SetTerm = {
    val res = parseAll(certificate, source) match {
      case Success(result, _) =>  // result: Term
        result 
      case failure: NoSuccess => throw ParserException(s"${failure.msg}")
    }
    res
  }

  private[safe] def parseFileAsCertificate(fileName: String): SetTerm = {
    val source = new java.io.BufferedReader(new java.io.FileReader(fileName))
    val res = parseCertificate(source)
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
      val envSeq         = _result.get(StrLit("defenv0")).getOrElse(Nil).toSeq
      _result           -= StrLit("defenv0")
      val initSeq        = _result.get(StrLit("definit0")).getOrElse(Nil).toSeq
      _result           -= StrLit("definit0")
      val retractionSeq  = _result.get(StrLit("_retraction")).getOrElse(Nil).toSeq
      _result           -= StrLit("_retraction")
      val allQueries     = envSeq.map {stmt => Query(stmt.terms.tail)} ++ initSeq.map {stmt => Query(stmt.terms.tail)} ++ querySeq
      Tuple4(_result.map {kv => (kv._1, kv._2.toSet)}.toMap, allQueries, importSeq, retractionSeq)
    case failure: NoSuccess => throw ParserException(s"${failure.msg}")
  }

  override def parseAsSegments(
    source: String
  ): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]] = {
    parseAsSegmentsHelper(parseAll(program, source))
  }

  override def parseAsSegments(
    source: java.io.Reader
  ): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]] = {
    parseAsSegmentsHelper(parseAll(program, source))
  }

  override def parseFileAsSegments(
    fileName: String
  ): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]] = {
    val source = new java.io.BufferedReader(new java.io.FileReader(fileName))
    val res = parseAsSegments(source)
    source.close()
    res
  }
}
