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

import safe.runtime.JVMInterpreter
import safe.safelog._
import safe.cache.SafeCache

import model._

trait FunctionLike {
  /** evalFunction() executes arbitrary code for jvm-based languages */
  def evalFunction(func: StrLit)(envContext: SafeCache[StrLit, EnvValue]): Term = Constant(StrLit("nil"))
  def evalSet(func: StrLit)(
      envContext: SafeCache[StrLit, EnvValue]
    , proofContext: SafeCache[SetId, ProofSubContext]
  ): Term = Constant(StrLit("nil"))
}

case class SlogResult(
    statements: Seq[Set[Statement]]
  , id: StrLit = StrLit("_")
  , attrName: StrLit = StrLit("nil")
  , tpe: StrLit = termType
  , indexAndEncode: Encoding = Encoding.Attr
) extends Term {
  override def toString(): String = {
     val resStr        = statements.flatten.map(x => x.toStringWithSays()).mkString(".\n  ")
    val resStrWithDot = if(resStr.isEmpty) resStr else s"${resStr}."
    s"""{
     |  $resStrWithDot
     |}""".stripMargin

  }
  def transposeToSeq(): Term = { 
    val slogResultTerms: Seq[Term] = statements.flatten.map(x => x.terms.head)
    val res = if(slogResultTerms.length == 1) {
        Structure(StrLit("_seq"), Seq(slogResultTerms.head, Structure(StrLit("_seq"), Nil)))
      } else slogResultTerms.foldRight(Structure(StrLit("_seq"), Nil)) { 
       (x,y) => Structure(StrLit("_seq"), Seq(x) ++: Seq(y))
    } 
    res
  } 
}

import scala.language.existentials // needed for existential type Class[_] due to a bug in Scala 
                                   // (Ref: https://issues.scala-lang.org/browse/SI-6541)
case class FunTerm(
    id: StrLit
  , code: String
  , args: Seq[Term]
  , compiledRef: Class[_]
  , attrName: StrLit = StrLit("nil")
  , tpe: StrLit = StrLit("scala")
  , indexAndEncode: Encoding = Encoding.Attr
)(implicit context: JVMInterpreter) extends Term with FunctionLike {

  override def primaryIndex(): StrLit = {
    val digest: Array[Byte] = model.Identity.hash(code.getBytes(StringEncoding), "SHA-1")
    // take the most significant 16 bits

    // BigInteger(int signum, byte[] magnitude)
    // Translates the sign-magnitude representation of a BigInteger into a
    // BigInteger. The sign is represented as an integer signum value: -1 for
    // negative, 0 for zero, or 1 for positive. The magnitude is a byte array
    // in big-endian byte-order: the most significant byte is in the zeroth
    // element. A zero-length magnitude array is permissible, and will result
    // in a BigInteger value of 0, whether signum is -1, 0 or 1. 
    StrLit(new java.math.BigInteger(1, digest).toString(16) + args.length)
  }
  override def bind(f: StrLit => Term): FunTerm = this.copy(args = args.map(_.bind(f)))
  override def toString(): String = "`main( args(" + args.mkString(", ") + ") ) {" + code + "}`"
  override def eval(): NumericConstant = {
    val boundedValues: Array[String] = args.map(arg => arg.id.name).toArray
    val result = context.eval(compiledRef, boundedValues).asInstanceOf[NumericConstant]
    result
  }
  override def evalFunction(func: StrLit = StrLit("_"))(envContext: SafeCache[StrLit, EnvValue]): Term = {
    val boundedValues: Array[String] = args.map {
      case arg: Constant => arg.id.name
      case arg: Variable => 
        envContext.get(arg.simpleName).getOrElse(
          throw UnSafeException(s"Unbound variable passed to a function: $arg")
        ).toString
    }.toArray
    val result = context.eval(compiledRef, boundedValues).toString()
    // if result starts with [ .. ], then treat it as a list
    SlangTerm.toSlangSeq(result)
  }
}

case class SetTerm(
    id: StrLit
  , argRefs: Seq[StrLit]
  , args: Seq[Term]
  , credential: Credential
  , attrName: StrLit = StrLit("nil")
  , tpe: StrLit = StrLit("safe")
  , indexAndEncode: Encoding = Encoding.Attr
)(implicit slogContext: SafelogContext) extends Term with FunctionLike {

  override def bind(f: StrLit => Term): SetTerm = this.copy(
    args = args.map {
      case c: Constant => c         // if already substituted locally, return
      case v @ _       => v.bind(f)
    }
  )
  
  override def evalSet(func: StrLit = StrLit("defguard"))(
      envContext: SafeCache[StrLit, EnvValue]
    , proofContext: SafeCache[SetId, ProofSubContext]
  ): Term = func match {

    case StrLit("defcon")   => credential match {
      case unsignedUnBoundCredential: UnsignedUnBoundCredential=>

        //println(s"defcon: argsRefs: $argRefs; args: $args")

        val cred: UnsignedCredential = SlangTerm.bindEnvVarInSlog(
            argRefs.zip(args).toMap
          , unsignedUnBoundCredential
        )(envContext)

        //println(s"defcon after binding: argsRefs: $argRefs; args: $args; stmts: ${cred.statements}")

        envContext.get(cred.name.id) match {
          case None => 
            envContext.put(cred.name.id, this.copy(id = cred.name.id, credential = cred))
            proofContext.put(cred.name.id, ProofSubContext(cred.name.id, cred.statements))
          case Some(existingCred: SetTerm) =>
            //val mergedStatements = SlangTerm.mergeStatements(existingContext.statements, cred.statements)
            //proofContext.put(cred.name.id, existingContext.copy(statements = mergedStatements))
            // we put in both so that we can use in both inference and post
            val mergedCred = cred.mergeCredential(existingCred.credential)
            envContext.put(cred.name.id, this.copy(credential = mergedCred))
            proofContext.put(cred.name.id, ProofSubContext(cred.name.id, mergedCred.statements))
          case other => throw UnSafeException(s"Possible name clash with env variable and a set term ${cred.name.id}: $other")
        }
        cred.name
      case _       => throw UnSafeException(s"unsigned credential expected but something else found: $credential")
    }

    case StrLit("defpost")  => credential match {
      case unsignedUnBoundCredential: UnsignedUnBoundCredential =>
        val principal: Principal = envContext.get(StrLit("Selfie")) match {
          case Some(p: Principal) =>
	    envContext.put(StrLit("Self"), Constant(StrLit(s"${p.scid.toString}"), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64))
	    envContext.put(StrLit("SelfKey"), Constant(StrLit(s"${p.subject.toString}"), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64))
            p
          case _                  => throw UnSafeException(s"cannot sign since principal (Selfie) not defined")
        }
        val cred: UnsignedCredential = SlangTerm.bindEnvVarInSlog(
            argRefs.zip(args).toMap
          , unsignedUnBoundCredential
        )(envContext)

        // sign the credential
        val (setId, cert) = cred.signAndEncode(principal)
        //println(s"signedCredential: $cert")
        //Constant(setId)
        Constant(cert)
    }

    case StrLit("defguard") => credential match {
      case unsignedUnBoundCredential: UnsignedUnBoundCredential =>

        //println(s"defguard: argsRefs: $argRefs; args: $args; stmts: ${credential.statements}")

        val cred: UnsignedCredential = SlangTerm.bindEnvVarInSlog(argRefs.zip(args).toMap, unsignedUnBoundCredential)(envContext)
        //println(s"defguard after binding: argsRefs: $argRefs; args: $args; stmts: ${cred.statements}")
        val importAll: Set[StrLit] = {
	  val importAllSeq: Seq[Term] = cred.imports.collect { 
            case Assertion(terms) => terms.head match {
	      case Structure(StrLit("importAll"), spkr +: Structure(StrLit("_seq"), importSeq, _, _, _) +: other, _, _, _) => 
                //importSeq
                val importAllKey = StrLit("importAll")
                var _mergedStmts: Map[Index, Set[Statement]] = proofContext.get(importSeq.head.id).get.statements
                importSeq.tail.foreach { imprt =>
                  //println(s"Import: $imprt")
                  val currentCred = proofContext.get(imprt.id).get
                  _mergedStmts = SlangTerm.mergeStatements(currentCred.statements, _mergedStmts)
                }
                proofContext.put(importAllKey, ProofSubContext(_mergedStmts))
                //println(s"""ProofContext importAll: ${proofContext.get(importAllKey).get.statements.values.mkString("\n")}""")
                /*
                proofContext.get(importAllKey).get.statements foreach {
                  case (idx, stmtSet) => println(stmtSet.map(_.toStringWithSays()).mkString("\n"))
                }
                */
                Seq(Constant(importAllKey))
	      case Structure(StrLit("import"), spkr +: Structure(StrLit("_seq"), importSeq, _, _, _) +: other, _, _, _) => 
                importSeq
	      case Structure(StrLit("import"), spkr +: importSeq, _, _, _) => 
                importSeq
            }
            case Retraction(terms) => terms.head match {
	      case Structure(StrLit("importAll"), spkr +: Structure(StrLit("_seq"), importSeq, _, _, _) +: other, _, _, _) => 
                importSeq.map(item => proofContext.remove(item.id))
                importSeq
	      case Structure(StrLit("import"), spkr +: importSeq, _, _, _) => 
                importSeq.map(item => proofContext.remove(item.id))
                importSeq
            }
	  }.flatten
          importAllSeq.map(t => StrLit(Term.stripQuotes(t.id.name))).toSet
	}
        
        //val importAll = cred.imports.toSet.map{i: String => StrLit(Term.stripQuotes(i))}
        val queries: Seq[Statement] = cred.queries

        val localContext: Map[Index, Set[Statement]] = SlangTerm.getDefConStatements(importAll, cred.statements)(envContext)

        val timerStart = System.nanoTime()
        val res: Seq[Seq[Statement]] = slogContext.solveWithContext(
             contextIds = importAll
           , queries = queries
           , isInteractive = false
          )(
             envContext = envContext
           , localSlangContext = localContext
           , proofContext = proofContext
          )
        val timerEnd = System.nanoTime()
        val slogResult = if(res.flatten.isEmpty) Constant("false") else SlogResult(res.map(x => x.toSet))
        slogResult
       case other     => throw UnSafeException(s"Undefined credential, $credential in evalSet")
     }
     case other     => throw UnSafeException(s"Undefined func, $other in evalSet")
   }

  override def toString(): String = {
    s"""SetTerm(id = $id; argRefs = ${argRefs.mkString(",")}; args = ${args.mkString(",")}; credential = ${credential.toString}"""
  }
}

object SetTerm {
  def apply(id: String, credential: Credential)(implicit slogContext: SafelogContext) = 
    new SetTerm(StrLit(id), Nil, Nil, credential)(slogContext)
  def apply(id: String, argRefs: Seq[StrLit], argTerms: Seq[Term], credential: Credential)(implicit slogContext: SafelogContext) = 
    new SetTerm(StrLit(id), argRefs, argTerms, credential)(slogContext)
}

object SlangTerm extends safe.safelog.TermLike {
  def toSlangSeq(str: String): Term = {
    val listExpr = """^\[(.*?)\]$""".r
    val listTerm = str match {
      case listExpr(elemStr) =>
        val elems = elemStr.split("""\s*,\s*""").toSeq.map(x => Constant(x))
        val test = elems.foldRight(Structure(StrLit("nil"), Nil)) { (x,y) => Structure(StrLit("_seq"), Seq(x) ++: Seq(y)) }
        normalizeTerms(Seq(test)).head // TODO: move this?
      case _ => Constant(str)
    }
   listTerm
  }

  def mergeStatements(stmtSet1: Map[Index, Set[Statement]], stmtSet2: Map[Index, Set[Statement]]): Map[Index, Set[Statement]] = {
    ((stmtSet1.keySet ++ stmtSet2.keySet).map {
      case i: Index =>
	val resultStatements: Set[Statement] = {
	  stmtSet1.get(i).getOrElse(Set.empty) ++ 
	  stmtSet2.get(i).getOrElse(Set.empty)
	}
	i -> resultStatements
    }).toMap
  }

  // TODO: check this
  def getDefConStatements(importAll: Set[StrLit], defguardLocalStatements: Map[Index, Set[Statement]])(
      envContext: SafeCache[StrLit, EnvValue]
  ): Map[Index, Set[Statement]] = {

    /*
    var _localContext: Map[Index, Set[Statement]] = defguardLocalStatements

    // build local context if set is issued only through defcon
    importAll.foreach {
      case setId if(envContext.containsKey(setId)) =>
	val defconSetStatements: Map[Index, Set[Statement]] = envContext.get(setId) match {
	  case Some(s: SetTerm) => s.credential match {
	     case unSignedCred: UnsignedCredential => unSignedCred.statements
	     case other => throw UnSafeException(s"UnsignedCredential expected but something else found: $other")
	  }
	  case other            => throw UnSafeException(s"Set term expected but something else found: $other")
	}
	_localContext = _localContext ++ ((_localContext.keySet ++ defconSetStatements.keySet).map {
	  case i: Index =>
	    val resultStatements: Set[Statement] = {
	      _localContext.get(i).getOrElse(Set.empty) ++ 
	      defconSetStatements.get(i).getOrElse(Set.empty)
	    }
	    i -> resultStatements
	}).toMap

      case _ => // no-op
    }
    _localContext.toMap
    */
    defguardLocalStatements
  }

  // [a,b] to Seq(a, b)
  def unfoldSeq(term: Term): Seq[Term] = term match {
    case Structure(StrLit("nil"), Nil, _, _, _) =>  Nil
    case Structure(StrLit("_seq"), term +: more +: Nil, _, _, _) => term +: unfoldSeq(more)
    //case _ => sys.error(s"unexpected term: $term") // should never happen
    case _ => Seq(term)
  }

  /* listify: converts a canonical list to its standard form
   * Examples: .(a,.(b,[])) == [a,b]. 
   *           .(.(.(a,[]),[]),[]) == [[[a]]].
   *           .(.(a,.(b,[])),.(c,[])) == [[a,b],c].
   *             .(.(a,[]),.(.(b,.(c,[])),[])) == [[a],[b,c]].
   *           .(a,.(b,.(f(d,e),[]))) == [a,b,f(d,e)].
   */
  private def listify(terms: Seq[Term]): Seq[Term] = {
    //@annotation.tailrec
    def loop(terms: Seq[Term]): Seq[Term] = terms match {
      case Structure(StrLit("_seq"), subTerms, _, _, _) +: rest => subTerms match {
        case Structure(StrLit("_seq"), moreSubTerms, _, _, _) +: tail => listify(Seq(subTerms.head)) ++: loop(subTerms.tail) ++: loop(rest)
        case _ => loop(subTerms) ++: loop(rest)
      }
      case Structure(name, subTerms, _, _, _) +: rest => Structure(name, loop(subTerms)) +: loop(rest) // if name, apply the predicate
      case head +: rest => head +: loop(rest) // else append the head to the list and loop
      case Nil => Nil
    }
    Seq(Structure(StrLit("_seq"), loop(terms)))
  }

  /* 
   * normalizeTerms: build the term tokens from the given statement (Structure, Constant, List)
   */
  def normalizeTerms(stmt: Seq[Term]): Seq[Term] = stmt match {
    case term :: rest => term match {
      case Structure(StrLit("_seq"), _, _, _, _) => listify(Seq(term)) ++: normalizeTerms(rest) // is it a list?
      case Structure(name, terms, _, _, _) => Structure(name, normalizeTerms(terms)) +: normalizeTerms(rest) // is it a predicate name?
      case _ => Seq(term) ++: normalizeTerms(rest) // then simply a Constant
    }
    case Nil => Nil
  }

  override def isGrounded(term: Term): Boolean = term match {
    case c: Constant => true
    case v: Variable => false
    case Structure(id, terms, _, _, _) =>
      val resMap = terms.map{x => isGrounded(x)} // TODO: doing more computation than necessary
      if(resMap.contains(false)) false else true
    case f: FunTerm => true
    case s: SetTerm => true
    case _ => false
  }

  def substEnvVar(term: Term)(envContext: SafeCache[StrLit, EnvValue]): Term = term match {
    case v @ Variable(id, attrName, tpe, _) if(v.isEnvVariable) => envContext.get(v.simpleName) match {
      case None                 => throw UnSafeException(s"Env variable $term not defined")
      case Some(c: Constant)    => Constant(StrLit(c.id.name), c.attrName, c.tpe, c.indexAndEncode)
      case Some(c)              => Constant(c.toString, attrName, tpe)
    }
    case Structure(id, terms, attrName, tpe, _) => Structure(id, terms.map(t => substEnvVar(t)(envContext)), attrName, tpe)
    case _            => term
  }

  // the global variables in slog with local scope of slang must be already bound; 
  // here, we are binding only the remaining variables which must be either env variables or not defined
  private def bindAllSetTermArgs(args: Map[StrLit, Term])(
    envContext: SafeCache[StrLit, EnvValue]
  ): Map[StrLit, Term] = args map {
    case (envVar: StrLit, envValue: Constant) => 
      (StrLit(s"$$${envVar.name}"), envValue)
    case (envVar: StrLit, envValue: Variable) if (envVar == StrLit("Self") | envVar == StrLit("SelfKey")) => 
      (envVar, envValue)
    case (envVar: StrLit, envValue: Structure) if (envValue.id == StrLit("_seq")) => // to accomodate importAll case
      val res = if(envValue.tpe == StrLit("Dn")) {
        (StrLit(s"$$${envVar.name}"), envValue)
      } else {
        val unfoldedTerms: Seq[Term] = SlangTerm.unfoldSeq(envValue)
        if(unfoldedTerms.head.id.name == "_seq") {
          (StrLit(s"$$${envVar.name}"), unfoldedTerms.head)
        }
        else (StrLit(s"$$${envVar.name}"), Structure(StrLit("_seq"), unfoldedTerms))
      }
      res
    case other => throw UnSafeException(s"Variable in slog is not bound: $other")
  }

  // TODO: This can go away
  def bindEnvVarInSlogPerVariable(
      args: Map[StrLit, Term]
    , unsignedUnBoundCredential: UnsignedUnBoundCredential
  )(implicit 
     envContext: SafeCache[StrLit, EnvValue]
  ): UnsignedCredential = {

    val unsignedBoundCredential: UnsignedUnBoundCredential = {
      var _unsignedBoundCredential: UnsignedUnBoundCredential = unsignedUnBoundCredential
      args.foreach {
        case (envVar: StrLit, envValue: Constant) => 
          _unsignedBoundCredential = _unsignedBoundCredential.bind(
            v => if (v.name == "$" + envVar.name) envValue else Variable(v)
          )
        case (envVar: StrLit, envValue: Variable) if (envVar == StrLit("Self") | envVar == StrLit("SelfKey")) => 
          _unsignedBoundCredential
        case (envVar: StrLit, envValue: Structure) if (envValue.id == StrLit("_seq")) => // to accomodate importAll case
          val unfoldedTerms: Seq[Term] = SlangTerm.unfoldSeq(envValue)
          _unsignedBoundCredential = _unsignedBoundCredential.bind{ v =>
             if (v.name == "$" + envVar.name) Structure(StrLit("_seq"), unfoldedTerms) else Variable(v)
          }
        case other => throw UnSafeException(s"Variable in slog is not bound: $other")
      }
      _unsignedBoundCredential
    }
    val cred: UnsignedCredential = unsignedBoundCredential.buildUnsignedCredential()
    cred
  }

  // performs binding in single scan rather than scanning per variable
  def bindEnvVarInSlog(
      args: Map[StrLit, Term]
    , unsignedUnBoundCredential: UnsignedUnBoundCredential
  )(implicit 
     envContext: SafeCache[StrLit, EnvValue]
  ): UnsignedCredential = {
     val bindedArgs: Map[StrLit, Term] = bindAllSetTermArgs(args)(envContext)
     unsignedUnBoundCredential.bind(bindedArgs)
  }

  def substQueries(args: Seq[Term], unsignedUnBoundCredential: UnsignedUnBoundCredential)(
    envContext: SafeCache[StrLit, EnvValue]
  ): Seq[Statement] = {

    // perform only when queries contains the variables $Self or $SelfKey
    val envMap: Map[StrLit, Term] = args.zipWithIndex.map { // TODO: may be slow to use zipWithIndex
      case (v: Variable, attrName: Int) =>
	envContext.get(v.id) match {
	  case None    => throw UnSafeException(s"Variable $$$v is undefined") // not yet resolved
	  case Some(c) => (StrLit("$" + v.id.name), Constant(c.toString))
	}
    }.toMap
    //println(s"envMap: $envMap")
    //val cred: UnsignedCredential = unsignedUnBoundCredential.substEnv(envMap)
    val cred: UnsignedCredential = unsignedUnBoundCredential.buildUnsignedCredential()
    // TODO: for each env call bind
    //println(s"allqueries: ${cred.queries}")
    cred.queries
  }

  def getFromEnvContext(varTerm: Variable)(envContext: SafeCache[StrLit, EnvValue]): Term = {
    val term = envContext.get(varTerm.simpleName) match { 
      case None                 => throw UnSafeException(s"Env variable $varTerm not defined")
      case Some(c: Constant)    => Constant(StrLit(c.id.name), c.attrName, c.tpe, c.indexAndEncode)
      case Some(c)              => Constant(c.toString)
    }
    term
  }

  // find the most general unifier for two terms
  def mostGenericUnifier(lhs: Term, rhs: Term)(envContext: SafeCache[StrLit, EnvValue]): Option[Term => Term] = {
    @inline
    def mguEnvVarInInterpolation(terms: Seq[Term]): Seq[Term] = {
      val termBinded = terms.map {
        case v: Variable if v.id.name.startsWith("$") =>
          val const: Term = getFromEnvContext(v)(envContext)
          const
        case other => other
      }
      termBinded
    }
    @inline
    def mguEnvVar(varTerm: Variable, lhs: Seq[Term], rhs: Seq[Term], unifier: Term => Term): Option[Term => Term] = {
      val const: Term = getFromEnvContext(varTerm)(envContext)
      val r = subst(varTerm.id, const)_ compose unifier
      recurse(lhs.map(r), rhs.map(r), r)
    }

    @annotation.tailrec
    def recurse(lhs: Seq[Term], rhs: Seq[Term], unifier: Term => Term): Option[Term => Term] = (lhs, rhs) match {
      // empty lists? no more work
      case (Nil, Nil) => Some(unifier)

      // TODO: occurs check which is necessary to prevent unification of the
      // terms s(X) and X that can lead to infinite recursion

      // anything unifies with a variable
      case (term +: tail1, (v @ Variable(id, _, _, _)) +: tail2) if(id.name != "Self") =>
        if(id.name.startsWith("^")) { // regex
          mguRegex(id, term, tail1, tail2, unifier)
        } else if(id.name.startsWith("$")) { // envVar
          mguEnvVar(v, lhs, rhs, unifier)
        } else {
          term match {
            case s @ Structure(StrLit("_interpolate"), Constant(body, _, _, _) +: Constant(termSeq, _, _, _) +: xterms, _, _, _) =>
              val bindedTerm = s.copy(terms = s.terms(0) +: s.terms(1) +: mguEnvVarInInterpolation(xterms))
              val r =  subst(id, bindedTerm)_ compose unifier
              recurse(tail1.map(r), tail2.map(r), r)
            case _ =>
              val r =  subst(id, term)_ compose unifier // given two partial functions, f and g, compose will return g(f(x))
              recurse(tail1.map(r), tail2.map(r), r)
          }
        }
      case ((v @ Variable(id, _, _, _)) +: tail1, term +: tail2) if(id.name != "Self")  =>
        if(id.name.startsWith("^")) { // regex
          mguRegex(id, term, tail1, tail2, unifier)
        } else if(id.name.startsWith("$")) { // envVar
          mguEnvVar(v, lhs, rhs, unifier)
        } else {
          term match {
            case s @ Structure(StrLit("_interpolate"), Constant(body, _, _, _) +: Constant(termSeq, _, _, _) +: xterms, _, _, _) =>

              val bindedTerm = s.copy(terms = s.terms(0) +: s.terms(1) +: mguEnvVarInInterpolation(xterms))
              val r =  subst(id, bindedTerm)_ compose unifier
              recurse(tail1.map(r), tail2.map(r), r)
            case _ =>
              val r =  subst(id, term)_ compose unifier // given two partial functions, f and g, compose will return g(f(x))
              recurse(tail1.map(r), tail2.map(r), r)
          }
        }
      // constants must match
      case (Constant(id1, _, _, _) +: tail1, Constant(id2, _, _, _) +: tail2) =>
        if (id1 == id2) recurse(tail1, tail2, unifier) else None

      // compounds must have matching atoms and matching arity
      // then arguments can be added to the list of values to check
      case (Structure(id1, term1, _, _, _) +: tail1, Structure(id2, term2, _, _, _) +: tail2) =>
        if (lhs.head.arity == rhs.head.arity && lhs.head.id == rhs.head.id) {
          // check for interpolation and envVar substitution
          (id1.name, id2.name) match {
            case ("_is" | "_unify", _) =>
              recurse(Seq(term1.head), term1.tail, unifier) // special case for is and =
            case (_, _) => recurse(term1 ++: tail1, term2 ++: tail2, unifier)
          }
        }
        else None

      // anything else cannot be unified
      case (_, _) => None
    }
    recurse(Seq(lhs), Seq(rhs), x => x)
  }
}

/*
case class SetTerm(
    id: StrLit
  , statements: Set[Statement]
)(implicit slogContext: SafelogContext) extends Term {
}
object SetTerm {
  def apply(id: String, statements: Set[Statement])(implicit slogContext: SafelogContext) = new SetTerm(StrLit(id), statements)(slogContext)
}


case class CredentialTerm(
    id: StrLit
  , credential: Credential
)(implicit slogContext: SafelogContext, self: Principal) extends Term {
}

object CredentialTerm {
  def apply(id: String, credential: Credential)(implicit slogContext: SafelogContext, self: Principal) = new CredentialTerm(StrLit(id), credential)(slogContext)(slogContext, self)
  def apply(credential: Term)(implicit slogContext: SafelogContext, self: Principal) = credential match {
    case Structure(id, Structure(StrLit("encoding"), Constant(encoding) +:  Nil) +: Structure(StrLit("signedData"), signedTerms) +: Structure(StrLit("signature"), Constant(signature) +: Nil)) => 
      new CredentialTerm(id, createCredential(signedTerms, encoding.canonicalName, signature.canonicalName))(slogContext, self)
    case Structure(id, Structure(StrLit("encoding"), Constant(encoding) +:  Nil) +: Structure(StrLit("recipientId"), recipientId) +: Structure(StrLit("encryptedData"), Constant(encryptedData) +: Nil) +: other) => 
      val credential: Credential = 
        if(recipientId.canonicalName == self.scid) {
          val signedData = decryptData(encryptedData.canonicalName, self)
          val signedTerms = parseCredential(signedData.canonicalName + ".")
          createCredential(signedTerms)
        } else {
          createCredential(findRecipient(other), encoding.canonicalName, signature.canonicalName)
        }
      new CredentialTerm(id, credential)(slogContext, self)
    case _ => throw ParserException(s"Certificate encoding not recognized") // TODO: add X509 support
  }

  @annotation.tailrec
  private def findRecipient(terms: Seq[Term]): Seq[Term] = terms match {
    case Structure(StrLit("recipientId"), recipientId) +: Structure(StrLit("encryptedData"), Constant(encryptedData) +: Nil) +: tail =>
      val credential: Credential =
       if(recipientId == self.scid) {
         val signedData = decryptData(encryptedData.canonicalName, self)
         parseCredential(signedData.canonicalName + ".")
       } else {
         findRecipient(tail)
       }
    case Nil => Nil
  }

  private def createCredential(signedTerms: Seq[Term], encoding: String, signature: String): Credential = signedTerms match {
    case    Structure(StrLit("status"), Constant(state) +: Constant(publishedDate) +: Constant(msg) +: Nil)
         +: Structure(StrLit("speakerId"), Constant(speakerId) +: Nil)
         +: Structure(StrLit("subjectId"), Constant(subjectId) +: Nil)
         +: Structure(StrLit("notBefore"), Constant(notBefore) +: Nil)
         +: Structure(StrLit("notAfter"), Constant(notAfter) +: Nil)
         +: Structure(StrLit("notAfter"), Constant(notAfter) +: Nil)
         +: SetTerm(name, statements)
         +: Structure(StrLit("signatureAlgorithm"), Constant(signatureAlgorithm) +: Nil) =>
      
        
    case _ => throw ParserException(s"Malformed signed data")   
  }

  private def decryptData(encryptedData: String, self: Principal): String = {
    throw NotImplementedException(s"Not yet implemented") // TODO
  }
}
*/

/** Structure is redefined to override the toString() function to sugar the sequence/lists 
case class Structure(
    id: StrLit
  , terms: Seq[Term]
  , attrName: StrLit = termIndex
  , tpe: StrLit = termType
  , indexAndEncode: Enc = 0
) extends Term with StructureLike {
  override def toString(): String = terms match {
    case Nil => id.name
    case x if (id == StrLit("_seq")) && (x.last == Structure(StrLit("nil"), Nil)) => 
      "[" + x.init.mkString(", ") + "]"                  //enumeratedList
    case x if (id == StrLit("_seq")) && (x.last == Variable(x.last.toString()) || x.last == Constant(x.last.toString())) =>
      "[" + x.init.mkString(", ") + " | " + x.last + "]" //pipedList
    case _ => 
      id + "(" + terms.mkString(", ") + ")"
  }
}
*/


