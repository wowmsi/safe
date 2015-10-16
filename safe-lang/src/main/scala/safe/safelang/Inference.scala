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

import scala.collection.{GenIterable, GenSeq, GenSet}
import scala.collection.parallel.immutable.ParSet
import scala.concurrent.ExecutionContext

import safe.safelog._

import model._

import SlangTerm._

import com.typesafe.config.ConfigFactory

import safe.cache.SafeCache

import safe.safesets.SafeSetsMessageProtocol.{PostSetWithName, IOCompleted, Fetch, RenderCompleted, SimplePost, SimpleGet, SimpleDelete}
import safe.safesets.client.{Work, WorkResult}

import akka.actor.{Actor, ActorRef, ActorLogging}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

trait InferenceImpl extends safe.safelog.InferenceImpl {
  this: InferenceService with SafeSetsService =>

  val predef: Set[StrLit] = Set(
      StrLit("getId2"), StrLit("getId3"), StrLit("getId1")
    , StrLit("getRootId1")
    , StrLit("computeId2")
    , StrLit("computeIdFromName2")
//    , StrLit("principal0"), StrLit("principal1"), StrLit("principal2")
    , StrLit("signingKey1"), StrLit("key1"), StrLit("id1")
    , StrLit("getSubject1")
    , StrLit("getEncoding1")
    , StrLit("getVersion1")
    , StrLit("getSignedData1")
    , StrLit("getSpeaker1")
    , StrLit("getPrincipal1")
    , StrLit("getSpeakerKey1")
    , StrLit("getSubjectKey1")
    , StrLit("getSpeakerRef1")
    , StrLit("getValidity1")
    , StrLit("getValidityFrom1")
    , StrLit("getValidityUntil1")
    , StrLit("getSignature1")
    , StrLit("getSignatureAlgorithm1")
    , StrLit("getStatus1")
    , StrLit("getSlotSet1")
    , StrLit("getName1")
    , StrLit("verifySignature2")
    , StrLit("parseSet1")
    , StrLit("concat2")
    , StrLit("scid0")
    //, StrLit("print1"), StrLit("println1")
    , StrLit("simplePost2")
    , StrLit("simpleGet1")
    , StrLit("simpleDelete1")
    , StrLit("transposeToSeq1")
  )


  // =============== Solver ==================== //
  //@annotation.tailrec // TODO: make this tailrec
  override def solveAQuery(
     contextIds: Set[SetId]
   , query: Statement
   , isInteractive: Boolean = false
  )(implicit 
     envContext: SafeCache[StrLit, EnvValue]
   , localSlangContext: Map[Index, Set[Statement]]
   , proofContext: SafeCache[SetId, ProofSubContext]
  ): Iterable[Term => Term] = {

    def solveSetTerm(
        setType: StrLit
      , setTerm: SetTerm
      , goals: Seq[Term]
      , defhead: Term
    ): Iterable[Tuple2[Term => Term, Seq[Term]]] = {
      
      val constantTerms: Seq[Term] = bindEnvVar(setTerm.args)(envContext)
      // check for empty arguments, i.e., check if the seq is empty
      val constantTermsMayBe: Option[Seq[Term]] = if(constantTerms == Nil) None else Some(constantTerms)
      val res = for {
	constantArgs  <- constantTermsMayBe
	sTerm  = setTerm.copy(args = constantArgs)(safelogContext)
	result = sTerm.evalSet(setType)(envContext, proofContext)
	if(result.toString != "false")
      } yield((x: Term) => if(x == defhead) result else x, goals.tail)
      res
    }

    def solveFunTerm(funTerm: FunTerm, goals: Seq[Term], defhead: Term): Iterable[Tuple2[Term => Term, Seq[Term]]] = {
      /* This is complicated by two things:
       * -- Each argument to a defun may in turn be another defun
       * -- There can be multiple definitions for each defun signature since defun is a rule rather than a function
       */
      /* First evaluate all arguments so that they all are constants */
      val constantTerms: Seq[Term] = bindEnvVar(funTerm.args)(envContext)

      // check for empty arguments, i.e., check if the seq is empty
      val constantTermsMayBe: Option[Seq[Term]] = if(constantTerms == Nil) None else Some(constantTerms)
      for {
	constantArgs <- constantTermsMayBe
	fTerm = funTerm.copy(args = constantArgs)(jvmContext(funTerm.tpe.name))
	result  = fTerm.evalFunction()(envContext)
	if(result.toString != "false")
      } yield((x: Term) => if(x == defhead) result else x, goals.tail)
    }

    def solveEnvTerm(envVar: Variable, envTerm: Term, goals: Seq[Term]): Iterable[Tuple2[Term => Term, Seq[Term]]] = envTerm match {
      case s @ Structure(StrLit("principal"), args, _, _, _) if Set(0, 1, 2).contains(s.arity) => 
	val principal = s.arity match {
	  case 0 => Principal()
	  case 1 => Principal(pemFile = args(0).id.name)
	  case 2 => Principal(algorithm = args(0).id.name, keyLength = args(1).id.name.toInt)
	}
	envContext.put(envVar.simpleName, principal)
	if(envVar.id == StrLit("Selfie")) {
	  envContext.put(StrLit("Self"), Constant(StrLit(s"${principal.scid.toString}"), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64))
	  envContext.put(StrLit("SelfKey"), Constant(StrLit(s"${principal.subject.toString}"), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64))
	}
	val result = Constant(StrLit(principal.scid.value.name), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
        //println(s"envVar: ${envVar.simpleName}, value: $result")
	Seq(((x: Term) => x match {
          case v: Variable if v.simpleName == envVar.simpleName => result
          case _ => x
        }, goals.tail))
	//Seq(((x: Term) => result, goals.tail))
      case s @ Structure(stdlibdef, args, _, _, _) if predef.contains(s.primaryIndex) => 
	val result = handleDefTerm(stdlibdef, args, s.arity)
	envContext.put(envVar.simpleName, result)
        //println(s"envVar: ${envVar.simpleName}, value: $result")
	//Seq(((x: Term) => if(x.id == envVar.id) result else x, goals.tail))
	//Seq(((x: Term) => result, goals.tail))
	Seq(((x: Term) => x match {
          case v: Variable if v.simpleName == envVar.simpleName => result
          case _ => x
        }, goals.tail))
      case _ =>
	val envValue = recurse(Seq(envTerm))
	val result = {
           val xRes = envValue.map(subst => subst(envTerm))   // only one value should be present
           if(xRes.isEmpty) envTerm else xRes.head
        }
	envContext.put(envVar.simpleName, result)
        //println(s"envVar: ${envVar.simpleName}, value: $result")
	//Seq(((x: Term) => if(x.id == envVar.id) {println(s"X: $x"); result} else {println(s"Y: $x"); x}, goals.tail))
	//Seq(((x: Term) => result, goals.tail))
	Seq(((x: Term) => x match {
          case v: Variable if v.simpleName == envVar.simpleName => result
          case _ => x
        }, goals.tail))
    }

    def solvePostTerm(args: Seq[Term], goals: Seq[Term], defhead: Term): Iterable[Tuple2[Term => Term, Seq[Term]]] = {
      val constantTerms = bindEnvVar(args)(envContext)
      val unfoldedTerms: Seq[Term] = constantTerms.map(term => SlangTerm.unfoldSeq(term)).flatten
      val futureSeq: Seq[Future[IOCompleted]] = unfoldedTerms.map {term =>
	val postTerm: Tuple3[String, UnsignedCredential, Principal] = envContext.get(term.id) match {
	  case Some(setTerm: SetTerm) => setTerm.credential match {
	    case cred: UnsignedCredential =>
	      val principal: Principal = envContext.get(StrLit("Selfie")) match {
		case Some(p: Principal) => p
		case _                  => throw UnSafeException(s"cannot sign since principal (Selfie) not defined")
	      }
	      val nameStr = if(cred.name.id.name == "") "" else cred.toString
	      Tuple3(nameStr, cred, principal)
	    case _       => throw UnSafeException(s"unsigned credential expected but something else found: ${setTerm.credential}")
	  }
	  case _ => throw UnSafeException(s"in memory set with name $term not found")
	}
	val workId = java.util.UUID.randomUUID().toString
	val future = ask(safeSetsClient, Work(workId, PostSetWithName(postTerm._1, postTerm._2, postTerm._3))).mapTo[IOCompleted]
	future
	//_waitingRefs += (postTerm._1)
      }
      val postedOut: Seq[IOCompleted] = Await.result(Future.sequence(futureSeq), timeout.duration)

      val postedScids = postedOut.map {
	case IOCompleted(setMap, originLink) if !setMap.isEmpty =>
          Constant(StrLit(originLink), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
	  //println(s"Post failed for id: $originLink")
	case _ => Constant(StrLit("nil"))
      }

      val result: Term = postedScids.foldRight(Structure(StrLit("nil"), Nil)) { (x,y) => Structure(StrLit("_seq"), Seq(x) ++: Seq(y))}
      Seq(((x: Term) => result, goals.tail))
    }

    def solveFetchTerm(args: Seq[Term], goals: Seq[Term], defhead: Term): Iterable[Tuple2[Term => Term, Seq[Term]]] = {
      val constantTerms = bindEnvVar(args)(envContext)
      val unfoldedTerms: Seq[Term] = constantTerms.map(term => SlangTerm.unfoldSeq(term)).flatten

      val notInProofContext: Seq[Term] = unfoldedTerms.collect {
        case x if proofContext.get(x.id) == None => x
      }

      val futureSeq: Seq[Future[RenderCompleted]] = notInProofContext.map { term =>
	val setId = term.id
	val workId = java.util.UUID.randomUUID().toString
	val future = if(setId.name == "nil") Future{RenderCompleted("nil", ProofSubContext(Map.empty))}
	  else ask(safeSetsClient, Work(workId, Fetch(setId.name))).mapTo[RenderCompleted]
	future
      }
      val fetchedSet: Seq[RenderCompleted] = Await.result(Future.sequence(futureSeq), timeout.duration)
      fetchedSet.foreach {
	case RenderCompleted(setId, proofSubContext) =>
	  //println(s"\n\n\n\n\n\n\n\nSetId: $setId; proofSubContext: $proofSubContext\n\n\n\n\n\n")
	  proofContext.put(StrLit(Term.stripQuotes(setId)), proofSubContext)
	  //println(s"ProofContext: ${proofContext.get(StrLit(setId))}")
	  //println(s"[RENDER; proofContext values]: ${proofContext.values()}")
	case other @ _ => throw UnSafeException(s"Error in fetch: $other")
      }
      val result: Term = unfoldedTerms.foldRight(Structure(StrLit("nil"), Nil)) { (x,y) => Structure(StrLit("_seq"), Seq(x) ++: Seq(y))}
      Seq(((x: Term) => result, goals.tail))
    }

    def solveGoalTerm(goal: Term, goals: Seq[Term], depth: Int): Iterable[Tuple2[Term => Term, Seq[Term]]] = {
      /** TODO:
	1. memo goals.head
	2. if goals.head is found again, then check for answers
	  2a. If no answers found, then freeze stack
	    -- execute other branches
	    -- on completion, resume this branch
	  2b. If answers found, then execute with those answers rather than doing an SLD
       */

      val statements: Set[Statement] = filterAndBind(
	  contextIds
	, goal
	, variable => Variable(s"${variable.name}_${depth}")
      )(localSlangContext, proofContext)

      //println(s"Statements: $statements")
      //println(s"proofContext: $proofContext")

      val res = for {
	statement <- statements
	//_ = logger.debug("branch: statement: " + statement)
	//_ = logger.debug("branch: statement.terms.head: " + statement.terms.head)
	//_ = println("branch: statement.terms.head: " + statement.terms.head)
	subst <- mostGenericUnifier(statement.terms.head, goal)(envContext)
	newGoals = (statement.terms.tail ++ goals.tail).map(subst)
	//_ = logger.debug("newGoals: " + newGoals)
	//_ = println("newGoals: " + newGoals)
      } yield (subst, newGoals)
      res
    }

    def evalConditionTerm(term: Term): Term = term match {
      case c: Constant => c
      case _           => 
         val termFn = recurse(Seq(term))
	 termFn.map(subst => subst(term)).toSeq.head
    }

    def evalCondition(term: Term): Boolean = term match {
      case Constant(StrLit("true"), _, _, _)  => true
      case Constant(StrLit("false"), _, _, _) => false
      case Structure(cond, leftTerm +: rightTerm +: Nil, _, _, _) if (
         cond == StrLit("_unify")
       | cond == StrLit("_lt")
       | cond == StrLit("_lteq")
       | cond == StrLit("_gt")
       | cond == StrLit("_gteq") 
      ) =>
	 val rightTermValue: Term = evalConditionTerm(rightTerm)
	 val leftTermValue: Term  = evalConditionTerm(leftTerm)
         if(rightTermValue.toString == leftTermValue.toString) true
         else {
           val simpleStructure = Structure(cond, leftTermValue +: rightTermValue +: Nil)
           simpleStructure.compare()
         }
      case _ => throw UnSafeException(s"Unrecognized term passed in ifThenElse: $term")
    }

    //@annotation.tailrec // TODO: if we support := divergence, tailrec is compromised
    def branch(
        contextIds: Set[SetId]
      , goals: Seq[Term]
      , depth: Int
    ): Iterable[Tuple2[Term => Term, Seq[Term]]] = {/*println(s"GoalsHead: ${goals.head}");*/ goals.head match {

      //case x => return(Seq(Tuple2((x: Term) => x, goals.tail)))

      case Structure(StrLit("head"), Structure(StrLit("_seq"), head +: other, _, _, _) +: Nil, _, _, _) =>
        Seq((x => Constant(head.id, StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral), goals.tail))
      case Structure(StrLit("last"), Structure(StrLit("_seq"), init :+ last, _, _, _) +: Nil, _, _, _)  =>
        Seq((x => Constant(last.id, StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral), goals.tail))
      case Structure(StrLit("tail"), Structure(StrLit("_seq"), head +: tail, attrName, tpe, idx) +: Nil, _, _, _)  =>
        Seq((x => Structure(StrLit("_seq"), tail, attrName, tpe, idx), goals.tail))
      case Structure(StrLit("toSeq"), Structure(StrLit("_seq"), xterms, attrName, tpe, idx) +: Nil, _, _, _) if(
        tpe == StrLit("Dn")
       ) =>
        val singleQuotedTerms = xterms.map(x => Constant(x.id, x.attrName, x.tpe, Encoding.AttrLiteral))
        val seqTerm: Term = singleQuotedTerms.foldRight(Structure(StrLit("_seq"), Nil)) { (x,y) => Structure(StrLit("_seq"), Seq(x) ++: Seq(y)) }
        Seq((x => seqTerm, goals.tail))

      case Structure(StrLit("defguard"), defhead +: setTerm +: Nil, _, _, _) => setTerm match {
        case s: SetTerm => solveSetTerm(StrLit("defguard"), s, goals, defhead)
        case _          => throw UnSafeException(s"SetTerm expected but something else found: $setTerm")
      }
        
      case Structure(StrLit("defun"), defhead +: funTerm +: Nil, _, _, _)   => funTerm match {
        case f: FunTerm => solveFunTerm(f, goals, defhead)
        case _          => throw UnSafeException(s"FunTerm expected but something else found: $funTerm")
      }

      case Structure(StrLit("definit"), xterms, _, _, _) => branch(contextIds, xterms ++ goals.tail, depth)

      case Structure(StrLit("defcon"), defhead +: setTerm +: Nil, _, _, _) => setTerm match {
        case s: SetTerm => solveSetTerm(StrLit("defcon"), s, goals, defhead)
        case _          => throw UnSafeException(s"SetTerm expected but something else found: $setTerm")
      }

      case Structure(StrLit("defenv"), (envVar @ Variable(_, _, _, _)) +: xterm +: Nil, _, _, _) => 
        solveEnvTerm(envVar, xterm, goals)
      case Structure(StrLit("post"), args, _, _, _)    => solvePostTerm(args, goals, Constant("defpost"))  // safesets API
      case Structure(StrLit("defpost"), defhead +: args, _, _, _) => solvePostTerm(args, goals, defhead)  // safesets API
      case Structure(StrLit("fetch"), args, _, _, _)   => solveFetchTerm(args, goals, Constant("defetch")) // safesets API
      case Structure(StrLit("defetch"), defhead +: args, _, _, _) => solveFetchTerm(args, goals, defhead) // safesets API

      case Constant(StrLit("true"), _, _, _)     => Seq((x=>x, goals.tail))
      case Constant(StrLit("false"), _, _, _)    => Nil
      case envVar: Variable if envVar.isEnvVariable() =>
        val bindedEnvVar = bindEnvVar(Seq(Variable(envVar.id.name.substring(1))))
        if(bindedEnvVar.isEmpty) {
          Nil
        } else {
          Seq(((x: Term)=>if(x.id == envVar.id) bindedEnvVar.head else x, goals.tail))
        }
      case Structure(StrLit("spec"), _, _, _, _) => Seq((x=>x, goals.tail))
      case Structure(printPred, xterms, _, _, _) if(
           (printPred == StrLit("print")) 
         | (printPred == StrLit("println"))
       ) => xterms.headOption match {

        case Some(c: Constant) =>
          if(printPred == StrLit("println")) println(c) else print(c)
          Seq((x=>x, goals.tail))
        case Some(c: Structure) if c.id.name == "_seq" =>
          if(printPred == StrLit("println")) println(c) else print(c)
          Seq((x=>x, goals.tail))
        case _                 =>
          val res = recurse(xterms).map(subst => subst(xterms.head))
          if(printPred == StrLit("println")) println(res.mkString(", ")) else print(res.mkString(", "))
          Seq((x=>x, goals.tail))
      }
      case Structure(StrLit("_subset"), leftTerm +: rightTerm +: Nil, _, _, _) =>
        //val leftValue  = solve(contextIds, Seq(Query(Seq(leftTerm))), false)
        //val rightValue = solve(contextIds, Seq(Query(Seq(rightTerm))), false)
        val leftValueTerms  = recurse(Seq(leftTerm))
        val rightValueTerms = recurse(Seq(rightTerm))
        val leftValue  = leftValueTerms.map{subst => subst(leftTerm)}
        val rightValue = rightValueTerms.map{subst => subst(rightTerm)}
        //println(s"leftValue: $leftValue")
        //println(s"rightValue: $rightValue")
        if(leftValue.toSet subsetOf rightValue.toSet) Seq((x=>x, goals.tail)) else Nil
      case goal @ Structure(name, xterms, _, _, _) if (Set(StrLit("_lt"), StrLit("_lteq"), StrLit("_gt"), StrLit("_gteq"), StrLit("_subset"), StrLit("_in")).contains(name)) =>
        if(goal.compare()) Seq((x=>x, goals.tail)) else Nil
      case goal @ Structure(name, xterms, _, _, _) if (
          name == StrLit("_plus")
        | name == StrLit("_minus")
        | name == StrLit("_times")
        | name == StrLit("_div")
        | name == StrLit("_rem")
        | name == StrLit("_max")
        | name == StrLit("_min")
      ) =>
        val decimalFormat: java.text.DecimalFormat= new java.text.DecimalFormat("###.####")
        val result = Constant(StrLit(s"${decimalFormat.format(goal.eval())}"), StrLit("nil"), StrLit("NumericConstant"))
        Seq((x => result, goals.tail)) // no need to evaluate; useful only for cases where direct arithmetic is requested, e.g., +(2, 3)
      case Structure(StrLit("_unify"), leftTerm +: rightTerm +: Nil, _, _, _) => 
        mostGenericUnifier(leftTerm, rightTerm)(envContext) map { subst => 
	  val newGoals = goals.tail.map(subst)
	  Seq((subst, newGoals))
	} getOrElse(Nil)
      case Structure(StrLit("_is"), xterms, _, _, _) => xterms match {
	case Variable(leftVar, _, _, _) +: rightTerm +: Nil => rightTerm match {
	  case f: FunTerm    =>
	    val result = Structure(StrLit("_unify"), Variable(leftVar) +: f.evalFunction()(envContext) +: Nil)
	    branch(contextIds, result +: goals.tail, depth)
	  case s: SetTerm    =>
	    val result = Structure(StrLit("_unify"), Variable(leftVar) +: s.evalSet(StrLit("defcon"))(envContext, proofContext) +: Nil)
	    branch(contextIds, result +: goals.tail, depth)
	  case s: Structure if (s.id.name == "_seq" & isGrounded(s)) =>  // if not _seq, then it is a function which needs to be solved
	    val result = Structure(StrLit("_unify"), Variable(leftVar) +: rightTerm +: Nil)
	    branch(contextIds, result +: goals.tail, depth)
	  case s: Structure  =>
	    val rightTermValue = recurse(Seq(rightTerm))
            // Note: rightTerm can yield more than one value due to the presence of a funTerm with multiple defintions.
            // In such cases, we pick the head value and proceed rather than branching the term
            // Any value of the resulting term rightTerm should be okay
	    // val result = rightTermValue.map(subst => Structure(StrLit("_unify"), Variable(leftVar) +: subst(rightTerm) +: Nil)).head
	    // branch(contextIds, result +: goals.tail, depth)
	    val result: Seq[Term] = rightTermValue.map(subst => Structure(StrLit("_unify"), Variable(leftVar) +: subst(rightTerm) +: Nil)).toSeq
	    result.map{res => branch(contextIds, res +: goals.tail, depth)}.flatten // tailrec issue here
	  case x if isGrounded(x) => // a constant
	    val result = Structure(StrLit("_unify"), Variable(leftVar) +: rightTerm +: Nil)
	    branch(contextIds, result +: goals.tail, depth)
	  case _ => throw UnSafeException(s"RightTerm is not grounded: $rightTerm")
	}
	case _ => throw UnSafeException(s"LeftTerm should be a variable: $xterms")
      }
      case s @ Structure(StrLit("_interpolate"), Constant(body, attrName, tpe, _) +: Constant(termSeq, _, _, _) +: terms, _, _, _) =>
        val res: String = Term.interpolate(body.name, termSeq.name, bindEnvVar(terms))
        Seq((x=>Constant(StrLit(res), attrName, tpe, Encoding.AttrLiteral), goals.tail))
      case s @ Structure(stdlibdef, args, _, _, _) if predef.contains(s.primaryIndex) => 
        val result = handleDefTerm(stdlibdef, args, s.arity)
	Seq(((x: Term) => result, goals.tail))
      case f: FunTerm => solveFunTerm(f, goals, f)
      case s: SetTerm => solveSetTerm(StrLit("defguard"), s, goals, s)
      case Structure(StrLit("_compare"), xterms, _, _, _) => xterms match {
        case Constant(leftTerm, _, _, _) +: Constant(rightTerm, _, _, _) +: Nil => 
          if(leftTerm == rightTerm) Seq((x=>x, goals.tail)) else Nil
	case leftTerm +: rightTerm +: Nil =>
	  val leftResult = Structure(StrLit("_is"), Variable("%Left") +: leftTerm +: Nil)
	  val rightResult = Structure(StrLit("_is"), Variable("%Right") +: rightTerm +: Nil)
	  val result = Structure(StrLit("_compare"), Variable("%Left") +: Variable("%Right") +: Nil)
	  branch(contextIds, leftResult +: rightResult +: result +: goals.tail, depth)
	case _ => throw UnSafeException(s"error in compare")
      }
      case Structure(StrLit("ifThenElse"), xterms, _, _, _) => xterms match {
        case Constant(StrLit("true"), _, _, _) +: Constant(leftVal, _, _, _) +: Constant(rightVal, _, _, _) +: Nil =>
          Seq(((x: Term) => xterms.tail.head, goals.tail)) // leftVal
        case Constant(StrLit("false"), _, _, _) +: Constant(leftVal, _, _, _) +: Constant(rightVal, _, _, _) +: Nil =>
          Seq(((x: Term) => xterms.last, goals.tail))      // rightVal
        case cond +: leftVar +: rightVar +: Nil =>
          val isTrue   = if(evalCondition(cond)) "true" else "false"
      	  val leftVal  = Structure(StrLit("_is"), Variable("%LeftVar") +: leftVar +: Nil)
	  val rightVal = Structure(StrLit("_is"), Variable("%RightVar") +: rightVar +: Nil)
	  val result   = Structure(StrLit("ifThenElse"), Constant(isTrue) +: Variable("%LeftVar") +: Variable("%RightVar") +: Nil)
	  branch(contextIds, leftVal +: rightVal +: result +: goals.tail, depth)
	case _ => throw UnSafeException(s"error in ifThenElse")
      }
      // case NegatedTerm(name, xterm) => branch(contextIds, Seq(xterm), depth) 
      // Note: using solveAQuery() here instead of branch() to keep the @tailrec satisfied for branch()
      // TODO: May overflow stack if number of negated terms are more.
      case NegatedTerm(name, xterm, _, _, _) => solveAQuery(contextIds, Query(Seq(xterm)), false) match {
	case Nil => Seq((x=>x, goals.tail))
	case _   => Nil
      }

      case other                                         => solveGoalTerm(other, goals, depth)
    }}

    /** bind variables to constants if found in envContext; apply the same for lists */
    //@annotation.tailrec
    def bindEnvVar(terms: Seq[Term])(implicit envContext: SafeCache[StrLit, EnvValue]): Seq[Term] = {
      val evaledTerms = terms match {
	case Structure(StrLit("_seq"), subTerms, _, _, _) +: rest => subTerms match {
	  case Structure(StrLit("_seq"), moreSubTerms, _, _, _) +: tail =>
            Structure(
              StrLit("_seq") , substTerm(subTerms.head, recurse(Seq(subTerms.head)).toIndexedSeq) ++: bindEnvVar(subTerms.tail) 
            ) +: bindEnvVar(rest)
	  case _ => 
            Structure(StrLit("_seq"), bindEnvVar(subTerms)) +: bindEnvVar(rest)
	}
	case Structure(StrLit("nil"), _, _, _, _) +: rest   => Structure(StrLit("nil"), Nil) +: bindEnvVar(rest)
	case Structure(_, _, _, _, _) +: rest      => 
          substTerm(terms.head, recurse(Seq(terms.head)).toIndexedSeq) ++: bindEnvVar(rest)
	case Constant(_, _, _, _) +: rest          => 
          Seq(terms.head) ++: bindEnvVar(rest)
	case (v @ Variable(varId, _, _, _)) +: rest  =>
          val envVar = v.simpleName
	  val res = envContext.get(envVar) match {
	    case Some(value: Constant)    => value +: bindEnvVar(rest)
	    case other if (
                 envVar == StrLit("Self")
               | envVar == StrLit("SelfKey")
            )                             => v +: bindEnvVar(rest) // Self can be bound lately at post time
	    case other                    => throw UnSafeException(s"Unbound variable found: ${terms.head}; ${other}; envContext: ${envContext.keySet()}; $envVar")
	  }
          res
	case s @ SetTerm(_,_,_,_: SignedCredential,_,_,_) +: rest => s ++: bindEnvVar(rest) // TODO: check?
	case x +: rest =>
          substTerm(x, recurse(Seq(x)).toIndexedSeq) ++: bindEnvVar(rest) // FunTerm or Variable
	case Nil       => Nil
      }
      evaledTerms
    }

    def handleDefTerm(name: StrLit, args: Seq[Term], arity: Int): Term = {
      val constantTerms = bindEnvVar(args)
      handleDefTermConst(name, constantTerms, arity)
    }

    def handleDefTermConst(name: StrLit, args: Seq[Term], arity: Int): Term = (name, arity) match {
      //case (StrLit("getId"), 3) =>
        //val subject = Subject(args(0).toString().replace("\"", "").replace("\'",""))
        //val id = subject.computeId(name = args(1).toString, hashAlgorithm = args(2).toString).value
        //Constant(id)
      case (StrLit("computeIdFromName"), 2) =>
	val nameHash = Identity.encode(Identity.hash(args(1).id.name.getBytes(StringEncoding), "MD5"), "base64URLSafe")
        val namespace     = s"${args(0).id.name}:${nameHash}"
        val setIdHash = Identity.hash(namespace.getBytes(StringEncoding), "SHA-256")
	val setId = StrLit(Identity.encode(setIdHash, "base64URLSafe"))
        //println(s"namespace: $namespace; name: ${args(1).id}; setId: $setId")
	Constant(setId, StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case (StrLit("computeId"), 2) =>
        val namespace     = s"${args(0).id.name}:${args(1).id.name}"
        //println(s"namespace: $namespace")
	val setId = Identity.hash(namespace.getBytes(StringEncoding), "SHA-256")
	Constant(StrLit(Identity.encode(setId, "base64URLSafe")), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case (StrLit("getId"), 2) =>
        //println(s"myargs: $args; ${args.length}")
        val subject = Subject(args(0).toString())
        val id = subject.computeId(name = args(1).toString).value
	Constant(StrLit(id.name), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral)
      case (StrLit("getRootId"), 1) =>
        val arg = args.head.toString()
        val argArray = arg.split(":")
	Constant(StrLit(s"${argArray(0)}"), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral)
      case (StrLit("println"), 1) =>
        println(args.mkString(", "))
        Constant(StrLit("true"))
      case (StrLit("print"), 1)   => 
        print(args.mkString(", "))
        Constant(StrLit("true"))
      case (StrLit("concat"), 2)  =>
        val result = args(0).id.name + args(1).id.name
	Constant(StrLit(result), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral)
      case (StrLit("scid"), 0)  =>
        val principal: Principal = envContext.get(StrLit("Selfie")) match {
	  case Some(p: Principal) => p
	  case _                  => throw UnSafeException(s"cannot sign since principal (Selfie) not defined")
	}
        val result = Object(rootId = Id(principal.scid.toString)).scid.value.name
	Constant(StrLit(result), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral)
      case (StrLit("parseSet"), 1)  =>
        val setTerm: SetTerm = (Parser()).parseCertificate(args(0).id.name)
        setTerm
      case (StrLit("getSpeaker"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(StrLit(sc.speaker.id.toString), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case (StrLit("getSubject"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(StrLit(sc.subject.id.toString), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case (StrLit("getSpeakerKey"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        val spkrKey: String = sc.speaker.speaker match {
          case Some(value) => value.toString
          case None        => // check whether it is an identity set
            if(sc.speaker.id.toString == sc.id.toString) { // now check for principal statement
              sc.statements.get(StrLit("principal")) match {
                case Some(stmts) => stmts.head.toString
                case _           => "nil"
              }
            } else {
              "nil"
            }
        }
        Constant(spkrKey)
      case (StrLit("getPrincipal"), 1)  => 
        val setId: StrLit = args(0) match {
            case Structure(StrLit("_seq"), xargs +: other, _, _, _) => xargs.id
            case _ => throw UnSafeException(s"Seq expected but something else found, ${args(0)}")
          }
        val proofSubContext = proofContext.get(setId).getOrElse(throw UnSafeException(s"Fetch seems to be failed for the set $setId"))
	val principalKey: String = proofSubContext.statements.get(StrLit("_principal")) match {
	    case Some(stmts) => stmts.head.terms.head match {
              case Structure(StrLit("principal"), speaker +: speakerKey +: Nil, _, _, _) => speakerKey.id.name
              case _ => throw UnSafeException(s"principal not found in set $setId")
            }
	    case _           => throw UnSafeException(s"principal not found in set $setId")
	  }
        Constant(StrLit(principalKey), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case (StrLit("getName"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(sc.name)
      case (StrLit("getId"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(sc.id)
      case (StrLit("getSpeakerRef"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(sc.speaker.speaksForRef.getOrElse("nil").toString)
      case (StrLit("getSignature"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(StrLit(sc.signature), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      case (StrLit("getSignedData"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(StrLit(sc.signedData))
      case (StrLit("getSignatureAlgorithm"), 1)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        Constant(StrLit(sc.signatureAlgorithm))
      //case (StrLit("getValdityFrom"), 1)  =>
       // val sc: SignedCredential = toSignedCredentialTerm(args(0))
        //Constant(sc.validity.notBefore.toString)
      //case (StrLit("getValdityUntil"), 1)  =>
       // val sc: SignedCredential = toSignedCredentialTerm(args(0))
        //Constant(sc.validity.notAfter.toString)
      case (StrLit("verifySignature"), 2)  =>
        val sc: SignedCredential = toSignedCredentialTerm(args(0))
        val speaker: Subject = Subject(args(1).id.name)
        // check whether the ids match
        if(speaker.id.toString != sc.speaker.id.toString) {
          throw UnSafeException(s"Given speaker and speaker on the set does not match: ${speaker.id}, ${sc.speaker.id}")
        }
        val isSignatureValid: Boolean = sc.verify(speaker)
        //println(s"speaker: $speaker")
        Constant(isSignatureValid.toString)
      case (StrLit("transposeToSeq"), 1)  => args(0) match {
        case s: SlogResult => s.transposeToSeq()
        case _             => throw new UnSafeException(s"Error convert logic set to seq. Does the logic set contains only facts?")
      }
      case (StrLit("simplePost"), 2)  =>
        simpleRestAPI('post, args(0).id.name, args(1).id.name)
      case (StrLit("simpleGet"), 1)  =>
        simpleRestAPI('get, args(0).id.name)
      case (StrLit("simpleDelete"), 1)  =>
        simpleRestAPI('delete, args(0).id.name)
      case (_, _)  => 
        println(s"handleDefTerm: "); 
        //Constant(s"Not yet done")
        Constant(StrLit("nil"))
    }

    def simpleRestAPI(action: Symbol, id: String, content: String = ""): Term = {
      val workId = java.util.UUID.randomUUID().toString
      val future: Future[IOCompleted] = action match {
        case 'post   =>
	  ask(safeSetsClient, Work(workId, SimplePost(id, content))).mapTo[IOCompleted]
        case 'get    =>
	  ask(safeSetsClient, Work(workId, SimpleGet(id))).mapTo[IOCompleted]
        case 'delete =>
	  ask(safeSetsClient, Work(workId, SimpleDelete(id))).mapTo[IOCompleted]
        case _       => throw UnSafeException(s"Not yet implemented")
     }
     val out: IOCompleted = Await.result(future, timeout.duration)
     //val result = if(out.setMap.isEmpty) Constant(StrLit("nil")) else {
      //   Constant(StrLit(id), StrLit("nil"), StrLit("StrLit"), Encoding.AttrBase64)
      // }
      Constant(StrLit(out.originLink), StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral) // TODO: This is a temporary hack
    }

    def toSignedCredentialTerm(term: Term): SignedCredential = term match {
      case SetTerm(id, argRefs, args, credential: SignedCredential, _, _, _) => credential
      case _                    => 
        throw UnSafeException(s"Expected Signed Credential argument on function but $term found")
    }

    def substTerm(term: Term, substFns: Seq[Term => Term]): Seq[Term] = {
      substFns.map(subst => subst(term))
    }

    //@annotation.tailrec
    def recurse(goals: Seq[Term], depth: Int = 1): Iterable[Term => Term] = {
      //println(s"goals: $goals")
      if(goals == Nil) {
/*
        val solutionsRaw = for {
	  subst <- result
        } yield query.terms.map(subst)
        val solutions = solutionsRaw.map(x => Assertion(x))

        if(query.terms.length > 1) success(Assertion(Constant("query(..)") +: query.terms), isInteractive)
	else success(Assertion(query.terms), isInteractive)
	*/

	Seq(x => x)
      }
      else if(depth > Config.config.maxDepth) {
	failure(query, "ran out of maximum depth", isInteractive)
	//System.exit(1) // just quit
	Seq(x => x) // probably going to infinite recurse //TODO: fix this
      } else {
        val endTime1 = System.currentTimeMillis()
        val res = for {
	  (solution, freshGoals) <- branch(contextIds, goals, depth) // branch for new goals
	  endTime2 = System.currentTimeMillis()
	  _ = logger.info(s"Time for branch at depth $depth is: ${endTime2 - endTime1} ms")
	  remainingSolutions <- recurse(freshGoals, depth + 1)
	} yield (solution andThen remainingSolutions)
        res
      }
    }
    val result = recurse(query.terms)
    result.toSeq
  }

  // NOTE: The only difference between solve and solveSlang is the proofContext.put() is taken to init for solveSlang
  def solveSlang(
      queries: Seq[Statement]
    , isInteractive: Boolean = false
  )(implicit
      proofContext: SafeCache[SetId, ProofSubContext] = InferenceService.proofContext
  ): Seq[Seq[Statement]] = {

    /**=== DEBUG Start=====
    if(true) {
      return Seq(Seq(Result(Constant("slogPong") +: Nil)))
    } 
    ====== DEBUG End==== **/
   
    logger.info(s"Starting inference")
    val t0 = System.nanoTime()
   
    val res = for {
      query <- queries
      t00 = System.nanoTime()
      //_   =  proofContext.put(StrLit("_object"), ProofSubContext(id = StrLit("_object"), statements = program))
      resultTerms = solveAQuery(Set(StrLit("_object")), query, isInteractive)(envContext, Map.empty, proofContext)
      t01 = System.nanoTime()
      _ = logger.info(s"Elapsed time for SOLVE is: ${(t01 - t00)/Math.pow(10, 6)} ms\n\n")
      //_ = println(s"Elapsed time for SOLVE is: $elapsedTime ms\n\n") // DEBUG
      _ = if(isInteractive) iterativePrint(resultTerms, query, isInteractive)
      subst <- resultTerms
      //if(!(subst(Constant(StrLit("defenvMayBe"))) == Constant(StrLit("nil")))) // check to verify if the output is nil
      //_ = println(s"query.terms: ${query.terms} -> ${query.terms.map(subst)}")
    } yield query.terms.length match {
      case 1 => Result(query.terms.map(subst))
      case _ => Result(Constant("query(_*)") +: query.terms.map(subst)) // > 1
    }
    val t1 = System.nanoTime()
    //val elapsedTime = (t1 - t0)/Math.pow(10, 6)
    logger.info(s"Elapsed time for solve is: ${(t1 - t0)/Math.pow(10, 6)} ms")
    if(res.isEmpty && isInteractive == true)
      SafelogException.printLabel('failure)

    Seq(res.toSeq)
  }

  /*
  //===== Actor stuff =====//

  import scala.collection.mutable.{Set=>MutableSet}
  //val _waitingRefs: MutableSet[String] = MutableSet.empty

  case class PostSet(items: Seq[Tuple3[String, UnsignedCredential, Principal]])

  import scala.collection.mutable.{Set => MutableSet}
  var postNames: MutableSet[String] = MutableSet.empty
  import safe.safesets.SafeSetsMessageProtocol._

  def receive: Receive = {
    case postTerms: PostSet => postTerms.items.foreach {postItem =>
      postNames += postItem._1
      safeSetsClient ! PostSetWithName(postItem._1, postItem._2, postItem._3)
    }
  }

  def waiting: Receive = {
    case "OK" =>
  }

  def waitHandler: Receive = {
    case IOCompleted(setMap, originLink) => 
      _waitingRefs -= setMap(originLink).name
      if(_waitingRefs.isEmpty) {
        println(s"all tokens are done")
      }
  }
  */

}

class Inference(val safeSetsClient: ActorRef) extends InferenceService with SafeSetsService with InferenceImpl
