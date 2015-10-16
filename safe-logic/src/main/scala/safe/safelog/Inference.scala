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

import scala.collection.{GenIterable, GenSeq, GenSet}
//import scala.collection.parallel.immutable.ParSet
import scala.concurrent.ExecutionContext.Implicits.global //TODO

import Term.{dropStatement, isGrounded, mostGenericUnifier, subst}

import com.typesafe.config.ConfigFactory

//import InferenceService.{envContext, localSlangContext, proofContext}
//import InferenceService.{proofContext}

import safe.cache.SafeCache

import SafelogException.printLabel

trait InferenceImpl extends com.typesafe.scalalogging.LazyLogging {
  slangInference: InferenceService =>

  protected def substQuery(
      terms: Seq[Term]
    , substSeq: Iterable[Term => Term]
    , accumulator: scala.collection.mutable.Map[String, Seq[String]]
  ): scala.collection.mutable.Map[String, Seq[String]] = {

    @annotation.tailrec
    def substQueryHelper(terms: Seq[Term], subst: Term => Term): Unit = terms match {
      case Variable(name, _, _, _) +: tail => 
        val head = terms.head
	val value = subst(head)
        if(value != head) {
	  if(!accumulator.contains(head.toString())) accumulator(head.toString()) = value.toString() +: Nil
	  else accumulator.update(head.toString(), value.toString() +: accumulator(head.toString()))
	}
	substQueryHelper(tail, subst)
      case Structure(name, xterms, _, _, _) +: tail => 
        val head = terms.head
	val value = subst(head)
        if(value != head) substQueryHelper(xterms ++: tail, subst)
	else substQueryHelper(tail, subst)
      case head +: tail =>  //TODO: we may simple ignore this case; Constant, FunTerm, and SetTerm substitutions may not make sense
	val value = subst(head)
        if(value != head) {
	  if(!accumulator.contains(head.toString())) accumulator(head.toString()) = value.toString() +: Nil
	  else accumulator.update(head.toString(), value.toString()+: accumulator(head.toString()))
	}
	substQueryHelper(tail, subst)
      //case Nil => accumulator.toMap
      case Nil => 
    }
    for (subst <- substSeq) { 
      substQueryHelper(terms, subst) 
    }
    accumulator
  }

  /**
   * solve: given a parsed data set, solve a sequence of queries
   */
  def solveWithValue(
      program: Map[Index, Set[Statement]]
    , queries: Seq[Statement]
    , isInteractive: Boolean = false
  ): Seq[Seq[Map[String, Seq[String]]]] =  {
    val res = for {
      query <- queries
      // TODO: Move the put to slang layer
      _     =  proofContext.put(StrLit("_object"), ProofSubContext(id = StrLit("_object"), statements = program))
      resultTerms = solveAQuery(Set(StrLit("_object")), query, isInteractive)
      accumulator = substQuery(query.terms, resultTerms, scala.collection.mutable.Map.empty[String, Seq[String]])
    } yield(accumulator.toMap)
    Seq(res.toSeq)
  }

  def solve(
      program: Map[Index, Set[Statement]]
    , queries: Seq[Statement]
    , isInteractive: Boolean = false
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
      _   =  proofContext.put(StrLit("_object"), ProofSubContext(id = StrLit("_object"), statements = program))
      resultTerms = solveAQuery(Set(StrLit("_object")), query, isInteractive)
      t01 = System.nanoTime() 
      _ = logger.info(s"Elapsed time for SOLVE is: ${(t01 - t00)/Math.pow(10, 6)} ms\n\n")
      //_ = println(s"Elapsed time for SOLVE is: $elapsedTime ms\n\n") // DEBUG
      _ = if(isInteractive) iterativePrint(resultTerms, query, isInteractive)
      subst <- resultTerms
      //if(!(subst(Constant('defenvMayBe)) == Constant('nil))) // check to verify if the output is nil
      //_ = println(s"query.terms: ${query.terms} -> ${query.terms.map(subst)}")
    } yield query.terms.length match {
      case 1 => Result(query.terms.map(subst))
      case _ => Result(Constant("query(_*)") +: query.terms.map(subst)) // > 1
    }

    val t1 = System.nanoTime()
    //val elapsedTime = (t1 - t0)/Math.pow(10, 6)
    logger.info(s"Elapsed time for solve is: ${(t1 - t0)/Math.pow(10, 6)} ms")
    if(res.isEmpty && isInteractive == true) 
      printLabel('failure)

    Seq(res.toSeq)
  }

  def solveWithContext(
      contextIds: Set[SetId]
    , queries: Seq[Statement]
    , isInteractive: Boolean
  )(  envContext: SafeCache[StrLit, EnvValue]
    , localSlangContext: Map[Index, Set[Statement]]
    , proofContext: SafeCache[SetId, ProofSubContext]
  ): Seq[Seq[Statement]] = {

    /**=== DEBUG Start=====
    if(true) {
      return Seq(Seq(Result(Constant("slogPong") +: Nil)))
    } 
    === DEBUG Start=====**/

    logger.info(s"Starting inference")
    val t0 = System.nanoTime()

    //println(s"In slogInference: $contextIds, queries: $queries")
    // TODO: launch a new instance
    val res = for {
      query <- queries
      t00 = System.nanoTime() 
      resultTerms = solveAQuery(contextIds, query, isInteractive)(envContext, localSlangContext, proofContext)
      t01 = System.nanoTime() 
      //elapsedTime = (t01 - t00)/Math.pow(10, 6)
      _ = logger.info(s"Elapsed time for SOLVE is: ${(t01 - t00)/Math.pow(10, 6)} ms\n\n")
      _ = if(isInteractive) iterativePrint(resultTerms, query, isInteractive)
      subst <- resultTerms
    } yield query.terms.length match {
      case 1 => Result(query.terms.map(subst))
      case _ => Result(Constant("query(_*)") +: query.terms.map(subst)) // > 1
    }

    val t1 = System.nanoTime()
    //val elapsedTime = (t1 - t0)/Math.pow(10, 6)
    logger.info(s"Elapsed time for solve is: ${(t1 - t0)/Math.pow(10, 6)} ms")
    if(res.isEmpty && isInteractive == true) 
      printLabel('failure)

    // if $Self is not defined, then substitute $Self_depth by $Self
    if(!envContext.containsKey(StrLit("Self"))) {
      val selfSubst: Term => Term = (x: Term) => x match {
        case v: Variable if(v.id.name.startsWith("$Self")) => Constant("$Self")
        case _ => x
      }
      val subst: Term => Term = (x: Term) => x match {
        case v: Variable  => selfSubst(x)
        case s: Structure => s.copy(terms = s.terms.map(selfSubst))
        case _ => x
      }
      Seq(res.toSeq.map{s => Result(s.terms.map(subst))})
    } else {
      Seq(res.toSeq)
    }
  }

  // success reports a value to REPL
  protected def success(statement: Statement, isInteractive: Boolean = false, more: Boolean = true): Boolean = {
    val ret: Boolean = if(isInteractive) {
      printLabel('success)
      println(statement)
      val next: Boolean = if(more) {
        printLabel('more)
        val reader = new jline.console.ConsoleReader(System.in, System.out)
        val input: Boolean = reader.readLine() match {
	  case s if s.toLowerCase.matches("""^[y;\n](es)?""") => true // no-op //TODO: \n does not work since the match expects to be given in double quotes
	  case _ => failure(statement); false // System.exit(0) // TODO: 
        }
        input
      } else false
      next
    } else false
    ret
  }

  protected def failure(query: Statement, msg: String = "", isInteractive: Boolean = false): Unit = {
    if(isInteractive) {
      printLabel('failure)
      println(msg)
    }
  }

  private def times(n: Int)(f: => Unit) = 1 to n foreach {_ => f}
  private def indentAndPrint(depth: Int, level: Int, msg: String = ""): Unit = {
    val indentAndPrintSpace = "    "
    times(depth+level)(print(indentAndPrintSpace)) 
    println(msg)
  }

/*
  lazy val memoQueryContext: util.Memo[Token, Seq[Statement]] = util.Memo {
  }

  def fetchQueryContextToken(token: Token): Seq[Statement] = {
    memoQueryContext(token)
  }
  */

  // TODO: fix this
  @annotation.tailrec
  final def iterativePrint(result: Iterable[Term => Term], query: Statement, isInteractive: Boolean): Unit = result.headOption match {
    case None => printLabel('failure) // no-op
    case Some(subst) => 
      val solutionsRaw = query.terms.map(subst)
      val more: Boolean = if(query.terms.length > 1) success(Assertion(Constant("query(_*)") +: solutionsRaw), isInteractive, true)
        else success(Assertion(solutionsRaw), isInteractive, true)
      if(more) iterativePrint(result.tail, query, isInteractive)
   }

  // =============== Solver ==================== //

  protected def filterAndBind(
      contextIds: Set[SetId]
    , goal: Term
    , freshVariable: StrLit => Variable
  )(  localSlangContext: Map[Index, Set[Statement]]
    , proofContext: SafeCache[SetId, ProofSubContext]
  ): Set[Statement] = {

    //println(s"In filterAndBind -- contextIds: $contextIds; goal: $goal; goal.primaryIndex: ${goal.primaryIndex}")
    //val index = if(goal.primaryIndex == '_Nil) goal.secondaryIndex else goal.primaryIndex
    val index = goal.primaryIndex
    val statements: Set[Set[Statement]] = {
      for {
	id                   <-  contextIds
	credentialSetMayBe   =   proofContext.get(id)
	credentialSet        <-  credentialSetMayBe
	stmtsMayBe           =   credentialSet.get(index)
	stmts                <-  stmtsMayBe
      } yield for {
	stmt                 <-  stmts
      } yield (stmt.bind(freshVariable))
    }

    val localSlangContextStatements: Set[Statement] = localSlangContext.get(index) match { 
      case Some(stmts) =>
        stmts.map{stmt => stmt.bind(freshVariable)}
      case _  => Set.empty
    }

    val res = statements.flatten ++ localSlangContextStatements
    res
  }

  //@annotation.tailrec // TODO: make this tailrec
  protected def solveAQuery(
     contextIds: Set[SetId]
   , query: Statement
   , isInteractive: Boolean = false
  )(implicit 
     envContext: SafeCache[StrLit, EnvValue]
   , localSlangContext: Map[Index, Set[Statement]] = Map.empty
   , proofContext: SafeCache[SetId, ProofSubContext]
  ): Iterable[Term => Term] = {

    /*
    def getStatements(credentialSet: ProofSubContext, goal: Term): Option[Set[Statement]] = credentialSet.get(goal.primaryIndex)
    import scala.util.{Success, Failure, Try}
    import scala.concurrent.{Await, Future, Promise}
    def filterAndBindInParallel(contextIds: Set[SetId], goal: Term, freshVariable: StrLit => Variable): Set[Statement] = {
      val statements: Set[Set[Statement]] = for {
        credentialSetsMayBe  <-  traverseAndCollectSuccess(contextIds)(proofContext.get)
        stmtsSets            <-  traverseAndCollectSuccess(credentialSetsMayBe)(getStatements, goal)
      } yield (stmtsSets.flatten) map { stmt =>
        (stmt.bind(freshVariable))
      }
    }
    */


    //val mystmts: Set[Statement] = proofContext.get(StrLit("_object")).get.get(query.terms.head.primaryIndex).getOrElse(Set.empty)

    def solveGoalTerm(goal: Term, goals: Seq[Term], depth: Int): Iterable[Tuple2[Term => Term, Seq[Term]]] = {
      /**
      val start: Long = System.nanoTime()
      val delay: Long = 100000 //100 microseconds
      while(start + delay >= System.nanoTime()){}
      //Thread.sleep(1)
      return(Seq(Tuple2((x: Term) => x, goals.tail)))
      **/
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

      //return(Seq(Tuple2((x: Term) => x, goals.tail)))

      //val statements: Set[Statement] = proofContext.get('_object).get.get(goal.primaryIndex).getOrElse(Set.empty)
      //val statements: Set[Statement] = Set(Assertion(Seq(Constant("hi"))), Assertion(Seq(Constant("bye"))))
      //val statements: Set[Statement] = mystmts

      val res = for {
        statement <- statements
        subst <- mostGenericUnifier(statement.terms.head, goal)
        newGoals = (statement.terms.tail ++ goals.tail).map(subst)
      } yield (subst, newGoals)

      res
    }
 

    @annotation.tailrec
    def branch(
        contextIds: Set[SetId]
      , goals: Seq[Term]
      , depth: Int
    ): Iterable[Tuple2[Term => Term, Seq[Term]]] = goals.head match {

      case Constant(StrLit("true"), _, _, _)     => Seq((x => x, goals.tail))
      case Constant(StrLit("false"), _, _, _)    => Nil
      case Structure(StrLit("spec"), _, _, _, _) => Seq((x => x, goals.tail))
      case Structure(StrLit("domainTerm"), speaker +: xterm +: Nil, _, _, _) => //if xterm.tpe.name == "Dn" =>
        Seq((x => x, goals.tail))
      case Structure(StrLit("domainTerm"), xterm +: Nil, _, _, _) => //if xterm.tpe.name == "Dn" => // TODO: what if speaker is present but domainTerm is nil?
        Seq((x => x, goals.tail))
       case Structure(StrLit("head"), Structure(StrLit("_seq"), head +: other, _, _, _) +: Nil, _, _, _) =>
        Seq((x => Constant(head.id, StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral), goals.tail))
      case Structure(StrLit("head"), speaker +: Structure(StrLit("_seq"), head +: other, _, _, _) +: Nil, _, _, _) =>
        Seq((x => Constant(head.id, StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral), goals.tail))
      case Structure(StrLit("last"), Structure(StrLit("_seq"), init :+ last, _, _, _) +: Nil, _, _, _)  =>
        Seq((x => Constant(last.id, StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral), goals.tail))
      case Structure(StrLit("last"), speaker +: Structure(StrLit("_seq"), init :+ last, _, _, _) +: Nil, _, _, _)  =>
        Seq((x => Constant(last.id, StrLit("nil"), StrLit("StrLit"), Encoding.AttrLiteral), goals.tail))
      case Structure(StrLit("tail"), Structure(StrLit("_seq"), head +: tail, attrName, tpe, idx) +: Nil, _, _, _)  =>
        if(tail == Nil) Seq((x => Constant("nil"), goals.tail))
        else Seq((x => Structure(StrLit("_seq"), tail, attrName, tpe, idx), goals.tail))
      case Structure(StrLit("tail"), speaker +: Structure(StrLit("_seq"), head +: tail, attrName, tpe, idx) +: Nil, _, _, _)  =>
        if(tail == Nil) Seq((x => Constant("nil"), goals.tail))
        else Seq((x => Structure(StrLit("_seq"), tail, attrName, tpe, idx), goals.tail))

      case Structure(StrLit("_subset"), leftTerm +: rightTerm +: Nil, _, _, _) =>
        //val leftValue  = solve(contextIds, Seq(Query(Seq(leftTerm))), false)
        //val rightValue = solve(contextIds, Seq(Query(Seq(rightTerm))), false)
        val leftValueTerms  = recurse(Seq(leftTerm))
        val rightValueTerms = recurse(Seq(rightTerm))
        val leftValue  = leftValueTerms.map{subst => subst(leftTerm)}
        val rightValue = rightValueTerms.map{subst => subst(rightTerm)}
        //println(s"leftValue: $leftValue")
        //println(s"rightValue: $rightValue")
        if(leftValue.toSet subsetOf rightValue.toSet) Seq((x => x, goals.tail)) else Nil
      case goal @ Structure(name, xterms, _, _, _) if (
           name == StrLit("_lt")
         | name == StrLit("_lteq")
         | name == StrLit("_gt")
         | name == StrLit("_gteq") 
         | name == StrLit("_subset") 
         | name == StrLit("_in")
       ) => if(goal.compare()) Seq((x => x, goals.tail)) else Nil

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
        //val numericConstant = Term.numberFromString(decimalFormat.format(goal.eval())) // TODO
        val numericConstant = decimalFormat.format(goal.eval()) 
        val result = Constant(s"$numericConstant", StrLit("nil"), StrLit("NumericConstant")) // TODO: infer the type
        Seq((x => result, goals.tail)) // no need to evaluate; useful only for cases where direct arithmetic is requested, e.g., +(2, 3)

      case Structure(StrLit("_unify"), leftTerm +: rightTerm +: Nil, _, _, _) => mostGenericUnifier(leftTerm, rightTerm) map { subst => 
        val newGoals = goals.tail.map(subst)
        Seq((subst, newGoals))
      } getOrElse(Nil)

      case Structure(StrLit("_is"), xterms, _, _, _) => xterms match {
	case Variable(leftVar, _, _, _) +: rightTerm +: Nil => rightTerm match {
	  case x if isGrounded(x) => rightTerm match {
	    case Structure(_, _, _, _, _) =>
	      val rightTermValue = recurse(Seq(rightTerm))
              val result = rightTermValue.map{subst => Structure(StrLit("_unify"), Variable(leftVar) +: subst(rightTerm) +: Nil)} // should have only one value
              val res = result.headOption match {// should return only one value
                case None    => //Seq(Constant(true))
                  Constant("nil") +: goals.tail
                case Some(v) if(v.id.name == "nil") => //Seq(Constant(true))
                  Constant("nil") +: goals.tail
                case Some(v) => v +: goals.tail
              }

	      branch(contextIds, res, depth)
	    case _ => // a constant
	      val result = Structure(StrLit("_unify"), Variable(leftVar) +: rightTerm +: Nil)
	      branch(contextIds, result +: goals.tail, depth)
          }
	  case _ => throw UnSafeException(s"RightTerm is not grounded: $xterms")
	}
	case other => throw UnSafeException(s"LeftTerm should be a variable: $other")
      }
      case Structure(StrLit("_interpolate"), Constant(body, attrName, tpe, _) +: Constant(termSeq, _, _, _) +: xterms, _, _, _) =>
        val res: String = Term.interpolate(body.name, termSeq.name, xterms)
        Seq((x=>Constant(StrLit(res), attrName, tpe, Encoding.AttrLiteral), goals.tail))
      case Structure(StrLit("rootId"), speaker +: xterms, attrName, tpe, _)  =>      // TODO: should rootId have speaker?
        if(xterms == Nil) throw UnSafeException(s"speaker undefined for the term: $speaker")
        val arg = recurse(xterms).map {
          case subst  => subst(xterms.head)
        }
        val xarg = if(arg.isEmpty) xterms.head.id.name else arg.head.id.name
        val argArray = xarg.split("\\:")
        val result = Constant(StrLit(s"${argArray(0)}"), attrName, tpe, Encoding.AttrLiteral)
	Seq(((x: Term) => result, goals.tail))
      case Structure(StrLit("speaksFor"), subjectSpeaker +: issuer +: subject +: other, _, _, _) if (
        (subjectSpeaker == issuer) && (issuer == subject)
      ) => Seq((x=>x, goals.tail))
      case Structure(StrLit("speaksForOn"), subjectSpeaker +: issuer +: subject +: other, _, _, _) if(
        (subjectSpeaker == issuer) && (issuer == subject)
      ) => Seq((x=>x, goals.tail))
      case Structure(printPred, xterms, _, _, _) if(
           (printPred == StrLit("print")) 
         | (printPred == StrLit("println"))
       ) => xterms.headOption match {

        case Some(c: Constant) =>
          if(printPred == StrLit("println")) println(c) else print(c)
          Seq((x=>x, goals.tail))
        case _                 =>
          val res = recurse(xterms).map(subst => subst(xterms.head))
          if(printPred == StrLit("println")) println(res.mkString(", ")) else print(res.mkString(", "))
          Seq((x=>x, goals.tail))
      }
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
      // case NegatedTerm(name, xterm) => branch(contextIds, Seq(xterm), depth) 
      // Note: using solveAQuery() here instead of branch() to keep the @tailrec satisfied for branch()
      // TODO: May overflow stack if number of negated terms are more.
      case NegatedTerm(name, xterm, _, _, _) => solveAQuery(contextIds, Query(Seq(xterm)), false) match {
	case Nil => Seq((x=>x, goals.tail))
	case _   => Nil
      }
      case other => solveGoalTerm(other, goals, depth)
    }


    def evalSeq(terms: Seq[Term]): Seq[Term] = {
      val evaledTerms = terms match {
        case Structure(StrLit("_seq"), Nil, _, _, _) +: rest      => Constant(StrLit("[]")) +: evalSeq(rest)
        case Structure(StrLit("_seq"), subTerms, _, _, _) +: rest => subTerms match {
          case Structure(StrLit("_seq"), moreSubTerms, _, _, _) +: tail =>
             Structure(StrLit("_seq"),
               substTerm(subTerms.head, recurse(Seq(subTerms.head)).toIndexedSeq) ++: evalSeq(subTerms.tail)
             ) +: evalSeq(rest)
          case _ => Structure(StrLit("_seq"), evalSeq(subTerms)) +: evalSeq(rest)
        }
        case Structure(_, _, _, _, _) +: rest => substTerm(terms.head, recurse(Seq(terms.head)).toIndexedSeq) ++: evalSeq(rest)
        case Constant(_, _, _, _) +: rest => Seq(terms.head) ++: evalSeq(rest)
        case x +: rest => substTerm(x, recurse(Seq(x)).toIndexedSeq) ++: evalSeq(rest) // FunTerm or SetTerm
        case Nil => Nil
      }
      evaledTerms
    }

    def substTerm(term: Term, substFns: Seq[Term => Term]): Seq[Term] = {
      substFns.map(subst => subst(term))
    }

    import scala.util.control.ControlThrowable
    //case class Returned[A](value: A) extends ControlThrowable {}
    case class Returned(value: Iterable[Term => Term]) extends ControlThrowable {}
    //def shortcut[A](a: => A) = try { a } catch { case Returned(v) => v }

    //@annotation.tailrec
    def recurse(goals: Seq[Term], depth: Int = 1): Iterable[Term => Term] = {
      if(goals == Nil) {
       /*
        val solutionsRaw = for {
	  subst <- result
        } yield query.terms.map(subst)
        val solutions = solutionsRaw.map(x => Assertion(x))

        if(query.terms.length > 1) success(Assertion(Constant("query(..)") +: query.terms), isInteractive)
	else success(Assertion(query.terms), isInteractive)
	*/
	//Seq(x => x)
        //val f: Seq[Term => Term] = Seq(x => x)
	//throw Returned(f)
        Seq(x => x)
      }
      else if(depth > Config.config.maxDepth) {
	failure(query, "ran out of maximum depth", isInteractive)
	//System.exit(1) // just quit
	Seq(x => x) // probably going to infinite recurse //TODO: fix this
      } else {
	  val endTime1 = System.nanoTime()
	  for {
	    //(solutionMayBe, freshGoalsMayBe) <- branch(contextIds, goals, depth) // branch for new goals
	    //_ = memoGoalTerm(goals.head, true)                                   // register the goal as visited
	    (solution, freshGoals) <- branch(contextIds, goals, depth) // branch for new goals
	    //(solution, freshGoalsMayBe) <- branch(contextIds, goals, depth) // branch for new goals
	    //(solution, freshGoals) = findFreshGoals(solutionMayBe, freshGoalsMayBe)
	    //freshGoals = admissibilityTest(goals.head, freshGoalsMayBe)
	    endTime2 = System.nanoTime()
	    _ = logger.info(s"Time for branch at depth $depth is: ${(endTime2 - endTime1)/Math.pow(10, 6)} ms")
	    //_ = println(s"Time for branch at depth $depth is: ${(endTime2 - endTime1)/Math.pow(10, 6)} ms")
	    //_ = println(s"FreshGoals: $freshGoals; solution: " + solution(goals.head))
	    remainingSolutions <- recurse(freshGoals, depth + 1)
	    //_ = println(solution)
	    //_ = println(remainingSolutions)
	    //_ = println(s"$depth")
	    //_ = println(s"goal: $freshGoals")
	  } yield (solution andThen remainingSolutions)

          /*
	  val branchResult = branch(contextIds, goals, depth).iterator

          @annotation.tailrec
          @inline
	  def branchLoop(preGoals: Seq[Term], acc: Iterable[Term => Term]): Iterable[Term => Term] = { 
            if(branchResult.hasNext) {
	      val (solution, freshGoals) = branchResult.next
	      //val isMatch = admissibilityTest(goals.head, freshGoals)
	      //val isMatch = Term.subsumes(freshGoals.head, goals.head)
	      //if(isMatch == true) {
	      if(false) {
		//println(s"isMatch: $isMatch, ${goals.head}, ${freshGoals.head}")
		val newGoals = freshGoals.tail ++ Seq(freshGoals.head) ++ preGoals // reorder
		branchLoop(newGoals, acc)
	      } else {
		val newGoals = freshGoals ++ preGoals
		//println(s"newGoals: $newGoals")
		//scala.io.StdIn.readLine()
		val out = for {
		  remainingSolutions <- recurse(newGoals, depth + 1)
		} yield (solution andThen remainingSolutions)
		//out ++ branchLoop(Nil)
		branchLoop(Nil, out ++ acc)
	      }
	    } else {
	      acc
	    }
         }
         branchLoop(Nil, Nil)
         */
      }
    }
    val result = try {recurse(query.terms)} catch { case Returned(v) => v }
    result.toSeq
  }

  def admissibilityTest(currentGoal: Term, freshGoals: Seq[Term]): Boolean = freshGoals match {
    case Nil => true
    case goal :: tail =>
      mostGenericUnifier(currentGoal, goal) match {
        case None => //println(s"no match: currentGoal: $currentGoal; freshGoal: $goal")
          //scala.io.StdIn.readLine()
          //freshGoals
          true
        case _    => //println(s"match: currentGoal: $currentGoal; freshGoal: $goal")
          //scala.io.StdIn.readLine()
          //admissibilityTest(currentGoal, tail) :+ goal
          false
      }
  }

  /*
  // TODO: this seems very expensive; analyze
  def findFreshGoals(solution: Term => Term, goals: Seq[Term]): (Term => Term, Seq[Term]) = goals match {
    case Nil => (solution, Nil)
    case goal :: tail =>
      //println(s"Goals: $goals; visitedGoals: ${__memoGoalTerm}")
      __memoGoalTerm.map {
	case (term: Term, visited: Boolean) => mostGenericUnifier(term, goal) collect {
	  case x => //println(s"Goal found: $goal"); return (x, tail)
	}
      }
      //println(s"goal not found: $goal")
      (solution, goals)
  }
  */

  /*
   * Seminaive Algorithm
   * Input: program, query
   * 
   *   p(i+1) :- p1(i), ..., pn(i), q1, ..., qm.
   * 
   *   foreach idb predicate p {
   *     p(0)       := Nil
   *     delta_p(0) := tuples produced by rules using only edb's
   *   
   *     i := 1
   *   
   *	 do {
   *	   p(i) := p(i - 1) union delta_p(i - 1)
   *	   compute bigDelta_p(i)
   *	   delta_p(i) := bigDelta_p(i) - p(i)
   *	   i := i + 1
   *	 } while delta_p(i) == Nil
   *   }
   * 
   *  
   *   evaluate(bigDelta_p(i)) {
   * 
   *     bigDelta_p(i) :- delta_p1(i-1), p2(i-1), ..., pn(i-1), q1, ..., qm.
   *     bigDelta_p(i) :- p1(i-1), delta_p2(i-1), ..., pn(i-1), q1, ..., qm.
   *     ...
   *     bigDelta_p(i) :- p1(i-1), p2(i-1), ..., delta_pn(i-1), q1, ..., qm.
   *
   *     p(i+1)        := p(i) union bigDelta_p(i)
   *   }
   */

  /*
  def solveWithForwardChaining(program: Seq[Statement], query: Statement, isInteractive: Boolean = false): Seq[Term] = {
    def findGroundRules(idb: Set[Statement], idbPredicates: Set[Term]): Set[Statement] = {
      import scala.collection.mutable.{Set => Set}
      var groundRules: Set[Statement] = Set.empty
      idb.foreach { idbRule =>
        val currentIdbRulePredicates: Set[Term] = idbRule.terms.toSet
        if(currentIdbRulePredicates.diff(idbPredicates) == Set.empty) groundRules += idbRule
      }
      groundRules.toSet
    }

    def partitionProgram(program: Set[Statement]): Tuple2[Set[Statement], Set[Statement]] = {
      import scala.collection.mutable.{Set => Set}
      var edb: Set[Statement] = Set.empty
      var idb: Set[Statement] = Set.empty
      program.foreach { statement =>
       if(statement.arity < 2) edb += statement else idb += statement
      }
      Tuple2(edb.toSet, idb.toSet)
    }

    def computeBigDelta(): Set[Term] = Set.empty // TODO

    val programSet: Set[Statement] = program.toSet
    val edb: Set[Statement] = programSet.filter(_.terms.length < 2) // all ground facts
    val idb: Set[Statement] = programSet diff edb
    val idbPredicates: Set[Term] = idb.map {statement => statement.terms.head}

    val groundRules: Set[Statement] = findGroundRules(idb, idbPredicates) // i.e., rules with no idb predicates in the body

    idbPredicates foreach { idbPredicate =>
      var pItr: Set[Term] = Set.empty
      var deltaItr: Set[Term] = {
        //val resultTerms: Iterable[Term => Term] = solveAQuery(groundRules.toSeq, Query(Seq(idbPredicate)), false)
        //resultTerms.map{subst => subst(idbPredicate)}.toSet
        Set.empty // TODO
      }
      var pNextItr: Set[Term] = Set.empty
      var deltaNextItr: Set[Term] = Set.empty
      var bigDeltaNextItr: Set[Term] = Set.empty
      while(!deltaItr.isEmpty) {
        pNextItr = pItr union deltaItr
        bigDeltaNextItr = computeBigDelta()
        deltaNextItr = bigDeltaNextItr diff pNextItr
        pItr = pNextItr
        deltaItr = deltaNextItr
      }
    }

    Nil
   */

    /*
    def buildModel(): Seq[Term] = {
      var knownFacts: Seq[Term] = Nil
      var increment: Seq[Term] = program.filter(_.arity < 2) // all ground facts
      while(increment != Nil) {
	val insts = eval(knownFacts, increment)
	knownFacts = increment ++: knownFacts
	increment = insts.head
      }
      knownFacts
    }
    */
  //}

  /*
  def importIfPresent(program: Map[Index, Set[Statement]]): Map[Index, Set[Statement]] = {
    val importStatements = program.get('import1)
    if(importStatements == null) program
    else {
       importStatements foreach { stmt =>
         importProgram(stmt, program)
       }
    }
    program //TODO: yield and flatten?
  }

  def importProgram(stmt: Statement, program: ConcurrentHashMap[Index, Set[Statement]]): ConcurrentHashMap[Index, Set[Statement]] = stmt.terms.head match {
    case Structure('import, Seq(Constant(fileName))) => printLabel('info); println(s"importing program with name $fileName ...") 
      val file = fileName.name
      val fStream = new java.io.InputStreamReader(this.getClass().getClassLoader().getResourceAsStream(file))
      val importedProgram = parse(fStream)
      dropStatement(stmt, program)
      importIfPresent(importedProgram)
    case _ => program
  }
  */

  /*
  lazy val __memoGoalTerm = scala.collection.mutable.Map.empty[Term, Boolean]
  def memoGoalTerm(goal: Term, visited: Boolean): Boolean = synchronized {
    __memoGoalTerm.getOrElseUpdate(goal, visited)
  }
  */
}

class Inference() extends InferenceService with InferenceImpl

