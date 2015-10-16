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

import scala.collection.mutable.{Set => MutableSet}

import safe.safelog.{Index, MutableCache, Statement, ProofSubContext, StrLit, UnSafeException, Assertion}

trait SafelangImpl extends safe.safelog.SafelogImpl  {
  slangInference: InferenceService with ParserService with SafeSetsService =>

  def init(source: String): Map[Index, Set[Statement]]
  def init(statements: Map[Index, Set[Statement]]): Map[Index, Set[Statement]]
  def initFile(fileName: String): Map[Index, Set[Statement]]
}

trait SafelangService extends InferenceService 
  with ParserService 
  with SafeSetsService 
  with SafelangImpl 
  with InferenceImpl 
  with parser.ParserImpl {

  private[this] def initHelper(stmts: Map[Index, Set[Statement]]): Map[Index, Set[Statement]] = {
    val defenvGoals = stmts.get(StrLit("defenv0")).getOrElse(Nil).toSeq
    val definitGoals = stmts.get(StrLit("definit0")).getOrElse(Nil).toSeq
    InferenceService.proofContext.put(StrLit("_object"), ProofSubContext(id = StrLit("_object"), statements = stmts))
    val allGoals = defenvGoals.map(s => Assertion(s.terms.tail)) ++ definitGoals
    //println(s"allGoals: $allGoals")
    solveSlang(allGoals, false)
    //println(s"Safelang init env terms: ${envContext.keySet()}")
    //println(s"""Safelang init env terms: ${envContext.values}""")
    stmts
  }

  def init(source: String): Map[Index, Set[Statement]] = {
    val stmts = parse(source)
    initHelper(stmts)
  }
  def init(statements: Map[Index, Set[Statement]]): Map[Index, Set[Statement]] = {
    initHelper(statements)
  }
  def initFile(fileName: String): Map[Index, Set[Statement]] = {
    val stmts = parseFile(fileName)
    initHelper(stmts)
  }
}

class Safelang(
    val safeSetsClient: akka.actor.ActorRef
  , val self: String
  , val saysOperator: Boolean
  , val _statementCache: MutableCache[Index, MutableSet[Statement]]
) extends SafelangService {

}
  
object Safelang {
  def apply(safeSetsClient: akka.actor.ActorRef) = new Safelang(
      safeSetsClient
    , Config.config.self
    , Config.config.saysOperator
    , new MutableCache[Index, MutableSet[Statement]]()
  )
}
