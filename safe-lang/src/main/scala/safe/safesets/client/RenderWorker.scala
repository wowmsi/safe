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



package safe.safesets
package client

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import org.joda.time.DateTime
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

import safe.cache.SafeCache
import safe.safelog.Credential
import safe.safelog.{Credential, ProofSubContext, Index, Statement, Validity, StrLit}
import safe.safelang.model._

import safe.safesets.SafeSetsMessageProtocol._

object RenderWorker {
  def props(setCache: SafeCache[SetId, SignedCredential]): Props = Props(classOf[RenderWorker], setCache)
  def name: String   = "RenderWorker"
}

class RenderWorker(setCache: SafeCache[SetId, SignedCredential]) extends Actor with ActorLogging {

  def receive: Receive = {
    case Render(refId: SetId, setRefs: Set[SetId]) => 
      sender() ! RenderCompleted(refId, render(refId, setRefs))
  }

  def render(refId: SetId, setRefs: Set[SetId]): ProofSubContext = {
    var notBefore: DateTime = new DateTime(2100, 5, 11, 1, 0)
    var notAfter: DateTime  = new DateTime(2010, 5, 11, 1, 0)
    val statements: MutableMap[Index, MutableSet[Statement]] = MutableMap.empty
    val queries: MutableSet[Statement] = MutableSet.empty

    //println(s"In Renderer: setRefs: $setRefs")

    setRefs.foreach { ref =>
      val signedCertificate: SignedCredential = setCache.get(ref).getOrElse(throw new Exception(s"Missing set with id $refId in setCache"))
      val signedStatements: Map[Index, Set[Statement]] = signedCertificate.statements 

      //println(s"\nsignedStatements: $ref -> ${signedStatements.values}\n")

      val signedQueries: Seq[Statement] = signedCertificate.queries
      val signedValidity: Validity = signedCertificate.validity

      // merge; retractions might not occur from the published certificates unless they are part of the defguard query
      signedStatements foreach {
        case (index, stmts) =>
          val mergedStatements = statements.get(index).getOrElse(MutableSet.empty) ++ stmts
          statements.put(index, mergedStatements)
      }

      queries ++= signedQueries
      if(notBefore.isAfter(signedValidity.notBefore)) {
        notBefore = signedValidity.notBefore
      }
      if(notAfter.isBefore(signedValidity.notAfter)) {
        notAfter = signedValidity.notAfter
      }
    }
    val allStatements = statements.map {
      case (index, stmts) => (index, stmts.toSet)
    }.toMap
    ProofSubContext(StrLit(refId), Validity(notBefore, notAfter), allStatements, queries.toSeq)
  }
}
