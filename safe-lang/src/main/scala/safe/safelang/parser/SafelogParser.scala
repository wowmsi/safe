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

import safe.safelog.{Assertion, ParserException, Statement, Structure, Term, Validity, MutableCache, Index, StrLit}

import model.{UnsignedCredential, UnsignedUnBoundCredential, SignedCredentialHolder, SpeaksFor, SpeaksForOn, SpeakerStmt, SubjectStmt, Identity}

import model.CredentialHelper._

trait SafelogParser extends safe.safelog.parser.ParserImpl {
  this: safe.safelog.ParserService =>

  /** Parse the unsigned credential from source string and extract the metadata and statements to UnsignedUnBoundCredential */
  def parseUnsignedCredential(
      source: String
    , args: Map[StrLit, Term]
    , nameMayBe: Option[Term] = None
    , immutable: Boolean = false
  ): UnsignedUnBoundCredential = {

    val _metadataCache: MutableCache[Index, MutableSet[Statement]] = new MutableCache[Index, MutableSet[Statement]]

    val res = super.parseSlog(source) match {

      case Success(_result, _) =>

        // filter meta predicates
        safe.safelog.Config.config.metadata.map {
          //case (id, value) => 
          case id => 
           val metaPredicate: StrLit = StrLit(s"_${id.name}")
           _metadataCache.put(metaPredicate, _result.get(metaPredicate).getOrElse(MutableSet.empty[Statement]))
           _result -= metaPredicate
        }
        // filter queries and retractions
        _metadataCache.put(StrLit("_query"), _result.get(StrLit("_query")).getOrElse(MutableSet.empty[Statement]))
        _result           -= StrLit("_query")
        _metadataCache.put(StrLit("_retraction"), _result.get(StrLit("_retraction")).getOrElse(MutableSet.empty[Statement]))
        _result           -= StrLit("_retraction")

        val assertions: Map[Index, Set[Statement]]     = _result.map {kv => (kv._1, kv._2.toSet)}.toMap
        val metadata: Map[Index, Set[Statement]]       = _metadataCache.map {kv => (kv._1, kv._2.toSet)}.toMap

	UnsignedUnBoundCredential(
            metadata
          , assertions
          , nameMayBe
          , args
          , immutable
          , source
        )
      case failure: NoSuccess => throw ParserException(s"${failure.msg}")
    }
    res
  }

  def parseSignedCredential(speaker: String, name: String, source: String): SignedCredentialHolder = {
     val res = super.parseSlog(source) match {
      case Success(_result, _) =>
        //println(s"PARSESIGNED: SOURCE: $source; RESULT: ${_result}")
        //scala.io.StdIn.readLine()
        val queries        = _result.get(StrLit("_query")).getOrElse(Nil).toSeq
        _result           -= StrLit("_query")

        val links: Seq[String]   = getAllFirstAttributes(_result.get(StrLit("_link")).getOrElse(Nil).toSeq)
        _result           -= StrLit("_link")

        // public key
        val principal      = getFirstAttribute(getUniqueStatement(_result.get(StrLit("_principal")).getOrElse(Nil).toSeq))
        //_result           -= StrLit("_principal")

        val speaksFor: Seq[SpeaksFor]     = _result.get(StrLit("_speaksFor")).getOrElse(Nil).toSeq.map(x => SpeaksFor(x))
        _result           -= StrLit("_speaksFor")
        val speaksForOn: Seq[SpeaksForOn] = _result.get(StrLit("_speaksForOn")).getOrElse(Nil).toSeq.map(x => SpeaksForOn(x))
        _result           -= StrLit("_speaksForOn")
        val assertions     = _result.map {kv => (kv._1, kv._2.toSet)}.toMap
        SignedCredentialHolder(name, source.replaceAll("\\s+$", ""), assertions, queries, links, principal, speaksFor, speaksForOn)
      case failure: NoSuccess => throw ParserException(s"${failure.msg}")
    }
    res
  }

  def parseEncryptedSignedCredential(
      principal: model.Principal
    , recipientId: String
    , encryptedSharedKey: String
    , encryptedData: String
    , encryptionAlgorithm: String
  ): String = {

    if(principal.scid.toString != Term.stripQuotes(recipientId)) {
      throw ParserException(s"Recipient and Subject does not match: $recipientId, ${principal.scid}")
    }
    val signedData: String = principal.decrypt(encryptedSharedKey, encryptedData, encryptionAlgorithm)
    signedData
  }
}
