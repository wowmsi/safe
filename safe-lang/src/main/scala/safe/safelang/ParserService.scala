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

import safe.safelog.{Index, MutableCache, Statement, ParserException}

import model.SignedCredentialHolder

trait ParserService extends safe.safelog.ParserService {
  private[safe] def parseCertificate(source: String): SetTerm
  private[safe] def parseCertificate(source: java.io.Reader): SetTerm
  private[safe] def parseFileAsCertificate(fileName: String): SetTerm
}

class Parser(
    val self: String
  , val saysOperator: Boolean
  , val _statementCache: MutableCache[Index, MutableSet[Statement]]
) extends ParserService with parser.ParserImpl

object Parser {
  def apply() = new Parser(Config.config.self, Config.config.saysOperator, new MutableCache[Index, MutableSet[Statement]]())
  def apply(self: String) = new Parser(self, true, new MutableCache[Index, MutableSet[Statement]]())

  def parseSignedCredential(speaker: String, name: String, stmts: String): SignedCredentialHolder = (name, stmts) match {
    case ("", "") => throw ParserException(s"identity set cannot be empty")
    case ("", _) => SafelogParserContext(speaker).parseSignedCredential(speaker, "", stmts)
    case (_, "") => SignedCredentialHolder(name, "", Map.empty[Index, Set[Statement]], Nil, Nil, None, Nil, Nil)
    case _       => SafelogParserContext(speaker).parseSignedCredential(speaker, name, stmts)
  }
}
