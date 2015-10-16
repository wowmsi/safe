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

import scala.collection.mutable.{Set => MutableSet}

trait ParserService {

  private[safelog] val saysOperator: Boolean
  private[safelog] val self: String          
  // _statementCache is accessed from Repl
  private[safelog] val _statementCache: MutableCache[Index, MutableSet[Statement]]


  // A cache of set of statements indexed by statement head functor + arity + principal
  // No support for concurrent operations at safelog level.
  // Used in repl and parser
  private[safe] def parseCmdLine(source: String): Tuple2[Option[MutableCache[Index, MutableSet[Statement]]], Symbol] // for REPL
  private[safe] def parseFileFresh(speaker: String, fileName: String): Map[Index, Set[Statement]]

  /**
   * parse: parses a list of logic sentences to a list of statements
   * source is either a string of statements or a file
   */
  def parse(source: String): Map[Index, Set[Statement]]
  def parse(source: java.io.Reader): Map[Index, Set[Statement]]
  def parseFile(fileName: String): Map[Index, Set[Statement]]

  // def compile(source: String): String // source: fileName, Output: compiled file name
  // def execute(source: String): Seq[Seq[Statement]] // source: compiled file name

  def parseAsSegments(source: String): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]]
  def parseAsSegments(source: java.io.Reader): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]]
  def parseFileAsSegments(fileName: String): Tuple4[Map[Index, Set[Statement]], Seq[Statement], Seq[Statement], Seq[Statement]]
}

class Parser(
    val self: String
  , val saysOperator: Boolean
  , val _statementCache: MutableCache[Index, MutableSet[Statement]]
) extends ParserService with parser.ParserImpl

object Parser {
  def apply() = new Parser(Config.config.self, Config.config.saysOperator, new MutableCache[Index, MutableSet[Statement]]())
  def apply(self: String) = new Parser(self, true, new MutableCache[Index, MutableSet[Statement]]())
}
