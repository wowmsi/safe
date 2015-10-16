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

trait SafelogImpl {
  self: InferenceService with ParserService =>

  //def solve(source: String): Seq[Seq[Statement]]
  //def solve(source: java.io.Reader): Seq[Seq[Statement]]

  //def solveWithValue(source: String): Seq[Seq[Map[String, Seq[String]]]]
  //def solveWithValue(source: java.io.Reader): Seq[Seq[Map[String, Seq[String]]]]
  
  def solve(source: String): Seq[Seq[Statement]] = {
    val renderedSet = parseAsSegments(source)
    solve(renderedSet._1, renderedSet._2, false)
  }
  def solve(source: java.io.Reader): Seq[Seq[Statement]] = {
    val renderedSet = parseAsSegments(source)
    solve(renderedSet._1, renderedSet._2, false)
  }
  def solveWithValue(source: String): Seq[Seq[Map[String, Seq[String]]]] = {
    val renderedSet = parseAsSegments(source)
    solveWithValue(renderedSet._1, renderedSet._2, false)
  }
  def solveWithValue(source: java.io.Reader): Seq[Seq[Map[String, Seq[String]]]] = {
    val renderedSet = parseAsSegments(source)
    solveWithValue(renderedSet._1, renderedSet._2, false)
  }
}

trait SafelogService extends InferenceService 
  with ParserService 
  with SafelogImpl 
  with InferenceImpl 
  with parser.ParserImpl

class Safelog(
    val self: String
  , val saysOperator: Boolean
  , val _statementCache: MutableCache[Index, MutableSet[Statement]]
) extends SafelogService

object Safelog {
  def apply() = new Safelog(Config.config.self, Config.config.saysOperator, new MutableCache[Index, MutableSet[Statement]]())
}
