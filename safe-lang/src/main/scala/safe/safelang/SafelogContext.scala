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

import safe.safelog.{Index, MutableCache, Statement}

class SafelogContext() extends safe.safelog.Inference

class SafelogParserContext(
    val self: String
  , val saysOperator: Boolean
  , val _statementCache: MutableCache[Index, MutableSet[Statement]]
) extends safe.safelog.Inference with safe.safelog.ParserService with parser.SafelogParser {
  require(saysOperator == true)
}

object SafelogParserContext {
  def apply(self: String) = new SafelogParserContext(self, true, new MutableCache[Index, MutableSet[Statement]]())
}
