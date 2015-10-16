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

import safe.cache.SafeCache

trait InferenceService {

  private[safelog] implicit val envContext: SafeCache[StrLit, EnvValue] = new SafeCache[StrLit, EnvValue](
      1024
    , 0.75f
    , 1
  )

  private[safelog] implicit val proofContext: SafeCache[SetId, ProofSubContext] = new SafeCache[SetId, ProofSubContext](
      1024        // initialCapacity, i.e., only one proof context by default with SetId as object
    , 0.75f       // loadFactor; we do not expect to rehash often
    , 1           // concurrencyLevel; not many writers at the same time // TODO: change
  )

  /**
   * solve: given a parsed data set, solve a sequence of queries
   */
  def solve(
      renderedSet: Map[Index, Set[Statement]]
    , queries: Seq[Statement]
    , isInteractive: Boolean
  ): Seq[Seq[Statement]]

  // localSlangContext is the set of statements made in a defguard and defcon other than the import statements from the context
  // useful when slang invokes slog
  def solveWithContext(
      contextIds: Set[SetId]
    , queries: Seq[Statement]
    , isInteractive: Boolean
  )(
      envContext: SafeCache[StrLit, EnvValue]
    , localSlangContext: Map[Index, Set[Statement]]
    , proofContext: SafeCache[SetId, ProofSubContext]
  ): Seq[Seq[Statement]] // Note: import statements are part of envContext

  def solveWithValue(
      renderedSet: Map[Index, Set[Statement]]
    , queries: Seq[Statement]
    , isInteractive: Boolean
  ): Seq[Seq[Map[String, Seq[String]]]]
}

object InferenceService {
}
