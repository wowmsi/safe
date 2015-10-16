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
  
import safe.cache.SafeCache
    
import safe.safelog.{Term, SetId, ProofSubContext, Credential, Index, Statement, EnvValue, StrLit}
import model._
    
trait InferenceService extends safe.safelog.InferenceService {
  private[safe] implicit override val envContext: SafeCache[StrLit, EnvValue] = new SafeCache[StrLit, EnvValue](
      128
    , 0.75f
    , 1
  )

  /**
   * solveSlang: given a parsed data set ("_object"), solve a sequence of queries
   */     
  def solveSlang(
      queries: Seq[Statement]
    , isInteractive: Boolean
  )(implicit
      proofContext: SafeCache[SetId, ProofSubContext]
  ): Seq[Seq[Statement]]
}

object InferenceService {
  private[safe] implicit val proofContext: SafeCache[SetId, ProofSubContext] = new SafeCache[SetId, ProofSubContext](
      1024         // initialCapacity, i.e., 1024 statements in a context
    , 0.75f        // loadFactor; we do not expect to rehash often
    , 16           // concurrencyLevel; not many writers at the same time
  )
}
