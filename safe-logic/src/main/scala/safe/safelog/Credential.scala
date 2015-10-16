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

trait Credential extends com.typesafe.scalalogging.LazyLogging {
  val validity: Validity
  val statements: Map[Index, Set[Statement]]
  val immutable: Boolean

  def get(idx: Index): Option[Set[Statement]] = {
    if(validity.isValidOn(Validity.now().plusSeconds(1))) statements.get(idx)
    else {
      logger.error(s"Validity for proof context with id $idx failed")
      None
    }
  }
  def values(): Option[Set[Statement]] = {
    if(validity.isValidOn(Validity.now().plusSeconds(1))) Some(statements.values.flatten.toSet)
    else {
      logger.error(s"Validity for proof context failed")
      None
    }
  }
  def mergeCredential(credential: Credential): Credential = this // implement this in case classes
  def bind(f: StrLit => Term): Credential = this // implement this in case classes
  def interpolate(args: Map[StrLit, Term]): Credential = this // implement this in case classes; args: Map[StrLit, Constant]
}

