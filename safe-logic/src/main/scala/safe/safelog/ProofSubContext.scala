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

case class ProofSubContext(
    id: SetId
  , validity: Validity
  , statements: Map[Index, Set[Statement]]
  , queries: Seq[Statement] = Nil
  , immutable: Boolean = false
) extends Credential

object ProofSubContext {
  def apply(statements: Map[Index, Set[Statement]]) = new ProofSubContext(StrLit("_object"), Validity(), statements)
  def apply(id: SetId, statements: Map[Index, Set[Statement]]) = new ProofSubContext(id, Validity(), statements)
  def apply(id: SetId, statementSet: Set[Statement]) = { // TODO: reserved predicates?
    val indexedStatements: Map[Index, Set[Statement]] = indexStatements(statementSet)
    new ProofSubContext(id, Validity(), indexedStatements)
  }

  private def indexStatements(statements: Set[Statement]): Map[Index, Set[Statement]] = {
    val _statementCache: MutableCache[Index, MutableSet[Statement]] = new MutableCache[Index, MutableSet[Statement]]()
    def addStatement(index: Index, s: Statement): MutableSet[Statement] = {
      val stmts: MutableSet[Statement] = _statementCache.get(index) map {
	v => v +=s
      } getOrElse {
	val _newSet = MutableSet.empty[Statement]
	_newSet += s
      }
      _statementCache.put(index, stmts)
      stmts
    }
    statements.foreach {stmt => addStatement(stmt.primaryIndex, stmt) }
    _statementCache.map {kv => (kv._1, kv._2.toSet)}.toMap
  }
}
