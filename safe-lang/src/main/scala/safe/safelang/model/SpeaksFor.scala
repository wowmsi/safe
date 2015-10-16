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
package model

import safe.safelog.{Assertion, Constant, Statement, Structure, UnSafeException, StrLit}

case class SpeaksFor(speaker: Id, subject: Id, delegatable: Boolean)

object SpeaksFor {
  def apply(statement: Statement): SpeaksFor = statement match {
    case Assertion(Structure(StrLit("speaksFor"), Constant(speaker, _, _, _) +: Constant(subject, _, _, _) +: Nil, _, _, _) +: Nil) => 
      new SpeaksFor(Id(speaker.toString), Id(subject.toString), false)
    case Assertion(Structure(StrLit("speaksFor"), 
         Constant(speaker, _, _, _) 
      +: Constant(subject, _, _, _) 
      +: Constant(delegatable, _, _, _) 
      +: Nil, _, _, _) 
    +: Nil) => 
      new SpeaksFor(Id(speaker.toString), Id(subject.toString), if(delegatable.toString == "true") true else false)
    case _ => throw UnSafeException(s"Not a valid speaksFor statement")
  }
}

case class SpeaksForOn(speaker: Id, subject: Id, objectId: Scid, delegatable: Boolean)

object SpeaksForOn {
  def apply(statement: Statement): SpeaksForOn = statement match {
    case Assertion(Structure(StrLit("speaksForOn"), 
         Constant(speaker, _, _, _) 
      +: Constant(subject, _, _, _) 
      +: Constant(objectId, _, _, _) 
      +: Nil, _, _, _) 
    +: Nil) => 
      new SpeaksForOn(Id(speaker.toString), Id(subject.toString), Scid(objectId.toString), false)
    case Assertion(Structure(StrLit("speaksForOn"), 
         Constant(speaker, _, _, _) 
      +: Constant(subject, _, _, _) 
      +: Constant(objectId, _, _, _) 
      +: Constant(delegatable, _, _, _)
      +: Nil, _, _, _) 
    +: Nil) => 
      new SpeaksForOn(Id(speaker.toString), Id(subject.toString), Scid(objectId.toString), if(delegatable.toString == "true") true else false)
    case _ => throw UnSafeException(s"Not a valid speaksForOn statement")
  }
}
