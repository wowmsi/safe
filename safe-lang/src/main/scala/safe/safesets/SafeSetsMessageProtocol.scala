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



package safe.safesets

import akka.actor.ActorRef

import safe.safelog.{Credential, ProofSubContext, StrLit}
import safe.safelang.model.{Principal, Subject, SignedCredential, UnsignedCredential}

object SafeSetsMessageProtocol {

  sealed trait SafeSetsMessage

  type SetId = String

  case class Fetch(link: SetId) extends SafeSetsMessage

  case class Render(
      link: SetId
    , setRefs: Set[SetId]
  ) extends SafeSetsMessage

  case class RenderCompleted(
      link: SetId
    , proofContext: ProofSubContext
  ) extends SafeSetsMessage

  // 1. master -> worker
  case class FetchSet(
      link: SetId
    , originLink: SetId
    , speaksForOriginLink: SetId
    , speakerId: SetId
    , subjectId: SetId
    , setName: String
  ) extends SafeSetsMessage

  object FetchSet {
    def apply(link: SetId, originLink: SetId) = {
      new FetchSet(link, originLink, "nil", "nil", "nil", "nil")
    }
  }

  // 2. worker -> master (for the leaf nodes)
  case class FetchIdSet(
      link: SetId
    , originLink: SetId
  ) extends SafeSetsMessage

  case class PostSetWithName(
      name: String
    , unsignedCertificate: UnsignedCredential
    , principal: Principal
  ) extends SafeSetsMessage

  case class SimplePost(
      id: SetId
    , content: String
  ) extends SafeSetsMessage

  case class SimpleGet(
      id: SetId
  ) extends SafeSetsMessage

  case class SimpleDelete(
      id: SetId
  ) extends SafeSetsMessage

  case class DeleteSet(
      link: SetId
    , principal: Principal
  ) extends SafeSetsMessage

  case class DeleteSetWithName(
      name: String
    , principal: Principal
  ) extends SafeSetsMessage

  // 3. master -> worker (if subject is already present in cache)
  case class IdSet(
      link: SetId
    , subject: Subject
    , originLink: SetId
  ) extends SafeSetsMessage

  // 4. master -> worker (after fetch/post/delete is done)
  case class IOCompleted(
      setMap: Map[SetId, SignedCredential]
    , originLink: SetId
  ) extends SafeSetsMessage

  case class VerificationFailed(link: SetId) extends SafeSetsMessage
  case class ParseFailed(link: SetId, msg: String) extends SafeSetsMessage
}
