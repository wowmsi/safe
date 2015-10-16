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
package client

import akka.actor.ActorRef
import scala.collection.immutable.Queue
import scala.collection.mutable.{Set => MutableSet}
import scala.language.existentials // for wild cards: Work[_]

import MasterWorkerMessageProtocol.{WorkerId, WorkId}


// WorkState on Master

object WorkState {

  def empty: WorkState  = WorkState(
      workPending         = Queue.empty
    , workAccepted        = Set.empty
    , workInProgress      = Map.empty
    , workCompleted       = Set.empty
    , workRequester       = Map.empty // store the requester reference
  )

  sealed trait WorkDomainEvent

  case class WorkAccepted(
      work: Work[_]
    , requester: Option[ActorRef] = None
  ) extends WorkDomainEvent
  case class WorkStarted(workId: WorkId) extends WorkDomainEvent
  case class WorkCompleted(workId: WorkId, result: Any) extends WorkDomainEvent
  case class WorkerFailed(workId: WorkId) extends WorkDomainEvent
  case class WorkerTimedOut(workId: WorkId) extends WorkDomainEvent
}

case class WorkState private (
    private val workPending: Queue[Work[_]]
  , private val workAccepted: Set[WorkId]
  , private val workInProgress: Map[WorkId, Work[_]]
  , private val workCompleted: Set[WorkId]
  , private val workRequester: Map[WorkId, Option[ActorRef]]
) {

  import WorkState._

  def hasWork: Boolean                      = workPending.nonEmpty
  def nextWork: Work[_]                     = workPending.head
  def isAccepted(workId: WorkId): Boolean   = workAccepted.contains(workId)
  def isInProgress(workId: WorkId): Boolean = workInProgress.contains(workId)
  def isDone(workId: WorkId): Boolean       = workCompleted.contains(workId)
  def isDoneRef(workId: WorkId): ActorRef   = {
    require(isDone(workId) == true) // TODO: tmp
    workRequester(workId).get   // .getOrElse(sys.error(s"Requester not found")) // TODO
  }
  def originRef(workId: WorkId): ActorRef   = {
    workRequester(workId).get   // .getOrElse(sys.error(s"Requester not found")) // TODO
  }

  def updated(event: WorkDomainEvent): WorkState = event match {
    case WorkAccepted(work, requester: Some[_]) =>
      copy(
          workPending     = workPending enqueue work
        , workAccepted    = workAccepted + work.workId
        , workRequester   = workRequester + (work.workId -> requester)
      )

    case WorkAccepted(work, requester: None.type) => // requester: None
      copy(
          workPending     = workPending enqueue work
        , workAccepted    = workAccepted + work.workId
      )

    case WorkStarted(workId) =>
      val (work, tail) = workPending.dequeue
      require(workId == work.workId, s"WorkStarted expected workId $workId == ${work.workId}")
      copy(
          workPending    = tail
        , workInProgress = workInProgress + (workId -> work)
      )

    case WorkCompleted(workId, result) =>
      copy(
          workCompleted       = workCompleted  + workId
        , workInProgress      = workInProgress - workId
      )

    case WorkerFailed(workId) =>
      copy(
          workPending    = workPending enqueue workInProgress(workId)
        , workInProgress = workInProgress - workId
      )

    case WorkerTimedOut(workId) =>
      copy(
          workPending    = workPending enqueue workInProgress(workId)
        , workInProgress = workInProgress - workId
      )
  }
}
