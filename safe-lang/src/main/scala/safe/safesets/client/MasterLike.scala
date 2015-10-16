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

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}

import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

import safe.cache.SafeCache
import safe.safelang.model.{SignedCredential}
import safe.safelog.{ProofSubContext}

import MasterWorkerMessageProtocol._
import SafeSetsMessageProtocol._

trait MasterLikeConfig {
  def workTimeout: FiniteDuration
  
  def setCacheInitialCapacity: Int
  def setCacheLoadFactor: Float
  def setCacheConcurrency: Int

  sealed trait WorkerStatus

  case object Idle extends WorkerStatus
  case class Busy(workId: String, deadline: Deadline) extends WorkerStatus
  case class WorkerState(ref: ActorRef, status: WorkerStatus)
}

trait MasterLike extends Actor with ActorLogging {
  config: MasterLikeConfig =>

  import WorkState._

  private var _workers: Map[MasterWorkerMessageProtocol.WorkerId, WorkerState] = Map.empty
  private var _workState = WorkState.empty
  private val _ioPending: MutableMap[SetId, MutableSet[SetId]]   = MutableMap.empty
  private val _ioComplete: MutableMap[SetId, MutableSet[SetId]]  = MutableMap.empty
  private val _workRequester: MutableMap[SetId, Option[ActorRef]]= MutableMap.empty

  val setCache: SafeCache[SetId, SignedCredential] = new SafeCache[SetId, SignedCredential](
      setCacheInitialCapacity
    , setCacheLoadFactor
    , setCacheConcurrency
  )

  def receive: Receive = {

    case MasterWorkerMessageProtocol.RegisterWorker(workerId) =>
      if(_workers.contains(workerId)) {
        _workers += (workerId -> _workers(workerId).copy(ref = sender()))
      } else {
        log.info(s"Worker registered: $workerId")
        _workers += (workerId -> WorkerState(sender(), status = Idle))
        if (_workState.hasWork)
          sender() ! MasterWorkerMessageProtocol.WorkIsReady
      }

    case MasterWorkerMessageProtocol.WorkerRequestsWork(workerId) =>
      if(_workState.hasWork) {
        val work = _workState.nextWork
        _workers.get(workerId) match {
          case Some(ws @ WorkerState(_, Idle)) =>
            val event = WorkStarted(work.workId)
            _workState = _workState.updated(event)
            log.info(s"Giving worker $workerId some work ${work.workId}")
            _workers += (
              workerId -> ws.copy(status = Busy(work.workId, Deadline.now + workTimeout))
            )
            sender() ! work
          case _ =>  // Status: Busy; ignore
        }
      }

    case MasterWorkerMessageProtocol.WorkIsDone(workerId, workId, result: IOCompleted) =>
      // idempotent
      if (_workState.isDone(workId)) {
        // previous Ack was lost, confirm again that this is done
        sender() ! MasterWorkerMessageProtocol.Ack(workId)
      } else if (!_workState.isInProgress(workId)) {
        log.info(s"Work $workId not in progress, reported as done by worker $workerId")
      }
      else {
        log.info(s"Work $workId is done by worker $workerId")
        //println(s"Work $workId is done by worker $workerId; ioPending: ${_ioPending}; result: $result")
        //println(s"Work $workId is done by worker $workerId; ioPending: ${_ioPending}")
        changeWorkerToIdle(workerId, workId)
        val event = WorkCompleted(workId, result)
        _workState = _workState.updated(event)

        // Ack back to worker
        sender() ! MasterWorkerMessageProtocol.Ack(workId)

        updateCacheState(result)

        //println(s"ioPending result.originLink: ${_ioPending.get(result.originLink)}")
        _ioPending.get(result.originLink) match {
           case Some(x) if x.isEmpty => 
             //val requester: ActorRef = _workRequester(result.originLink).get
             //println(s"Calling renderer")
             render(result, workId)
             //requester ! IOCompleted(Map.empty, result.originLink)
             context.become(receive) // wait for RenderCompleted
             //requester ! IOCompleted(_ioComplete(result.originLink).map{case x: SetId => (x, null)}.toMap, result.originLink)
           case Some(x) => // not empty; wait
             //println(s"pendingIO: $x")
           case _       => // not fetch
             val requester: ActorRef = _workState.isDoneRef(workId)
             //println(s"In MasterLike: requester: ${requester}; result: $result")
             requester ! result
        }

      }

    case MasterWorkerMessageProtocol.WorkFailed(workerId, workId) =>
      if (_workState.isInProgress(workId)) {
        log.info(s"Work $workId failed by worker $workerId")
        changeWorkerToIdle(workerId, workId)
        val event = WorkerFailed(workId)
        _workState = _workState.updated(event)
        notifyWorkers()
      }

    case work @ Work(workId, job: Fetch) =>
      // idempotent
      if (_workState.isAccepted(workId)) {
        // resend Ack if it is not delivered to the sender
        sender() ! MasterWorkerMessageProtocol.Ack(workId)
      } else {
        log.info(s"Accepted work: ${work.workId}")
        // check if the link is found in setCache
        if(!cacheHit(job.link, job.link)) { //TODO: XXX this may not work if the same link is sent twice
          _ioPending.put(job.link, MutableSet(job.link))
	  val subWork = Work(workId, job = FetchSet(job.link, job.link))
	  val event = WorkAccepted(subWork, Some(sender()))
          //println(s"SETCACHE KEYS FIRST: ${setCache.keySet}")
          _workRequester.put(job.link, Some(sender()))
	  // Ack back to original sender
	  //sender() ! MasterWorkerMessageProtocol.Ack(workId)
	  _workState = _workState.updated(event)
	  notifyWorkers()
        } else {
          //println(s"SETCACHE KEYS: ${setCache.keySet}")
          _workRequester.put(job.link, Some(sender())) //TODO: TMP FIX; this condition may be met if a post is followed by a fetch
        }
      }

    case work @ Work(workId, job: FetchSet) =>
      // idempotent
      if (_workState.isAccepted(workId)) {
        log.info(s"Work is already accepted: ${work.workId}")
        // resend Ack if it is not delivered to the sender
        sender() ! MasterWorkerMessageProtocol.Ack(workId)
      } else {
        log.info(s"Subset updated: ${workId}")
        if(!cacheHit(job.link, job.originLink)) {
	  _ioPending(job.originLink) += job.link
          val requester = _workRequester(job.originLink)
	  val event = WorkAccepted(work, requester)
	  // Ack back to original sender
	  sender() ! MasterWorkerMessageProtocol.Ack(workId)
	  _workState = _workState.updated(event)
	  notifyWorkers()
        }
      }

    case work @ Work(workId, job) => job match {
      //case PostSetWithName(_, _, _) | DeleteSetWithName(_, _) | DeleteSet(_, _) =>
      case PostSetWithName(_, _, _) | DeleteSetWithName(_, _) | DeleteSet(_, _) | SimpleGet(_) | SimplePost(_, _) | SimpleDelete(_) =>
	if (_workState.isAccepted(workId)) {
	  log.info(s"Work is already accepted: ${workId}")
	  // resend Ack if it is not delivered to the sender
	  //sender() ! MasterWorkerMessageProtocol.Ack(workId)
	} else {
	  val event = WorkAccepted(work, Some(sender()))
          //_ioPending.put(job.link, MutableSet(job.link)) // this is for cache update and refresh; ouch: we may not know the link (yet)
	  log.info(s"Work accepted: ${workId}")
	  //println(s"Work accepted: ${workId}, job: $job")
	  // Ack back to original sender
	  //sender() ! MasterWorkerMessageProtocol.Ack(work.workId)
	  _workState = _workState.updated(event)
	  notifyWorkers()
	}
      case _ => log.info(s"Unknown job type found") // ignore
    }

    case RenderCompleted(originLink: SetId, proofContext: ProofSubContext) =>
      sender() ! PoisonPill
      //val requester: ActorRef = _workState.isDoneRef(workId)

      //println(s"Links: ${_workRequester}")
      val requester: ActorRef = _workRequester.get(originLink).flatten match {
        case Some(actorRef) => actorRef
        case _              => {
          //println(s"All workRequester refs: ${_workRequester.keySet}")
          throw sys.error(s"origin link not found: $originLink")
        } // TODO
      }
      _workRequester.remove(originLink)
      //println(s"Render completed for link: $originLink; requester: $requester")
      _ioComplete -= proofContext.id.name
      requester ! RenderCompleted(originLink, proofContext)
  }

  def notifyWorkers(): Unit =
    if (_workState.hasWork) {
      // could pick a few random instead of all
      val work = _workState.nextWork
      _workers.foreach {
        case w @ (_, WorkerState(ref, Idle))    => 
          //println(s"Worker Idle: $w")
          ref ! MasterWorkerMessageProtocol.WorkIsReady
        case w @ _                              => 
          //println(s"Worker Busy: $w")
          // busy worker; ignore
      }
    }

  def changeWorkerToIdle(workerId: String, workId: String): Unit =
    _workers.get(workerId) match {
      case Some(ws @ WorkerState(_, Busy(`workId`, _))) =>
        _workers += (workerId -> ws.copy(status = Idle))
      case _ => // may happen if the worker dies; ignore anyways
    }

  def updateCacheState(result: IOCompleted): Unit = {
    // update setCache and pending status
    result.setMap.foreach {
      case (id: SetId, credential: SignedCredential) =>
        //println(s"\n\nIn MasterLike: (id: $id, credential: $credential)\n\n")
        //println(s"\n\nIn MasterLike: ($result)\n\n")

	setCache.put(id, credential)
	val exisitingSet = _ioComplete.get(result.originLink)
	_ioComplete.put(result.originLink,  exisitingSet.getOrElse(MutableSet.empty) + id)
        //println(s"_ioPending1: ${_ioPending.get(result.originLink)}")
	_ioPending.get(result.originLink) match {
           case Some(orgLinkSet) => 
             if(orgLinkSet.size == 1) { // to handle the case where link starts with full path, i.e., http://
               if(orgLinkSet.head == result.originLink) {
                 orgLinkSet -= result.originLink 
               } else {
                 orgLinkSet -= id
               }
             } else {
               orgLinkSet -= id
             }
             //println(s"_ioPending2: ${_ioPending.get(result.originLink)}, id: $id")
           case _ =>
        }
    }
  }

  def render(result: IOCompleted, workId: WorkId): Unit = {
    // render
    //if(_ioPending.get(result.originLink).orElse(Some("")).isEmpty) { // TODO: check this
      _ioPending -= result.originLink
      log.info(s"Creating renderer")
      //println(s"Creating renderer")
      //val newWorkId: WorkId = java.util.UUID.randomUUID().toString
      val renderWorker: ActorRef = context.actorOf(
	  RenderWorker.props(setCache)
	, s"${RenderWorker.name}-$workId"
	//, s"${RenderWorker.name}-$newWorkId"
      ) // TODO: use a router?

      //println(s"Calling renderer")
      renderWorker ! Render(result.originLink, _ioComplete(result.originLink).toSet)
    //} else { println(s"Not calling renderer") }
  }

  def cacheHit(id: SetId, originId: SetId): Boolean = setCache.get(id) match {
    case None         => false
    case Some(cred)   => 
      val result = IOCompleted(Map(id -> cred), originId)
      // enqueue links
      cred.links foreach { link =>
        self ! FetchSet(link, originId)
      }
      updateCacheState(result)
      render(result, originId)
      true
  }
}
