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

import akka.actor.{Actor, ActorInitializationException, ActorLogging, ActorRef, Cancellable} 
import akka.actor.{DeathPactException, OneForOneStrategy, Props, ReceiveTimeout, Terminated}
import akka.actor.SupervisorStrategy.{Restart, Stop}

import java.util.UUID
import scala.concurrent.duration._

import MasterWorkerMessageProtocol._

trait WorkerLikeConfig {
  def workExecutor: ActorRef
  def masterRef: ActorRef
  def recontactInterval: FiniteDuration
  def receiveTimeout: FiniteDuration
}

trait WorkerLike extends Actor with ActorLogging {
  config: WorkerLikeConfig =>

  import context.dispatcher

  var currentWorkId: Option[String] = None

  val workerId = UUID.randomUUID().toString

  // Schedules a message to be sent repeatedly with an initial delay and
  // frequency. E.g. if you would like a message to be sent immediately and
  // thereafter every 500ms you would set delay=Duration.Zero and
  // interval=Duration(500, TimeUnit.MILLISECONDS)

  def registerWorker(): Cancellable = context.system.scheduler.schedule(
      initialDelay = 0.seconds
    , interval     = recontactInterval
    , receiver     = masterRef
    , message      = RegisterWorker(workerId)
  )
  
  def workId: String = currentWorkId.getOrElse(throw new IllegalStateException(s"worker undefined -- not working"))

  override def supervisorStrategy = OneForOneStrategy() {
    case _: ActorInitializationException => Stop
    case _: DeathPactException           => Stop
    case _: Exception                    =>
      currentWorkId map { workId => sendToMaster(WorkFailed(workerId, workId)) }
      context.become(idle)
      Restart
  }

  override def postStop(): Unit = registerWorker().cancel()

  def receive = idle

  def idle: Receive = {
    case WorkIsReady =>
      log.info("Worker requests work")
      sendToMaster(WorkerRequestsWork(workerId))
    case work: Work[_] =>
      log.info(s"Got work: ${work.job}")
      currentWorkId = Some(work.workId)
      workExecutor ! work.job
      context.become(working)
    case work => //log.info(s"yet to implement: $work") // TODO: TMP
      currentWorkId = Some("work")
      workExecutor ! work
      context.become(working)
  }

  def working: Receive = {
    case WorkerExecutorMessageProtocol.WorkCompleted(result) =>
      log.info(s"Work is complete. Result: $result")
      //println(s"Work is complete. Result: $result")
      sendToMaster(WorkIsDone(workerId, workId, result))
      context.setReceiveTimeout(receiveTimeout)
      context.become(waitForWorkIsDoneAck(result))
    case WorkerExecutorMessageProtocol.MoreWork(fetchSet) =>
      log.info(s"More work: $fetchSet")
      //println(s"More work: $fetchSet")
      sendToMaster(Work(java.util.UUID.randomUUID().toString, fetchSet))
    case _: Work[_] =>
      log.info("[warn] Master tells me to work, while I'm working.")
  }

  def waitForWorkIsDoneAck(result: Any): Receive = {
    case Ack(id) if id == workId =>
      sendToMaster(WorkerRequestsWork(workerId))
      context.setReceiveTimeout(Duration.Undefined)
      context.become(idle)
    case ReceiveTimeout =>
      log.info(s"No ack from master, retrying")
      sendToMaster(WorkIsDone(workerId, workId, result))
  }

  override def unhandled(message: Any): Unit = message match {
    case Terminated(workExecutor)   => context.stop(self)
    case WorkIsReady                =>
    case _                          => super.unhandled(message)
  }
  
  def sendToMaster(msg: Any): Unit = {
    masterRef ! msg
  }
}
