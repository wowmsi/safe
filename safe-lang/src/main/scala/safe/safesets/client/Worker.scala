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

import akka.actor.{Actor, ActorRef, Props}
import scala.concurrent.duration.FiniteDuration

object Worker {
  def props(
      masterRef: ActorRef
    , recontactInterval: FiniteDuration
    , receiveTimeout: FiniteDuration
    , storeURI: String
  ): Props = Props(classOf[Worker], masterRef, recontactInterval, receiveTimeout, storeURI)

  def name: String = "SafeSetsWorker"
}

class Worker(
    val masterRef: ActorRef
  , val recontactInterval: FiniteDuration
  , val receiveTimeout: FiniteDuration
  , val storeURI: String
) extends WorkerLike
  with WorkerLikeConfig {

  val workExecutor: ActorRef = context.actorOf(WorkExecutor.props(storeURI), WorkExecutor.name)

  registerWorker()            // registerWorker with a master
  context.watch(workExecutor) // watch for workExecutor termination
}
