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

object Master {
  def props(workTimeout: FiniteDuration): Props =
    Props(classOf[Master], workTimeout, 65536, 0.99f, 16) // 2^16 * 1 kB ~ 64MB

  def props(
      workTimeout: FiniteDuration
    , setCacheInitialCapacity: Int
    , setCacheLoadFactor: Float
    , setCacheConcurrency: Int
  ): Props = 
    Props(classOf[Master], workTimeout, setCacheInitialCapacity, setCacheLoadFactor, setCacheConcurrency)

  def name: String = "SafeSetsMaster"
}

class Master(
    val workTimeout: FiniteDuration
  , val setCacheInitialCapacity: Int
  , val setCacheLoadFactor: Float
  , val setCacheConcurrency: Int
) extends MasterLike 
  with MasterLikeConfig {

}
