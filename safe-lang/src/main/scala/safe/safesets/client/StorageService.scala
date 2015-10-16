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

import akka.actor.Actor

import spray.http._
import spray.client.pipelining._

import scala.concurrent.Future

import safe.safesets.SafeSetsMessageProtocol._

/*
trait StorageConfig {
  def storeURI: String
}
*/

trait StorageAPI {
  def fetchSet(id: SetId): Future[HttpResponse]
  def postSet(id: SetId, content: String): Future[HttpResponse]
  def deleteSet(id: SetId): Future[HttpResponse]
}

trait StorageService extends StorageAPI {
  this: Actor =>

  def storeURI: String

  import context.dispatcher // execution context for futures
  
  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  def getUrl(id: SetId): String = {
    val uri = Uri(id)
    val url = if(uri.scheme != "https" && uri.scheme != "http") {
      s"${storeURI}/$id"
    } else {
      uri.toString()
    }
    url
  }

  def fetchSet(id: SetId): Future[HttpResponse] = {
    pipeline(Get(getUrl(id)))
  }

  def postSet(id: SetId, content: String): Future[HttpResponse] = {
    pipeline(Post(getUrl(id), content))
  }
  def deleteSet(id: SetId): Future[HttpResponse] = {
    pipeline(Delete(getUrl(id)))
  }
}
