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

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
import safe.safelog.{UnSafeException, Validity}
import CredentialState._

case class Status(state: String, signedTime: DateTime, message: String = "") {
  override def toString(): String = s"""status('$state', '${Validity.format.print(signedTime)}', '$message')"""
}
object Status {
  def apply(state: String): Status = new Status(state, new DateTime(), "")
  def apply(state: String, signedTime: String, message: String): Status = {
    val format: DateTimeFormatter = ISODateTimeFormat.dateTime()
    new Status(state, format.parseDateTime(signedTime), message)
  }
  // TODO: remove?
  private def getState(state: String): CredentialState = state match {
    case "active"    => CredentialState.Active
    case "revoked"   => CredentialState.Revoked
    case "suspended" => CredentialState.Suspended
    case _           => throw UnSafeException(s"state $state not recognized")
  }
}
