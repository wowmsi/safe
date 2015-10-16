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

import com.typesafe.config.{ConfigException, ConfigFactory}
import scala.util.Try

import scala.concurrent.duration._

class Config(config: com.typesafe.config.Config) {
  config.checkValid(ConfigFactory.defaultReference(), "safelang")
  def this() {
    this(ConfigFactory.load())
  }
  val self: String                   = Try(config.getString("safelang.self")).getOrElse("Self")
  val saysOperator: Boolean          = Try(config.getBoolean("safelang.saysOperator")).getOrElse(false)
  val maxDepth: Int                  = Try(config.getInt("safelang.maxDepth")).getOrElse(111)
  val akkaTimeout: FiniteDuration    = Try(
    FiniteDuration(config.getDuration("safelang.akkaTimeout", MILLISECONDS), MILLISECONDS)
  ).getOrElse(FiniteDuration(25, SECONDS))
}

object Config {
  val config = new Config(ConfigFactory.load("application")) // default config context
}
