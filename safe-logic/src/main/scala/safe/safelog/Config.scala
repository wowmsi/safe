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



package safe.safelog

import com.typesafe.config.{ConfigException, ConfigFactory}
import scala.util.Try

class Config(config: com.typesafe.config.Config) {
  config.checkValid(ConfigFactory.defaultReference(), "safelog")
  def this() {
    this(ConfigFactory.load())
  }
  val self: String                   = Try(config.getString("safelog.self")).getOrElse("Self")
  val saysOperator: Boolean          = Try(config.getBoolean("safelog.saysOperator")).getOrElse(false)
  val intraQueryParallelism: Boolean = Try(config.getBoolean("safelog.intraQueryParallelism")).getOrElse(false)
  val initialContextSize: Int        = Try(config.getInt("safelog.initialContextSize")).getOrElse(16)
  val maxDepth: Int                  = Try(config.getInt("safelog.maxDepth")).getOrElse(11111)
  val reserved: Map[StrLit, Int]     = Map(   // Map of reserved symbols with their arity
      StrLit("version")            ->  1
    , StrLit("speaker")            ->  2
    , StrLit("subject")            ->  2
    , StrLit("principal")          ->  1
    , StrLit("signatureAlgorithm") ->  1
    //, StrLit("speaksFor")          ->  2
    //, StrLit("speaksForOn")        ->  3
    , StrLit("name")               ->  1
    , StrLit("link")               ->  1
    , StrLit("validity")           ->  3
    , StrLit("tag")                ->  1
    , StrLit("import")             ->  1
    , StrLit("importAll")          ->  1
    , StrLit("recipient")          ->  4
    , StrLit("encoding")           ->  1
  )
  val metadata: Set[StrLit] = Set(
      StrLit("version")
    , StrLit("speaker")
    , StrLit("subject")
    , StrLit("signatureAlgorithm")
    , StrLit("name")
    , StrLit("validity")
    , StrLit("import")
    , StrLit("importAll")
    , StrLit("recipient")
    , StrLit("encoding")
  )
}

object Config {
  val config = new Config(ConfigFactory.load("application")) // default config context
}
