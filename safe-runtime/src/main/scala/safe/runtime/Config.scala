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



package safe.runtime
    
import com.typesafe.config.ConfigFactory

class Config(config: com.typesafe.config.Config) {
  config.checkValid(ConfigFactory.defaultReference(), "saferuntime")
  def this() {
    this(ConfigFactory.load())
  }             
  val jvmCompilerPath: String        = config.getString("saferuntime.jvmCompilerPath")
}             
                
object Config {
  val config = new Config(ConfigFactory.load("application")) // default config context
}
