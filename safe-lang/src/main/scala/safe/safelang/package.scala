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



package safe

import akka.util.Timeout
  
import safe.runtime.{Lang, JVMContext, JVMInterpreter}

package object safelang {
  // These values should come from config
  val StringEncoding: String = "UTF-8"

  if(java.security.Security.getProvider(org.bouncycastle.jce.provider.BouncyCastleProvider.PROVIDER_NAME) == null) {
    java.security.Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider())
  }

  val safelogContext: SafelogContext = new SafelogContext() // for inference but not parsing
  private[safelang] val jvmContext: Map[Lang, JVMInterpreter] = JVMContext.interpreters

  private[safelang] implicit val timeout = Timeout(Config.config.akkaTimeout)
  private[safelang] implicit val ec      = scala.concurrent.ExecutionContext.Implicits.global // customize the execution context if required
}
