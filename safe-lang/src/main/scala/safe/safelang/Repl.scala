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

import akka.actor.{ActorRef, ActorSystem}
import scala.collection.mutable.{Set => MutableSet}

import safe.safelog.{Index, MutableCache, Statement}

class Repl(
    val safeSetsClient: ActorRef
  , self: String
  , saysOperator: Boolean
  , _statementCache: MutableCache[Index, MutableSet[Statement]]
) extends safe.safelog.Repl(self, saysOperator, _statementCache) with SafelangService {

  override val stdPromptMsg = "slang> "
  override val greet = s"Safe Language v0.1: $date (To quit, press Ctrl+D or q.)"
}
  
object Repl {

  import akka.util.Timeout
  import com.typesafe.config.ConfigFactory
  import java.util.concurrent.TimeUnit
  import scala.concurrent.duration._

  def main(args: Array[String]): Unit = {

    val usage = """
      Usage: Repl [--port|-p number] [--file|-f fileName] [--numWorkers|-n number] [--args|-a fileArguments] [--help|-h]
    """

    val optionalArgs = Map(
        "--port"       -> 'port
      , "-p"           -> 'port
      , "--file"       -> 'file
      , "-f"           -> 'file
      , "--args"       -> 'fileArgs
      , "-a"           -> 'fileArgs
      , "--numWorkers" -> 'numWorkers
      , "-n"           -> 'numWorkers
      , "--help"       -> 'help
      , "-h"           -> 'help
    )

    val argMap = safe.safelog.Util.readCmdLine(
        args.toSeq
      , requiredArgs = Nil
      , optionalArgs
      , Map('help -> "false", 'port -> "4001", 'numWorkers -> "2")
    )

    if(argMap('help) == "true") {
      println(usage)
    } else {

      val port: Int = argMap('port).toInt
      val numWorkers: Int = argMap('numWorkers).toInt

      val conf = ConfigFactory.parseString(s"akka.remote.netty.tcp.port=${port}")
	.withFallback(ConfigFactory.load())

      //val shutdownGracefully: Boolean = conf.getBoolean("safe.safelang.system.shutdownGracefully")
      val shutdownGracefully: Boolean = true
      val setCacheInitialCapacity = conf.getInt("safe.safesets.cache.initialCapacity")
      val setCacheLoadFactor      = conf.getString("safe.safesets.cache.loadFactor").toFloat
      val setCacheConcurrency     = conf.getInt("safe.safesets.cache.concurrency")
      val storeURI                = conf.getString("safe.safesets.storeURI")
      //val requestTimeout          = Timeout(conf.getDuration("safe.service.requestTimeout", TimeUnit.SECONDS), TimeUnit.SECONDS)
      val requestTimeout: FiniteDuration          = FiniteDuration(conf.getDuration("safe.safesets.requestTimeout", TimeUnit.SECONDS), TimeUnit.SECONDS)

      val system = ActorSystem("SafeSystem", conf)

      val master = system.actorOf(
	  safe.safesets.client.Master.props(requestTimeout, setCacheInitialCapacity, setCacheLoadFactor, setCacheConcurrency)
	, safe.safesets.client.Master.name
      )

      for(i <- 1 to numWorkers) {
	system.actorOf(safe.safesets.client.Worker.props(master, 10.seconds, 30.seconds, storeURI), safe.safesets.client.Worker.name + i)
      } 

      val safeSetsClient: ActorRef = master
      val inference = new Repl(safeSetsClient, Config.config.self, Config.config.saysOperator, new MutableCache[Index, MutableSet[Statement]]())

      try {
        (argMap.get('file), argMap.get('fileArgs)) match {
          case (None, None)     => inference.repl() // The init call 
          case (Some(sf), None) => inference.printOutput(inference.evalFileWithTime(sf, 'ms)) // Evaluate expressions from a file.
          case (Some(sf), Some(args)) => 
            val argSeq = args.split(",").toSeq
            var _fileContents = scala.io.Source.fromFile(sf).mkString
            argSeq.zipWithIndex.foreach{ case (arg, idx) =>
              _fileContents = _fileContents.replaceAll(s"\\$$${idx + 1}", s"'$arg'")
            }
            // write to a temp file // TODO: fix this
            import java.nio.file.{Paths, Files}
            import java.nio.charset.StandardCharsets
            //val tmpFileName = s"/tmp/$sf" //TODO: extract fileName from path
            val tmpFileName = s"/tmp/${java.util.UUID.randomUUID()}"
            Files.write(Paths.get(tmpFileName), _fileContents.getBytes(StandardCharsets.UTF_8))
 
            inference.printOutput(inference.evalFileWithTime(tmpFileName, 'ms)) // Evaluate expressions from a file.
          case (None, Some(args)) => //ignore args or throw an exception?
        }
      } catch {
        case err: Throwable => 
          //println(err)
          err.printStackTrace()
          gracefulShutdown(system)
      }
      if(shutdownGracefully) gracefulShutdown(system)
    }
  }

  def gracefulShutdown(system: ActorSystem, prompt: Boolean = false): Unit = {
    if(!prompt) {
      system.shutdown()
      sys.addShutdownHook(system.shutdown())
    } else {
      // Allow an operator to shutdown the service gracefully
      val terminate: Boolean = {
	def loop(): Boolean = scala.io.StdIn.readLine() match {
	  case s if s.toLowerCase.matches("""^[y\n](es)?""") => true
	  case _ => println(s"terminate? [y(es)?]"); loop()
	}
	loop()
      }
      if(terminate) system.shutdown()

      /**
       * Ensure that the constructed ActorSystem is shut down when the JVM shuts down
       */
      sys.addShutdownHook(system.shutdown())
    }
  }
}
