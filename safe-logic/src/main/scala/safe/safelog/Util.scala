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

object Util {

  def readCmdLine(
      args: Seq[String]
    , requiredArgs: Seq[Symbol]
    , optionalArgs: Map[String, Symbol] = Map.empty
    , optionValues: Map[Symbol, String] = Map[Symbol, String]().withDefaultValue("")
  ) : Map[Symbol, String] = {
   
    val res = args match {
      case Nil                                                => optionValues
      case key +: value +: tail if optionalArgs.contains(key) => readCmdLine(tail, requiredArgs, optionalArgs, optionValues ++ Map(optionalArgs(key) -> value))
      case value +: tail if !requiredArgs.isEmpty             => readCmdLine(tail, requiredArgs.tail, optionalArgs, optionValues ++ Map(requiredArgs.head -> value))
      case Seq("--help") | Seq("-h")                          => optionValues ++ Map('help -> "true")
      case other +: tail                                      => sys.error(s"Unknown option [$other] specified")
    }
    res
  }
}
