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

import scala.util.control.NoStackTrace

sealed abstract class SafelogException(message: String) extends RuntimeException(message)

case class UnSafeException(message: String) extends SafelogException(message) with NoStackTrace
case class ParserException(message: String) extends SafelogException(message) with NoStackTrace
case class InfiniteLoopException(message: String) extends SafelogException(message) with NoStackTrace
case class NumericException(message: String) extends SafelogException(message) with NoStackTrace
case class NotImplementedException(message: String) extends SafelogException(message) with NoStackTrace
case class UnSupportedOperatorException(message: String) extends SafelogException(message) with NoStackTrace

object SafelogException {
  /* A helper function to pretty print the label info */
  def printLabel(resultType: Symbol) = resultType match {
    case 'success => println("[" + Console.GREEN + "satisfied" +  Console.RESET + "] ")
    case 'failure => println("[" + Console.RED + "unsatisfied" +  Console.RESET + "] ")
    case 'info => print("[" + Console.YELLOW + "info" + Console.RESET + "] ")
    case 'more => print("[" + Console.BLUE + "more?" + Console.RESET + "] ")
    case 'warn => print("[" + Console.RED+ "warn" + Console.RESET + "] ")
  }
}
