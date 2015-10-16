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
package safelog

case class StrLit(val name: String) extends AnyVal

/*
case class StrLit (val name: String) extends Serializable {
  // Converts this symbol to a string.
  //override def toString(): String = "'" + name

  @throws(classOf[java.io.ObjectStreamException])
  //private def readResolve(): Any = StrLit.apply(name)
  override def hashCode = name.hashCode()
  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
}

object StrLit {
  //def apply(name: String): StrLit = new StrLit(name)
  //protected def valueFromKey(name: String): StrLit = new StrLit(name)
  //protected def keyFromValue(sym: StrLit): Option[String] = Some(sym.name)
}
*/
