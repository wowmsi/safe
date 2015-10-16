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

package object safelog {
  type NumericConstant = Double
  def toNumericConstant(value: StrLit): NumericConstant = try { value.toString.toDouble } catch {
    case ex: NumberFormatException => throw NumericException(s"Cannot perform numeric operations on given input: $value")
  } 

  //type TypedConstant = AnyVal
  //type TypedConstant = Double
  class TypedConstant(val value: String) extends AnyVal {
    override def toString(): String = value
  }

  type SetId = StrLit
  type Index = StrLit
  type EnvValue = AnyRef

  /**
   * 0 -> Attr
   * 1 -> IndexAttr
   * 2 -> AttrLiteral
   * 3 -> IndexAttrLiteral
   * 4 -> AttrHex
   * 5 -> IndexAttrHex
   * 6 -> AttrBase64
   * 7 -> IndexAttrBase64
   */
  type Encoding = Encoding.Value
  object Encoding extends Enumeration {
    val Attr, IndexAttr, AttrLiteral, IndexAttrLiteral, AttrHex, IndexAttrHex, AttrBase64, IndexAttrBase64 = Value
  }

  class Index2(val value: String) extends AnyVal { //type Index = String // specifically, lower case string
    override def toString(): String = value
  }
  object Index2 {
    def apply(idx: Int): Index2 = new Index2(idx.toString)
    def apply(idx: String): Index2 = new Index2(idx)
    def apply(idx: Term): Index2 = new Index2(idx.toString)
  }

  val termType: StrLit = StrLit("StrLit")
  val termIndex: StrLit = StrLit("nil")

  // These values should come from config
  val StringEncoding: String = "UTF-8"
  val MaxSymbolSize: Int     = Math.pow(2, 16).toInt

  case class SType(value: Byte) extends AnyVal
  val SNil: Byte        = 0.toByte
  val SConstant: Byte   = 1.toByte
  val SVariable: Byte   = 2.toByte
  val SStructure: Byte  = 3.toByte
  val SNegatedTerm: Byte= 4.toByte
  val SAssertion: Byte  = 5.toByte
  val SResult: Byte     = 6.toByte
  val SRetraction: Byte = 7.toByte
  val SQuery: Byte      = 8.toByte
  val SQueryAll: Byte   = 9.toByte
}

