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

import safe.safelog.StrLit

sealed abstract class Identity {
  val value: StrLit
  override def toString() = value.name
}

// Note: ignore quotes for all ids
//case class Id(value: StrLit) extends Identity
class Id(val value: StrLit) extends Identity
case class Guid(value: StrLit) extends Identity
case class Scid(speakerId: Id, name: Option[Identity]) extends Identity {
  val value: StrLit = StrLit(toString())
  override def toString() =  name match {
    case None       => s"${speakerId.toString}"
    case Some(n)    => s"${speakerId.toString}:${n.toString}"
  }
}

object Id {
  def apply(value: String): Id = value match {
    case Identity.encodingPattern(_, v) => new Id(StrLit(v))
    case _                              => new Id(StrLit(value))
  }
  def apply(value: Array[Byte]): Id = new Id(StrLit(Identity.encode(value)))
  def apply(value: Scid): Id = value.name match {
    case None => value.speakerId
    case Some(name) =>
      val fullName = value.name match {
        case Some(setName) => s"${value.speakerId.toString}:${setName}"
        case None          => s"${value.speakerId.toString}"
      }
      //println(s"SCID: $fullName")
      val setId = Identity.hash(fullName.getBytes(StringEncoding), "SHA-256")
      Id(Identity.encode(setId, "base64URLSafe"))
  }
}
object Guid {
  def apply(): Guid = new Guid(StrLit(Identity.base64EncodeURLSafe(Identity.hash(Entity.secureRandom.nextInt.toString.getBytes(StringEncoding), "MD5"))))
  //def apply(value: String): Guid = new Guid(StrLit(value))
  def apply(value: Array[Byte]): Guid = new Guid(StrLit(Identity.encode(value)))
}
object Scid {
  def apply(value: String): Scid = {
    val sList: Array[String] = value.split(':')
    if(sList.length == 2) new Scid(Id(sList(0)), Some(Id(sList(1))))
    else if(sList.length == 1) new Scid(Id(sList(0)), None)
    else throw new Exception(s"Unrecognized scid: $value")
  }
  def apply(speakerId: Id, name: String): Scid = {
    if(name.isEmpty) new Scid(speakerId, None)
    else new Scid(speakerId, Some(Id(Identity.base64EncodeURLSafe(Identity.hash(name.getBytes("UTF-8"), "MD5")))))
  }
  def apply(speakerId: Id, name: Guid): Scid = {
    //if(name.isEmpty) new Scid(speakerId, None)
    //else 
    new Scid(speakerId, Some(Id(name.toString)))
  }
}

object Identity {

  import org.apache.commons.codec.binary.Base64

  val encodingPattern = """([a-zA-Z])'(.+)'""".r

  /** create a SHA hash from a Byte array
   * possible hash algorithms: MD5, SHA-1, SHA-256, SHA-384, SHA-512 
   */
  def hash(in: Array[Byte], algorithm: String = "SHA-256"): Array[Byte] = algorithm match {
    case "MD5" | "SHA-1" | "SHA-256" | "SHA-384" | "SHA-512" => java.security.MessageDigest.getInstance(algorithm).digest(in)
    case _ => sys.error(s"Unknown hash algorithm provided: $algorithm")
  }

  def encode(content: Array[Byte], encoding: String = "base64URLSafe") = encoding match {
    case "base64"        => base64Encode(content)
    case "base64URLSafe" => base64EncodeURLSafe(content)
    case "hex"           => hexEncode(content)
    case _               => sys.error(s"Unknown encoding is provided: $encoding")
  }

  /** encode a Byte array in Base 64 
   *  {A-Za-z0-9+/}
   */
  def base64Encode(in: Array[Byte]): String = new String(Base64.encodeBase64(in))

  /** encode a Byte array in Base 64 in a way that's safe for use in URLs
   *  {A-Za-z0-9-_}
   */
  def base64EncodeURLSafe(in: Array[Byte]): String = Base64.encodeBase64URLSafeString(in)

  /** decode a String in Base 64 
   *  Note: '+' and '-' both decode to 62. '/' and '_' both decode to 63. This means decoder seamlessly handles both
   *  URL_SAFE and STANDARD base64. (The encoder, on the other hand, needs to know ahead of time what to emit.)
   */
  def base64Decode(in: String): Array[Byte] = Base64.decodeBase64(in)


  /** encode a byte array as hexadecimal characters */
  def hexEncode(in: Array[Byte]): String = {
    val sb = new StringBuilder
    val len = in.length
    def addDigit(in: Array[Byte], pos: Int, len: Int, sb: StringBuilder) {
      if (pos < len) {
        val b: Int = in(pos)
        val msb = (b & 0xf0) >> 4
        val lsb = (b & 0x0f)
        sb.append((if (msb < 10) ('0' + msb).asInstanceOf[Char] else ('a' + (msb - 10)).asInstanceOf[Char]))
        sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[Char] else ('a' + (lsb - 10)).asInstanceOf[Char]))

        addDigit(in, pos + 1, len, sb)
      }
    }
    addDigit(in, 0, len, sb)
    sb.toString
  }

  def hexDecode(str: String): Array[Byte] = {
    def byteOf(in: Char): Int = in match {
      case c if (c >= '0'  && c <= '9') => (c - '0')
      case c if (c >= 'a' && c <= 'f') => (c - 'a') + 10
      case c if (c >= 'A' && c <= 'F') => (c - 'A') + 10
      case _ => sys.error(s"Invalid hex character: $in")
    }
    val ret = new Array[Byte](str.length / 2)
    for(i <- 0 until str.length by 2) {
      val c1: Char = str.charAt(i)
      val c2: Char = str.charAt(i + 1)
      ret(i >> 1) = ((byteOf(c1) << 4) | byteOf(c2)).toByte // ret(i / 2) = (byteOf(c1) * 16 + byteOf(c2)).toByte
    }
    ret
  }

  /** Compare two strings in a way that does not vary if the strings
   * are determined to be not equal early (test every byte... avoids
   * timing attacks */
  def secureEquals(s1: String, s2: String): Boolean = (s1, s2) match {
    case (null, null) => true
    case (null, _) => false
    case (_, null) => false
    case (a, b) => secureEquals(a.getBytes("UTF-8"), b.getBytes("UTF-8"))
  }

  /** Compare two byte arrays in a way that does not vary if the arrays are 
   * determined to be not equal early (test every byte; avoids timing attacks) 
   */
  def secureEquals(s1: Array[Byte], s2: Array[Byte]): Boolean = (s1, s2) match {
    case (null, null) => true
    case (null, _) => false
    case (_, null) => false
    case (a, b) => {
      val la = a.length
      val lb = b.length
      var ret = true
      var pos = 0
      while (pos < la && pos < lb) {
        ret &= (a(pos) == b(pos))
        pos += 1
      }
      ret && la == lb
    }
  }
}
