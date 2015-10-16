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

/** A statement is a sequence of terms. */
abstract class Statement {
  val terms: Seq[Term]

  def bind(f: StrLit => Term): Statement = this

  // useful at slang level
  def bind(args: Map[StrLit, Term]): Statement = {
    var _statement = this
    args.foreach {
      case (varName, varValue) =>
        _statement = _statement.bind{v => if(v.name == varName.name) varValue else Variable(v)}
    }
    _statement
  }

  def toStringWithSays(): String = toString()
  def toStringCompact(speaker: String): String  = toString()

  lazy val arity: Int = terms.length // TODO: TMP due to serialization exception

  /** Index for efficient search
   *
   * This can be pushed to Statement level, but precomputing here is useful
   * for indexing query terms; otherwise the index should be built on-demand 
   * during inference
   */
  def primaryIndex(): StrLit = terms.head.primaryIndex() // index on principal functor and arity
  //def primaryIndex(): StrLit = StrLit(terms.head.id.name + terms.head.arity) // index on principal functor and arity
  //def primaryIndex(): StrLit = StrLit(terms.head.id.name + terms.head.arity) // index on principal functor and arity
  //def primaryIndex(): StrLit = if(terms.head.primaryIndex == StrLit("_Nil")) terms.head.secondaryIndex else terms.head.primaryIndex
  //def secondaryIndex(): StrLit = ""     // index on first argument
  //def ternaryIndex(): StrLit   = ""     // index on second argument
  //val (secondaryIndex: StrLit, ternaryIndex: StrLit) = (terms.head.secondaryIndex, terms.head.ternaryIndex)  
  //def secondaryIndex(): StrLit = terms.head.secondaryIndex
  def secondaryIndex(): StrLit = terms.head match {
    case s @ Structure(pred, Constant(c, _, _, _) +: other, _, _, _) => StrLit(s.primaryIndex() + c.toString)
    case _ => terms.head.primaryIndex()
  }
}

case class Assertion(terms: Seq[Term]) extends Statement {
  override def toString() = Statement.toString(terms, ".")
  override def toStringWithSays() = Statement.toStringWithSays(terms, ".")
  override def toStringCompact(speaker: String)  = Statement.toStringCompact(speaker, terms, ".")
  override def bind(f: StrLit => Term): Statement = this.copy(terms.map(_.bind(f)))
}
case class Result(terms: Seq[Term]) extends Statement {
  override def toString() = Statement.toString(terms, "")
  override def toStringWithSays() = Statement.toStringWithSays(terms, "")
  override def toStringCompact(speaker: String)  = Statement.toStringCompact(speaker, terms, "")
  override def bind(f: StrLit => Term): Statement = this.copy(terms.map(_.bind(f)))
}
case class Retraction(terms: Seq[Term]) extends Statement {
  override def toString() = Statement.toString(terms, "~")
  override def toStringWithSays() = Statement.toStringWithSays(terms, "~")
  override def toStringCompact(speaker: String)  = Statement.toStringCompact(speaker, terms, "~")
  override def bind(f: StrLit => Term): Statement = this.copy(terms.map(_.bind(f)))
}
case class Query(terms: Seq[Term]) extends Statement {
  override def toString() = Statement.toString(terms, "?")
  override def toStringWithSays() = Statement.toStringWithSays(terms, "?")
  override def toStringCompact(speaker: String)  = Statement.toStringCompact(speaker, terms, "?")
  override def bind(f: StrLit => Term): Statement = this.copy(terms.map(_.bind(f)))
}
case class QueryAll(terms: Seq[Term]) extends Statement {
  override def toString() = Statement.toString(terms, "??")
  override def toStringWithSays() = Statement.toStringWithSays(terms, "??")
  override def toStringCompact(speaker: String)  = Statement.toStringCompact(speaker, terms, "??")
  override def bind(f: StrLit => Term): Statement = this.copy(terms.map(_.bind(f)))
}

object Statement {
  import java.nio.ByteBuffer
  import Term.{read, write}
  import scala.collection.mutable.ListBuffer

/*
  import java.nio.{ByteBuffer, ByteOrder}
  val byteBuffer: ByteBuffer = ByteBuffer.allocateDirect(1024).order(ByteOrder.nativeOrder())

  //val a = Assertion(Structure('test, Constant(StrLit("1")) +: Variable(StrLit("Ya")) +: Constant(StrLit("a")) +: Constant(StrLit("3")) +: Nil) +: Constant(StrLit("a")) +: Variable(StrLit("ZZZZ")) +: Nil)
  val a = Assertion(Structure('test, Constant(StrLit("1"))(StrLit("String")) +: Variable(StrLit("Ya")) +: Constant(StrLit("a")) +: Constant(StrLit("3")) +: Nil) +: Constant(StrLit("a")) +: Variable(StrLit("ZZZZ"))(StrLit("Ipv4")) +: Nil)
  println(s"Statement: $a")
  writeStatement(a, byteBuffer)
  val lastPosition = byteBuffer.position
  println(lastPosition)
  byteBuffer.rewind()
  val array: Array[Byte] = new Array[Byte](lastPosition)
  println(array.length)
  byteBuffer.get(array)
  for(i <- 0 until lastPosition) {
    print(array(i)) 
    print(" ")
  }
  byteBuffer.rewind()
  val b = readStatement(byteBuffer)
  println(s"Unmarshalled statement: $b")
 */

  //=============Function for pretty printing==================//
  def toString(terms: Seq[Term], endsWith: String): String = Term.normalizeTerms(terms) match {
    case head +: Nil => head + endsWith
    case head +: tail => head + " :- " + tail.mkString(", ") + "."
    case _ => ""
  }

  def toStringWithSays(terms: Seq[Term], endsWith: String): String = Term.normalizeTerms(terms) match {
    case head +: Nil  => head.toStringWithSays() + endsWith
    case head +: tail => head.toStringWithSays() + " :- " + tail.map(x => x.toStringWithSays()).mkString(", ") + "."
    case _ => ""
  }

  def toStringCompact(speaker: String, terms: Seq[Term], endsWith: String): String = Term.normalizeTerms(terms) match {
    case head +: Nil  => head.toStringCompact(speaker) + endsWith
    case head +: tail => 
      head.toStringCompact(speaker) + " :- " + tail.map(x => x.toStringCompact(speaker)).mkString(", ") + "."
    case _ => ""
  }

  private[safelog] def writeStatement(statement: Statement, byteBuffer: ByteBuffer): Unit = statement match {
    case Assertion(terms: Seq[Term]) =>
      byteBuffer.put(SAssertion)
      terms.foreach {term => write(term, byteBuffer)}
      byteBuffer.put(SNil)
    case Result(terms: Seq[Term]) =>
      byteBuffer.put(SResult)
      terms.foreach {term => write(term, byteBuffer)}
      byteBuffer.put(SNil)
    case Retraction(terms: Seq[Term]) =>
      byteBuffer.put(SRetraction)
      terms.foreach {term => write(term, byteBuffer)}
      byteBuffer.put(SNil)
    case Query(terms: Seq[Term]) =>
      byteBuffer.put(SQuery)
      terms.foreach {term => write(term, byteBuffer)}
      byteBuffer.put(SNil)
    case QueryAll(terms: Seq[Term]) =>
      byteBuffer.put(SQueryAll)
      terms.foreach {term => write(term, byteBuffer)}
      byteBuffer.put(SNil)
    case _ => throw new UnSafeException("Unknown typed passed")
  }

  @annotation.tailrec
  private def recurse(byteBuffer: ByteBuffer, terms: ListBuffer[Term] = ListBuffer()): ListBuffer[Term] = {
    read(byteBuffer) match {
      case Constant(StrLit("nil"), _, _, _) => terms
      case term => 
        terms += term
        recurse(byteBuffer, terms)
    }
  }
  
  private[safelog] def readStatement(byteBuffer: ByteBuffer): Statement = byteBuffer.get() match {
    case SNil         => Assertion(Seq(Constant(StrLit("Nil"))))
    case SAssertion   => Assertion(recurse(byteBuffer))
    case SResult      => Result(recurse(byteBuffer))
    case SRetraction  => Retraction(recurse(byteBuffer))
    case SQuery       => Query(recurse(byteBuffer))
    case SQueryAll    => QueryAll(recurse(byteBuffer))
    case x            => throw new UnSafeException(s"Unknown typed passed: $x")
  }
}
