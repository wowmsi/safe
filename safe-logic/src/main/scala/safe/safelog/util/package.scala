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

package object util {

  def time[A](block: => A, format: Symbol = 's): Tuple2[A, Double] = {
    val exponent: Int = format match {
      case 'ms => 6   // millisec
      case 'm => 9*60 // min
      case _ => 9     // sec
    }
     
    val now = System.nanoTime
    val result = block // call-by-name
    val timeUnit = (System.nanoTime - now) / Math.pow(10, exponent)
    //println("%f seconds".format(sec))
    (result, timeUnit)
  }

  def toList(slangList: String): List[String] = {
    val listExpr = """^\[(.*?)\]$""".r
    val sList = slangList match {
      case listExpr(elems) => elems.split("""\s*,\s*""").toList // TODO: FIXME -- do we need parser combinator here to handle more efficiently?
      case _ => List(slangList) //sys.error("passed input is not a list")
    }
    sList
  }
  
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A): B = cache.getOrElseUpdate(x, f(x))
  }

  def sha1(msg: String, argc: Int=0): String = {
    import java.security.MessageDigest
    import java.math.BigInteger
    val digest = MessageDigest.getInstance("SHA-1").digest(msg.getBytes)
    //"sha" + new BigInteger(1, digest).toString(16) + argc
    new BigInteger(1, digest).toString(16) + argc
  }

  def typeOf[T: Manifest](t: T): Manifest[T] = manifest[T]

  //@annotation.tailrec
  // finds the combinations of elems
  // example: 
  // {{{ 
  //     val a = combinationList(List(List(1, 2), List(3, 4)))
  //     a: List[List[Int]] = List(List(1, 3), List(2, 3), List(1, 4), List(2, 4))
  // }}}

  def combinationList[T](ls: List[List[T]]): List[List[T]] = ls match {
    case Nil => Nil :: Nil
    case head :: tail => 
      val rec = combinationList[T](tail)
      rec.flatMap(r => head.map(e => e :: r))
  }

  // May be too much performance penalty?
  /* Usage:
  scala> "123" match { case r"(\d+)$d" => d.toInt case _ => 0 }
  res36: Int = 123

  scala> "10+15" match { case r"(\d\d)${first}\+(\d\d)${second}" => first.toInt+second.toInt case _ => 0 }
  res38: Int = 25
  */
  implicit class Regex(sc: StringContext) {
    def r = new scala.util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  /**If str is surrounded by quotes it return the content between the quotes*/
  def unquote(str: String) = {
    if (str != null && str.length >= 2 && str.charAt(0) == '\"' && str.charAt(str.length - 1) == '\"')
      str.substring(1, str.length - 1)
    else if (str != null && str.length >= 2 && str.charAt(0) == '\'' && str.charAt(str.length - 1) == '\'')
      str.substring(1, str.length - 1)
    else
      str
  }

  import scala.util.{Success, Failure, Try}
  import scala.concurrent.{Await, Future, Promise}
  import scala.concurrent.ExecutionContext.Implicits.global

  def futureToFutureWithTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover{ case x => Failure(x) }

  def futureWithTry[T](f: Future[T]): Future[Try[T]] = {
    val p = Promise[Try[T]]()
    f onComplete p.success
    p.future
  }

  def traverseAndCollectSuccessWithNoError[A, B](seq: Set[A])(f: A => Future[B]): Future[Set[B]] = {
    Future.traverse(seq)(f andThen futureWithTry) map { outSeq =>
     val res = outSeq collect{ case Success(x) => x }
     //logger.debug(s"Result is: $res")
     res
    }
  }

  def traverseAndCollectSuccess[A, B](seq: Set[A])(f: A => Future[B]): Future[Set[B]] = {
    Future.traverse(seq)(f andThen futureWithTry) map { outSeq =>
     outSeq collect { case Failure(e) => }//logger.error(s"Failed at function $f with message ${e.getMessage}") }
     val res = outSeq collect{ case Success(x) => x }
     //logger.debug(s"Result is: $res")
     res
    }
  }

  def transformOptionToFuture[A](o: Option[Future[A]]): Future[Option[A]] = o.map(f => f.map(Option(_))).getOrElse(Future.successful(None))

/*
  import org.apache.commons.codec.binary.Base64
  val base64 = "data:([a-z]+);base64,(.*)".r
  def decodeBase64 (src: String): Option[(String, Array[Byte])] = src match {
    case base64(mimetype, data) => Some( (mimetype, Base64.decodeBase64(data.getBytes("utf-8"))) )
    case _ => None
  }
*/
}
