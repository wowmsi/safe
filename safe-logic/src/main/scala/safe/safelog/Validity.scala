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

import org.joda.time.{DateTime, Period}
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat, PeriodFormatter, ISOPeriodFormat}

/**
 * Helper class to hold the certificate validity range.
 * 
 * @param notBefore the earliest date/time the credential may be used
 * @param notAfter the last date/time the credential may be used
 */
case class Validity(notBefore: DateTime, notAfter: DateTime, refresh: Period) {
  // check whether the certificate is valid at a given time; default is current time
  def isValidOn(date: DateTime): Boolean = {
    (notBefore == null || notBefore.isBefore(date)) && (notAfter == null || notAfter.isAfter(date))
  }

  override def toString(): String = s"""validity('${Validity.format.print(notBefore)}', '${Validity.format.print(notAfter)}', '${Validity.periodFormat.print(refresh)}')""".stripMargin


  /*
  override def toString(): String = s"""
    | notBefore('${Validity.format.print(notBefore)}'),
    | notAfter('${Validity.format.print(notAfter)}')
    """.stripMargin
  */
}

object Validity {
  val format: DateTimeFormatter = ISODateTimeFormat.dateTime()
  val periodFormat: PeriodFormatter = ISOPeriodFormat.standard()
  //def now() = new DateTime() // TODO: [CHANGE] Should be turned on in a real production setting
  def now(): DateTime = new DateTime(2014, 5, 11, 1, 0) // "05/11/2014 01:00:00"
  //def defaultPeriod(): Period = new Period(hours = 24, minutes = 0, seconds = 0, millis = 0)
  def defaultPeriod(): Period = new Period(24, 0, 0, 0)
  def apply(): Validity = {
    val notBefore = now()
    new Validity(notBefore.minusMinutes(10), notBefore.plusYears(3), defaultPeriod())
  }

  def apply(notBefore: String, notAfter: String): Validity = {
    new Validity(format.parseDateTime(notBefore), format.parseDateTime(notAfter), defaultPeriod())
  }
  def apply(notBefore: DateTime, notAfter: DateTime): Validity = {
    new Validity(notBefore, notAfter, defaultPeriod())
  }
  def apply(notBeforeMayBe: Option[String], notAfterMayBe: Option[String], refreshMayBe: Option[String]): Validity = (notBeforeMayBe, notAfterMayBe, refreshMayBe) match {
    case (None, None, None) =>  
      val notBefore = now()
      new Validity(notBefore.minusMinutes(10), notBefore.plusYears(3), defaultPeriod())
    case (None, None, Some(refresh)) =>  
      val notBefore = now()
      new Validity(notBefore.minusMinutes(10), notBefore.plusYears(3), periodFormat.parsePeriod(refresh))
    case (Some(notBefore), Some(notAfter), Some(refresh)) =>
      new Validity(format.parseDateTime(notBefore), format.parseDateTime(notAfter), periodFormat.parsePeriod(refresh))
    case (Some(notBefore), Some(notAfter), None) =>
      new Validity(format.parseDateTime(notBefore), format.parseDateTime(notAfter), defaultPeriod())
    case (Some(notBefore), None, None) =>
      val notAfter = now().plusYears(3)
      new Validity(format.parseDateTime(notBefore), notAfter, defaultPeriod())
    case (None, Some(notAfter), None) =>
      val notBefore = now()
      new Validity(notBefore.minusMinutes(10), format.parseDateTime(notAfter), defaultPeriod())
    case (Some(notBefore), None, Some(refresh)) =>
      val notAfter = now().plusYears(3)
      new Validity(format.parseDateTime(notBefore), notAfter, periodFormat.parsePeriod(refresh))
    case (None, Some(notAfter), Some(refresh)) =>
      val notBefore = now()
      new Validity(notBefore.minusMinutes(10), format.parseDateTime(notAfter), periodFormat.parsePeriod(refresh))
  }

  val dateRegex = """\s*(\$now)\s*(([\+\-])\s*([\d]+)(\.(milli|second|minute|hour|day|week|month|year)s?)?)?""".r

  def getDate(date: String, now: DateTime = new DateTime(), format: DateTimeFormatter = ISODateTimeFormat.dateTime()): String = date match {
    case dateRegex(nowEnv, _, op, num, _, tpe) =>
      if(op == null) now.toString
      else if(op == "+") {
        val newTpe = tpe.capitalize
        val newOp = s"plus${newTpe}s"
        //val res = now.newOp(num) // TODO: use macros
        // enumerate for now
        val res = if(tpe == "null") now.plus(num.toLong)
          else if(tpe == "year") now.plusYears(num.toInt)
          else if(tpe == "minute") now.plusMinutes(num.toInt)
          else sys.error("not yet implemented")
        format.print(res)
      } else if(op == "-") {
        val res = if(tpe == "null") now.minus(num.toInt)
          else if(tpe == "year") now.minusYears(num.toInt)
          else if(tpe == "minute") now.minusMinutes(num.toInt)
          else sys.error("not yet implemented")
        format.print(res)
      } else {
        //logger.error("Unknown date operation specified")
        sys.error("Unknown date operation specified")
      }
    case  _ => date
  }
}
