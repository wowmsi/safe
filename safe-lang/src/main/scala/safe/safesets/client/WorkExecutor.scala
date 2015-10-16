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



package safe.safesets
package client

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.collection.mutable.{Map => MutableMap}
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent._

import spray.http._
import spray.client.pipelining._

import safe.safelog.Credential
import safe.safelang._
import safe.safelang.model._
import safe.safesets.SafeSetsMessageProtocol._


object WorkExecutor {
  def props(storeURI: String): Props = Props(classOf[WorkExecutor], storeURI)
  def name: String   = "WorkExecutor"

  case class Completed(originLink: SetId)
}

class WorkExecutor(val storeURI: String) extends Actor with StorageService with ActorLogging {
  import WorkerExecutorMessageProtocol.{MoreWork, WorkCompleted}

  import WorkExecutor._

  import context.dispatcher // execution context for futures

  val slangParser = Parser()

  // fetchSession is needed due to forward reference to idSet (leaf), which may inturn signed by a third party, and hence forming a chain.
  val fetchSession: MutableMap[SetId, SignedCredential] = MutableMap.empty
  val verifiedIds: MutableMap[SetId, Boolean]           = MutableMap.empty
  val speaksForIds: MutableMap[SetId, Boolean]          = MutableMap.empty

  var workDelegator: Option[ActorRef] = None

  def receive: Receive =  fetchHandler orElse postHandler orElse deleteHandler

  def replaceAllHtml(content: String): String = {
    val res = content.replaceAll("(<pre>|</pre>|<a.*?>|</a>)", "")
    res
  }

  def fetchHandler: Receive = {

    case msg @ SimpleGet(id: SetId) =>
      val requester = sender()
      fetchSet(id) onComplete {
	case Failure(msg) =>
	  workDelegator.get ! ParseFailed(id, s"No set with id: $id found")
	  context.become(receive)
	case Success(response) =>
	  val cert = response.status.intValue match {
	    case 200 =>
	      val content = response.entity.asString
	      requester ! WorkCompleted(IOCompleted(Map.empty, content))
	      context.become(receive)
	    case 300 | 301 | 302 | 303 | 307 => // redirect
	      //redirectLink()
	      throw new Exception("fetch failed: redirect detetced: $id")
	    case statusCode => // error?
	      throw new Exception(s"fetch failed: unknown status code returned: $statusCode, $id")
	  }
     }
     context.become(receive)

    case msg @ FetchSet(link: SetId, originLink: SetId, speaksForOriginLink: SetId, speakerId: SetId, subjectId: SetId, setName: String) =>

      if(workDelegator.getOrElse(None) == None) workDelegator = Some(sender())

      //println(s"In SafeSetsWorkerExecutor: $msg")
      log.info(s"In SafeSetsWorkerExecutor: $msg")

      if(fetchSession.contains(link)) {
        
      } else {
	fetchSet(link) onComplete {
	  case Failure(msg) =>
            //println(s"Fetch failed for link $link with msg: $msg")
	    workDelegator.get ! ParseFailed(link, s"No set with id: $link found")
	    context.become(receive)
	  case Success(response) =>
	    val cert = response.status.intValue match {
	      case 200 =>
		response.entity.asString
	      case 300 | 301 | 302 | 303 | 307 => // redirect
		//redirectLink()
		throw new Exception("fetch failed: redirect detetced: $link")
	      case statusCode => // error?
		throw new Exception(s"fetch failed: unknown status code returned: $statusCode, $link")
	    }

            //println(s"Set fetched for the link $link is $cert")
	    val certificate: SignedCredential = slangParser.parseCertificate(replaceAllHtml(cert)).credential match {
	      case c: SignedCredential => c
	      case _ => throw new Exception("parsing failed: credential is not signed properly") // TODO
	    }
	    if(certificate.speaker.id.toString == certificate.id) { // id set found
	      //println(s"Identity set found for id: ${certificate.id}")
	      certificate.principalSubject match {
		case None =>
		  println(Console.RED + s"Identity set found but principal() predicate is missing" + Console.RESET)
		  workDelegator.get ! ParseFailed(link, s"Identityset found but principal() predicate is missing")
		  context.become(receive)
		case Some(subject) =>
		  //val subject: Subject = Subject(sub)
		  if(certificate.verify(subject)) {
		    log.info(s"Certificate with id ${certificate.id} verified successfully; subject: $subject")
		    //println(s"Certificate with id ${certificate.id} verified successfully; subject: $subject")
		    verifiedIds(certificate.id)  = true
		    speaksForIds(certificate.id) = true
		    fetchSession(certificate.id) = certificate
		    //verify any other certs which are dependent on this identity set principal()
		    self ! IdSet(certificate.id, subject, originLink)
		  }
		  else {
		    println(Console.RED + s"Certificate verification failed for setId: ${certificate.id}; subject: $subject" + Console.RESET)
		    workDelegator.get ! VerificationFailed(link)
		  }
		  context.become(receive)
	      }
	    } else {
	      //println(s"In SafeSetsWorkerExecutor: requesting for id set: ${certificate.speaker.id}")
	      log.info(s"In SafeSetsWorkerExecutor: requesting for id set: ${certificate.speaker.id}")

              if(speaksForOriginLink != "nil") { // speaksFor subject set in which the subject made stmt such as -- subject: speaks(spkr, subject).
		//println(s"In SafeSetsWorkerExecutor: speaksFor case: ${speakerId}")
		log.info(s"In SafeSetsWorkerExecutor: speaksFor case: ${speakerId}")
		// check for speaksFor or speaksForOn on the statements
		val speaksForStatements = certificate.speaksFor.collect {
		  case SpeaksFor(spkr, subj, delegatable) if ((spkr.toString == subjectId) && (subj.toString == speakerId)) => true
		}
		val speaksForOnStatements = certificate.speaksForOn.collect { 
		  case SpeaksForOn(spkr, subj, objectId, delegatable) if (
		      (spkr.toString == subjectId) 
		    & (subj.toString == speakerId)
		    & (objectId.toString == setName) // TODO: objectId contains cred #'application/slog'; will fail
		  ) => true
		}
		if((speaksForStatements ++ speaksForOnStatements).contains(false)) {
		  throw new Exception(s"fetch failed: speaksFor validation failed for setId: ${speaksForOriginLink}")
		} else {
		   //println(s"In SafeSetsWorkerExecutor: speaksFor verified successfully for setId: ${certificate.id}")
		   speaksForIds(speaksForOriginLink)  = true
		}
              }
	      // request for id set verification
	      fetchSession(certificate.id) = certificate
	      verifiedIds(certificate.id)  = false
	      //println(s"FetchSet(${certificate.speaker.id}, ${originLink})")
	      //workDelegator.get ! FetchIdSet(certificate.speakerId, originLink)
	      self ! FetchSet(certificate.speaker.id.toString, originLink)

	      // check for speaksFor or speaksForOn in the speakerStmt
	      (certificate.speaker.id, certificate.subject.id, certificate.speaker.speaksForRef) match {
		case (spkr, subj, None) if spkr.toString != subj.toString => 
		  throw new Exception(s"speaker ($spkr) and subject ($subj) ids differ but speaksFor ref is not found") // speaks-for ref is not present
		case (spkr, subj, None) =>       // no speaks-for; no-op
		  //println(s"No speaksFor case")
		  speaksForIds(certificate.id)  = true
		case (spkr, subj, Some(ref)) =>  // speaks-for case
		  //println(s"speaksFor case: ${certificate.id}")
		  speaksForIds(certificate.id)  = false
		  val refLink = ref.name.getOrElse(ref.speakerId)            // refLink: Id
		  //self ! FetchSet(subj.toString, originLink)               // fetch subject id-set to verify the speaksFor statement
		  self ! FetchSet(refLink.toString, originLink, certificate.id, spkr.toString, subj.toString, certificate.name)  // speaksFor
		  self ! FetchSet(spkr.toString, originLink)                 // fetch speaker id-set
	      }
	    }
	    if((!verifiedIds.values.toSet.contains(false)) && (!speaksForIds.values.toSet.contains(false))) {
	      log.info(s"In SafeSetsWorkerExecutor: calling completeHandler")
	      context.become(completeHandler)
	      self ! Completed(originLink)
	    }
	}
      }
    case msg @ IdSet(link: SetId, subject: Subject, originLink: SetId) =>
      log.info(s"In SafeSetsWorkerExecutor: $msg")
      verifiedIds foreach {
        case (id, false) if fetchSession(id).speaker.id.toString == subject.scid.toString =>
          if(fetchSession(id).verify(subject)) {
            verifiedIds(id)  = true
            log.info(s"Certificate with id ${id} verified successfully")
          }
          else {
            println(Console.RED + s"Certificate verification failed for setId: (id: $id, link: $link)" + Console.RESET)
            sender() ! VerificationFailed(link)
         }
        case (id, isVerified) => 
          //println(s"In IdSet: ($id, $isVerified); speaker: ${fetchSession(id).speaker.id}; link: $link")
          //println(s"In IdSet: (id: $id, isVerified: $isVerified, speakerId: ${fetchSession(id).speaker.id}, link: $link, subjectId: ${subject.scid.toString}")
      }
      //println(s"verifiedIds: $verifiedIds")
      //println(s"speaksForIds: $speaksForIds")
      if((!verifiedIds.values.toSet.contains(false)) && (!speaksForIds.values.toSet.contains(false))) {
        log.info(s"In SafeSetsWorkerExecutor -- IdSet: calling completeHandler")
        context.become(completeHandler)
        self ! Completed(originLink)
      }
  }

  def completeHandler: Receive = {
    case msg @ Completed(originLink: SetId) =>
      log.info(s"In SafeSetsWorkerExecutor: $msg, workDelegator: $workDelegator")
      //println(s"In SafeSetsWorkerExecutor: $msg, workDelegator: $workDelegator; fetchSession: $fetchSession")
      // enqueue links
      fetchSession foreach {
	case (id, cred) => cred.links foreach { link =>
	  workDelegator.get ! MoreWork(FetchSet(link, originLink))
	}
      }
      workDelegator.get ! WorkCompleted(IOCompleted(fetchSession.toMap, originLink))
      workDelegator = None
      fetchSession.clear
      verifiedIds.clear
      context.become(receive)
  }

  def postHandler: Receive = {
    case msg @ SimplePost(id: String, content: String) =>
      val requester = sender()
      postSet(id, content) onComplete {
	case Success(response) if (response.status.intValue == 204) => // 204 No content
	  requester ! WorkCompleted(IOCompleted(Map.empty, content))
	case _ =>
	  requester ! WorkCompleted(IOCompleted(Map.empty, id))
      }
      context.become(receive)

    case msg @ PostSetWithName(name: String, unsignedCredential: UnsignedCredential, speaker: Principal) =>
      log.info(s"In SafeSetsWorkerExecutor: $msg")
      val requester = sender()
      val id = unsignedCredential.computeId(speaker).toString
      
      fetchSet(id) onComplete {
        case Failure(_) =>
          requester ! WorkCompleted(ParseFailed(id, s"Fetch failed: $id"))
          //println(s"In SafeSetsWorkerExecutor -- fetch failed")
          context.become(receive)
        case Success(response) =>
          //println(s"In SafeSetsWorkerExecutor -- response: ${response.status.intValue}")
          response.status.intValue match {
            case 404 => 
              val encoding = if(unsignedCredential.encoding.isEmpty) Seq("slang") else unsignedCredential.encoding
              val certificateSeq = encoding map { format =>
                val (_, certificate) = unsignedCredential.signAndEncode(speaker, format)
                //println(s"certificate: $certificate")
                certificate
              }
              val allCertificates = certificateSeq.mkString("\n") // TODO
              postSet(id, allCertificates) onComplete {
		case Success(response) if (response.status.intValue == 204 | response.status.intValue == 200) => // 204 No content
                  //println(s"PostSet complete with success")
                  val signedCredential: SignedCredential = 
                    slangParser.parseCertificate(replaceAllHtml(allCertificates)).credential match {
	       	      case c: SignedCredential => c
		      case _ => throw new Exception("parsing failed: credential is not signed properly") // TODO
	            }
                  requester ! WorkCompleted(IOCompleted(Map(id -> signedCredential), id))
		case response =>
                  println(s"Posting Set failed: ${response}")
                  requester ! WorkCompleted(IOCompleted(Map.empty, id))
		  //requester ! WorkCompleted(IOCompleted(Map.empty, id))
		  //sender() ! ParseFailed(id, s"Post failed: $id")
		  //context.become(receive)
	      }
              context.become(receive)

            case 200 =>
              val content = response.entity.asString
	      val retrievedCredential: SignedCredential = slangParser.parseCertificate(replaceAllHtml(content)).credential match {
		case c: SignedCredential => 
                  //println(s"Fetched Content String: $content")
                  //println(s"RETRIVED CRED: $c\n")
                  //scala.io.StdIn.readLine()
                  c
		case _ => throw new Exception("parsing failed: credential is not signed properly") // TODO
	      }
	      if(retrievedCredential.speaker.id.toString == speaker.scid.toString) {
		// merge set contents
                
                //println(s"Current credential: $unsignedCredential")
                //println(s"RetreivedCredential: $retrievedCredential")
		val mergedCredential: UnsignedCredential = if(retrievedCredential.subject.id.toString == retrievedCredential.speaker.id.toString) { // self-signed
                    unsignedCredential.mergeCredential(retrievedCredential) // TODO: extend to multiple foramts
                  } else { // speaker-issued
                    unsignedCredential.mergeCredential(retrievedCredential.setData, retrievedCredential.speaker.id.toString, retrievedCredential.name)
                  }
                //println(s"MergedCredential: $mergedCredential")
                val encoding = if(mergedCredential.encoding.isEmpty) Seq("safe") else mergedCredential.encoding
		val (_, certificate) = mergedCredential.signAndEncode(speaker, encoding.head) // TODO
                //println(s"In SafeSetsWorkerExecutor; certificate: $certificate")
		// post
		postSet(id.toString(), certificate) onComplete {
		  case Success(response) if (response.status.intValue == 204 | response.status.intValue == 200) =>
		    val signedCredential: SignedCredential = 
                      slangParser.parseCertificate(replaceAllHtml(certificate)).credential match {
		        case c: SignedCredential => c
		        case _ => throw new Exception("parsing failed: credential is not signed properly") // TODO
		      }
		    requester ! WorkCompleted(IOCompleted(Map(id -> signedCredential), id))
		  case _ => 
		    requester ! WorkCompleted(Map.empty, id)
		    //sender() ! ParseFailed(id, s"Delete failed: $id")
		    //context.become(receive)
		}
		context.become(receive)
	      } else {
		requester ! WorkCompleted(ParseFailed(id.toString(), s"Speaker ${speaker.scid} does have own set with reference $id"))
	      }

            case 300 | 301 | 302 | 303 | 307 => // redirect
	      //redirectLink()
              throw new Exception("fetch failed: redirect detetced")
	    case statusCode => // error?
              throw new Exception(s"fetch failed: unknown status code returned: $statusCode")
          }
      }
  }

  def deleteHandler: Receive = {
    case msg @ SimpleDelete(id: SetId) =>
      val requester = sender()
      deleteSet(id) onComplete {
	case Success(response) if (response.status.intValue == 204) => // 204: no content
	  requester ! WorkCompleted(IOCompleted(Map.empty, id))
	  context.become(receive)
	case _ => 
	  requester ! WorkCompleted(ParseFailed(id, s"Delete failed: $id"))
	  context.become(receive)
      }

    case msg @ DeleteSet(link: SetId, speaker: Principal) =>
      val requester = sender()
      log.info(s"In SafeSetsWorkerExecutor: $msg")
      fetchSet(link) onComplete {
        case Failure(_) =>
          requester ! WorkCompleted(ParseFailed(link, s"Fetch failed: $link"))
          context.become(receive)
        case Success(response) =>
          response.status.intValue match {
            case 404 =>
              requester ! WorkCompleted(IOCompleted(Map.empty, link))
            case 200 =>
              val content = response.entity.asString
	      val retrievedCredential: SignedCredential = slangParser.parseCertificate(replaceAllHtml(content)).credential match {
		case c: SignedCredential => c
		case _ => throw new Exception("parsing failed: credential is not signed properly") // TODO
	      }
	      if(retrievedCredential.speaker.id.toString == speaker.scid.toString) {
		deleteSet(link) onComplete {
                  case Success(response) if (response.status.intValue == 204) => // 204: no content
		    requester ! WorkCompleted(IOCompleted(Map.empty, link))
		    context.become(receive)
                  case _ => 
                    requester ! WorkCompleted(ParseFailed(link, s"Delete failed: $link"))
                    context.become(receive)
                }
	      }
	      else {
		requester ! WorkCompleted(ParseFailed(link, s"Speaker ${speaker.scid} does have own set with reference $link"))
	      }
            case 300 | 301 | 302 | 303 | 307 => // redirect
	      //redirectLink()
              throw new Exception("fetch failed: redirect detetced")
	    case statusCode => // error?
              throw new Exception(s"fetch failed: unknown status code returned: $statusCode")
          }
      }
    case msg @ DeleteSetWithName(name: String, speaker: Principal) =>
      log.info(s"In SafeSetsWorkerExecutor: $msg")
      val id = speaker.subject.computeId(name).toString() // TODO: tmp
      self ! DeleteSet(id, speaker)
  }
}
