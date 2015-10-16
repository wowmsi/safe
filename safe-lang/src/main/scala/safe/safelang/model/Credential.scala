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

import safe.safelog.{
    Assertion, Credential, ProofSubContext, Index, Statement, Validity
  , NotImplementedException, Retraction, Term, UnSafeException, Query 
  , QueryAll, Constant, Structure, ParserException, Variable, SafelogException
  , Encoding, StrLit
}

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}

case class UnsignedUnBoundCredential(
    metadata: Map[Index, Set[Statement]]
  , statements: Map[Index, Set[Statement]]
  , nameMayBe: Option[Term]
  , args: Map[StrLit, Term]
  , immutable: Boolean
  , source: String        // useful to construct name if credential is immutable
) extends Credential {

  import model.CredentialHelper._ // helper functions to extract data from statements

  val validity: Validity = Validity() // TODO: validity is a part of Credential trait; does not matter what we assign here

  def bind(args: Map[StrLit, Term]): UnsignedCredential = {
    val isBindRequired: Boolean = {
      val isVariableUnBound: Seq[Boolean] = args.collect {
        case (envVar: StrLit, envValue: Variable) => true
      }.toSeq
      if(isVariableUnBound.size == args.size) false else true
    }
    def getBindedStatements(statements: Map[Index, Set[Statement]]): Map[Index, Set[Statement]] = if(isBindRequired) {
	(statements.keySet.map {
	  case idx: Index =>
	    val resultStatements: Set[Statement] = {
	      statements.get(idx).getOrElse(Set.empty).map{stmt => stmt.bind(args)}
	    }
	    idx -> resultStatements
	}).toMap
    } else statements

    val nameMayBeBinded = {
      var _term = nameMayBe
      args.foreach {
        case (varName, varValue) =>
          _term = _term.map(_.bind{v => if(v.name == varName.name) varValue else Variable(v)})
      }
      _term
    }
    val credentialBinded = this.copy(
        metadata   = getBindedStatements(metadata)
      , statements = getBindedStatements(statements)
      , nameMayBe  = nameMayBeBinded //TODO: check; ignoring args as ubounded args may be only $Self or $SelfKey
    )
    credentialBinded.buildUnsignedCredential()
  }

  override def bind(f: StrLit => Term): UnsignedUnBoundCredential = {
    val metadataBinded = metadata.map {
      case (k, v) => (k, v.map(_.bind(f)))
    }
    //val statementsBinded = statements
    // this is expensive since we bind per variable with multiple passes on slog statements
    val statementsBinded = statements.map {
      case (k, v) => (k, v.map(_.bind(f)))
    }
    val nameMayBeBinded = nameMayBe.map(_.bind(f))
    val argsBinded: Map[StrLit, Term] = args.map {
      case (k: StrLit, v: Constant) => (k, v)        // this will ensure the constants are not replaced by later env variables
      case (k: StrLit, v: Variable) => (k, v.bind(f))
      case other                    => throw UnSafeException(s"slog variable is not bounded: $other") // should never catch this
    }
    this.copy(metadata = metadataBinded, statements = statementsBinded, nameMayBe = nameMayBeBinded,  args = argsBinded)
  }

  /** buildUnsignedCredential() is called after bind, i.e., after all the global variables binded and are available in args*/
  def buildUnsignedCredential(): UnsignedCredential = {
  
    val encoding: Seq[String] = getAllFirstAttributes(metadata.get(StrLit("_encoding")).getOrElse(Nil).toSeq)
    val version: String       = getFirstAttribute(getUniqueStatement(metadata.get(StrLit("_version")).getOrElse(Nil).toSeq)).getOrElse("1")

    val speaker: Option[SpeakerStmt] = {
      val speakerStmt: Seq[String] = getAllAttributes(getUniqueStatement(metadata.get(StrLit("_speaker")).getOrElse(Nil).toSeq))
      val spkr: Option[SpeakerStmt] = speakerStmt.length match { 
        case 0          => None // no speaker defined
        case l if l < 3 =>      // self: speaker(publicKey, ref-for-speaksFor)
	  throw ParserException(s"speaker statement is malformed: ${speakerStmt}")
        case _          =>
	  Some(SpeakerStmt(speakerStmt(0), speakerStmt(1), speakerStmt(2)))
	}
      spkr
    }
    
    val subject: Option[SubjectStmt] = {
      val subjectStmt: Seq[String] = getAllAttributes(getUniqueStatement(metadata.get(StrLit("_subject")).getOrElse(Nil).toSeq))
      val subj: Option[SubjectStmt] = subjectStmt.length match {
        case 0          => None // no subject defined
        case l if l < 3 =>      // self: subject(subject-id, publicKey)
	  throw ParserException(s"subject statement is malformed: ${subjectStmt}")
        case _          =>
	  Some(SubjectStmt(
              subjectStmt(1)
            , subjectStmt(2)
          ))
	}
      subj
    }

    val recipient: Option[RecipientStmt] = {
      val recipientStmt: Seq[String] = getAllAttributes(getUniqueStatement(metadata.get(StrLit("_recipient")).getOrElse(Nil).toSeq))
      val subj: Option[RecipientStmt] = recipientStmt.length match {
        case 0          => None // no subject defined
        case l if l < 5 =>      // self: recipient(subject-id, publicKey, sharedKeyLocation, encryptionAlgorithm)
	  throw ParserException(s"recipient statement is malformed: ${recipientStmt}")
        case _          =>
	  Some(RecipientStmt(
              recipientStmt(1)
            , recipientStmt(2)
            , recipientStmt(3)
            , recipientStmt(4)
          ))
	}
      subj
    }

    val validity: Validity = {
      val validityStmt: Seq[String] = getAllAttributes(getUniqueStatement(metadata.get(StrLit("_validity")).getOrElse(Nil).toSeq))
      val validityOut: Validity = validityStmt.length match { 
	case 0          => Validity()  // no validity range defined
	case l if l < 3 =>             // self: validity(notBefore, notAfter)
	  throw ParserException(s"validity statement is malformed: ${validityStmt}")
	case _          =>
	  Validity(validityStmt(1), validityStmt(2))
	}
	validityOut
    }

    val signatureAlgorithm: String     =  // TODO: make the default configurable
      getFirstAttribute(getUniqueStatement(metadata.get(StrLit("_signatureAlgorithm")).getOrElse(Nil).toSeq)).getOrElse("SHA256withRSA")

    // TODO: Publishing queries as a part of certificate and inhaling queries?
    val queries: Seq[Statement] = metadata.get(StrLit("_query")).getOrElse(Nil).toSeq

    val retractions: Seq[Statement] = metadata.get(StrLit("_retraction")).getOrElse(Nil).toSeq

    /*
    val imports: Seq[String] = {
      val importAllStmts: Seq[Statement] = metadata.get(StrLit("_importAll")).getOrElse(Nil).toSeq
      //println(s"importAllStmt: $importAllStmts")
      // unfold all the seq terms
      val importAllSeq: Seq[Term] = importAllStmts.collect { stmt => stmt.terms.head match {
        case Structure(StrLit("importAll"), spkr +: Structure(StrLit("_seq"), importSeq, _, _, _) +: other, _, _, _) => importSeq
      }}.flatten
      getAllFirstAttributes(metadata.get(StrLit("_import")).getOrElse(Nil).toSeq) ++ importAllSeq.map(t => t.id.name)
    }
    */
    val imports: Seq[Statement] = metadata.get(StrLit("_import")).getOrElse(Nil).toSeq ++ metadata.get(StrLit("_importAll")).getOrElse(Nil).toSeq

    val links: Seq[String] = getAllFirstAttributes(statements.get(StrLit("_link")).getOrElse(Nil).toSeq)

    //val nameInSet = getFirstAttribute(getUniqueStatement(metadata.get(StrLit("_name")).getOrElse(Nil).toSeq))
    val nameInSet: Option[Term] = getUniqueStatement(metadata.get(StrLit("_name")).getOrElse(Nil).toSeq).map(s => s.terms.head match {
      case Structure(StrLit("name"), spkr +: value +: Nil, _, _, _) => value
    })
    val principalMayBe = getFirstAttribute(getUniqueStatement(statements.get(StrLit("_principal")).getOrElse(Nil).toSeq))
    val name: Term = getName(nameInSet, nameMayBe, principalMayBe, queries, source)

    UnsignedCredential(
	encoding
      , version
      , speaker
      , subject
      , validity
      , name
      , statements
      , signatureAlgorithm
      , queries
      , retractions
      , imports
      , links
      , args
      , immutable
    )
  }
}

case class UnsignedCredential(
    encoding: Seq[String]
  , version: String
  , speakerStmt: Option[SpeakerStmt]
  , subjectStmt: Option[SubjectStmt]
  , validity: Validity
  , name: Term
  , statements: Map[Index, Set[Statement]]
  , signatureAlgorithm: String
  , queries: Seq[Statement]
  , retractions: Seq[Statement]
  , imports: Seq[Statement]
  , links: Seq[String]
  , args: Map[StrLit, Term]
  , immutable: Boolean
) extends Credential {

  private def getStatements(speaker: String): String = {
    val stmts: String = statements.values.flatten.toSeq.map(x => x.toStringCompact(speaker)).mkString("\n      ")
    stmts
  }

  def prepareSignedData(principal: Principal, cred: String, subjectStmt: Option[SubjectStmt], isEncrypted: Boolean): String = {
    val format: DateTimeFormatter = ISODateTimeFormat.dateTime()
    //val now: String = format.print(new DateTime()) // TODO: [CHANGE] Should be turned on in a real production setting
    val now: String = format.print(new DateTime(2014, 2, 13, 16, 30)) // "02/13/2014 16:30:00"
    val encodingSeq: Seq[String] = if(encoding.isEmpty) Seq("slang") else encoding

    val speaker: SpeakerStmt = speakerStmt.getOrElse(SpeakerStmt(principal.scid.toString))
    val subject: SubjectStmt = subjectStmt.getOrElse(SubjectStmt(principal.scid.toString))
    // this is important
    //require(speaker.id == principal.scid.toString)
    if(speaker.id.toString != principal.scid.toString) throw UnSafeException(s"Speaker does not match with the speaker provided in the set")

    val typeTag: String = if(isEncrypted) "cred #'application/slog-encrypted'" else "cred #'application/slog;charset=utf8;hash=md5'"

    s"""
    |    version('$version'),
    |    $speaker,
    |    $subject,
    |    $validity,
    |    $typeTag -> ${cred},
    |    signatureAlgorithm('$signatureAlgorithm')""".stripMargin
  }

  /*
  def sign(speaker: Principal): Array[Byte] = {
    val dataToSign = prepareSignedData(speaker)
    speaker.sign(dataToSign.getBytes(StringEncoding), signatureAlgorithm)
  }
  */

  override def mergeCredential(credential: Credential): UnsignedCredential = {
    val retractStmts: Seq[Statement] = retractions match { // retreived credential should not have retraction stmts
      case Nil           => Nil
      case stmts         => stmts.map{ stmt => Assertion(stmt.terms)}
    }
    mergeStatements(credential.statements, retractStmts.toSet)
  }

  def mergeStatements(subjectStmts: Map[Index, Set[Statement]], retractStmts: Set[Statement] = Set.empty): UnsignedCredential = {
    copy(statements = {
      ((statements.keySet ++ subjectStmts.keySet).map {
        case i: Index =>
          val resultStatements: Set[Statement] = {
            statements.get(i).getOrElse(Set.empty) ++ 
            subjectStmts.get(i).getOrElse(Set.empty) -- 
            retractStmts
          }
         //if(!resultStatements.isEmpty) {
            i -> resultStatements
          //}
      }).toMap
    })
  }

  // useful during post when speaker and subject are different
  def mergeCredential(setData: String, speaker: String, name: String): UnsignedCredential = {
    val signedStatements = safe.safelang.Parser.parseSignedCredential(speaker = speaker, name = name, stmts = setData)
    val assertions = signedStatements.assertions
    mergeStatements(assertions)
  }

  private def signAndGetData(speaker: Principal, cred: String, subjectStmt: Option[SubjectStmt], isEncrypted: Boolean): Tuple2[Array[Byte], String] = {
    val dataToSign = prepareSignedData(speaker, cred, subjectStmt, isEncrypted)
    val signature  = speaker.sign(dataToSign.getBytes(StringEncoding), signatureAlgorithm)
    (signature, dataToSign)
  }

  def signAndEncode(speaker: Principal, format: String = "slang"): Tuple2[String, String] = format.toLowerCase match {
    case "safe" | "slang"  => 
      encodeToSlang(speaker, computeId(speaker))
    case "x509"            => 
      throw NotImplementedException(s"X509 certificate format not yet implemented; use safe as an alternative")
  }

  def computeId(principal: Principal): Id = {
    val speaker: SpeakerStmt = speakerStmt.getOrElse(SpeakerStmt(principal.scid.toString))
    val subject: SubjectStmt = subjectStmt.getOrElse(SubjectStmt(principal.scid.toString))
    val nameStr = name.id.name
    val id = speaker.speaksForRef match {
      case None             => principal.subject.computeId(nameStr)
      case Some(speakerRef) => 
	val parentSpeaker = speakerRef.name match {
	  case None => // speakerRef is specified as combined hash
	    subject.id
	  case Some(_) if(speakerRef.speakerId == subject.id) => //
	    subject.id
	  case _ => throw ParserException(s"speakerRef principal (${speakerRef.speakerId}) does not match with subject (${subject.id})")
	}
	val setId  = Scid(parentSpeaker, nameStr)
	Id(setId)
    }
    id
  }

  private def encodeToSlang(speaker: Principal, setId: Id): Tuple2[String, String] = {
    val nameStr = if(name.id.name == "" | name.id.name == "nil") s"" else s"'${name.id.name}'"
    val stmts: String = getStatements(speaker.scid.toString)

    val cred: String = if(immutable) {
        s"""|u$nameStr{{
        |      ${stmts}
        |    }}""".stripMargin
      }
      else {
        s"""|$nameStr{
        |      ${stmts}
        |    }""".stripMargin
      }

    val cert: String = {
      val (credential: String, isEncrypted: Boolean, newSubjectStmt: Option[SubjectStmt]) = subjectStmt match {
        /*
	case Some(rStmt) if rStmt.sharedKeyLocation != "nil" => // case encrypt slog data
	  val recipient = rStmt.subject.getOrElse { 
	    throw new Exception(s"Fetching recipient's key for encryption is not yet supported")
	  }
	  val (encryptedSharedKey: Array[Byte], encryptedData: Array[Byte]) = 
	    recipient.encrypt(cred.getBytes(StringEncoding), rStmt.sharedKeyLocation, rStmt.encryptionAlgorithm)
	  val encodedSharedKey = Identity.base64EncodeURLSafe(encryptedSharedKey)
	  val encodedEncryptedData = Identity.base64EncodeURLSafe(encryptedData)
          (s"u'$encodedEncryptedData'", true, Some(SubjectStmt(rStmt.id, rStmt.subject, encodedSharedKey, rStmt.encryptionAlgorithm)))
        */
	case _ => (cred, false, subjectStmt)
      }
      val (signature, signedData) = signAndGetData(speaker, credential, newSubjectStmt, isEncrypted)
      val signatureEncoded = Identity.base64EncodeURLSafe(signature)
      val certStmt = 	s"""|cert #'application/slang;charset=ut8;hash=sha256' -> u'$setId'(
	|  signedData($signedData
	|  ),
	|  signature(u'$signatureEncoded')
	|).""".stripMargin
      certStmt
    }
    (setId.toString, cert)
  }

  def render(name: StrLit = StrLit("queryContext")): ProofSubContext = { // XXX: TODO
    // fetch all the links
    // check their state
    // validity is the minimum of the sets available
    ProofSubContext(name, validity, statements, queries)
  }
}

case class SignedCredential(
    id: String
  , version: String
  , speaker: SpeakerStmt
  , subject: SubjectStmt
  , validity: Validity
  , name: String
  , statements: Map[Index, Set[Statement]]
  , signatureAlgorithm: String
  , signature: String
  , queries: Seq[Statement]
  , links: Seq[String]
  , principalSubject: Option[Subject]
  , speaksFor: Seq[SpeaksFor]
  , speaksForOn: Seq[SpeaksForOn]
  , setData: String
  , immutable: Boolean
) extends Credential {

  def signedData(): String = {
    val format: DateTimeFormatter = ISODateTimeFormat.dateTime()
    //println(s"NAME: $name")
    val nameStr = if(name == "") "" 
      else if (name.startsWith("\"")) {
        name
      } else if(name.contains("->")) {
        name
      }
      else s"'${name}'"
    val stmts = statements.values.flatten.toSeq.mkString("\n       ")
    s"""
	|    version('$version'),
	|    $speaker,
	|    $subject,
	|    $validity,
	|    $nameStr{
	|      ${setData}
	|    },
	|    signatureAlgorithm('$signatureAlgorithm')""".stripMargin
  }

  def verify(speaker: Subject): Boolean = { 
    //val sig: Signature = Signature.getInstance(signatureAlgorithm, "BC")
    val sig: java.security.Signature = java.security.Signature.getInstance(signatureAlgorithm)
    sig.initVerify(speaker.publicKey)
    //println(s"\n\n\n\nSignedData: ${signedData()}; signature: $signature; id: $id")
    sig.update(signedData().getBytes())
    sig.verify(Identity.base64Decode(signature))
  }

  def render(): ProofSubContext = { // XXX: TODO
    // fetch all the links
    // check their state
    // validity is the minimum of the sets available
    ProofSubContext(StrLit(id), validity, statements, queries)
  }

  def decrypt(principal: Principal): Array[Byte] = sys.error(s"Not yet implemented")
}

case class SignedCredentialHolder(
    name: String
  , setData: String
  , assertions: Map[Index, Set[Statement]]
  , queries: Seq[Statement]
  , links: Seq[String]
  , principal: Option[String]
  , speaksFor: Seq[SpeaksFor]
  , speaksForOn: Seq[SpeaksForOn]
)

object CredentialState extends Enumeration {
  type CredentialState = Value
  val Active, Revoked, Suspended = Value                                      
}

object CredentialHelper {
  /** Get a name string from a statement */
  @inline
  def getName(
      nameInSet: Option[Term]
    , nameMayBe: Option[Term]
    , principalMayBe: Option[String]
    , queries: Seq[Statement]
    , source: String
  ): Term = (nameMayBe, principalMayBe) match {

    case (Some(Variable(StrLit("%ImmutableHash"), _, _, _)), _) =>
      Constant(Identity.base64EncodeURLSafe(Identity.hash(source.getBytes(StringEncoding), "MD5")))

    case (Some(setName: Constant), _) => // constant
      if(nameInSet == None) setName
      else if (!queries.isEmpty) {
        // model.Guid().value.name 
        // assign a random name; Note: secureRandom is expensive
        Constant(java.util.UUID.randomUUID().toString())
      }                             
      else throw ParserException(s"name needs to be unique for a set: $nameInSet; $setName")
    case (Some(Structure(StrLit("_interpolate"), Constant(body, attrName, tpe, enc) +: Constant(termSeq, _, _, _) +: xterms, _, _, _)), _) => // interpolation
      if(nameInSet == None) {
        val bindedStr: String = Term.interpolate(body.name, termSeq.name, xterms)
        Constant(StrLit(bindedStr), attrName, tpe, Encoding.AttrLiteral)
      }
      else if (!queries.isEmpty) {
        // model.Guid().value.name 
        // assign a random name; Note: secureRandom is expensive
        Constant(java.util.UUID.randomUUID().toString())
      }                             
      else throw ParserException(s"name needs to be unique for a set: $nameMayBe")

    case (None, Some(sub)) => // identity set
      // if name predicate found, then extracts it; issue an warning and report it else return empty
      nameInSet match {
        case None                                                 => Constant("") // Constant(StrLit("nil"))
        case Some(setName) if setName.id.name == "nil"            => Constant("")
        case Some(Structure(StrLit("_interpolate"), Constant(body, attrName, tpe, enc) +: Constant(termSeq, _, _, _) +: xterms, _, _, _) ) => // interpolation

          val bindedStr: String = Term.interpolate(body.name, termSeq.name, xterms)
          SafelogException.printLabel('warn) 
          println(s"Set name $bindedStr found where no name expected since the set contains a principal() statement")
          Constant(StrLit(bindedStr), attrName, tpe, Encoding.AttrLiteral)
        case Some(value) =>
          SafelogException.printLabel('warn) 
          println(s"Set name $value found where no name expected since the set contains a principal() statement")
          Constant(value.id.name)
      }
    case _           =>
      nameInSet match {
        case None =>  
          // model.Guid().value.name 
          // assign a random name; Note: secureRandom is expensive
          if (!queries.isEmpty) {
            Constant(java.util.UUID.randomUUID().toString())
          }
          else throw ParserException(s"name needs to be unique for a set: $nameMayBe")
        case Some(Structure(StrLit("_interpolate"), Constant(body, attrName, tpe, enc) +: Constant(termSeq, _, _, _) +: xterms, _, _, _)) => // interpolation
          val bindedStr: String = Term.interpolate(body.name, termSeq.name, xterms)
          Constant(StrLit(bindedStr), attrName, tpe, Encoding.AttrLiteral)
        case Some(value) =>
          Constant(value.id.name)
      }
  }

  /** Given a sequence statements, get all attributes that matches a functor */
  @annotation.tailrec
  def findPredicate(stmts: Seq[Statement], functor: StrLit): Seq[String] = stmts match {
    case Nil => Nil
    case Assertion(Structure(pred, values, _, _, _) +: rest) +: other if pred == functor =>
     values.map(v => v.toString)
    case head +: other => findPredicate(other, functor)  
  }

  /** Given a statement, get all attributes of the head predicate as string */
  def getAllAttributes(stmt: Option[Statement]): Seq[String] = stmt match {
    case None => Nil
    case Some(s) => s.terms.head match {
      case Structure(pred, values, _, _, _) => values.map(v => v.toString)
      case _  => throw ParserException(s"statement does not match given functor")
    }
    case _  => throw ParserException(s"statement does not match given functor")
  }

  /** Given a sequence of statements, extracts the first attributes from each statement */
  def getAllFirstAttributes(stmts: Seq[Statement]): Seq[String] = {
    var _acc: Seq[String] = Nil

    @annotation.tailrec
    def recurse(stmts: Seq[Statement]): Unit = stmts match {
      case Nil    =>  Nil
      case Assertion(Structure(pred, value +: Nil, _, _, _) +: rest) +: other =>  
       _acc +:= value.id.name
       recurse(other)
      case Assertion(Structure(pred, speaker +: value +: Nil, _, _, _) +: rest) +: other =>  
       _acc +:= value.id.name
       recurse(other)
      case head +: other =>  recurse(other)
    }

    recurse(stmts)
    _acc.toSeq
  }

  /** Given a statement of a specific predicate, extracts the first attribute */
  @inline
  def getFirstAttribute(stmt: Option[Statement]): Option[String] = stmt match {
    case None      => None
    case Some(s) => s.terms.head match {
      case Structure(pred, principal +: Nil, _, _, _)          =>
        Some("") // for name()
      case Structure(pred, principal +: value +: Nil, _, _, _) => 
        Some(value.id.name)
      case Structure(pred, principal +: Structure(StrLit("_interpolate"), iValue, _, _, _) +: Nil, _, _, _) =>
        Some(s"_interpolate(${iValue.mkString(",")})")
      case _  => throw ParserException(s"statement does not match given functor: ${s.terms.head}")
    }
  }

  /** Given a sequence of statement of a specific predicate, finds the first unique statement */
  @inline
  def getUniqueStatement(stmts: Seq[Statement]): Option[Statement] = stmts.length match {
    case 0        => None
    case 1        => Some(stmts.head)
    case _        => throw ParserException(s"Unique ${stmts} excepted but multiple found")
  }
}
