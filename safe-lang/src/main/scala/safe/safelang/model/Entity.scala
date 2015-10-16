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

import safe.safelog.{ParserException, UnSupportedOperatorException, UnSafeException}

trait Encrypter {
  def encrypt(
      dataToEncrypt: Array[Byte]
    , sharedKeyLocation: String
    , encryptionAlgorithm: String
  ): Tuple2[Array[Byte], Array[Byte]] 
}

sealed abstract class Entity {
  def scid: Scid   // self-certifying identity
}

case class Subject(publicKey: java.security.PublicKey) extends Entity with Encrypter {
  val id: Id = Id(Identity.hash(publicKey.getEncoded()))
  override val scid: Scid = Scid(id, "")

  def computeScid(name: String): Scid = {
    if(name.isEmpty) scid else Scid(id, name)
  }
  def computeId(name: String): Id = {
    if(name.isEmpty) id
    else Id(Scid(id, name))
  }

  //==== Java crypto libraries for encryption and decryption ===//
  import java.security.spec._
  import javax.crypto._
  import javax.crypto.spec._

  def encrypt(
      dataToEncrypt: Array[Byte]
    , sharedKeyLocation: String
    , encryptionAlgorithm: String = "AES"
  ): Tuple2[Array[Byte], Array[Byte]] = sharedKeyLocation match {

    case "nil" => // create a new shared key
      //val aesCipher: SecretKeySpec = encodeSharedKey(createSharedKey(keySize: Int = 256))
      throw ParserException(s"sharedKeyLocation is required")
    case path => 
      val pkCipher  = Cipher.getInstance("RSA")
      val aesCipher = Cipher.getInstance("AES")

      val aesKeySpec: SecretKeySpec = loadSharedKey(path)
      // encrypt AES key with recipient public key
      pkCipher.init(Cipher.ENCRYPT_MODE, publicKey)
      val encryptedSharedKey: Array[Byte] = pkCipher.doFinal(aesKeySpec.getEncoded())

      // encrypt data with shared AES key
      aesCipher.init(Cipher.ENCRYPT_MODE, aesKeySpec)
      val encryptedData: Array[Byte] = aesCipher.doFinal(dataToEncrypt)

      (encryptedSharedKey, encryptedData)
  }

  /** Create a new AES key */
  private def createSharedKey(keySize: Int): SecretKey = {
    val kgen: KeyGenerator = KeyGenerator.getInstance("AES")
    kgen.init(keySize)
    val key: SecretKey = kgen.generateKey()
    key
  }
  private def encodeSharedKey(key: SecretKey): SecretKeySpec = {
    val aesKey: Array[Byte] = key.getEncoded()
    val aesKeySpec = new SecretKeySpec(aesKey, "AES")
    aesKeySpec
  }
  private def loadSharedKey(path: String): SecretKeySpec = {
    val base64EncodedString: String = scala.io.Source.fromFile(path).mkString
    val aesKey: Array[Byte] = Identity.base64Decode(base64EncodedString)
    val aesKeySpec = new SecretKeySpec(aesKey, "AES")
    aesKeySpec 
  }
  private def saveSharedKey(key: SecretKey, path: String): Unit = {
    val aesKey: Array[Byte] = key.getEncoded()
    val keyAsString: String = Identity.base64EncodeURLSafe(aesKey)
      safe.safelog.util.printToFile(new java.io.File(path)) { p =>
      keyAsString.foreach(p.print)
    }
  }

  override def toString() = {
    Identity.base64EncodeURLSafe(publicKey.getEncoded())
  }
}

object Subject {
  def apply(subject: String): Subject = {
    val (encodingType: String, sub: String) = subject match {
      case Identity.encodingPattern(enc, subjectValue) => (enc, subjectValue)
      case _                                           => ("u", subject)
    }
    val subjectBytes: Array[Byte] = encodingType match {
      case "u" => Identity.base64Decode(sub)
      case "h" => Identity.hexDecode(sub)
      case _   => throw UnSupportedOperatorException(s"Encoding type, $encodingType not supported")
    }
    val publicKey: java.security.PublicKey = try{
      java.security.KeyFactory.getInstance("RSA").generatePublic(new java.security.spec.X509EncodedKeySpec(subjectBytes))
    } catch {
      case ex: java.io.IOException => 
        throw UnSafeException(s"Error in generating public key from spec -- invalid key, $sub, specified: $ex")
      case other: Throwable => 
        throw UnSafeException(s"Error in generating public key from spec -- invalid key, $sub, specified: $other")
    }
    new Subject(publicKey)
  }
}

case class SubjectStmt(id: Id, subject: Option[Subject]) extends Entity {
  override val scid: Scid = Scid(id, "") 
  override def toString() = subject match { // TODO: this may not work with encryption
    case None       => 
      s"subject(u'$id', nil)"
    case Some(s)    => 
      s"subject(u'$id', u'$s')"
  }
}
object SubjectStmt {
  def apply(id: String) = {
    new SubjectStmt(Id(id), None)
  }
  def apply(id: String, sub: String) = (id, sub) match {
    case ("nil", "nil") => throw ParserException(s"illegal subjectStmt")
    case ("nil", _)  => 
      val subject = Subject(sub)
      new SubjectStmt(subject.id, Some(subject))
    case (_, "nil")  => 
      new SubjectStmt(Id(id), None)
    case (_, _)  => 
      val subject = Subject(sub)
      if(id == subject.id.toString) new SubjectStmt(subject.id, Some(subject))
      else throw ParserException(s"id:$id does not match with subject hash, $subject")
  }
}

case class RecipientStmt(id: Id, subject: Option[Subject], sharedKeyLocation: String, encryptionAlgorithm: String) extends Entity {
  override val scid: Scid = Scid(id, "") 
  override def toString() = subject match { // TODO: this may not work with encryption
    case None       => 
      if(sharedKeyLocation == "nil")
        s"recipient(u'$id', nil, nil, nil)"
      else s"recipient(u'$id', nil, u'$sharedKeyLocation', '${safe.safelog.Term.stripQuotes(encryptionAlgorithm)}')"
    case Some(s)    => 
      if(sharedKeyLocation == "nil")
        s"recipient(u'$id', u'$s', nil, nil)"
      else s"recipient(u'$id', u'$s', u'$sharedKeyLocation', '${safe.safelog.Term.stripQuotes(encryptionAlgorithm)}')"
  }
}
object RecipientStmt {
  def apply(id: String) = {
    new RecipientStmt(Id(id), None, "nil", "nil")
  }
  def apply(id: String, sub: String) = (id, sub) match {
    case ("nil", "nil") => throw ParserException(s"illegal subjectStmt")
    case ("nil", _)  => 
      val subject = Subject(sub)
      new RecipientStmt(subject.id, Some(subject), "nil", "nil")
    case (_, "nil")  => 
      new RecipientStmt(Id(id), None, "nil", "nil")
    case (_, _)  => 
      val subject = Subject(sub)
      if(id == subject.id.toString) new RecipientStmt(subject.id, Some(subject), "nil", "nil")
      else throw ParserException(s"id:$id does not match with subject hash, $subject")
  }
  def apply(id: String, subject: String, sharedKeyLocation: String, encryptionAlgorithm: String) = (id, subject) match {
    case ("nil", "nil") => throw ParserException(s"illegal subjectStmt")
    case ("nil",  _   ) => 
      val sub = Subject(subject)
      new RecipientStmt(sub.id, Some(sub), sharedKeyLocation, encryptionAlgorithm)
    case (_, "nil")     => 
      new RecipientStmt(Id(id), None, sharedKeyLocation, encryptionAlgorithm)
    case (_, _)         => 
      val sub = Subject(subject)
      if(sub.id.toString != id) throw ParserException(s"id and subject does not match: $id, $subject")
      new RecipientStmt(sub.id, Some(sub), sharedKeyLocation, encryptionAlgorithm)
  }
}

case class SpeakerStmt(id: Id, speaker: Option[Subject], speaksForRef: Option[Scid]) extends Entity {
  override val scid: Scid = Scid(id, "")
  override def toString() = (speaker, speaksForRef) match {
    case (None, None)          => s"speaker(u'$id', nil, nil)"
    case (None, Some(link))    => s"speaker(u'$id', nil, u'$link')"
    case (Some(s), Some(link)) => s"speaker(u'$id', u'$s', u'$link')"
    case (Some(s), None)       => s"speaker(u'$id', nil, nil)"
  }
}
object SpeakerStmt {
  def apply(id: String) = {
    new SpeakerStmt(Id(id), None, None)
  }
  def apply(id: String, spkr: String) = {
    val subject = Subject(spkr)
    if(id.isEmpty) new SpeakerStmt(subject.id, Some(subject), None)
    else if(id == subject.id.toString) new SpeakerStmt(subject.id, Some(subject), None)
    else throw ParserException(s"id:$id does not match with subject hash, $subject")
  }
  def apply(id: String, spkr: String, speaksForRef: String) = (id, spkr, speaksForRef) match {
    case ("nil", "nil", "nil") => throw ParserException(s"illegal speakerStmt")
    case (_, "nil", "nil")  => new SpeakerStmt(Id(id), None, None)
    case ("nil", _, "nil")  => 
      val subject = Subject(spkr)
      new SpeakerStmt(subject.id, Some(subject), None)
    case ("nil", _, _)  => 
      val subject = Subject(spkr)
      new SpeakerStmt(subject.id, Some(subject), Some(Scid(speaksForRef)))
    case (_, "nil", _)  => 
      new SpeakerStmt(Id(id), None, Some(Scid(speaksForRef)))
    case (_, _, _)  => 
      val subject = Subject(spkr)
      if(id == subject.id.toString) new SpeakerStmt(subject.id, Some(subject), Some(Scid(speaksForRef)))
      else throw ParserException(s"id:$id does not match with subject hash, $subject")
  }
}

case class Object(rootId: Id, id: Guid) extends Entity {
  override val scid: Scid = Scid(rootId, id)
}
object Object {
  def apply(rootId: Id): Object = {
    new Object(rootId, Guid())
  }
}

trait Signer {
  def sign(dataToSign: Array[Byte], signatureAlgorithm: String): Array[Byte]
}

trait Decrypter {
  def decrypt(encryptedSharedKey: String, encryptedData: String, encryptedAlgorithm: String): String
}

case class Principal(keyPair: java.security.KeyPair) extends Entity with Signer with Decrypter {

  //==== Java crypto libraries for encryption and decryption ===//
  import java.security.spec._
  import javax.crypto._
  import javax.crypto.spec._


  val subject: Subject = Subject(keyPair.getPublic())
  override val scid: Scid = subject.scid
  override def sign(dataToSign: Array[Byte], signatureAlgorithm: String = "SHA256withRSA"): Array[Byte] = {
    val signer = java.security.Signature.getInstance(signatureAlgorithm)
    signer.initSign(keyPair.getPrivate())
    signer.update(dataToSign)
    signer.sign()
  }


  def decrypt(
      encryptedSharedKey: String
    , encryptedData: String
    , encryptionAlgorithm: String = "AES" // TODO: not used for now
  ): String = {
     val pkCipher  = Cipher.getInstance("RSA")
     val aesCipher = Cipher.getInstance("AES")

     // decrypt AES key with recipient private key
     pkCipher.init(Cipher.DECRYPT_MODE, keyPair.getPrivate())
     val encryptedSharedKeyBase64Decode = Identity.base64Decode(encryptedSharedKey)
     val sharedAESKey: Array[Byte] = pkCipher.doFinal(encryptedSharedKeyBase64Decode)

     val aesKeySpec = new SecretKeySpec(sharedAESKey, "AES")

     // now decrypt data using shared secret key
     aesCipher.init(Cipher.DECRYPT_MODE, aesKeySpec)
     val encryptedDataBase64Decode = Identity.base64Decode(encryptedData)
     val data: Array[Byte] = aesCipher.doFinal(encryptedDataBase64Decode)
    
     val dataStr = new String(data)
     println(s"Data is: $dataStr")
     dataStr
  }

  override def toString(): String = {
    val pubKey = Identity.base64EncodeURLSafe(keyPair.getPublic().getEncoded())
    val signingKey = Identity.base64EncodeURLSafe(keyPair.getPrivate().getEncoded())
    s"['$pubKey', '$signingKey']"
  }
}

object Principal {
  // TODO: write a built-in function for saving this new key pair
  def apply(algorithm: String = "RSA", keyLength: Int = 2048): Principal = {
    val keyPairGenerator = java.security.KeyPairGenerator.getInstance(algorithm)
    keyPairGenerator.initialize(keyLength, Entity.secureRandom)
    new Principal(keyPairGenerator.generateKeyPair())
  }
  def apply(pemFile: String): Principal = {
    import java.io.{File, FileInputStream, FileReader, InputStreamReader}
    import java.security.KeyPair
    import org.bouncycastle.openssl.{PEMKeyPair, PEMParser}
    import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter

    val parser: PEMParser = try {
      val file = this.getClass().getClassLoader().getResourceAsStream(pemFile)
      val pFile = if(file == null) { // if absolute path is provided
        new FileInputStream(pemFile)
      } else file
      new PEMParser(new InputStreamReader(pFile))
    } catch {
      case e: java.io.FileNotFoundException => 
        val fn = new java.io.File(pemFile)
        throw ParserException("The path I read is: " + fn.getCanonicalPath() + ", canRead=" + fn.canRead() + ", exists=" + fn.exists())
      case e: java.io.IOException => throw ParserException(s"Error reading file: $pemFile; error msg: $e")
      case e: NullPointerException => throw ParserException(s"Error reading file: $pemFile; error msg: $e")
      case _: Throwable => throw ParserException(s"Error reading file: $pemFile")
    } 
    val pemKeyPair: PEMKeyPair = parser.readObject().asInstanceOf[PEMKeyPair]
    val keyPair: KeyPair = new JcaPEMKeyConverter().getKeyPair(pemKeyPair)
    new Principal(keyPair)
  }
}

object Entity {

  //Initialize SecureRandom; this is a lengthy operation (100's of ms) and to be done only upon initialization of the application
  val secureRandom = java.security.SecureRandom.getInstance("SHA1PRNG") // == new java.security.SecureRandom()
  //val secureRandom = new scala.util.Random(secureRandom)

  // guid is 128 bits
  //def guid(algorithm: String = "MD5", encoding: String = "base64URLSafe"): Guid = Guid(Identity.hash(secureRandom.nextInt.toString, algorithm))

  /** return a random long modulo a number */
  def randomLong(mod: Long): Long = math.abs(secureRandom.nextLong) % mod

  /** return a random int modulo a number */
  def randomInt(mod: Int): Int = math.abs(secureRandom.nextInt) % mod

  /** return a random long modulo a number */
  def randomBigInt(mod: Int): java.math.BigInteger = new java.math.BigInteger(mod, secureRandom)

}
