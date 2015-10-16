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

import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.{TreeMap, TreeSet}

/**
 * Basic data structure in logic: a term
 *
 * toString() converts a term to a readable string.
 */
abstract class Term /*extends Marshaler[Term]*/ { // Not sealed as we would expand further from slang

  /** Each term is identified by a unique id */
  def id: StrLit

  /** Optional attribute name on each term */
  def attrName: StrLit

  /** Optional type on each term */
  def tpe: StrLit

  /** Optional index specifier on each term along with encoding*/
  def indexAndEncode: Encoding

  /** Term's arity */
  def arity: Int = 0

  def eval(): NumericConstant = 0

  def range(): TreeSet[NumericConstant] = TreeSet.empty

  /**
  0: no index/index
  1: Literal
  2: Hex
  3: Base64
  Total 4 bits: 16 values
  */

  override def toString(): String = {
    def encodingStr(id: String, enc: Encoding, annotate: Boolean = false): String = (enc, annotate) match {
      case  (Encoding.Attr, true)               => s"$id"                            // no index
      case  (Encoding.IndexAttr, true)          => s"$id"                            // indexable
      case  (Encoding.AttrLiteral, true)        => s"'$id'"                          // Literal + no index
      case  (Encoding.IndexAttrLiteral, true)   => s"'$id'"                          // Literal + indexable
      case  (Encoding.AttrHex, true)            => s"h'${id.toLong.toHexString}'"    // Hex + no index
      case  (Encoding.IndexAttrHex, true)       => s"h'${id.toLong.toHexString}'"    // Hex + indexable
      case  (Encoding.AttrBase64, true)         => s"u'$id'"                         // Base64 + no index
      case  (Encoding.IndexAttrBase64, true)    => s"u'$id'"                         // Base64 + indexable
      case  (Encoding.Attr, false)              => s"-> $id"                         // no index
      case  (Encoding.IndexAttr, false)         => s"->> $id"                        // indexable
      case  (Encoding.AttrLiteral, false)       => s"-> '$id'"                       // Literal + no index
      case  (Encoding.IndexAttrLiteral, false)  => s"->> '$id'"                      // Literal + indexable
      case  (Encoding.AttrHex, false)           => s"-> h'${id.toLong.toHexString}'" // Hex + no index
      case  (Encoding.IndexAttrHex, false)      => s"->> h'${id.toLong.toHexString}'"// Hex + indexable
      case  (Encoding.AttrBase64, false)        => s"-> u'$id'"                      // Base64 + no index
      case  (Encoding.IndexAttrBase64, false)   => s"->> u'$id'"                     // Base64 + indexable
      case  _                                   => throw ParserException(s"Unrecognized encoding found: $enc")
    }

    val idName = if(id.name.startsWith("^")) { // regex variable
      s"""r"${id.name.substring(1)}""""
    } else {
      id.name
    }

    if(tpe == termType && attrName == StrLit("nil")) s"${encodingStr(idName, indexAndEncode, true)}"
    else if((attrName == StrLit("nil"))) s"#${tpe.name} ${encodingStr(idName, indexAndEncode)}"
    else if((tpe == termType)) s"${attrName.name} ${encodingStr(idName, indexAndEncode)}"
    else s"${attrName.name} #${tpe.name} ${encodingStr(idName, indexAndEncode)}"
  }

  def toStringWithSays(): String                = toString()
  def toStringCompact(speaker: String): String  = toString()

  /* get the set of (unbounded) variables in this term if present; useful for range restriction check for safety*/
  def unboundVariables: Set[Term] = Set.empty

  /* tests whether all the variables are bound */
  def isGround: Boolean = if(unboundVariables.isEmpty) true else false

  /** bind constructs a fresh term; useful for creating new variable instances */
  def bind(f: StrLit => Term): Term = this

  /** Index for efficient search
   *
   * This can be pushed to Statement level, but precomputing here is useful
   * for indexing query terms; otherwise the index should be built on-demand 
   * during inference
   */
  def primaryIndex(): StrLit   = StrLit(id.name + arity) // index on principal functor and arity and first argument may be?
  //def ternaryIndex(): StrLit   = StrLit("")                         // index on second argument

  //def marshall(value: A, out: DataOutput): Unit = Term
  //def unmarshall(numBytes: Int, in: DataInput): A
}

trait ConstantLike extends Term {
  override def eval() = tpe match {
    case StrLit("Byte")            => id.name.toByte
    case StrLit("Short")           => id.name.toShort
    case StrLit("Int")             => id.name.toInt
    case StrLit("Long")            => id.name.toLong
    case StrLit("Float")           => id.name.toFloat
    case StrLit("Double")          => id.name.toDouble
    case StrLit("NumericConstant") => toNumericConstant(id)
    case _                => throw ParserException(s"Cannot perform numeric operations on given input: $id: $tpe")
  }
}

trait VariableLike extends Term {
  override def bind(f: StrLit => Term): Term = f(id)
  override val unboundVariables: Set[Term] = if(isEnvVariable()) Set.empty else Set(this) // ignore env variables
  def isEnvVariable(): Boolean = if(id.name.startsWith("$")) true else false
  override def primaryIndex(): StrLit = StrLit("nil")
  val simpleName: StrLit = if(id.name.startsWith("$") | id.name.startsWith("?")) { // but not _
    Term.stripDepth(id.name.substring(1))
  } else {
    Term.stripDepth(id.name)
  }
}

trait StructureLike extends Term {
  val terms: Seq[Term]
  override val arity: Int = terms.length
  override def eval(): NumericConstant = (id, terms) match {
    case (StrLit("_plus"), head +: tail)  => tail.foldLeft(head.eval)((l, r)  => l + r.eval)
    case (StrLit("_minus"), head +: tail) => tail.foldLeft(head.eval)((l, r)  => l - r.eval)
    case (StrLit("_times"), head +: tail) => tail.foldLeft(head.eval)((l, r)  => l * r.eval)
    case (StrLit("_div"), head +: tail)   => tail.foldLeft(head.eval)((l, r)  => l / r.eval)
    case (StrLit("_rem"), head +: tail)   => tail.foldLeft(head.eval)((l, r)  => l % r.eval)
    case (StrLit("_max"), head +: tail)   => tail.foldLeft(head.eval)((l, r)  => scala.math.max(l, r.eval))
    case (StrLit("_min"), head +: tail)   => tail.foldLeft(head.eval)((l, r)  => scala.math.min(l, r.eval))
    case _ => sys.error(s"Unsupported arthimetic operator: $id")
  }
  override val unboundVariables: Set[Term] = terms.collect {
    case t @ Variable(id, _, _, _) if !id.name.startsWith("$") => Seq(t) // ignore env variables
    case t @ Structure(id, xterms, _, _, _) => t.unboundVariables.toSeq
  }.flatten.toSet

  override def range(): TreeSet[NumericConstant] = (id, terms) match {
    case (StrLit("_range"), xterms) => collection.immutable.TreeSet[NumericConstant]() ++ xterms.map(_.eval)
    case _ => throw NotImplementedException("Not yet implemented")
  }
  // if a term has natural ordering, compare() is helpful
  def compare(): Boolean = (id, terms) match {
    case (StrLit("_unify"), head +: tail +: Nil)    => head.eval  == tail.eval
    case (StrLit("_lt"), head +: tail +: Nil)       => head.eval  <= tail.eval
    case (StrLit("_lteq"), head +: tail +: Nil)     => head.eval  <  tail.eval
    case (StrLit("_gt"), head +: tail +: Nil)       => head.eval  >  tail.eval
    case (StrLit("_gteq"), head +: tail +: Nil)     => head.eval  >= tail.eval
    //case (StrLit("_subset"), head +: tail +: Nil)   => head.toSet subsetOf tail.toSet
    case (StrLit("_in"), head +: tail +: Nil)       => tail.range contains head.eval
    case _ => throw UnSupportedOperatorException(s"Unsupported comparison operater: $id")
  }
  val infixOps: Map[StrLit, String] = Map(
      StrLit("_compare") -> "=:="
    , StrLit("_is")      -> ":="
    , StrLit("_not")     -> "!"
    , StrLit("_unify")   -> "="
    , StrLit("_subset")  -> "<:<"
    , StrLit("_in")      -> "<:"
    , StrLit("_lt")      -> "<"
    , StrLit("_lteq")    -> "<="
    , StrLit("_gt")      -> ">"
    , StrLit("_gteq")    -> ">="
    , StrLit("_at")      -> "@"
  )
  def getIdWithType(): String = {
    if((tpe == termType) && (attrName == StrLit("nil"))) id.name 
    else if(attrName == StrLit("nil")) s"#${tpe.name} -> ${id.name}"
    else if(tpe == termType) s"${attrName.name} -> ${id.name}"
    else s"${attrName.name} #${tpe.name} -> ${id.name}"
  }
  override def toString(): String =  (Config.config.saysOperator, id, terms) match {

     case (_, StrLit("_seq"), xterms) if (tpe == StrLit("Path")) =>
     if(xterms.head.id.name == "./") s"""path"${xterms.tail.mkString("/")}""""
     if(xterms.head.id.name == "/") s"""path"${xterms.head + xterms.tail.mkString("/")}""""
     else s"""path"${xterms.mkString("/")}""""
    case (_, StrLit("_seq"), xterms) if (tpe == StrLit("Dn")) =>
     if(xterms.head.id.name == ".") s"""dn"${xterms.tail.reverse.mkString(".")}""""
     else s"""dn"${xterms.reverse.mkString(".")}""""

    case (_, StrLit("_seq"), Nil) =>
      "[]"
    //case (_, StrLit("_seq"), xterms) if (xterms.last == Structure(StrLit("_seq"), Nil)) =>
     // "[" + xterms.init.mkString(", ") + "]"                  //enumeratedList
    //case (_, StrLit("_seq"), xterms) if (xterms.last == Variable(xterms.last.toString()) || xterms.last == Constant(xterms.last.toString())) =>
    case (_, StrLit("_seq"), xterms) if (xterms.last == Variable(xterms.last.toString())) =>
      "[" + xterms.init.mkString(", ") + " | " + xterms.last + "]" //pipedList
    case (_, StrLit("_seq"), xterms) =>
      "[" + xterms.mkString(", ") + "]"                  //enumeratedList

    case (_, StrLit("_set"), xterms) =>
      "#{" + xterms.mkString(", ") + "}"                 //enumerated set

    // == support for lists in slang End  ==/
    case (true, StrLit("_interpolate"), speaker +: termStr +: other)             =>
      speaker + ": " + s""""$termStr""""
    case (false, StrLit("_interpolate"), termStr +: other)                       => // TODO: since interpolate is 2nd level predicate toStringCompact calls toString()
      s""""$termStr""""
    case (true, StrLit("_fact"), speaker +: Constant(value, _, _, _) +: Nil)     =>
      speaker + ": " + value.name
    case (false, StrLit("_fact"), Constant(value, _, _, _) +: Nil)               =>
      value.name
    case (_, x, left +: right +: Nil) if infixOps.contains(x)           =>
      left.toString + " " +  infixOps.get(x).get + " " + right.toString
    case (true, _, speaker +: tail)                                     =>
      speaker + ": " + getIdWithType() + "(" + tail.mkString(", ") + ")"
    case (false, x, _)                                                  =>
      getIdWithType() + "(" + terms.mkString(", ") + ")"
  }
  override def toStringWithSays(): String = (id, terms) match {
    case (StrLit("_interpolate"), speaker +: termStr +: other)             =>
      speaker + ": " + s""""$termStr""""
    case (StrLit("_fact"), speaker +: Constant(value, _, _, _) +: Nil)     => 
      speaker + ": " + value.name
    case (x, left +: right +: Nil) if infixOps.contains(x)        => 
      left.toStringWithSays + " " +  infixOps.get(x).get + " " + right.toStringWithSays
    case (_, speaker +: tail)                                     => 
      speaker + ": " + getIdWithType() + "(" + tail.mkString(", ") + ")"
  }
  override def toStringCompact(speaker: String): String = (id, terms) match {
    case (StrLit("_interpolate"), subject +: termStr +: other)  if (
      (subject.id.name == speaker) | (subject.id.name == "$Self")
    ) => s"""$termStr""""
    case (StrLit("_fact"), subject +: Constant(value, _, _, _) +: Nil) if subject.id.name == speaker   => 
      value.name
    case (StrLit("_fact"), subject +: Constant(value, _, _, _) +: Nil) if subject.id.name == "$Self"   => 
      value.name
    case (x, left +: right +: Nil) if infixOps.contains(x) => 
      left.toStringCompact(speaker) + " " +  infixOps.get(x).get + " " + right.toStringCompact(speaker)
    case (_, subject +: tail) if subject.id.name == speaker     => 
      getIdWithType() + "(" + tail.mkString(", ") + ")"
    case (_, subject +: tail) if subject.id.name == "$Self"     => 
      getIdWithType() + "(" + tail.mkString(", ") + ")"
    case (_, subject +: Nil) if subject.id.name == speaker      => 
      getIdWithType() + "()"
    case (_, subject +: Nil) if subject.id.name == "$Self"      => 
      getIdWithType() + "()"
    case (StrLit("_fact"), subject +: Constant(value, _, _, _) +: Nil)              => 
      subject + ": " + value.name
    case (_, subject +: tail)                                              => 
      subject + ": " + getIdWithType() + "(" + tail.mkString(", ") + ")"
  }

  /*
  override def primaryIndex(): StrLit   = (id, terms) match {
    case (_, Constant(c, _, _, _) +: other) if Config.config.reserved.contains(id) => 
      StrLit(id.name) // index on principal functor and arity
    case (_, Variable(StrLit("$Self"), _, _, _) +: other) if Config.config.reserved.contains(id) => 
      StrLit(id.name)  // index on principal functor and arity
    case _                         => StrLit(id.name + arity) // index on principal functor and arity
  }
  */
}

trait NegatedTermLike extends Term {
  val term: Term
  override def toString(): String = id + "(" + term + ")"
  override val unboundVariables: Set[Term] = term.unboundVariables
}

case class Constant(
    id: StrLit
  , attrName: StrLit = termIndex
  , tpe: StrLit = termType
  , indexAndEncode: Encoding = Encoding.Attr
) extends Term with ConstantLike

object Constant {
  def apply(id: String): Constant = new Constant(StrLit(id), termIndex, termType)
  def apply(id: String, attrName: StrLit, tpe: StrLit): Constant = new Constant(StrLit(id), attrName, tpe)
  /*
  def apply(
      id: String
    , attrName: StrLit
    , tpe: StrLit
    , indexAndEncode: Encoding
  ): Constant = new Constant(StrLit(id), attrName, tpe, indexAndEncode)
  */
}

case class Variable(
    id: StrLit
  , attrName: StrLit = termIndex
  , tpe: StrLit = termType
  , indexAndEncode: Encoding = Encoding.Attr
) extends Term with VariableLike

object Variable {
  def apply(id: String): Variable = new Variable(StrLit(id), termIndex, termType)
  def apply(id: String, attrName: StrLit, tpe: StrLit): Variable = new Variable(StrLit(id), attrName, tpe)
  /*
  def apply(
      id: String
    , attrName: StrLit
    , tpe: StrLit
    , indexAndEncode: Encoding
  ): Variable = new Variable(StrLit(id), attrName, tpe, indexAndEncode)
  */
}

case class Structure(
    id: StrLit
  , terms: Seq[Term]
  , attrName: StrLit = termIndex
  , tpe: StrLit = termType
  , indexAndEncode: Encoding = Encoding.Attr
) extends Term with StructureLike {
  override def bind(f: StrLit => Term): Term = (id, terms) match {
    case (StrLit("_interpolate"), Constant(body, attrName, tpe, enc) +: Constant(termSeq, _, _, _) +: xterms) =>
      val bindedTerms = xterms.map(_.bind(f))

      val globalVariablesGround: Boolean = {
        val boolSeq = bindedTerms collect {
          case c: Constant                               => true
          case v: Variable if(v.id.name.startsWith("?")) => true
          case v: Variable if(v.id.name.startsWith("$")) => false
        }
        if(boolSeq.contains(false)) false else true
      }
      if(!globalVariablesGround) this.copy(terms = terms.map(_.bind(f)))
      else {
        val bindedStr: String = Term.interpolate(body.name, termSeq.name, bindedTerms, isPost = true)
        //val res         = Constant(bindedStr, attrName, tpe, Encoding.AttrLiteral) // bindedStr may contain local variables --- hence, leave the string with double quotes for possible another interpolation during inference
        val localVariablesGround: Boolean = {
	  val boolSeq = bindedTerms collect {
	    case v: Variable if(v.id.name.startsWith("?")) => false
	    case _                                         => true
	  }
	  if(boolSeq.contains(false)) false else true
        }
        val res = if(!localVariablesGround) {
          this.copy(
            terms = Constant(StrLit(bindedStr), attrName, tpe, enc) +: Constant(bindedTerms.mkString(",")) +: bindedTerms
          )
        } else {
          Constant(StrLit(bindedStr), attrName, StrLit("StrLit"), Encoding.AttrLiteral)
        }
        res
      }
    case _   => this.copy(terms = terms.map(_.bind(f)))
  }
}

object Structure {
  def apply(id: String, terms: Seq[Term]): Structure = new Structure(StrLit(id), terms, termIndex, termType)
  def apply(id: String, terms: Seq[Term], attrName: StrLit, tpe: StrLit): Structure = new Structure(StrLit(id), terms, attrName, tpe)
  /*
  def apply(
      id: String
    , terms: Seq[Term]
    , attrName: StrLit
    , tpe: StrLit
    , indexAndEncode: Encoding
  ): Structure = new Structure(StrLit(id), terms, attrName, tpe, indexAndEncode)
  */
}

case class NegatedTerm(
    id: StrLit
  , term: Term
  , attrName: StrLit = termIndex
  , tpe: StrLit = termType
  , indexAndEncode: Encoding = Encoding.Attr
) extends Term with NegatedTermLike {
  override def bind(f: StrLit => Term): Term = this.copy(term = term.bind(f))
}

object NegatedTerm {
  def apply(id: String, term: Term): NegatedTerm = new NegatedTerm(StrLit(id), term)
  def apply(id: String, term: Term, attrName: StrLit, tpe: StrLit): NegatedTerm = new NegatedTerm(StrLit(id), term, attrName, tpe)
  /*
  def apply(
      id: String
    , term: Term
    , attrName: StrLit
    , tpe: StrLit
    , indexAndEncode: Encoding
  ): NegatedTerm = new NegatedTerm(StrLit(id), term, attrName, tpe, indexAndEncode)
  */
}

trait TermLike { // For reusing at higher layer (slang)

  /* A substitue function that replaces a variable (varName) with a value (term)
   * on the given term applyto.
   * Example: applyto := hello(X).
   * Fact: hello(world).
   * Invoking subst:
   *   subst(varName: X, term: world)(hello(X)): hello(world)
   */
  def subst(varName: StrLit, term: Term)(applyto: Term): Term = {
    applyto.bind(n => if (n == varName) term else Variable(n))
  }

  // find if the lhs term subsumes rhs term, i.e, whether rhs term is an instance of lhs term
  def subsumes(lhs: Term, rhs: Term): Boolean = {
    @annotation.tailrec
    def recurse(lhs: Seq[Term], rhs: Seq[Term]): Boolean = (lhs, rhs) match {
      case (Nil, Nil) => true
      case (Variable(id, _, _, _) +: tail1, term +: tail2) =>
        recurse(tail1, tail2)
      case (Structure(id1, term1, _, _, _) +: tail1, Structure(id2, term2, _, _, _) +: tail2) =>
	if (lhs.head.arity == rhs.head.arity && lhs.head.id == rhs.head.id) {
          recurse(term1 ++: tail1, term2 ++: tail2)
        } else false
      case (_, _) => false
    }
    recurse(Seq(lhs), Seq(rhs))
  }

  @inline
  private[safe] def mguRegex(id: StrLit, term: Term, lhs: Seq[Term], rhs: Seq[Term], unifier: Term => Term): Option[Term => Term] = {
    val underlying = s"""${id.name.substring(1)}""".r
    if(underlying.pattern.matcher(term.id.name).matches)  {
      val r = subst(id, term)_ compose unifier
      mguSeq(lhs.map(r), rhs.map(r), r)
    }
    else None
  }

  @annotation.tailrec
  private[safe] final def mguSeq(lhs: Seq[Term], rhs: Seq[Term], unifier: Term => Term): Option[Term => Term] = (lhs, rhs) match {

    // empty lists? no more work
    case (Nil, Nil) => Some(unifier)

    // occurs check is only necessary at slang 

    // anything unifies with a variable
    case (term +: tail1, Variable(id, attrName, tpe, indexAndEncode) +: tail2) if(
      (id.name != "Self" & tpe == termType & attrName == termIndex & indexAndEncode == Encoding.Attr) |
      (id.name != "Self" & term.attrName == attrName & term.tpe == tpe & term.indexAndEncode == indexAndEncode)
    ) =>

      if(id.name.startsWith("^")) { // regex
        mguRegex(id, term, tail1, tail2, unifier)
      } else {
        val r = subst(id, term)_ compose unifier // given two partial functions, f and g, compose will return g(f(x))
        mguSeq(tail1.map(r), tail2.map(r), r)
      }

    case (Variable(id, attrName, tpe, indexAndEncode) +: tail1, term +: tail2) if(
      (id.name != "Self" & tpe == termType & attrName == termIndex & indexAndEncode == Encoding.Attr) |
      (id.name != "Self" & term.attrName == attrName & term.tpe == tpe & term.indexAndEncode == indexAndEncode)
    ) =>

      if(id.name.startsWith("^")) { // regex
        mguRegex(id, term, tail1, tail2, unifier)
      } else {
        val r = subst(id, term)_ compose unifier // given two partial functions, f and g, compose will return g(f(x))
        mguSeq(tail1.map(r), tail2.map(r), r)
    }
   // constants must match
    case (Constant(id1, attrName1, tpe1, ie1) +: tail1, Constant(id2, attrName2, tpe2, ie2) +: tail2) if(
      attrName1 == attrName2 & tpe1 == tpe2 & ie1 == ie2
    ) =>
      if (id1 == id2) mguSeq(tail1, tail2, unifier) else None

    // structures must have matching atoms and matching arity
    // then arguments can be added to the list of values to check
    case ((s1 @ Structure(id1, term1, attrName1, tpe1, ie1)) +: tail1, (s2 @ Structure(id2, term2, attrName2, tpe2, ie2)) +: tail2) if(
      attrName1 == attrName2 & tpe1 == tpe2 & ie1 == ie2
    ) =>
      if (s1.arity == s2.arity && id1 == id2) {
        id1 match {
          case StrLit("_is") | StrLit("_unify") =>
            mguSeq(Seq(term1.head), term1.tail, unifier) // special case for is and =
          case _ => mguSeq(term1 ++: tail1, term2 ++: tail2, unifier)
        }
      }
      else None

    // anything else cannot be unified
    case (_, _) => None
  }

  // find the most general unifier for two terms
  private[safe] def mostGenericUnifier(lhs: Term, rhs: Term): Option[Term => Term] = {
    mguSeq(Seq(lhs), Seq(rhs), x => x)
  }

  def isGrounded(term: Term): Boolean = term.isGround

  /*
  def isGrounded(term: Term): Boolean = term match {
    case Constant(x) => true
    case Variable(x) => false
    case Structure(id, terms) =>
      terms.foreach(x => isGrounded(x) match  {
        case false => return false
	case _ => 
      })
      true
    case _ => false
  }
  */

  import java.nio.{ByteBuffer, ByteOrder}
  import scala.collection.mutable.ListBuffer
  //import scala.reflect.runtime.universe._ // for quasi-quotes

/*
  //val byteBuffer: ByteBuffer = ByteBuffer.allocateDirect(1024).order(ByteOrder.nativeOrder())
  //marshall(Constant('Test))

  def marshall(term: Term, encoding: String = "binary"): Array[Byte] = {

    val a = Structure('test, Constant(StrLit("1")) +: Variable(StrLit("Ya")) +: Constant(StrLit("a")) +: Constant(StrLit("3")) +: Nil)
    //val a = Structure('a, Nil)
    //val a = Constant('a)

    val w = write(a, byteBuffer)
    println(byteBuffer)
    println(byteBuffer.toString)
    byteBuffer.rewind()
    //val bytesToStore: Array[Byte] = new Array[Byte](40)
    //byteBuffer.get(bytesToStore)
    //val t = Constant(new String(bytesToStore, StringEncoding))
    //println(s"T: $t")
    val r = read(byteBuffer)
    println(r)
    val res: Array[Byte] = Array(1)
    res
  }
*/

  @inline
  private def writeStrLit(id: StrLit, tpe: StrLit, byteBuffer: ByteBuffer): Unit = {
    val bytes = id.name.getBytes(StringEncoding)
    val bytesLength = bytes.length
    val bytesLengthOpt: Char = if(bytesLength < MaxSymbolSize) bytesLength.toChar else throw new UnSafeException("MaxStrLitSize exceeded")
    byteBuffer.putChar(bytesLengthOpt)
    byteBuffer.put(bytes)
    if(tpe != termType) {
      val tpeBytes = tpe.name.getBytes(StringEncoding)
      val tpeBytesLength = tpeBytes.length
      val tpeBytesLengthOpt: Char = if(tpeBytesLength < MaxSymbolSize) tpeBytesLength.toChar else throw new UnSafeException("MaxStrLitSize exceeded")
      byteBuffer.putChar(tpeBytesLengthOpt)
      byteBuffer.put(tpeBytes)
    }
  }

  @inline
  private def readStrLit(byteBuffer: ByteBuffer): StrLit = {
    val bufferSize = byteBuffer.getChar()
    val bytesToStore: Array[Byte] = new Array[Byte](bufferSize)
    byteBuffer.get(bytesToStore)
    StrLit(new String(bytesToStore, StringEncoding))
  }

  private[safelog] def write(term: Term, byteBuffer: ByteBuffer): Unit = term match {
    case c @ Constant(id, _, _, _) =>
      if(c.tpe == termType) byteBuffer.put(SConstant) else byteBuffer.put(setBit(SConstant))
      writeStrLit(id, c.tpe, byteBuffer)
    case v @ Variable(id, _, _, _) =>
      if(v.tpe == termType) byteBuffer.put(SVariable) else byteBuffer.put(setBit(SVariable))
      writeStrLit(id, v.tpe, byteBuffer)
    case s @ Structure(id, xterms, _, _, _) =>
      if(s.tpe == termType) byteBuffer.put(SStructure) else byteBuffer.put(setBit(SStructure))
      writeStrLit(id, s.tpe, byteBuffer)
      xterms.foreach{xterm => write(xterm, byteBuffer)}
      byteBuffer.put(SNil) // Nil
    case n @ NegatedTerm(id, term, _, _, _) =>
      byteBuffer.put(SNegatedTerm)
      write(term, byteBuffer)
    case _ => throw new UnSafeException("Unknown typed passed") 
  }

  def getBit(num: Byte, pos: Int = 7): Boolean = ((num & (1 << pos)) != 0)
  def setBit(num: Byte, pos: Int = 7): Byte    =  (num | (1 << pos)).toByte
  def getType(num: Byte, pos: Int = 7): Byte   =  (num & 0xff & 0x7f).toByte // num & 0xff will range from 0-255 and then take the least significant 7 bits

  private[safelog] def read(byteBuffer: ByteBuffer): Term = {
    val typeField: Byte = byteBuffer.get()
    val tpe: Boolean    = getBit(typeField) 
    println(s"field: $typeField")
    val out = getType(typeField)
    println(s"typeField: $out")
    getType(typeField) match {
      case SNil       => Constant(StrLit("nil"))
      case SConstant  => if(tpe) new Constant(readStrLit(byteBuffer), StrLit("nil"), readStrLit(byteBuffer)) else Constant(readStrLit(byteBuffer)) // TODO: XXX indexAndEncode not implemented
      case SVariable  => if(tpe) new Variable(readStrLit(byteBuffer), StrLit("nil"), readStrLit(byteBuffer)) else Variable(readStrLit(byteBuffer)) // TODO: XXX indexAndEncode not implemented
      case SStructure =>
	val id: StrLit = readStrLit(byteBuffer)
	@annotation.tailrec
	@inline
	def recurse(byteBuffer: ByteBuffer, terms: ListBuffer[Term] = ListBuffer()): ListBuffer[Term] = {
	  read(byteBuffer) match {
	    case Constant(StrLit("nil"), _, _, _) => terms
	    case term           => 
	      terms += term
	      recurse(byteBuffer, terms)
	  }
	}
	val terms = recurse(byteBuffer)
	//if(tpe) Structure(id, terms)(readStrLit(byteBuffer)) else Structure(id, terms)
        Structure(id, terms)
      case SNegatedTerm =>
	val id: StrLit     = readStrLit(byteBuffer)
	val term: Term     = read(byteBuffer)
	NegatedTerm(id, term)
      case x => throw new UnSafeException(s"Unknown typed passed: $x, $typeField")
    }
  }

  def dropStatement(stmt: Statement, program: ConcurrentHashMap[Index, Set[Statement]]): Unit = {
    val orgStatements: Set[Statement] = program.get(stmt.primaryIndex)
    val stmts = orgStatements - stmt
    if(stmts.isEmpty) { // if statement set is empty, drop the index
      program.remove(stmt.primaryIndex)
    }
  }

  def stripQuotes(name: String): String = name.replaceAll("\"(.*)\"$","$1") // strip double quotes if any
			                      .replaceAll("'(.*)'$","$1")   // strip single quotes if any

  def numberFromString[T: Numeric: ReadNumeric](str: String) = implicitly[ReadNumeric[T]].readNumeric(str)
}

object Term extends TermLike {

  /** Interpolate a string by substituting the local variables */
  def interpolate(body: String, termSeq: String, argValues: Seq[Term], isPost: Boolean = false): String = {
    val args: Array[String] = termSeq.split(",")
    var _mutableStr: String = body
    args.zipWithIndex foreach {
      case (arg: String, idx: Int) if argValues.isDefinedAt(idx) =>
        val substValue: String = argValues(idx) match {
          case c: Constant                                         => c.id.name
          case v: Variable if (isPost & v.id.name.startsWith("?")) => v.id.name
          case s: Structure if (s.id.name == "_seq")               => Term.normalizeTerms(s +: Nil).head.toString
          case s @ Structure(StrLit("_interpolate"), Constant(body, _, _, _) +: Constant(termSeq, _, _, _) +: xterms, _, _, _) =>
            //interpolate(body.name, termSeq.name, xterms)(envContext)
            interpolate(body.name, termSeq.name, xterms)
          case s: Structure  => s.toString // useful at slang level
          case other         => throw UnSafeException(s"Unbound variables found in interpolation: $other; $argValues")
        }
       _mutableStr = _mutableStr.replace(s"$arg", substValue) // TODO: this is simpler than string interpolation albeit expensive
       //_mutableStr = s"\\$arg[,;]?(\\b)?".r.replaceAllIn(_mutableStr, substValue) // TODO: this is simpler than string interpolation albeit expensive
       _mutableStr = _mutableStr.replace(s"?(${arg.substring(1)})", substValue) // TODO: this is simpler than string interpolation albeit expensive
       //val variableRegex = s"""([\$$\?])(?:\()?${arg.substring(1)}(?:\))?""".r
       //_mutableStr = variableRegex.replaceAllIn(_mutableStr, substValue) // TODO: this is simpler than string interpolation albeit expensive
      case (arg: String, idx: Int) => throw UnSafeException(s"Unbound variables found in interpolation: $arg")
    }
    _mutableStr.toString
  }

  /** These functions exists to purely support slang lists */
  /* listify: converts a canonical list to its standard form
   * Examples: .(a,.(b,[])) == [a,b]. 
   *           .(.(.(a,[]),[]),[]) == [[[a]]].
   *           .(.(a,.(b,[])),.(c,[])) == [[a,b],c].
   *             .(.(a,[]),.(.(b,.(c,[])),[])) == [[a],[b,c]].
   *           .(a,.(b,.(f(d,e),[]))) == [a,b,f(d,e)].
   */
  private def listify(terms: Seq[Term]): Seq[Term] = {
    //@annotation.tailrec
    def loop(terms: Seq[Term]): Seq[Term] = terms match {
      case Structure(StrLit("_seq"), subTerms, _, _, _) +: rest => subTerms match {
        case Structure(StrLit("_seq"), moreSubTerms, _, _, _) +: tail => listify(Seq(subTerms.head)) ++: loop(subTerms.tail) ++: loop(rest)
        case _ => loop(subTerms) ++: loop(rest)
      }
      case Structure(name, subTerms, _, _, _) +: rest => Structure(name, loop(subTerms)) +: loop(rest) // if name, apply the predicate
      case head +: rest => head +: loop(rest) // else append the head to the list and loop
      case Nil => Nil
    }
    Seq(Structure(StrLit("_seq"), loop(terms)))
  }

  /* 
   * normalizeTerms: build the term tokens from the given statement (Structure, Constant, List)
   */
  def normalizeTerms(stmt: Seq[Term]): Seq[Term] = stmt match {
    case term +: rest => term match {
      case Structure(StrLit("_seq"), _, _, _, _)            => listify(Seq(term)) ++: normalizeTerms(rest) // is it a list?
      case Structure(StrLit("_interpolate"), _, _, _, _)    => Seq(term) ++: normalizeTerms(rest) // is it a interploation?
      case Structure(name, terms, _, _, _)         => Structure(name, normalizeTerms(terms)) +: normalizeTerms(rest) // is it a predicate name?
      case _ => Seq(term) ++: normalizeTerms(rest) // then simply a Constant
    }
    case Nil => Nil
  }

  // useful for env variables
  def stripDepth(variable: String): StrLit = {
    StrLit("(_\\d+)$".r.replaceAllIn(variable, ""))
  }
}

object ReadNumeric {
  implicit object ReadInt extends ReadNumeric[Int] { def readNumeric(string: String) = string.toInt }
  implicit object ReadDouble extends ReadNumeric[Double] { def readNumeric(string: String) = string.toDouble }
  implicit object ReadFloat extends ReadNumeric[Float] { def readNumeric(string: String) = string.toFloat }
}
trait ReadNumeric[A] {
  def readNumeric(string: String): A
}
