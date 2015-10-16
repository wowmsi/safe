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



package safe.runtime

import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile}
import scala.reflect.internal.util.{BatchSourceFile}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.io.{VirtualDirectory, AbstractFile}

case class ScalaInterpreter(targetDir: Option[java.io.File]) extends JVMInterpreter with com.typesafe.scalalogging.LazyLogging {
  override val name: String = "scala"

  val target = targetDir match {
    case Some(dir) => AbstractFile.getDirectory(dir)
    case None => new VirtualDirectory("(memory)", None)
  }

  val classCache = scala.collection.mutable.Map[String, Class[_]]()

  private val settings = new Settings()
  settings.deprecation.value = true   // enable detailed deprecation warnings
  settings.unchecked.value = true     // enable detailed unchecked warnings
  settings.usejavacp.value = true     // needed when fork is used with sbt
  settings.outputDirs.setSingleOutput(target)

 /**
  * see: [[http://lampsvn.epfl.ch/svn-repos/scala/scala-experimental/trunk/bootstrap/src/compiler/scala/tools/nsc/Global.scala]]
  * -Ystop-after:refchecks // reference/override checking, translate nested objects; checks whether file would compile if you compile it
  * -Ystop-after:parser   // syntactic correctness 
  */
  //settings.stopAfter.value = List("parser")
  //settings.stopAfter.value = List("refchecks")
  //settings.usejavacp.value = true // outside sbt

  // some value for type should be passed due to the limitation of sbt/scala classpath
  settings.embeddedDefaults[ScalaInterpreter.type]

  private val global = new Global(settings)

  val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)

  /** Check for syntactic correctness 
   */
  private def parse(code: String, args: Array[String], className: Option[String]) = {
    val clsName = className match {
      case Some(x) => x
      case None => classNameForCode(code, args.length)
    }
    val sourceFiles = List(new BatchSourceFile("(inline)", wrapCodeInClass(clsName, code, args)))
    logger.debug("WrappedCode: " + wrapCodeInClass(clsName, code, args))
    lazy val run = new global.Run
    run.compileSources(sourceFiles)
  }

  /**
   * Compiles the code as a class into the class loader of this compiler.
   *
   * @param code
   * @return
   */
  def compile(code: String, args: Array[String]): Class[_] = {
    val className = classNameForCode(code, args.length)
    findClass(className).getOrElse {
      parse(code, args, Some(className))
      findClass(className).get
    }
  }

  private def findClass(className: String): Option[Class[_]] = {
    synchronized {
      classCache.get(className).orElse {
        try {
          val cls = classLoader.loadClass(className)
          classCache(className) = cls
          Some(cls)
        } catch {
          case e: ClassNotFoundException => None
        }
      }
    }
  }

  private def classNameForCode(code: String, argc: Int): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-1").digest(code.getBytes) 
    "sha" + new java.math.BigInteger(1, digest).toString(16) + argc
  }

  private def wrapCodeInClass2(className: String, code: String, argNames: Array[String]): String = {
    /* 
     * val X = args(0)
     * val Y = args(1)
     */
    val argMacro = for(i <- 0 until argNames.length) yield (
      s"""
          private String ${argNames(i)} = args($i);
       """
    )
    val argMacroAsString = argMacro.mkString("") + "\n"

    s"""
      public class $className {
        Function1<String[] args, Object> apply = new AbstractFunction1<String[] args, Object>() {
          public String apply(String[] args) {
	    $argMacroAsString
            return $code
          }
        }
     }
    """
  }

  /*
   * Wrap source code in a new class with an apply method.
   */
  private def wrapCodeInClass(className: String, code: String, argNames: Array[String]): String = {
    /* 
     * val X = args(0)
     * val Y = args(1)
     */
    val argMacro = for(i <- 0 until argNames.length) yield (
      s"""
          val ${argNames(i)} = args($i)
       """
    )
    val argMacroAsString = argMacro.mkString("") + "\n"

    s"""
      class $className extends Function1[Array[String], Any] {
        def apply(args: Array[String]) = {
	  $argMacroAsString
          $code
        }
      }
    """
  }

  /**
   * Compiles the source string into the class loader and
   * evaluates it.
   *
   * @param code the code to be compiled and evaluated
   * @param args the arguments to apply
   * @return 
   */     
  def eval(code: String, args: Array[String] = Array()): Any = {
    val cls: Class[_] = compile(code, args)
    eval(cls, args) 
  }     

  /**
   * Compiles the source string into the class loader and
   * evaluates it.
   *
   * @param compiledCode the code in compiled form
   * @param args the arguments to apply
   * @return computed value after applying the args to the code
   */
  def eval(compiledCode: Class[_], args: Array[String]): Any = {
    compiledCode.getConstructor().newInstance().asInstanceOf[Function1[Array[String], Any]].apply(args)
  }
}
