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


import scala.collection.JavaConversions._
import scala.collection.mutable.{Set => MutableSet}

import SafelogException.printLabel

class Repl(
    self: String
  , saysOperator: Boolean
  , _statementCache: MutableCache[Index, MutableSet[Statement]]
) extends Safelog(self, saysOperator, _statementCache) { // class is useful for unit testing and for modularity (i.e., extending from slang)

  val stdPromptMsg = "slog> "
  val date = new java.text.SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z").format(java.util.Calendar.getInstance().getTime())
  val greet = s"Safe Logic v0.1: $date (To quit, press Ctrl+D or q.)"

  private val _assertionsInMemory: MutableSet[Statement] = MutableSet.empty     // parsed assertions
  private val _queriesInMemory: MutableSet[Statement] = MutableSet.empty        // parsed queries
  private val _retractionsInMemory: MutableSet[Retraction] = MutableSet.empty   // parsed retractions
  private val _queriesInMemoryFresh: MutableSet[Statement] = MutableSet.empty   // parsed queries which are fresh, i.e., not executed yet
  private var _inputScanned: StringBuilder = new StringBuilder()  // stores incomplete statements that end with other than (.|?|~)

  private val replCmdSeq = Seq(
      "clear."
    , "help."
    , "import!(<file>)."
    , "ls."
    , "ls(facts)."
    , "ls(rules)."
    , "ls(queries)."
    , "ls(retractions)."
    , "ls(all)."
    , "quit."
  )

  // [[http://jline.github.io/jline2/apidocs/reference/jline/console/ConsoleReader.html]]
  private def consoleReader(fileName: Option[String]): jline.console.ConsoleReader = {
    import java.io.File
    import scala.collection.JavaConverters._
    import scala.io.Source
    import jline.console.completer.StringsCompleter
    import jline.console.history.FileHistory

    fileName match {
      case Some(fn) => 
        val file = Term.stripQuotes(fn)
        try {
          val fStream = this.getClass().getClassLoader().getResourceAsStream(file) match { // for externalized resources 
            case null => this.getClass().getClassLoader().getResourceAsStream(file + ".slog")
            case x => x
          }
          if(fStream == null) throw new NullPointerException() // crappy java
	  val reader = new jline.console.ConsoleReader(fStream, System.out)
	  reader.setHistoryEnabled(false)
	  reader.setExpandEvents(false) // for ! issue
	  reader
       } catch {
         case e: java.io.FileNotFoundException => val fn = new File(file)
           throw ParserException("The path I read is: " + fn.getCanonicalPath() + ", canRead=" + fn.canRead() + ", exists=" + fn.exists())
         case e: java.io.IOException => throw ParserException(s"Error reading file: $file")
         case e: NullPointerException => throw ParserException(s"Error reading file: $file")
         case _: Throwable => throw ParserException(s"Error reading file: $file")
       }
      case _ => 
	val reader = new jline.console.ConsoleReader(System.in, System.out)
	reader.setHistory(new FileHistory(new File(System.getProperty("user.home") + "/.slang_history")))
	reader.setHistoryEnabled(true)
	reader.setExpandEvents(false) // for ! issue
        val completer = new StringsCompleter(replCmdSeq.asJavaCollection)
        reader.addCompleter(completer)
	reader
    }
  }

  // The main read-eval-print-loop aka REPL
  def repl(): Unit = {
    println(greet)
    readEval(consoleReader(None), stdPromptMsg, true)
    println("leaving")
  }

  def flushHistory(reader: jline.console.ConsoleReader): Unit =
    try {
      printLabel('info); println("Bye!") //Ctrl-D returns null
      reader.getHistory.asInstanceOf[jline.console.history.FileHistory].flush(); // TODO: check for auto-trimming or purging
    } catch {
      case e: Exception => {} // can raise when reading from a file since history is not set to false
    }

  private def evalReplCmd(cmd: Term): Boolean = cmd match {

    case Constant(StrLit("clear"), _, _, _) | Structure(StrLit("clear"), Nil, _, _, _) => 
      printLabel('info); println("clearing cached statements") 
      _assertionsInMemory.clear()
      _queriesInMemory.clear()
      _statementCache.clear()
      true

    case Constant(StrLit("ls"), _, _, _) | Structure(StrLit("ls"), Nil, _, _, _) => 
      printLabel('info); println("list of assertions made so far ...")
      _assertionsInMemory.foreach(println)
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("_fact"), Variable(StrLit("$Self"), _, _, _) +: Constant(StrLit("ls"), _, _, _) +: Nil, _, _, _) 
       | Structure(StrLit("ls"), Nil, _, _, _) => 
      printLabel('info); println("list of assertions made so far ...")
      _assertionsInMemory.foreach(println)
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("ls"), Seq(Constant(StrLit("all"), _, _, _)), _, _, _) => 
      printLabel('info); println("list of all statements made so far...")
      _assertionsInMemory.foreach(println)
      _retractionsInMemory.foreach(println)
      _queriesInMemory.foreach(println)
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("ls"), Seq(Constant(StrLit("facts"), _, _, _)), _, _, _) => 
      printLabel('info); println("facts made so far ...")
      _assertionsInMemory.filter(_.terms.length < 2).foreach(println)
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("ls"), Seq(Constant(StrLit("rules"), _, _, _)), _, _, _) => 
      printLabel('info); println("rules made so far ....")
      _assertionsInMemory.filter(_.terms.length > 1).foreach(println)
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("ls"), Seq(Constant(StrLit("queries"), _, _, _)), _, _, _) => 
      printLabel('info); println("queries made so far ....")
      _queriesInMemory.foreach(println)
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("ls"), Seq(Constant(StrLit("retractions"), _, _, _)), _, _, _) => 
      printLabel('info); println("retractions made so far ....")
      _retractionsInMemory.foreach(println)
      _statementCache.remove(cmd.primaryIndex)
      true

    case Constant(StrLit("help"), _, _, _) | Structure(StrLit("help"), Nil, _, _, _) => 
      printLabel('info); println("commands list") 
      println("help.                     display this message")
      println("import!(<file>).          import file with name <file>")
      println("ls.                       list of assertions made so far")
      println("ls(facts).                list of facts made so far")
      println("ls(rules).                list of rules made so far")
      println("ls(queries).              list of queries made so far")
      println("ls(retractions).          list of retractions made so far")
      println("ls(all).                  list of all statements made so far")
      println("q().                      quit the interpreter")
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("import!"), xterms, _, _, _) => 
      val (fileName, subjectSha) = xterms match {
         case Seq(Constant(fileName, _, _, _)) => (fileName, "")
         case Seq(Constant(fileName, _, _, _), Constant(sub, _, _, _)) => (fileName, sub)
      }
      printLabel('info); println("importing file ...") 

      val file = fileName.name
      val fStream = new java.io.InputStreamReader(this.getClass().getClassLoader().getResourceAsStream(file))
      val (knowledgeBase, time) = util.time(parse(fStream))
      logger.info(s"Time for import is $time")
      println("Imported in %f milliseconds".format(time))
      val allStatements = knowledgeBase
      allStatements.values().flatten.foreach {
        case s @ Assertion(stmt) => stmt.head match {
	   case Structure(StrLit("import!"), _, _, _, _) => evalReplCmd(stmt.head)
	   case _ => _assertionsInMemory += s
	}
        case s @ Retraction(stmt) => _retractionsInMemory += s
        case s @ Query(stmt) => _queriesInMemory += s
        case s @ QueryAll(stmt) => _queriesInMemory += s
	case x => println(s"Repl: Something is malformed during parsing, $x")
      }
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("trace"), Seq(Constant(StrLit("on"), _, _, _)), _, _, _) => printLabel('info); println("starting the trace ...")
      //env._trace = true
      _statementCache.remove(cmd.primaryIndex)
      true

    case Structure(StrLit("trace"), Seq(Constant(StrLit("off"), _, _, _)), _, _, _) => printLabel('info); println("trace is off ...") 
      //env._trace = false
      _statementCache.remove(cmd.primaryIndex)
      true

    case _ => false
  }

  //val quit = """(q(uit)?\s*(\(\))?\s*[.?]+\s*$)""".r // q. | quit. | q(). | quit(). | q? | quit? | ..

  // Get a stream of input strings; Stream implements lazy lists where elements are evaluated only when they are needed
  @annotation.tailrec
  private def readEval(reader: jline.console.ConsoleReader, promptMsg: String = "", interactiveMode: Boolean = false): Seq[Statement] = {

    def updateQuery(query: Statement, stmt: Seq[Term]): Unit = {
      if(interactiveMode && !evalReplCmd(stmt.head)) {
	_queriesInMemory += query
	_queriesInMemoryFresh += query
      }
      if(!interactiveMode) {
	_queriesInMemory += query
	_queriesInMemoryFresh += query
      }
    }

    if(interactiveMode) reader.setPrompt(promptMsg)

    reader.readLine() match {
      case null => _inputScanned.append("q.")
      case str  => _inputScanned.append(str)
    }

    _inputScanned.toString match {
      //case quit(_,_,_) =>     //Ctrl-D returns null
       // if(interactiveMode) flushHistory(reader)
       // Nil
      case str => parseCmdLine(str) match {
	case (Some(program), 'success) => 
          try {
	    program.values().flatten.map { // TODO: Looping through program.values() for each input is not necessary
	      case assertion @ Assertion(stmt) =>
		if(interactiveMode && !evalReplCmd(stmt.head)) _assertionsInMemory += assertion
		if(!interactiveMode) _assertionsInMemory += assertion
                if(assertion.terms.head.id.name == "clear") throw new UnSafeException("clear")
	      case query @ Query(stmt) => 
		program.remove(StrLit("_query"))
		updateQuery(query, stmt)
	      case query @ QueryAll(stmt) => 
		program.remove(StrLit("_query"))
		updateQuery(query, stmt)
	      case _ => Nil
	   }
         } catch {
           case _: UnSafeException =>
         }
         // handle retractions
         program.get(StrLit("_retraction")).map{r => r.foreach{rs => retractStatement(rs, program)}}

         if(_queriesInMemoryFresh.nonEmpty) {
	   try {
            val programMap = program.map { kv => (kv._1, kv._2.toSet)}.toMap
            val (solutions: Seq[Seq[Statement]], time: Double) = util.time(solve(programMap, _queriesInMemoryFresh.toSeq, interactiveMode), 'ms)
              logger.info(s"Solve completed in $time milliseconds")
	      if(solutions.nonEmpty) true //printLabel('success) //println(s"""All solutions: ${solutions.mkString(", ")}""") 
	      else printLabel('failure)
           } catch {
              case ex: Throwable => 
                logger.error(ex.toString)
           }
	 }
	 _inputScanned.clear()
	 _queriesInMemoryFresh.clear()
	 if(interactiveMode) readEval(reader, stdPromptMsg, true) else readEval(reader)
        case (None, 'quit) => 
	  _inputScanned.clear()
          if(interactiveMode) flushHistory(reader)
          Nil
	case (None, 'comment) =>  // a line starting with a comment
	  _inputScanned.clear()
	  if(interactiveMode) readEval(reader, stdPromptMsg, true) else readEval(reader)
	case (None, 'paste) => 
	  _inputScanned.clear()
	  if(interactiveMode) readEval(reader, " | ", true) else readEval(reader)
	case (None, 'continuation) => // empty line or incomplete statement
	  if(interactiveMode) readEval(reader, " | ", true) else readEval(reader)
	case (Some(stmt), 'builtIn) =>  // is it a builtIn? // TODO:
	  // call the appropriate builtIn function
	  // builtInHandler()
	  //_assertionsInMemory -= stmt
	  _inputScanned.clear()
	  if(interactiveMode) readEval(reader, stdPromptMsg, true) else readEval(reader)
        case (_, 'failure) =>
	  _inputScanned.clear()
	  if(interactiveMode) readEval(reader, stdPromptMsg, true) else readEval(reader)
        case (_, 'error) =>
	  _inputScanned.clear()
	  if(interactiveMode) readEval(reader, stdPromptMsg, true) else readEval(reader)
      }
    }
  }

  def retractStatement(
      statement: Statement
    , _program: MutableCache[Index, MutableSet[Statement]]
  ): Unit = statement.terms match {

    case r @ Constant(StrLit("_withArity"), _, _, _) +: Constant(predicate, _, _, _) +: Constant(arity, _, _, _) +: Nil =>
      val index = StrLit(predicate.name + arity.name)
      val retraAssert: Seq[Statement] = _program.get(index) match {
        case None    => Nil
        case Some(x) => x.toSeq
      }
      _assertionsInMemory  --= retraAssert
      _retractionsInMemory  += Retraction(statement.terms)
      _program.remove(index)
      _program.remove(StrLit("_retraction"))
    case terms  =>
      val retraAssert = Assertion(terms)
      _assertionsInMemory  -= retraAssert
      _retractionsInMemory += Retraction(terms)
      val index = StrLit(statement.terms.head.id.name + statement.terms.head.arity)
      val stmts: MutableSet[Statement] = _program.get(index).getOrElse(MutableSet.empty)
      _program.put(index, stmts - retraAssert)
      _program.remove(StrLit("_retraction"))
  }

  def evalFileWithTime(fileName: String, unit: Symbol): Tuple2[Seq[Statement], Double] = util.time(evalOnFile(fileName).head, unit)
  def evalFileReaderWithTime(fileReader: java.io.BufferedReader, unit: Symbol): Tuple2[Seq[Statement], Double] = util.time(evalFileReader(fileReader).head, unit)

  def evalOnFile(fileName: String): Seq[Seq[Statement]] = {
    val fileReader = try {
      new java.io.BufferedReader(new java.io.FileReader(fileName))
    } catch {
      case e: java.io.FileNotFoundException => val fn = new java.io.File(fileName)
        throw ParserException("The path I read is: " + fn.getCanonicalPath() + ", canRead=" + fn.canRead() + ", exists=" + fn.exists())
      case e: java.io.IOException => throw ParserException(s"Error reading file: $fileName")
      case e: NullPointerException => throw ParserException(s"Error reading file: $fileName")
      case _: Throwable => throw ParserException(s"Error reading file: $fileName")
    }
    evalFileReader(fileReader)
  }

  def evalFileReader(fileReader: java.io.BufferedReader): Seq[Seq[Statement]] = {
    val res = util.time(parseAsSegments(fileReader), 'ms) match {
      case (statements, parseTime) => println("Parsing completed in %f milliseconds".format(parseTime))
        val (res, solveTime) = util.time(solve(statements._1, statements._2, false), 'ms)
        println("Solve completed in %f milliseconds".format(solveTime))
        res
      case _ => Nil
    }
    res
  }

  def solveLocal(program: Map[Index, Set[Statement]], isInteractive: Boolean = false): Seq[Seq[Statement]] = {
    program.values.flatten.map {
      case assertion @ Assertion(stmt) =>
	_assertionsInMemory += assertion
      case retraction @ Retraction(stmt) =>
        val retraAssert = Assertion(retraction.terms)
        _assertionsInMemory -= retraAssert
	_retractionsInMemory += retraction
      case query @ Query(stmt) =>
	_queriesInMemory += query
	_queriesInMemoryFresh += query
      case query @ QueryAll(stmt) =>
	_queriesInMemory += query
	_queriesInMemoryFresh += query
      case _ => Nil
    }
    try {
      val (solutions: Seq[Seq[Statement]], time: Double) = util.time(solve(program, _queriesInMemoryFresh.toSeq, isInteractive), 'ms)
      logger.info(s"Solve completed in $time milliseconds")
      solutions
    } catch {
      case ex: Throwable => 
        logger.error(ex.toString)
        Nil 
    }
  }

  def printOutput(out: Tuple2[Seq[Statement], Double]): Unit = out match {
    case (Nil, _) => printLabel('failure)
    case (_, 0.0) => printLabel('success)
      out._1.map(println(_))
      printLabel('info)
    case _ => printLabel('success)
      out._1.map(println(_))
      printLabel('info)
      logger.info("Run completed in %f milliseconds".format(out._2))
  }

  /*
  def importSets(statements: Seq[Statement]): SafeCache[SetId, CredentialSet] = {
    val importStatements = program.get('import1)
    if(importStatements == null) program
    else {
       importStatements foreach { stmt =>
         importProgram(stmt, program)
       }
    }
    program //TODO: yield and flatten?
  }

  def importProgram(stmt: Statement, program: ConcurrentHashMap[Index, Set[Statement]]): ConcurrentHashMap[Index, Set[Statement]] = stmt.terms.head match {
    case Structure('import, Seq(Constant(fileName))) => printLabel('info); println(s"importing program with name $fileName ...") 
      val file = fileName.name
      val fStream = new java.io.InputStreamReader(this.getClass().getClassLoader().getResourceAsStream(file))
      val importedProgram = parse(fStream)
      dropStatement(stmt, program)
      importIfPresent(importedProgram)
    case _ => program
  }
  */
}

object Repl {
  //def apply() = new Repl(Config.config.self, Config.config.saysOperator, new MutableCache[Index, MutableSet[Statement]]())
  val inference = new Repl(Config.config.self, Config.config.saysOperator, new MutableCache[Index, MutableSet[Statement]]())

  def main(args: Array[String]): Unit = {

    val usage = """
      Usage: Repl [--file|-f fileName] [--help|-h]
    """

    val optionalArgs = Map(
        "--file"       -> 'file
      , "-f"           -> 'file
    )

    val argMap = Util.readCmdLine(args.toSeq, requiredArgs = Nil, optionalArgs, Map('help -> "false"))

    if(argMap('help) == "true") {
      println(usage)
    } else if(argMap.get('file).getOrElse("nil") != "nil") {
      inference.printOutput(inference.evalFileWithTime(argMap.get('file).get, 'ms)) // Evaluate expressions from a file.
    } else {
      inference.repl() // The init call 
    }
    //throw new Exception("Incorrect arguments")
  }
}
