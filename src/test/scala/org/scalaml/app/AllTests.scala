package org.scalaml.app

import org.apache.log4j.Logger

	// ScalaMl classes
import org.scalaml.util.DisplayUtils._

import org.scalaml.app.chap1._

object AllTests extends ScalaMlTest {
  import scala.util.Properties
	import scala.annotation.switch
	
	val chapter: String = "All tests"
	val maxExecutionTime: Int = -1
	val sparkEnabled = false
	
	private val CONFIGURATION = 
		" ****** Scala for Machine Learning - V. 0.99.1 - Execution examples ****** \n" +
		"Recommended SBT/JVM configuration:\n -Xmx4096 (or higher)\n" +
		" -XX:MaxPermSize=512m (or higher)\n -XX:ReservedCodeCacheSize=256m (or higher)\n" +
		s"Context:\nUser:${Properties.userName}, OS:${Properties.osName}\n\n" +
		"Warning: Apache Spark 1.5.0 and earlier is not compatible with Scala 2.11\n" +
		"You need to set scalaVersion := 2.10.x in built.sbt to build and run Apache Spark\n" +
		" **************************************************************\n"
		
	private val logger = Logger.getLogger("AllTests")
	
	def run(): Unit = {
		show("Full test run: may takes several minutes", logger)
		
		// Chapter 1
		evaluate(MinMaxEval)
		evaluate(LogBinRegressionEval)
		/*evaluate(LogBinRegressionEval2)
		evaluate(PlotterEval)*/
	}
		
	def header(args: Array[String]): Unit = {
		val buf = new StringBuilder("\nCommand line configuration for output:")
		
		args.foreach(arg => buf.append(s" $arg"))
	  buf.append("\n")
			// Display configuration and settings information  regarding OS
		buf.append(CONFIGURATION)
		if( !Properties.isWin && !Properties.isMac)
			buf.append("The library has not be tested for this Operating System")
	
			// Correct version of Java
		buf.append(s"Java version: ${Properties.javaVersion}\n")
		if(!Properties.isJavaAtLeast("1.7"))
			buf.append("Incompatible version of Java, should be 1.7 or later\n")
			
			// Correct version of Scala and AKka
		val scalaVersion = Properties.versionNumberString
		buf.append(s"Scala version: $scalaVersion\n")
		(scalaVersion.charAt(2): @switch) match {
			case '9' => buf.append("Scala version should be 2.10.4 or higher")
			case '1' =>
				(scalaVersion.charAt(3): @switch) match {
					case '0' => buf.append("Compatible Akka version should be 2.2.4 or lower")
					case '1' => buf.append("Compatible Akka version should be 2.3.4 or higher")
				}
			case _ => buf.append("Could not initialize")
		}
		show(buf.toString, logger)
		count = 0
	}
	var count: Int = _	
	def testCount: String = { count += 1;  String.valueOf(count) }
}

object AllTestsApp extends App {
  if( args.length > 0 ) {
    init(args)
    AllTests.header(args)
  } else {
    AllTests.header(Array[String]("console", "chart"))
  }
  AllTests.run()
}