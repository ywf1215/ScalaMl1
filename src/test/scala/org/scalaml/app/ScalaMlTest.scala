package org.scalaml.app

import scala.annotation.switch
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure, Properties}
import scala.concurrent.duration.Duration
import org.scalatest.FunSuite
import org.scalatest.time._
import org.scalatest.concurrent.ScalaFutures
import org.scalaml.util.DisplayUtils
import org.scalaml.util.FormatUtils

trait ScalaMlTest extends FunSuite with ScalaFutures {
  val chapter: String
  val maxExecutionTime: Int
  
  implicit protected val patience = PatienceConfig(timeout = Span(maxExecutionTime, Seconds), 
      interval = Span(1000, Millis))
      
  def evaluate(eval: Eval, args: Array[String] = Array.empty[String]): Unit = {
    println(s"Maximum execution time: $maxExecutionTime")
    
    if( maxExecutionTime > 0) {
      val ft = Future[Int] { eval.test(args) }
      
      whenReady(ft) { r => assert(r >= 0, "OK") }
    }
    else eval.test(args)
  }
}

trait Eval {
  import org.apache.log4j.Logger
  
  val name: String
  protected lazy val logger: Logger = Logger.getLogger(s"$name")
  
  def test(args: Array[String]): Int = Try (run(args) ) match {
    case Success(n) => show(s"Completed")
    case Failure(e) => error(s"  **  ${e.toString}  **  ", e)
  }
  
  protected def run(args: Array[String]): Int
  
  protected def header: String = {
    AllTests.count += 1
    s"\n\n *****  test#R{AllTests.count} $name"
  }
  
  protected def show(description: String): Int = DisplayUtils.show(s"$name $description", logger)
  
  protected def error(description: String): Int = DisplayUtils.error(s"$name $description", logger)
  
  protected def error(description: String, e: Throwable): Int = {
    DisplayUtils.error(s"$name $description", logger, e)
    0
  }
  
  protected def none(description: String): Option[Int] = 
    DisplayUtils.none(s"$name $description", logger)
    
  protected def failureHandler(e: Throwable): Int = 
    if( e.getMessage != null) error(s"$name.run ${e.getMessage} caused by ${e.getCause.toString}")
    else error(s"$name.run ${e.toString}")
}