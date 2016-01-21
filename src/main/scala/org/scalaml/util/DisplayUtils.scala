package org.scalaml.util

import org.apache.log4j.Logger

object DisplayUtils {
  private val DEFAULT_SHOW_RETURN = 0			// Default return value after info display
	private val DEFAULT_ERROR_RETURN = -1		// Default return value for error
	private val DEST_CONSOLE = 0x01					// Flag to dump computation results on std out
	private val DEST_LOGGER = 0x02					// Flag to dump computation results into log4j log
	private val DEST_CHART = 0x04						// Flag to plot computation results
	
	private val LOG_DESTINATION = Map[String, Int](
		"console" -> DEST_CONSOLE, 
		"logger" -> DEST_LOGGER, 
		"chart" -> DEST_CHART, "none" -> 0x00
	)
	
	implicit class extendTry[T](_try: scala.util.Try[T]) {
    def error[U >: T](default: => U, comment: String, logger: Logger): U =
      _try.getOrElse({ processError(comment, logger); default})
  }
  
  implicit class extendOption[T](_option: Option[T]) {
    def error[U >: T](default: => U, comment: String, logger: Logger): U =
      _option.getOrElse({ processError(comment, logger); default})
  }
	
	private var destination: Int = DEST_CONSOLE + DEST_CHART
	
	def init(args: Array[String]): Unit =
	  destination = args./:(0)((dest, arg) => dest + LOG_DESTINATION.getOrElse(arg, 0))
	  
	final def isChart: Boolean = (destination & 0x04) == 0x04
	
	final def align(label: String, length: Int): String = {
    require( !label.isEmpty, "DisplayUtils.align Label is undefined")
		require(length < 128, 
				s"DisplayUtils.align Size of label placement ${label.length} is incorrect")
		
		if( length < label.length)
		  label
		else {
		  val blankChars = new Array[Char](length - label.length)
		  label + new String(blankChars)
		}
  }
	  
	final def show[T](t: T, logger: Logger, alignment: Int = -1): Int = {
    print(if(alignment != -1) align(t.toString, alignment) else t.toString, logger)
    DEFAULT_SHOW_RETURN
  }
	
	final def show[T](seq: Seq[T], logger: Logger): Int = {
	  seq.mkString(" ")
	  DEFAULT_SHOW_RETURN
	}
	
	final def error[T](t: T, logger: Logger): Int = {
	  processError(t, logger)
	  DEFAULT_ERROR_RETURN
	}
	
	final def error[T](t: T, logger: Logger, e: Throwable): Int = {
		processError(t, logger, e)
		DEFAULT_ERROR_RETURN
	}
	
	final def none[T](t: T, logger: Logger): None.type = none(t, logger)
	
	final def none[T](t: T, logger: Logger, e: Throwable): None.type = {
		processError(t, logger, e)
		None
	}
	
	final def failure[T](t: T, logger: Logger, e: Throwable): Int = {
	  processError(t, logger, e)
	  DEFAULT_ERROR_RETURN
	}
	
	private def processError[T](t: T, logger: Logger, e: Throwable): Unit =
	  print(s"Error: ${t.toString} with ${e.toString}", logger)
	  
	private def processError[T](t: T, logger: Logger): Unit =
	  print(s"Error: ${t.toString}", logger)
	
  private def print(msg: String, logger: Logger): Unit = {
    if( (destination & 0x01) == 0x01)
      Console.println(msg)
    if( (destination & 0x02) == 0x02)
        {logger.error(msg); println("log") }
  }
}