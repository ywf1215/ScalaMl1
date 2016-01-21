package org.scalaml.app.chap1

import java.awt.Color
import scala.util.{Try, Success, Failure}
import scala.io.Source
import org.apache.log4j.Logger
import org.scalaml.plots._
import org.scalaml.stats.{Stats, MinMaxVector}
import org.scalaml.trading.{Signal, YahooFinancials}
import org.scalaml.core.Types
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.regression.logistic.LogBinRegression
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, YahooFinancials._
import org.scalaml.stats.XTSeries._

object LogBinRegressionEval extends Eval {
  
}