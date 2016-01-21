package org.scalaml.trading

import scala.annotation.implicitNotFound

import org.scalaml.ga.{Operator, Gene, Quantization}
import org.scalaml.trading.operator._
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.stats.XTSeries
import org.scalaml.util.DisplayUtils
import Gene._

final class Signal(
    id: String,
    target: Double,
    op: SOperator,
    xt: DblVector,
    weights: DblVector)(implicit quant: Quantization, encoding: Encoding)
      extends Gene(id, target, op) {
  import Signal._
  check(xt, weights)
}

object Signal {
  
  private val MAX_TIME_SERIES_SIZE = 10000000
  
  private def check(xt: DblVector, weights: DblVector): Unit = {
    require( xt.nonEmpty, "Signal.check Cannot create a signal with undefined time series input")
		require( xt.size < MAX_TIME_SERIES_SIZE, 
				s"Signalcheck Size of the time series input, ${xt.size} if out of range")
				

		require( weights.nonEmpty, "Signal.check Cannot create a signal with undefined weights")
		require(weights.size < MAX_TIME_SERIES_SIZE, 
					s"Signalcheck Number of weights ${weights.size} if out of range")
		require(xt.size == weights.size, 
					s"Signal The number of weights ${weights.size} is != size of data ${xt.size}")
  }
}