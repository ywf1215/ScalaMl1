package org.scalaml.stats

import org.apache.log4j.Logger
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.Stats._
import org.scalaml.util.DisplayUtils

import scala.annotation.implicitNotFound
import scala.util.Try

class MinMax[T <: AnyVal](val values: XSeries[T])(implicit f: T => Double) {
  require( values.nonEmpty, "MinMax: Cannot initialize stats with undefined values")
  
  def this(values: Array[T])(implicit f: T => Double) = this(values.toVector)
}

class MinMaxVector(series: Vector[DblArray]) {
  val minMaxVector: Vector[MinMax[Double]] = series.transpose.map(new MinMax[Double](_))
  
  final def normalize(low: Double = 0.0, high: Double = 1.0): Vector[DblArray] = 
    minMaxVector.map(_.normalize(low, high))
}