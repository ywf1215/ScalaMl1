package org.scalaml.stats

import org.apache.log4j.Logger
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.Stats._
import org.scalaml.util.DisplayUtils

import scala.annotation.implicitNotFound
import scala.util.Try

@implicitNotFound(msg = "MinMax conversion to Double undefined")
@throws(classOf[IllegalArgumentException])
class MinMax[T <: AnyVal](val values: XSeries[T])(implicit f: T => Double) {
  require( values.nonEmpty, "MinMax: Cannot initialize stats with undefined values")
  
  def this(values: Array[T])(implicit f: T => Double) = this(values.toVector)
  
  case class ScaleFactors(low: Double, high: Double, ratio: Double)
  
  private val logger = Logger.getLogger("MinMax")
  
  private[this] val zero = (Double.MaxValue, -Double.MaxValue)
  private[this] var scaleFactors: Option[ScaleFactors] = None
  
  protected[this] val minMax = values./:(zero){(mM, x) => {
    val min = mM._1
    val max = mM._2
    (if(x < min) x else min, if(x > max) x else max)
  }}
  
  final def min = minMax._1
  final def max = minMax._2
  
  @throws(classOf[IllegalStateException])
  final def normalize(low: Double = 0.0, high: Double = 1.0): DblVector = 
    setScaleFactors(low, high).map(scale => {
      values.map ( x => (x - min) * scale.ratio + scale.low )
    })
    .getOrElse(throw new IllegalStateException("MinMax.normalize normalization params undefined"))
    
  final def normalize(value: Double): Try[Double] = Try {
    scaleFactors.map( scale =>
      if(value <= min) scale.low
      else if (value >= max) scale.high
      else (value - min) * scale.ratio + scale.low
    ).get
  }
  
  private def setScaleFactors(low: Double, high: Double): Option[ScaleFactors] =
    if( high < low + STATS_EPS)
      DisplayUtils.none(s"MinMax.set found high - low = $high - $low <= 0 required > ", logger)
    else {
      val ratio = (high - low) / (max - min)
      
      if( ratio < STATS_EPS)
        DisplayUtils.none(s"MinMax.set found ratio $ratio required > EPS ", logger)
      else {
        scaleFactors = Some(ScaleFactors(low, high, ratio))
        scaleFactors
      }
    }

}

class MinMaxVector(series: Vector[DblArray]) {
  val minMaxVector: Vector[MinMax[Double]] = series.transpose.map(new MinMax[Double](_))
  
  final def normalize(low: Double = 0.0, high: Double = 1.0): Vector[DblArray] = 
    minMaxVector.map(_.normalize(low, high)).transpose.map(_.toArray)
    
  final def normalize(x: DblArray): Try[DblArray] = {
    val normalized = minMaxVector.zip(x).map{ case( from, to) => from.normalize(to) }
    
    if( normalized.contains( None) )
      throw new IllegalStateException("MinMax.normalize normalization params undefined")
    Try(normalized.map(_.get).toArray)
    
  }
}

object MinMax {
  def apply[T <: AnyVal](values: XSeries[T])(implicit f: T => Double): Try[MinMax[T]] =
    Try(new MinMax[T](values))
}