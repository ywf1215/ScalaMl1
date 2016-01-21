package org.scalaml.stats

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.Stats._

@throws(classOf[IllegalArgumentException])
class Stats[T <: AnyVal](values: XSeries[T])(implicit f: T => Double) extends MinMax[T](values) {
  require( values.nonEmpty, "Stats: Cannot initialize stats with undefined values")
  
  private[this] val sums = values./:((0.0, 0.0))((acc, s) => (acc._1 + s, acc._2 + s*s))
  
  @inline
  lazy val mean = sums._1/values.size
  
  lazy val variance = (sums._2 - mean*mean*values.size)/(values.size-1)
  
  lazy val stdDev = Math.sqrt(variance)
  
  final def lidstoneMean(smoothing: Double, dim: Int): Double = {
    require( smoothing >0.0 && smoothing <= 1.0, 
				s"Stats.lidstoneMean Lidstone smoothing factor $smoothing is out of range")
		require(dim > 0, s"Stats.lidstoneMean Dimension for Lidstone factor $dim is out of range")
		
		(sums._1 + smoothing)/(values.size + smoothing*dim)
  }
  
  final def laplaceMean(dim: Int): Double = {
    require(dim > 0, s"Stats.laplaceMean Dimension for Lidstone factor $dim is out of range")
    (sums._1 + 1.0)/(values.size + dim)
  }
  
  def zScore: DblVector = {
    if(stdDev <= STATS_EPS )
      throw new IllegalStateException("Stats.normalize Cannot compute zScore -  divide by zero")
    values.map(x => (x - mean)/stdDev )
  }
  
  final def gauss(x: Double): Double = {
    val y = (x - mean)/stdDev
    INV_SQRT_2PI * Math.exp(-0.5*y*y)/stdDev
  }
}

object Stats {
  final val STATS_EPS = 1e-12
  final val INV_SQRT_2PI = 1.0/Math.sqrt(2.0*Math.PI)
  
  def apply[T <: AnyVal](values: Vector[T])(implicit f: T => Double): Stats[T] =
    new Stats[T](values)
    
  def apply[T <: AnyVal](values: Array[T])(implicit f: T => Double): Stats[T] =
    new Stats[T](values.toVector)
    
  def apply[T <: AnyVal](values: Iterator[T])(implicit f: T => Double): Stats[T] =
    new Stats[T](values.toVector)
    
  final def gauss(mean: Double, stdDev: Double, x: Double): Double = {
    require(Math.abs(stdDev) >= STATS_EPS, 
				s"Stats.gauss, Gauss standard deviation $stdDev is close to zero")
		
		val y = (x - mean)/stdDev
		INV_SQRT_2PI*Math.exp(-0.5*y*y)/stdDev
  }
  
  final val LOG_2PI = -Math.log(2.0*Math.PI)
  final def logGauss(mean: Double, stdDev: Double, x: Double): Double = {
    val y = (x - mean)/stdDev
    -LOG_2PI - Math.log(stdDev) - 0.5*y*y
  }
  
  val logNormal = logGauss(0.0, 1.0, _: Double)
  
  final def gauss(x: Double*): Double = {
    require(x.size > 2, s"Stats.gauss Number of parameters ${x.size} is out of range")
    gauss(x(0), x(1), x(2))
  }
  
  final def logGauss(x: Double*): Double = logGauss(x(0), x(1), x(2))
  
  val normal = gauss(0.0, 1.0, _: Double)
  
  
  final def bernoulli(mean: Double, p: Int): Double = mean*p + (1-mean)*(1-p)
  
  @throws(classOf[IllegalArgumentException])
  final def bernoulli(x: Double*): Double = {
    require(x.size > 2, s"Stats.bernoulli found ${x.size} arguments required > 2")
    bernoulli(x(0), x(1).toInt)
  }
}