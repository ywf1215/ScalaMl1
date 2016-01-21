package org.scalaml.stats

import scala.annotation.implicitNotFound
import scala.collection.immutable.VectorBuilder
import scala.util.Try
import scala.reflect.ClassTag
import scala.language.implicitConversions

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._

object XTSeries {
  final val EPS = 1-20
  
  private val logger = Logger.getLogger("XTSeries")
  
  @throws(classOf[IllegalArgumentException])
  def zipWithShift[T](xv: XSeries[T], n: Int): Vector[(T, T)] = {
    require( n > 0 && n < xv.size,  
			s"XTSeries.zipWithShift found shift n= $n required n < ${xv.size}") 
		
		xv.drop(n).zip(xv.view.dropRight(n))
  }
  
  def zipWithShift[T](xv: Array[T], n: Int): Array[(T, T)] = {
    require( n > 0 && n < xv.length,  
			s"XTSeries.zipWithShift found shift n= $n required n < ${xv.length}")
			
		xv.drop(n).zip(xv.view.dropRight(n))
  }
  
  def zipWithShift1[T](xv: XSeries[T]): Vector[(T, T)] = xv.zip(xv.view.drop(1))
  
  def zipWithShift1[T](xv: Array[T]): Array[(T, T)] = xv.zip(xv.view.drop(1))
  
  @throws(classOf[IllegalArgumentException])
  def splitAt[T](xv: XSeries[T], n: Int): (XSeries[T], XSeries[T]) = {
    require( n > 0 && n < xv.size, s"XTSeries.splitAt found index $n required index < ${xv.size}")
    
    val splitArr = xv.splitAt(n)
    (splitArr._1, splitArr._2)
  }
  
  def dimension[T](xt: XVSeries[T]): Int = xt.head.length
  
  implicit def xvseries2Vector[T <: AnyVal](xv: XVSeries[T])
      (implicit f: T => Double): Vector[DblArray] =
    xv.map( _.map(_.toDouble)) 
  
  @implicitNotFound(msg = "XTSeries.normalize convertion from $T to Double undefined")
	@throws(classOf[IllegalStateException])
	@throws(classOf[IllegalArgumentException])
  def normalize[T <: AnyVal](
      xt: XSeries[T],
      low: Double,
      high: Double)(implicit ordering: Ordering[T], f: T => Double): Try[DblVector] =
    Try (Stats[T](xt).normalize(low, high) )
    
  def normalize[T <: AnyVal](xt: XSeries[T])
      (implicit ordering: Ordering[T], f: T => Double): Try[DblVector] =
    normalize(xt, 0.0, 1.0)
  
  @throws(classOf[IllegalArgumentException])
	@implicitNotFound(msg = "XTSeries.normalize conversion from $T to Double undefined")
	def normalize[T <: AnyVal](
	    xt: XVSeries[T])
	    (implicit order: Ordering[T], m: Manifest[T], f: T => Double): Try[Vector[DblArray]] = {
    require( xt.nonEmpty,
				"XTSeries.normalize Cannot normalize an undefined time series of elements")
		require( dimension(xt) > 0, 
				"XTSeries.normalize Incorrect function to normalize a single dimension time series")
		
		var k = 0
		val res = new Array[Array[T]](xt.size)
		val dim = dimension(xt)
		
		val min = Array.fill(dim)( Double.MaxValue)
		val max = Array.fill(dim)(-Double.MaxValue)
		
		val _xv = xt.toVector
		
		while( k < xt.size) {
		  var j = 0
		  while( j < dim) {
		    if(_xv(k)(j) < min(j))
		      min(j) = _xv(k)(j)
		    else if(_xv(k)(j) > max(j))
		      max(j) = _xv(k)(j)
		    j += 1
		  }
		  k += 1
		}

    val data = new VectorBuilder[DblArray]
    k = 0
    
    Try {
      while( k < xt.size) {
        var j = 0
        val arr = new Array[Double](dim)
        while( j < dim) {
          arr(j) = (_xv(k)(j) - min(j))/(max(j)-min(j))
          j += 1
        }
        data += arr
        k += 1
      }
      data.result()
    }
  }
  
  @throws(classOf[IllegalArgumentException])
  def zScore[T <: AnyVal](xt: XSeries[T])(implicit f: T => Double): Try[DblVector] = Try(Stats[T](xt).zScore )
  
  def zScores[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): Try[XVSeries[Double]] = {
    require( xt.nonEmpty, "XTSeries.zScoring Cannot zScore an undefined time series")
    import scala.collection.immutable.VectorBuilder
    
    val stats = statistics(xt)
    var k = 0
    val dimension = xt.head.length
    
    val data = new VectorBuilder[DblArray]
    
    Try {
      while( k < xt.size) {
        var j = 0
        val arr = Array.fill(dimension)(0.0)
        while( j < dimension) {
          arr(j) = (xt(k)(j) - stats(j).mean)/stats(j).stdDev
          j += 1
        }
        data += arr
        k += 1
      }
      data.result()
    }
  }
  
  def unit[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): Try[Vector[DblArray]] =
    Try(xt.map(_.map(_.toDouble)))
    
  def zipToXVSeries[T](x: XVSeries[T], y: XVSeries[T])
    (f: (Array[T], Array[T]) => Double): XSeries[Double] = {
    require( x.size == y.size,  
			s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")
		x.zip(y.view).map{ case(_x, _y) => f(_x, _y)}
  }
  
  def zipToXSeries[T](x: Vector[T], y: Vector[T])(f: (T, T) => Double): XSeries[Double] = {
    require( x.size == y.size,  
			s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")

		x.zip(y.view).map{ case (_x, _y) => f(_x, _y)}
  }
  
  def zipToArray[T](x: Array[T], y: Array[T])(f: (T, T) => Double): DblArray = {
    require( x.length == y.length,
			s"XTSeries.zipSeries found x.length = ${x.length} != y.length  ${y.length}")
		x.zip(y.view).map{ case(_x, _y) => f(_x, _y)}
  }
  
  def zipToSeries[T](x: Vector[T], y: Vector[T])(implicit f: T => Double): XVSeries[Double] = {
    require( x.size == y.size,  
			  s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")
	  x.zip(y.view).map{ case(_x, _y) => Array[Double](_x, _y)}
  }
  
  def zipToSeries[T: ClassTag](x: Vector[T], y: Vector[T], nSteps: Int)
      (implicit f: T => Double): XVSeries[Double] = {
    require( nSteps > 0, s"XTSeries.zipSeries found nSteps = $nSteps, required > 0" )
		require( x.size == y.size,  
			s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")
			
		x.zip(y.view).map{ case (_x, _y) => Array[Double](_x, _y)}.dropRight(nSteps)
  }
  
  def inner[T <: AnyVal](xt: Array[T], zt: DblArray)(implicit f: T => Double): Double =
    xt.zip(zt).map{ case(x, z) => x*z }.sum
    
  def transfor[T: ClassTag](xt: XVSeries[T]): Try[XVSeries[T]] = Try(xt.transpose.map(_.toArray))
  
  def statistics[T <: AnyVal](xt: XSeries[T])(implicit f: T => Double): Stats[T] = Stats[T](xt)
  
  def statistics[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): Vector[Stats[T]] = {
    require( xt.nonEmpty || dimension(xt) > 0, "XTSeries.statistics input time series undefined")
    xt.transpose.map( Stats[T]( _ ))
  }
}