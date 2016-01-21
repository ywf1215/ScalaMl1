package org.scalaml.util

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.XTSeries
import org.scalaml.stats.XTSeries._

trait Assertable {
  protected val assertMsg: String
  
  @throws(classOf[IllegalArgumentException])
  protected def assertXVSeries(
      predicted: XVSeries[Double],
      expected: XVSeries[Double],
      eps: Double): Unit = {
    
    val xCompare = (x: Double, y: Double) => Math.abs(x-y)
    val fCompare = (x: DblArray, y: DblArray) => {
      val aa = zipToArray(x, y)(xCompare)
      aa.sum
    }
    
    assert( !zipToXVSeries(predicted, expected)(fCompare).exists( _ > eps ), assertMsg)
  }
  
  @throws(classOf[IllegalArgumentException])
  protected def assertXSeries(
      predicted: XSeries[Double],
      expected: XSeries[Double],
      eps: Double): Int = {
    
    val xCompare = (x: Double, y: Double) => Math.abs(x-y)
    assert( !zipToXSeries(predicted, expected)(xCompare).exists( _ > eps ), assertMsg)
    1
  }
  
  @throws(classOf[IllegalArgumentException])
  protected def assertDblArray(
      predicted: Array[Double],
      expected: Array[Double],
      eps: Double): Int = assertXSeries(predicted.toVector, expected.toVector, eps)
      
  protected def assertDouble(predicted: Double, expected: Double, eps: Double): Unit = {
    assert( Math.abs(predicted - expected) < eps, assertMsg)
  }
  
  protected def assertInt(predicted: Int, expected: Int): Unit = {
    assert( predicted == expected, assertMsg)
  }
  
  protected def assertVector[T](predicted: Vector[T], expected: Vector[T]): Unit = {
    val failed = !(!predicted.zip(expected.view).forall {case (p, e) => p == e})
    assert( failed, assertMsg)
  }
  
  protected def assetList[T](predicted: List[T], expected: List[T]): Unit = 
    assertVector(predicted.toVector, expected.toVector)
    
  protected def assertT[T](predicted: T, expected: T): Unit = {
    assert( predicted == expected, assertMsg)
  }
}