package org.scalaml.app.chap1

import org.scalaml.stats.{MinMaxVector, XTSeries}
import org.scalaml.util.Assertable
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.app.Eval

object MinMaxEval extends Eval with Assertable {
  
  val name: String = "ETransformEval"
  protected val assertMsg: String = "MinMaxEval normalization failed"
  
  val values: XVSeries[Double] = Vector[DblArray](
		Array[Double](2.6, 1.7, 9.9),
		Array[Double](-2.9, 11.7, 29.9),
		Array[Double](0.6, -17.5, 50.5),
		Array[Double](12.0, 0.2, -34.8)
	)
	
	val expected: XVSeries[Double] = Vector[DblArray](
		Array[Double](0.3691 ,0.6575,0.5240),
		Array[Double](0.0,1.0,0.7584),
		Array[Double](0.2349,0.0,1.0),
		Array[Double](1.0,0.6061,0.0)
	)
	
	val value1 = Array[Double](2.6, 1.7, 9.9)
	val expected1 = Array[Double](0.3691,0.6575, 0.5240)
	val value2 = Array[Double](-70.9, 1.7, 90.9)
	val expected2 = Array[Double](0.0,0.6575,1.0)
	
	override protected def run(args: Array[String]): Int = {
    show(s"$header Evaluation eTransform monad")
    
    val minMaxes = new MinMaxVector(values)
    
    val res = minMaxes.normalize(0.0, 1.0)
    assertXVSeries(res, expected, 1e-2)
    res.map(r => show(s"Normalized data ${r.mkString(",")}"))
    
    val res1 = minMaxes.normalize(value1)
    assertDblArray(res1.get, expected1, 1e-2)
    val res2 = minMaxes.normalize(value2)
    assertDblArray(res2.get, expected2, 1e-2)
    
  }
}