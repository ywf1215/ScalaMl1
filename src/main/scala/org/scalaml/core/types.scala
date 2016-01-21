package org.scalaml.core

import scala.language.implicitConversions
import scala.util.Try

object Types {
  
  object ScalaMl {
    type DblPair = (Double, Double)
    type DblMatrix = Array[DblArray]
    type DblArray = Array[Double]
    type DblVector = Vector[Double]
    type DblPairVector = Vector[DblPair]
    
    type XSeries[T] = Vector[T]
    type XVSeries[T] = Vector[Array[T]]
    
    case class Pair(p: DblPair) {
      def + (o: Pair): Pair = Pair((p._1 + o.p._1, p._2 + o.p._2))
      def / (o: Pair): Pair = Pair((p._1 / o.p._1, p._2 + o.p._2))
    }
    
    final def sqr(x: Double): Double = x*x
    
    implicit def intToDouble(n: Int): Double = n.toDouble
    
    import scala.reflect.ClassTag
    implicit def t2Array[T: ClassTag](t: T): Array[T] = Array.fill(1)(t)
    implicit def array2DblArray[T <: AnyVal](vt: Array[T])(implicit f: T => Double): DblArray =
      vt.map( _.toDouble)
      
    @throws(classOf[IllegalArgumentException])
    implicit def /(m: DblMatrix, row: Int, z: Double): Unit = {
      require(row < m.length, s"/ matrix column $row out of bounds")
			require(Math.abs(z) > 1e-32, s"/ divide column matrix by $z too small")
			
			m(row).indices.foreach( m(row)(_) /= z)
    }
    
    implicit def seriesT2Double[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): DblMatrix =
      xt.map( _.map(_.toDouble)).toArray
      
    implicit def dblPairs2DblMatrix2(x: ((Double, Double), (Double, Double))): DblMatrix =
      Array[DblArray](Array[Double](x._1._1, x._1._2), Array[Double](x._2._1, x._2._2))
      
    implicit def /(v: DblArray, n: Int): Try[DblArray] = Try(v.map(_/n))
    
    @throws(classOf[IllegalArgumentException])
    def toText(v: DblArray, index: Boolean): String = {
      require( v.length > 0, 
						"ScalaMl.toText Cannot create a textual representation of a undefined vector")
			
			if( index) v.zipWithIndex.map{ case(x, n) => s"$x:$n"}.mkString(", ")
			else v.mkString(", ").trim
    }
    
    @throws(classOf[IllegalArgumentException])
    def toText(m: DblMatrix, index: Boolean): String = {
      require( m.length > 0, 
					"ScalaMl.toText Cannot create a textual representation of a undefined vector")
			
			if(index)
			  m.zipWithIndex.map{ case(v, n) => s"$n:${toText(v, index)}"}.mkString("\n")
			else
			  m.map(v => s"${toText(v, index)}").mkString("\n")
    }
  }
  
  val emptyString = ""
  
}