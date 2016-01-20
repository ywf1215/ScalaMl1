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
  }
  
}