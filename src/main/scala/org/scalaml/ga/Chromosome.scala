package org.scalaml.ga

import org.scalaml.ga.Chromosome._

import scala.annotation.implicitNotFound
import scala.collection._
import scala.util.Random

final class Chromosome[T <: Gene](val code: List[T]) {
  require( code.nonEmpty, "Chromosome Cannot create a chromosome from undefined genes")
  
}

object Chromosome {
  
  def apply[T <: Gene](code: List[T]): Chromosome[T] = new Chromosome[T](code)
  
  
  type Pool[T <: Gene] = mutable.ArrayBuffer[Chromosome[T]]
}