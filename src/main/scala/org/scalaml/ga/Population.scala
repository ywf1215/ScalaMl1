package org.scalaml.ga

import scala.collection.mutable.ArrayBuffer
	// 3rd party libraries
import org.apache.log4j.Logger
	// Scala for Machine learning classes
import org.scalaml.ga.Chromosome._

case class GeneticIndices(chOpIdx: Int, geneOpIdx: Int) {
  override def toString: String = s"ch index: $chOpIdx gene index: $geneOpIdx"
}

class Population[T <: Gene](limit: Int, val chromosomes: Pool[T]) {
  
}

object Population {
  
}