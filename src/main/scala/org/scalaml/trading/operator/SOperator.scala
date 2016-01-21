package org.scalaml.trading.operator

import org.scalaml.ga.Operator

class SOperator(_id: Int) extends Operator {
  
  override def id: Int = _id
  
  override def apply(idx: Int): SOperator = SOperator.SOPERATORS(idx)
  override def toString: String = id.toString
}

object NONE extends SOperator(0) { override def toString: String = "NA" }

object LESS_THAN extends SOperator(1) { override def toString: String = "<" }

object GREATER_THAN extends SOperator(2) {override def toString: String = ">" }

object EQUAL extends SOperator(3) { override def toString: String = "=" }

object SOperator {
  protected val SOPERATORS = Array[SOperator](NONE, LESS_THAN, GREATER_THAN, EQUAL)
}