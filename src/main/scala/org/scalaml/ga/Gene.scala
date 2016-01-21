package org.scalaml.ga

import java.util

import scala.annotation.implicitNotFound

trait Operator {
  def id: Int = -1
  
  def apply(id: Int): Operator
}

object NO_OPERATOR extends Operator {
  override def id: Int = -1
  def apply(idx: Int): Operator = NO_OPERATOR
}

import org.scalaml.ga.Gene._

case class Quantization(toInt: Double => Int, toDouble: Int => Double) {
  def this(R: Int) = this((x: Double) => (x*R).floor.toInt, (n: Int) => n/R)
}

@implicitNotFound("Gene encoding requires double to integer conversion")
@implicitNotFound("Gene encoding requires quantization")
@throws(classOf[IllegalArgumentException])
class Gene(
    val id: String,
    val target: Double,
    val op: Operator)(implicit quant: Quantization, encoding: Encoding) {
  
  require( !id.isEmpty, "Cannot create a signal with undefined id")
  
  lazy val bits = apply(target, op)
  
  def apply(value: Double, operator: Operator): util.BitSet = {
    val bitset = new java.util.BitSet(encoding.length)
    
    encoding.rOp foreach(i => if( ((operator.id>>i) & 0x01) == 0x01) bitset.set(i))
    
    encoding.rValue foreach(i => if( (( quant.toInt(value)>>i) & 0x01) == 0x01) bitset.set(i) )
    
    bitset
  }
  
  def unapply(bitSet: util.BitSet): (Double, Operator) =
    (quant.toDouble(convert(encoding.rValue, bits)), op(convert(encoding.rOp, bits)))
  
  override def clone: Gene = 
    Range(0, bits.length)./:(Gene(id, target, op))((enc, n) => {
      if( bits.get(n))
        enc.bits.set(n)
      enc
    })
   
  def toGene(id: String, target: Double, op: Operator) = new Gene(id, target, op)
  
  def score: Double = -1.0
  
  
}

object Gene {
  
  def apply(id: String, target: Double, op: Operator)
			(implicit quant: Quantization, encoding: Encoding): Gene = 
		new Gene(id, target, op)
  
  class Encoding(nValueBits: Int, nOpBits: Int) {
    val rValue = Range(0, nValueBits)
    val length = nValueBits + nOpBits
    val rOp = Range(nValueBits, length)
  }
  
  private def convert(r: Range, bits: util.BitSet): Int =
    r./:(0)((v, i) => v + (if(bits.get(i)) 1<<i else 0))
}