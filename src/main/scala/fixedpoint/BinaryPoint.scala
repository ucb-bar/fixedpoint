// SPDX-License-Identifier: Apache-2.0

// This file contains definitions of the following traits/classes/objects taken from Chisel's deprecated code:
// - BinaryPoint
// - KnownBinaryPoint
// - UnknownBinaryPoint
// - HasBinaryPoint
//
// HasBinaryPoint uses Record as a self-type as opposed to Bits used in Chisel's code.

package fixedpoint

import chisel3.{ChiselException, Num, Record}

/** Chisel types that have binary points support retrieving
  * literal values as `Double` or `BigDecimal`
  */

object NumBP {
  def toDouble(value: BigInt, binaryPoint: BinaryPoint): Double = {
    binaryPoint match {
      case KnownBinaryPoint(n) => Num.toDouble(value, n)
      case x =>
        throw new ChiselException(s"Error converting BigDecimal $value to BigInt, binary point must be known, not $x")
    }
  }

  def toBigDecimal(value: BigInt, binaryPoint: BinaryPoint): BigDecimal = {
    binaryPoint match {
      case KnownBinaryPoint(n) => Num.toBigDecimal(value, n)
      case x =>
        throw new ChiselException(s"Error converting BigDecimal $value to BigInt, binary point must be known, not $x")
    }
  }
}

trait HasBinaryPoint {
  self: Record =>

  def binaryPoint: BinaryPoint

  /** Return the [[Double]] value of this instance if it is a Literal
    *
    * @note this method may throw an exception if the literal value won't fit in a Double
    */
  def litToDoubleOption: Option[Double] = {
    litOption match {
      case Some(bigInt: BigInt) =>
        Some(NumBP.toDouble(bigInt, binaryPoint))
      case _ => None
    }
  }

  /** Return the double value of this instance assuming it is a literal (convenience method)
    */
  def litToDouble: Double = litToDoubleOption.get

  /** Return the [[BigDecimal]] value of this instance if it is a Literal
    *
    * @note this method may throw an exception if the literal value won't fit in a BigDecimal
    */
  def litToBigDecimalOption: Option[BigDecimal] = {
    litOption match {
      case Some(bigInt: BigInt) =>
        Some(NumBP.toBigDecimal(bigInt, binaryPoint))
      case _ => None
    }
  }

  /** Return the [[BigDecimal]] value of this instance assuming it is a literal (convenience method)
    *
    * @return
    */
  def litToBigDecimal: BigDecimal = litToBigDecimalOption.get
}

object BinaryPoint {
  def apply(x: Int): BinaryPoint = KnownBinaryPoint(x)
  def apply(): BinaryPoint = UnknownBinaryPoint
}

sealed abstract class BinaryPoint {
  type W = Int
  def max(that:              BinaryPoint): BinaryPoint = this.op(that, _ max _)
  def +(that:                BinaryPoint): BinaryPoint = this.op(that, _ + _)
  def +(that:                Int):         BinaryPoint = this.op(this, (a, b) => a + that)
  def shiftRight(that:       Int): BinaryPoint = this.op(this, (a, b) => 0.max(a - that))
  def dynamicShiftLeft(that: BinaryPoint): BinaryPoint =
    this.op(that, (a, b) => a + (1 << b) - 1)

  def known: Boolean
  def get:   W
  protected def op(that: BinaryPoint, f: (W, W) => W): BinaryPoint
}

case object UnknownBinaryPoint extends BinaryPoint {
  def known: Boolean = false
  def get:   Int = None.get
  def op(that: BinaryPoint, f: (W, W) => W): BinaryPoint = this
  override def toString: String = ""
}

sealed case class KnownBinaryPoint(value: Int) extends BinaryPoint {
  def known: Boolean = true
  def get:   Int = value
  def op(that: BinaryPoint, f: (W, W) => W): BinaryPoint = that match {
    case KnownBinaryPoint(x) => KnownBinaryPoint(f(value, x))
    case _                   => that
  }
  override def toString: String = s"<<${value.toString}>>"
}
