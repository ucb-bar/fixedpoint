// SPDX-License-Identifier: Apache-2.0

// This file contains the definitions of FixedPoint class and companion object. Much of Chisel's original code
// is reused, but Record is inherited from instead of Bits. Relevant methods from Bits and Chisel's FixedPoint
// have also been implemented in order to maximally replicate the original FixedPoint interface.

// Notes:
// - Not being able to extend cloneSuperType behavior makes it difficult to use user-defined FixedPoint with Muxes,
// and also to implement typeEquivalent fully
// - Not being able to extend MonoConnect behavior makes it difficult to properly connect FixedPoints with
// different BinaryPoints, especially if inside other Bundles and Vecs
// - Cannot Mux1H with aggregates with inferred widths

package fixedpoint

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.OpaqueType
import chisel3.experimental.SourceInfo
import chisel3.internal.sourceinfo.{SourceInfoTransform, SourceInfoWhiteboxTransform}

import scala.collection.immutable.SeqMap
import scala.language.experimental.macros
import chisel3.util.Cat

object FixedPoint extends NumObject {

  /** Create a FixedPoint type with inferred width. */
  def apply(): FixedPoint = apply(UnknownWidth, BinaryPoint())

  /** Create a FixedPoint type or port with fixed width. */
  def apply(width: Width, binaryPoint: BinaryPoint): FixedPoint = new FixedPoint(width, binaryPoint)

  /** Create a FixedPoint literal with inferred width from BigInt.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigInt(value: BigInt, width: Width, binaryPoint: BinaryPoint): FixedPoint = {
    apply(value, width, binaryPoint)
  }

  /** Create a FixedPoint literal with inferred width from BigInt.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigInt(value: BigInt, binaryPoint: BinaryPoint = 0.BP): FixedPoint = {
    apply(value, UnknownWidth, binaryPoint)
  }

  /** Create a FixedPoint literal with inferred width from BigInt.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigInt(value: BigInt, width: Int, binaryPoint: Int): FixedPoint =
    if (width == -1) {
      apply(value, UnknownWidth, BinaryPoint(binaryPoint))
    } else {
      apply(value, KnownWidth(width), BinaryPoint(binaryPoint))
    }

  /** Create a FixedPoint literal with inferred width from Double.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromDouble(value: Double, width: Width, binaryPoint: BinaryPoint): FixedPoint = {
    fromBigInt(
      toBigInt(value, binaryPoint.get),
      width = width,
      binaryPoint = binaryPoint
    )
  }

  /** Create a FixedPoint literal with inferred width from BigDecimal.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigDecimal(value: BigDecimal, width: Width, binaryPoint: BinaryPoint): FixedPoint = {
    fromBigInt(
      toBigInt(value, binaryPoint.get),
      width = width,
      binaryPoint = binaryPoint
    )
  }

  /** Create a FixedPoint port with specified width and binary position. */
  def apply(value: BigInt, width: Width, binaryPoint: BinaryPoint): FixedPoint = {
    val _width = if (width.known) width else (1 + value.bitLength).W
    new FixedPoint(_width, binaryPoint).Lit(_.data -> value.S(_width))
  }

  /** Create a FixedPoint bundle with its data port connected to an SInt literal
    */
  private[fixedpoint] def fromData(
    binaryPoint: BinaryPoint,
    data:        Data,
    widthOption: Option[Width] = None
  )(
    implicit sourceInfo: SourceInfo
  ): FixedPoint = {
    val _new = Wire(
      FixedPoint(
        widthOption.getOrElse(recreateWidth(data)),
        binaryPoint
      )
    )
    _new.data := data.asTypeOf(_new.data)
    _new
  }

  private[fixedpoint] def recreateWidth[T <: Data](d: T): Width = {
    d.widthOption.fold[Width](UnknownWidth)(_.W)
  }

  /** Align all FixedPoints in a (possibly heterogeneous) sequence by width and binary point
    */
  private[fixedpoint] def dataAligned[T <: Data](in: Iterable[T])(implicit sourceInfo: SourceInfo): Seq[T] = {
    val bps = in.collect {
      case el: FixedPoint =>
        el.requireKnownBP()
        el.binaryPoint
    }

    val out =
      if (bps.isEmpty) in
      else {
        val maxBP = bps.fold(0.BP)(_.max(_))
        val maxWidth = in.map {
          case el: FixedPoint => recreateWidth(el) + (maxBP.get - el.binaryPoint.get)
          case nonFp => recreateWidth(nonFp)
        }.fold(0.W)(_.max(_))

        in.map {
          case el: FixedPoint =>
            val shift = maxBP.get - el.binaryPoint.get
            fromData(
              maxBP,
              if (shift > 0) el.data << shift else el.data,
              Some(maxWidth)
            ).asInstanceOf[T]
          case nonFp => nonFp
        }
      }
    out.toSeq
  }

  private[fixedpoint] def dataAligned(in: FixedPoint*)(implicit sourceInfo: SourceInfo): Seq[FixedPoint] =
    dataAligned(in)

  class ImplicitsCls private[fixedpoint] {

    implicit class fromDoubleToLiteral(double: Double) {
      def F(binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromDouble(double, UnknownWidth, binaryPoint)
      }

      def F(width: Width, binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromDouble(double, width, binaryPoint)
      }
    }

    implicit class fromBigDecimalToLiteral(bigDecimal: BigDecimal) {
      def F(binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromBigDecimal(bigDecimal, UnknownWidth, binaryPoint)
      }

      def F(width: Width, binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromBigDecimal(bigDecimal, width, binaryPoint)
      }
    }
  }

  object Implicits extends ImplicitsCls

}

sealed class FixedPoint private[fixedpoint] (width: Width, private var _inferredBinaryPoint: BinaryPoint)
    extends Record
    with OpaqueType
    with Num[FixedPoint]
    with HasBinaryPoint {
  if (binaryPoint.known) require(binaryPoint.get >= 0, "Negative binary point is not supported")
  private val data: SInt = SInt(width)
  val elements:     SeqMap[String, SInt] = SeqMap("" -> data)

  def binaryPoint: BinaryPoint = _inferredBinaryPoint

  private def requireKnownBP(message: String = "Unknown binary point is not supported in this operation"): Unit =
    if (!binaryPoint.known) throw new ChiselException(message)

  private def additiveOp(that: FixedPoint, f: (SInt, SInt) => SInt)(implicit sourceInfo: SourceInfo): FixedPoint = {
    val Seq(_this, _that) = FixedPoint.dataAligned(this, that).map(WireDefault(_))
    FixedPoint.fromData(binaryPoint.max(that.binaryPoint), f(_this.data, _that.data))
  }

  private def comparativeOp(that: FixedPoint, f: (SInt, SInt) => Bool): Bool = {
    val Seq(_this, _that) = FixedPoint.dataAligned(this, that).map(WireDefault(_))
    f(_this.data, _that.data)
  }

  private def connectOp(that: Data, c: (Data, Data) => Unit)(implicit sourceInfo: SourceInfo): Unit = {
    that match {
      case that: FixedPoint =>
        if (binaryPoint.known) {
          c(data, that.setBinaryPoint(binaryPoint.get).data)
        } else {
          if (that.binaryPoint.known) {
            this._inferredBinaryPoint = BinaryPoint(that.binaryPoint.get)
          }
          c(data, that.data)
        }
      case that @ DontCare =>
        c(data, that)
      case _ => throw new ChiselException(s"Cannot connect ${this} and ${that}")
    }
  }

  override def do_+(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = additiveOp(that, _ + _)

  override def do_-(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = additiveOp(that, _ - _)

  def do_+%(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = additiveOp(that, _ +% _)

  def do_+&(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = additiveOp(that, _ +& _)

  def do_-%(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = additiveOp(that, _ -% _)

  def do_-&(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = additiveOp(that, _ -& _)

  def do_unary_-(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, -data)

  def do_unary_-%(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data.unary_-%)

  override def do_*(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(binaryPoint + that.binaryPoint, data * that.data)

  override def do_/(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    throw new ChiselException(s"division is illegal on FixedPoint types")

  override def do_%(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    throw new ChiselException(s"mod is illegal on FixedPoint types")

  override def do_<(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool = comparativeOp(that, _ < _)

  override def do_<=(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool = comparativeOp(that, _ <= _)

  override def do_>(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool = comparativeOp(that, _ > _)

  override def do_>=(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool = comparativeOp(that, _ >= _)

  override def do_abs(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data.abs)

  def do_floor(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()
    // Set the fractional part to zeroes
    val floored = Cat(data >> binaryPoint.get, 0.U(binaryPoint.get.W.min(width)))
    FixedPoint.fromData(binaryPoint, floored, Some(width))
  }

  def do_ceil(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()
    // Get a number with the fractional part set to ones
    val almostOne = ((1 << binaryPoint.get) - 1).S
    // Add it to the number and floor it
    val ceiled = (this + FixedPoint.fromData(binaryPoint, almostOne)).floor
    FixedPoint.fromData(binaryPoint, ceiled, Some(width))
  }

  def do_round(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()
    // Add 0.5 to the number and then floor it
    val rounded = (this + 0.5.F(1.BP)).floor.setBinaryPoint(binaryPoint.get)
    FixedPoint.fromData(binaryPoint, rounded, Some(width))
  }

  def do_===(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool = comparativeOp(that, _ === _)

  def do_=/=(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool = comparativeOp(that, _ =/= _)

  def do_!=(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool = comparativeOp(that, _ =/= _)

  def do_>>(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data >> that)

  def do_>>(that: BigInt)(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data >> that)

  def do_>>(that: UInt)(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data >> that)

  def do_<<(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data << that)

  def do_<<(that: BigInt)(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data << that)

  def do_<<(that: UInt)(implicit sourceInfo: SourceInfo): FixedPoint = FixedPoint.fromData(binaryPoint, data << that)

  def +%(that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def +&(that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def -%(that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def -&(that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def unary_- : FixedPoint = macro SourceInfoTransform.noArg

  def unary_-% : FixedPoint = macro SourceInfoTransform.noArg

  def floor: FixedPoint = macro SourceInfoTransform.noArg

  def ceil: FixedPoint = macro SourceInfoTransform.noArg

  def round: FixedPoint = macro SourceInfoTransform.noArg

  def ===(that: FixedPoint): Bool = macro SourceInfoTransform.thatArg

  def =/=(that: FixedPoint): Bool = macro SourceInfoTransform.thatArg

  def !=(that: FixedPoint): Bool = macro SourceInfoTransform.thatArg

  def >>(that: Int): FixedPoint = macro SourceInfoWhiteboxTransform.thatArg

  def >>(that: BigInt): FixedPoint = macro SourceInfoWhiteboxTransform.thatArg

  def >>(that: UInt): FixedPoint = macro SourceInfoWhiteboxTransform.thatArg

  def <<(that: Int): FixedPoint = macro SourceInfoWhiteboxTransform.thatArg

  def <<(that: BigInt): FixedPoint = macro SourceInfoWhiteboxTransform.thatArg

  def <<(that: UInt): FixedPoint = macro SourceInfoWhiteboxTransform.thatArg

  override def connect(that: Data)(implicit sourceInfo: SourceInfo): Unit = connectOp(that, _ := _)

  override def bulkConnect(that: Data)(implicit sourceInfo: SourceInfo): Unit = connectOp(that, _ <> _)

  def apply(x: BigInt): Bool = data.apply(x)

  def apply(x: Int): Bool = data.apply(x)

  def apply(x: UInt): Bool = data.apply(x)

  def apply(x: Int, y: Int): UInt = data.apply(x, y)

  def apply(x: BigInt, y: BigInt): UInt = data.apply(x, y)

  def extract(x: BigInt): Bool = data.extract(x)

  def extract(x: UInt): Bool = data.extract(x)

  final def asSInt: SInt = data.asSInt

  def do_asFixedPoint(binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP(s"cannot call $this.asFixedPoint(binaryPoint=$binaryPoint), you must specify a known binaryPoint")
    FixedPoint.fromData(binaryPoint, data, Some(width))
  }

  def do_setBinaryPoint(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP(s"cannot set new binary point if current binary point is unknown")
    val diff = that - binaryPoint.get
    FixedPoint.fromData(
      that.BP,
      if (diff > 0) data << diff
      else if (diff < 0) data >> -diff
      else data
    )
  }

  final def asFixedPoint(that: BinaryPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def setBinaryPoint(that: Int): FixedPoint = macro SourceInfoTransform.thatArg

  def widthKnown: Boolean = data.widthKnown

  override def litOption: Option[BigInt] = data.litOption

  override def litValue: BigInt = data.litValue

  override def toString: String = {
    litToDoubleOption match {
      case Some(value) => s"FixedPoint$width$binaryPoint($value)"
      case _           =>
        // Can't use stringAccessor so will have to extract from data field's toString...
        val suffix = ".*?([(].*[)])".r
          .findFirstMatchIn(data.toString)
          .fold("")(_.group(1))
        s"FixedPoint$width$binaryPoint$suffix"
    }
  }
}
