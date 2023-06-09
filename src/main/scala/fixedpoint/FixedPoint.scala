// SPDX-License-Identifier: Apache-2.0

// This file contains the definitions of FixedPoint class and companion object. Much of Chisel's original code
// is reused, but Bundle is inherited from instead of Bits. Relevant methods from Bits and Chisel's FixedPoint
// have also been implemented in order to maximally replicate the original FixedPoint interface.

// Notes:
// - Not being able to extend cloneSuperType behavior makes it difficult to use user-defined FixedPoint with Muxes,
// and also to implement typeEquivalent fully
// - Not being able to extend MonoConnect behavior makes it difficult to properly connect FixedPoints with
// different BinaryPoints, especially if inside other Bundles and Vecs
// - Can't inherit from Record & OpaqueType yet because dsptools's iotesters-based tests break
// - Cannot Mux1H with aggregates with inferred widths

package fixedpoint

import chisel3.{fromIntToBinaryPoint => _, fromDoubleToLiteral => _, _}
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.internal.firrtl.Width
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.stage.ChiselStage
import fixedpoint.shadow.{Mux, Mux1H, MuxCase, MuxLookup, PriorityMux}

object FixedPoint extends NumObject {

  /** Create a FixedPoint type with inferred width. */
  def apply(): FixedPoint = apply(Width(), BinaryPoint())

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
    apply(value, Width(), binaryPoint)
  }

  /** Create a FixedPoint literal with inferred width from BigInt.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigInt(value: BigInt, width: Int, binaryPoint: Int): FixedPoint =
    if (width == -1) {
      apply(value, Width(), BinaryPoint(binaryPoint))
    } else {
      apply(value, Width(width), BinaryPoint(binaryPoint))
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
  def apply(value: BigInt, width: Width, binaryPoint: BinaryPoint): FixedPoint =
    new FixedPoint(width, binaryPoint).Lit(_.data -> value.S(width))

  /** Create a FixedPoint bundle with its data port connected to an SInt literal
    */
  private[fixedpoint] def fromData(width: Width, binaryPoint: BinaryPoint, data: SInt): FixedPoint = {
    val _new = Wire(FixedPoint(width, binaryPoint))
    _new.data := data
    _new
  }

  private def recreateWidth[T <: Data](d: T): Width = d.widthOption match {
    case Some(w) => w.W
    case None    => Width()
  }

  /** Align all FixedPoints in a (possibly heterogeneous) sequence by width and binary point
    */
  private[fixedpoint] def dataAligned[T <: Data](in: Iterable[T]): Seq[T] = {

    val bps = in.collect {
      case el: FixedPoint =>
        el.requireKnownBP()
        el.binaryPoint
    }

    val out: Iterable[T] = if (bps.nonEmpty) {
      val maxBP = bps.fold(0.BP)(_.max(_))
      val maxWidth = in.map { el =>
        val width = recreateWidth(el)
        val extra = el match {
          case el: FixedPoint => maxBP.get - el.binaryPoint.get
          case _ => 0
        }
        width + extra.W
      }.fold(0.W)(_.max(_))
      in.map {
        case el: FixedPoint =>
          val shift = maxBP.get - el.binaryPoint.get
          fromData(
            maxWidth,
            maxBP,
            (if (shift > 0) el.data << shift else el.data).asSInt
          ).asInstanceOf[T]
        case nonFp => nonFp
      }
    } else in
    out.toSeq
  }

  private[fixedpoint] def dataAligned(in: FixedPoint*): Seq[FixedPoint] = dataAligned(in)

  class ImplicitsCls private[fixedpoint] {

    implicit class fromDoubleToLiteral(double: Double) {
      def F(binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromDouble(double, Width(), binaryPoint)
      }

      def F(width: Width, binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromDouble(double, width, binaryPoint)
      }
    }

    implicit class fromBigDecimalToLiteral(bigDecimal: BigDecimal) {
      def F(binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromBigDecimal(bigDecimal, Width(), binaryPoint)
      }

      def F(width: Width, binaryPoint: BinaryPoint): FixedPoint = {
        FixedPoint.fromBigDecimal(bigDecimal, width, binaryPoint)
      }
    }
  }

  object Implicits extends ImplicitsCls

}

sealed class FixedPoint private[fixedpoint] (width: Width, private var _inferredBinaryPoint: BinaryPoint)
    extends Bundle
    with Num[FixedPoint]
    with HasBinaryPoint {
  val data:        SInt = SInt(width)
  def binaryPoint: BinaryPoint = _inferredBinaryPoint

  private def requireKnownBP(message: => Any = "Unknown binary point is not supported in this operation"): Unit = {
    require(_inferredBinaryPoint.isInstanceOf[KnownBinaryPoint], message)
  }

  private def additiveOp(that: FixedPoint, f: (SInt, SInt) => SInt, width: Width = Width()): FixedPoint = {
    val Seq(dis, dat) = FixedPoint.dataAligned(this, that)
    FixedPoint.fromData(width, _inferredBinaryPoint.max(that._inferredBinaryPoint), f(dis.data, dat.data))
  }

  private def comparativeOp(that: FixedPoint, f: (SInt, SInt) => Bool): Bool = {
    val Seq(dis, dat) = FixedPoint.dataAligned(this, that)
    WireDefault(f(dis.data, dat.data))
  }

  private def connectOp(
    that: Data,
    c:    (Data, Data) => Unit
  )(
    implicit sourceInfo:   SourceInfo,
    connectCompileOptions: CompileOptions
  ): Unit =
    that match {
      case that: FixedPoint =>
        if (_inferredBinaryPoint.isInstanceOf[KnownBinaryPoint]) {
          c(data, that.setBinaryPoint(_inferredBinaryPoint.get).data)
        } else {
          if (that._inferredBinaryPoint.isInstanceOf[KnownBinaryPoint]) {
            this._inferredBinaryPoint = BinaryPoint(that._inferredBinaryPoint.get)
          }
          c(data, that.data)
        }
      case that @ DontCare =>
        c(data, that)
      case _ => throw new ChiselException(s"Cannot connect ${this} and ${that}")
    }

  override def do_+(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    additiveOp(that, _ + _)

  override def do_-(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    additiveOp(that, _ - _)

  override def do_*(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint + that._inferredBinaryPoint, data * that.data)

  override def do_/(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throw new ChiselException(s"division is illegal on FixedPoint types")

  override def do_%(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throw new ChiselException(s"mod is illegal on FixedPoint types")

  override def do_<(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    comparativeOp(that, _ < _)

  override def do_<=(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    comparativeOp(that, _ <= _)

  override def do_>(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    comparativeOp(that, _ > _)

  override def do_>=(that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    comparativeOp(that, _ >= _)

  override def do_abs(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint, data.abs)

  def +%(that: FixedPoint): FixedPoint = additiveOp(that, _ +% _)
  def +&(that: FixedPoint): FixedPoint = additiveOp(that, _ +& _)
  def -%(that: FixedPoint): FixedPoint = additiveOp(that, _ -% _)
  def -&(that: FixedPoint): FixedPoint = additiveOp(that, _ -& _)
  def unary_-  : FixedPoint = FixedPoint.fromData(width, _inferredBinaryPoint, -data)
  def unary_-% : FixedPoint = FixedPoint.fromData(width, _inferredBinaryPoint, data.unary_-%)
  def ===(that: FixedPoint): Bool = comparativeOp(that, _ === _)
  def =/=(that: FixedPoint): Bool = comparativeOp(that, _ =/= _)
  def !=(that:  FixedPoint): Bool = comparativeOp(that, _ =/= _)

  def >>(that: Int): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint, (data >> that).asSInt)
  def >>(that: BigInt): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint, (data >> that).asSInt)
  def >>(that: UInt): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint, (data >> that).asSInt)
  def <<(that: Int): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint, (data << that).asSInt)
  def <<(that: BigInt): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint, (data << that).asSInt)
  def <<(that: UInt): FixedPoint =
    FixedPoint.fromData(Width(), _inferredBinaryPoint, (data << that).asSInt)

  override def connect(that: Data)(implicit sourceInfo: SourceInfo, connectCompileOptions: CompileOptions): Unit =
    connectOp(that, _ := _)(sourceInfo, connectCompileOptions)
  override def bulkConnect(that: Data)(implicit sourceInfo: SourceInfo, connectCompileOptions: CompileOptions): Unit =
    connectOp(that, _ <> _)(sourceInfo, connectCompileOptions)

  override def connectFromBits(
    that: Bits
  )(
    implicit sourceInfo: SourceInfo,
    compileOptions:      CompileOptions
  ): Unit = {
    this.data := that.asSInt
  }

  def apply(x:   BigInt): Bool = data.apply(x)
  def apply(x:   Int):    Bool = data.apply(x)
  def apply(x:   UInt): Bool = data.apply(x)
  def apply(x:   Int, y: Int): UInt = data.apply(x, y)
  def apply(x:   BigInt, y: BigInt): UInt = data.apply(x, y)
  def extract(x: BigInt): Bool = data.extract(x)
  def extract(x: UInt):   Bool = data.extract(x)

  final def asSInt: SInt = data.asSInt
  final def asFixedPoint(binaryPoint: BinaryPoint): FixedPoint = {
    binaryPoint match {
      case KnownBinaryPoint(_) =>
        FixedPoint.fromData(width, binaryPoint, data)
      case UnknownBinaryPoint =>
        throw new ChiselException(
          s"cannot call $this.asFixedPoint(binaryPoint=$binaryPoint), you must specify a known binaryPoint"
        )
    }
  }

  def setBinaryPoint(that: Int): FixedPoint = {
    _inferredBinaryPoint match {
      case KnownBinaryPoint(current) =>
        val diff = that - current
        FixedPoint.fromData(
          width + diff,
          that.BP,
          (if (diff >= 0) { data << diff }
           else { data >> -diff }).asSInt
        )
      case UnknownBinaryPoint =>
        throw new ChiselException(
          s"cannot set new binary point if current binary point is unknown"
        )
    }
  }

  def widthKnown: Boolean = data.widthKnown

  override def typeEquivalent(that: Data): Boolean = {
    // Can't compare binaryPoints since cloneSuperType doesn't work in case of user-defined FixedPoint
    this.getClass == that.getClass && FixedPoint.recreateWidth(this) == FixedPoint.recreateWidth(that)
  }

  override def litOption: Option[BigInt] = data.litOption

  override def toString: String = {
    litToDoubleOption match {
      case Some(value) => s"FixedPoint$width$binaryPoint($value)"
      case _           =>
        // Can't use stringAccessor so will have to extract from data field's toString()...
        val suffix = ".*?([(].*[)])".r.findFirstMatchIn(data.toString()) match {
          case Some(m) => m.group(1)
          case None    => ""
        }
        s"FixedPoint$width$binaryPoint$suffix"
    }
  }

}
