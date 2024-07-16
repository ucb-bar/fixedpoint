// SPDX-License-Identifier: Apache-2.0

import circt.stage.ChiselStage
import chisel3.testers.BasicTester
import chisel3.{Mux => _, _}
import fixedpoint._
import fixedpoint.shadow.Mux
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FixedPointLiteralSpec extends AnyFlatSpec with Matchers {
  behavior.of("fixed point utilities")

  they should "allow conversion between doubles and the bigints needed to represent them" in {
    val initialDouble = 0.125
    val bigInt = FixedPoint.toBigInt(initialDouble, 4)
    val finalDouble = FixedPoint.toDouble(bigInt, 4)

    initialDouble should be(finalDouble)
  }

  they should "have their literals support to double and to BigDecimal" in {
    val d = -7.125
    val lit1 = d.F(3.BP)
    lit1.litToDouble should be(d)

    val d2 = BigDecimal("1232123213131123.125")
    val lit2 = d2.F(3.BP)
    lit2.litToBigDecimal should be(d2)

    // Numbers that are too big will throw exception
    intercept[ChiselException] {
      lit2.litToDouble
    }
  }
}

//noinspection TypeAnnotation,EmptyParenMethodAccessedAsParameterless
class FixedPointFromBitsTester extends BasicTester {
  val uint = 3.U(4.W)
  val sint = (-3).S

  val fp = FixedPoint.fromDouble(3.0, 4.W, 0.BP)
  val fp_tpe = FixedPoint(4.W, 1.BP)
  val uint_result = FixedPoint.fromDouble(1.5, 4.W, 1.BP)
  val sint_result = FixedPoint.fromDouble(-1.5, 4.W, 1.BP)
  val fp_result = FixedPoint.fromDouble(1.5, 4.W, 1.BP)

  val uint2fp = uint.asTypeOf(fp_tpe)
  val sint2fp = sint.asTypeOf(fp_tpe)
  val fp2fp = fp.asTypeOf(fp_tpe)

  val uintToFp = uint.asFixedPoint(1.BP)
  val sintToFp = sint.asFixedPoint(1.BP)
  val fpToFp = fp.asFixedPoint(1.BP)

  val negativefp = (-3.5).F(4.BP)
  val positivefp = 3.5.F(4.BP)

  assert(-positivefp === negativefp)
  assert(positivefp === -negativefp)

  assert(uint2fp === uint_result)
  assert(sint2fp === sint_result)
  assert(fp2fp === fp_result)

  assert(uintToFp === uint_result)
  assert(sintToFp === sint_result)
  assert(fpToFp === fp_result)

  assert(positivefp.abs === positivefp)
  assert(negativefp.abs === positivefp)
  assert(negativefp.abs =/= negativefp)

  val f1bp5 = 1.5.F(1.BP)
  val f6bp0 = 6.0.F(0.BP)
  val f6bp2 = 6.0.F(2.BP)

  val f1bp5shiftleft2 = Wire(FixedPoint(UnknownWidth, BinaryPoint()))
  val f6bp0shiftright2 = Wire(FixedPoint(UnknownWidth, BinaryPoint()))
  val f6bp2shiftright2 = Wire(FixedPoint(UnknownWidth, BinaryPoint()))

  f1bp5shiftleft2 := f1bp5 << 2
  f6bp0shiftright2 := f6bp0 >> 2
  f6bp2shiftright2 := f6bp2 >> 2

  assert(f1bp5shiftleft2 === f6bp0)
  assert(f1bp5shiftleft2 === 6.0.F(8.BP))

  // shifting does not move binary point, so in first case below one bit is lost in shift
  assert(f6bp0shiftright2 === 1.0.F(0.BP))
  assert(f6bp2shiftright2 === 1.5.F(2.BP))

  stop()
}

class FixedPointMuxTester extends BasicTester {
  val largeWidthLowPrecision = 6.0.F(4.W, 0.BP)
  val smallWidthHighPrecision = 0.25.F(2.W, 2.BP)
  val unknownWidthLowPrecision = 6.0.F(0.BP)
  val unknownFixed = Wire(FixedPoint())
  unknownFixed := smallWidthHighPrecision

  assert(Mux(true.B, largeWidthLowPrecision, smallWidthHighPrecision) === 6.0.F(0.BP))
  assert(Mux(false.B, largeWidthLowPrecision, smallWidthHighPrecision) === 0.25.F(2.BP))
  assert(Mux(false.B, largeWidthLowPrecision, unknownFixed) === 0.25.F(2.BP))
  assert(Mux(true.B, unknownWidthLowPrecision, smallWidthHighPrecision) === 6.0.F(0.BP))

  stop()
}

class SBP extends Module {
  val io = IO(new Bundle {
    val in = Input(FixedPoint(6.W, 2.BP))
    val out = Output(FixedPoint(4.W, 0.BP))
  })
  io.out := io.in.setBinaryPoint(0)
}

class SBPTester extends BasicTester {
  val dut = Module(new SBP)
  dut.io.in := 3.75.F(2.BP)

  assert(dut.io.out === 3.0.F(0.BP))

  val test = Wire(FixedPoint(10.W, 5.BP))
  // Initialize test, avoiding a "Reference test is not fully initialized" error from firrtl.
  test := 0.0.F(5.BP)
  val q = test.setBinaryPoint(18)
  assert(q.getWidth.U === 23.U)

  stop()
}

class NegativeShift(t: => Bits) extends Module {
  val io = IO(new Bundle {})
  Reg(t) >> -1
}

class FixedPointLitExtractTester extends BasicTester {
  assert(-4.75.F(2.BP)(1) === false.B)
  assert(-4.75.F(2.BP)(2) === true.B)
  assert(-4.75.F(2.BP)(100) === true.B)
  assert(-4.75.F(2.BP)(3, 0) === "b1101".U)
  assert(-4.75.F(2.BP)(9, 0) === "b1111101101".U)

  assert(-4.75.F(6.W, 2.BP)(1) === false.B)
  assert(-4.75.F(6.W, 2.BP)(2) === true.B)
  assert(-4.75.F(6.W, 2.BP)(100) === true.B)
  assert(-4.75.F(6.W, 2.BP)(3, 0) === "b1101".U)
  assert(-4.75.F(6.W, 2.BP)(9, 0) === "b1111101101".U)

  assert(-0.125.F(2.W, 4.BP)(0) === false.B)
  assert(-0.125.F(2.W, 4.BP)(1) === true.B)
  assert(-0.125.F(2.W, 4.BP)(100) === true.B)
  assert(-0.125.F(2.W, 4.BP)(1, 0) === "b10".U)
  assert(0.0625.F(2.W, 4.BP)(0) === true.B)
  assert(0.0625.F(2.W, 4.BP)(1) === false.B)
  assert(0.0625.F(2.W, 4.BP)(100) === false.B)
  assert(0.0625.F(2.W, 4.BP)(1, 0) === "b01".U)
  stop()
}

class FixedPointUnaryFuncTester(f: FixedPoint => FixedPoint, inExpected: Seq[(FixedPoint, FixedPoint)])
    extends BasicTester {
  inExpected.foreach {
    case (in, expected) =>
      val out = f(in)
      assert(out === expected, cf"Wrong value: in=${in}; out=${out}; expected=${expected}")
      assert(out.widthOption == in.widthOption, f"Width changed: in=${in}; out=${out}")
      assert(out.binaryPoint == in.binaryPoint, f"Binary point changed: in=${in}; out=${out}")
  }
  stop()
}

class FixedPointFloorTester
    extends FixedPointUnaryFuncTester(
      _.floor,
      Seq(
        -4.75.F(8.W, 2.BP) -> -5.0.F(8.W, 2.BP),
        55.5.F(8.W, 2.BP) -> 55.0.F(8.W, 2.BP),
        -4.0.F(2.BP) -> -4.0.F(2.BP),
        0.125.F(8.W, 4.BP) -> 0.0.F(8.W, 4.BP),
        3.0.F(0.BP) -> 3.0.F(0.BP),
        // Overflow to zero when binaryPoint >= width
        0.25.F(2.W, 2.BP) -> 0.F(0.BP),
        -0.5.F(2.W, 2.BP) -> 0.F(0.BP),
        0.0625.F(2.W, 4.BP) -> 0.F(0.BP),
        -0.125.F(2.W, 4.BP) -> 0.F(0.BP)
      )
    )

class FixedPointCeilTester
    extends FixedPointUnaryFuncTester(
      _.ceil,
      Seq(
        -4.75.F(8.W, 2.BP) -> -4.0.F(8.W, 2.BP),
        55.5.F(8.W, 2.BP) -> 56.0.F(8.W, 2.BP),
        -4.0.F(2.BP) -> -4.0.F(2.BP),
        0.125.F(8.W, 4.BP) -> 1.0.F(8.W, 4.BP),
        3.0.F(0.BP) -> 3.0.F(0.BP),
        // Overflow to zero when binaryPoint >= width
        0.25.F(2.W, 2.BP) -> 0.F(0.BP),
        -0.5.F(2.W, 2.BP) -> 0.F(0.BP),
        0.0625.F(2.W, 4.BP) -> 0.F(0.BP),
        -0.125.F(2.W, 4.BP) -> 0.F(0.BP)
      )
    )

class FixedPointRoundTester
    extends FixedPointUnaryFuncTester(
      _.round,
      Seq(
        -4.75.F(8.W, 2.BP) -> -5.0.F(8.W, 2.BP),
        25.5.F(8.W, 2.BP) -> 26.0.F(8.W, 2.BP),
        -4.0.F(2.BP) -> -4.0.F(2.BP),
        0.125.F(8.W, 3.BP) -> 0.0.F(8.W, 3.BP),
        3.0.F(0.BP) -> 3.0.F(0.BP),
        // Overflow to zero when binaryPoint >= width
        0.25.F(2.W, 2.BP) -> 0.F(0.BP),
        -0.5.F(2.W, 2.BP) -> 0.F(0.BP),
        0.0625.F(2.W, 4.BP) -> 0.F(0.BP),
        -0.125.F(2.W, 4.BP) -> 0.F(0.BP)
      )
    )

class FixedPointSpec extends ChiselPropSpec with Utils {
  property("should allow set binary point") {
    assertTesterPasses { new SBPTester }
  }
  property("should allow fromBits") {
    assertTesterPasses { new FixedPointFromBitsTester }
  }
  property("should mux different widths and binary points") {
    assertTesterPasses { new FixedPointMuxTester }
  }
  property("Negative shift amounts are invalid") {
    a[ChiselException] should be thrownBy extractCause[ChiselException] {
      ChiselStage.emitCHIRRTL(new NegativeShift(FixedPoint(1.W, 0.BP).asSInt))
    }
  }
  property("Bit extraction on literals should work for all non-negative indices") {
    assertTesterPasses(new FixedPointLitExtractTester)
  }

  property("Floor operation works") {
    assertTesterPasses { new FixedPointFloorTester }
  }

  property("Ceil operation works") {
    assertTesterPasses { new FixedPointCeilTester }
  }

  property("Round operation works") {
    assertTesterPasses { new FixedPointRoundTester }
  }

  property("Negative binary point is invalid") {
    assertThrows[IllegalArgumentException](new BasicTester { 2.F((-1).BP) })
    assertThrows[IllegalArgumentException](new BasicTester { 1.F(0.BP).setBinaryPoint(-1) })
    assertThrows[IllegalArgumentException](new BasicTester { FixedPoint(4.W, (-2).BP) })
  }
}
