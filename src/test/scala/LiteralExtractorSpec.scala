// SPDX-License-Identifier: Apache-2.0

import chisel3.experimental.BundleLiterals._
import chisel3.testers.BasicTester
import chisel3._
import fixedpoint._

class LiteralExtractorSpec extends ChiselFlatSpec {
  "litValue" should "return the literal value" in {
    assert(1.25.F(2.BP).litValue === BigInt("101", 2))
    assert(2.25.F(2.BP).litValue === BigInt("1001", 2))
    assert(0.0625.F(2.W, 4.BP).litValue === BigInt("01", 2))

    assert(-1.25.F(2.BP).litValue === BigInt("-101", 2))
    assert(-2.25.F(2.BP).litValue === BigInt("-1001", 2))
    assert(-0.0625.F(2.W, 4.BP).litValue === BigInt("-01", 2))
  }

  "litToDouble" should "return the literal value" in {
    assert(1.25.F(2.BP).litToDouble == 1.25)
    assert(2.25.F(2.BP).litToDouble == 2.25)
    assert(0.0625.F(2.W, 4.BP).litToDouble == 0.0625)

    assert(-1.25.F(2.BP).litToDouble == -1.25)
    assert(-2.25.F(2.BP).litToDouble == -2.25)
    assert(-0.0625.F(2.W, 4.BP).litToDouble == -0.0625)

    // test rounding
    assert(1.24.F(1.BP).litToDouble == 1.0)
    assert(1.25.F(1.BP).litToDouble == 1.5)
    assert(0.0624.F(2.W, 3.BP).litToDouble == 0.0)
    assert(0.0625.F(2.W, 3.BP).litToDouble == 0.125)
  }

  "doubles and big decimals" should "round trip properly" in {
    intercept[ChiselException] {
      NumBP.toBigDecimal(BigInt("1" * 109, 2), 0.BP) // this only works if number takes less than 109 bits
    }

    intercept[ChiselException] {
      NumBP.toDouble(BigInt("1" * 54, 2), 0.BP) // this only works if number takes less than 54 bits
    }

    val bigInt108 = BigInt("1" * 108, 2)
    val bigDecimal = Num.toBigDecimal(bigInt108, 2)

    val bigIntFromBigDecimal = Num.toBigInt(bigDecimal, 2)

    bigIntFromBigDecimal should be(bigInt108)

    val bigInt53 = BigInt("1" * 53, 2)

    val double = Num.toDouble(bigInt53, 2)

    val bigIntFromDouble = Num.toBigInt(double, 2)

    bigIntFromDouble should be(bigInt53)
  }

  "literals declared outside a builder context" should "compare with those inside builder context" in {
    class InsideBundle extends Bundle {
      val x = FixedPoint(8.W, 4.BP)
    }

    class LitInsideOutsideTester(outsideLiteral: InsideBundle) extends BasicTester {
      val insideLiteral = (new InsideBundle).Lit(_.x -> 6.125.F(8.W, 4.BP))

      // the following errors with "assertion failed"

      println(outsideLiteral === insideLiteral)
      // chisel3.assert(outsideLiteral === insideLiteral)

      // the following lines of code error
      // with "chisel3.internal.BundleLitBinding cannot be cast to chisel3.internal.ElementLitBinding"

      chisel3.assert(outsideLiteral.x === insideLiteral.x)
      chisel3.assert(outsideLiteral.x === 6.125.F(4.BP))

      stop()
    }

    val outsideLiteral = (new InsideBundle).Lit(_.x -> 6.125.F(8.W, 4.BP))
    assertTesterPasses { new LitInsideOutsideTester(outsideLiteral) }

  }
}
