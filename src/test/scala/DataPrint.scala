// SPDX-License-Identifier: Apache-2.0

import chisel3.experimental.BundleLiterals._
import circt.stage.ChiselStage
import chisel3._
import fixedpoint._
import org.scalatest.matchers.should.Matchers

class DataPrintSpec extends ChiselFlatSpec with Matchers {
  object EnumTest extends ChiselEnum {
    val sNone, sOne, sTwo = Value
  }

  class PartialBundleTest extends Bundle {
    val a = UInt(8.W)
    val b = Bool()
    val c = SInt(8.W)
    val e = FixedPoint(5.W, 3.BP)
    val f = EnumTest.Type()
  }

  "Data types" should "have a meaningful string representation" in {
    ChiselStage.emitCHIRRTL {
      new RawModule {
        FixedPoint(5.W, 3.BP).toString should be("FixedPoint<5><<3>>")
      }
    }
  }

  "Literals" should "have a meaningful string representation" in {
    ChiselStage.emitCHIRRTL {
      new RawModule {
        2.25.F(6.W, 2.BP).toString should be("FixedPoint<6><<2>>(2.25)")
        (-2.25).F(6.W, 2.BP).toString should be("FixedPoint<6><<2>>(-2.25)")
        (new PartialBundleTest).Lit().toString should be(
          "PartialBundleTest(a=UInt<8>(DontCare), b=Bool(DontCare), c=SInt<8>(DontCare), e=FixedPoint<5><<3>>(DontCare), f=EnumTest(DontCare))"
        )
      }
    }
  }
}
