// SPDX-License-Identifier: Apache-2.0

import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import chisel3.experimental.BundleLiterals._
import chisel3.testers.BasicTester
import fixedpoint._

class BundleLiteralSpec extends ChiselFlatSpec with Utils {

  class LongBundle extends Bundle {
    val a = UInt(48.W)
    val b = SInt(32.W)
    val c = FixedPoint(16.W, 4.BP)
  }

  "bundle literals" should "pack" in {
    assertTesterPasses {
      new BasicTester {
        val longBundleLit =
          (new LongBundle).Lit(_.a -> 0xdeaddeadbeefL.U, _.b -> (-0x0beef00dL).S(32.W), _.c -> 4.5.F(16.W, 4.BP))
        longBundleLit.litOption should equal(
          Some(
            (BigInt(0xdeaddeadbeefL) << 48)
              + (BigInt(0xffffffffL - 0xbeef00dL + 1) << 16)
              + BigInt(72)
          )
        )
        chisel3.assert(longBundleLit.asUInt === longBundleLit.litOption.get.U)

        stop()
      }
    }
  }
}
