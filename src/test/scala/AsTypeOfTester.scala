// SPDX-License-Identifier: Apache-2.0

import chisel3.testers.BasicTester
import chisel3._
import fixedpoint._

class AsTypeOfBundleTester extends BasicTester {
  class MultiTypeBundle extends Bundle {
    val u = UInt(4.W)
    val s = SInt(4.W)
    val fp = FixedPoint(4.W, 3.BP)
  }

  val bun = new MultiTypeBundle

  val bunAsTypeOf = ((4 << 8) + (15 << 4) + (12 << 0)).U.asTypeOf(bun)

  assert(bunAsTypeOf.u === 4.U)
  assert(bunAsTypeOf.s === -1.S)
  assert(bunAsTypeOf.fp === FixedPoint.fromDouble(-0.5, 4.W, 3.BP))

  stop()
}

class AsTypeOfSpec extends ChiselFlatSpec {
  behavior.of("asTypeOf")

  it should "work with Bundles containing Bits Types" in {
    assertTesterPasses { new AsTypeOfBundleTester }
  }
}
