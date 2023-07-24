// SPDX-License-Identifier: Apache-2.0

import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import chisel3.experimental.BundleLiterals._
import chisel3.testers.BasicTester
import fixedpoint._

class EqualityModule(lhsGen: => Data, rhsGen: => Data) extends Module {
  val out = IO(Output(Bool()))

  val lhs = lhsGen
  val rhs = rhsGen

  out := lhs === rhs
}

class EqualityTester(lhsGen: => Data, rhsGen: => Data) extends BasicTester {
  val module = Module(new EqualityModule(lhsGen, rhsGen))

  assert(module.out)

  stop()
}

class DataEqualitySpec extends ChiselFlatSpec with Utils {
  object MyEnum extends ChiselEnum {
    val sA, sB = Value
  }

  class MyBundle extends Bundle {
    val a = UInt(8.W)
    val b = Bool()
    val c = MyEnum()
  }

  class LongBundle extends Bundle {
    val a = UInt(48.W)
    val b = SInt(32.W)
    val c = FixedPoint(16.W, 4.BP)
  }

  class RuntimeSensitiveBundle(gen: => Bundle) extends Bundle {
    val a = UInt(8.W)
    val b: Bundle = gen
  }

  behavior.of("FixedPoint === FixedPoint")
  it should "pass with equal values" in {
    assertTesterPasses {
      new EqualityTester(4.5.F(16.W, 4.BP), 4.5.F(16.W, 4.BP))
    }
  }
  it should "fail with differing values" in {
    assertTesterFails {
      new EqualityTester(4.5.F(16.W, 4.BP), 4.6.F(16.W, 4.BP))
    }
  }

  behavior.of("Bundle === Bundle")
  it should "throw a ChiselException with differing runtime types" in {
    (the[ChiselException] thrownBy extractCause[ChiselException] {
      assertTesterFails {
        new EqualityTester(
          (new RuntimeSensitiveBundle(new MyBundle)).Lit(
            _.a -> 1.U,
            _.b -> (new MyBundle).Lit(
              _.a -> 42.U,
              _.b -> false.B,
              _.c -> MyEnum.sB
            )
          ),
          (new RuntimeSensitiveBundle(new LongBundle)).Lit(
            _.a -> 1.U,
            _.b -> (new LongBundle).Lit(
              _.a -> 42.U,
              _.b -> 0.S,
              _.c -> 4.5.F(16.W, 4.BP)
            )
          )
        )
      }
    }).getMessage should include("Runtime types differ")
  }
}
