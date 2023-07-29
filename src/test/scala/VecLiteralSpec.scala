// SPDX-License-Identifier: Apache-2.0

import chisel3.experimental.VecLiterals._
import chisel3.testers.BasicTester
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._

import scala.language.reflectiveCalls

class VecLiteralSpec extends ChiselFreeSpec with Utils {
  //TODO: decide what behavior here should be
  "This doesn't work should it" ignore {
    assertTesterPasses {
      new BasicTester {
        def vecFactory = Vec(2, FixedPoint(8.W, 4.BP))

        val vecWire1 = Wire(Output(vecFactory))
        val vecLit1 = vecFactory.Lit(0 -> (1.5).F(8.W, 4.BP))
        val vecLit2 = vecFactory.Lit(1 -> (3.25).F(8.W, 4.BP))

        vecWire1 := vecLit1
        vecWire1 := vecLit2
        printf("vw1(0) %x  vw1(1) %x\n", vecWire1(0).asUInt, vecWire1(1).asUInt)
        chisel3.assert(vecWire1(0) === (1.5).F(8.W, 4.BP))
        chisel3.assert(vecWire1(1) === (3.25).F(8.W, 4.BP))
        stop()
      }
    }
  }

  "partially initialized Vec literals should assign" in {
    assertTesterPasses {
      new BasicTester {
        def vecFactory = Vec(2, FixedPoint(8.W, 4.BP))

        val vecWire1 = Wire(Output(vecFactory))
        val vecWire2 = Wire(Output(vecFactory))
        val vecLit1 = vecFactory.Lit(0 -> (1.5).F(8.W, 4.BP))
        val vecLit2 = vecFactory.Lit(1 -> (3.25).F(8.W, 4.BP))

        vecWire1 := vecLit1
        vecWire2 := vecLit2
        vecWire1(1) := (0.5).F(8.W, 4.BP)
        printf("vw1(0) %x  vw1(1) %x\n", vecWire1(0).asUInt, vecWire1(1).asUInt)
        chisel3.assert(vecWire1(0) === (1.5).F(8.W, 4.BP))
        chisel3.assert(vecWire1(1) === (0.5).F(8.W, 4.BP)) // Last connect won
        chisel3.assert(vecWire2(1) === (3.25).F(8.W, 4.BP))
        stop()
      }
    }
  }
}
