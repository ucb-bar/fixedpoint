// SPDX-License-Identifier: Apache-2.0

import chisel3.experimental.Analog
import chisel3.stage.ChiselStage
import chisel3.testers.BasicTester
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._

class CrossConnects(inType: Data, outType: Data) extends Module {
  val io = IO(new Bundle {
    val in = Input(inType)
    val out = Output(outType)
  })
  io.out := io.in
}

class CrossConnectTester(inType: Data, outType: Data) extends BasicTester {
  val dut = Module(new CrossConnects(inType, outType))
  dut.io := DontCare
  stop()
}

class ConnectSpec extends ChiselPropSpec with Utils {
  property("SInt := FixedPoint should fail") {
    intercept[ChiselException] {
      extractCause[ChiselException] {
        ChiselStage.elaborate { new CrossConnectTester(FixedPoint(16.W, 8.BP), UInt(16.W)) }
      }
    }
  }
  property("UInt := FixedPoint should fail") {
    intercept[ChiselException] {
      extractCause[ChiselException] {
        ChiselStage.elaborate { new CrossConnectTester(FixedPoint(16.W, 8.BP), UInt(16.W)) }
      }
    }
  }
  property("FixedPoint := FixedPoint should succeed") {
    assertTesterPasses { new CrossConnectTester(FixedPoint(16.W, 8.BP), FixedPoint(16.W, 8.BP)) }
  }
  property("FixedPoint := SInt should fail") {
    intercept[ChiselException] {
      extractCause[ChiselException] {
        ChiselStage.elaborate { new CrossConnectTester(SInt(16.W), FixedPoint(16.W, 8.BP)) }
      }
    }
  }
  property("FixedPoint := UInt should fail") {
    intercept[ChiselException] {
      extractCause[ChiselException] {
        ChiselStage.elaborate { new CrossConnectTester(UInt(16.W), FixedPoint(16.W, 8.BP)) }
      }
    }
  }
  property("Analog := FixedPoint should fail") {
    intercept[ChiselException] {
      extractCause[ChiselException] {
        ChiselStage.elaborate { new CrossConnectTester(Analog(16.W), FixedPoint(16.W, 8.BP)) }
      }
    }
  }
  property("FixedPoint := Analog should fail") {
    intercept[ChiselException] {
      extractCause[ChiselException] {
        ChiselStage.elaborate { new CrossConnectTester(FixedPoint(16.W, 8.BP), Analog(16.W)) }
      }
    }
  }
}
