// SPDX-License-Identifier: Apache-2.0

import chisel3.{SInt, UInt}

package object fixedpoint extends FixedPoint.ImplicitsCls {

  implicit class fromSIntToFixedPoint(sInt: SInt) {
    def asFixedPoint(binaryPoint: BinaryPoint): FixedPoint = FixedPoint.fromData(binaryPoint, sInt)
  }

  implicit class fromUIntToFixedPoint(uInt: UInt) {
    def asFixedPoint(binaryPoint: BinaryPoint): FixedPoint = FixedPoint.fromData(binaryPoint, uInt.asSInt)
  }

  implicit class fromIntToBinaryPoint(int: Int) {
    def BP: BinaryPoint = BinaryPoint(int)
  }

}
