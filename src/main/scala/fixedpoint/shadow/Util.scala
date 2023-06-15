// SPDX-License-Identifier: Apache-2.0

// Utility methods for the shadow package

package fixedpoint.shadow

import chisel3.Data
import fixedpoint.FixedPoint

object Util {
  def processArgs[T <: Data](in: Seq[T]): Seq[T] =
    FixedPoint.dataAligned(in)

  def processArgs[T <: Data, U <: Data](in: Iterable[(T, U)]): Seq[(T, U)] =
    in.map(_._1)
      .zip(FixedPoint.dataAligned(in.map(_._2)))
      .toSeq

  def processArgs[T <: Data, U <: Data](default: U, in: Seq[(T, U)]): (U, Seq[(T, U)]) = {
    val aligned = FixedPoint.dataAligned(default +: in.map(_._2))
    (aligned.head, in.map(_._1).zip(aligned.tail))
  }
}
