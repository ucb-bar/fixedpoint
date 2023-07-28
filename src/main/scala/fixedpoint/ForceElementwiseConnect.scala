// SPDX-License-Identifier: Apache-2.0

// This file contains the definition of a trait a Bundle/Record needs to extend if it containts FixedPoints
// in order to make connections work properly

package fixedpoint

import chisel3.{ChiselException, CompileOptions, Data, DontCare, Record}
import chisel3.experimental.SourceInfo

import scala.reflect.ClassTag

trait ForceElementwiseConnect[T <: Record] extends Record {
  implicit val ct: ClassTag[T]
  private def connectOp(
    that: Data,
    c:    (Data, Data) => Unit
  )(
    implicit sourceInfo:   SourceInfo,
    connectCompileOptions: CompileOptions
  ): Unit =
    that match {
      case that: T =>
        this.elements.zip(that.elements).foreach(x => c(x._1._2, x._2._2))
      case that @ DontCare =>
        this.elements.foreach(x => c(x._2, that))
      case _ =>
        throw new ChiselException(s"Cannot connect ${this} and ${that}")
    }

  override def connect(that: Data)(implicit sourceInfo: SourceInfo, connectCompileOptions: CompileOptions): Unit =
    connectOp(that, _ := _)(sourceInfo, connectCompileOptions)

  override def bulkConnect(that: Data)(implicit sourceInfo: SourceInfo, connectCompileOptions: CompileOptions): Unit =
    connectOp(that, _ <> _)(sourceInfo, connectCompileOptions)
}
