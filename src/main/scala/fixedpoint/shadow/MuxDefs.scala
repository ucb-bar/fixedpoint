// SPDX-License-Identifier: Apache-2.0

// This file contains shadowing definitions of chisel3's Mux objects which specifically handle the case
// of parameters containing FixedPoint objects.

package fixedpoint.shadow

import chisel3.{Bits, Bool, Data, EnumType, SourceInfoDoc, UInt}

object Mux extends SourceInfoDoc {
  def apply[T <: Data](cond: Bool, con: T, alt: T): T = {
    val Seq(con_processed, alt_processed) = Util.processArgs(Seq(con, alt))
    chisel3.Mux(cond, con_processed, alt_processed)
  }
}

object Mux1H {
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T = chisel3.util.Mux1H(sel, Util.processArgs(in))
  def apply[T <: Data](in:  Iterable[(Bool, T)]): T = chisel3.util.Mux1H(Util.processArgs(in))
  def apply[T <: Data](sel: UInt, in: Seq[T]): T = chisel3.util.Mux1H(sel, Util.processArgs(in))
  def apply(sel:            UInt, in: UInt):   Bool = chisel3.util.Mux1H(sel, in)
}

///** Builds a Mux tree under the assumption that multiple select signals
// * can be enabled. Priority is given to the first select signal.
// *
// * @example {{{
// * val hotValue = chisel3.util.PriorityMux(Seq(
// *  io.selector(0) -> 2.U,
// *  io.selector(1) -> 4.U,
// *  io.selector(2) -> 8.U,
// *  io.selector(4) -> 11.U,
// * ))
// * }}}
// * Returns the output of the Mux tree.
// */
object PriorityMux {
  def apply[T <: Data](in:  Seq[(Bool, T)]): T = chisel3.util.PriorityMux(Util.processArgs[Bool, T](in))
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T = chisel3.util.PriorityMux(sel, Util.processArgs(in))
  def apply[T <: Data](sel: Bits, in: Seq[T]): T = chisel3.util.PriorityMux(sel, in)
}

///** Creates a cascade of n Muxs to search for a key value.
// *
// * @example {{{
// * MuxLookup(idx, default,
// *     Array(0.U -> a, 1.U -> b))
// * }}}
// */
object MuxLookup {

  /** @param key a key to search for
    * @param default a default value if nothing is found
    * @param mapping a sequence to search of keys and values
    * @return the value found or the default if not
    */
  def apply[T <: Data](key: UInt, default: T)(mapping: Seq[(UInt, T)]): T = {
    val (proc_default, proc_mapping) = Util.processArgs(default, mapping)
    chisel3.util.MuxLookup(key, proc_default)(proc_mapping)
  }

  def apply[S <: EnumType, T <: Data](key: S, default: T)(mapping: Seq[(S, T)]): T = {
    val (proc_default, proc_mapping) = Util.processArgs(default, mapping)
    chisel3.util.MuxLookup(key, proc_default)(proc_mapping)
  }
}

///** Given an association of values to enable signals, returns the first value with an associated
// * high enable signal.
// *
// * @example {{{
// * MuxCase(default, Array(c1 -> a, c2 -> b))
// * }}}
// */
object MuxCase {

  /** @param default the default value if none are enabled
    * @param mapping a set of data values with associated enables
    * @return the first value in mapping that is enabled
    */
  def apply[T <: Data](default: T, mapping: Seq[(Bool, T)]): T = {
    val (proc_default, proc_mapping) = Util.processArgs(default, mapping)
    chisel3.util.MuxCase(proc_default, proc_mapping)
  }
}
