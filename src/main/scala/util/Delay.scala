package fpu.util

import chisel3._

object Delay {
  def apply[T <: Data](data: T, t: Int): T = {
    // assert(t > 0)
    (0 until t).foldLeft(data) { (prev, _) => RegNext(prev) }
  }
}
