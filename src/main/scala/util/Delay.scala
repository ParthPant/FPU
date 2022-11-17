package fpu

import chisel3._

object Delay {
    def apply[T <: Data](data: T, t: Int): T = {
        def makereg(data: T, n: Int): T = {
            if (n < t)
                makereg(RegNext(data), n+1)
            else 
                data
        }
        makereg(data, 0)
    }
}
