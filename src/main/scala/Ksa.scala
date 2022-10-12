package fpu

import chisel3._

// implementation directly taken from https://raw.githubusercontent.com/chisel-lang/ccc19/master/jack-higher.pdf

trait PrefixSum {
    def apply[T](summands: Seq[T]) (associativeOp: (T, T) => T) : Vector[T]
}

object DensePrefixSum extends PrefixSum {
    def apply[T](summands: Seq[T]) (associativeOp: (T, T) => T) : Vector[T] = {
        def helper(offset: Int, x: Vector[T]) : Vector[T] = {
            if (offset >= x.size) {
                x
            } else {
                val layer = Vector.tabulate(x.size) { i =>
                    if (i < offset) {
                        x(i)
                    } else {
                        associativeOp(x(i-offset), x(i))
                    }
                }
                helper(offset << 1, layer)
            }
        }

        helper(1, summands.toVector)
    }
}

class KoggeStoneAdder(val width: Int) extends Module {
    require(width > 0)
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val sum = Output(UInt(width.W))
        val cout = Output(UInt(1.W))
    })

    val as = io.a.asBools
    val bs = io.b.asBools
    
    val pairs = (false.B, io.cin.asBool) +: as.zip(bs).map { case (a, b) => (a ^ b, a && b) }
    val pgs = DensePrefixSum(pairs) {
        case ((pp, gp), (pi, gi)) => (pi && pp, (pi && gp) || gi)
    }

    val cs = pgs.map(_._2)
    val ps = pairs.map(_._1).drop(1) :+ false.B

    val ss = ps.zip(cs).map {case (p, c) => p ^ c}

    io.sum := VecInit(ss.slice(0, width)).asUInt
    io.cout := ss(width).asUInt
}
