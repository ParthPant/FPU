package fpu

import chisel3._

class FA extends Module {
    val io = IO(new Bundle {
        val a = Input(Bool())
        val b = Input(Bool())
        val cin = Input(Bool())

        val s = Output(Bool())
        val cout = Output(Bool())
    })

    val axorb = io.a ^ io.b

    io.s := axorb ^ io.cin
    io.cout := (io.a & io.b) || (axorb & io.cin)
}

object FA {
    def apply(a: Bool, b: Bool, cin: Bool) = {
        val fa = Module (new FA)
        fa.io.a := a
        fa.io.b := b
        fa.io.cin := cin

        (fa.io.s, fa.io.cout)
    }
}

class RippleCarryAdder(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val s = Output(UInt(width.W))
        val cout = Output(UInt(1.W))
    })

    val ss = Wire(Vec(width, Bool()))
    val cs = Wire(Vec(width, Bool()))

    for (i <- 0 until width) {
        val (si, ci) = FA(io.a(i).asBool, io.b(i).asBool, if (i == 0) io.cin.asBool else cs(i-1))
        ss(i) := si
        cs(i) := ci
    }

    io.s := ss.asUInt
    io.cout := cs.last.asUInt
}

object RippleCarryAdder {
    def apply (width: Int) (a: UInt, b: UInt, cin: UInt) = {
        val mod = Module(new RippleCarryAdder(width))
        mod.io.a := a
        mod.io.b := b
        mod.io.cin := cin
        (mod.io.s, mod.io.cout)
    }
}

class FastAdderPipelined(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val Sum = Output(UInt(width.W))
        val Cout = Output(UInt(1.W))
    })

    val valency = 4

    val gpts_0 = GPTGen(width)(io.a, io.b)
    val groups = gpts_0.grouped(valency).toVector.map{
        case group => ReduceGroup(valency)(group)
    }.flatten

    def reduce(gpts: Seq[GPT], cin: Bool) : (UInt, UInt) = {
        def nextLayer(prev: Seq[GPT], cin: Bool, groupoffset: Int) : (UInt, UInt) = {
            if (valency*groupoffset > prev.size) {
                val gs = prev.map(_.g)
                val ps = prev.map(_.p)
                val ts = prev.map(_.t)

                val cs = cin +: gs.zip(ps).map{ case (gi, pi) => gi | (pi & cin) }
                val ss = for (i <- 0 until width) yield {
                    cs(i) ^ ts(i)
                }
                (VecInit(ss).asUInt, cs.last.asUInt)
            } else {
                val layer = VecInit(Vector.tabulate(prev.size) { i =>
                    if (i < valency*groupoffset) {
                        prev(i)
                    } else {
                        // println(s"$i -> ${i - i%4 - groupoffset*4 + 4 - 1}")
                        val (gpti, gptj) = (prev(i), prev(i - i%valency - groupoffset*valency + valency - 1))
                        GPTInit(gpti.g | (gpti.p & gptj.g), gpti.p & gptj.p, gpti.t)
                    }
                })

                val reg = RegNext(layer)
                val cinreg = RegNext(cin)
                nextLayer(reg, cinreg, groupoffset<<1)
            }
        }

        nextLayer(gpts, cin, 1)
    }
    
    val (sum, cout) = reduce(groups, io.cin.asBool)
    io.Sum := sum
    io.Cout := cout
}

object FastAdderPipelined {
    def apply (width: Int) (a: UInt, b: UInt, cin: UInt) : (UInt, UInt) = {
        val mod = Module (new FastAdderPipelined(width))
        mod.io.a := a
        mod.io.b := b
        mod.io.cin := cin

        (mod.io.Sum, mod.io.Cout)
    }
}
