package fpu

import chisel3._

class GPT extends Bundle {
    val g = UInt(1.W)
    val p = UInt(1.W)
    val t = UInt(1.W)
}

object GPTInit {
    def apply(g: UInt, p: UInt, t: UInt) = {
        val bun = Wire(new GPT)
        bun.g := g
        bun.p := p
        bun.t := t
        bun
    }
}

class GPTGen(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))

        val gpts = Output(Vec(width, new GPT))
    })

    val as = io.a.asBools
    val bs = io.b.asBools
    io.gpts := VecInit(as.zip(bs)
                .map{ case (ai, bi) => (ai & bi, ai || bi, ai ^ bi) }
                .map{ 
                    case x => {
                        GPTInit(x._1, x._2, x._3)
                    }
                })
}

object GPTGen {
    def apply(width: Int) (a: UInt, b: UInt) = {
        val mod = Module (new GPTGen(width))
        mod.io.a := a
        mod.io.b := b
        mod.io.gpts
    }
}

class ReduceNibble extends Module {
    val io = IO(new Bundle {
        val gptin = Input(Vec(4, new GPT))
        val gptout = Output(Vec(4, new GPT))
    })

    val gptin0 = io.gptin(0)
    val gptout0 = gptin0

    val gptin1 = io.gptin(1)
    val gptout1 = GPTInit(gptin1.g | (gptin0.g & gptin1.p), gptin1.p & gptin0.p, gptin1.t)

    val gptin2 = io.gptin(2)
    val gptout2 = GPTInit(gptin2.g | (gptin2.p & gptin1.g) | (gptin0.g & gptin1.p & gptin2.p), gptin2.p & gptin1.p & gptin0.p, gptin2.t)

    val gptin3 = io.gptin(3)
    val gptout3 = GPTInit(gptin3.g | (gptin3.p & gptin2.g) | (gptin3.p & gptin2.p & gptin1.g) | (gptin0.g & gptin1.p & gptin2.p & gptin3.p), gptin3.p & gptin2.p & gptin1.p & gptin0.p, gptin3.t)

    io.gptout := VecInit(gptout0, gptout1, gptout2, gptout3)
}

object ReduceNibble {
    def apply(gptin: Vec[GPT]) = {
        val mod = Module( new ReduceNibble )
        mod.io.gptin := gptin
        mod.io.gptout
    }
}