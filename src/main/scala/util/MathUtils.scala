package fpu

import chisel3._
import chisel3.experimental.BundleLiterals._

class GPT extends Bundle {
    val g = UInt(1.W)
    val p = UInt(1.W)
    val t = UInt(1.W)

    def dot (that: GPT) : GPT = GPTInit(this.g | (this.p & that.g), this.p & that.p, this.t)
}

object GPTInit {
    def apply(g: UInt, p: UInt, t: UInt) : GPT = {
        val bun = Wire(new GPT)
        bun.g := g
        bun.p := p
        bun.t := t
        bun
    }
}

class FloatingPoint extends Bundle {
    val significand = UInt(24.W)
    val exp = UInt(8.W)
    val sign = UInt(1.W)
}

object FloatingPoint {
    def apply(sign: UInt, mant: UInt, exp: UInt) : FloatingPoint = {
        val bun = Wire(new FloatingPoint)
        bun.sign := sign
        bun.significand := mant
        bun.exp := exp
        bun
    }

    def open(fp: FloatingPoint) : Float = {
        val sign = fp.sign.litValue
        val exp = fp.exp.litValue
        val mant = fp.significand.litValue

        val res = (sign<<31) | (exp<<23) | (mant & 0x7FFFFF)
        java.lang.Float.intBitsToFloat(res.toInt)
    }

    def make(num: Float) = {
        val n = java.lang.Float.floatToIntBits(num.abs)
        val sign = if (num < 0) 1 else 0
        val exp = (n>>23) & 0xFF
        val mant = (1<<23) | (n&0x7FFFFF)

        val floatingPoint = new FloatingPoint 
        floatingPoint.Lit(_.sign -> sign.U, _.exp -> exp.U, _.significand -> mant.U)
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

class ReduceGroup (val valency: Int) extends Module {
    val io = IO(new Bundle {
        val gptin = Input(Vec(valency, new GPT))
        val gptout = Output(Vec(valency, new GPT))
    })

    val gptout0 = io.gptin(0)
    val gptout1 = io.gptin(1) dot io.gptin(0)
    val gptout2 = io.gptin(2) dot io.gptin(1) dot io.gptin(0)
    val gptout3 = io.gptin(3) dot io.gptin(2) dot io.gptin(1) dot io.gptin(0)

    io.gptout := VecInit(gptout0, gptout1, gptout2, gptout3)
}

object ReduceGroup {
    def apply(valency: Int) (gptin: Seq[GPT]) : Vec[GPT] = {
        val mod = Module( new ReduceGroup(valency) )
        mod.io.gptin := VecInit(gptin)
        mod.io.gptout
    }
}
