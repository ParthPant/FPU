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
