package fpu

import chisel3._
import chisel3.experimental.BundleLiterals._

class CAS extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val ctrl = Input(Bool())
    val cin = Input(Bool())

    val S = Output(Bool())
    val Cout = Output(Bool())
  })

  val (s, cout) = FA(io.a ^ io.ctrl, io.b, io.cin)
  io.S := s
  io.Cout := cout
}

object CAS {
  def apply(a: Bool, b: Bool, ctrl: Bool, cin: Bool): (Bool, Bool) = {
    val mod = Module(new CAS)
    mod.io.a := a
    mod.io.b := b
    mod.io.ctrl := ctrl
    mod.io.cin := cin
    (mod.io.S, mod.io.Cout)
  }
}

class CSub extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val ctrl = Input(Bool())
    val cin = Input(Bool())

    val S = Output(Bool())
    val Cout = Output(Bool())
  })

  val (d, bout) = FS(io.a, io.b, io.cin)
  io.S := Mux(io.ctrl, d, io.a)
  io.Cout := bout
}

object CSub {
  def apply(a: Bool, b: Bool, ctrl: Bool, cin: Bool): (Bool, Bool) = {
    val mod = Module(new CSub)
    mod.io.a := a
    mod.io.b := b
    mod.io.ctrl := ctrl
    mod.io.cin := cin
    (mod.io.S, mod.io.Cout)
  }
}

class GPT extends Bundle {
  val g = UInt(1.W)
  val p = UInt(1.W)
  val t = UInt(1.W)

  def dot(that: GPT): GPT =
    GPTInit(this.g | (this.p & that.g), this.p & that.p, this.t)
}

object GPTInit {
  def apply(g: UInt, p: UInt, t: UInt): GPT = {
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
  def apply(sign: UInt, mant: UInt, exp: UInt): FloatingPoint = {
    val bun = Wire(new FloatingPoint)
    bun.sign := sign
    bun.significand := mant
    bun.exp := exp
    bun
  }

  def open(fp: FloatingPoint): Float = {
    val sign = fp.sign.litValue
    val exp = fp.exp.litValue
    val mant = fp.significand.litValue

    val res = (sign << 31) | (exp << 23) | (mant & 0x7fffff)
    java.lang.Float.intBitsToFloat(res.toInt)
  }

  def make(num: Float) = {
    val n = java.lang.Float.floatToIntBits(num.abs)
    val sign = if (num < 0) 1 else 0
    val exp = (n >> 23) & 0xff
    val mant = (1 << 23) | (n & 0x7fffff)

    val floatingPoint = new FloatingPoint
    floatingPoint.Lit(_.sign -> sign.U, _.exp -> exp.U, _.significand -> mant.U)
  }

}
