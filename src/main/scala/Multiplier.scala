package fpu

import chisel3._
import chisel3.util._

class Multiplier(val w: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(w.W))
        val x = Input(UInt(w.W))
        val p = Output(UInt((2*w).W))
        val carry = Output(UInt(1.W))
        val done = Output(Bool())
    })

    val addState = RegInit(0.B)
    val startState = RegInit(1.B)
    val partialProd = RegInit(0.U((2*w).W))
    val multiplier = RegInit(0.U(w.W))
    val adder = Module(new KoggeStoneAdder(w))
    val prevCarry = RegInit(0.U(1.W))
    val bitCount = RegInit(w.U)

    val sum = adder.io.sum

    adder.io.a := partialProd(2*w-1, w)
    adder.io.b := Mux(multiplier(0), io.a, 0.U)
    adder.io.cin := prevCarry 
    io.carry := prevCarry
    prevCarry := adder.io.cout
    io.p := partialProd
    startState := 0.B
    addState := !addState
    io.done := Mux(bitCount===0.U, 1.U, 0.U)

    when (startState) {
        multiplier := io.x
    } .otherwise {
        when (addState) {
            partialProd := Cat(adder.io.sum, partialProd(w-1, 0))
        } .otherwise {
            partialProd := partialProd >> 1
            multiplier := multiplier >> 1
            bitCount := bitCount - 1.U
        }
    }
}