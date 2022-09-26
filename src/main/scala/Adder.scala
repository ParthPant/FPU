package fpu

import chisel3._

class FullAdder extends Module {
    val io = IO(new Bundle {
        val in1 = Input(UInt(1.W))
        val in2 = Input(UInt(1.W))
        val cin = Input(UInt(1.W))
        val sum = Output(UInt(1.W))
        val cout = Output(UInt(1.W))
    })

    val in1_xor_in2 = io.in1 ^ io.in2
    io.sum := in1_xor_in2 ^ io.cin
    io.cout := (io.in1 & io.in2) | (in1_xor_in2 & io.cin)
}

class Adder(val n:Int) extends Module {
    val io = IO(new Bundle {
        val in1 = Input(UInt(n.W))
        val in2 = Input(UInt(n.W))
        val cin = Input(UInt(1.W))
        val sum = Output(UInt(n.W))
        val cout = Output(UInt(1.W))
    })

    val FAs = Array.fill(n)(Module(new FullAdder()))
    val carry = Wire(Vec(n+1, UInt(1.W)))
    val sum = Wire(Vec(n, UInt(1.W)))

    carry(0) := io.cin

    for (i <- 0 until n) {
        FAs(i).io.in1 := io.in1(i)
        FAs(i).io.in2 := io.in2(i)
        FAs(i).io.cin := carry(i)
        carry(i+1) := FAs(i).io.cout
        sum(i) := FAs(i).io.sum
    }

    io.sum := sum.asUInt
    io.cout := carry(n)
}