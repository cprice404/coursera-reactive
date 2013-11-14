package simulations

import common._
import simulations.Wire

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig)}
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1Inv, a2Inv, a1InvAnda2Inv = new Wire
    inverter(a1, a1Inv)
    inverter(a2, a2Inv)
    andGate(a1Inv, a2Inv, a1InvAnda2Inv)
    inverter(a1InvAnda2Inv, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    if (out.length != math.pow(2, c.length))
      throw new IllegalArgumentException(s"Demultiplexer: mismatch between number of control wires (${c.length}) and number of output wires (${out.length})")

    if (c.length == 0) {
      orGate(in, in, out.head)
    } else if (c.length == 1) {
      val c0 = c(0)
      val o1 = out(0)
      val o0 = out(1)
      val cInv = new Wire
      andGate(in, c0, o1)
      inverter(c0, cInv)
      andGate(in, cInv, o0)
    } else if (c.length == 2) {
      val c2 = c(0)
      val c1 = c(1)
      val o3 = out(0)
      val o2 = out(1)
      val o1 = out(2)
      val o0 = out(3)

      val c1Inv, c2Inv = new Wire
      inverter(c2, c2Inv)
      inverter(c1, c1Inv)

      val o3input = new Wire
      andGate(c1, c2, o3input)
      andGate(in, o3input, o3)

      val o2input = new Wire
      andGate(c2, c1Inv, o2input)
      andGate(in, o2input, o2)

      val o1input = new Wire
      andGate(c1, c2Inv, o1input)
      andGate(in, o1input, o1)

      val o0input = new Wire
      andGate(c1Inv, c2Inv, o0input)
      andGate(in, o0input, o0)

    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
