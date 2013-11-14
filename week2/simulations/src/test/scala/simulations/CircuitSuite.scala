package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  def orGateTest(label: String, orGateFn: (Wire, Wire, Wire) => Unit) : Unit = {
    test(label + " example") {
      val in1, in2, out = new Wire
      orGateFn(in1, in2, out)
      in1.setSignal(false)
      in2.setSignal(false)
      run

      assert(out.getSignal === false, "or 1")

      in1.setSignal(true)
      run

      assert(out.getSignal === true, "or 2")

      in2.setSignal(true)
      run

      assert(out.getSignal === true, "or 3")

      in1.setSignal(false)
      run

      assert(out.getSignal === true, "or 4")

      in2.setSignal(false)
      run

      assert(out.getSignal === false, "or 5")
    }
  }

  orGateTest("orGate", orGate)
  orGateTest("orGate2", orGate2)


  def getOutSignal(w: Wire) : Int = w.getSignal match {
    case true => 1
    case false => 0
  }

  def getOutSignals(ws: List[Wire]) : List[Int] =
    ws map getOutSignal

  def checkOutputs(os: List[Wire], expected: List[Int], msg: String) : Unit = {
    assert(getOutSignals(os) === expected, msg)
  }


  test("demux 0-c example") {
    val in, out = new Wire
    val os = List(out)

    demux(in, List(), os)

    in.setSignal(false)
    run
    checkOutputs(os, List(0), "demux 0-c 1")

    in.setSignal(true)
    run
    checkOutputs(os, List(1), "demux 0-c 2")
  }


  test("demux 1-c example") {
    val in, c1, out0, out1 = new Wire
    val os = List(out1, out0)

    demux(in, List(c1), os)

    in.setSignal(false)
    c1.setSignal(false)
    run
    checkOutputs(os, List(0,0), "demux 1-c 1")

    in.setSignal(true)
    run
    checkOutputs(os, List(0,1), "demux 1-c 2")

    c1.setSignal(true)
    run
    checkOutputs(os, List(1,0), "demux 1-c 3")

    in.setSignal(false)
    run
    checkOutputs(os, List(0,0), "demux 1-c 4")
  }

  test("demux 2-c example") {
    val in, c1, c2, out0, out1, out2, out3 = new Wire
    val os = List(out3, out2, out1, out0)

    demux(in, List(c2, c1), List(out3, out2, out1, out0))

    in.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    run
    checkOutputs(os, List(0,0,0,0), "demux 2-c 1")

    in.setSignal(true)
    run
    checkOutputs(os, List(0,0,0,1), "demux 2-c 2")

    c2.setSignal(true)
    run
    checkOutputs(os, List(0,1,0,0), "demux 2-c 3")

    c1.setSignal(true)
    run
    checkOutputs(os, List(1,0,0,0), "demux 2-c 4")

    c2.setSignal(false)
    run
    checkOutputs(os, List(0,0,1,0), "demux 2-c 5")

    in.setSignal(false)
    run
    checkOutputs(os, List(0,0,0,0), "demux 2-c 6")
  }

}
