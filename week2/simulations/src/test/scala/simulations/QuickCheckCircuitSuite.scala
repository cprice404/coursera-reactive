package simulations

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

class QuickCheckDemux extends Properties("CircuitSimulator") {

  case class DemuxCircuit(input: Wire, controls: List[Wire], outputs: List[Wire], circuit: CircuitSimulator) {
    override def toString() = {
      s"DemuxCircuit (${controls.length} control wires, ${outputs.length} output wires)"
    }
  }

  // TODO: we're limiting numCs to 10 since we have to create
  // 2^numCs Wires for the outputs.

  val MAX_CONTROL_WIRES = 10

  lazy val genDemuxCircuit: Gen[DemuxCircuit] = for {
    numCs <- choose[Int](0, MAX_CONTROL_WIRES)
  } yield {
    val in = new Wire
    val cs = List.fill(numCs)(new Wire)
    val os = List.fill(math.pow(2,numCs).toInt)(new Wire)
    val circuit = new CircuitSimulator {
      val InverterDelay: Int = 1
      val AndGateDelay: Int = 3
      val OrGateDelay: Int = 5
    }
    circuit.demux(in, cs, os)
    new DemuxCircuit(in, cs, os, circuit)
  }

  implicit lazy val arbDemux: Arbitrary[DemuxCircuit] = Arbitrary(genDemuxCircuit)


  property("all outputs are false if input is false") =
    forAll(genDemuxCircuit, Gen.listOfN(MAX_CONTROL_WIRES, arbitrary[Boolean])) {
      (d: DemuxCircuit, cVals: List[Boolean]) =>
        val DemuxCircuit(i, cs, os, circuit) = d

        i.setSignal(false)
        Range(0, cs.length).map((i) => {cs(i).setSignal(cVals(i))})
        circuit.run
        os.forall(_.getSignal == false)
    }

  property("exactly one output is true if input is true") =
    forAll(genDemuxCircuit, Gen.listOfN(MAX_CONTROL_WIRES, arbitrary[Boolean])) {
      (d: DemuxCircuit, cVals: List[Boolean]) =>
        val DemuxCircuit(i, cs, os, circuit) = d

        i.setSignal(true)
        Range(0, cs.length).map((i) => cs(i).setSignal(cVals(i)))
        circuit.run
        os.filter(_.getSignal == true).length == 1
    }

  property("if input is true, the correct output is true based on the control wires") =
    forAll(genDemuxCircuit,
            Gen.listOfN(MAX_CONTROL_WIRES, arbitrary[Boolean]),
            Gen.listOfN(MAX_CONTROL_WIRES, arbitrary[Boolean])) {
      (d: DemuxCircuit, initCVals: List[Boolean], nextCVals: List[Boolean]) =>
        val DemuxCircuit(i, cs, os, circuit) = d
        val revOs = os.reverse

        def hotO(controlWires: List[Wire]) = {
          cs.map(_.getSignal).
            map(if (_) 1 else 0).
            foldLeft(0)((sum, c) => (2 * sum) + c)
        }
        def expectedOutput(i:Int, expectedHot:Int) = {
          if (i == expectedHot) true else false
        }
        def isExpectedOutput(i:Int, expectedHot:Int) = {
//          println(s"Checking wire $i; expected hot wire: $expectedHot; this wire: ${revOs(i).getSignal}")
          revOs(i).getSignal == expectedOutput(i, expectedHot)
        }

        i.setSignal(true)
        Range(0, cs.length).map((i) => cs(i).setSignal(initCVals(i)))
        circuit.run
        val expectedHot = hotO(cs)
        val firstHotCheck = Range(0, revOs.length).forall(isExpectedOutput(_, expectedHot))

        Range(0, cs.length).map((i) => cs(i).setSignal(nextCVals(i)))
        circuit.run
        val expectedHot2 = hotO(cs)
        val secondHotCheck = Range(0, revOs.length).forall(isExpectedOutput(_, expectedHot2))

        i.setSignal(false)
        circuit.run
        val coolCheck = os.forall(_.getSignal == false)

        firstHotCheck & secondHotCheck & coolCheck
    }

}

@RunWith(classOf[JUnitRunner])
class QuickCheckCircuitSuite
  extends FunSuite
  with Checkers {

  test("QuickCheckDemux") {
    check(new QuickCheckDemux)
  }
}
