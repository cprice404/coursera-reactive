package simulations

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck._
import Prop._
import Gen._

class QuickCheckDemux extends Properties("CircuitSimulator") {

  case class DemuxCircuit(input: Wire, controls: List[Wire], outputs: List[Wire], circuit: CircuitSimulator) {
    override def toString() = {
      s"DemuxCircuit (${controls.length} control wires, ${outputs.length} output wires)"
    }
  }

  // TODO: we're limiting numCs to 10 since we have to create
  // 2^numCs Wires for the outputs.  This means that there are
  // really only 10 possible circuits to generate here, which
  // in turn means that it's pointless to run the tests more
  // than 10 times... but I'm a n00b and don't know how to
  // get ScalaCheck/Generators to work around this, and it
  // doesn't seem worth the effort atm.
  lazy val genDemuxCircuit: Gen[DemuxCircuit] = for {
    numCs <- choose[Int](0, 10)
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
    forAll { d: DemuxCircuit =>
      val DemuxCircuit(i, cs, os, circuit) = d

      i.setSignal(false)
      circuit.run
      os.map((w: Wire) => w.getSignal).forall(_ == false)
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
