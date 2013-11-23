package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("prevalence rate"){
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate,
      "prevalence rate should be 0.01"
      )
  }

  test("dead person stays dead"){
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val(row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while(!es.agenda.isEmpty && es.agenda.head.time < testDays){
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
    }
  }

  test("life cycle"){
    val es = new EpidemySimulator

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    val infectedPerson = (es.persons.find{_.infected}).get

    //before incubation time
    while(es.agenda.head.time < incubationTime){
      assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
    	assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
    	assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
    	assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
    	es.next
    }

    //incubation time has passed, there should be an event for getting sick
    assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
    while(es.agenda.head.time == incubationTime) es.next
    assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

    //wait for dieTime
    while(es.agenda.head.time < dieTime){
    	assert(infectedPerson.infected == true, "Sick person keeps infected")
    	assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
    	assert(infectedPerson.immune == false, "Sick person is not immune")
    	assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
    	es.next
    }

    assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
    while(es.agenda.head.time == dieTime) es.next
  }


  test("transmissibility rate"){
	  var infectedTimes = 0
	  for(i <- 0 to 100){
		  val es = new EpidemySimulator
		  val healthyPerson = (es.persons find {p => !p.infected}).get
		  es.persons.filter(p => p != healthyPerson) foreach {_.infected = true}

      while(es.agenda.head.time < 6) es.next

      infectedTimes = infectedTimes + (if(healthyPerson.infected) 1 else 0)
	  }
	  assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }

  test("dead should also be sick and infected") {
    val es = new EpidemySimulator
    while (es.currentTime < 500) {
      es.next
      val invalidPersons = es.persons.filter(p => p.dead & !(p.sick & p.infected))
      if (!invalidPersons.isEmpty) invalidPersons.map(p => {
        println(s"Found invalid person: $p\nHistory:\n${p.history}")
      })
      assert(invalidPersons.isEmpty,
        "A person should be sick and infected when they die, and should remain sick and infected afterwards.\n" +
        "Invalid persons:\n" + invalidPersons.map((p) => s"$p\n"))
    }
  }

  test("neighboringRooms") {
    // TODO: it's obviously stupid that we have to construct an EpidemySimulator
    // in order to test the neighboringRooms method; should move it somewhere else.
    val es = new EpidemySimulator
    val c1 = Coord(1,1)
    assert(es.neighboringRooms(c1).toSet ==
      Set(Coord(0,1), Coord(1,0), Coord(2,1), Coord(1,2)),
    s"Invalid neighboring rooms for $c1: ${es.neighboringRooms(c1)}")

    val maxRow = es.SimConfig.roomRows - 1
    val maxCol = es.SimConfig.roomColumns - 1

    val c2 = Coord(0,0)
    assert(es.neighboringRooms(c2).toSet ==
      Set(Coord(0,1), Coord(1,0), Coord(0, maxCol),
        Coord(maxRow,0)),
      s"Invalid neighboring rooms for $c2: ${es.neighboringRooms(c2)}")

    val c3 = Coord(maxRow,maxCol)
    assert(es.neighboringRooms(c3).toSet ==
      Set(Coord(maxRow,maxCol - 1), Coord(maxRow,0),
        Coord(0, maxCol),
        Coord(maxRow - 1,maxCol)),
      s"Invalid neighboring rooms for $c3: ${es.neighboringRooms(c3)}")
  }

  test("each alive person should register a move event at least once every five days") {

    val es = new EpidemySimulator

    def validateMove(m:EpidemySimulator#PersonAction) = {
      m match {
        case a: EpidemySimulator#PersonMoveAction => {
          assert(((a.from.row == a.to.row) &
                    (List(1,es.SimConfig.roomColumns - 1).
                      contains(math.abs(a.from.col - a.to.col)))) |
                  ((a.from.col == a.to.col) &
                    (List(1,es.SimConfig.roomRows - 1).
                      contains(math.abs(a.from.row - a.to.row)))),
            s"When moving, 'to' room must be neighbor of 'from' room; from: ${a.from}, to: ${a.to}")
        }
        case _ => assert(false, s"Invalid PersonMoveAction: $m")
      }
    }

    def numPersonsWithMove = {
      val accMap = Map[Symbol, Int]('move -> 0, 'airmove -> 0)
      es.persons.filter(!_.dead).foldLeft(accMap)((acc : Map[Symbol, Int], p: EpidemySimulator#Person) => {
        val skipMoves = p.history.movesOfType('skipmove)
        val airMoves = p.history.movesOfType('airmove)
        val moves = p.history.movesOfType('move)
        for (m <- moves) validateMove(m)

        val hasMove = skipMoves.length > 0 | airMoves.length > 0 | moves.length > 0
        if (! hasMove) {
          println(s"Found person without move in history: $p\n${p.history}")
        }
        assert(hasMove, s"Found person without move in history! $p")

        val updatedMap =
          if (moves.length > 0) acc.updated('move, acc('move) + 1)
          else acc

        if (airMoves.length > 0) updatedMap.updated('airmove, updatedMap('airmove) + 1)
        else updatedMap
      })
    }

    while(es.agenda.head.time < 6) es.next

    // TODO: this is an arbitrary value and not an ideal way to write a test,
    // but we just want to make sure that most of the people actually moved
    val minimumNumPersonsMoved = (es.persons.length * 0.5).toInt
    var numMoved = numPersonsWithMove
    assert(numMoved('move) > minimumNumPersonsMoved,
      s"Expected at least $minimumNumPersonsMoved people to move; actual number was ${numMoved('move)}.")
    if (es.SimConfig.airTransportEnabled)
      assert(numMoved('airmove) >= 1 & numMoved('airmove) <= 15,
      s"Expected between 1 and 15 people to move by air.")

    es.persons.map(_.clearHistory)

    while(es.agenda.head.time < 12) es.next

    numMoved = numPersonsWithMove

    assert(numMoved('move) > minimumNumPersonsMoved,
      s"Expected at least $minimumNumPersonsMoved people to move; actual number was ${numMoved('move)}.")
    if (es.SimConfig.airTransportEnabled)
      assert(numMoved('airmove) >= 1 & numMoved('airmove) <= 15,
        s"Expected between 1 and 15 people to move by air.")

  }
}