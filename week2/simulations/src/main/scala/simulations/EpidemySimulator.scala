package simulations

import math.random
import scala.collection.mutable.ListBuffer

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalanceRate = 0.01
    val mortalityRate = 0.25
    val transmissibilityRate = 0.40

    val infectedToSickDelay = 6
    val sickToMortalityDelay = 14 - infectedToSickDelay
    val mortalityToImmuneDelay = 16 - (infectedToSickDelay + sickToMortalityDelay)
    val immuneToHealthyDelay = 18 - (infectedToSickDelay + sickToMortalityDelay + mortalityToImmuneDelay)
  }

  import SimConfig._

  val initialNumInfected = prevalanceRate * population

  def createPerson(i:Int) : Person = {
    val p = new Person(i)
    p.scheduleMove
    if (i <= initialNumInfected) p.infect
    p
  }

  val persons: List[Person] = (1 to population map createPerson).toList

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    val history = new PersonHistory()

    override def toString = {
      s"P(id:$id|in:$infected|s:$sick|im:$immune|d:$dead|l:($row,$col))"
    }

    def scheduleMove : Unit = {
      afterDelay(randomBelow(5) + 1) { move }
    }

    def roomContainsInfectedPeople = {
      persons.exists((p) => (p.row == row) & (p.col == col) & (p.infected))
    }

    def move = {
      if (!dead) {
        // TODO: implement actual moving
        if (roomContainsInfectedPeople) {
          if (!immune & !infected & (random <= transmissibilityRate)) {
            infect
          }
        }
        scheduleMove
      }
    }

    def infect = {
      if (!dead) {
        infected = true
        history.record('infect)
        afterDelay(infectedToSickDelay) { sicken }
      }
    }

    def sicken = {
      if (!dead) {
        sick = true
        history.record('sicken)
        afterDelay(sickToMortalityDelay) { maybeDie }
      }
    }

    def maybeDie = {
      if (!dead) {
        if (random <= mortalityRate) {
          dead = true
          history.record('die)
        } else {
          afterDelay(mortalityToImmuneDelay) { immunize }
        }
      }
    }

    def immunize = {
      if (!dead) {
        immune = true
        sick = false
        history.record('immunize)
        afterDelay(immuneToHealthyDelay) { heal }
      }
    }

    def heal = {
      if (!dead) {
        infected = false
        immune = false
        history.record('heal)
      }
    }

  }

  class PersonHistory {
    val history = ListBuffer[PersonAction]()

    def record(action:Symbol) = {
      history += PersonAction(currentTime, action)
    }

    override def toString() = {
      history.mkString("\n")
    }
  }

  case class PersonAction(time:Int, action:Symbol) {
    override def toString = {
      s"Time: $time ; Action: $action"
    }
  }


  // Helper / debug methods
  def numInfected = { persons.count(_.infected) }
  def numSick = { persons.count(_.sick) }
  def numDead = { persons.count(_.dead) }
  def sickRooms = { persons.filter(_.infected).map((p) => List(p.row, p.col)).distinct }
  def numInSickRooms = { persons.count((p) => sickRooms.contains(List(p.row, p.col))) }
  def status = { s"infected: ${numInfected}; sick: ${numSick}; dead: ${numDead}; inSickRooms: ${numInSickRooms}"}

}
