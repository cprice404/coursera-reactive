package simulations

class Simulator {
  type Action = () => Unit

  protected type Agenda = List[WorkItem]

  case class WorkItem(time: Int, action: Action)

  protected[simulations] var agenda: Agenda = List()
  protected[simulations] var currentTime = 0

  protected def afterDelay(delay: Int)(action: => Unit) {
    val item = WorkItem(currentTime + delay, () => action)
    def insert(ag: Agenda): Agenda =
      if (ag.isEmpty || item.time < ag.head.time) item :: ag
      else ag.head :: insert(ag.tail)
    agenda = insert(agenda)
  }

  protected[simulations] def next {
    agenda match {
      case List() => {}
      case WorkItem(time, action) :: rest =>
        agenda = rest
        currentTime = time
        action()
    }
  }

  def runSteps(numSteps:Int) {
    0 to numSteps map((i) => {
      if (!agenda.isEmpty) { next }
    })
    println (s"Current time is now ${currentTime}")
  }

  def run {
    println("*** New propagation ***")
    while (!agenda.isEmpty) { next }
  }
}
