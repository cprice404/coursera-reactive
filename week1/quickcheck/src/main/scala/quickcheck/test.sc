import quickcheck.{IntHeap, BinomialHeap}

/**
 * Created with IntelliJ IDEA.
 * User: cprice
 * Date: 11/9/13
 * Time: 10:33 AM
 * To change this template use File | Settings | File Templates.
 */

trait PrintableHeap extends BinomialHeap with IntHeap {
  def printNode(t: Node) : Unit = {
    print("[")
    print("(" + t.x + "," + t.r + ")")
    if (! t.c.isEmpty) {
      print("->")
      printNodes(t.c)
    }
    print("]")
  }

  def printNodes(ts: H) : Unit = ts match {
    case Nil => Unit
    case t::ts => {
      printNode(t)
      printNodes(ts)
    }
  }

  def printHeap(ts: H) : Unit = ts match {
      case Nil => Unit //println("empty")
      case t::ts => {
        printNode(t)
        println
        printHeap(ts)
      }
  }
}

new PrintableHeap {
//  printHeap(empty)
  printHeap(insert(6, insert(4, empty)))
  println("-----------------")
  printHeap(insert(10, insert(6, insert(4, empty))))
  println("-----------------")
  printHeap(insert(3, insert(10, insert(6, insert(4, empty)))))
  println("-----------------")
  def add(xs:List[A], h: H) : H = xs match {
    case Nil => h
    case x::xs => add(xs, insert(x, h))
  }
  printHeap(add(List(4, 6, 10, 3), empty))
  println("-----------------")
  printHeap(add(List(4, 6, 10, 3, 2, 9, 5), empty))
  println("-----------------")
  printHeap(add(List(4, 6, 10, 3, 2, 9, 5, 1), empty))
}











































