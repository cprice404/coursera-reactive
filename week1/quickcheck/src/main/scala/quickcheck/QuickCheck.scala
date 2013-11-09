package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import math._
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
    (b > a) ==> {
      val h = insert(a, insert(b, empty))
      findMin(h) == a
    }
  }

  property("insDeleteEmpty") = forAll { a: A =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("sorted") = forAll { h: H =>
    def popAll(h: H) : List[A] = {
      @tailrec
      def popAcc(as : List[A], h: H) : List[A] = {
        if (isEmpty(h)) as
        else {
          val a = findMin(h)
          popAcc(as :+ a, deleteMin(h))
        }
      }
      popAcc(List[A](), h)
    }

    val as:List[A] = popAll(h)
    as == as.sorted
  }

  lazy val genHeap: Gen[H] = for {
    isEmpty <- arbitrary[Boolean]
    h <- if (isEmpty) value(empty) else nonEmptyHeaps
  } yield h

  lazy val nonEmptyHeaps: Gen[H] = for {
    a <- arbitrary[A]
    h <- genHeap
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
