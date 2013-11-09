package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import math._
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def popAll(h: H) : List[A] = {
    @tailrec
    def popAcc(as : List[A], h: H) : List[A] = {
      if (isEmpty(h)) as else popAcc(as :+ findMin(h), deleteMin(h))
    }
    popAcc(List[A](), h)
  }

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
    val as:List[A] = popAll(h)
    as == as.sorted
  }

  property("minMeld") = forAll { (h1: H, h2: H) =>
    (! (isEmpty(h1) || isEmpty(h2))) ==> {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val meldMin = findMin(meld(h1,h2))
      meldMin == min(min1, min2)
    }
  }

  property("sortedAndAssocMeld") = forAll { (h1: H, h2: H) =>
    val as1:List[A] = popAll(meld(h1, h2))
    val as2:List[A] = popAll(meld(h2, h1))
    (as1 == as1.sorted) && (as1 == as2)
  }

  property("insDeleteMin") = forAll(nonEmptyHeaps, arbitrary[A]) { (h: H, a: A) =>
    val as1 = popAll(h)
    val as2 = popAll(deleteMin(insert(findMin(h), h)))
    as1 == as2
  }

  lazy val genHeap: Gen[H] = for {
    // give ourselves a 10% chance of creating an empty heap
    h <- frequency((1, value(empty)), (10, nonEmptyHeaps))
  } yield h

  lazy val nonEmptyHeaps: Gen[H] = for {
    a <- arbitrary[A]
    h <- genHeap
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}

