package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("timedOut") {
    val ticks = Observable.interval(100 millis)

    val done = Promise[Unit]()
    val observed = mutable.Buffer[Long]()
    ticks.timedOut(1).subscribe(
      next => observed += next,
      error => Unit,
      () => done.success()
    )

    Await.ready(done.future, 2 seconds)

    assert(observed == Seq(0,1,2,3,4,5,6,7,8,9), observed)
  }

  class Observed[T]() {
    val observed:mutable.Buffer[T] = mutable.Buffer[T]()
    var isComplete:Boolean = false
  }

  def testSubscribeRecovered[T](o:Observable[Try[T]]) : Observed[Try[T]] = {
    val observed = new Observed[Try[T]]()
    o subscribe(
      next => observed.observed += next,
      error => { throw new IllegalStateException("recovered stream should not have errors!") },
      () => observed.isComplete = true
    )
    observed
  }

  test("recovered") {
    val err = new IllegalStateException("doh")
    val obsWithErrors = Observable(1, 2, 3) ++ Observable(err)
    val observed = testSubscribeRecovered(obsWithErrors.recovered)
    assert(observed.observed == Seq(Success(1), Success(2),
      Success(3), Failure(err)),
      observed.observed)
    assert(observed.isComplete, "Observable did not complete!")
  }

  test("concatRecovered with error") {
    val err = new Exception
    val cat:Observable[Try[Int]] = Observable(1,2,3,4,5).concatRecovered((num:Int) =>
      if (num != 4) Observable(num) else Observable(err))
    val observed = testSubscribeRecovered(cat)
    assert(observed.observed == Seq(Success(1), Success(2), Success(3),
      Failure(err), Success(5)), observed.observed)
    assert(observed.isComplete, "Observable did not complete")
  }

  test("concatRecovered without error") {
    val cat:Observable[Try[Int]] = Observable(1,2,3).concatRecovered((num:Int) => Observable[Int](num, num, num))
    val observed = testSubscribeRecovered(cat)
    assert(observed.observed ==
      Seq(Success(1), Success(1), Success(1),
        Success(2), Success(2), Success(2),
        Success(3), Success(3), Success(3)),
      observed.observed)
    assert(observed.isComplete, "Observable did not complete")
  }

}