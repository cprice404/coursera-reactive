package suggestions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scala.concurrent.{Await, Future}
import rx.lang.scala.Observable
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
//import rx.lang.scala.subscriptions.Subscription
import suggestions.observablex.ObservableEx

import scala.language.postfixOps
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class ObservableExTest extends FunSuite {
  def testSubscribe(o:Observable[String], buff:mutable.Buffer[String]) {
    o.subscribe(
      next => buff += s"next: '$next'",
      error => buff += s"error: '${error.getMessage()}'",
      () => buff += "complete"
    )
  }

  test("Should be able to construct an observable from a future") {
    val f:Future[String] = Future { "foo" }
    val o:Observable[String] = ObservableEx(f)

    val observed = mutable.Buffer[String]()
    testSubscribe(o, observed)
    assert(observed == Seq("next: 'foo'", "complete"), observed)
  }

  test("observable from future should support errors correctly") {
    val f:Future[String] = Future { throw new IllegalStateException("doh!") }
    val o:Observable[String] = ObservableEx(f)

    val observed = mutable.Buffer[String]()
    testSubscribe(o, observed)
    Await.ready(f, 1 second)
    assert(observed == Seq("error: 'doh!'"), observed + " / " + Seq("error: 'doh!'"))
  }
}
