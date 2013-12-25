package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case object ResendSnapshots

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  def scheduleResend() = {
    context.system.scheduler.scheduleOnce(100 millis, self, ResendSnapshots)
  }

  def resendSnapshot(s: (ActorRef, Replicate)) = {
    val (a, Replicate(k, v, id)) = s
    replica ! Snapshot(k, v, id)
  }
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case r @ Replicate(k, v, id) =>
      acks = acks.updated(id, (context.sender, r))
      scheduleResend()
      replica ! Snapshot(k, v, nextSeq)
    case ResendSnapshots =>
      if (! acks.isEmpty) {
        acks.values.foreach(resendSnapshot)
        scheduleResend()
      }
    case SnapshotAck(k, snapshotId) =>
      val (a, Replicate(_, _, origId)) = acks(snapshotId)
      a ! Replicated(k, origId)
      acks = acks - snapshotId
  }

}
