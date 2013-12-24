package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case object ResendPersists

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]


  val persistence = context.actorOf(persistenceProps)
  var pendingPersistenceAcks = Map.empty[Long, (Persist, ActorRef)]

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }


  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Get(k, id) =>
      context.sender ! GetResult(k, kv.get(k), id)
    case Insert(k, v, id) =>
      kv = kv.updated(k, v)
      context.sender ! OperationAck(id)
    case Remove(k, id) =>
      kv = kv - k
      context.sender ! OperationAck(id)
  }


  var nextSeqId: Long = 0

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(k, id) =>
      context.sender ! GetResult(k, kv.get(k), id)
    case Snapshot(k, v, id) =>
      if (id < nextSeqId) context.sender ! SnapshotAck(k, id)
      else if (id == nextSeqId) {
        v match {
          case Some(value) =>
            kv = kv.updated(k, value)
          case None =>
            kv = kv - k
        }
        nextSeqId += 1
        val p = Persist(k, v, id)
        pendingPersistenceAcks = pendingPersistenceAcks.updated(id, (p, context.sender))
        persistence ! p
        scheduleResend()
      }
    case Persisted(k, id) =>
      val (_, a) = pendingPersistenceAcks(id)
      a ! SnapshotAck(k, id)
      pendingPersistenceAcks = pendingPersistenceAcks - id
    case ResendPersists =>
      pendingPersistenceAcks.values.foreach(resendPersist)
  }

  def scheduleResend() = {
    context.system.scheduler.scheduleOnce(100 millis, self, ResendPersists)
  }

  def resendPersist(pa: (Persist, ActorRef)) : Unit = {
    val (p, _) = pa
    persistence ! p
    scheduleResend()
  }



}