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
import kvstore.Persistence.Persist

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
  case object ResendReplicates
  case class PendingAck(a:ActorRef, k:String, v:Option[String], numRetries:Int)

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
  var pendingPersistenceAcks = Map.empty[Long, PendingAck]
  var pendingReplicationAcks = Map.empty[Long, (Set[ActorRef], PendingAck)]
  val MAX_PRIMARY_PERSISTENCE_RETRIES = 10
  val MAX_REPLICATION_RETRIES = 10

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
      persist(k, Option(v), id)
      replicate(k, Option(v), id)
    case Remove(k, id) =>
      kv = kv - k
      persist(k, None, id)
      replicate(k, None, id)
    case Persisted(k, id) =>
      val PendingAck(a, _, _, _) = pendingPersistenceAcks(id)
      if (! pendingReplicationAcks.contains(id)) a ! OperationAck(id)
      pendingPersistenceAcks = pendingPersistenceAcks - id
    case ResendPersists =>
      pendingPersistenceAcks = pendingPersistenceAcks.filter(sendPersistenceFailure)
      pendingPersistenceAcks = pendingPersistenceAcks.map(resendPersist)
    case Replicas(rs) =>
      val secs = rs - context.self
      removeOldReplicas(secs)
      addNewReplicas(secs)
    case ResendReplicates =>
      pendingReplicationAcks = pendingReplicationAcks.filter(sendReplicationFailure)
      pendingReplicationAcks = pendingReplicationAcks.map(resendReplicate)
    case Replicated(k, id) =>
      val (rs, pa @ PendingAck(a, _, _, _)) = pendingReplicationAcks(id)
      val remaining = rs - context.sender
      if (! remaining.isEmpty) pendingReplicationAcks = pendingReplicationAcks.updated(id, (remaining, pa))
      else {
        pendingReplicationAcks = pendingReplicationAcks - id
        if (! pendingPersistenceAcks.contains(id)) a ! OperationAck(id)
      }
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
        persist(k,v,id)
      }
    case Persisted(k, id) =>
      val PendingAck(a, _, _, _) = pendingPersistenceAcks(id)
      a ! SnapshotAck(k, id)
      pendingPersistenceAcks = pendingPersistenceAcks - id
    case ResendPersists =>
      pendingPersistenceAcks.foreach(resendPersist)
  }


  def persist(k:String, v:Option[String], id:Long) = {
    val p = Persist(k, v, id)
    pendingPersistenceAcks = pendingPersistenceAcks.updated(id, PendingAck(context.sender, k, v, 0))
    persistence ! p
    scheduleResendPersists()
  }

  def scheduleResendPersists() = {
    context.system.scheduler.scheduleOnce(100 millis, self, ResendPersists)
  }

  def resendPersist(entry: (Long, PendingAck)) : (Long, PendingAck) = {
    val (id, PendingAck(a, k, v, numRetries)) = entry
    persistence ! Persist(k, v, id)
    scheduleResendPersists()
    (id, PendingAck(a, k, v, numRetries + 1))
  }

  def sendPersistenceFailure(entry: (Long, PendingAck)) : Boolean = {
    val (id, PendingAck(a, _, _, numRetries)) = entry
    if (numRetries < MAX_PRIMARY_PERSISTENCE_RETRIES) true
    else {
      a ! OperationFailed(id)
      false
    }
  }

  def removeOldReplicas(rs: Set[ActorRef]) : Unit = {
    val extras = secondaries.keys.filterNot(replica => rs.contains(replica))
    extras.foreach(replica => {
      val replicator = secondaries(replica)
      context.stop(replica)
      context.stop(replicator)
      secondaries = secondaries - replica
      replicators = replicators - replicator
    })
  }

  def addNewReplicas(rs: Set[ActorRef]) : Unit = {
    val news = rs -- secondaries.keySet
    news.foreach(replica => {
      val replicator = context.actorOf(Replicator.props(replica))
      secondaries = secondaries.updated(replica, replicator)
      replicators = replicators + replicator
    })
  }

  def replicate(k:String, v:Option[String], id:Long) = {
    if (! replicators.isEmpty) {
      val r = Replicate(k, v, id)
      pendingReplicationAcks = pendingReplicationAcks.updated(id,
        (replicators, PendingAck(context.sender, k, v, 0)))
      replicators.foreach(replicator => replicator ! r)
      scheduleResendReplicates()
    }
  }

  def scheduleResendReplicates() = {
    context.system.scheduler.scheduleOnce(100 millis, self, ResendReplicates)
  }

  def resendReplicate(entry: (Long, (Set[ActorRef], PendingAck))) : (Long, (Set[ActorRef], PendingAck)) = {
    val (id, (rs, PendingAck(a, k, v, numRetries))) = entry
    rs.foreach(r => r ! Replicate(k, v, id))
    scheduleResendReplicates()
    (id, (rs, PendingAck(a, k, v, numRetries + 1)))
  }

  def sendReplicationFailure(entry: (Long, (Set[ActorRef], PendingAck))) : Boolean = {
    val (id, (rs, PendingAck(a, _, _, numRetries))) = entry
    if (numRetries < MAX_REPLICATION_RETRIES) true
    else {
      a ! OperationFailed(id)
      false
    }
  }

}
