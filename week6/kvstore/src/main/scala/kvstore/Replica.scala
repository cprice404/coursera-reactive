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
  case class MessageId(a:ActorRef, id:Long)
  case class PendingAck(mid:MessageId, k:String, v:Option[String], numRetries:Int)
  type ReplAckEntry = (ActorRef, Map[Long, PendingAck])
  type ReplAckMap = Map[ActorRef, Map[Long, PendingAck]]
  def ReplAckMap() = Map.empty[ActorRef, Map[Long, PendingAck]]

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
  val MAX_PRIMARY_PERSISTENCE_RETRIES = 10
  val MAX_REPLICATION_RETRIES = 10

  var nextPersistId: Long = 0
  // map from persistenceId to Pending Ack
  var pendingPersistenceAcks = Map.empty[Long, PendingAck]
  // map from orig message id to persist id
  var ackIdToPersistId = Map.empty[MessageId, Long]
  // map from replicator instance to replicate id to pending ack
  var pendingReplicationAcks = Map.empty[ActorRef, Map[Long, PendingAck]]
  // map from orig message id to replicator to replicate id
  var ackIdToReplicatorId = Map.empty[MessageId, Map[ActorRef, Long]]
  // map from replicator to next replicate id
  var replicatorNextId = Map.empty[ActorRef, Long]

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
    case Persisted(k, persistId) =>
      val PendingAck(mid @ MessageId(a, origId), _, _, _) = pendingPersistenceAcks(persistId)
      if (! ackIdToReplicatorId.contains(mid)) a ! OperationAck(origId)
      pendingPersistenceAcks = pendingPersistenceAcks - persistId
      ackIdToPersistId = ackIdToPersistId - mid
      nextPersistId += 1
    case ResendPersists =>
      pendingPersistenceAcks = pendingPersistenceAcks.filter(sendPersistenceFailure)
      pendingPersistenceAcks = pendingPersistenceAcks.map(resendPersist)
    case Replicas(rs) =>
      val secs = rs - context.self
      removeOldReplicas(secs)
      addNewReplicas(secs)
    case ResendReplicates =>
      val tooManyRetries: Set[MessageId] = messageIdSet(
        filterByAck(pendingReplicationAcks)((pa:PendingAck) => {
          val PendingAck(_, _, _, numRetries) = pa
          numRetries >= MAX_REPLICATION_RETRIES
        }))
//      {
//        for {
//          acksMapEntry <- filterByAck(pendingReplicationAcks)((pa:PendingAck) => {
//            val PendingAck(_, _, _, numRetries) = pa
//            numRetries < MAX_REPLICATION_RETRIES
//          })
//          ack <- acksMapEntry._2
//        } yield ack.messageId
//      }.toSet
      tooManyRetries.foreach(mid => {
        val MessageId(a, origId) = mid
        a ! OperationFailed(origId)
      })
      pendingReplicationAcks = filterByAck(pendingReplicationAcks)((pa:PendingAck) => {
        val PendingAck(mid, _, _, _) = pa
        ! tooManyRetries.contains(mid)
      })
//        pendingReplicationAcks.
//        foldLeft(ReplAckMap())(filterPendingReplByMessageId(tooManyRetries))
//      pendingReplicationAcks = pendingReplicationAcks.flatMap(sendFailuresForReplicator)
      pendingReplicationAcks.foreach(resendReplicatesForReplicator)
    case Replicated(k, repId) =>
      val r:ActorRef = context.sender
//      val ackId = replicatorIdToAckId(r, repId)
//      val (rs, pa @ PendingAck(a, _, _, _)) = pendingReplicationAcks(ackId)
//      val remaining = rs - context.sender
//      replicatorIdToAckId = replicatorIdToAckId - ((r, repId))
//      ackIdToReplicatorId = ackIdToReplicatorId - ((r, ackId))
//      if (! remaining.isEmpty) pendingReplicationAcks = pendingReplicationAcks.updated(ackId, (remaining, pa))
//      else {
//        pendingReplicationAcks = pendingReplicationAcks - ackId
//        if (! pendingPersistenceAcks.contains(ackId)) a ! OperationAck(ackId)
//      }
      val pas = pendingReplicationAcks(r)
      val remainingPas = pas - repId
      if (remainingPas.isEmpty) pendingReplicationAcks = pendingReplicationAcks - r
      else pendingReplicationAcks = pendingReplicationAcks.updated(r, remainingPas)

      val PendingAck(mid @ MessageId(a, origId), _, _, _) = pas(repId)
      val replsForMsg = ackIdToReplicatorId(mid) - r
      if (! replsForMsg.isEmpty) ackIdToReplicatorId = ackIdToReplicatorId.updated(mid, replsForMsg)
      else {
        ackIdToReplicatorId = ackIdToReplicatorId - mid
        if (! pendingPersistenceAcks.contains(ackIdToPersistId(mid))) a ! OperationAck(origId)
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
    case Persisted(k, persistId) =>
      val PendingAck(mid @ MessageId(a, origId), _, _, _) = pendingPersistenceAcks(persistId)
      a ! SnapshotAck(k, origId)
      pendingPersistenceAcks = pendingPersistenceAcks - persistId
      ackIdToPersistId = ackIdToPersistId - mid
    case ResendPersists =>
      pendingPersistenceAcks.foreach(resendPersist)
  }


  def persist(k:String, v:Option[String], origId:Long) = {
    val p = Persist(k, v, nextPersistId)
    val mid = MessageId(context.sender, origId)
    ackIdToPersistId = ackIdToPersistId.updated(mid, nextPersistId)
    pendingPersistenceAcks = pendingPersistenceAcks.updated(nextPersistId,
        PendingAck(mid, k, v, 0))
    nextPersistId += 1
    persistence ! p
    scheduleResendPersists()
  }

  def scheduleResendPersists() = {
    context.system.scheduler.scheduleOnce(100 millis, self, ResendPersists)
  }

  def resendPersist(entry: (Long, PendingAck)) : (Long, PendingAck) = {
    val (persistId, PendingAck(mid, k, v, numRetries)) = entry
    persistence ! Persist(k, v, persistId)
    scheduleResendPersists()
    (persistId, PendingAck(mid, k, v, numRetries + 1))
  }

  def sendPersistenceFailure(entry: (Long, PendingAck)) : Boolean = {
    val (_, PendingAck(MessageId(a, origId), _, _, numRetries)) = entry
    if (numRetries < MAX_PRIMARY_PERSISTENCE_RETRIES) true
    else {
      a ! OperationFailed(origId)
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

  def replicate(k:String, v:Option[String], origId:Long) = {
    replicators.foreach(replicator => {
      val mid:MessageId = MessageId(context.sender, origId)

      val repId:Long = replicatorNextId.getOrElse(replicator, 0)
      replicatorNextId = replicatorNextId.updated(replicator, repId + 1)

//      replicatorIdToAckId = replicatorIdToAckId.updated((replicator, repId), id)
//      ackIdToReplicatorId = ackIdToReplicatorId.updated((replicator, id), repId)

      val rids = ackIdToReplicatorId.getOrElse(mid, Map.empty[ActorRef, Long])
      ackIdToReplicatorId = ackIdToReplicatorId.updated(mid, rids.updated(replicator, repId))

      val pas = pendingReplicationAcks.getOrElse(replicator, Map.empty[Long, PendingAck])
      pendingReplicationAcks = pendingReplicationAcks.updated(replicator,
        pas.updated(repId, PendingAck(mid, k, v, 0)))

      val r = Replicate(k, v, repId)
      replicator ! r
    })
    if (! replicators.isEmpty) {
//      pendingReplicationAcks = pendingReplicationAcks.updated(id,
//        (replicators, PendingAck(context.sender, k, v, 0)))
      scheduleResendReplicates()
    }
  }

  def scheduleResendReplicates() = {
    context.system.scheduler.scheduleOnce(100 millis, self, ResendReplicates)
  }
//
//  def resendReplicate(entry: (Long, (Set[ActorRef], PendingAck))) : (Long, (Set[ActorRef], PendingAck)) = {
//    val (id, (rs, PendingAck(a, k, v, numRetries))) = entry
//    rs.foreach(r => r ! Replicate(k, v, ackIdToReplicatorId((r, id))))
//    scheduleResendReplicates()
//    (id, (rs, PendingAck(a, k, v, numRetries + 1)))
//  }

//  def sendFailuresForReplicator(entry: ReplAckEntry) : List[ReplAckEntry] = {
//    val (r, replicateIds) = entry
//    val remainingAcks = replicateIds.filter(sendReplicationFailure)
//    if (remainingAcks.isEmpty) List()
//    else List((r, remainingAcks))
//  }
//
//  def sendReplicationFailure(entry: (Long, PendingAck)) : Boolean = {
//    val (replicateId, PendingAck(MessageId(a, origId), _, _, numRetries)) = entry
//    if (numRetries < MAX_REPLICATION_RETRIES) true
//    else {
//      a ! OperationFailed(origId)
//      false
//    }
//  }

  def messageIdSet(ackMap: ReplAckMap) : Set[MessageId] = {
    ackMap.values.flatMap((pendingAcksMap) => {
      pendingAcksMap.values.map(pa => {
        val PendingAck(mid, _, _, _) = pa
        mid
      })
    }).toSet
  }

  def filterByAck(ackMap:ReplAckMap)(f:(PendingAck) => Boolean) : ReplAckMap = {
    ackMap.foldLeft(ReplAckMap())((acc:ReplAckMap, entry:ReplAckEntry) => {
      val (replicator, acks) = entry
      val filteredAcks = acks.filter((ackEntry:(Long, PendingAck)) => {
        val (_, pendingAck) = ackEntry
        f(pendingAck)
      })
      if (filteredAcks.isEmpty) acc
      else acc.updated(replicator, filteredAcks)
    })
  }

  def resendReplicatesForReplicator(entry: ReplAckEntry) : Unit = {
    val (r, pendingAcksMap) = entry
    pendingAcksMap.foreach((e: (Long, PendingAck)) => {
      val (replicateId, PendingAck(_, k, v, _)) = e
      r ! Replicate(k, v, replicateId)
    })
  }

//  def filterPendingReplByMessageId(mids:Set[MessageId])(acc:ReplAckMap, entry:ReplAckEntry) : ReplAckMap = {
//    val (replicator, pendingAcksMap) = entry
//    val filteredAcks = pendingAcksMap.filterNot(e => {
//      val (replicateId, PendingAck(mid, _, _, _, _)) = e
//      mids.contains(mid)
//    })
//    if (filteredAcks.isEmpty) acc
//    else acc.updated(replicator, filteredAcks)
//  }

}
