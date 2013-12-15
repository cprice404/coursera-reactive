/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case o:Operation =>
      root ! o
    case GC =>
      ???
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = ???

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def subtreePosition(x:Int) : Position = if (x < elem) Left else Right

  def subtreeContains(c:Contains) = {
    subtrees.get(subtreePosition(c.elem)) match {
      case Some(node) => node ! c
      case None => c.requester ! ContainsResult(c.id, result = false)
    }
  }

  def subtreeInsert(i:Insert) = {
    val p = subtreePosition(i.elem)
    subtrees.get(p) match {
      case Some(node) => node ! i
      case None => {
        subtrees = subtrees +
          (p -> context.actorOf(BinaryTreeNode.props(i.elem, initiallyRemoved = false)))
        i.requester ! OperationFinished(i.id)
      }
    }
  }

  def subtreeRemove(r:Remove) = {
    subtrees.get(subtreePosition(r.elem)) match {
      case Some(node) => node ! r
      case None => r.requester ! OperationFinished(r.id)
    }
  }

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case c @ Contains(requester, id, x) =>
      if (x == elem) requester ! ContainsResult(id, ! removed)
      else subtreeContains(c)
    case i @ Insert(requester, id, x) =>
      if (x != elem) subtreeInsert(i)
      else {
        removed = false
        requester ! OperationFinished(id)
      }
    case r @ Remove(requester, id, x) =>
      if (x != elem) subtreeRemove(r)
      else {
        removed = true
        requester ! OperationFinished(id)
      }
    case CopyTo(node) =>
      ???
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???

}
