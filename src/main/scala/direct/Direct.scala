package p2p.chat.direct
import p2p.chat.common.{AccountId, LocalTimestamp}

import scala.collection.immutable.HashMap

trait UID:
  val sender: AccountId
  val recipient: AccountId
  val creation: LocalTimestamp
  def uid = Key(sender, recipient, creation)

trait Payload:
  val text: String

trait Timestamped:
  val timestamp: LocalTimestamp

enum Action extends UID, Timestamped:
  case Update(sender: AccountId, recipient: AccountId, creation: LocalTimestamp, timestamp: LocalTimestamp, text: String)
  case Delete(sender: AccountId, recipient: AccountId, creation: LocalTimestamp, timestamp: LocalTimestamp)

enum State extends Timestamped:
  case Updated(timestamp: LocalTimestamp, text: String)
  case Deleted(timestamp: LocalTimestamp)

case class ListItem(sender: AccountId, recipient: AccountId, creation: LocalTimestamp, text: String) extends UID, Payload

case class Key(sender: AccountId, recipient: AccountId, creation: LocalTimestamp) extends UID

case class Aggregate(messagesMap: HashMap[Key, State]):
  def +(action: Action): Aggregate = action match
    case Action.Delete(sender, recipient, creation, timestamp) =>
      messagesMap.get(action.uid) match
        case Some(state) =>
          if (timestamp > state.timestamp) Aggregate(messagesMap.updated(action.uid, State.Deleted(timestamp)))
          else Aggregate(messagesMap)
        case None => Aggregate(messagesMap.updated(action.uid, State.Deleted(timestamp)))
    case Action.Update(sender, recipient, creation, timestamp, text) =>
      messagesMap.get(action.uid) match
        case Some(state) =>
          if (timestamp > state.timestamp) Aggregate(messagesMap.updated(action.uid, State.Updated(timestamp, text)))
          else Aggregate(messagesMap)
        case None => Aggregate(messagesMap.updated(action.uid, State.Updated(timestamp, text)))
  def list(me: AccountId, other: AccountId): List[ListItem] =
    messagesMap.toSeq
      .filter { case (uid, state) => (uid.sender == me && uid.recipient == other) || (uid.sender == other && uid.sender == me) }
      .flatMap { case (uid, state) =>
        state match {
          case State.Deleted(timestamp)       => Seq()
          case State.Updated(timestamp, text) => Seq(ListItem(uid.sender, uid.recipient, uid.creation, text))
        }
      }
      .sortBy(_.creation)
      .toList
