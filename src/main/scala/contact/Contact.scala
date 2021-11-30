package p2p.chat.contact
import p2p.chat.common.{AccountId, LocalTimestamp}

import scala.collection.immutable.HashMap

trait UID:
  val owner: AccountId
  val contact: AccountId
  def uid = Key(owner, contact)

trait Payload:
  val name: String
  val description: String

trait Timestamped:
  val timestamp: LocalTimestamp

enum Action extends UID, Timestamped:
  case Update(owner: AccountId, contact: AccountId, timestamp: LocalTimestamp, name: String, description: String)
  case Delete(owner: AccountId, contact: AccountId, timestamp: LocalTimestamp)

enum State extends Timestamped:
  case Updated(timestamp: LocalTimestamp, name: String, description: String)
  case Deleted(timestamp: LocalTimestamp)

case class ListItem(owner: AccountId, contact: AccountId, name: String, description: String) extends UID, Payload

case class Key(owner: AccountId, contact: AccountId) extends UID

case class Aggregate(contactsMap: HashMap[Key, State]):
  def +(action: Action): Aggregate = action match
    case Action.Delete(owner, contact, timestamp) =>
      contactsMap.get(action.uid) match
        case Some(state) =>
          if (timestamp > state.timestamp) Aggregate(contactsMap.updated(action.uid, State.Deleted(timestamp)))
          else Aggregate(contactsMap)
        case None => Aggregate(contactsMap.updated(action.uid, State.Deleted(timestamp)))
    case Action.Update(owner, contact, timestamp, name, description) =>
      contactsMap.get(action.uid) match
        case Some(state) =>
          if (timestamp > state.timestamp) Aggregate(contactsMap.updated(action.uid, State.Updated(timestamp, name, description)))
          else Aggregate(contactsMap)
        case None => Aggregate(contactsMap.updated(action.uid, State.Updated(timestamp, name, description)))
  def list(owner: AccountId): List[ListItem] =
    contactsMap.toSeq
      .filter { case (uid, state) => uid.owner == owner }
      .flatMap { case (uid, state) =>
        state match {
          case State.Deleted(timestamp)                    => Seq()
          case State.Updated(timestamp, name, description) => Seq(ListItem(uid.owner, uid.contact, name, description))
        }
      }
      .sortBy(_.name)
      .toList
