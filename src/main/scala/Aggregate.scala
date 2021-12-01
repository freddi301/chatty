package p2p.chat
import p2p.chat.common.{AccountId, LocalTimestamp}

import scala.collection.immutable.HashMap

sealed trait BigAction

package contact {
  enum Action extends BigAction, UID, Timestamped:
    case Update(owner: AccountId, contact: AccountId, timestamp: LocalTimestamp, name: String, description: String)
    case Delete(owner: AccountId, contact: AccountId, timestamp: LocalTimestamp)
}

package direct {
  enum Action extends BigAction, UID, Timestamped:
    case Update(sender: AccountId, recipient: AccountId, creation: LocalTimestamp, timestamp: LocalTimestamp, text: String)
    case Delete(sender: AccountId, recipient: AccountId, creation: LocalTimestamp, timestamp: LocalTimestamp)
}

package group {
  enum Action extends BigAction, UID, Timestamped:
    case Update(sender: AccountId, group: GroupId, creation: LocalTimestamp, timestamp: LocalTimestamp, text: String)
    case Delete(sender: AccountId, group: GroupId, creation: LocalTimestamp, timestamp: LocalTimestamp)
}

case class Aggregate(contacts: contact.Aggregate, directs: direct.Aggregate, groups: group.Aggregate):
  def +(action: BigAction): Aggregate = action match
    case action: contact.Action => this.copy(contacts = contacts + action)
    case action: direct.Action  => this.copy(directs = directs + action)
    case action: group.Action   => this.copy(groups = groups + action)

object Aggregate:
  def empty() = Aggregate(
    contacts = contact.Aggregate(HashMap()),
    directs = direct.Aggregate(HashMap()),
    groups = group.Aggregate(HashMap())
  )
