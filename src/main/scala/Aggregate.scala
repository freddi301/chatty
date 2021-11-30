package p2p.chat

import scala.collection.immutable.HashMap

type Action = contact.Action | direct.Action | group.Action

case class Aggregate(contacts: contact.Aggregate, directs: direct.Aggregate, groups: group.Aggregate):
  def +(action: Action): Aggregate = action match
    case action: contact.Action => this.copy(contacts = contacts + action)
    case action: direct.Action  => this.copy(directs = directs + action)
    case action: group.Action   => this.copy(groups = groups + action)

object Aggregate:
  def empty() = Aggregate(
    contacts = contact.Aggregate(HashMap()),
    directs = direct.Aggregate(HashMap()),
    groups = group.Aggregate(HashMap())
  )
