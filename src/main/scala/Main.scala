package p2p.chat
import scala.collection.immutable.HashMap
import common.{AccountId, LocalTimestamp}

object Main {
  def main(args: Array[String]): Unit =
    var state = p2p.chat.Aggregate.empty()
    println(state.contacts.list(AccountId()))
    state = state + contact.Action.Update(
      owner = AccountId(),
      contact = AccountId(),
      timestamp = LocalTimestamp.now,
      name = "Fred",
      description = "boring"
    )
    println(state.contacts.list(AccountId()))
    state = state + contact.Action.Delete(owner = AccountId(), contact = AccountId(), timestamp = LocalTimestamp.now)
    println(state.contacts.list(AccountId()))
}
