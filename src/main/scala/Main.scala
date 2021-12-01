package p2p.chat

import scala.collection.immutable.HashMap
import common.{AccountId, LocalTimestamp}
import org.getshaka.nativeconverter.NativeConverter
import typescript.TypeScript

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

    println(showStruct[p2p.chat.direct.Action])
    println(typescript.dts[BigAction])
    given NativeConverter[BigAction] = NativeConverter.derived
    println(contact.Action.Delete(AccountId(), AccountId(), LocalTimestamp.now).toJson)

}
