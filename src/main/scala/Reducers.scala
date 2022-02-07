package reducers
import scala.collection.immutable.HashMap

case class P2PChat[AccountId, Timestamp <: Ordered[Timestamp]](zero: Timestamp):
  trait Timestamped:
    val timestamp: Timestamp
  object Contact:
    enum State extends Timestamped:
      case Updated(timestamp: Timestamp, name: String, description: String)
      case Deleted(timestamp: Timestamp)
    enum Action extends Timestamped:
      case Update(timestamp: Timestamp, name: String, description: String)
      case Delete(timestamp: Timestamp)
    val initial: State = State.Deleted(zero)
    def reducer(state: State, action: Action): State =
      if action.timestamp < state.timestamp then state
      else
        action match
          case Action.Update(timestamp, name, description) => State.Updated(timestamp, name, description)
          case Action.Delete(timestamp)                    => State.Deleted(timestamp)
  object Contacts:
    case class UID(owner: AccountId, contact: Timestamp)
    val reducer = Contact.reducer.keyed[UID](Contact.initial)

type Reducer[State, Action] = (State, Action) => State

extension [State, Action](reducer: Reducer[State, Action])
  def keyed[Key](initial: State): Reducer[HashMap[Key, State], (Key, Action)] = (state, a) =>
    a match
      case (key, action) => state.updated(key, reducer(state.getOrElse(key, initial), action))

//  (state: HashMap, action) => state.updated(action.0, reducer(state.getOrElse(action.0, initial), action.1))
