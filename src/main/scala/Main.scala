
object Main {
  case class Machine(finiteStateMachine: FiniteStateMachine) {
    private[this] var current: Option[NodeState] = finiteStateMachine.states.find(item => item._1 == finiteStateMachine.initialStateName).map(_._2)
    private[this] var name: String = finiteStateMachine.initialStateName
    def transition(event: String): Unit = {
      if (current.isDefined) {
        val transition = current.flatMap(nodeState => nodeState.transitions.find(transition => transition._1 == event))
        val nextStateName = transition.map(_._2.target)
        current = finiteStateMachine.states.find(state => state._1 == nextStateName.getOrElse(name)).map(_._2)
        nextStateName.foreach(nextName => name = nextName)
      }
    }
    def getStateName: String = {
      current match {
        case None => "None"
        case value => s"$name ${value.toString}"
      }
    }
  }

  def validate(finiteStateMachine: FiniteStateMachine): Option[String] = {
    val initState = finiteStateMachine.initialStateName
    val states = finiteStateMachine.states

    states.find(item => item._1 == initState) match {
      case Some(_) => None
      case None => Some(s"initial state $initState is not given in the states")
    }

  }

  def interpreter(finiteStateMachine: FiniteStateMachine): Either[Machine, String] = {
    validate(finiteStateMachine) match {
      case Some(errorMessage) => Right(errorMessage)
      case None => Left(Machine(finiteStateMachine))
    }
  }

  case class Transition(target: String)
  case class NodeState(transitions: Map[String, Transition])
  case class FiniteStateMachine(name: String, initialStateName: String, states: Map[String, NodeState])
  def main(args: Array[String]): Unit = {
    val toggleState = FiniteStateMachine("toggle-state", "off", Map[String, NodeState](
      "on" -> NodeState(Map("TOGGLE" -> Transition("off"))),
      "off" -> NodeState(Map("TOGGLE" -> Transition("on")))
    ))
    val res = interpreter(toggleState)
    res match {
      case Left(machine) =>
        println(machine.getStateName)
        machine.transition("TOGGLE")
        println(machine.getStateName)
        machine.transition("NINJA")
        println(machine.getStateName)
      case Right(error) =>
        println(error)
    }
  }
}
