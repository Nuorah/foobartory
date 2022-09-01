import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.util.UUID
import scala.util.Random

object Robot {
  case object ChangeWorkpost
  case object MineFoo
  case object MineBar
  case object AssembleFoobar
  case class SellFoobar(quantity: Int)
  case object BuildRobot
}

class Robot extends Actor {

  import Robot._
  import Factory._

  def receive = {
    case ChangeWorkpost => {
      println(s"${self.path.name} changing workpost...")
      Thread.sleep(2500)
    }
    case MineFoo => {
      println(s"${self.path.name} mining foo...")
      Thread.sleep(500)
      sender() ! FooMined

    }
    case MineBar => {
      println(s"${self.path.name} mining bar...")
      Thread.sleep(Random.between(250, 1000))
      sender() ! BarMined
    }
    case AssembleFoobar => {
      println(s"${self.path.name} assembling foobar...")
      Thread.sleep(1000)
      if (math.random() < 0.6) {
        sender() ! FoobarAssembledSuccess
      } else {
        sender() ! FoobarAssembledFailure
      }
    }
    case SellFoobar(quantity) => {
      println(s"${self.path.name} selling ${quantity} foobar(s)...")
      Thread.sleep(5000)
      sender() ! FoobarSold(quantity)
    }
    case BuildRobot => {
      println(s"${self.path.name} building robot...")
      sender() ! RobotBuilt
    }

  }
}

object Factory {
  case object AssignTask
  case object AssignFirstTask
  case object FooMined
  case object BarMined
  case object FoobarAssembledSuccess
  case object FoobarAssembledFailure
  case class FoobarSold(quantity: Int)
  case object RobotBuilt

}

class Factory extends Actor {

  import Factory._
  import Robot._

  var foo: Int = 0
  var bar: Int = 0
  var foobar: Int = 0
  var euro: Int = 0
  var robots: Int = 2

  val robot1 = context.actorOf(Props[Robot](), name = "robot-1")
  val robot2 = context.actorOf(Props[Robot](), name = "robot-2")


  def receive = {
    case AssignFirstTask =>
      robot1 ! MineFoo
      robot2 ! MineFoo

    case AssignTask =>
      if (foo >= 5) {
        foo -= 5
        sender() ! BuildRobot
      } else {
        sender() ! MineFoo
      }

    case FooMined =>
      val senderName = sender().path.name
      foo += 1
      println(s"${senderName} mined a foo: ${foo} foos available")
      if (foo > 9) {
        sender() ! ChangeWorkpost
        sender() ! MineBar
      } else {
        sender() ! MineFoo
      }

    case BarMined =>
      val senderName = sender().path.name
      bar += 1
      println(s"${senderName} mined a bar: ${bar} bars available")
      if (bar > 6) {
        sender() ! ChangeWorkpost
        if (bar > 0 && foo > 3) {
          bar -= 1
          foo -= 1
          sender() ! AssembleFoobar
        } else {
          sender() ! MineFoo
        }
      } else {
        sender() ! MineBar
      }

    case FoobarAssembledSuccess =>
      val senderName = sender().path.name
      foobar += 1
      println(s"${senderName} assembled a foobar successfuly: ${foobar} foobars available")
      if (bar > 0 && foo > 3) {
        bar -= 1
        foo -= 1
        sender() ! AssembleFoobar
      } else {
        sender() ! ChangeWorkpost
        if (foobar >= 5) {
          sender() ! SellFoobar(Random.between(1, 6))
        } else {
          sender() ! MineFoo

        }
      }

    case FoobarAssembledFailure =>
      val senderName = sender().path.name
      bar += 1
      println(s"${senderName} failed to assemble a foobar: ${foobar} foobars available")
      if (bar > 0 && foo > 3) {
        bar -= 1
        foo -= 1
        sender() ! AssembleFoobar
      } else {
        sender() ! ChangeWorkpost
        if (foobar >= 5) {
          val quantity = Random.between(1, 6)
          foobar -= quantity
          sender() ! SellFoobar(quantity)
        } else {
          sender() ! MineFoo

        }
      }

    case FoobarSold(quantity) =>
      val senderName = sender().path.name
      euro += quantity
      println(s"${senderName} sold ${quantity} foobars: ${euro} euros available")
      if (foobar >= 5) {
        val quantity = Random.between(1, 6)
        foobar -= quantity
        sender() ! SellFoobar(quantity)
      } else {
        sender()! ChangeWorkpost
        if (euro >= 3 && foo >= 6) {
          euro -= 3
          foo -= 6
          sender() ! BuildRobot
        } else {
          sender() ! MineFoo
        }
      }



    case RobotBuilt =>
      val senderName = sender().path.name
      val name = s"robot-${robots + 1}"
      val childRef = context.actorOf(Props[Robot](), name)
      robots += 1
      println(s"${senderName} built a robot, there are ${robots} robot now")
      if (robots == 30) {
        println("30 ROBOTS BUILT!")
        context.system.terminate()
      }
      childRef ! MineFoo
      if (euro >= 3 && foo >= 6) {
        euro -= 3
        foo -= 6
        sender() ! BuildRobot
      } else {
        sender() ! ChangeWorkpost
        sender() ! MineFoo
      }



    case _ => println("huh?")
  }
}

object Main extends App {

  import Factory._

  val system = ActorSystem("HelloSystem")
  // default Actor constructor
  val foobartory = system.actorOf(Props[Factory], name = "helloactor")
  foobartory ! AssignFirstTask
  while(true) {Thread.sleep(100)}
}