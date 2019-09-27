package playground

import akka.actor._

object ActorIntro extends App {

  val actorSystem = ActorSystem("FirstActorSystem")
  println(actorSystem.name)

  /***  Stateful Counter ***/

  /*object Counter{
    case object Increment
    case object Decrement
    case object Print
  }


  class Counter extends Actor {

    import Counter._

    var count = 0

    def receive:Receive = {

      case Increment => count += 1
      case Decrement => count -= 1
      case Print => println(s"[counter] Current Count is $count")
    }


  }

  val counter = actorSystem.actorOf(Props[Counter],"counter")

  import Counter._
  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print


   */

  /***  Stateless Counter ***/

  object Counter{
    case object Increment
    case object Decrement
    case object Print
  }


  class Counter extends Actor {

    import Counter._

    def receive:Receive = countReceive(0)

    def countReceive(x:Int):Receive = {
      case Increment => context.become(countReceive(x+1))
      case Decrement => context.become(countReceive(x-1))
      case Print => println(s"[counter] Current Count is $x")

    }


  }

  val counter = actorSystem.actorOf(Props[Counter],"counter")

  import Counter._
  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  /***  Stateful BankAccount ***/


  /*object BankAccount{
    case class Deposit(x:Int)
    case class Withdraw(x:Int)
    case object Statement
    case class TransactionSuccess(message:String)
    case class TransactionFailure(message:String)
  }


  class BankAccount extends Actor {

    import BankAccount._

    var balance = 0

    def receive:Receive = {

      case Deposit(amount) =>
        if (amount<0) sender() ! TransactionFailure("Invalid Deposit Amount")
        else {
          balance += amount
          sender() ! TransactionSuccess("Successful Transaction ")
        }
      case Withdraw(amount) =>
        if (amount < 0) sender() ! TransactionFailure("Invalid Withdrawal Amount")
        else if(amount > balance) sender() ! TransactionFailure("Insufficient balance")
        else {
          balance -= amount
          sender() ! TransactionSuccess("Successful Transaction")
        }
      case Statement => sender() ! s"[bank_account] Current Balance is $balance"
    }


  }

  object Person{
    case class StartTransaction(account:ActorRef)
  }

  class Person extends Actor{
    import Person._
    import BankAccount._

    override def receive: Receive = {

      case StartTransaction(account) =>
        account ! Deposit(1000)
        account ! Withdraw(500)
        account ! Statement

      case message => println(message.toString)
    }
  }

  import Person._
  val account1 = actorSystem.actorOf(Props[BankAccount],"account1")
  val person1 = actorSystem.actorOf(Props[Person],"person1")
  person1 ! StartTransaction(account1)

   */



  /***  Stateless BankAccount ***/


  object BankAccount{
    case class Deposit(x:Int)
    case class Withdraw(x:Int)
    case object Statement
    case class TransactionSuccess(message:String)
    case class TransactionFailure(message:String)
  }


  class BankAccount extends Actor {

    import BankAccount._


    def receive:Receive = transact(0)

    def transact(balance:Int):Receive = {
      case Deposit(amount) =>
        if (amount<0) sender() ! TransactionFailure("Invalid Deposit Amount")
        else {
          context.become(transact(balance+amount))
          sender() ! TransactionSuccess("Successful Transaction ")
        }
      case Withdraw(amount) =>
        if (amount < 0) sender() ! TransactionFailure("Invalid Withdrawal Amount")
        else if(amount > balance) sender() ! TransactionFailure("Insufficient balance")
        else {
          context.become(transact(balance-amount))
          sender() ! TransactionSuccess("Successful Transaction")
        }
      case Statement => sender() ! s"[bank_account] Current Balance is $balance"
    }


  }

  object Person{
    case class StartTransaction(account:ActorRef)
  }

  class Person extends Actor{
    import Person._
    import BankAccount._

    override def receive: Receive = {

      case StartTransaction(account) =>
        account ! Deposit(1000)
        account ! Withdraw(500)
        account ! Statement

      case message => println(message.toString)
    }
  }

  import Person._
  val account1 = actorSystem.actorOf(Props[BankAccount],"account1")
  val person1 = actorSystem.actorOf(Props[Person],"person1")
  person1 ! StartTransaction(account1)


}


