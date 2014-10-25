import mybot._

import scala.util.Random

class ControlFunctionFactory {
  def create = new Bot().respond _
}

class Bot {
  val rnd = new Random
  def respond(input: String) = {
    try {
      ProtocolTranslator.fromServer(input) map {
        _ match {
          case Welcome(_, _, _, _) => Move((1,1))
          case React(_, _, _, _, _, _, _, _, _) => Move((rnd.nextInt(3)-1,rnd.nextInt(3)-1))
          case Goodbye(_) => Say("Received Goodbye")
        }
      } mkString("|")
    } catch {
      case e:Exception =>
        println("Error on input: " + input)
        e.printStackTrace()
    }
  }
}
