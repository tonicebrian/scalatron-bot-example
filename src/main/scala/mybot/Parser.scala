package mybot

import scala.util.parsing.combinator._

/**
 * Created by cebrian on 18/10/14.
 */
trait Parser extends JavaTokenParsers {

  // Parser
  lazy val parser : Parser[List[ServerMsg]] = repsep(opcode,"|")

  // Server Messages
  lazy val opcode : Parser[ServerMsg] = welcomeOp |
                                reactOp |
                                goodbyeOp

  lazy val welcomeOp : Parser[Welcome] = "Welcome(" ~> repsep(kv,",") <~ ")" ^^ {
    case kvs  => {
      val attrs = kvs.toMap
      Welcome(attrs("name"),attrs("apocalypse").toInt,attrs("round").toInt)
    }
  }

  // React(generation=int,name=string,time=int,view=string,energy=string,master=int:int,coltlision=int:int,slaves=int,...)
  lazy val reactOp : Parser[React] = "React(" ~> kvs <~ ")" ^^ {
    case kvs =>
      val extraMap = List("generation","name","time","view","energy","master","collision","slaves")
                    .foldLeft(kvs){case (m,s) => m-s}
      val extra = extraMap.map(t => t._1+"="+t._2).mkString(",") match {
        case "" => None
        case s:String => Some(s)
      }

      React(kvs("generation").toInt, kvs("name"), kvs("time").toInt, kvs("view"),
            kvs("energy").toInt, toTuple(kvs.get("master")), toTuple(kvs.get("collision")),
            kvs.get("slaves").map(_.toInt), extra)
  }

  // Goodbye(energy=int)
  lazy val goodbyeOp : Parser[Goodbye] = "Goodbye(energy=" ~> wholeNumber <~ ")" ^^ { case energy => Goodbye(energy.toInt) }

  // Helper functions
  private def kv : Parser[(String,String)] = ident ~ "=" ~ regex("""[\?_WMmSsPpBb:\-\/\w\d]+""".r) ^^ { case key ~ "=" ~ value => (key,value) }
  private def kvs : Parser[Map[String,String]] = repsep(kv,",") ^^ {_.toMap}
  private def toTuple(data:Option[String]) = data.map { s =>
    val array = s.split(":")
    (array(0).toInt,array(1).toInt)
  }
}


object ProtocolTranslator extends Parser{
  def fromServer(input:String) = parseAll(parser,input) match {
    case Success(result, _) => result
    case result : NoSuccess => sys.error(result.get)
  }

  def toServer(messages : List[ClientMsg]) = messages.mkString("|")
}
