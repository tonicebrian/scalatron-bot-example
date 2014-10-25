/**
 * Created by cebrian on 18/10/14.
 */
package object mybot {
  abstract sealed class ServerMsg
  case class Welcome(val name:String,val apocalypse:Int,val round:Int,val maxslaves:Option[Int] = None) extends ServerMsg
  case class React(val generation:Int, name:String, time:Int, view:String, energy:Int,
                   master:Option[(Int,Int)] = None,collision:Option[(Int,Int)] = None,
                   slaves:Option[Int] = None, extra:Option[String] = None) extends ServerMsg
  case class Goodbye(val energy:Int) extends ServerMsg


  abstract sealed class ClientMsg
  case class Move(val direction:(Int,Int)) extends ClientMsg {
    override def toString = "Move(direction="+direction._1+":"+direction._2+")"
  }
  case class Spawn(val direction:(Int,Int),val name:String,val energy:Int, val extra:String) extends ClientMsg {
    override def toString = "Spawn(direction="+direction._1+":"+direction._2+",name="+name+",energy="+energy+","+extra+")"
  }
  case class Set(val elems:Map[String,String]) extends ClientMsg {
    override def toString = {
      elems.map { case (key,value) => key+"="+value }
    } mkString("Set(",",",")")
  }
  case class Explode(val size:Int) extends ClientMsg {
    override def toString = "Explode(size="+size+")"
  }
  case class Say(val text:String) extends ClientMsg {
    override def toString = "Say(text="+text+")"
  }
  case class Status(val text:String) extends ClientMsg {
    override def toString = "Status(text="+text+")"
  }
  case class Log(val text:String) extends ClientMsg {
    override def toString = "Log(text="+text+")"
  }
  case class DrawLine(val from:(Int,Int),val to:(Int,Int),val color:String) extends ClientMsg {
    override def toString = "DrawLine(from="+from._1+":"+from._2+",to="+to._1+":"+to._2+",color="+color+")"
  }
  case class MarkCell(val position:(Int,Int),val color:String) extends ClientMsg {
    override def toString = "MarckCell(position="+position._1+":"+position._2+",color="+color+")"
  }
}