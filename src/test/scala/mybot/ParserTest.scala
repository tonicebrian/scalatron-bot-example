package mybot

import org.scalatest.matchers.MustMatchers
import org.scalatest.FlatSpec

/**
 * Created by cebrian on 18/10/14.
 */
class ParserTest extends FlatSpec
with MustMatchers
{

  "Our parser" must "recognize the React order" in {
     val msg = "React(generation=0,time=100,view=__W_W_W__,energy=100,name=MyBot)"
     val result = ProtocolTranslator.fromServer(msg)
     result.head must be (React(0,"MyBot",100,"__W_W_W__",100))
  }
  it must "recognize extra parameters previously set" in {
    val msg = "React(generation=0,time=100,view=__W_W_W__,energy=100,name=MyBot,role=missile)"
    val result = ProtocolTranslator.fromServer(msg)
    result.head must be (React(0,"MyBot",100,"__W_W_W__",100,extra=Some("role=missile")))
  }
  it must "understand React commands from taken from server" in {
    val msg = "React(generation=0,time=0,view=??????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????_??????????????????????????????___????????????????????????????____???????????????????????????______?????????????????????????_______??????????????????????W?_________??WWWWWWWWWWWWWWWWWWWW__________?WWWWWWWWWWWWWWWWWWWW____________________________WW?_______________________????????_______________M____WWWWWW?????____________________W??????????____________________W??????????____________________W??????????____________________W??????????____________________W??????????_____________________??????????______________________?????????______________________?????????_______________________????????________________________???????_________________________??????__________________________?????___________________________????____________________________???____________________________W??,energy=1000,name=Test)"
    val results = ProtocolTranslator.fromServer(msg)
    results.head must be (React(0,"Test",0,"??????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????_??????????????????????????????___????????????????????????????____???????????????????????????______?????????????????????????_______??????????????????????W?_________??WWWWWWWWWWWWWWWWWWWW__________?WWWWWWWWWWWWWWWWWWWW____________________________WW?_______________________????????_______________M____WWWWWW?????____________________W??????????____________________W??????????____________________W??????????____________________W??????????____________________W??????????_____________________??????????______________________?????????______________________?????????_______________________????????________________________???????_________________________??????__________________________?????___________________________????____________________________???____________________________W??",1000))
  }
  it must "understand Welcome commands taken from server" in {
    val msg = "Welcome(name=Test,apocalypse=5000,round=0)"
    val results = ProtocolTranslator.fromServer(msg)
    results.head must be (Welcome("Test",5000,0))
  }

  "The translator" must "know how to create Move messages" in {
    val move = Move((1,1))
    val result = ProtocolTranslator.toServer(List(move))
    result must be ("Move(direction=1:1)")
  }
  it must "know how to send more than one order" in {
    val move = Move((1,1))
    val result = ProtocolTranslator.toServer(List(move,move))
    result must be ("Move(direction=1:1)|Move(direction=1:1)")
  }
  it must "know how to format the Set command" in {
    val set = Set(Map("key1" -> "value1", "key2" -> "value2"))
    val result = ProtocolTranslator.toServer(List(set))
    result must be ("Set(key1=value1,key2=value2)")
  }
}
