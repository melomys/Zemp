package Zemp

import scala.collection.mutable.ArrayBuffer
import org.scalajs.dom
import org.scalajs.dom.html


object ImportExport
{

  def export(stueck: Array[ArrayBuffer[Ton]],titel : String): Unit =
  {
    dom.console.log("in export")

    var result = "";
    var array : Array[Any] = Array("Nummer 1\r\n","Nummer 2")
    for(stimme <- stueck)
      {
        for(ton <- stimme)
          {
            result = result + ton.hoehe + "," + ton.start + "," + ton.laenge + "," + ton.text
            if(!ton.equals(stimme(stimme.length-1))) result = result + ";"
          }
        if(!stimme.equals(stueck(stueck.length-1))) result = result + "|"
      }


    var a :scalajs.js.Array[scalajs.js.Any] = scalajs.js.Array(result)
    var file = new dom.Blob(a)


    var el = dom.document.getElementById("export")
    var url = dom.URL.createObjectURL(file)
    el.setAttribute("href", url)
    el.setAttribute("download",titel + ".zemp")
    var ev = new dom.Event()
   // var event = new dom.MouseEvent()
   // el.dispatchEvent(event)
    el.innerHTML = "Hier klicken";




  }


}
