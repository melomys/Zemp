package Zemp

import scala.collection.mutable.ArrayBuffer
import org.scalajs.dom
import org.scalajs.dom.html


object ImportExport
{

  def definiereEvents(): Unit =
  {
      var importFeld = dom.document.getElementById("import").asInstanceOf[html.Input]
      var importButton = dom.document.getElementById("importButton").asInstanceOf[html.Button]

    importButton.onclick = (e: dom.MouseEvent) =>
      {
        val stueckString = importFeld.value
            Project.setStueck(importStueck(stueckString))
      }


  }


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
        if(!stimme.equals(stueck(stueck.length-1))) result = result + "#"
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




  }

  def importStueck(string :String) : Array[ArrayBuffer[Ton]] =
  {
    println(string)
    dom.console.log("anfang import")
    var stimmen = string.split("#")
    stimmen.map(stimme => println(stimme))
    var stueck = new Array[ArrayBuffer[Ton]](stimmen.length)
    dom.console.log("vor erster Schleife")
    var indexStimme = 0
    for(stimme <- stimmen)
      {
        stueck(indexStimme) = new ArrayBuffer[Ton]
        println("index: " + stueck.indexOf(stimme))
        dom.console.log("töne: ")
        var toene = stimme.split(";")
        toene.map(ton => println(ton))
        for(ton <- toene)
          {
            dom.console.log("zweite Schleife")
            var teile = ton.split(",")
            teile.map(teil => println(teil))
            var neuerTon = Ton(teile(0),teile(1).toDouble,teile(2).toDouble,teile(3))
            println(neuerTon)
            stueck(indexStimme).append(neuerTon)
          }
        println("-")
        indexStimme = indexStimme + 1
      }
    dom.console.log("fettischdd")
    dom.console.log(stueck(0))
    return stueck
  }


}
