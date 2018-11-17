package Zemp

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.KeyboardEvent

import scala.scalajs.js.annotation.JSExport


case class Point(x: Int, y: Int)

case class Ton(hoehe : String,start: Double, laenge : Double)
@JSExport
object Project
{

  val takt = 5

    var canvas : html.Canvas = null
    var ctx : dom.CanvasRenderingContext2D = null
    val tones = Array("C","B","Ab","G","F","Eb","D","C1")
    val randOben = 150
    val randSeite = 50
    val intervall = 30
    val puffer = intervall/2
    val abstandToene = 30

  val zeilenHoehe = tones.length*abstandToene - 1 + 2*abstandToene

  val fontHeight = 15
  val font = fontHeight+"pt Calibri"
  val fontStyle = "#101010"

    val farbeHintergrund = "#d0d0d0"
    val farbeErsteStimme = "#DD1E1ECC"

    //laenge der linie
    var laenge = 0;
    //platz zwischen erstem schlag und linienbeginn
    var emptySpace = 0

  @JSExport
  def main(can: html.Canvas): Unit = {
    canvas = can;

    ctx = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]
    println(ctx.font)

 canvas.width = dom.window.innerWidth.asInstanceOf[Int]
    canvas.height = dom.window.innerHeight.asInstanceOf[Int]
    laenge = canvas.width - 2*randSeite;
    emptySpace = laenge - (((laenge - puffer)/intervall) - ((laenge -puffer)/intervall)%takt)*intervall

    println("canvas: "+ canvas.height)
    println("window: "+ dom.window.innerHeight)
    println("topOffset: "+ canvas.offsetTop)


    var lastDown = Point(-1,-1)

    dom.document.onkeypress = (e: dom.KeyboardEvent) =>
      {
        if(e.keyCode == 27)
          {
            lastDown = Point(-1,-1)
          }

      }

    canvas.onmousedown = (e: dom.MouseEvent) =>
    {
      println("("+e.clientX+ "/" + e.clientY + ")")
     println(getNaechstesY(e.clientY.toInt))
      getNaechstesX(e.clientX.toInt)

    }
    canvas.onmouseup = (e:dom.MouseEvent) =>
      {
        val x = getXCoordinateCanvas(e.clientX.toInt)
        val y = getYCoordinateCanvas(e.clientY.toInt)
        if(lastDown.x == -1)
        {
          lastDown = Point(x,y)

        }else
          {
            ctx.lineWidth = 2
            ctx.strokeStyle = farbeErsteStimme
            ctx.beginPath()
            ctx.moveTo(getNaechstesX(lastDown.x),getNaechstesY(lastDown.y))
            ctx.lineTo(getNaechstesX(x),getNaechstesY(y))
            ctx.stroke()

            // für kein kontinuierliches Ziehen die Kommentierung der beiden folgenden Zeilen tauschen
              lastDown = Point(x,y);
         //   lastDown = Point(-1,-1)


          }

      }
    zeichneHinterGrund()
//    dom.window.setInterval(() => run, 50)
  }

  def zeichneHinterGrund() =
  {
  ctx.strokeStyle =farbeHintergrund
  for (zeile <- 0 to 1)
  {
    for (i <- 0 to 7)
    {
      ctx.font = font
      ctx.fillStyle = fontStyle
      ctx.lineWidth = 1

      ctx.fillText(tones(i), randSeite - 30, randOben + i * abstandToene + zeile * zeilenHoehe + fontHeight / 2)
      ctx.beginPath()

      ctx.moveTo(randSeite, randOben + i * abstandToene + zeile * zeilenHoehe)
      ctx.lineTo(randSeite + laenge, randOben + i * abstandToene + zeile * zeilenHoehe)
      ctx.stroke()
    }
    for (i <- 0 to (laenge - emptySpace) / intervall)
    {
      ctx.beginPath()
      if (i % takt == 0) ctx.lineWidth = 2 else ctx.lineWidth = 1
      ctx.moveTo(randSeite + laenge - i * intervall, randOben + zeile * zeilenHoehe)
      ctx.lineTo(randSeite + laenge - i * intervall, randOben + zeile * zeilenHoehe + intervall * (tones.length - 1))
      ctx.stroke()
    }
  }

}
  def getNaechstesX(x: Int): Int = {
    val temp = math.max(math.min(laenge+randSeite, x), randSeite+emptySpace)
    //abStartPoint beschreibt die Pixelanzahl im verhältnis zum ersten Schlag
    var abStartPoint = temp-randSeite- emptySpace
    //stellt sicher, dass das resultierende x im gültigen bereich liegt
    abStartPoint = math.max(0, abStartPoint)
    abStartPoint = math.min(abStartPoint, laenge - emptySpace)
    val rest = abStartPoint % intervall;
    if(rest < intervall/2) temp - rest else x+intervall-rest
  }

  def getNaechstesY(y: Int) : Int = {
    val temp = y - randOben
    val rest = temp%abstandToene
    println("voll: " +(y-randOben))
    println("rest: " + rest)
    if(rest < abstandToene/2 ) temp+randOben-rest else temp+randOben+abstandToene-rest

  }

  def getYCoordinateCanvas(y: Int) : Int = {
    y - canvas.offsetTop.toInt
  }
  def getXCoordinateCanvas(x: Int): Int = {
    x - canvas.offsetLeft.toInt
  }
}
