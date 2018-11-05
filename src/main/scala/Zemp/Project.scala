package Zemp

import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport


case class Point(x: Int, y: Int)
@JSExport
object Project
{
  @JSExport
def main(canvas: html.Canvas): Unit = {
    var ctx = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

 canvas.width = dom.window.innerWidth.asInstanceOf[Int]
    canvas.height = dom.window.innerHeight.asInstanceOf[Int]

    println("canvas: "+ canvas.height)
    println("window: "+ dom.window.innerHeight)
    println("topOffset: "+ canvas.offsetTop)

    val takt = 5

    val tones = Array("C","B","Ab","G","F","Eb","D","C")
    val randOben = 150
    val randSeite = 50
    val intervall = 30
    val puffer = intervall/2
    val abstandToene = 30;
    val fontHeight = 15
    val zeilenHoehe = tones.length*abstandToene - 1 + 2*abstandToene

    val farbeHintergrund = "#d0d0d0"
    val farbeErsteStimme = "#DD1E1ECC"

    //laenge der linie
    val laenge = canvas.width - 2*randSeite;
    //platz zwischen erstem schlag und linienbeginn
    val emptySpace = laenge - (((laenge - puffer)/intervall) - ((laenge -puffer)/intervall)%takt)*intervall

    ctx.font = fontHeight.toString+ "pt Calibri"
    ctx.fillStyle="#101010"

    var lastDown = Point(-1,-1)


def zeichneHinterGrund() =
{
  ctx.strokeStyle =farbeHintergrund
  for (zeile <- 0 to 1)
  {
    for (i <- 0 to 7)
    {
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

    canvas.onmousedown = (e: dom.MouseEvent) =>
    {
      println("("+e.clientX+ "/" + e.clientY + ")")
     println(getNaechstesY(e.clientY.toInt))
      getNaechstesX(e.clientX.toInt)
     if(lastDown.x != -1)
       {

         // lastDown = Point(-1,-1)
       }

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
            lastDown = Point(-1,-1)
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




    zeichneHinterGrund()
//    dom.window.setInterval(() => run, 50)
  }
}
