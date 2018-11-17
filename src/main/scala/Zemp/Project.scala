package Zemp

import org.scalajs.dom
import org.scalajs.dom.html

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.annotation.JSExport


case class Point(x: Int, y: Int)

case class Ton(hoehe: String, start: Double, laenge: Double)

@JSExport
object Project
{


  var stueck: ArrayBuffer[Ton] = ArrayBuffer()
  val tones = Array("C", "B", "Ab", "G", "F", "Eb", "D", "C1")
  val takt = 5

  var canvas: html.Canvas = null
  var ctx: dom.CanvasRenderingContext2D = null

  val randOben = 150
  val randSeite = 50
  val intervall = 30
  val puffer = intervall / 2
  val abstandToene = 30

  val zeilenHoehe = tones.length * abstandToene - 1 + 2 * abstandToene

  val fontHeight = 15
  val font = fontHeight + "pt Calibri"
  val fontStyle = "#101010"

  val farbeHintergrund = "#d0d0d0"
  val farbeErsteStimme = "#DD1E1ECC"

  //laenge der linie
  var laenge = 0;
  //platz zwischen erstem schlag und linienbeginn
  var emptySpace = 0


  var lastDown = Point(-1, -1)

  @JSExport
  def main(can: html.Canvas): Unit =
  {
    initializiere(can)
    definiereEvents
    zeichneHinterGrund
    dom.window.setInterval(() => zeichne, 50)


  }

  def initializiere(can: html.Canvas) =
  {
    canvas = can;

    ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    println(ctx.font)

    canvas.width = dom.window.innerWidth.asInstanceOf[Int]
    canvas.height = dom.window.innerHeight.asInstanceOf[Int]
    laenge = canvas.width - 2 * randSeite;
    emptySpace = laenge - (((laenge - puffer) / intervall) - ((laenge - puffer) / intervall) % takt) * intervall

    println("canvas: " + canvas.height)
    println("window: " + dom.window.innerHeight)
    println("topOffset: " + canvas.offsetTop)


  }

  def definiereEvents() =
  {

    // auf Escape (keyCode = 27) wird kontinuierliche Zeichnung unterbrochen
    dom.document.onkeypress = (e: dom.KeyboardEvent) =>
    {
      if (e.keyCode == 27)
      {
        lastDown = Point(-1, -1)
      }

    }

    canvas.onmousedown = (e: dom.MouseEvent) =>
    {

      println("MouseDown on: ("+e.clientX+"/"+e.clientY+")")
      getNaechstesX(e.clientX.toInt)

    }
    canvas.onmouseup = (e: dom.MouseEvent) =>
    {

      val x = getXCoordinateCanvas(e.clientX.toInt)
      val y = getYCoordinateCanvas(e.clientY.toInt)
      if (lastDown.x == -1)
      {
        lastDown = Point(x, y)

      } else
      {


        //hinzufuegen zu stueck

        //start ist der letzte Punkt an dem eine Note hinzugefuegt worden ist
        val start = getSchlagpunkt(lastDown.x)
        //note kriegt den String zugewiesen auf den geklickt wurde
        val note = getNote(y)

        val laenge = getSchlagpunkt(x) - start

        stueck.append(Ton(note, start, laenge))



        // für kein kontinuierliches Ziehen die Kommentierung der beiden folgenden Zeilen tauschen
        lastDown = Point(x, y)
        //   lastDown = Point(-1,-1)


      }

    }
  }

  def zeichne() =
  {
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    zeichneHinterGrund()


    ctx.lineWidth = 2
    ctx.strokeStyle = farbeErsteStimme

    for (i <- 0 to stueck.length - 1)
    {
      ctx.beginPath
      ctx.moveTo(getXKoordinateZumZeichnenAusTon(stueck(i)), getYKoordinateZumZeichnenAusTon(stueck(i)))
      ctx.lineTo(getXKoordinateZumZeichnen(stueck(i).start + stueck(i).laenge), getYKoordinateZumZeichnenAusTon(stueck(i)))

      println("Punkt zum Zeichnen: ("+ getXKoordinateZumZeichnen(stueck(i).start + stueck(i).laenge) + "/" + getYKoordinateZumZeichnenAusTon(stueck(i)) + ")")
      ctx.stroke

    }

  }

  def zeichneHinterGrund() =
  {
    ctx.strokeStyle = farbeHintergrund
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

  def getNaechstesX(x: Int): Int =
  {
    val temp = math.max(math.min(laenge + randSeite, x), randSeite + emptySpace)
    //abStartPoint beschreibt die Pixelanzahl im verhältnis zum ersten Schlag
    var abStartPoint = temp - randSeite - emptySpace
    //stellt sicher, dass das resultierende x im gültigen bereich liegt
    abStartPoint = math.max(0, abStartPoint)
    abStartPoint = math.min(abStartPoint, laenge - emptySpace)
    val rest = abStartPoint % intervall;
    if (rest < intervall / 2) temp - rest else x + intervall - rest
  }

  def getNaechstesY(y: Int): Int =
  {
    val temp = y - randOben
    val rest = temp % abstandToene
    //println("voll: " +(y-randOben))
    // println("rest: " + rest)
    if (rest < abstandToene / 2) y - rest else y + abstandToene - rest

  }

  def getSchlagpunkt(x: Int): Int =
  {
    //ab dem ersten Schlag
    (getNaechstesX(x) - randSeite - emptySpace) / intervall + 1

  }

  def getNote(y: Int): String =
  {
    val temp = (getNaechstesY(y) - randOben) / abstandToene
    tones(temp)
  }

  def getYCoordinateCanvas(y: Int): Int =
  {
    y - canvas.offsetTop.toInt
  }

  def getXCoordinateCanvas(x: Int): Int =
  {
    x - canvas.offsetLeft.toInt
  }

  def getXKoordinateZumZeichnenAusTon(ton: Ton): Int =
  {
    getXKoordinateZumZeichnen(ton.start)

  }

  def getYKoordinateZumZeichnenAusTon(ton: Ton): Int =
  {
    val y = tones.indexOf(ton.hoehe)
    randOben + abstandToene * y

  }

  def getXKoordinateZumZeichnen(x: Double): Int =
  {

    (randSeite + emptySpace + (x-1) * intervall).toInt

  }
}

