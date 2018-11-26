package Zemp

import org.scalajs.dom
import org.scalajs.dom.html
import outwatch.dom._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.annotation.JSExport


case class Pointt(x: Int, y: Int)

case class Ton(hoehe: String, start: Double, laenge: Double)

@JSExport
object Project
{


  var stueck: ArrayBuffer[Ton] = ArrayBuffer()
  val tones = Array("C", "B", "Ab", "G", "F", "Eb", "D", "C1")
  val takt = 5

  var canvas: html.Canvas = null
  var ctx: dom.CanvasRenderingContext2D = null


  val intervall = 30
  val abstandToene = 15
  val zeilenAnzahl = 4

  val randOben = 5 * abstandToene
  val randSeite = 50
  val puffer = intervall / 2

  val zeilenHoehe = (tones.length - 1) * abstandToene + 3 * abstandToene

  val fontHeight = 15
  val font = fontHeight + "pt Calibri"
  val fontStyle = "#101010"

  val farbeHintergrund = "#d0d0d0"
  val farbeErsteStimme = "#DD1E1ECC"
  val farbeHint = "#0000ff"

  //laenge der linie
  var laengeHorizontalLinie = 0;
  //platz zwischen erstem schlag und linienbeginn
  var emptySpace = 0


  var lastDown = Pointt(-1, -1)
  var hintTon = Ton("C", 0, 0)

  import monix.execution._
  import monix.reactive._
  import rx._

  implicit def RxAsValueObservable: AsValueObservable[Rx] = new AsValueObservable[Rx]
  {
    override def as[T](stream: Rx[T]): ValueObservable[T] = new ValueObservable[T]
    {
      def value = Option(stream.now)

      def observable = Observable.create[T](OverflowStrategy.Unbounded)
        { observer =>
          implicit val ctx = Ctx.Owner.Unsafe
          val obs = stream.triggerLater(observer.onNext(_))
          Cancelable(() => obs.kill())
        }
    }
  }

  implicit object VarAsObserver extends AsObserver[Var]
  {
    override def as[T](stream: Var[_ >: T]): Observer[T] = new Observer.Sync[T]
    {
      override def onNext(elem: T): Ack =
      {
        stream() = elem
        Ack.Continue
      }

      override def onError(ex: Throwable): Unit = throw ex

      override def onComplete(): Unit = ()
    }
  }

  // if you want to use managed()
  implicit def obsToCancelable(obs: Obs): Cancelable =
  {
    Cancelable(() => obs.kill())
  }

  def main(args: Array[String]): Unit =
  {

    val canvas: html.Canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]
    // val myComponent = div(color := "magenta", "Hello World", cls := "hello")

    //OutWatch.renderReplace("#app", myComponent).unsafeRunSync()


    // val dynamicSize: Observable[VDomModifier] = Observable.interval(1 second).map(i => fontSize := s"${i}px")
    // val meineVal = Handler.unsafe("3")

    // val newDiv = div(meineVal)

    // OutWatch.renderInto("#app", newDiv).unsafeRunSync()

    // meineVal.onNext("neuer String")

    initializiere(canvas)
    definiereEvents
    //zeichneHinterGrund

    //    paper.setup(canvas)
    //
    //    dom.console.log(paper)
    //    dom.console.log(paper.view.toString)
    //
    //
    //    paper.view.draw
    //    val path = new Path
    //   // path.strokeColor = new Color("black")
    //
    //
    //    path.strokeColor = "black"
    //
    //    val start = new Point(200,200)
    //    val start2 = new Point(300,300)
    //    path.moveTo(start)
    //    val end = new Point(400,400)
    //    path.lineTo(end)
    //
    //    dom.console.log(path)
    //    dom.console.log(start)
    //    dom.console.log(end)
    //    paper.view.draw

    dom.window.setInterval(() => zeichne, 100)
  }

  @JSExport
  def main2(can: html.Canvas): Unit =
  {


  }

  def initializiere(can: html.Canvas) =
  {
    canvas = can;

    ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    // println(ctx.font)

    canvas.width = dom.window.innerWidth.asInstanceOf[Int]
    canvas.height = dom.window.innerHeight.asInstanceOf[Int]
    laengeHorizontalLinie = canvas.width - 2 * randSeite;
    emptySpace = laengeHorizontalLinie - (((laengeHorizontalLinie - puffer) / intervall) - ((laengeHorizontalLinie - puffer) / intervall) % takt) * intervall


  }

  def definiereEvents() =
  {

    // auf Escape (keyCode = 27) wird kontinuierliche Zeichnung unterbrochen
    dom.document.onkeypress = (e: dom.KeyboardEvent) =>
    {
      if (e.keyCode == 27)
      {
        lastDown = Pointt(-1, -1)
      }

    }

    canvas.onmousedown = (e: dom.MouseEvent) =>
    {

      //  println("MouseDown on: (" + e.clientX + "/" + e.clientY + ")")
      getNaechstesX(e.clientX.toInt)
    }
    canvas.onmouseup = (e: dom.MouseEvent) =>
    {

      val x = getXCoordinateFromCanvas(e.clientX.toInt)
      val y = getYCoordinateFromCanvas(e.clientY.toInt)
      if (lastDown.x == -1)
      {
        val start = getSchlagpunkt(x,y)
        val ton = Ton("TesteObLastDownGueltigSeinKann",start,0)
        if(isValid(ton)) lastDown = Pointt(x, y) else lastDown.x == -1


      } else
      {


        //hinzufuegen zu stueck

        //start ist der letzte Punkt an dem eine Note hinzugefuegt worden ist
        val start = getSchlagpunkt(lastDown.x, lastDown.y)
        //note kriegt den String zugewiesen auf den geklickt wurde
        val note = getNote(y)

        val laenge = getSchlagpunkt(x, y) - start


        val ton = Ton(note, start, laenge)


        if (isValid(ton))
        {
          stueck.append(ton)
          stueck = stueck.sortWith((A, B: Ton) => A.start < B.start)
          //  println(stueck)

          hintTon = Ton(note, start + laenge, 0)


          // für kein kontinuierliches Ziehen die Kommentierung der beiden folgenden Zeilen tauschen
          lastDown = Pointt(x, y)
          //   lastDown = Point(-1,-1)

        }
      }

    }
    canvas.onmousemove = (e: dom.MouseEvent) =>
    {
      //wenn noch nichts angeklickt wurde muss hier nichts weiter getan werden
      if (lastDown.x != -1)
      {
        val x = getXCoordinateFromCanvas(e.clientX.toInt)
        val y = getYCoordinateFromCanvas(e.clientY.toInt)

        val start = getSchlagpunkt(lastDown.x, lastDown.y)
        //note kriegt den String zugewiesen auf den geklickt wurde
        val note = getNote(y)

        val laenge = getSchlagpunkt(x, y) - start

        hintTon = Ton(note, start, laenge)
        if(!isValid(hintTon)) hintTon = Ton(note,start,0)

      }
    }

    def isValid(ton: Ton): Boolean =
    {
      if (ton.laenge <= 0 && !ton.hoehe.equals("TesteObLastDownGueltigSeinKann")) return false
      if(stueck.length == 0) return true

      def rek(i: Int): Boolean =
      {
        println(stueck)
        if (i == stueck.length) {

          if(stueck(stueck.length - 1).start + stueck(stueck.length-1).laenge > ton.start) return false
          return true;
        }
        if (ton.start == stueck(i).start)
        {
          return false
        }
        else if (ton.start < stueck(i).start)
        {
          if( i > 0 && stueck(i-1).start + stueck(i-1).laenge > ton.start) return false
          if(ton.start + ton.laenge > stueck(i).start) return false
          return true
        }else{
          rek(i+1)
        }

      }


      rek(0)

//      for(i <- 0 to stueck.length)
//        {
//            if(ton.start == stueck(i).start) return false
//            if(ton.start < stueck(i).start)
//              {
//                if(ton.start + ton.laenge > stueck(i).start) return false
//                return true
//              }
//        }
//      return true


    }
  }

  def zeichne() =
  {


    var lastPoint = Pointt(0, 0)
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    zeichneHinterGrund()
    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / intervall

    ctx.lineWidth = 2
    ctx.strokeStyle = farbeErsteStimme

    for (i <- 0 to stueck.length - 1)
    {


      val start = stueck(i).start
      val laenge = stueck(i).laenge


      if (existiertDirekterVorgaengerTon(stueck(i)))
      {
        ctx.beginPath
        ctx.moveTo(lastPoint.x, lastPoint.y)
        ctx.lineTo(lastPoint.x, getYKoordinateZumZeichnenAusTon(stueck(i)) + zeilenAbstand(start))
        ctx.stroke
      }
      ctx.beginPath
      ctx.moveTo(getXKoordinateZumZeichnenAusTon(stueck(i)), getYKoordinateZumZeichnenAusTon(stueck(i)) + zeilenAbstand(start))


      //zeile ist immer die relative zeile zum startschlag
      def rek(zeile: Int): Unit =
      {
        //  println(start + laenge - (getZeile(start) + zeile) * schlaegeProZeile + " <= schlaegeProZeile: " + schlaegeProZeile)

        if (start + laenge - (getZeile(start) + zeile) * schlaegeProZeile <= schlaegeProZeile)
        {
          var restSchlaege = start % schlaegeProZeile
          if (restSchlaege == 0) restSchlaege = schlaegeProZeile
          lastPoint = Pointt(getXKoordinateZumZeichnen(restSchlaege + laenge - zeile * schlaegeProZeile), getYKoordinateZumZeichnenAusTon(stueck(i)) + zeilenAbstand(start) + (zeile * zeilenHoehe))
          ctx.lineTo(lastPoint.x, lastPoint.y)
          ctx.stroke
        }
        else
        {
          ctx.lineTo(getXKoordinateZumZeichnen(schlaegeProZeile + 1), getYKoordinateZumZeichnenAusTon(stueck(i)) + zeilenAbstand(start) + (zeile * zeilenHoehe))
          ctx.stroke
          //dom.window.alert("hallo")
          ctx.beginPath
          ctx.moveTo(getXKoordinateZumZeichnen(1), getYKoordinateZumZeichnenAusTon(stueck(i)) + zeilenAbstand(start) + ((zeile + 1) * zeilenHoehe))
          rek(zeile + 1)
        }
      }

      rek(0)

    }
    zeichneHint

  }

  def zeichneHinterGrund() =
  {
    ctx.strokeStyle = farbeHintergrund
    for (zeile <- 0 to zeilenAnzahl - 1)
    {
      for (i <- 0 to tones.length - 1)
      {
        ctx.font = font
        ctx.fillStyle = fontStyle
        ctx.lineWidth = 1

        ctx.fillText(tones(i), randSeite - 30, randOben + i * abstandToene + zeile * zeilenHoehe + fontHeight / 2)
        ctx.beginPath()

        ctx.moveTo(randSeite, randOben + i * abstandToene + zeile * zeilenHoehe)
        ctx.lineTo(randSeite + laengeHorizontalLinie, randOben + i * abstandToene + zeile * zeilenHoehe)
        ctx.stroke()
      }
      for (i <- 0 to (laengeHorizontalLinie - emptySpace) / intervall)
      {

        ctx.beginPath()
        if (i % takt == 0) ctx.lineWidth = 2 else ctx.lineWidth = 1
        ctx.moveTo(randSeite + laengeHorizontalLinie - i * intervall, randOben + zeile * zeilenHoehe)
        ctx.lineTo(randSeite + laengeHorizontalLinie - i * intervall, randOben + zeile * zeilenHoehe + abstandToene * (tones.length - 1))
        ctx.stroke()
      }
    }

  }

  def zeichneHint(): Unit =
  {
    val reverse = stueck.reverse
    ctx.strokeStyle = farbeHint;
    if (lastDown.x != -1 && hintTon.laenge > 0)
    {
      val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / intervall
      val start = hintTon.start

      val laenge = hintTon.laenge
      ctx.beginPath
      ctx.moveTo(getXKoordinateZumZeichnenAusTon(hintTon), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(hintTon.start))

      //zeile ist immer die relative zeile zum startschlag
      def rek(zeile: Int): Unit =
      {

        if (start + laenge - (getZeile(start) + zeile) * schlaegeProZeile <= schlaegeProZeile)
        {
          var restSchlaege = start % schlaegeProZeile
          if (restSchlaege == 0) restSchlaege = schlaegeProZeile
          ctx.lineTo(getXKoordinateZumZeichnen(restSchlaege + laenge - zeile * schlaegeProZeile), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(start) + (zeile * zeilenHoehe))
          ctx.stroke
        }
        else
        {
          ctx.lineTo(getXKoordinateZumZeichnen(schlaegeProZeile + 1), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(start) + (zeile * zeilenHoehe))
          ctx.stroke
          //dom.window.alert("hallo")
          ctx.beginPath
          ctx.moveTo(getXKoordinateZumZeichnen(1), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(start) + ((zeile + 1) * zeilenHoehe))
          rek(zeile + 1)
        }
      }

      rek(0)

      if (stueck.length > 0)
      {
        if (hintTon.start < stueck(0).start)
        {

          if (hintTon.start + hintTon.laenge == stueck(0).start)
          {
            ctx.beginPath
            ctx.moveTo(getXKoordinateZumZeichnen(hintTon.start + hintTon.laenge), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(hintTon.start + hintTon.laenge))
            ctx.lineTo(getXKoordinateZumZeichnen(stueck(0).start), getYKoordinateZumZeichnenAusTon(stueck(0)) + zeilenAbstand(stueck(0).start))
            ctx.stroke
          }
        } else
        {


          def rek2(i: Int): Unit =
          {
            if (reverse(i).start < hintTon.start)
            {

              val ton = reverse(i)
              if (ton.start + ton.laenge == hintTon.start)
              {
                ctx.beginPath
                ctx.moveTo(getXKoordinateZumZeichnen(ton.start + ton.laenge), getYKoordinateZumZeichnenAusTon(ton) + zeilenAbstand(ton.start))
                ctx.lineTo(getXKoordinateZumZeichnen(hintTon.start), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(hintTon.start))
                ctx.stroke
              }
              if (i > 0 && hintTon.start + hintTon.laenge == reverse(i - 1).start)
              {
                ctx.beginPath
                ctx.moveTo(getXKoordinateZumZeichnen(hintTon.start + hintTon.laenge), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(hintTon.start + hintTon.laenge))
                ctx.lineTo(getXKoordinateZumZeichnen(reverse(i - 1).start), getYKoordinateZumZeichnenAusTon(reverse(i - 1)) + zeilenAbstand(reverse(i - 1).start))
                ctx.stroke
              }


            } else
            {
              rek2(i + 1)
            }

          }

          rek2(0)
        }

      }
    }

  }

  def getNaechstesX(x: Int): Int =
  {
    val temp = math.max(math.min(laengeHorizontalLinie + randSeite, x), randSeite + emptySpace)
    //abStartPoint beschreibt die Pixelanzahl im verhältnis zum ersten Schlag
    var abStartPoint = temp - randSeite - emptySpace
    //stellt sicher, dass das resultierende x im gültigen bereich liegt
    abStartPoint = math.max(0, abStartPoint)
    abStartPoint = math.min(abStartPoint, laengeHorizontalLinie - emptySpace)
    val rest = abStartPoint % intervall;
    if (rest < intervall / 2) temp - rest else x + intervall - rest
  }

  def getNaechstesY(y: Int): Int =
  {

    val rest = y % abstandToene
    //println("voll: " +(y-randOben))
    // println("rest: " + rest)
    if (rest < abstandToene / 2) y - rest else y + abstandToene - rest

  }

  def getSchlagpunkt(x: Int, y: Int): Int =
  {
    val zeile: Int = (y - randOben) / zeilenHoehe

    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / intervall
    //ab dem ersten Schlag
    //  println("zeile: " + zeile)
    // println("schlagpunkt: " + ((getNaechstesX(x) - randSeite - emptySpace) / intervall + 1))
    (getNaechstesX(x) - randSeite - emptySpace) / intervall + 1 + zeile * schlaegeProZeile

  }

  def getNote(y: Int): String =
  {
    //anzahl der toene ab dem höchsten möglichen Ton pro Zeile

    //manchmal +1 nach abstandToene manchmal nicht >: aber wahrscheinlich nicht
    val temp = ((getNaechstesY(y) - randOben) / abstandToene) % (zeilenHoehe / abstandToene)

    //wenn temp im bereich von tones liegt, kann direkt der ton String gespeichert werden
    if (temp >= 0 && temp < tones.length) tones(temp) else temp.toString


  }

  def getYCoordinateFromCanvas(y: Int): Int =
  {
    y - canvas.offsetTop.toInt
  }

  def getXCoordinateFromCanvas(x: Int): Int =
  {
    x - canvas.offsetLeft.toInt
  }

  def getXKoordinateZumZeichnenAusTon(ton: Ton): Int =
  {
    val schlaegeProZeile = ((laengeHorizontalLinie - emptySpace) / intervall)
    val moduloSchlag = ton.start % schlaegeProZeile
    getXKoordinateZumZeichnen(if (moduloSchlag == 0) schlaegeProZeile else moduloSchlag)

  }

  def getYKoordinateZumZeichnenAusTon(ton: Ton): Int =
  {

    var y = tones.indexOf(ton.hoehe)
    if (y == -1) y = ton.hoehe.toInt
    randOben + abstandToene * y

  }

  def getXKoordinateZumZeichnen(x: Double): Int =
  {

    (randSeite + emptySpace + (x - 1) * intervall).toInt

  }

  //gibt den abstand wieder, der auf die y  koordinate raufgerchnet werden muss,
  // je nach dem in welcher zeile man ist

  def zeilenAbstand(schlag: Double): Int =
  {
    getZeile(schlag) * zeilenHoehe

  }

  def getZeile(schlag: Double): Int =
  {
    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / intervall
    ((schlag - 1) / schlaegeProZeile).toInt
  }

  def existiertDirekterVorgaengerTon(ton: Ton): Boolean =
  {
    def rek(i: Int): Boolean =
    {
      if (i == stueck.length)
      {
        false
      } else
      {
        if (ton.start == stueck(i).start + stueck(i).laenge) true else rek(i + 1)
      }
    }

    rek(0)

  }

}

