package Zemp

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.DragEvent
import outwatch.dom._
import rx._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.annotation.JSExport


case class Pointt(x: Int, y: Int)

case class Ton(hoehe: String, start: Double, laenge: Double, text: String = "jo")

@JSExport
object Project
{

  var stueck = Array(new ArrayBuffer[Ton], new ArrayBuffer[Ton], new ArrayBuffer[Ton])
  var aktuelleStimme = stueck(0)
  val tones = Array("C", "B", "Ab", "G", "F", "Eb", "D", "C1")

  var takt = 5
  var titel = "Titel"


  val maxSchlaegeProZeile = 32;

  var canvas: html.Canvas = null
  var ctx: dom.CanvasRenderingContext2D = null
  var textFeld: html.Input = null
  var taktFeld: html.Input = null
  var zeilenFeld: html.Input = null
  var titelFeld :html.Input = null
  var speichernButton: html.Button = null
  var textFeldIndex = -1

  val intervallViertel = 32
  val aktuelleNotenLaenge = intervallViertel

  val abstandToene = intervallViertel
  var zeilenAnzahl = 2

  val randOben = 3 * abstandToene
  val randSeite = 50
  val puffer = intervallViertel / 2


  val zeilenAbstand = 3 * abstandToene
  val zeilenHoehe = (tones.length - 1) * abstandToene + zeilenAbstand

  val fontHeight = 15
  val font = fontHeight + "pt Calibri"
  val fontStyle = "#101010"
  val fontHeightText = 12
  val fontText = fontHeightText + "pt Calibri"
  val fontBoldText = "bold " + fontText


  val farbeHintergrund = "#d0d0d0"
  // val farbeErsteStimme = "#DD1E1ECC"

  val stimmenFarben = Array("#dd1e1e", "#54b2d1", "#4de87b")
  val farbeHint = "#0000ff"

  //laenge der linie
  var laengeHorizontalLinie = 0;
  //platz zwischen erstem schlag und linienbeginn
  var emptySpace = 0


  var lastDown = Pointt(-1, -1)
  var hintTon = Ton("C", 0, 0)
  var selection: Ton = null
  var bewegterTonIndex = -1
  var bewegterTonIndexHorizontale = -1
  var alterStart = 0.0
  var laengeTon = 0.0
  var laengeVorgaengerTon = 0.0

  var drag = false

  import monix.execution._
  import monix.reactive._

  class MyAudio(url : String) extends html.Audio
  {

  }

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

    // val myComponent = div(color := "magenta", "Hello World", cls := "hello")

    //OutWatch.renderReplace("#app", myComponent).unsafeRunSync()


    // val dynamicSize: Observable[VDomModifier] = Observable.interval(1 second).map(i => fontSize := s"${i}px")
    // val meineVal = Handler.unsafe("3")

    // val newDiv = div(meineVal)

    // OutWatch.renderInto("#app", newDiv).unsafeRunSync()

    // meineVal.onNext("neuer String")


    //  canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]
    //  paper.setup(canvas)
    // dom.console.log(pdf)
    initializiere()
    definiereEvents
    ImportExport.definiereEvents

    var a = dom.document.getElementById("audio").asInstanceOf[html.Audio]
    a.play
    //Thread.sleep(5000)
    //a.setAttribute("loop", "false")
    dom.console.log(a.duration)
   a.play
    dom.console.log(2)


    var played = 0
    a.addEventListener("ended", (e : dom.Event) =>
    {
      if(played<3)
        {
          a.play
          played = played +1
          println(played)
        }
    })


    //var b = new MyAudio("rtn.mp3")

    def ton() : Unit =
    {
      a.play
    }



    //zeichneHinterGrund

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


  def initializiere() =
  {
    canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]

    ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    //paper.setup(canvas)test
    //dom.console.log(paper)
    //   var p = new Path
    //  dom.console.log(p)


    textFeld = dom.document.getElementById("textAendern").asInstanceOf[html.Input]
    taktFeld = dom.document.getElementById("takt").asInstanceOf[html.Input]
    zeilenFeld = dom.document.getElementById("zeilen").asInstanceOf[html.Input]
    titelFeld = dom.document.getElementById("titel").asInstanceOf[html.Input]
    zeilenFeld.value = zeilenAnzahl + ""

    speichernButton = dom.document.getElementById("speichern").asInstanceOf[html.Button]

    onResize
  }

  def onResize(): Unit =
  {
    var canvasDiv = dom.document.getElementById("canvasDiv").asInstanceOf[html.Div]
    canvasDiv.setAttribute("style", "height: " + (dom.window.innerHeight - 50) + "px")
    laengeHorizontalLinie = puffer + intervallViertel * (maxSchlaegeProZeile - maxSchlaegeProZeile % takt)
    emptySpace = laengeHorizontalLinie - (((laengeHorizontalLinie - puffer) / intervallViertel) - ((laengeHorizontalLinie - puffer) / intervallViertel) % takt) * intervallViertel
  //  dom.console.log("emptySpace: " + emptySpace)
  //  dom.console.log("schlaege: " + (maxSchlaegeProZeile - maxSchlaegeProZeile % takt))
    canvas.width = laengeHorizontalLinie + randSeite * 2
    canvas.height = randOben + zeilenAnzahl * (zeilenHoehe)

  }


  def definiereEvents() =
  {


    val export = dom.document.getElementById("Exportieren").asInstanceOf[html.Button]
    export.onmousedown = (e: dom.MouseEvent) =>
    {

      ImportExport.pdfExport(stueck)
    }

    // auf Escape (keyCode = 27) wird kontinuierliche Zeichnung unterbrochen
    dom.document.onkeypress = (e: dom.KeyboardEvent) =>
    {
      if (e.keyCode == 27)
      {
        lastDown = Pointt(-1, -1)
        selection = null
      }
      else if (e.keyCode == 46)
      {
        aktuelleStimme.remove(aktuelleStimme.indexOf(selection))
        selection = null
      }


    }


    canvas.onmousedown = (e: dom.MouseEvent) =>
    {

      textFeld.value = ""
      textFeldIndex = -1
      //verschiebung
      if (lastDown.x == -1)
      {
        val x = getXCoordinateFromCanvas(e.clientX.toInt)
        val y = getYCoordinateFromCanvas(e.clientY.toInt)

        val schlagPunkt = getSchlagpunkt(x, y)
        val note = getNote(y, schlagPunkt)

        for (ton <- aktuelleStimme)
        {
          if (schlagPunkt >= ton.start && schlagPunkt < ton.start + ton.laenge && bewegterTonIndex == -1)
          {
            if (ton.hoehe.equals(note))
            {
              bewegterTonIndex = aktuelleStimme.indexOf(ton)
              selection = ton
            }

            if (schlagPunkt == ton.start && existiertDirekterVorgaengerTon(ton, aktuelleStimme))
            {
              val indexTon = if (tones.indexOf(ton.hoehe) != -1) tones.indexOf(ton.hoehe) else ton.hoehe.toInt
              val indexTonVorgaenger = if (tones.indexOf(aktuelleStimme(aktuelleStimme.indexOf(ton) - 1).hoehe) != -1) tones.indexOf(aktuelleStimme(aktuelleStimme.indexOf(ton) - 1).hoehe) else aktuelleStimme(aktuelleStimme.indexOf(ton) - 1).hoehe.toInt
              val indexNote = if (tones.indexOf(note) != -1) tones.indexOf(note) else note.toInt
              if ((indexTon > indexNote && indexTonVorgaenger <= indexNote) || indexTon < indexNote && indexTonVorgaenger >= indexNote)
              {
                bewegterTonIndexHorizontale = aktuelleStimme.indexOf(ton)
                alterStart = ton.start
                laengeTon = ton.laenge
                laengeVorgaengerTon = aktuelleStimme(aktuelleStimme.indexOf(ton) - 1).laenge
              }
            }

          }
        }
      }


    }

    canvas.onmouseup = (e: dom.MouseEvent) =>
    {

      val x = getXCoordinateFromCanvas(e.clientX.toInt)
      val y = getYCoordinateFromCanvas(e.clientY.toInt)
      bewegterTonIndex = -1
      bewegterTonIndexHorizontale = -1
      if (lastDown.x == -1)
      {
        val start = getSchlagpunkt(x, y)
        val ton = Ton("TesteObLastDownGueltigSeinKann", start, 0)
        if (isValid(ton)) lastDown = Pointt(x, y) else lastDown.x == -1


      } else
      {


        //hinzufuegen zu stueck

        //start ist der letzte Punkt an dem eine Note hinzugefuegt worden ist
        val start = getSchlagpunkt(lastDown.x, lastDown.y)
        //note kriegt den String zugewiesen auf den geklickt wurde


        val laenge = getSchlagpunkt(x, y) - start

        val note = getNote(y, start)
        val ton = Ton(note, start, laenge)


        if (isValid(ton))
        {
          aktuelleStimme.append(ton)
          for (i <- 0 to stueck.length - 1)
          {
            if (aktuelleStimme.equals(stueck(i)))
            {
              stueck(i) = aktuelleStimme.sortWith((A, B: Ton) => A.start < B.start)
              aktuelleStimme = stueck(i)
              //zeilen anzahl anpassen

              val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge
              var letzterSchlag = aktuelleStimme(aktuelleStimme.length - 1).start + aktuelleStimme(aktuelleStimme.length - 1).laenge
              var grenzeFuerNeueZeile = aktuelleNotenLaenge / intervallViertel * takt
              if(letzterSchlag % schlaegeProZeile > schlaegeProZeile - grenzeFuerNeueZeile)
                {
                  zeilenAnzahl = math.max(2, getZeile(letzterSchlag)+2)
                  onResize()
                }

            }
          }

          hintTon = Ton(note, start + laenge, 0)


          // für kein kontinuierliches Ziehen die Kommentierung der beiden folgenden Zeilen tauschen
          lastDown = Pointt(x, y)
          //   lastDown = Point(-1,-1)

        }
      }

    }
    canvas.onmousemove = (e: dom.MouseEvent) =>
    {

      val x = getXCoordinateFromCanvas(e.clientX.toInt)
      val y = getYCoordinateFromCanvas(e.clientY.toInt)
      //wenn noch nichts angeklickt wurde muss hier nichts weiter getan werden
      if (lastDown.x != -1)
      {
        val start = getSchlagpunkt(lastDown.x, lastDown.y)
        val laenge = getSchlagpunkt(x, y) - start
        //note kriegt den String zugewiesen auf den geklickt wurde
        val note = getNote(y, start)


        hintTon = Ton(note, start, laenge)
        if (!isValid(hintTon)) hintTon = Ton(note, start, 0)

        //verschiebung von Toenen
      } else if (bewegterTonIndex != -1)
      {

        val bewegterTon = Ton(aktuelleStimme(bewegterTonIndex).hoehe, aktuelleStimme(bewegterTonIndex).start, aktuelleStimme(bewegterTonIndex).laenge, aktuelleStimme(bewegterTonIndex).text)
        aktuelleStimme(bewegterTonIndex) = Ton(getNote(y, bewegterTon.start), bewegterTon.start, bewegterTon.laenge, bewegterTon.text)
        selection = aktuelleStimme(bewegterTonIndex)
      } else if (bewegterTonIndexHorizontale != -1)
      {
        val schlagpunkt = getSchlagpunkt(x, y)
        val diff = alterStart - schlagpunkt
        aktuelleStimme(bewegterTonIndexHorizontale) = Ton(aktuelleStimme(bewegterTonIndexHorizontale).hoehe, schlagpunkt, laengeTon + diff, aktuelleStimme(bewegterTonIndexHorizontale).text)
        aktuelleStimme(bewegterTonIndexHorizontale - 1) = Ton(aktuelleStimme(bewegterTonIndexHorizontale - 1).hoehe, aktuelleStimme(bewegterTonIndexHorizontale - 1).start, laengeVorgaengerTon - diff, aktuelleStimme(bewegterTonIndexHorizontale - 1).text)

      }
    }

    def isValid(ton: Ton): Boolean =
    {
      if (ton.laenge <= 0 && !ton.hoehe.equals("TesteObLastDownGueltigSeinKann")) return false
      if (aktuelleStimme.length == 0) return true

      def rek(i: Int): Boolean =
      {
        if (i == aktuelleStimme.length)
        {

          if (aktuelleStimme(aktuelleStimme.length - 1).start + aktuelleStimme(aktuelleStimme.length - 1).laenge > ton.start) return false
          return true;
        }
        if (ton.start == aktuelleStimme(i).start)
        {
          return false
        }
        else if (ton.start < aktuelleStimme(i).start)
        {
          if (i > 0 && aktuelleStimme(i - 1).start + aktuelleStimme(i - 1).laenge > ton.start) return false
          if (ton.start + ton.laenge > aktuelleStimme(i).start) return false
          return true
        } else
        {
          rek(i + 1)
        }

      }


      rek(0)


    }

    val stimmenSpinner = dom.document.getElementById("stimme").asInstanceOf[html.Select]
    stimmenSpinner.onmouseup = (e: dom.MouseEvent) =>
    {
      aktuelleStimme = stueck(stimmenSpinner.value.toInt - 1)
      lastDown = Pointt(-1, -1)

    }
    textFeld.onkeypress = (e: dom.KeyboardEvent) =>
    {

      //Tab
      if (e.keyCode == 9)
      {
        e.preventDefault()
        if (!e.shiftKey)
        {
          if (aktuelleStimme.length > textFeldIndex + 1)
          {
            textFeldIndex = textFeldIndex + 1
          }
        } else
        {
          if (textFeldIndex > 0)
          {
            textFeldIndex = textFeldIndex - 1
          }
        }
        textFeld.value = aktuelleStimme(textFeldIndex).text
      }


    }
    textFeld.onkeyup = (e: dom.KeyboardEvent) =>
    {

      if (aktuelleStimme.length > 0)
      {
        aktuelleStimme(textFeldIndex) = Ton(aktuelleStimme(textFeldIndex).hoehe, aktuelleStimme(textFeldIndex).start, aktuelleStimme(textFeldIndex).laenge, textFeld.value)
      }
    }
    textFeld.onmousedown = (e: dom.MouseEvent) =>
    {
      if (textFeldIndex == -1)
      {
        textFeldIndex = 0
      }
      if (aktuelleStimme.length > textFeldIndex)
      {
        textFeld.value = aktuelleStimme(textFeldIndex).text
      }
    }
    taktFeld.onkeyup = (e: dom.KeyboardEvent) =>
    {
      try
      {
        takt = taktFeld.value.toInt
        onResize
      } catch
      {
        case ex: NumberFormatException =>
      }
    }

    zeilenFeld.onkeyup = (e: dom.KeyboardEvent) =>
    {
      try
      {
        zeilenAnzahl = zeilenFeld.value.toInt
        onResize
      } catch
      {
        case ex: NumberFormatException =>
      }
    }

    dom.window.addEventListener("resize", (e: dom.UIEvent) =>
    {
      onResize()
    })

    speichernButton.onmousedown = (e: dom.MouseEvent) =>
    {
      //  dom.console.log("in eventhandling")
      ImportExport.export(stueck, "Test123")


    }

    titelFeld.onkeydown = (e:dom.KeyboardEvent) =>
      {
        titel = titelFeld.value
      }


  }

  def zeichne() =
  {

    var lastPoint = Pointt(0, 0)
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    if(!canvas.classList.contains("dragOver"))
    {


      zeichneHinterGrund()
      val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge

      ctx.lineWidth = 2

      for (stimmenIndex <- 0 to stueck.length - 1)
      {
        val stimme = stueck(stimmenIndex)
        ctx.strokeStyle = stimmenFarben(stimmenIndex)

        for (i <- 0 to stimme.length - 1)
        {

          ctx.lineWidth = 2
          val start = stimme(i).start
          val laenge = stimme(i).laenge


          if (existiertDirekterVorgaengerTon(stimme(i), stimme))
          {
            ctx.beginPath
            ctx.moveTo(lastPoint.x, lastPoint.y)
            ctx.lineTo(lastPoint.x, getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start))
            ctx.stroke
          }

          //zeichne Gesang
          var gesangSchonGezeichnet = false
          ctx.font = if (i == textFeldIndex) fontBoldText else fontText
          if (i > 0)
          {
            val hoeheVorgaenger = if (tones.indexOf(stimme(i - 1).hoehe) != -1) tones.indexOf(stimme(i - 1).hoehe) else stimme(i - 1).hoehe.toInt
            val hoeheVonAktuellem = if (tones.indexOf(stimme(i).hoehe) != -1) tones.indexOf(stimme(i).hoehe) else stimme(i).hoehe.toInt
            if (hoeheVorgaenger < hoeheVonAktuellem)
            {
              gesangSchonGezeichnet = true
              ctx.fillText(stimme(i).text, getXKoordinateZumZeichnenAusTon(stimme(i)), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + 1.4 * fontHeightText)
            }

          }
          if (!gesangSchonGezeichnet)
          {
            ctx.fillText(stimme(i).text, getXKoordinateZumZeichnenAusTon(stimme(i)), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) - 0.7 * fontHeightText)
          }

          if (stimme(i).equals(selection)) ctx.lineWidth = 3 else ctx.lineWidth = 2
          ctx.beginPath
          ctx.moveTo(getXKoordinateZumZeichnenAusTon(stimme(i)), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start))


          //zeile ist immer die relative zeile zum startschlag
          def rek(zeile: Int): Unit =
          {
            //  println(start + laenge - (getZeile(start) + zeile) * schlaegeProZeile + " <= schlaegeProZeile: " + schlaegeProZeile)

            if (start + laenge - (getZeile(start) + zeile) * schlaegeProZeile <= schlaegeProZeile)
            {
              var restSchlaege = start % schlaegeProZeile
              if (restSchlaege == 0) restSchlaege = schlaegeProZeile
              lastPoint = Pointt(getXKoordinateZumZeichnen(restSchlaege + laenge - zeile * schlaegeProZeile), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + (zeile * zeilenHoehe))
              ctx.lineTo(lastPoint.x, lastPoint.y)
              ctx.stroke
            }
            else
            {
              ctx.lineTo(getXKoordinateZumZeichnen(schlaegeProZeile + 1), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + (zeile * zeilenHoehe))
              ctx.stroke
              ctx.beginPath
              ctx.moveTo(getXKoordinateZumZeichnen(1), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + ((zeile + 1) * zeilenHoehe))
              rek(zeile + 1)
            }
          }

          rek(0)

        }
      }
      zeichneHint
    }

  }

  def zeichneHinterGrund() =
  {
    ctx.strokeStyle = farbeHintergrund
    ctx.font = font
    ctx.fillStyle = fontStyle

    for (zeile <- 0 to zeilenAnzahl - 1)
    {
      for (i <- 0 to tones.length - 1)
      {


        ctx.lineWidth = 1
        ctx.fillText(tones(i), randSeite - 30, randOben + i * abstandToene + zeile * zeilenHoehe + fontHeight / 2)


        ctx.beginPath()
        ctx.moveTo(randSeite, randOben + i * abstandToene + zeile * zeilenHoehe)
        ctx.lineTo(randSeite + laengeHorizontalLinie, randOben + i * abstandToene + zeile * zeilenHoehe)
        ctx.stroke()
      }
      for (i <- 0 to (laengeHorizontalLinie - emptySpace) / intervallViertel)
      {

        ctx.beginPath()
        if (i % takt == 0) ctx.lineWidth = 2 else ctx.lineWidth = 1
        ctx.moveTo(randSeite + laengeHorizontalLinie - i * intervallViertel, randOben + zeile * zeilenHoehe)
        ctx.lineTo(randSeite + laengeHorizontalLinie - i * intervallViertel, randOben + zeile * zeilenHoehe + abstandToene * (tones.length - 1))
        ctx.stroke()
      }
    }

  }

  def zeichneHint(): Unit =
  {
    val reverse = aktuelleStimme.reverse
    ctx.strokeStyle = farbeHint;
    if (lastDown.x != -1 && hintTon.laenge > 0)
    {
      val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge
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

      if (aktuelleStimme.length > 0)
      {
        if (hintTon.start < aktuelleStimme(0).start)
        {

          if (hintTon.start + hintTon.laenge == aktuelleStimme(0).start)
          {
            var moduloSchlaege = (start + laenge) % schlaegeProZeile
            if (moduloSchlaege == 0) moduloSchlaege = schlaegeProZeile
            ctx.beginPath
            ctx.moveTo(getXKoordinateZumZeichnen(moduloSchlaege), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(hintTon.start + hintTon.laenge))
            ctx.lineTo(getXKoordinateZumZeichnen(moduloSchlaege), getYKoordinateZumZeichnenAusTon(aktuelleStimme(0)) + zeilenAbstand(aktuelleStimme(0).start))
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
                var moduloSchlaege = (start) % schlaegeProZeile
                if (moduloSchlaege == 0) moduloSchlaege = schlaegeProZeile
                ctx.beginPath
                ctx.moveTo(getXKoordinateZumZeichnen(moduloSchlaege), getYKoordinateZumZeichnenAusTon(ton) + zeilenAbstand(ton.start + ton.laenge))
                ctx.lineTo(getXKoordinateZumZeichnen(moduloSchlaege), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(hintTon.start))
                ctx.stroke
              }
              if (i > 0 && hintTon.start + hintTon.laenge == reverse(i - 1).start)
              {
                var moduloSchlaege = (start + laenge) % schlaegeProZeile
                if (moduloSchlaege == 0) moduloSchlaege = schlaegeProZeile
                ctx.beginPath
                ctx.moveTo(getXKoordinateZumZeichnen(moduloSchlaege), getYKoordinateZumZeichnenAusTon(hintTon) + zeilenAbstand(hintTon.start + hintTon.laenge))
                ctx.lineTo(getXKoordinateZumZeichnen(moduloSchlaege), getYKoordinateZumZeichnenAusTon(reverse(i - 1)) + zeilenAbstand(reverse(i - 1).start))
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
    val rest = abStartPoint % aktuelleNotenLaenge;
    if (rest < aktuelleNotenLaenge / 2) temp - rest else x + aktuelleNotenLaenge - rest
  }

  def getNaechstesY(y: Int): Int =
  {

    val rest = y % abstandToene
    //println("voll: " +(y-randOben))
    // println("rest: " + rest)
    if (rest < abstandToene / 2) y - rest else y + abstandToene - rest

  }

  def getSchlagpunkt(x: Int, y: Int): Double =
  {
    //rule
    // 1. wenn sich der schlag auf dem grid befindet, nimm diese Zeile
    // 2. zeile vom chronologisch vorherigem, bei 0 Elementen 1 Zeile
    // 3. falls sich der klick zu tief ist, immer die Zeile nehmen, bei der es ein tiefer ton ist

    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge
    var zeile: Int = (y - randOben) / zeilenHoehe
    val zeilenRest = (y - randOben) % zeilenHoehe

    //klick war nicht auf dem Grid
    if (zeilenRest > (zeilenHoehe - zeilenAbstand))
    {
      if (aktuelleStimme.length > 0)
      {
        //nur der pseudovorgaenger, geht davon aus dass nur kontinuierlich geklickt wurde
        // wenn hier ein zwischenschritt hinzugefügt wird funktioniert das hier noch nicht
        val endeVonVorgaenger = aktuelleStimme.last.start + aktuelleStimme.last.laenge
        val zeileVonEndeVonVorgaenger = ((endeVonVorgaenger - 1) / schlaegeProZeile).toInt
        if (zeileVonEndeVonVorgaenger == zeile + 1)
        {
          zeile = zeile + 1
        }
        else if (zeileVonEndeVonVorgaenger == zeile + 2 && endeVonVorgaenger % schlaegeProZeile == 1)
        {
          zeile = zeile + 1
        }
      }

    }

    //ab dem ersten Schlag
    //  println("zeile: " + zeile)
    // println("schlagpunkt: " + ((getNaechstesX(x) - randSeite - emptySpace) / intervall + 1))

    val schlagPunkt = (getNaechstesX(x) - randSeite - emptySpace) / aktuelleNotenLaenge + 1 + zeile * schlaegeProZeile
    schlagPunkt
  }

  def getNote(y: Int, schlag: Double): String =
  {
    //anzahl der toene ab dem höchsten möglichen Ton pro Zeile
    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge
    val imVerhaeltnisZuErstemTonImSchlag = ((getNaechstesY(y)) - randOben) - (((schlag - 1) / schlaegeProZeile).toInt * zeilenHoehe)

    //val temp = ((getNaechstesY(y) - randOben) / abstandToene) % (zeilenHoehe / abstandToene)
    val temp = (imVerhaeltnisZuErstemTonImSchlag / abstandToene).toInt % (zeilenHoehe / abstandToene)

    //wenn temp im bereich von tones liegt, kann direkt der ton String gespeichert werden
    if (temp >= 0 && temp < tones.length) tones(temp) else temp.toString


  }

  def getYCoordinateFromCanvas(y: Int): Int =
  {
    y - canvas.getBoundingClientRect().top.toInt
  }

  def getXCoordinateFromCanvas(x: Int): Int =
  {
    x - canvas.getBoundingClientRect().left.toInt
  }

  def getXKoordinateZumZeichnenAusTon(ton: Ton): Int =
  {
    val schlaegeProZeile = ((laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge)
    val moduloSchlag = ton.start % schlaegeProZeile
    getXKoordinateZumZeichnen(if (moduloSchlag == 0) schlaegeProZeile else moduloSchlag)

  }

  def getYKoordinateZumZeichnenAusTon(ton: Ton): Int =
  {

    var index = tones.indexOf(ton.hoehe)
    if (index == -1) index = ton.hoehe.toInt
    randOben + abstandToene * index

  }

  def getXKoordinateZumZeichnen(x: Double): Int =
  {

    (randSeite + emptySpace + (x - 1) * aktuelleNotenLaenge).toInt

  }

  //gibt den abstand wieder, der auf die y  koordinate raufgerchnet werden muss,
  // je nach dem in welcher zeile man ist

  def zeilenAbstand(schlag: Double): Int =
  {
    getZeile(schlag) * zeilenHoehe

  }

  def getZeile(schlag: Double): Int =
  {
    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge
    ((schlag - 1) / schlaegeProZeile).toInt
  }

  def existiertDirekterVorgaengerTon(ton: Ton, stimme: ArrayBuffer[Ton]): Boolean =
  {
    def rek(i: Int): Boolean =
    {
      if (i == stimme.length)
      {
        false
      } else
      {
        if (ton.start == stimme(i).start + stimme(i).laenge) true else rek(i + 1)
      }
    }

    rek(0)

  }
  def setStueck(neuesStueck : Array[ArrayBuffer[Ton]]): Unit =
  {
    stueck = neuesStueck
  }

}

