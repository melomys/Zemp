package Zemp

import Zemp.Project.{farbeHintergrund, takt,font, fontStyle,titel, tones, zeilenAnzahl,stimmenFarben}

import scala.collection.mutable.ArrayBuffer
import org.scalajs.dom
import org.scalajs.dom.html


object ImportExport
{
   var faktor = 0.0

  //pdf darstellung ist um paar pixel verschoben
  val pdfOffSet = 1

  var randSeite = 0
  var randOben  = 0
      var intervallViertel =0
      var zeilenHoehe = 0
      var abstandToene = 0
      var laengeHorizontalLinie= 0
      var emptySpace = 0
      var aktuelleNotenLaenge = 0

  def pdfExport(stueck: Array[ArrayBuffer[Ton]]): Unit =
    {
      var doc = new jsPdf("landscape","pt")
      faktor = doc.internal.pageSize.getWidth()/Project.canvas.width

      randSeite = math.round(Project.randSeite*faktor).toInt
      randOben  = math.round(Project.randOben*faktor).toInt
      intervallViertel = math.round(Project.intervallViertel*faktor).toInt
      zeilenHoehe = math.round(Project.zeilenHoehe*faktor).toInt
      abstandToene = math.round(Project.abstandToene*faktor).toInt
      laengeHorizontalLinie= math.round(Project.laengeHorizontalLinie*faktor).toInt
      emptySpace = math.round(Project.emptySpace*faktor).toInt
      aktuelleNotenLaenge = math.round(Project.aktuelleNotenLaenge*faktor).toInt

      //Zentrierung vom Titel
      val fontSize = doc.internal.getFontSize()
      val pageWidth = doc.internal.pageSize.getWidth()
      val scaleFactor = doc.internal.scaleFactor
      val titelWidth = doc.getStringUnitWidth(titel) * fontSize / scaleFactor

      val xCoord = (pageWidth - titelWidth) / 2
      val yCoord = 2.5 * fontSize / scaleFactor

      doc.text(titel, xCoord, yCoord)


      //zeichnen vom Hintergrund

      doc.setDrawColor(farbeHintergrund)

     // ctx.font = font
      //ctx.fillStyle = fontStyle

      for (zeile <- 0 to zeilenAnzahl - 1)
      {
        for (i <- 0 to tones.length - 1)
        {
          doc.setLineWidth(1)
          doc.text(tones(i), randSeite - 30, randOben + i * abstandToene + zeile * zeilenHoehe + fontSize / 2)


          doc.line(randSeite,randOben+i*abstandToene+zeile*zeilenHoehe,randSeite+laengeHorizontalLinie,randOben+i*abstandToene+zeile*zeilenHoehe)

        }
        for (i <- 0 to (laengeHorizontalLinie.toInt - emptySpace.toInt) / intervallViertel.toInt)
        {

          if (i % takt == 0) doc.setLineWidth(2) else doc.setLineWidth(1)
          doc.line(randSeite + laengeHorizontalLinie - i * intervallViertel, randOben + zeile * zeilenHoehe,randSeite + laengeHorizontalLinie - i * intervallViertel, randOben + zeile * zeilenHoehe + abstandToene * (tones.length - 1))
        }
      }


      //zeichnen von den Stimmen
      val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge

      var lastPoint = Pointt(0,0)
    for (stimmenIndex <- 0 to stueck.length - 1)
    {

      val stimme = stueck(stimmenIndex)

      doc.setDrawColor(stimmenFarben(stimmenIndex))
      doc.setTextColor(stimmenFarben(stimmenIndex))

      for (i <- 0 to stimme.length - 1)
      {

        doc.setLineWidth(2)
        val start = stimme(i).start
        val laenge = stimme(i).laenge


        if (Project.existiertDirekterVorgaengerTon(stimme(i), stimme))
        {
          doc.line(lastPoint.x, lastPoint.y,lastPoint.x,  getYKoordinateZumZeichnenAusTon(stimme(i)) +  zeilenAbstand(start))
        }

        //zeichne Gesang
        var gesangSchonGezeichnet = false
        if (i > 0)
        {
          val hoeheVorgaenger = if (tones.indexOf(stimme(i - 1).hoehe) != -1) tones.indexOf(stimme(i - 1).hoehe) else stimme(i - 1).hoehe.toInt
          val hoeheVonAktuellem = if (tones.indexOf(stimme(i).hoehe) != -1) tones.indexOf(stimme(i).hoehe) else stimme(i).hoehe.toInt
          if (hoeheVorgaenger < hoeheVonAktuellem)
          {
            gesangSchonGezeichnet = true
            doc.text(stimme(i).text,  getXKoordinateZumZeichnenAusTon(stimme(i)),  getYKoordinateZumZeichnenAusTon(stimme(i)) +  zeilenAbstand(start) + 1.4 * fontSize)
          }

        }
        if (!gesangSchonGezeichnet)
        {
          doc.text(stimme(i).text,  getXKoordinateZumZeichnenAusTon(stimme(i)),  getYKoordinateZumZeichnenAusTon(stimme(i)) +  zeilenAbstand(start) - 0.7 * fontSize)
        }

        var aktuellerStartPunkt = Pointt( getXKoordinateZumZeichnenAusTon(stimme(i)),  getYKoordinateZumZeichnenAusTon(stimme(i)) +  zeilenAbstand(start))

        //zeile ist immer die relative zeile zum startschlag
        def rek(zeile: Int): Unit =
        {

          if (start + laenge - (Project.getZeile(start) + zeile) * schlaegeProZeile <= schlaegeProZeile)
          {
            var restSchlaege = start % schlaegeProZeile
            if (restSchlaege == 0) restSchlaege = schlaegeProZeile
            lastPoint = Pointt( getXKoordinateZumZeichnen(restSchlaege + laenge - zeile * schlaegeProZeile),  getYKoordinateZumZeichnenAusTon(stimme(i)) +  zeilenAbstand(start) + (zeile * zeilenHoehe))
            doc.line(aktuellerStartPunkt.x,aktuellerStartPunkt.y,lastPoint.x,lastPoint.y)
          }
          else
          {
            doc.line(aktuellerStartPunkt.x,aktuellerStartPunkt.y, getXKoordinateZumZeichnen(schlaegeProZeile + 1),  getYKoordinateZumZeichnenAusTon(stimme(i)) +  zeilenAbstand(start) + (zeile * zeilenHoehe))
            aktuellerStartPunkt = Pointt( getXKoordinateZumZeichnen(1),  getYKoordinateZumZeichnenAusTon(stimme(i)) +  zeilenAbstand(start) + ((zeile + 1) * zeilenHoehe))
            rek(zeile + 1)
          }
        }

        rek(0)

      }
    }


     doc.save(Project.titelFeld.value + ".pdf")

    }


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
    var stimmen = string.split("#")
    var stueck = new Array[ArrayBuffer[Ton]](stimmen.length)
    var indexStimme = 0
    for(stimme <- stimmen)
      {
        stueck(indexStimme) = new ArrayBuffer[Ton]
        var toene = stimme.split(";")
        for(ton <- toene)
          {
            var teile = ton.split(",")
            var neuerTon = Ton(teile(0),teile(1).toDouble,teile(2).toDouble,teile(3))
            stueck(indexStimme).append(neuerTon)
          }
        indexStimme = indexStimme + 1
      }
    return stueck
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

    pdfOffSet+ (randSeite + emptySpace + (x - 1) * aktuelleNotenLaenge).toInt

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

}
