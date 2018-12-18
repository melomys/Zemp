package Zemp

import Zemp.Project.{farbeHintergrund, stimmenFarben, takt, titel, tones, zeilenAnzahl}
import org.scalajs.dom
import org.scalajs.dom.html

import scala.collection.mutable.ArrayBuffer


object ImportExport
{
  var faktor = 0.0

  //pdf darstellung ist um paar pixel verschoben
  val pdfOffSet = 1

  var randSeite = 0
  var randOben = 0
  var intervallViertel = 0
  var zeilenHoehe = 0
  var abstandToene = 0
  var laengeHorizontalLinie = 0
  var emptySpace = 0
  var aktuelleNotenLaenge = 0

  def pdfExport(stueck: Array[ArrayBuffer[Ton]]): Unit =
  {
    var doc = new jsPdf("landscape", "pt")
    faktor = doc.internal.pageSize.getWidth() / Project.canvas.width

    randSeite = math.round(Project.randSeite * faktor).toInt
    randOben = math.round(Project.randOben * faktor).toInt
    intervallViertel = math.round(Project.intervallViertel * faktor).toInt
    zeilenHoehe = math.round(Project.zeilenHoehe * faktor).toInt
    abstandToene = math.round(Project.abstandToene * faktor).toInt
    laengeHorizontalLinie = math.round(Project.laengeHorizontalLinie * faktor).toInt
    emptySpace = math.round(Project.emptySpace * faktor).toInt
    aktuelleNotenLaenge = math.round(Project.aktuelleNotenLaenge * faktor).toInt

    // Zeichnung und Zentrierung vom Titel
    val fontSize = doc.internal.getFontSize()

    val pageWidth = doc.internal.pageSize.getWidth()
    val scaleFactor = doc.internal.scaleFactor
    val titelWidth = doc.getStringUnitWidth(titel) * fontSize / scaleFactor

    val xCoord = (pageWidth - titelWidth) / 2
    val yCoord = 2.5 * fontSize / scaleFactor

    doc.text(Project.titelFeld.value, xCoord, yCoord)


    //zeichnen vom Hintergrund

    doc.setDrawColor(farbeHintergrund)

    // ctx.font = font
    //ctx.fillStyle = fontStyle

    for (page <- 1 to ((zeilenAnzahl + 1)) / 2)
    {

      if (page > 1)
      {
        doc.addPage("a4", "l")
      }
      for (zeile <- 0 to math.min(zeilenAnzahl - (page - 1) * 2 - 1, 1))
      {
        for (i <- 0 to tones.length - 1)
        {
          doc.setDrawColor(farbeHintergrund)
          doc.setLineWidth(1)
          doc.text(tones(i), randSeite - 30, randOben + i * abstandToene + zeile * zeilenHoehe + fontSize / 2)


          doc.line(randSeite, randOben + i * abstandToene + zeile * zeilenHoehe, randSeite + laengeHorizontalLinie, randOben + i * abstandToene + zeile * zeilenHoehe)

        }
        for (i <- 0 to (laengeHorizontalLinie.toInt - emptySpace.toInt) / intervallViertel.toInt)
        {

          if (i % takt == 0) doc.setLineWidth(2) else doc.setLineWidth(1)
          doc.line(randSeite + laengeHorizontalLinie - i * intervallViertel, randOben + zeile * zeilenHoehe, randSeite + laengeHorizontalLinie - i * intervallViertel, randOben + zeile * zeilenHoehe + abstandToene * (tones.length - 1))
        }

        //Fuss Zeichnen
        doc.setFontSize(fontSize - 3)

        val fussText = Project.titelFeld.value + " - " + page
        val fussWidth = doc.getStringUnitWidth(fussText) * doc.internal.getFontSize() / scaleFactor
        val xCoord = randSeite + laengeHorizontalLinie - fussWidth
        val yCoord = doc.internal.pageSize.getHeight() - 2 * doc.internal.getFontSize()

        doc.text(fussText, xCoord, yCoord)


        doc.setFontSize(fontSize)
      }


    }

    //zeichnen von den Stimmen
    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge

    var lastPoint = Pointt(0, 0)
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

          // doc.setPage(getZeile(lastPoint.x)/2 + 1)
          doc.setDrawColor(stimmenFarben(stimmenIndex))
          doc.line(lastPoint.x, lastPoint.y, lastPoint.x, getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start))
        }

        //zeichne Gesang

        doc.setPage(getZeile(stimme(i).start) / 2 + 1)
        doc.setDrawColor(stimmenFarben(stimmenIndex))
        var gesangSchonGezeichnet = false
        if (i > 0)
        {
          val hoeheVorgaenger = if (tones.indexOf(stimme(i - 1).hoehe) != -1) tones.indexOf(stimme(i - 1).hoehe) else stimme(i - 1).hoehe.toInt
          val hoeheVonAktuellem = if (tones.indexOf(stimme(i).hoehe) != -1) tones.indexOf(stimme(i).hoehe) else stimme(i).hoehe.toInt
          if (hoeheVorgaenger < hoeheVonAktuellem)
          {
            gesangSchonGezeichnet = true
            doc.text(stimme(i).text, getXKoordinateZumZeichnenAusTon(stimme(i)), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + 1.4 * fontSize)
          }

        }
        if (!gesangSchonGezeichnet)
        {
          doc.text(stimme(i).text, getXKoordinateZumZeichnenAusTon(stimme(i)), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) - 0.7 * fontSize)
        }

        var aktuellerStartPunkt = Pointt(getXKoordinateZumZeichnenAusTon(stimme(i)), math.round(getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start)).toInt)

        //zeile ist immer die relative zeile zum startschlag
        def rek(zeile: Int): Unit =
        {

          if (start + laenge - (Project.getZeile(start) + zeile) * schlaegeProZeile <= schlaegeProZeile)
          {
            var restSchlaege = start % schlaegeProZeile
            if (restSchlaege == 0) restSchlaege = schlaegeProZeile
            if ((getZeile(start) + zeile) % 2 != 0)
            {
              lastPoint = Pointt(getXKoordinateZumZeichnen(restSchlaege + laenge - zeile * schlaegeProZeile), math.round((getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + (zeile * zeilenHoehe))).toInt)
            }
            else
            {
              lastPoint = Pointt(getXKoordinateZumZeichnen(restSchlaege + laenge - zeile * schlaegeProZeile), math.round((getYKoordinateZumZeichnenAusTon(stimme(i)))))
            }
            doc.setPage((getZeile(start) + zeile) / 2 + 1)
            doc.line(aktuellerStartPunkt.x, aktuellerStartPunkt.y, lastPoint.x, lastPoint.y)
          }
          else
          {
            doc.setPage((getZeile(start) + zeile) / 2 + 1)

            doc.line(aktuellerStartPunkt.x, aktuellerStartPunkt.y, getXKoordinateZumZeichnen(schlaegeProZeile + 1), getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + (zeile * zeilenHoehe))

            if ((getZeile(start) + zeile + 1) % 2 != 0)
            {
              aktuellerStartPunkt = Pointt(getXKoordinateZumZeichnen(1), math.round((getYKoordinateZumZeichnenAusTon(stimme(i)) + zeilenAbstand(start) + (((zeile + 1)) * zeilenHoehe)) % doc.internal.pageSize.getHeight()).toInt)
            }
            else
            {
              aktuellerStartPunkt = Pointt(getXKoordinateZumZeichnen(1), (getYKoordinateZumZeichnenAusTon(stimme(i))))
            }
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


    dom.document.ondragover = (e: dom.DragEvent) =>
    {
      e.preventDefault()
      e.stopPropagation()
      Project.canvas.classList.add("dragOver")
    }


    dom.document.ondragleave = (e: dom.DragEvent) =>
    {

      e.preventDefault()
      e.stopPropagation()
      Project.canvas.classList.remove("dragOver")
    }

    dom.document.ondragend = (e: dom.DragEvent) =>
    {
      e.preventDefault()
      e.stopPropagation()
      Project.canvas.classList.remove("dragOver")
    }

    dom.document.ondrop = (e: dom.DragEvent) =>
    {
      e.preventDefault()
      e.stopPropagation()
      Project.canvas.classList.remove("dragOver")
      var file = e.dataTransfer.files(0)
      dom.console.log(file.name)
      dom.console.log(file)
      var reader = new dom.FileReader
      var text = reader.readAsText(file)
      reader.onload = (e: dom.Event) =>
      {
        try
        {
          var neuesStueck = Array(new ArrayBuffer[Ton], new ArrayBuffer[Ton], new ArrayBuffer[Ton], new ArrayBuffer[Ton], new ArrayBuffer[Ton], new ArrayBuffer[Ton])

          val tmpStueck = importStueck(reader.result.toString)

          var index = 0;
          for (stimme <- tmpStueck)
          {
            neuesStueck(index) = stimme;
            index = index + 1
          }
          Project.setStueck(neuesStueck)
        } catch
        {
          case e: Exception =>
        }

      }
    }

    val speichernButton = dom.document.getElementById("speichern").asInstanceOf[html.Button]
    speichernButton.onmousedown = (e: dom.MouseEvent) =>
    {
      //  dom.console.log("in eventhandling")
      ImportExport.export(Project.stueck)


    }


  }


  def export(stueck: Array[ArrayBuffer[Ton]]): Unit =
  {


    var result = "";
    for (i <- 0 to stueck.length - 1)
    {
      var stimme = stueck(i)
      println(stimme)
      if (stimme.length == 0) result = result + "-"
      for (ton <- stimme)
      {
        result = result + ton.hoehe + "," + ton.start + "," + ton.laenge + "," + ton.text
        if (!ton.equals(stimme(stimme.length - 1))) result = result + ";"
      }
      if (i != stueck.length - 1) result = result + "#"
    }


    var a: scalajs.js.Array[scalajs.js.Any] = scalajs.js.Array(result)
    var file = new dom.Blob(a)


    var el = dom.document.getElementById("export").asInstanceOf[html.Anchor]
    var url = dom.URL.createObjectURL(file)
    el.setAttribute("href", url)
    el.setAttribute("download", titel + ".zemp")
    el.click
    // var event = new dom.MouseEvent()
    // el.dispatchEvent(event)


  }

  def importStueck(string: String): Array[ArrayBuffer[Ton]] =
  {
    var stimmen = string.split("#")
    var stueck = new Array[ArrayBuffer[Ton]](stimmen.length)
    var indexStimme = 0
    for (stimme <- stimmen)
    {
      stueck(indexStimme) = new ArrayBuffer[Ton]
      if (!stimme.equals("-"))
      {
        var toene = stimme.split(";")
        for (ton <- toene)
        {
          var teile = ton.split(",")
          var neuerTon = Ton(teile(0), teile(1).toDouble, teile(2).toDouble, teile(3))
          stueck(indexStimme).append(neuerTon)
        }
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

    pdfOffSet + (randSeite + emptySpace + (x - 1) * aktuelleNotenLaenge).toInt

  }

  //gibt den abstand wieder, der auf die y  koordinate raufgerchnet werden muss,
  // je nach dem in welcher zeile man ist

  def zeilenAbstand(schlag: Double): Int =
  {
    getZeile(schlag) % 2 * zeilenHoehe

  }

  def getZeile(schlag: Double): Int =
  {
    val schlaegeProZeile = (laengeHorizontalLinie - emptySpace) / aktuelleNotenLaenge

    println("in getZeile: " + ((schlag - 1) / schlaegeProZeile).toInt)
    ((schlag - 1) / schlaegeProZeile).toInt
  }

}
