package Zemp

import scala.scalajs.js
import scala.scalajs.js.annotation._


  //def addImage(imgData: String, format: String, x : Int, y: Int,width: Int, height: Int, alias :String ):Unit = js.native


@js.native
@JSImport("jspdf",JSImport.Default)
object jsPdf extends js.Object
{


}




@js.native
@JSImport("jspdf", JSImport.Default)
class jsPdf(orientation : String = "p",unit : String = "mm", format : String = "a4") extends js.Object
{

  def internal : Internal = js.native

  def save(filename : String) : jsPdf = js.native

  def line(x1 : Int, y1 : Int, x2 : Int, y2 : Int) : jsPdf = js.native

  def setLineWidth(width : Int) : jsPdf = js.native

  def setDrawColor(ch1 : Double , ch2: Double, ch3 : Double, ch4 : Double): jsPdf = js.native

  def setDrawColor(ch1 : String) : jsPdf = js.native

  def text(text : String, x : Double, y: Double) : jsPdf = js.native

  def getStringUnitWidth(text : String) : Double = js.native

  def setFontSize(size : Double) : jsPdf = js.native


}

trait Internal extends js.Object
{
  def getFontSize() : Double

  def pageSize : PageSize

  def scaleFactor : Double
}

trait PageSize extends js.Object
{
  def getWidth() : Double

  def getHeight() : Double

  def width : Double

}