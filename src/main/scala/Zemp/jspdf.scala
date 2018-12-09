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
  def save(filename : String) : jsPdf = js.native
}