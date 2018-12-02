package jspdf

import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSImport("jspdf", "jsPdf")
class jsPdf() extends js.Object
{

  def addImage(imgData: String, format: String, x : Int, y: Int,width: Int, height: Int, alias :String ):Unit = js.native

}
