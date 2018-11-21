package Zemp

import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSImport("paper", JSImport.Default)
object paper extends js.Object
{
  def setup(canvas: html.Canvas): Unit = js.native

  def view: View = js.native

}


trait View extends js.Object
{

  def draw(): Unit
}

@js.native
@JSImport("paper", "Point")
class Point(x: Double, y: Double) extends js.Object
{

  def add(pointArray: Array[Int]): Point = js.native
}

@js.native
@JSImport("paper", "Path")
class Path() extends js.Object
{
  var strokeColor: String = js.native

  def moveTo(point: Point): Unit = js.native

  def lineTo(point: Point): Unit = js.native
}

