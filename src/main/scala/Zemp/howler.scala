package Zemp

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport


@js.native
@JSImport("howler",JSImport.Default)
object howler extends js.Object
{
  def mute(muted : Boolean) : Unit = js.native
}


@js.native
@JSImport("howler", JSImport.Default)
class Howl(src : Array[String],volume :Double = 1,html5 : Boolean = false, loop : Boolean = false, preload : Boolean = true, autoplay : Boolean = false, mute : Boolean = false , sprite : js.Object = null, rate : Double = 1, pool : Double = 5,format: Array[js.Object] = Array(),xhrWithCredentials : Boolean = false) extends js.Object
{
  def load() : Unit = js.native
}
