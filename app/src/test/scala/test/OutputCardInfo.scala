package test

import me.herbix.ts.util.Lang

/**
  * Created by Chaofan on 2016/9/6.
  */
object OutputCardInfo {
  def main(args: Array[String]) {
    var i = 0
    for ((name, desc) <- Lang.cardInfo) {
      println(s"[$i]")
      println(name)
      println(desc)
      i += 1
    }
  }
}
