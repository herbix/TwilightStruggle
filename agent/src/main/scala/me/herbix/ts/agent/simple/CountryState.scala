package me.herbix.ts.agent.simple

/**
  * Created by Chaofan on 2016/9/30.
  */
class CountryState(val importance: Float = 0,
                   val threat: Float = 0) {

  override def toString = f"$threat%.1f:$importance%.1f"

}
