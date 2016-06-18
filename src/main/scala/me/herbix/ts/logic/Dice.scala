package me.herbix.ts.logic

import java.util.Random

/**
  * Created by Chaofan on 2016/6/12.
  */
class Dice extends Random {
  def roll() = nextInt(6) + 1
}
