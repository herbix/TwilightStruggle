// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic

/**
  * Created by Chaofan on 2016/7/5.
  */
trait GameTrait {
  def nextState(input: Operation): Unit
  def rollBackBeforeHistory(historyId: Int): Unit
}
