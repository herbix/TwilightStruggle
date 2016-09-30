package me.herbix.ts.agent

import me.herbix.ts.logic.Operation
import me.herbix.ts.util.OperationHint

/**
  * Created by Chaofan on 2016/9/27.
  */
trait AgentTrait {
  def pickOperation(hint: OperationHint): Operation
}
