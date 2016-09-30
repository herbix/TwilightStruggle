package me.herbix.ts.agent.simple

import me.herbix.ts.logic.Country
import me.herbix.ts.util.InfluenceProvider

/**
  * Created by Chaofan on 2016/9/30.
  */
class CountryState(agent: SimpleAgent,
                   influenceProvider: InfluenceProvider,
                   val country: Country,
                   val importance: Float = 0,
                   val threat: Float = 0,
                   val gain: Float = 0) {
  
  val faction = agent.agentFaction
  val opponentFaction = agent.opponentFaction

  val addInfluenceScore = {
    if (influenceProvider.getController(country) == faction)
      threat * 0.4
    else if (influenceProvider.getController(country) == opponentFaction)
      importance / 4
    else
      importance + threat - threat / country.stability + gain
  }

  val removeInfluenceScore = importance

  override def toString = f"$addInfluenceScore%.1f"

}
