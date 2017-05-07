package me.herbix.ts.util

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.card.Card
import me.herbix.ts.logic.{Faction, Country, Game}

/**
  * Created by Chaofan on 2016/7/24.
  */
object ConditionBuilder {
  def influence = new InfluenceCondition(List.empty)
  def selectCard = new CardCondition(List.empty)
  def selectCountry = new CountryCondition(List.empty)

  val orInfluence: (Game, Map[Country, Int]) => Boolean = (_, _) => throw new NotImplementedError
  val orCountry: (Game, Set[Country]) => Boolean = (_, _) => throw new NotImplementedError
  val orCard: (Game, Card) => Boolean = (_, _) => throw new NotImplementedError

  val orSet: Set[(Game, _ <: AnyRef) => Boolean] = Set(orInfluence, orCard, orCountry)
}

class Condition[T](val items: List[(Game, T) => Boolean]) {
  private def toMethodImpl(game: Game, param: T): Boolean = {
    var lastCondition = true
    var followOr = false
    for (item <- items) {
      if (!ConditionBuilder.orSet.contains(item)) {
        if (followOr) {
          lastCondition ||= item(game, param)
        } else {
          if (!lastCondition) {
            return false
          }
          lastCondition = item(game, param)
        }
        followOr = false
      } else {
        followOr = true
      }
    }
    lastCondition
  }

  def build: (Game, T) => Boolean = toMethodImpl
}

class InfluenceCondition(items: List[(Game, Map[Country, Int]) => Boolean]) extends Condition[Map[Country, Int]](items) {

  def inRegion(regions: Region*) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(_._1.regions.exists(regions.contains))))

  def max(count: Int) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(_._2 <= count)))

  def notControlledBy(faction: Faction) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(t => game.getController(t._1) != faction)))

  def notControlled = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(t => game.getController(t._1) == Neutral)))

  def inCountry(names: String*) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(e => names.contains(e._1.name))))

  def inCountry(names: Set[String]) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(e => names(e._1.name))))

  def noInfluence = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(e => game.influence(e._1, US) == 0 && game.influence(e._1, USSR) == 0)))

  def notInRegion(regions: Region*) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(!_._1.regions.exists(regions.contains))))

  def notInCountry(names: String*) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(e => !names.contains(e._1.name))))

  def gameCanAddInfluence = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => game.canAddInfluence(game.operatingPlayer)(detail)))

  def inRegionShownInCardData = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(_._1.regions(game.currentCardData.asInstanceOf[Region]))))

  def or = new InfluenceCondition(items :+ ConditionBuilder.orInfluence)

  def all = this

}


class CardCondition(items: List[(Game, Card) => Boolean]) extends Condition[Card](items) {

  def minModifiedOp(count: Int) = new CardCondition(items :+
    ((game: Game, card: Card) => game.modifyOp(game.operatingPlayer, card.op) >= count))

  def minModifiedOpLargerThanCardData = new CardCondition(items :+
    ((game: Game, card: Card) => game.modifyOp(game.operatingPlayer, card.op) >= game.currentCardData.asInstanceOf[Int]))

  def minModifiedOp(faction: Faction, count: Int) = new CardCondition(items :+
    ((game: Game, card: Card) => game.modifyOp(faction, card.op) >= count))

  def canDiscardBy(faction: Faction) = new CardCondition(items :+
    ((game: Game, card: Card) => card.canDiscard(game, faction)))

  def canDiscard = new CardCondition(items :+
    ((game: Game, card: Card) => card.canDiscard(game, game.operatingPlayer)))

  def isOppositeCard = new CardCondition(items :+
    ((game: Game, card: Card) => card.faction == Faction.getOpposite(game.operatingPlayer)))

  def canHeld = new CardCondition(items :+
    ((game: Game, card: Card) => card.canHeld(game)))

  def canEvent = new CardCondition(items :+
    ((game: Game, card: Card) => card.canEvent(game, game.operatingPlayer)))

  def or = new CardCondition(items :+ ConditionBuilder.orCard)

  def all = this

}

class CountryCondition(items: List[(Game, Set[Country]) => Boolean]) extends Condition[Set[Country]](items) {

  def inRegion(regions: Region*) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(_.regions.exists(regions.contains))))

  def notControlled = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(game.getController(_) == Neutral)))

  def withName(names: String*) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(c => names.contains(c.name))))

  def withName(names: Set[String]) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(c => names(c.name))))

  def extraCondition(cond: (Game, Set[Country]) => Boolean) = new CountryCondition(items :+ cond)

  def maxStability(stability: Int) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(_.stability <= stability)))

  def canCoupWithoutFlags = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(c => game.canCoupWithoutFlags(game.operatingPlayer, c))))

  def notBattlefield = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(!_.isBattlefield)))

  def influenceMoreOrEqual(faction: Faction, count: Int) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(c => game.influence(c, faction) >= count)))

  def or = new CountryCondition(items :+ ConditionBuilder.orCountry)

  def all = this

}