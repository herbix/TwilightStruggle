package me.herbix.ts.util

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.card.Card
import me.herbix.ts.logic.{Country, Game}

/**
  * Created by Chaofan on 2016/7/24.
  */
object ConditionBuilder {
  def influence = new InfluenceCondition(List.empty)
  def selectCard = new CardCondition(List.empty)
  def selectCountry = new CountryCondition(List.empty)
}

class Condition[T](val items: List[(Game, T) => Boolean]) {
  private def toMethodImpl(game: Game, param: T): Boolean = {
    for (item <- items) {
      if (!item(game, param)) {
        return false
      }
    }
    true
  }

  def build: (Game, T) => Boolean = toMethodImpl
}

class InfluenceCondition(items: List[(Game, Map[Country, Int]) => Boolean]) extends Condition[Map[Country, Int]](items) {

  def inRegion(regions: Region*) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(_._1.regions.exists(regions.contains))))

  def max(count: Int) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(_._2 <= count)))

  def notControlledBy(faction: Faction) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(_._1.getController(game) != faction)))

  def notControlled = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(_._1.getController(game) == Neutral)))

  def inCountry(names: String*) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(e => names.contains(e._1.name))))

  def inCountry(names: Set[String]) = new InfluenceCondition(items :+
    ((game: Game, detail: Map[Country, Int]) => detail.forall(e => names(e._1.name))))

}


class CardCondition(items: List[(Game, Card) => Boolean]) extends Condition[Card](items) {

  def minModifiedOp(count: Int) = new CardCondition(items :+
    ((game: Game, card: Card) => game.modifyOp(game.operatingPlayer, card.op) >= count))

  def minModifiedOp(faction: Faction, count: Int) = new CardCondition(items :+
    ((game: Game, card: Card) => game.modifyOp(faction, card.op) >= count))

  def canDiscard(faction: Faction) = new CardCondition(items :+
    ((game: Game, card: Card) => card.canDiscard(game, faction)))

  def canDiscard = new CardCondition(items :+
    ((game: Game, card: Card) => card.canDiscard(game, game.operatingPlayer)))
}

class CountryCondition(items: List[(Game, Set[Country]) => Boolean]) extends Condition[Set[Country]](items) {

  def inRegion(regions: Region*) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(_.regions.exists(regions.contains))))

  def notControlled = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(_.getController(game) == Neutral)))

  def withName(names: String*) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(c => names.contains(c.name))))

  def withName(names: Set[String]) = new CountryCondition(items :+
    ((game: Game, detail: Set[Country]) => detail.forall(c => names(c.name))))

}