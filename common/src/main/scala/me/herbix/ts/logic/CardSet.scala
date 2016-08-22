package me.herbix.ts.logic

import java.util.Random

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.card.{Cards, Card}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/12.
  */
class CardSet extends mutable.Iterable[Card] {
  private val cards = mutable.TreeSet[Card]()
  private var hasChinaCard = false

  def cardCount = cards.size + (if (hasChinaCard) 1 else 0)
  def canPlayCardCount(game: Game, faction: Faction) = count(_.canPlay(game, faction))
  def cardCountExcludingChinaCard = cards.size
  def has(card: Card): Boolean = cards(card) || (card == Cards.chinaCard && hasChinaCard)
  def clear() = {
    cards.clear()
    hasChinaCard = false
  }
  def isEmptyExcludingChinaCard = cards.isEmpty
  override def iterator: Iterator[Card] =
    if (hasChinaCard)
      (cards + Cards.chinaCard).iterator
    else
      cards.iterator
  def iteratorExcludingChinaCard: Iterator[Card] = cards.iterator

  def join(iterable: Iterable[Card]): Unit = {
    iterable.foreach(add)
  }

  def pick(random: Random): Card = {
    val id = random.nextInt(cardCountExcludingChinaCard)
    cards.toStream.slice(id, id + 1).head
  }

  def pickAndRemove(random: Random): Card = {
    val card = pick(random)
    remove(card)
    card
  }

  def remove(card: Card): Unit = {
    if (card == Cards.chinaCard) {
      hasChinaCard = false
    } else {
      cards.remove(card)
    }
  }

  def add(card: Card): Unit = {
    if (card == Cards.chinaCard) {
      hasChinaCard = true
    } else {
      cards.add(card)
    }
  }

}
