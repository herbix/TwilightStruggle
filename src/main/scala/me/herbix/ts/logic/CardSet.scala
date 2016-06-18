package me.herbix.ts.logic

import java.util.Random

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/12.
  */
class CardSet {
  val cards = mutable.Set[Card]()

  def cardCount = cards.size
  def isEmpty = cards.isEmpty

  def join(iterable: Iterable[Card]): Unit = {
    cards ++= iterable
  }

  def pick(random: Random): Card = {
    val id = random.nextInt(cardCount)
    cards.toStream.slice(id, id + 1).head
  }

  def pickAndRemove(random: Random): Card = {
    val card = pick(random)
    remove(card)
    card
  }

  def remove(card: Card): Unit = {
    cards.remove(card)
  }

  def add(card: Card): Unit = {
    cards.add(card)
  }
}
