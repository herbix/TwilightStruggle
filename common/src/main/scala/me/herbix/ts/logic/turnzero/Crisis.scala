package me.herbix.ts.logic.turnzero

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Flags
import me.herbix.ts.logic.card._

import scala.collection.mutable

/**
  * Created by Chaofan on 2017/4/15.
  */
abstract class Crisis(val id: Int) {
  Crisis.byId(id) = this
  def effect(value: Int, game: GameTurnZero): Unit =
    value match {
      case 1 => effect1(game)
      case 2|3 => effect23(game)
      case 4|5 => effect45(game)
      case 6 => effect6(game)
    }

  def effect1(game: GameTurnZero): Unit
  def effect23(game: GameTurnZero): Unit
  def effect45(game: GameTurnZero): Unit
  def effect6(game: GameTurnZero): Unit
  def noEffect = /* Do nothing */ 0
}

object Crisis {
  def byId = mutable.Map[Int, Crisis]()
  def fromId(id: Int): Crisis = {
    Crisis.byId(id)
  }

  val setA = Set(CrisisYaltaAndPotsdam, CrisisVEDay, Crisis1945UKElection)
  val setB = Set(CrisisIsrael, CrisisChineseCivilWar, CrisisVJDay)
}

object CrisisYaltaAndPotsdam extends Crisis(0) {
  override def effect1(game: GameTurnZero): Unit = {
    game.addFlag(USSR, TZFlags.ussrEuropePlus1)
    game.addFlag(USSR, TZFlags.ussrVietnamOrArab)
  }

  override def effect23(game: GameTurnZero): Unit = noEffect

  override def effect45(game: GameTurnZero): Unit = {
    game.addFlag(US, TZFlags.usMarshall)
  }

  override def effect6(game: GameTurnZero): Unit = {
    effect45(game)
    game.addFlag(US, TZFlags.usGoesFirst)
  }
}

object CrisisVEDay extends Crisis(1) {
  override def effect1(game: GameTurnZero): Unit = {
    effect23(game)
    game.modifyInfluence(USSR, true, Map(
      game.theWorldMap.countries("W.Germany") -> 1,
      game.theWorldMap.countries("Austria") -> 1
    ))
  }

  override def effect23(game: GameTurnZero): Unit = {
    game.addFlag(USSR, TZFlags.ussrEastEuropePlus2)
  }

  override def effect45(game: GameTurnZero): Unit = noEffect

  override def effect6(game: GameTurnZero): Unit = {
    game.modifyInfluence(US, true, Map(game.theWorldMap.countries("E.Germany") -> 2))
    game.modifyInfluence(USSR, false, Map(game.theWorldMap.countries("E.Germany") -> 1))
    game.replaceEarlyCard(Card002EuropeScoring, CardTZ01EuropeScoring)
    game.addFlag(Neutral, TZFlags.blockadeNegated)
  }
}

object Crisis1945UKElection extends Crisis(2) {
  override def effect1(game: GameTurnZero): Unit = {
    game.modifyInfluence(US, false, Map(game.theWorldMap.countries("UK") -> 2))
  }

  override def effect23(game: GameTurnZero): Unit = noEffect

  override def effect45(game: GameTurnZero): Unit = {
    game.addFlag(Neutral, TZFlags.socialistGovernmentsNegated)
  }

  override def effect6(game: GameTurnZero): Unit = {
    game.moveCardFromEarlyToMid(Card007SocialistGovernmentsTZ)
    game.addFlag(Neutral, TZFlags.suezCrisisNegated)
  }
}

object CrisisIsrael extends Crisis(3) {
  override def effect1(game: GameTurnZero): Unit = {
    effect23(game)
    game.addFlag(USSR, TZFlags.ussrMidEastPlus2)
  }

  override def effect23(game: GameTurnZero): Unit = {
    game.modifyInfluence(USSR, true, Map(game.theWorldMap.countries("Syria") -> 1))
  }

  override def effect45(game: GameTurnZero): Unit = noEffect

  override def effect6(game: GameTurnZero): Unit = {
    game.modifyInfluence(US, true, Map(
      game.theWorldMap.countries("Israel") -> 1,
      game.theWorldMap.countries("Lebanon") -> 1,
      game.theWorldMap.countries("Jordan") -> 1
    ))
  }
}

object CrisisChineseCivilWar extends Crisis(4) {
  override def effect1(game: GameTurnZero): Unit = {
    game.removeCardFromGame(Card076UssuriRiverSkirmish, 2)
  }

  override def effect23(game: GameTurnZero): Unit = noEffect

  override def effect45(game: GameTurnZero): Unit = {
    game.addFlag(USSR, Flags.CantPlayChinaCard)
  }

  override def effect6(game: GameTurnZero): Unit = {
    effect45(game)
    game.setTaiwanBattlefield()
    game.modifyInfluence(US, true, Map(game.theWorldMap.countries("Taiwan") -> 3))
    game.replaceEarlyCard(Card035Taiwan, CardTZ02NationalistChina)
  }
}

object CrisisVJDay extends Crisis(5) {
  override def effect1(game: GameTurnZero): Unit = {
    effect23(game)
    game.modifyInfluence(USSR, true, Map(
      game.theWorldMap.countries("Japan") -> 1,
      game.theWorldMap.countries("S.Korea") -> 1
    ))
    game.moveCardFromEarlyToMid(Card027USJapanPact)
  }

  override def effect23(game: GameTurnZero): Unit = {
    game.modifyInfluence(USSR, true, Map(
      game.theWorldMap.countries("S.Korea") -> 1
    ))
  }

  override def effect45(game: GameTurnZero): Unit = noEffect

  override def effect6(game: GameTurnZero): Unit = {
    game.removeCardFromGame(Card011KoreanWar, 1)
    game.addFlag(US, TZFlags.defconLock)
  }
}

object CrisisUnknown extends Crisis(6) {
  override def effect1(game: GameTurnZero): Unit = ???
  override def effect6(game: GameTurnZero): Unit = ???
  override def effect23(game: GameTurnZero): Unit = ???
  override def effect45(game: GameTurnZero): Unit = ???
}