package me.herbix.ts.test

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, ByteArrayOutputStream}

import me.herbix.ts.logic._
import me.herbix.ts.logic.Faction._
import me.herbix.ts.util.Serializer
import org.junit.Test
import org.junit.Assert._

/**
  * Created by Chaofan on 2016/7/13.
  */
class GameSerializationTest {

  val game = new Game

  def c(name: String): Country = WorldMap.countries(name)

  @Test
  def doTest(): Unit = {

    game.playerId = 0

    game.setRandomSeed(0L)

    // player 0 is US while 1 is USSR
    game.nextState(new OperationChooseFaction(0, US))
    game.nextState(new OperationChooseFaction(1, USSR))

    game.nextState(new OperationModifyInfluence(1, USSR, true, Map(c("E.Germany") -> 3, c("Poland") -> 3)))
    game.nextState(new OperationModifyInfluence(0, US, true, Map(c("W.Germany") -> 4, c("Italy") -> 3)))

    val out = new ByteArrayOutputStream
    Serializer.writeGameState(game, new DataOutputStream(out))

    val historyId = game.currentHistory.last.id
    val state = game.stateStack.top

    // headline
    game.nextState(new OperationSelectCard(1, USSR, Some(Card003MidEastScoring)))
    game.nextState(new OperationSelectCard(0, US, Some(Card005FiveYearPlan)))

    // round 1 USSR
    game.nextState(new OperationSelectCardAndAction(1, USSR, Card010Blockade, Action.Operation))
    game.nextState(new OperationSelectOperation(1, USSR, Action.Influence))
    game.nextState(new OperationModifyInfluence(1, USSR, true, Map(c("N.Korea") -> 1)))

    // round 1 US
    game.nextState(new OperationSelectCardAndAction(0, US, Card018CaptureNazi, Action.Event))

    // round 2 USSR
    game.nextState(new OperationSelectCardAndAction(1, USSR, Card034NuclearTestBan, Action.Operation))
    game.nextState(new OperationSelectOperation(1, USSR, Action.Coup))
    game.nextState(new OperationSelectCountry(1, USSR, Set(c("Iran"))))

    // round 2 US
    game.nextState(new OperationSelectCardAndAction(1, USSR, Card020OlympicGames, Action.Event))

    val in = new DataInputStream(new ByteArrayInputStream(out.toByteArray))
    Serializer.readGameState(game, in)

    assertEquals(9, game.hand(USSR).cardCount)
    assertTrue(game.hand(US).has(Card005FiveYearPlan))
    assertTrue(game.hand(US).has(Card018CaptureNazi))

    assertEquals(8, game.hand(US).cardCount)
    assertTrue(game.hand(USSR).has(Card003MidEastScoring))
    assertTrue(game.hand(USSR).has(Card010Blockade))
    assertTrue(game.hand(USSR).has(Card034NuclearTestBan))

    assertEquals(0, game.discards.cardCount)

    assertEquals(3, game.influence(c("N.Korea"), USSR))
    assertEquals(0, game.round)
    assertEquals(0, game.vp)
    assertEquals(5, game.defcon)
    assertEquals(0, game.space(US).level)
    assertEquals(0, game.military(USSR))

    assertFalse(game.flags.hasFlag(Flags.Defcon4Penalty))

    assertEquals(historyId, game.currentHistory.last.id)

    assertEquals(state, game.stateStack.top)
  }

}
