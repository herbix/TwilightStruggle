package test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic._
import me.herbix.ts.logic.card._
import me.herbix.ts.util.Serializer
import org.junit.Assert._
import org.junit.Test

/**
  * Created by Chaofan on 2016/7/9.
  */
class OperationSerializationTest {

  implicit val game = GameFactory.createGameByVariant(GameVariant.Standard)

  def c(name: String): Country = game.theWorldMap.countries(name)

  @Test
  def doTest(): Unit = {
    def TestOperation(op: Operation): Unit = {
      val out = new ByteArrayOutputStream()
      op.writeToStream(new DataOutputStream(out))
      val in = new ByteArrayInputStream(out.toByteArray)
      val newOp = Serializer.readOperation(new DataInputStream(in))

      assertOperationEquals(op, newOp)
    }

    TestOperation(new OperationChooseFaction(0, US))
    TestOperation(new OperationChooseFaction(Integer.MAX_VALUE, USSR))
    TestOperation(new OperationChooseFaction(Integer.MIN_VALUE, Neutral))

    TestOperation(new OperationCubaMissileRequest(2, US, false))
    TestOperation(new OperationCubaMissileRequest(100, USSR, true))

    TestOperation(new OperationIntValue(0, US, 0))
    TestOperation(new OperationIntValue(3, USSR, 1))
    TestOperation(new OperationIntValue(2, US, Integer.MAX_VALUE))
    TestOperation(new OperationIntValue(7, USSR, Integer.MIN_VALUE))

    TestOperation(new OperationModifyInfluence(0, USSR, true, Map()))
    TestOperation(new OperationModifyInfluence(9, USSR, true, Map(c("Japan") -> 10)))
    TestOperation(new OperationModifyInfluence(2, US, false, Map(c("Cuba") -> 1, c("Canada") -> 100)))
    TestOperation(new OperationModifyInfluence(23, USSR, true, Map(c("UK") -> Integer.MAX_VALUE)))
    TestOperation(new OperationModifyInfluence(6, USSR, false, Map(c("Panama") -> Integer.MIN_VALUE)))

    TestOperation(new OperationSelectCard(0, US, None))
    TestOperation(new OperationSelectCard(12, USSR, Some(CardUnknown)))
    TestOperation(new OperationSelectCard(98, USSR, Some(Card110AwacsSale)))
    TestOperation(new OperationSelectCard(32, US, Some(Card006ChinaCard)))

    for (action <- Action.values) TestOperation(new OperationSelectCardAndAction(23, US, Card001AsiaScoring, action))

    TestOperation(new OperationSelectCards(0, USSR, Set()))
    TestOperation(new OperationSelectCards(0, USSR, Set(Card001AsiaScoring)))
    TestOperation(new OperationSelectCards(0, USSR, Set(Card110AwacsSale)))
    TestOperation(new OperationSelectCards(0, USSR, Set(Card032UNIntervention, Card093IranContra)))

    TestOperation(new OperationSelectCountry(0, US, Set()))
    TestOperation(new OperationSelectCountry(0, US, Set(c("Japan"))))
    TestOperation(new OperationSelectCountry(0, US, Set(c("Panama"), c("N.Korea"))))

    for (action <- Action.values) TestOperation(new OperationSelectOperation(0, USSR, action))

    for (region <- Region.values) TestOperation(new OperationSelectRegion(0, US, region))

    TestOperation(new OperationYesNo(2, US, false))
    TestOperation(new OperationYesNo(100, USSR, true))

  }

  def assertOperationEquals(expected: Operation, actual: Operation): Unit = {
    assertEquals(expected.getClass, actual.getClass)

    assertEquals(expected.playerId, actual.playerId)
    assertEquals(expected.faction, actual.faction)

    (expected, actual) match {
      case (e: OperationCubaMissileRequest, a: OperationCubaMissileRequest) =>
        assertEquals(e.isResponse, a.isResponse)
      case (e: OperationIntValue, a: OperationIntValue) =>
        assertEquals(e.value, a.value)
      case (e: OperationModifyInfluence, a: OperationModifyInfluence) =>
        assertEquals(e.isAdd, a.isAdd)
        assertEquals(e.detail.size, a.detail.size)
        assert(e.detail.forall(t => a.detail.contains(t._1) && a.detail(t._1) == t._2))
      case (e: OperationSelectCard, a: OperationSelectCard) =>
        assertEquals(e.card, a.card)
      case (e: OperationSelectCardAndAction, a: OperationSelectCardAndAction) =>
        assertEquals(e.card, a.card)
        assertEquals(e.action, a.action)
      case (e: OperationSelectCards, a: OperationSelectCards) =>
        assertEquals(e.cards.size, a.cards.size)
        assert(e.cards.forall(a.cards(_)))
      case (e: OperationSelectCountry, a: OperationSelectCountry) =>
        assertEquals(e.detail.size, a.detail.size)
        assert(e.detail.forall(a.detail(_)))
      case (e: OperationSelectOperation, a: OperationSelectOperation) =>
        assertEquals(e.action, a.action)
      case (e: OperationSelectRegion, a: OperationSelectRegion) =>
        assertEquals(e.region, a.region)
      case (e: OperationYesNo, a: OperationYesNo) =>
        assertEquals(e.value, a.value)
      case _ =>
    }

  }

}
