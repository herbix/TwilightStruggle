package me.herbix.ts.logic.card

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
object Cards extends CardsTrait

trait CardsTrait {
  protected val cardMap = mutable.Map[Int, Card]()

  protected def addCard(card: Card): Unit = cardMap += card.id -> card
  protected def removeCard(card: Card): Unit = cardMap -= card.id
  protected def replaceCard(from: Card, to: Card): Unit = {
    removeCard(from)
    addCard(to)
  }

  addCard(CardUnknown)

  addCard(Card001AsiaScoring)
  addCard(Card002EuropeScoring)
  addCard(Card003MidEastScoring)
  addCard(Card004DuckNCover)
  addCard(Card005FiveYearPlan)
  addCard(Card006ChinaCard)
  addCard(Card007SocialistGovernments)
  addCard(Card008Fidel)
  addCard(Card009VietnamRevolts)
  addCard(Card010Blockade)
  addCard(Card011KoreanWar)
  addCard(Card012RomanianAbdication)
  addCard(Card013ArabIsraeliWar)
  addCard(Card014COMECON)
  addCard(Card015Nasser)
  addCard(Card016WarsawPact)
  addCard(Card017DeGaulle)
  addCard(Card018CaptureNazi)
  addCard(Card019TrumanDoctrine)
  addCard(Card020OlympicGames)
  addCard(Card021NATO)
  addCard(Card022IndependentReds)
  addCard(Card023MarshallPlan)
  addCard(Card024IndoPakistaniWar)
  addCard(Card025Containment)
  addCard(Card026CIACreated)
  addCard(Card027USJapanPact)
  addCard(Card028SuezCrisis)
  addCard(Card029EastEuropeanUnrest)
  addCard(Card030Decolonization)
  addCard(Card031RedScarePurge)
  addCard(Card032UNIntervention)
  addCard(Card033DeStalinization)
  addCard(Card034NuclearTestBan)
  addCard(Card035Taiwan)
  addCard(Card036BrushWar)
  addCard(Card037MidAmericaScoring)
  addCard(Card038SEAsiaScoring)
  addCard(Card039ArmsRace)
  addCard(Card040CubaMissile)
  addCard(Card041NuclearSubs)
  addCard(Card042Quagmire)
  addCard(Card043SALTNegotiations)
  addCard(Card044BearTrap)
  addCard(Card045Summit)
  addCard(Card046HowILearnStopWorry)
  addCard(Card047Junta)
  addCard(Card048KitchenDebates)
  addCard(Card049MissileEnvy)
  addCard(Card050WeWillBuryYou)
  addCard(Card051BrezhnevDoctrine)
  addCard(Card052PortugueseCrumbles)
  addCard(Card053SouthAfricaUnrest)
  addCard(Card054Allende)
  addCard(Card055WillyBrandt)
  addCard(Card056MuslimRevolution)
  addCard(Card057ABMTreaty)
  addCard(Card058CulturalRevolution)
  addCard(Card059FlowerPower)
  addCard(Card060U2Incident)
  addCard(Card061Opec)
  addCard(Card062LoneGunman)
  addCard(Card063ColonialRearGuards)
  addCard(Card064PanamaCanalReturned)
  addCard(Card065CampDavidAccords)
  addCard(Card066PuppetGovernments)
  addCard(Card067GrainSales)
  addCard(Card068JohnPaulII)
  addCard(Card069LatinAmericaSquads)
  addCard(Card070OASFounded)
  addCard(Card071NixonPlaysTheChinaCard)
  addCard(Card072SadatExpelsSoviets)
  addCard(Card073ShuttleDiplomacy)
  addCard(Card074VoiceOfAmerica)
  addCard(Card075LiberationTheology)
  addCard(Card076UssuriRiverSkirmish)
  addCard(Card077AskNotWhatYourCountry)
  addCard(Card078AllianceForProgress)
  addCard(Card079AfricaScoring)
  addCard(Card080SmallStep)
  addCard(Card081SouthAmericaScoring)
  addCard(Card082IranianHostage)
  addCard(Card083TheIronLady)
  addCard(Card084ReaganBombsLibya)
  addCard(Card085StarWars)
  addCard(Card086NorthSeaOil)
  addCard(Card087Reformer)
  addCard(Card088BarracksBombing)
  addCard(Card089ShootDownKAL007)
  addCard(Card090Glasnost)
  addCard(Card091OrtegaInNicaragua)
  addCard(Card092Terrorism)
  addCard(Card093IranContra)
  addCard(Card094Chernobyl)
  addCard(Card095LatinAmericaDebtCrisis)
  addCard(Card096TearDownThisWall)
  addCard(Card097EvilEmpire)
  addCard(Card098AldrichAmes)
  addCard(Card099PershingII)
  addCard(Card100WarGames)
  addCard(Card101Solidarity)
  addCard(Card102IranIraqWar)
  addCard(Card103Defectors)
  addCard(Card104CambridgeFive)
  addCard(Card105SpecialRelationship)
  addCard(Card106NORAD)
  addCard(Card107Che)
  addCard(Card108OurManInTehran)
  addCard(Card109YuriAndSamantha)
  addCard(Card110AwacsSale)

  def fromId(id: Int): Card = cardMap.getOrElse(id, null)

  def earlyWarSet = cardMap.filter(e => (e._1 > 0 && e._1 <= 35 && e._1 != 6) || e._1 == 103).values
  def midWarSet = cardMap.filter(e => e._1 > 35 && e._1 <= 81).values
  def lateWarSet = cardMap.filter(e => e._1 > 81 && e._1 <= 102).values
  def earlyWarOptionalSet = cardMap.filter(e => e._1 > 103 && e._1 <= 106).values
  def midWarOptionalSet = cardMap.filter(e => e._1 > 106 && e._1 <= 108).values
  def lateWarOptionalSet = cardMap.filter(e => e._1 > 108 && e._1 <= 110).values
  def chinaCard = cardMap(6)

  def allCards = cardMap.values

  def isEarlyWarCard(card: Card): Boolean = {
    val i = card.id
    i <= 35 || (i >= 103 && i <= 106)
  }

  def isMidWarCard(card: Card): Boolean = {
    val i = card.id
    (i >= 36 && i <= 81) || (i >= 107 && i <= 108)
  }

  def isLateWarCard(card: Card): Boolean = {
    val i = card.id
    (i >= 82 && i <= 102) || (i >= 109 && i <= 110)
  }

  def getCardPeriod(card: Card): Int =
    if (isEarlyWarCard(card)) 1
    else if (isMidWarCard(card)) 2
    else if (isLateWarCard(card)) 3
    else 4

  def isCardStarred(card: Card): Boolean = {
    card.isRemovedAfterEvent && card != Card073ShuttleDiplomacy && card != Card049MissileEnvy
  }
}
