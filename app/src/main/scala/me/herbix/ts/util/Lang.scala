package me.herbix.ts.util

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic._
import me.herbix.ts.logic.card._
import me.herbix.ts.logic.chinesecivilwar.CCWFlags
import me.herbix.ts.logic.turnzero.{TZCards, TZFlags}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
object Lang {
  val USSR = Localization.string("USSR")
  val US = Localization.string("US")
  val Neutral = Localization.string("Neutral")

  val vp = Localization.string("vp")
  val defcon = Localization.string("defcon")
  val military = Localization.string("military")
  val spaceComma = Localization.string("spaceComma")
  val deck = Localization.string("deck")
  val turn = Localization.string("turn")
  val currentPlayer = Localization.string("currentPlayer")

  val songti = Localization.string("songti")
  val heiti = Localization.string("heiti")
  val lishu = Localization.string("lishu")
  val kaiti = Localization.string("kaiti")

  val selectFaction = Localization.string("selectFaction")
  val waitingForOpposite = Localization.string("waitingForOpposite")
  val spectator = Localization.string("spectator")

  val selfHand = Localization.string("selfHand")
  val oppositeHand = Localization.string("oppositeHand")
  val discardedCards = Localization.string("discardedCards")
  val eventCards = Localization.string("eventCards")
  val usHand = Localization.string("usHand")
  val ussrHand = Localization.string("ussrHand")

  val putEastEurope = Localization.string("putEastEurope")
  val putWestEurope = Localization.string("putWestEurope")
  val putExtra = Localization.string("putExtra")

  val country = Localization.string("country")
  val influence = Localization.string("influence")

  val done = Localization.string("done")
  val cancel = Localization.string("cancel")
  val reset = Localization.string("reset")
  val remove = Localization.string("remove")

  val earlyWar = Localization.string("earlyWar")
  val midWar = Localization.string("midWar")
  val lateWar = Localization.string("lateWar")
  val special = Localization.string("special")

  val goodFlag = Localization.string("goodFlag")
  val badFlag = Localization.string("badFlag")

  val crisisCard = Localization.string("crisisCard")

  val selectHeadline = Localization.string("selectHeadline")

  val selectCardAndAction = Localization.string("selectCardAndAction")

  val space = Localization.string("space")
  val event = Localization.string("event")
  val operation = Localization.string("operation")
  val eventFirst = Localization.string("eventFirst")
  val operationFirst = Localization.string("operationFirst")

  val addInfluence = Localization.string("addInfluence")
  val realignment = Localization.string("realignment")
  val coup = Localization.string("coup")

  val operationSelect = Localization.string("operationSelect")
  val operationAddInfluence = Localization.string("operationAddInfluence")
  val operationRealignment = Localization.string("operationRealignment")
  val operationCoup = Localization.string("operationCoup")

  val increase = Localization.string("increase")
  val decrease = Localization.string("decrease")

  val discardHeldCard = Localization.string("discardHeldCard")

  val yes = Localization.string("yes")
  val no = Localization.string("no")

  val take8rounds = Localization.string("take8rounds")

  val selectQuagmireDiscard = Localization.string("selectQuagmireDiscard")
  val selectScoringCard = Localization.string("selectScoringCard")

  val thisTurnFlag = Localization.string("thisTurnFlag")
  val duringSetupFlag = Localization.string("duringSetupFlag")

  val removeFromGame = Localization.string("removeFromGame")

  val improve = Localization.string("improve")
  val degrade = Localization.string("degrade")
  val keep = Localization.string("keep")

  val selectRegion = Localization.string("selectRegion")

  val play = Localization.string("play")
  val giveBack = Localization.string("giveBack")

  val gameOver = Localization.string("gameOver")
  val winnerIs = Localization.string("winnerIs")
  val drawGame = Localization.string("drawGame")

  val presence = Localization.string("presence")
  val domination = Localization.string("domination")
  val control = Localization.string("control")

  val success = Localization.string("success")

  val chooseStateCarft = Localization.string("chooseStateCarft")

  val sum = Localization.string("sum")
  val battlefieldCountry = Localization.string("battlefieldCountry")
  val nonBattlefieldCountry = Localization.string("nonBattlefieldCountry")
  val opponentNear = Localization.string("opponentNear")

  val southEastAsiaScore = Localization.string("southEastAsiaScore")

  val chineseCivilWar = Localization.string("chineseCivilWar")

  val historyStartGame = Localization.string("historyStartGame")
  val historyTurnStart = Localization.string("historyTurnStart")
  val historyTurnHeadline = Localization.string("historyTurnHeadline")
  val historyTurnRound = Localization.string("historyTurnRound")
  val historyPickCard = Localization.string("historyPickCard")
  val historyGetCard = Localization.string("historyGetCard")
  val historyDiscardCard = Localization.string("historyDiscardCard")
  val historyLoseCard = Localization.string("historyLoseCard")
  val historyModifyInfluence = Localization.string("historyModifyInfluence")
  val historyModifyInfluenceDetail = Localization.string("historyModifyInfluenceDetail")
  val historyPlayHeadline = Localization.string("historyPlayHeadline")
  val historyEvent = Localization.string("historyEvent")
  val historyCardAction = Localization.string("historyCardAction")
  val historyCardActionOpposite = Localization.string("historyCardActionOpposite")
  val historyCardOperation = Localization.string("historyCardOperation")
  val historyOperationSpace = Localization.string("historyOperationSpace")
  val historyOperationRealignment = Localization.string("historyOperationRealignment")
  val historyOperationCoup = Localization.string("historyOperationCoup")
  val historySpace = Localization.string("historySpace")
  val historyVp = Localization.string("historyVp")
  val historyDefconImprove = Localization.string("historyDefconImprove")
  val historyDefconDegrade = Localization.string("historyDefconDegrade")
  val historyDefconStay = Localization.string("historyDefconStay")
  val historyMilitary = Localization.string("historyMilitary")
  val historyMilitaryReset = Localization.string("historyMilitaryReset")
  val historyScoring = Localization.string("historyScoring")
  val historyWar = Localization.string("historyWar")
  val historyRollDice = Localization.string("historyRollDice")
  val historyRollDiceModified = Localization.string("historyRollDiceModified")
  val historyPokeChest = Localization.string("historyPokeChest")
  val historyAddFlag = Localization.string("historyAddFlag")
  val historyRemoveFlag = Localization.string("historyRemoveFlag")
  val historyAddFlagNeutral = Localization.string("historyAddFlagNeutral")
  val historyRemoveFlagNeutral = Localization.string("historyRemoveFlagNeutral")
  val historyRegion = Localization.string("historyRegion")
  val historyYesNo = Localization.string("historyYesNo")
  val historyAddCardToDeck = Localization.string("historyAddCardToDeck")
  val historyRemoveCardFromDeck = Localization.string("historyRemoveCardFromDeck")
  val historyCrisis = Localization.string("historyCrisis")

  val twilightStruggle = Localization.string("twilightStruggle")
  val inputDebugCommand = Localization.string("inputDebugCommand")
  val help = Localization.string("help")
  val cardListStandard = Localization.string("cardListStandard")
  val cardListPromo = Localization.string("cardListPromo")
  val cardListTurnZero = Localization.string("cardListTurnZero")
  val singlePlayer = Localization.string("singlePlayer")
  val multiPlayer = Localization.string("multiPlayer")
  val exit = Localization.string("exit")
  val room = Localization.string("room")
  val roomId = Localization.string("roomId")
  val roomName = Localization.string("roomName")
  val gameVersion = Localization.string("gameVersion")
  val `new` = Localization.string("`new`")
  val join = Localization.string("join")
  val connecting = Localization.string("connecting")
  val connectFailed = Localization.string("connectFailed")
  val gameVariant = Localization.string("gameVariant")
  val extraInfluence = Localization.string("extraInfluence")
  val drawGameWinner = Localization.string("drawGameWinner")
  val extraCards = Localization.string("extraCards")
  val optional = Localization.string("optional")
  val promo = Localization.string("promo")
  val versionNotMatch = Localization.string("versionNotMatch")
  val newRoom = Localization.string("newRoom")
  val optionalCards = Localization.string("optionalCards")
  val promoCards = Localization.string("promoCards")
  val standard = Localization.string("standard")
  val turnZero = Localization.string("turnZero")
  val start = Localization.string("start")
  val player = Localization.string("player")
  val player1 = Localization.string("player1")
  val player2 = Localization.string("player2")
  val playerId = Localization.string("playerId")
  val playerName = Localization.string("playerName")
  val startGame = Localization.string("startGame")
  val agent = Localization.string("agent")
  val gameSettings = Localization.string("gameSettings")

  val early = Localization.string("early")
  val mid = Localization.string("mid")
  val late = Localization.string("late")

  val europe = Localization.string("europe")
  val asia = Localization.string("asia")
  val midEast = Localization.string("midEast")
  val africa = Localization.string("africa")
  val midAmerica = Localization.string("midAmerica")
  val southAmerica = Localization.string("southAmerica")
  val eastEurope = Localization.string("eastEurope")
  val westEurope = Localization.string("westEurope")
  val southEastAsia = Localization.string("southEastAsia")

  val countryNames = mutable.Map[String, String]()
  for (item <- CountryInfo.infoItems) {
    countryNames += item.name -> item(0).content
  }

  def getFactionName(faction: Faction) = faction match {
    case Faction.US => US
    case Faction.USSR => USSR
    case Faction.Neutral => Neutral
  }

  def getActionName(action: Action) = action match {
    case Action.Space => space
    case Action.Event => event
    case Action.Operation => operation
    case Action.Influence => addInfluence
    case Action.Realignment => realignment
    case Action.Coup => coup
  }

  def getRegionName(region: Region) = region match {
    case Region.Europe => europe
    case Region.Asia => asia
    case Region.MidEast => midEast
    case Region.Africa => africa
    case Region.MidAmerica => midAmerica
    case Region.SouthAmerica => southAmerica
    case Region.EastEurope => eastEurope
    case Region.WestEurope => westEurope
    case Region.SouthEastAsia => southEastAsia
  }

  val cardInfo = new Array[(String, String)](300)
  for (item <- CardInfo.infoItems) {
    val id = item.name.toInt
    val title = item.lines.head.content + (if (item("star") != null) "*" else "")
    val desc = item.lines
      .drop(1)
      .filter(_("comment") == null)
      .map(_.content).mkString("\n")
    cardInfo(id) = (title, desc)
  }

  FlagsTrait.initFlags()
  val flagInfo = new Array[(Map[Faction, String], String)](120)
  val turnZeroFlagTips = mutable.Map[Int, String]()
  for (item <- FlagInfo.infoItems) {
    val name = item.name
    val id = if (name.startsWith("CCW")) {
      CCWFlags.FlagIdOffset + 1
    } else if (name.startsWith("TZ")) {
      TZFlags.FlagIdOffset + 1 + name.substring(2).toInt
    } else {
      name.toInt
    }
    val neutralName = item.lines.head.content
    val usName = item("US") match {case null => neutralName case x => x}
    val ussrName = item("USSR") match {case null => neutralName case x => x}
    val desc = item.lines.drop(1).filter(_("tip") == null).map(_.content).mkString("\n")
    item.lines.find(_("tip") != null).map(_.content) match {
      case Some(tip) => turnZeroFlagTips += id - TZFlags.FlagIdOffset -> tip
      case None =>
    }
    flagInfo(id) = (Map(Faction.US -> usName, Faction.USSR -> ussrName, Faction.Neutral -> neutralName), desc)
  }

  val cardTips = mutable.Map.empty[Int, Array[String]]
  for (item <- CardTipInfo.infoItems) {
    val name = item.name
    val id = if (name.startsWith("P")) {
      PromoCards.Offset + name.substring(1).toInt
    } else if (name.startsWith("TZ")) {
      TZCards.IdInc + name.substring(2).toInt
    } else {
      name.toInt
    }
    val arr = item.lines.map(_.content).toArray
    cardTips += id -> arr
  }

  val spaceInfo = new Array[(String, String)](11)
  for (item <- SpaceInfo.infoItems) {
    val id = item.name.toInt
    val title = item.lines.head.content + (if (item("star") != null) "*" else "")
    val desc = item.lines.drop(1).map(_.content).mkString("\n")
    spaceInfo(id) = (title, desc)
  }

  val crisisInfo = Array.ofDim[(String, String)](7, 5)
  for (item <- CrisisInfo.infoItems) {
    val crisis = item.name.substring(0, 1).toInt - 1
    val stage = item.name.substring(2).toInt - 1

    val title = item.lines.head.content
    val desc = item.lines
      .drop(1)
      .map(_.content).mkString("\n")
    crisisInfo(crisis)(stage) = (title, desc)
  }

  def toString(flagData: Any): String = {
    flagData match {
      case null => null
      case region: Region => getRegionName(region)
      case _ => null
    }
  }

  def getPeriodName(period: Int) = period match {
    case 1 => early
    case 2 => mid
    case 3 => late
    case 4 => special
  }
}
