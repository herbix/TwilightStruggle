package me.herbix.ts.util

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic._
import me.herbix.ts.logic.card._
import me.herbix.ts.logic.chinesecivilwar.CCWFlags
import me.herbix.ts.logic.turnzero.{TZCards, TZFlags, CardTZ02NationalistChina}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
object Lang {
  val USSR = "苏联"
  val US = "美国"
  val Neutral = "中立"

  val vp = "ＶＰ: "
  val defcon = "核战: %s"
  val military = "军事: "
  val spaceComma = "太空: "
  val deck = "牌堆: %s"
  val turn = "回合: %s-%s"
  val currentPlayer = "当前玩家为："

  val songti = "宋体"
  val heiti = "黑体"
  val lishu = "隶书"
  val kaiti = "楷体"

  val selectFaction = "选择你的阵营"
  val waitingForOpposite = "等待对方行动"
  val spectator = "观察者模式"

  val selfHand = "自己手牌"
  val oppositeHand = "对手手牌"
  val discardedCards = "弃牌堆"
  val eventCards = "事件用牌"
  val usHand = "美国手牌"
  val ussrHand = "苏联手牌"

  val putEastEurope = "请在东欧放置%s点影响力"
  val putWestEurope = "请在西欧放置%s点影响力"
  val putExtra = "请在已有国放置%s点影响力"

  var country = "国家"
  val influence = "影响力"

  val done = "完成"
  val cancel = "取消"
  val reset = "重置"
  val remove = "去除"

  val earlyWar = "冷战早期"
  val midWar = "冷战中期"
  val lateWar = "冷战后期"
  val special = "特殊"

  val goodFlag = "有利标记"
  val badFlag = "不利标记"

  val crisisCard = "危机牌"

  val selectHeadline = "请选择头条牌"

  val selectCardAndAction = "请选择要打出的牌"

  val space = "太空"
  val event = "事件"
  val operation = "行动"
  val eventFirst = "先事件"
  val operationFirst = "先行动"

  val addInfluence = "增加影响力"
  val realignment = "调整阵营"
  val coup = "政变"

  val operationSelect = "请用%s行动力来行动"
  val operationAddInfluence = "请用%s行动力来增加影响力"
  val operationRealignment = "请调整%1$s个国家（剩余%2$s）"
  val operationCoup = "请政变%1$s个国家（行动力%2$s）"

  val increase = "增加"
  val decrease = "减少"

  val discardHeldCard = "可以弃掉一张持牌"

  val yes = "是"
  val no = "否"

  val take8rounds = "是否要进行8个行动轮"

  val selectQuagmireDiscard = "请弃一张2以上行动力的牌"
  val selectScoringCard = "请打出一张计分牌"

  val thisTurnFlag = "回合结束时标记失效。"
  val duringSetupFlag = "此标记在开局时生效。"

  val removeFromGame = "此牌于事件发动后移出游戏"

  val improve = "改善"
  val degrade = "恶化"
  val keep = "不变"

  val selectRegion = "请选择一个区域"

  val play = "打出"
  val giveBack = "归还"

  val gameOver = "游戏结束"
  val winnerIs = "胜利者是%s"
  val drawGame = "平局"

  val presence = "存在"
  val domination = "支配"
  val control = "控制"

  val success = "胜利"

  val chooseStateCarft = "请选择一张状态修改牌"

  val sum = "总数"
  val battlefieldCountry = "战场国"
  val nonBattlefieldCountry = "非战场国"
  val opponentNear = "对方邻国"

  val southEastAsiaScore =
    "缅甸，老挝/柬埔寨，越南，马来西亚，印度尼西亚，菲律宾的控制者各+1VP\n" +
    "泰国的控制者+2VP"

  val chineseCivilWar = "中国内战"

  val historyStartGame = "开始游戏"
  val historyTurnStart = "第%s回合开始"
  val historyTurnHeadline = "第%s回合头条"
  val historyTurnRound = "第%s回合第%s行动轮"
  val historyPickCard = "%s抽取了%s张牌"
  val historyGetCard = "%s获得了“%s”"
  val historyDiscardCard = "%s丢弃了“%s”"
  val historyLoseCard = "%s失去了“%s”"
  val historyModifyInfluence = "%2$s了%1$s影响力："
  val historyModifyInfluenceDetail = "%1$s %2$s -> %3$s"
  val historyPlayHeadline = "%s打出“%s”作为头条"
  val historyEvent = "%s发动了“%s”的事件"
  val historyCardAction = "%s打出“%s”用于%s"
  val historyCardActionOpposite = "%s打出“%s”，先%s，后%s"
  val historyCardOperation = "%s用“%s”的行动力%s点进行%s行动"
  val historyOperationSpace = "%s的太空竞赛掷点为%s"
  val historyOperationRealignment = "在%s进行调整阵营：美国掷得%s，调整为%s；苏联掷得%s，调整为%s"
  val historyOperationCoup = "%s在%s发动政变，掷得%s，行动力%s，影响力变数得%s"
  val historySpace = "%s的太空竞赛等级从%s前进到%s"
  val historyVp = "%s得%sVP"
  val historyDefconImprove = "核战等级从%s级改善到%s级"
  val historyDefconDegrade = "核战等级从%s级恶化到%s级"
  val historyDefconStay = "核战等级保持%s级不变"
  val historyMilitary = "%s的军事行动数从%s增加到%s"
  val historyMilitaryReset = "重置%s的军事行动数"
  val historyScoring = "%s计分\n美国：控制国家%s个，战场国%s个\n苏联：控制国家%s个，战场国%s个"
  val historyWar = "%s操纵，入侵%s，掷点得%s，调整为%s"
  val historyRollDice = "%s掷出了%s"
  val historyRollDiceModified = "%s掷出了%s，调整为%s"
  val historyPokeChest = "%s戳了%s的胸口"
  val historyAddFlag = "%s获得了标记“%s”"
  val historyRemoveFlag = "%s失去了标记“%s”"
  val historyAddFlagNeutral = "双方获得了标记“%s”"
  val historyRemoveFlagNeutral = "双方失去了标记“%s”"
  val historyRegion = "%s选择了%s"
  val historyYesNo = "%s选择了%s"
  val historyAddCardToDeck = "“%s”被加入%s牌堆"
  val historyRemoveCardFromDeck = "“%s”被从%s牌堆移除"
  val historyCrisis = "开始处理危机牌“%s”"

  val countryNames = mutable.Map[String, String]()

  countryNames += "China" -> "中国"
  countryNames += "Mexico" -> "墨西哥"
  countryNames += "Guatemala" -> "危地马拉"
  countryNames += "El Salvador" -> "萨尔瓦多"
  countryNames += "Honduras" -> "洪都拉斯"
  countryNames += "Costa Rica" -> "哥斯达黎加"
  countryNames += "Cuba" -> "古巴"
  countryNames += "Nicaragua" -> "尼加拉瓜"
  countryNames += "Haiti" -> "海地"
  countryNames += "Dominican Rep" -> "多明尼加共和国"
  countryNames += "Panama" -> "巴拿马"
  countryNames += "Colombia" -> "哥伦比亚"
  countryNames += "Ecuador" -> "厄瓜多尔"
  countryNames += "Peru" -> "秘鲁"
  countryNames += "Chile" -> "智利"
  countryNames += "Argentina" -> "阿根廷"
  countryNames += "Venezuela" -> "委内瑞拉"
  countryNames += "Bolivia" -> "玻利维亚"
  countryNames += "Paraguay" -> "巴拉圭"
  countryNames += "Uruguay" -> "乌拉圭"
  countryNames += "Brazil" -> "巴西"
  countryNames += "Canada" -> "加拿大"
  countryNames += "UK" -> "英国"
  countryNames += "Benelux" -> "比荷卢联盟"
  countryNames += "France" -> "法国"
  countryNames += "Spain/Portugal" -> "西班牙/葡萄牙"
  countryNames += "Norway" -> "挪威"
  countryNames += "Denmark" -> "丹麦"
  countryNames += "W.Germany" -> "西德"
  countryNames += "Sweden" -> "瑞典"
  countryNames += "Italy" -> "意大利"
  countryNames += "Greece" -> "希腊"
  countryNames += "Turkey" -> "土耳其"
  countryNames += "Finland" -> "芬兰"
  countryNames += "Austria" -> "奥地利"
  countryNames += "E.Germany" -> "东德"
  countryNames += "Poland" -> "波兰"
  countryNames += "Czechoslovakia" -> "捷克斯洛伐克"
  countryNames += "Hungary" -> "匈牙利"
  countryNames += "Yugoslavia" -> "南斯拉夫"
  countryNames += "Romania" -> "罗马尼亚"
  countryNames += "Bulgaria" -> "保加利亚"
  countryNames += "Lebanon" -> "黎巴嫩"
  countryNames += "Syria" -> "叙利亚"
  countryNames += "Israel" -> "以色列"
  countryNames += "Iraq" -> "伊拉克"
  countryNames += "Iran" -> "伊朗"
  countryNames += "Libya" -> "利比亚"
  countryNames += "Egypt" -> "埃及"
  countryNames += "Jordan" -> "约旦"
  countryNames += "Gulf States" -> "海湾国家"
  countryNames += "Saudi Arabia" -> "沙特阿拉伯"
  countryNames += "Afghanistan" -> "阿富汗"
  countryNames += "Pakistan" -> "巴基斯坦"
  countryNames += "India" -> "印度"
  countryNames += "Burma" -> "缅甸"
  countryNames += "Laos/Cambodia" -> "老挝/柬埔寨"
  countryNames += "Thailand" -> "泰国"
  countryNames += "Vietnam" -> "越南"
  countryNames += "Malaysia" -> "马来西亚"
  countryNames += "Indonesia" -> "印度尼西亚"
  countryNames += "Australia" -> "澳大利亚"
  countryNames += "Philippines" -> "菲律宾"
  countryNames += "Japan" -> "日本"
  countryNames += "Taiwan" -> "台湾"
  countryNames += "S.Korea" -> "韩国"
  countryNames += "N.Korea" -> "朝鲜"
  countryNames += "Tunisia" -> "突尼斯"
  countryNames += "Algeria" -> "阿尔及利亚"
  countryNames += "Morocco" -> "摩洛哥"
  countryNames += "West African States" -> "西非国家"
  countryNames += "Saharan States" -> "撒哈拉联邦"
  countryNames += "Sudan" -> "苏丹"
  countryNames += "Ivory Coast" -> "科特迪瓦"
  countryNames += "Nigeria" -> "尼日利亚"
  countryNames += "Ethiopia" -> "埃塞俄比亚"
  countryNames += "Somalia" -> "索马里"
  countryNames += "Cameroon" -> "喀麦隆"
  countryNames += "Zaire" -> "扎伊尔"
  countryNames += "Kenya" -> "肯尼亚"
  countryNames += "Angola" -> "安哥拉"
  countryNames += "Zimbabwe" -> "津巴布韦"
  countryNames += "SE African States" -> "东南非洲诸国"
  countryNames += "Botswana" -> "博茨瓦纳"
  countryNames += "South Africa" -> "南非"

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
    case Region.Europe => "欧洲"
    case Region.Asia => "亚洲"
    case Region.MidEast => "中东"
    case Region.Africa => "非洲"
    case Region.MidAmerica => "中美洲"
    case Region.SouthAmerica => "南美洲"
    case Region.EastEurope => "东欧"
    case Region.WestEurope => "西欧"
    case Region.SouthEastAsia => "东南亚"
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
    case 1 => "前期"
    case 2 => "中期"
    case 3 => "后期"
    case 4 => "特殊"
  }
}
