package me.herbix.ts.util

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic._
import me.herbix.ts.logic.card._

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

  val heiti = "黑体"
  val lishu = "隶书"

  val selectFaction = "选择你的阵营"
  val waitingForOpposite = "等待对方行动"
  val spectator = "观察者模式"

  val selfHand = "自己手牌"
  val oppositeHand = "对手手牌"
  val discardedCards = "弃牌堆"
  val eventCards = "事件用牌"

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

  val goodFlag = "有利标记"
  val badFlag = "不利标记"

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
  val selectQuagmireScoringCard = "请打出一张计分牌"

  val thisTurnFlag = "回合结束时标记失效。"

  val improve = "改善"
  val degrade = "恶化"
  val keep = "不变"

  val selectRegion = "请选择一个区域"

  val play = "打出"
  val giveBack = "归还"

  val gameOver = "游戏结束"
  val winnerIs = "胜利者是%s"
  val drawGame = "平局"

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

  val cardInfo = new Array[(String, String)](120)
  var cardInfoCount = 0
  def addCardInfo(name: String, desc: String): Unit = {
    cardInfo(cardInfoCount) = (name, desc)
    cardInfoCount += 1
  }

  addCardInfo("未知", "不可知此牌的信息。")
  addCardInfo("亚洲计分",
    "存在: 3\n" +
    "支配: 7\n" +
    "控制: 9\n" +
    "每个控制的战场国+1VP\n" +
    "每个控制的每个与对方超级大国相邻的国家+1VP\n" +
    "回合结束时不可持有此牌"
  )
  addCardInfo("欧洲计分",
    "存在: 3\n" +
    "支配: 7\n" +
    "控制: 胜利\n" +
    "每个控制的战场国+1VP\n" +
    "每个控制的每个与对方超级大国相邻的国家+1VP\n" +
    "回合结束时不可持有此牌"
  )
  addCardInfo("中东计分",
    "存在: 3\n" +
    "支配: 5\n" +
    "控制: 7\n" +
    "每个控制的战场国+1VP\n" +
    "回合结束时不可持有此牌"
  )
  addCardInfo("躲避与掩护", "核战等级恶化1级，然后美国获得5减核战等级的VP。")
  addCardInfo("五年计划", "苏联玩家须从手中随机弃置一牌，若是美国相关的事件则立即发生，否则不发生而进入弃牌堆。")
  addCardInfo("中国牌",
    "若将行动力全部用在亚洲，则行动力+1。使用之后交给对手。\n" +
    "在第10回合结束时持有此牌的玩家得1VP。\n" +
    "若是被美国玩家使用，取消“台湾决议”的效果。"
  )
  addCardInfo("社会主义政府",
    "从西欧移除3点美国影响力，每个国家最多移除2点。\n" +
    "若是已打出“铁娘子”，则此牌不可作事件打出。"
  )
  addCardInfo("菲德尔·卡斯特罗*", "移除古巴的所有美国影响力，苏联获得足以控制古巴的影响力。")
  addCardInfo("越南起义*",
    "在越南增加2点苏联影响力。\n" +
    "在本回合内，苏联如果将行动力全和在东南亚，则可以行动力+1。"
  )
  addCardInfo("封锁*", "美国玩家需弃掉一张行动力3或以上的手牌，否则移除美国在西德的所有影响力。")
  addCardInfo("朝鲜战争*",
    "朝鲜入侵韩国。掷骰子，减去美国控制的韩国邻国数，若得4~6，则苏联胜利。\n" +
    "苏联增加2军事行动。\n" +
    "胜利奖励：苏联得2VP，美国在韩国的影响力全部替换成苏联影响力。"
  )
  addCardInfo("罗马尼亚颠覆*", "移除罗马尼亚的所有美国影响力，苏联获得足以控制罗马尼亚的影响力。")
  addCardInfo("阿以战争",
    "泛阿拉伯联盟入侵以色列。掷骰子，减去美国控制的以色列及以色列邻国数，若得4~6，则苏联胜利。\n" +
    "苏联增加2军事行动。\n" +
    "胜利奖励：苏联得2VP，美国在以色列的影响力全部替换成苏联影响力。\n" +
    "若是已打出“戴维营协定”，则此牌不可作事件打出。"
  )
  addCardInfo("经济互助委员会*", "在美国未控制的4个东欧国家各增加1点苏联影响力。")
  addCardInfo("埃及总统纳赛尔*", "在埃及增加2点苏联影响力，移除美国的一半影响力（向上取整）。")
  addCardInfo("华沙条约组织成立*",
    "移除4个东欧国家的全部美国影响力，或在东欧增加5点苏联影响力，每个国家最多增加2点。\n" +
    "打出此牌后，“北大西洋公约组织”可以被作为事件打出。"
  )
  addCardInfo("戴高乐领导法国*",
    "在法国移除2点美国影响力，增加1点苏联影响力。\n" +
    "取消“北大西洋公约组织”对法国的效果。"
  )
  addCardInfo("逮捕纳粹科学家*", "当前玩家太空竞赛前进1格。")
  addCardInfo("杜鲁门主义*", "移除苏联在欧洲1个双方未控制国家的所有影响力。")
  addCardInfo("奥林匹克运动会",
    "当前玩家发起奥运会，对手可以选择参加或抵制。" +
    "若参加，则双方各掷骰一次，发起方点数+2，高者得2VP，平局重掷。" +
    "若抵制，则核战等级恶化1级，发起方视作打出一张4行动力的牌进行行动。"
  )
  addCardInfo("北大西洋公约组织*",
    "在打出“马歇尔计划”或“华沙条约组织成立”后可作为事件打出。\n" +
    "美国在欧洲控制的国家不能被苏联政变或调整阵营，也不能成为局部战争的目标。"
  )
  addCardInfo("独立的红色*",
    "在南斯拉夫，罗马尼亚，保加利亚，匈牙利和捷克斯洛伐克之中的1个国家增加美国影响力，数值和此国的苏联影响力相等。"
  )
  addCardInfo("马歇尔计划*",
    "在西欧7个未被苏联控制的国家增加1点美国影响力。\n" +
  "打出此牌后，“北大西洋公约组织”可以被作为事件打出。"
  )
  addCardInfo("印巴战争",
    "印度或巴基斯坦入侵另一方（由玩家选择）。掷骰子，减去对手控制的被入侵国邻国数，若得4~6，则当前玩家胜利。\n" +
    "当前玩家增加2军事行动。\n" +
    "胜利奖励：当前玩家得2VP，对手在被入侵国的影响力全部替换成当前玩家影响力。"
  )
  addCardInfo("遏制政策*", "本回合美国打出的所有行动牌行动力+1（上限为4）。")
  addCardInfo("建立中情局*", "苏联公开手牌。\n然后美国视作打出一张1行动力的牌进行行动。")
  addCardInfo("美日共同防卫协定*", "美国获得足以控制日本的影响力。\n苏联不能在日本进行政变或调整阵营。")
  addCardInfo("苏伊士运河危机*", "从法国，英国和以色列移除4点美国影响力，每个国家最多2点。")
  addCardInfo("东欧剧变", "冷战早中期：从东欧的3个国家各移除1点苏联影响力。\n冷战后期：从东欧的3个国家各移除2点苏联影响力。")
  addCardInfo("反殖民化", "在任意4个非洲或东南亚国家各增加1点苏联影响力。")
  addCardInfo("红色恐慌/党内清洗", "对手本回合打出的所有行动牌行动力-1（下限是1）。")
  addCardInfo("联合国干预",
    "将此牌和一张对手相关的牌同时打出，那张牌不发动事件并进入弃牌堆，当前玩家可以使用那张牌的行动力进行行动。\n" +
    "此牌不能在头条打出。"
  )
  addCardInfo("去斯大林化*", "苏联可以重新配置最多4点影响力到非美国控制的国家，每个国家最多放置2点影响力。")
  addCardInfo("禁止核试验", "当前玩家获得核战等级减2的VP，然后核战等级改善2级。")
  addCardInfo("台湾决议*",
    "在亚洲计分或终局计分时，若美国控制台湾，则台湾被视作战场国，其他时刻和情况下不被视为战场国。\n" +
    "美国打出中国牌时已发动的此牌效果消失。"
  )
  addCardInfo("局部战争",
    "入侵一个稳定性为1或2的国家。掷骰子，减去对手控制的被入侵国邻国数，若得3~6，则当前玩家胜利。\n" +
    "当前玩家增加3军事行动。\n" +
    "胜利奖励：当前玩家得1VP，对手在被入侵国的影响力全部替换成当前玩家影响力。"
  )
  addCardInfo("中美洲计分",
    "存在: 1\n" +
    "支配: 3\n" +
    "控制: 5\n" +
    "每个控制的战场国+1VP\n" +
    "每个控制的每个与对方超级大国相邻的国家+1VP\n" +
    "回合结束时不可持有此牌"
  )
  addCardInfo("东南亚计分*",
    "每个控制的非战场国+1VP\n" +
    "控制泰国+2VP\n" +
    "回合结束时不可持有此牌"
  )
  addCardInfo("军备竞赛", "比较双方军事行动，\n若当前玩家较多，则可得1VP，\n若当前玩家较多且满足军事行动需求，则可再得2VP。")
  addCardInfo("古巴导弹危机*",
    "核战等级设为2级。\n" +
    "本回合对手的任何政变将引发核战，使其输掉游戏。\n" +
    "对手在任何时候，作为苏联移除古巴的2点影响力，或作为美国移除西德和土耳其之一的2点影响力，就可以取消此牌效果。"
  )
  addCardInfo("核潜艇*", "本回合美国在战场国的政变将不会降低核战等级（不影响古巴导弹危机的效果）。")
  addCardInfo("困境*",
    "在下个行动轮，美国玩家不能进行通常行动，而是必须弃一张2或以上行动力的牌，然后掷骰，" +
    "若是1~4则取消此牌效果，否则以后的行动轮也须重复以上行动。" +
    "如果此轮因无符合条件的牌可弃而没有弃牌，则美国玩家不能掷骰，在这轮只能打出计分牌。\n" +
    "取消“北美防空司令部[可选牌]”的效果。"
  )
  addCardInfo("限制战略武器谈判*",
    "核战等级改善2级。\n" +
    "本回合双方政变的掷骰-1。\n" +
    "当前玩家可以从弃牌堆选一张非计分牌，给对手确认后回收到手牌。"
  )
  addCardInfo("捕熊陷阱*",
    "在下个行动轮，苏联玩家不能进行通常行动，而是必须弃一张2或以上行动力的牌，然后掷骰，" +
    "若是1~4则取消此牌效果，否则以后的行动轮也须重复以上行动。" +
    "如果此轮因无符合条件的牌可弃而没有弃牌，则苏联玩家不能掷骰，在这轮只能打出计分牌。"
  )
  addCardInfo("首脑会议", "双方各掷骰一次，加上各自支配或控制的区域数，胜者可得2VP并使核战等级改善或恶化1级。平局不重掷。")
  addCardInfo("我如何学会不再担忧*", "核战等级设为任意（1~5）等级。当前玩家增加5军事行动。")
  addCardInfo("军人集团",
    "在中美洲或南美洲的任意1个国家增加2点影响力。\n" +
    "当前玩家可以使用此牌的行动力在中美洲或南美洲发动一次免费政变或调整阵营。"
  )
  addCardInfo("厨房辩论*", "若美国控制的战场国总数比苏联多，戳一下对手的胸口，然后获得2VP！")
  addCardInfo("导弹嫉妒",
    "将此牌与对手手牌里行动力最高的牌互换，若有多张，则由对手选择。\n" +
    "若这张牌是当前玩家的事件或双方事件，则立刻发生；若是对方事件，则不发生而使用其行动力进行行动。\n" +
    "对方下个行动轮必须以行动力方式使用掉“导弹嫉妒”。"
  )
  addCardInfo("“我们要埋葬你们”*",
    "除非下个行动轮美国作为事件打出“联合国干预”，否则苏联在美国打出牌生效前得3VP。\n" +
    "核战等级恶化1级。"
  )
  addCardInfo("勃列日涅夫主义*", "本回合苏联打出的所有行动牌行动力+1（上限为4）。")
  addCardInfo("葡萄牙帝国崩溃*", "在安哥拉和东南非洲诸国各增加2点苏联影响力。")
  addCardInfo("南非动乱", "苏联在南非增加2点影响力，或者在南非增加1点，其1个邻国增加2点影响力。")
  addCardInfo("智利总统阿连德*", "苏联在智利增加2点影响力。")
  addCardInfo("维利·勃兰特*",
    "苏联得1VP。\n" +
    "苏联在西德增加1点影响力。\n" +
    "取消“北大西洋公约组织”对西德的效果。\n" +
    "若是已打出“推倒柏林墙”，则此牌不可作事件打出。打出“推倒柏林墙”时，已打出的此牌效果无效。"
  )
  addCardInfo("穆斯林革命",
    "在苏丹，伊朗，伊拉克，埃及，利比亚，沙特阿拉伯，叙利亚，约旦之中的2个国家移除美国的所有影响力。\n" +
    "若是已打出“出售预警机给沙特[可选牌]”，则此牌不可作事件打出。"
  )
  addCardInfo("反弹道导弹条约", "核战等级改善1级。\n然后当前玩家视作打出一张4行动力的牌进行行动。")
  addCardInfo("文化大革命*", "若美国持有中国牌，则苏联获得之并正面朝上，为可使用的状态。\n若苏联持有中国牌，则苏联得1VP。")
  addCardInfo("花的力量*",
    "美国作为事件或行动打出一张战争牌时（太空竞赛不算），苏联得2VP。\n" +
    "战争牌有：朝鲜战争，阿以战争，印巴战争，局部战争，两伊战争。\n" +
    "若是已打出“‘邪恶帝国’”，则此牌不可作事件打出。打出“‘邪恶帝国’”时，已打出的此牌效果无效。"
  )
  addCardInfo("U2击坠事件*", "苏联得1VP。\n若本回合剩余行动轮内某方作为事件打出“联合国干预”，则苏联再得1VP。")
  addCardInfo("石油输出国组织",
    "苏联每控制一个以下国家得1VP：埃及，伊朗，利比亚，沙特阿拉伯，伊拉克，海湾国家，委内瑞拉。\n" +
    "若是已打出“北海石油”，则此牌不可作事件打出。"
  )
  addCardInfo("“孤胆枪手”*", "美国公开手牌。\n然后苏联视作打出一张1行动力的牌进行行动。")
  addCardInfo("殖民地后卫", "在任意4个非洲或东南亚国家各增加1点美国影响力。")
  addCardInfo("归还巴拿马运河*", "在巴拿马，哥斯达黎加，委内瑞拉各增加1点美国影响力。")
  addCardInfo("戴维营协定*", "美国得1VP。\n美国在以色列，约旦，埃及各增加1点影响力。\n不可再作为事件打出“阿以战争”。")
  addCardInfo("傀儡政府*", "美国可以在当前没有任何影响力的3个国家各放置1点影响力。")
  addCardInfo("向苏联出售谷物", "美国玩家从苏联玩家手牌中随机选一张牌，打出或归还。若无手牌或归还，则使用本牌行动力进行行动。")
  addCardInfo("约翰·保罗二世当选教皇*",
    "在波兰移除2点苏联影响力，增加1点美国影响力。\n" +
    "打出此牌后，“团结工会”可以被作为事件打出。"
  )
  addCardInfo("拉丁美洲敢死队", "本回合内，当前玩家在中南美的政变+1行动力，对方玩家的-1行动力。")
  addCardInfo("美洲国家组织成立*", "在中南美增加2点美国影响力。")
  addCardInfo("尼克松打出中国牌*", "若美国持有中国牌，则得2VP。\n若苏联持有中国牌，则美国获得之并正面朝下，不可在本回合使用。")
  addCardInfo("萨达特驱逐苏维埃*", "在埃及移除所有苏联影响力，增加1点美国影响力。")
  addCardInfo("穿梭外交",
    "打出在美国玩家面前。\n" +
    "在下个中东计分或亚洲计分时，苏联少计算1个战场国（由美国选择），然后将此牌放入弃牌堆。\n" +
    "此牌不影响终局计分。"
  )
  addCardInfo("美国之音", "从非欧洲国家移除4点苏联影响力，每个国家最多2点。")
  addCardInfo("解放神学", "在中美洲增加3点苏联影响力，每个国家最多2点。")
  addCardInfo("乌苏里江冲突*",
    "若苏联持有中国牌，则美国获得之并正面朝上，为可使用的状态。\n" +
    "若美国持有中国牌，则在亚洲放置4点美国影响力，每个国家最多2点。"
  )
  addCardInfo("“不要问国家能为你……”*",
    "美国玩家可以弃掉任意张牌（包括计分牌），然后从牌堆抽等量的牌。" +
    "必须先弃掉所有要弃的牌后才可抽牌。"
  )
  addCardInfo("进步同盟*", "美国每有一个控制的中南美战场国，得1VP。")
  addCardInfo("非洲计分",
    "存在: 1\n" +
    "支配: 4\n" +
    "控制: 6\n" +
    "每个控制的战场国+1VP\n" +
    "回合结束时不可持有此牌"
  )
  addCardInfo("“一小步……”", "如果当前玩家太空竞赛落后，则太空竞赛前进2格，只得第2格的VP。")
  addCardInfo("南美计分",
    "存在: 2\n" +
    "支配: 5\n" +
    "控制: 6\n" +
    "每个控制的战场国+1VP\n" +
    "回合结束时不可持有此牌"
  )
  addCardInfo("伊朗人质危机*",
    "在伊朗移除所有美国影响力，增加2点苏联影响力。\n" +
    "“恐怖主义”对美国的效果加倍。"
  )
  addCardInfo("铁娘子*",
    "美国得1VP\n" +
    "在阿根廷增加1点苏联影响力，移除英国的所有苏联影响力。\n" +
    "不可再作为事件打出“社会主义政府”。"
  )
  addCardInfo("里根轰炸利比亚*", "在利比亚每有2点苏联影响力美国，得1VP。")
  addCardInfo("星球大战*", "若美国在太空竞赛领先，则从弃牌堆选一张非计分牌，发动其事件。")
  addCardInfo("北海石油*", "“石油输出国组织”不再可作为事件打出。\n美国本回合可以进行8个行动轮。")
  addCardInfo("改革者*",
    "在欧洲增加4点苏联影响力（若苏联VP领先则是6点），每个国家最多2点。\n" +
    "苏联不能在欧洲发动政变。\n" +
    "增强“开放”的效果。"
  )
  addCardInfo("海军基地爆炸袭击*", "移除黎巴嫩的所有美国影响力。再从中东移除2点美国影响力。")
  addCardInfo("苏联击落大韩航空007号班机*",
    "核战等级恶化1级。\n" +
    "美国得2VP。\n" +
    "若美国控制韩国，则美国可以以4点行动力来放置影响力或调整阵营。"
  )
  addCardInfo("开放*",
    "苏联得2VP。\n" +
    "核战等级改善1级。\n" +
    "若“改革者”生效，则苏联可以以4点行动力来放置影响力或调整阵营。"
  )
  addCardInfo("奥尔特加当选尼加拉瓜总统*",
    "移除尼加拉瓜的所有美国影响力。" +
    "然后苏联可以使用本牌的行动力在尼加拉瓜的1个邻国进行免费政变。"
  )
  addCardInfo("恐怖主义", "对手须随机弃1张牌，若苏联打出此牌且“伊朗人质危机”生效，则美国要弃2张牌。\n弃牌上的事件不发动。")
  addCardInfo("伊朗门丑闻*", "本回合美国调整阵营掷骰点数-1。")
  addCardInfo("切尔诺贝利*", "美国玩家指定1个区域，本回合内苏联玩家不能在此区域进行增加影响力的行动。")
  addCardInfo("拉美债务危机", "美国玩家需弃一张行动力3及以上的牌，否则苏联在2个南美国家的影响力加倍。")
  addCardInfo("推倒柏林墙*",
    "在东德增加3点美国影响力。\n" +
    "然后美国可以使用本牌的行动力在欧洲进行免费政变或调整阵营。\n" +
    "不能作为事件打出“维利·勃兰特”，取消已打出“维利·勃兰特”的效果。"
  )
  addCardInfo("“邪恶帝国”*",
    "美国得1VP。\n" +
    "不能作为事件打出“花的力量”，取消已打出“花的力量”的效果。"
  )
  addCardInfo("奥德里奇·艾姆斯泄密*", "本回合美国公开手牌。\n苏联从美国手中选一张牌弃掉。")
  addCardInfo("部署潘兴II导弹*", "苏联得1VP。\n从西欧的最多3个国家各移除1点美国影响力。")
  addCardInfo("战争游戏*", "如果核战等级为2，当前玩家可以给对方6VP，之后不经过终局计分，直接结束游戏。")
  addCardInfo("团结工会*",
    "在打出“约翰·保罗二世当选教皇”后可作为事件打出。\n" +
    "在波兰增加3点美国影响力。"
  )
  addCardInfo("两伊战争*",
    "伊朗或伊拉克入侵另一方（由玩家选择）。掷骰子，减去对手控制的被入侵国邻国数，若得4~6，则当前玩家胜利。\n" +
    "当前玩家增加2军事行动。\n" +
    "胜利奖励：当前玩家得2VP，对手在被入侵国的影响力全部替换成当前玩家影响力。"
  )
  addCardInfo("叛逃者",
    "在头条打出此牌可以取消苏联的头条事件并将其放入弃牌堆。\n" +
    "若苏联在行动轮打出此牌（太空竞赛除外），则美国得1VP。"
  )
  addCardInfo("剑桥五杰",
    "美国玩家展示手中的所有计分牌，苏联在其中1张的对应区域内增加1点影响力。\n" +
    "此牌不能在后期作为事件打出。"
  )
  addCardInfo("美英特殊关系",
    "若美国控制英国且“北大西洋公约组织”未生效，美国在英国的1个邻国增加1点影响力。\n" +
    "若美国控制英国且“北大西洋公约组织”已生效，美国在西欧的1个国家增加2点影响力并得2VP。"
  )
  addCardInfo("北美防空司令部*",
    "若美国控制加拿大，在某个行动轮后核战等级变为2时，美国可在已有美国影响力的1个国家增加1点影响力。\n" +
    "此效果会被“困境”取消。"
  )
  addCardInfo("切·格瓦拉",
    "苏联可用此牌的行动力在中美洲，南美洲或非洲的1个非战场国进行政变。\n" +
    "若政变移除了美国影响力，则苏联可以在这些区域的另一个非战场国进行第二次政变。"
  )
  addCardInfo("我们的人在德黑兰*",
    "若美国控制至少1个中东国家，则美国玩家从牌堆抽5张牌，" +
    "选择其中任意张展示并弃入弃牌堆，不发动事件，剩余的牌放回牌堆，并洗牌。"
  )
  addCardInfo("尤里和萨曼莎*", "本回合剩下的行动轮内，每当美国发动政变，苏联就得1VP。")
  addCardInfo("出售预警机给沙特*", "美国在沙特阿拉伯增加2点影响力。\n不可作为事件打出“穆斯林革命”。")

  val flagInfo = new Array[(Map[Faction, String], String)](120)
  var flagInfoCount = 0
  def addFlagInfo(name: String, desc: String): Unit = {
    flagInfo(flagInfoCount) = (Map(Faction.US -> name, Faction.USSR -> name, Faction.Neutral -> name), desc)
    flagInfoCount += 1
  }
  def addFlagInfo(usName: String, ussrName: String, neutralName: String, desc: String): Unit = {
    flagInfo(flagInfoCount) = (Map(Faction.US -> usName, Faction.USSR -> ussrName, Faction.Neutral -> neutralName), desc)
    flagInfoCount += 1
  }

  addFlagInfo("太空竞赛限制", "%1$s本回合已尝试过1次太空竞赛，除非%1$s有“动物实验”标记，否则不能进行太空竞赛尝试。")
  addFlagInfo("太空竞赛限制", "%1$s本回合已尝试过2次太空竞赛，不能进行太空竞赛尝试。")
  addFlagInfo("动物实验", "%1$s每回合可以进行2次太空竞赛尝试。")
  addFlagInfo("常驻轨道", "%1$s的对手须先选择和展示头条牌。")
  addFlagInfo("登月计划", "%1$s可以弃掉1张持牌。")
  addFlagInfo("空间基地", "%1$s可以进行8个行动轮。")
  addFlagInfo("面朝下的中国牌", "%1$s刚从对手接收中国牌，本回合不能使用。")
  addFlagInfo("核战等级限制（欧洲）", "双方不能在欧洲调整阵营或发动政变。")
  addFlagInfo("核战等级限制（亚洲）", "双方不能在亚洲调整阵营或发动政变。")
  addFlagInfo("核战等级限制（中东）", "双方不能在中东调整阵营或发动政变。")
  addFlagInfo("越南起义", "在本回合内，苏联如果将行动力全和在东南亚，则可以行动力+1。")
  addFlagInfo("戴高乐领导法国", "取消“北大西洋公约组织”对法国的效果，苏联可在法国调整阵营或发动政变。")
  addFlagInfo("遏制政策", "勃列日涅夫主义", "遏制政策/勃列日涅夫主义", "本回合%s打出的所有行动牌行动力+1（上限为4）。")
  addFlagInfo("美日共同防卫协定", "苏联不能在日本进行政变或调整阵营。")
  addFlagInfo("红色恐慌", "党内清洗", "红色恐慌/党内清洗", "%s本回合打出的所有行动牌行动力-1（下限是1）。")
  addFlagInfo("台湾决议",
    "在亚洲计分或终局计分时，若美国控制台湾，则台湾被视作战场国，其他时刻和情况下不被视为战场国。\n" +
    "美国打出中国牌时此标记失效。")
  addFlagInfo("古巴导弹危机",
    "本回合%s的任何政变将引发核战，使其输掉游戏。\n" +
    "在任何时候，作为苏联移除古巴的2点影响力，或作为美国移除西德和土耳其之一的2点影响力，就可以使此标记失效。\n" +
    "点击此标记以移除影响力。")
  addFlagInfo("核潜艇", "本回合美国在战场国的政变将不会降低核战等级（不影响古巴导弹危机的效果）。")
  addFlagInfo("困境", "捕熊陷阱", "困境/捕熊陷阱",
    "在下个行动轮，%1$s玩家不能进行通常行动，而是必须弃一张2或以上行动力的牌，然后掷骰，" +
    "若是1~4则取消此牌效果，否则以后的行动轮也须重复以上行动。" +
    "如果此轮因无符合条件的牌可弃而没有弃牌，则%1$s玩家不能掷骰，在这轮只能打出计分牌。")
  addFlagInfo("“我们要埋葬你们”", "除非下个行动轮美国作为事件打出“联合国干预”，否则苏联在美国打出牌生效前得3VP。")
  addFlagInfo("维利·勃兰特",
    "取消“北大西洋公约组织”对西德的效果，苏联可在西德调整阵营或发动政变。\n" +
    "打出“推倒柏林墙”时，此标记失效。")
  addFlagInfo("花的力量",
    "美国作为事件或行动打出一张战争牌时（太空竞赛不算），苏联得2VP。\n" +
    "战争牌有：朝鲜战争，阿以战争，印巴战争，局部战争，两伊战争。\n" +
    "打出“‘邪恶帝国’”时，此标记失效。")
  addFlagInfo("U2击坠事件", "若某方作为事件打出“联合国干预”，则苏联得1VP，然后此标记失效。")
  addFlagInfo("戴维营协定", "不可作为事件打出“阿以战争”。")
  addFlagInfo("约翰·保罗二世当选教皇", "可作为事件打出“团结工会”。")
  addFlagInfo("穿梭外交",
      "在下个中东计分或亚洲计分时，苏联少计算1个战场国（由美国选择），然后此标记失效，“穿梭外交”牌进入弃牌堆。\n" +
      "此标记不影响终局计分。")
  addFlagInfo("伊朗人质危机", "“恐怖主义”对美国的效果加倍。")
  addFlagInfo("铁娘子", "不可作为事件打出“社会主义政府”。")
  addFlagInfo("北海石油", "不可作为事件打出“石油输出国组织”。")
  addFlagInfo("北海石油", "美国本回合可以进行8个行动轮。")
  addFlagInfo("伊朗门丑闻", "本回合美国调整阵营掷骰点数-1。")
  addFlagInfo("“邪恶帝国”", "不可作为事件打出“花的力量”。")
  addFlagInfo("华沙条约组织成立", "可作为事件打出“北大西洋公约组织”。")
  addFlagInfo("北大西洋公约组织", "苏联不能对美国在欧洲控制的国家进行政变，调整阵营或作为局部战争的目标。")
  addFlagInfo("马歇尔计划", "可作为事件打出“北大西洋公约组织”。")
  addFlagInfo("限制战略武器谈判", "双方政变的掷骰-1。")
  addFlagInfo("导弹嫉妒", "%s下个行动轮必须以行动力方式使用掉“导弹嫉妒”。")
  addFlagInfo("拉丁美洲敢死队", "%s在中南美的政变+1行动力。")
  addFlagInfo("拉丁美洲敢死队", "%s在中南美的政变-1行动力。")
  addFlagInfo("改革者", "苏联不能在欧洲发动政变。\n增强“开放”的效果。")
  addFlagInfo("切尔诺贝利", "苏联不能在%2$s进行增加影响力的行动。")
  addFlagInfo("推倒柏林墙", "不可作为事件打出“维利·勃兰特”。")
  addFlagInfo("奥德里奇·艾姆斯泄密", "美国公开手牌。")
  addFlagInfo("北美防空司令部",
      "若美国控制加拿大，在某个行动轮后核战等级变为2时，美国可在已有美国影响力的1个国家增加1点影响力。\n" +
      "打出“困境”时，此标记失效。")
  addFlagInfo("尤里和萨曼莎", "本回合剩下的行动轮内，每当美国发动政变，苏联就得1VP。")
  addFlagInfo("出售预警机给沙特", "不可作为事件打出“穆斯林革命”。")
  addFlagInfo("中国内战",
      "双方不持有“中国牌”。\n" +
      "“文化大革命”，“台湾决议”不可作为事件打出。\n" +
      "“红色恐慌/党内清洗”不可被苏联作为事件打出。\n" +
      "“乌苏里江冲突”，“尼克松打出中国牌”视作美国持有“中国牌”。\n" +
      "“朝鲜战争”掷骰得点-1。")

  val cardTips = mutable.Map.empty[Card, Array[String]]
  cardTips += Card007SocialistGovernments -> Array("请从西欧移除%s美国影响力")
  cardTips += Card010Blockade -> Array("请弃一张3以上行动力的牌")
  cardTips += Card014COMECON -> Array("请在东欧%s国各加1影响力")
  cardTips += Card016WarsawPact -> Array("是否要移除美国的影响力", "请选择%s个东欧国家", "请在东欧增加%s影响力")
  cardTips += Card019TrumanDoctrine -> Array("请移除欧洲%s国苏联影响力")
  cardTips += Card020OlympicGames -> Array("是否要参加奥运会")
  cardTips += Card022IndependentReds -> Array("请选择%s个独立的红色国家")
  cardTips += Card023MarshallPlan -> Array("请在西欧%s国各加1影响力")
  cardTips += Card024IndoPakistaniWar -> Array("请选择印巴战争被入侵国")
  cardTips += Card026CIACreated -> Array("请确认对方手牌")
  cardTips += Card028SuezCrisis -> Array("请移除%s美国影响力")
  cardTips += Card029EastEuropeanUnrest -> Array("请选择%s个东欧国家")
  cardTips += Card030Decolonization -> Array("请选择非洲或东南亚%s国")
  cardTips += Card032UNIntervention -> Array("请选择要打出的牌")
  cardTips += Card033DeStalinization -> Array("请先移除%s苏联影响力", "请放置%s苏联影响力")
  cardTips += Card036BrushWar -> Array("请选择被入侵国")
  cardTips += Card040CubaMissile -> Array("请选择去除影响力的国家")
  cardTips += Card043SALTNegotiations -> Array("请从弃牌中回收一张牌")
  cardTips += Card045Summit -> Array("请改变核战等级")
  cardTips += Card046HowILearnStopWorry -> Array("请改变核战等级")
  cardTips += Card047Junta -> Array("请选择%s个中南美国家", "", "请调整%1$s个国家（剩余%2$s）", "请政变%1$s个国家（行动力%2$s）")
  cardTips += Card049MissileEnvy -> Array("请选择一张交给对手的牌")
  cardTips += Card053SouthAfricaUnrest -> Array("请选择增加影响力的国家")
  cardTips += Card056MuslimRevolution -> Array("请选择%s个穆斯林革命国家")
  cardTips += Card062LoneGunman -> Array("请确认对方手牌")
  cardTips += Card063ColonialRearGuards -> Array("请选择非洲或东南亚%s国")
  cardTips += Card066PuppetGovernments -> Array("请选择%s个无影响力国")
  cardTips += Card067GrainSales -> Array("请选择打出或归还")
  cardTips += Card070OASFounded -> Array("请在中南美放置%s影响力")
  cardTips += Card074VoiceOfAmerica -> Array("请移除非欧洲%s苏联影响力")
  cardTips += Card075LiberationTheology -> Array("请在中美洲放置%s影响力")
  cardTips += Card076UssuriRiverSkirmish -> Array("请在亚洲放置%s影响力")
  cardTips += Card077AskNotWhatYourCountry -> Array("请弃掉任意张牌")
  cardTips += Card085StarWars -> Array("请从弃牌中选一张牌")
  cardTips += Card087Reformer -> Array("请在欧洲增加%s影响力", "请在欧洲增加%s影响力")
  cardTips += Card088BarracksBombing -> Array("请移除中东%s美国影响力")
  cardTips += Card089ShootDownKAL007 -> Array("", "请用%s行动力来增加影响力", "请调整%1$s个国家（剩余%2$s）")
  cardTips += Card090Glasnost -> Array("", "请用%s行动力来增加影响力", "请调整%1$s个国家（剩余%2$s）")
  cardTips += Card091OrtegaInNicaragua -> Array("请政变%1$s个国家（行动力%2$s）")
  cardTips += Card095LatinAmericaDebtCrisis -> Array("请弃一张3以上行动力的牌", "请选择%s个南美国家")
  cardTips += Card096TearDownThisWall -> Array("", "请调整%1$s个国家（剩余%2$s）", "请政变%1$s个国家（行动力%2$s）")
  cardTips += Card098AldrichAmes -> Array("请弃对手一张牌")
  cardTips += Card099PershingII -> Array("请移除西欧%s国1美国影响力")
  cardTips += Card102IranIraqWar -> Array("请选择两伊战争被入侵国")
  cardTips += Card104CambridgeFive -> Array("请选择一张计分牌", "请在相应区域增加%s影响力")
  cardTips += Card105SpecialRelationship -> Array("请选择英国的%s个邻国", "请选择%s个西欧国家")
  cardTips += Card107Che -> Array("请政变%1$s个国家（行动力%2$s）", "请政变%1$s个国家（行动力%2$s）")
  cardTips += Card108OurManInTehran -> Array("请弃掉任意张牌")

  val spaceInfo = new Array[(String, String)](11)
  spaceInfo(0) = ("遥望天空",
      "遥望天空 -> 人造卫星：2行动力，掷骰1-3"
    )
  spaceInfo(1) = ("人造卫星",
      "先手2VP，后手1VP。\n" +
      "遥望天空 -> 人造卫星：2行动力，掷骰1-3\n" +
      "人造卫星 -> 动物实验：2行动力，掷骰1-4"
    )
  spaceInfo(2) = ("动物实验",
      "每回合可以进行2次太空竞赛尝试。\n" +
      "人造卫星 -> 动物实验：2行动力，掷骰1-4\n" +
      "动物实验 -> 载人航天：2行动力，掷骰1-3"
    )
  spaceInfo(3) = ("人造卫星",
      "先手2VP。\n" +
      "动物实验 -> 载人航天：2行动力，掷骰1-3\n" +
      "载人航天 -> 常驻轨道：2行动力，掷骰1-4"
    )
  spaceInfo(4) = ("常驻轨道",
      "对手须先选择和展示头条牌。\n" +
      "载人航天 -> 常驻轨道：2行动力，掷骰1-4\n" +
      "常驻轨道 -> 探月计划：3行动力，掷骰1-3"
    )
  spaceInfo(5) = ("探月计划",
      "先手3VP，后手1VP。\n" +
      "常驻轨道 -> 探月计划：3行动力，掷骰1-3\n" +
      "探月计划 -> 登月计划：3行动力，掷骰1-4"
    )
  spaceInfo(6) = ("登月计划",
      "可以弃掉1张持牌。\n" +
      "探月计划 -> 登月计划：3行动力，掷骰1-4\n" +
      "登月计划 -> 航天飞机：3行动力，掷骰1-3"
    )
  spaceInfo(7) = ("航天飞机",
      "先手4VP，后手2VP。\n" +
      "登月计划 -> 航天飞机：3行动力，掷骰1-3\n" +
      "航天飞机 -> 空间基地：3行动力，掷骰1-2"
    )
  spaceInfo(8) = ("空间基地",
      "先手2VP。\n" +
      "可以进行8个行动轮。\n" +
      "航天飞机 -> 空间基地：3行动力，掷骰1-2"
    )

  def toString(flagData: Any): String = {
    flagData match {
      case null => null
      case region: Region => getRegionName(region)
      case _ => null
    }
  }

}
