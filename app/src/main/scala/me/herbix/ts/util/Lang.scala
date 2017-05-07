package me.herbix.ts.util

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic._
import me.herbix.ts.logic.card._
import me.herbix.ts.logic.turnzero.{TZFlags, CardTZ02NationalistChina}

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
  val nonbattlefieldCountry = "非战场国"
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
  addFlagInfo("越南起义", "在本回合内，苏联如果将行动力全部用在东南亚，则可以行动力+1。")
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

  addFlagInfo("扎伊尔总统蒙博托", "扎伊尔的稳定度变为3。")
  addFlagInfo("克里姆林流感", "下个行动轮必须要打出一张计分牌，如果没有计分牌，则跳过下个行动轮。")

  addFlagInfo("中国内战",
      "双方不持有“中国牌”。\n" +
      "“文化大革命”，“台湾决议”不可作为事件打出。\n" +
      "“红色恐慌/党内清洗”不可被苏联作为事件打出。\n" +
      "“乌苏里江冲突”，“尼克松打出中国牌”视作美国持有“中国牌”。\n" +
      "“朝鲜战争”掷骰得点-1。")
  addFlagInfo("斯大林目标欧洲", "苏联开局时增加1点在欧洲任意位置的影响力。")
  addFlagInfo("斯大林目标亚洲", "苏联开局时可以取“越南起义”或“阿以战争”加入手牌（开局手牌数不变）。")
  addFlagInfo("杜鲁门的意识", "美国开局时可以取“马歇尔计划”加入手牌（开局手牌数不变）。")
  addFlagInfo("强硬的杜鲁门", "美国可以在冷战早期先于苏联行动。")
  addFlagInfo("苏联席卷东欧", "苏联开局时在东欧增加2点影响力。")
  addFlagInfo("西欧盟军奋进", "“封锁”无效。")
  addFlagInfo("联合政府领导英国", "“社会主义政府”在第1~2回合无效。")
  addFlagInfo("保守党领导英国", "“苏伊士运河危机”无效。")
  addFlagInfo("以色列的危机", "苏联开局时在中东增加2点影响力（每个国家最多1点）.")
  addFlagInfo("美国速援蒋介石", "台湾成为战场国。")
  addFlagInfo("识局势的日本", "核战等级不可小于2，美国无视核战等级限制。")

  val cardTips = mutable.Map.empty[Int, Array[String]]
  cardTips += Card007SocialistGovernments.id -> Array("请从西欧移除%s美国影响力")
  cardTips += Card010Blockade.id -> Array("请弃一张3以上行动力的牌")
  cardTips += Card014COMECON.id -> Array("请在东欧%s国各加1影响力")
  cardTips += Card016WarsawPact.id -> Array("是否要移除美国的影响力", "请选择%s个东欧国家", "请在东欧增加%s影响力")
  cardTips += Card019TrumanDoctrine.id -> Array("请移除欧洲%s国苏联影响力")
  cardTips += Card020OlympicGames.id -> Array("是否要参加奥运会")
  cardTips += Card022IndependentReds.id -> Array("请选择%s个独立的红色国家")
  cardTips += Card023MarshallPlan.id -> Array("请在西欧%s国各加1影响力")
  cardTips += Card024IndoPakistaniWar.id -> Array("请选择印巴战争被入侵国")
  cardTips += Card026CIACreated.id -> Array("已经确认对方手牌")
  cardTips += Card028SuezCrisis.id -> Array("请移除%s美国影响力")
  cardTips += Card029EastEuropeanUnrest.id -> Array("请选择%s个东欧国家")
  cardTips += Card030Decolonization.id -> Array("请选择非洲或东南亚%s国")
  cardTips += Card032UNIntervention.id -> Array("请选择要打出的牌")
  cardTips += Card033DeStalinization.id -> Array("请先移除%s苏联影响力", "请放置%s苏联影响力")
  cardTips += Card036BrushWar.id -> Array("请选择被入侵国")
  cardTips += Card040CubaMissile.id -> Array("请选择去除影响力的国家")
  cardTips += Card043SALTNegotiations.id -> Array("请从弃牌中回收一张牌")
  cardTips += Card045Summit.id -> Array("请改变核战等级")
  cardTips += Card046HowILearnStopWorry.id -> Array("请改变核战等级")
  cardTips += Card047Junta.id -> Array("请选择%s个中南美国家", "", "请调整%1$s个国家（剩余%2$s）", "请政变%1$s个国家（行动力%2$s）")
  cardTips += Card049MissileEnvy.id -> Array("请选择一张交给对手的牌")
  cardTips += Card053SouthAfricaUnrest.id -> Array("请选择增加影响力的国家")
  cardTips += Card056MuslimRevolution.id -> Array("请选择%s个穆斯林革命国家")
  cardTips += Card062LoneGunman.id -> Array("已经确认对方手牌")
  cardTips += Card063ColonialRearGuards.id -> Array("请选择非洲或东南亚%s国")
  cardTips += Card066PuppetGovernments.id -> Array("请选择%s个无影响力国")
  cardTips += Card067GrainSales.id -> Array("请选择打出或归还")
  cardTips += Card070OASFounded.id -> Array("请在中南美放置%s影响力")
  cardTips += Card074VoiceOfAmerica.id -> Array("请移除非欧洲%s苏联影响力")
  cardTips += Card075LiberationTheology.id -> Array("请在中美洲放置%s影响力")
  cardTips += Card076UssuriRiverSkirmish.id -> Array("请在亚洲放置%s影响力")
  cardTips += Card077AskNotWhatYourCountry.id -> Array("请弃掉任意张牌")
  cardTips += Card085StarWars.id -> Array("请从弃牌中选一张牌")
  cardTips += Card087Reformer.id -> Array("请在欧洲增加%s影响力", "请在欧洲增加%s影响力")
  cardTips += Card088BarracksBombing.id -> Array("请移除中东%s美国影响力")
  cardTips += Card089ShootDownKAL007.id -> Array("", "请用%s行动力来增加影响力", "请调整%1$s个国家（剩余%2$s）")
  cardTips += Card090Glasnost.id -> Array("", "请用%s行动力来增加影响力", "请调整%1$s个国家（剩余%2$s）")
  cardTips += Card091OrtegaInNicaragua.id -> Array("请政变%1$s个国家（行动力%2$s）")
  cardTips += Card095LatinAmericaDebtCrisis.id -> Array("请弃一张3以上行动力的牌", "请选择%s个南美国家")
  cardTips += Card096TearDownThisWall.id -> Array("", "请调整%1$s个国家（剩余%2$s）", "请政变%1$s个国家（行动力%2$s）")
  cardTips += Card098AldrichAmes.id -> Array("请弃对手一张牌")
  cardTips += Card099PershingII.id -> Array("请移除西欧%s国1美国影响力")
  cardTips += Card102IranIraqWar.id -> Array("请选择两伊战争被入侵国")
  cardTips += Card104CambridgeFive.id -> Array("请选择一张计分牌", "请在相应区域增加%s影响力")
  cardTips += Card105SpecialRelationship.id -> Array("请选择英国的%s个邻国", "请选择%s个西欧国家")
  cardTips += Card107Che.id -> Array("请政变%1$s个国家（行动力%2$s）", "请政变%1$s个国家（行动力%2$s）")
  cardTips += Card108OurManInTehran.id -> Array("请弃掉任意张牌")
  cardTips += CardTZ02NationalistChina.id -> Array("请在亚洲放置%s影响力")
  cardTips += CardP01NonAlignMovement.id -> Array("请选择%s个国家移除影响力")
  cardTips += CardP04Stanislav.id -> Array("请改变核战等级")
  cardTips += CardP06FirstLightning.id -> Array("请选择要打出的牌")

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

  val turnZeroFlagInfo = mutable.Map[Int, String](
    1 -> "请在欧洲放置%s点影响力",
    2 -> "请选择要加入起始手牌的牌",
    3 -> "请选择要加入起始手牌的牌",
    5 -> "请在东欧放置%s点影响力",
    9 -> "请在中东放置%s点影响力"
  )

}
