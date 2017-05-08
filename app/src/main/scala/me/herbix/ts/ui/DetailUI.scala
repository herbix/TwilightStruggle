package me.herbix.ts.ui

import java.awt._
import javax.swing.JPanel

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.RegionState.RegionState
import me.herbix.ts.logic.Region.{Region, RegionState}
import me.herbix.ts.logic.SpaceLevel.SpaceLevel
import me.herbix.ts.logic._
import me.herbix.ts.logic.card.Card
import me.herbix.ts.logic.turnzero.{Crisis, GameTurnZero}
import me.herbix.ts.util.Resource._
import me.herbix.ts.util.{Lang, MapValue, Resource}

import scala.collection.immutable.List

/**
  * Created by Chaofan on 2016/6/17.
  */
class DetailUI(game: Game) extends JPanel {

  object DetailMode extends Enumeration {
    type DetailMode = Value
    val CountryMode, CardMode, FlagMode, SpaceMode, RegionMode, CrisisMode, EmptyMode = Value
  }

  import DetailMode._

  var mode = EmptyMode
  var country: Country = null
  var card: Card = null
  var flag: Flag = null
  var flagData: Any = null
  var flagFaction: Faction = US
  var space: SpaceLevel = null
  var region: Region = null
  var crisis: Crisis = null
  var crisisEffect: Int = 0

  def setCountry(country: Country): Unit = {
    mode = CountryMode
    this.country = country
    repaint()
  }

  def setCard(card: Card): Unit = {
    mode = CardMode
    this.card = card
    repaint()
  }

  def setFlag(faction: Faction, flag: Flag, flagData: Any): Unit = {
    mode = FlagMode
    this.flag = flag
    this.flagData = flagData
    this.flagFaction = faction
    repaint()
  }

  def setSpace(space: SpaceLevel): Unit = {
    mode = SpaceMode
    this.space = space
    repaint()
  }

  def setRegion(region: Region): Unit = {
    mode = RegionMode
    this.region = region
    repaint()
  }

  def setCrisis(crisis: Crisis, crisisEffect: Int): Unit = {
    mode = CrisisMode
    this.crisis = crisis
    this.crisisEffect = crisisEffect
    repaint()
  }

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    mode match {
      case DetailMode.CountryMode =>
        paintCountry(g2d)
      case DetailMode.CardMode =>
        paintCard(g2d)
      case DetailMode.FlagMode =>
        paintFlag(g2d)
      case DetailMode.SpaceMode =>
        paintSpace(g2d)
      case DetailMode.RegionMode =>
        paintRegion(g2d)
      case DetailMode.CrisisMode =>
        paintCrisis(g2d)
      case _ =>
    }
  }

  val stroke1 = new BasicStroke(2)
  val stroke2 = new BasicStroke(2, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10, Array(4, 3), 0)
  val stroke3 = new BasicStroke(1)
  val countryTriangleX = Array(0, 180, 180)
  val countryTriangleY = Array(120, 30, 120)

  def paintCountry(g: Graphics2D): Unit = {
    country match {
      case game.theWorldMap.countryChina => paintChina(g)
      case _ => paintOtherCountry(g)
    }
  }

  def paintChina(g: Graphics2D): Unit = {
    g.translate(10, 20)
    g.scale(1 / 1.35, 1 / 1.35)

    g.drawImage(Resource.chineseCivilWarBg, 0, 0, 243, 182, null)

    g.setStroke(stroke1)
    g.setColor(Color.BLACK)
    g.drawRect(0, 0, 243, 182)

    g.setColor(Resource.countryCriticalTextColor)
    g.fillRect(202, 11, 30, 30)
    g.setColor(Color.BLACK)
    g.drawRect(202, 11, 30, 30)

    g.setColor(Resource.regionColor(Region.Asia))
    g.fillRect(11, 11, 90, 90)
    g.setColor(Color.BLACK)
    g.drawRect(11, 11, 90, 90)

    g.setStroke(stroke3)
    g.setFont(Resource.textFont2)
    g.setColor(Color.WHITE)
    val fm1 = g.getFontMetrics
    val str1 = Lang.chineseCivilWar
    val w1 = fm1.stringWidth(str1)
    g.drawString(str1, (243-w1)/2, 175)

    g.setFont(Resource.countryStabilityFont)
    g.setColor(Resource.countryCriticalStabilityColor)
    val fm2 = g.getFontMetrics
    val str2 = country.stability.toString
    val w2 = fm2.stringWidth(str2)
    g.drawString(str2, 201+(30-w2)/2, 36)

    g.setFont(Resource.countryInfluenceFont)
    val fm = g.getFontMetrics

    val ussrInfluence = game.influence(country, USSR)
    val ussrDrawColor = ussrColor
    val ussrBgColor = Color.WHITE

    if (ussrInfluence > 0) {
      if (ussrInfluence >= country.stability) {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrDrawColor, ussrBgColor, 15, 15)
      } else {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrBgColor, ussrDrawColor, 15, 15)
      }
    }
  }

  def paintOtherCountry(g: Graphics2D): Unit = {
    val colorTitle = if (country.isBattlefield) Resource.countryCriticalTitleColor else Resource.countryTitleColor
    val colorStability = if (country.isBattlefield) Resource.countryCriticalStabilityColor else Resource.countryStabilityColor
    val colorText = if (country.isBattlefield) Resource.countryCriticalTextColor else Resource.countryTextColor
    val (color1, color2) = {
      val colorRegions = country.regions.filter(Resource.regionColor.contains)
      if (colorRegions.size == 1) {
        (Resource.regionColor(colorRegions.head), null)
      } else if (colorRegions(Region.Asia)){
        (Resource.regionColor(Region.Asia), Resource.regionColor(Region.SouthEastAsia))
      } else if (colorRegions(Region.WestEurope)) {
        (Resource.regionColor(Region.WestEurope), Resource.regionColor(Region.EastEurope))
      } else {
        (null, null)
      }
    }

    g.translate(10, 20)

    g.setColor(colorTitle)
    g.fillRect(0, 0, 150, 30)
    g.setColor(colorStability)
    g.fillRect(150, 0, 30, 30)
    g.setColor(color1)
    g.fillRect(0, 30, 180, 90)

    if (color2 != null) {
      g.setColor(color2)
      g.fillPolygon(countryTriangleX, countryTriangleY, 3)
    }

    g.setStroke(stroke1)
    g.setColor(Color.BLACK)
    g.drawRect(0, 0, 180, 120)
    g.drawLine(0, 30, 180, 30)
    g.drawLine(150, 0, 150, 30)

    g.setStroke(stroke2)
    g.drawLine(90, 30, 90, 120)

    g.setStroke(stroke3)
    g.setFont(Resource.textFont2)
    g.setColor(colorText)
    val fm1 = g.getFontMetrics
    val str1 = Lang.countryNames(country.name)
    val w1 = fm1.stringWidth(str1)

    if (w1 > 145) {
      val trans = g.getTransform
      g.scale(145.0 / w1, 1)
      g.drawString(str1, 2.5f*w1/145, 25)
      g.setTransform(trans)
    } else {
      g.drawString(str1, (150-w1)/2, 25)
    }

    g.setFont(Resource.countryStabilityFont)
    val fm2 = g.getFontMetrics
    val str2 = country.stability.toString
    val w2 = fm2.stringWidth(str2)
    g.drawString(str2, 150+(30-w2)/2, 25)

    g.setFont(Resource.countryInfluenceFont)
    val fm = g.getFontMetrics

    val usInfluence = game.influence(country, US)
    val ussrInfluence = game.influence(country, USSR)
    val usDrawColor = usColor
    val usBgColor = Color.WHITE
    val ussrDrawColor = ussrColor
    val ussrBgColor = Color.WHITE

    if (usInfluence > 0) {
      if (usInfluence - ussrInfluence >= country.stability) {
        drawInfluenceToken(g, fm, usInfluence.toString, usDrawColor, usBgColor, 4, 34)
      } else {
        drawInfluenceToken(g, fm, usInfluence.toString, usBgColor, usDrawColor, 4, 34)
      }
    }
    if (ussrInfluence > 0) {
      if (ussrInfluence - usInfluence >= country.stability) {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrDrawColor, ussrBgColor, 94, 34)
      } else {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrBgColor, ussrDrawColor, 94, 34)
      }
    }
  }

  def drawInfluenceToken(g: Graphics2D, fm: FontMetrics, str: String, bgColor: Color, color: Color, x: Int, y: Int): Unit = {
    g.setColor(bgColor)
    g.fillRoundRect(x, y, 82, 82, 20, 20)
    g.setColor(Color.BLACK)
    g.drawRoundRect(x, y, 82, 82, 20, 20)
    g.setColor(color)
    g.drawString(str, x + (82 - fm.stringWidth(str)) / 2, y + 64)
  }

  val cardStarX = new Array[Int](10)
  val cardStarY = new Array[Int](10)
  val cardStarR = 20.0
  val cardStarR2 = cardStarR * Math.sin(Math.PI / 10) / Math.sin(Math.PI * 126 / 180)

  for (i <- 0 until 5) {
    cardStarX(i*2) = (-Math.sin(i * Math.PI * 2 / 5 - Math.PI / 5) * cardStarR2).toInt
    cardStarY(i*2) = -(Math.cos(i * Math.PI * 2 / 5 - Math.PI / 5) * cardStarR2).toInt
    cardStarX(i*2+1) = (-Math.sin(i * Math.PI * 2 / 5) * cardStarR).toInt
    cardStarY(i*2+1) = -(Math.cos(i * Math.PI * 2 / 5) * cardStarR).toInt
  }

  def paintCard(g: Graphics2D): Unit = {
    val period = game.theCards.getCardPeriod(card)
    val titleColor = period match {
      case 1 => Resource.cardTitleEarlyWar
      case 2 => Resource.cardTitleMidWar
      case 3 => Resource.cardTitleLateWar
      case _ => Resource.cardTitleSpecial
    }
    val titleText = period match {
      case 1 => Lang.earlyWar
      case 2 => Lang.midWar
      case 3 => Lang.lateWar
      case _ => Lang.special
    }

    g.translate(10, 20)

    paintTitle(g, titleColor, titleText, card.faction, card.op)

    val name = Lang.cardInfo(card.id)._1
    val desc = Lang.cardInfo(card.id)._2

    paintName(g, name)
    paintDesc(g, desc)
  }

  def paintTitle(g: Graphics2D, titleColor: Color, titleText: String, faction: Faction, operationPoint: Int): Unit = {
    g.setColor(titleColor)
    g.fillRect(0, 0, 180, 17)
    g.setColor(Color.BLACK)
    g.drawRect(0, 0, 180, 17)

    g.setFont(Resource.cardTitleFont)
    g.setColor(Color.WHITE)
    val fm = g.getFontMetrics
    g.drawString(titleText, 38 + (142 - fm.stringWidth(titleText)) / 2, 15)

    val trans = g.getTransform
    g.translate(19, 9)
    g.setColor(Color.BLACK)
    g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
    g.translate(-1, 0)
    faction match {
      case Faction.US =>
        g.setColor(Resource.cardStarUS2)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
        g.setColor(Resource.cardStarUS1)
        g.drawPolygon(cardStarX, cardStarY, cardStarX.length)
      case Faction.USSR =>
        g.setColor(Resource.cardStarUSSR2)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
        g.setColor(Resource.cardStarUSSR1)
        g.drawPolygon(cardStarX, cardStarY, cardStarX.length)
      case Faction.Neutral =>
        g.setColor(Resource.cardStarNeutral2)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
        g.setColor(Resource.cardStarNeutral1)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length / 2 + 1)
    }
    if (operationPoint > 0) {
      g.setFont(Resource.cardOpFont)
      g.setColor(if (faction == USSR) Color.WHITE else Color.BLACK)
      val str = operationPoint.toString
      val fm = g.getFontMetrics
      g.drawString(str, -fm.stringWidth(str) / 2, 7)
    }
    g.setTransform(trans)
  }

  def paintName(g: Graphics2D, name: String): Unit = {
    val trans = g.getTransform
    g.setColor(Color.BLACK)
    g.setFont(Resource.cardNameFont)
    val fm = g.getFontMetrics
    g.translate(90, 0)
    val strWidth = fm.stringWidth(name)
    if (strWidth > 180) {
      g.scale(180.0 / strWidth, 1)
    }
    g.drawString(name, -strWidth / 2, 50)
    g.setTransform(trans)
  }

  def paintDesc(g: Graphics2D, desc: String, startRow: Int = 78, width: Int = 180, breakLine: Int = 19, newLine: Int = 25): Unit = {
    g.setFont(Resource.cardDescFont)
    g.setColor(Color.black)

    val fm = g.getFontMetrics

    val lines = desc.split("\n")
    var currentRow = startRow
    var currentCol = 0
    val ca = Array[Char](0)

    for (line <- lines) {
      val toDraw = "  " + line
      for (i <- 0 until toDraw.length) {
        val c = toDraw.charAt(i)
        val cw = fm.charWidth(c)
        if (currentCol + cw > width && c != '，' && c != '。') {
          currentRow += breakLine
          currentCol = 0
        }
        ca(0) = c
        if (c == '~') {
          g.drawChars(ca, 0, 1, currentCol, currentRow + fm.getHeight / 2 - 2)
        } else {
          g.drawChars(ca, 0, 1, currentCol, currentRow)
        }
        currentCol += cw
      }
      currentRow += newLine
      currentCol = 0
    }
  }

  def paintFlag(g: Graphics2D): Unit = {
    val titleColor = if (flag.isGoodFlag) Resource.cardTitleEarlyWar else Resource.cardTitleLateWar
    val titleText = if (flag.isGoodFlag) Lang.goodFlag else Lang.badFlag

    g.translate(10, 20)

    paintTitle(g, titleColor, titleText, flagFaction, 0)

    val name = Lang.flagInfo(flag.id)._1(flagFaction)
    val desc =
      (if (flagFaction == Neutral)
        Lang.flagInfo(flag.id)._2
      else
        String.format(Lang.flagInfo(flag.id)._2, Lang.getFactionName(flagFaction), Lang.toString(flagData))
      ) + (flag.flagType match {
        case FlagType.ThisTurn => "\n" + Lang.thisTurnFlag
        case FlagType.DuringSetup => "\n" + Lang.duringSetupFlag
        case _ => ""
      })


    paintName(g, name)
    paintDesc(g, desc)
  }

  def paintSpace(g: Graphics2D): Unit = {
    val level = space.level

    val transform = g.getTransform
    g.translate((getWidth - MapValue.spaceSize._1) / 2, 10)

    val x = if (level == 0) {
      MapValue.space0._1
    } else {
      MapValue.space0._1 + (MapValue.space8._1 - MapValue.space0._1) / 8 * level + 3
    }
    val y = MapValue.space0._2
    g.drawImage(worldMap, 0, 0, MapValue.spaceSize._1, MapValue.spaceSize._2, x, y, x + MapValue.spaceSize._1, y + MapValue.spaceSize._2, null)

    g.setTransform(transform)

    g.translate(10, 20 + MapValue.spaceSize._2 - 30)

    val name = Lang.spaceInfo(level)._1
    val desc = Lang.spaceInfo(level)._2

    paintName(g, name)
    paintDesc(g, desc)
  }

  val stroke4 = new BasicStroke(2, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10, Array(1, 4), 0)
  val stroke5 = new BasicStroke(1, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10, Array(4, 4), 0)

  val regionTriangleX = Array(0, 180, 180)
  val regionTriangleY = Array(34, 0, 34)

  def paintRegion(g: Graphics2D): Unit = {
    val (color1, color2) = region match {
      case Region.Europe =>
        (Resource.regionColor(Region.WestEurope), Resource.regionColor(Region.EastEurope))
      case Region.SouthEastAsia =>
        (Resource.regionColor(Region.Asia), Resource.regionColor(Region.SouthEastAsia))
      case r =>
        (Resource.regionColor(r), null)
    }

    g.translate(10, 20)

    g.setColor(Resource.regionContentColor)
    g.fillRect(0, 0, 180, 125)
    g.setColor(color1)
    g.fillRect(0, 0, 180, 34)

    if (color2 != null) {
      g.setColor(color2)
      g.fillPolygon(regionTriangleX, regionTriangleY, 3)
    }

    g.setStroke(stroke1)
    g.setColor(Color.BLACK)
    g.drawRect(0, 0, 180, 125)
    g.drawLine(0, 34, 180, 34)

    g.setStroke(stroke3)
    g.setFont(Resource.textFont2)
    g.setColor(Color.BLACK)
    val fm1 = g.getFontMetrics
    val str1 = Lang.getRegionName(region)
    val w1 = fm1.stringWidth(str1)

    g.drawString(str1, (180-w1) / 2, 27)

    if (region != Region.SouthEastAsia) {
      val info = if (region == Region.Europe && game.isInstanceOf[GameTurnZero] && game.asInstanceOf[GameTurnZero].isEuropeAlliedBerlin) {
        game.asInstanceOf[GameTurnZero].EuropeAlliedBerlinInfo
      } else {
        Region.ScoringInfo(region)
      }

      paintRegionInfo(g, Lang.presence, info._1.toString, Resource.regionContentFont, Resource.regionContentFont2, 59)
      paintRegionInfo(g, Lang.domination, info._2.toString, Resource.regionContentFont, Resource.regionContentFont2, 86)

      if (info._3 > 200) {
        paintRegionInfo(g, Lang.control, Lang.success, Resource.regionContentFont, Resource.regionContentFont, 113)
      } else {
        paintRegionInfo(g, Lang.control, info._3.toString, Resource.regionContentFont, Resource.regionContentFont2, 113)
      }

      paintRegionInfoDetail(g, info)

    } else {
      g.translate(10, 0)
      paintDesc(g, Lang.southEastAsiaScore, 56, 160, 17, 21)
      g.translate(-10, 0)

      paintSEAsiaRegionInfoDetail(g)
    }

    g.setStroke(stroke5)
    g.setColor(Color.GRAY)
    g.drawLine(90, 135, 90, 270)
    g.drawLine(0, 248, 180, 248)
  }

  def paintRegionInfo(g: Graphics2D, name: String, value: String, font1: Font, font2: Font, y: Int): Unit = {
    g.setFont(font1)
    val fm3 = g.getFontMetrics
    val w1 = fm3.stringWidth(name)
    g.drawString(name, 10, y)

    g.setFont(font2)
    val fm2 = g.getFontMetrics

    g.setStroke(stroke4)
    val str1 = value
    val r1 = fm2.stringWidth(str1)
    g.drawLine(w1 + 20, y - 2, 160 - r1, y - 2)
    g.drawString(str1, 170 - r1, y)
  }

  def paintRegionInfoDetail(g: Graphics2D, info: (Int, Int, Int)) = {
    val presence = info._1
    val domination = info._2
    val control = info._3

    val targetCountries = game.theWorldMap.regionCountries(region) - game.theWorldMap.countryChina
    val battlefieldCount = targetCountries.count(_.isBattlefield)

    var usBattlefieldCountries = targetCountries.filter(country => country.isBattlefield && game.getController(country) == US)
    val ussrBattlefieldCountries = targetCountries.filter(country => country.isBattlefield && game.getController(country) == USSR)

    var usBattlefield = usBattlefieldCountries.size
    val usNonBattlefield = targetCountries.count(country => !country.isBattlefield && game.getController(country) == US)
    var ussrBattlefield = ussrBattlefieldCountries.size
    val ussrNonBattlefield = targetCountries.count(country => !country.isBattlefield && game.getController(country) == USSR)
    val usAll = usBattlefield + usNonBattlefield
    val ussrAll = ussrBattlefield + ussrNonBattlefield

    var shuttleDiplomacyCount = 0

    val taiwan = game.theWorldMap.countries("Taiwan")
    if (taiwan.regions(region) && game.getController(taiwan) == US && game.flags.hasFlag(US, Flags.Taiwan)) {
      usBattlefield += 1
      usBattlefieldCountries += taiwan
    }

    val useShuttleDiplomacy = game.flags.hasFlag(Flags.ShuttleDiplomacy) && (region == Region.Asia || region == Region.MidEast) && ussrBattlefield > 0
    if (useShuttleDiplomacy) {
      ussrBattlefield -= 1
      shuttleDiplomacyCount = -1
    }

    val usPresence = usBattlefield > 0 || usNonBattlefield > 0
    val ussrPresence = ussrBattlefield > 0 || ussrNonBattlefield > 0
    val usDomination = usBattlefield > ussrBattlefield && usAll > ussrAll && usNonBattlefield > 0
    val ussrDomination = ussrBattlefield > usBattlefield && ussrAll > usAll && ussrNonBattlefield > 0
    val usControl = usBattlefield == battlefieldCount && usAll > ussrAll
    val ussrControl = ussrBattlefield == battlefieldCount && ussrAll > usAll

    var usVp = if (usControl) control else if (usDomination) domination else if (usPresence) presence else 0
    var ussrVp = if (ussrControl) control else if (ussrDomination) domination else if (ussrPresence) presence else 0

    usVp += usBattlefield
    ussrVp += ussrBattlefield

    val usNearOpponent = targetCountries.filter(country => game.getController(country) == US && country.adjacentCountries(game.theWorldMap.countryUSSR))
    val ussrNearOpponent = targetCountries.filter(country => game.getController(country) == USSR && country.adjacentCountries(game.theWorldMap.countryUS))

    usVp += usNearOpponent.size
    ussrVp += ussrNearOpponent.size

    if (useShuttleDiplomacy) {
      val battlefieldAndNear = targetCountries.find(country => {
        game.getController(country) == USSR && country.adjacentCountries(game.theWorldMap.countryUS) && country.isBattlefield
      })
      if (battlefieldAndNear.nonEmpty) {
        ussrVp -= 1
        shuttleDiplomacyCount = -2
      }
    }

    paintRegionInfoByFaction(g, info,
      if (usControl) RegionState.Control
      else if (usDomination) RegionState.Domination
      else if (usPresence) RegionState.Presence
      else RegionState.Nop,
      usBattlefieldCountries,
      usNearOpponent,
      0,
      usVp,
      0,
      Resource.usColor
    )

    paintRegionInfoByFaction(g, info,
      if (ussrControl) RegionState.Control
      else if (ussrDomination) RegionState.Domination
      else if (ussrPresence) RegionState.Presence
      else RegionState.Nop,
      ussrBattlefieldCountries,
      ussrNearOpponent,
      shuttleDiplomacyCount,
      ussrVp,
      95,
      Resource.ussrColor
    )
  }

  def paintRegionInfoByFaction(g: Graphics2D, info: (Int, Int, Int),
                               state: RegionState, battlefieldCountries: Set[Country],
                               nearOpponentCountries: Set[Country], shuttleDiplomacyCount: Int,
                               vp: Int, x: Int, color: Color): Unit = {
    val presence = info._1
    val domination = info._2
    val control = info._3

    var paintList = List.empty[(String, Int)]
    state match {
      case RegionState.Presence => paintList :+= Lang.presence -> presence
      case RegionState.Domination => paintList :+= Lang.domination -> domination
      case RegionState.Control => paintList :+= Lang.control -> control
      case RegionState.Nop =>
    }

    var byCountriesList = List.empty[(String, Int)]
    for (country <- battlefieldCountries) {
      if (nearOpponentCountries.contains(country)) {
        byCountriesList :+= Lang.countryNames(country.name) -> 2
      } else {
        byCountriesList :+= Lang.countryNames(country.name) -> 1
      }
    }
    for (country <- nearOpponentCountries) {
      if (!battlefieldCountries.contains(country)) {
        byCountriesList :+= Lang.countryNames(country.name) -> 1
      }
    }
    if (shuttleDiplomacyCount < 0) {
      byCountriesList :+= Lang.cardInfo(73)._1 -> shuttleDiplomacyCount
    }

    if (paintList.size + byCountriesList.size <= 6) {
      paintScoreList(g, paintList ++ byCountriesList :+ (Lang.sum -> vp), x, color)
      return
    }

    paintList :+= Lang.battlefieldCountry -> battlefieldCountries.size

    if (nearOpponentCountries.nonEmpty) {
      paintList :+= Lang.opponentNear -> nearOpponentCountries.size
    }

    if (shuttleDiplomacyCount != 0) {
      paintList :+= Lang.cardInfo(73)._1 -> shuttleDiplomacyCount
    }

    paintScoreList(g, paintList :+ (Lang.sum -> vp), x, color)
  }

  def paintSEAsiaRegionInfoDetail(g: Graphics2D): Unit = {
    val targetCountries = game.theWorldMap.regionCountries(region)
    val usCountries = targetCountries.filter(game.getController(_) == US)
    val ussrCountries = targetCountries.filter(game.getController(_) == USSR)

    paintSEAsiaRegionInfoByFaction(g, usCountries, usCountries.size + usCountries.count(_.isBattlefield), 0, Resource.usColor)
    paintSEAsiaRegionInfoByFaction(g, ussrCountries, ussrCountries.size + ussrCountries.count(_.isBattlefield), 95, Resource.ussrColor)
  }

  def paintSEAsiaRegionInfoByFaction(g: Graphics2D, countries: Set[Country], vp: Int, x: Int, color: Color): Unit = {
    if (countries.size <= 6) {
      paintScoreList(g,
        countries.map(c => if (c.isBattlefield) Lang.countryNames(c.name) -> 2 else Lang.countryNames(c.name) -> 1).toList :+ (Lang.sum -> vp),
        x, color)
    } else {
      paintScoreList(g,
        List(Lang.battlefieldCountry -> countries.count(_.isBattlefield) * 2, Lang.nonBattlefieldCountry -> countries.count(!_.isBattlefield), Lang.sum -> vp),
        x, color)
    }
  }

  def paintScoreList(g: Graphics2D, paintList: List[(String, Int)], x: Int, color: Color): Unit = {
    g.setFont(Resource.cardDescFont)

    val fm = g.getFontMetrics

    var y = 150

    for ((item, value) <- paintList.dropRight(1)) {
      g.setColor(Color.black)
      g.drawString(item, x, y)
      g.setColor(color)
      var v = if (value < 200) value.toString else Lang.success
      if (v.length > 5) {
        v = v.substring(0, 5) + "..."
      }
      g.drawString(v, x + 85 - fm.stringWidth(v), y)
      y += 18
    }

    val (item, value) = paintList.last
    y = 265
    g.setColor(Color.black)
    g.drawString(item, x, y)
    g.setColor(color)
    var v = if (value < 200) value.toString else Lang.success
    if (v.length > 5) {
      v = v.substring(0, 5) + "..."
    }
    g.drawString(v, x + 85 - fm.stringWidth(v), y)
  }

  def paintCrisis(g: Graphics2D): Unit = {
    val titleColor = Resource.cardTitleLateWar
    val titleText = Lang.crisisCard

    g.translate(10, 20)

    paintTitle(g, titleColor, titleText, Neutral, 0)

    val info = Lang.crisisInfo(crisis.id)(crisisEffect)
    val name = info._1
    val desc = (crisisEffect match {
      case 0 => "1:\n"
      case 1 => "2~3:\n"
      case 2 => "4~5:\n"
      case 3 => "6:\n"
      case 4 => ""
    }) + info._2

    paintName(g, name)
    paintDesc(g, desc)
  }

}
