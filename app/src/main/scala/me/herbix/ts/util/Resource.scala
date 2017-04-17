package me.herbix.ts.util

import java.awt.{Color, Font, Image}
import javax.imageio.ImageIO
import javax.swing.ImageIcon

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.chinesecivilwar.CCWFlags
import me.herbix.ts.logic.turnzero.{TZCards, TZFlags}
import me.herbix.ts.logic.{Faction, Flags, FlagsTrait}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
object Resource {
  val worldMap = ImageIO.read(getClass.getResourceAsStream("/worldmap.jpg"))
  val card = (0 to 110)
    .map(i => (i, ImageIO.read(getClass.getResourceAsStream(f"/cards/$i%03d.png"))))
    .toMap ++
      (1 to 12)
    .map(i => (i + TZCards.IdInc, ImageIO.read(getClass.getResourceAsStream(f"/turnzero/cards/$i%02d.png"))))
    .toMap
  val cardBg = card.map(pair => {
    pair._1 match {
      case 78 => (78, ImageIO.read(getClass.getResourceAsStream("/cards/078_bg.png")))
      case 95 => (95, ImageIO.read(getClass.getResourceAsStream("/cards/095_bg.png")))
      case _ => pair
    }
  })

  Flags.init()
  CCWFlags.init()
  TZFlags.init()
  val flag = (0 to FlagsTrait.flagId).map(i => {
    val (dir, id) =
      if (i > 46) {
        ("turnzero/flags", i - 47)
      } else {
        ("flags", i)
      }
    val in = getClass.getResourceAsStream(f"/$dir/$id%02d.png")
    if (in != null) {
      val img = ImageIO.read(in)
      (i, Map(US -> img, USSR -> img, Neutral -> img))
    } else {
      val inA = getClass.getResourceAsStream(f"/$dir/$id%02dA.png")
      val inS = getClass.getResourceAsStream(f"/$dir/$id%02dS.png")
      (i, Map(US -> ImageIO.read(inA), USSR -> ImageIO.read(inS)))
    }
  }).toMap

  val crisis = (1 to 6).map(i => {
    ImageIO.read(getClass.getResourceAsStream(f"/turnzero/crisis/crisis$i%d.png"))
  }).toArray

  val crisisBack = ImageIO.read(getClass.getResourceAsStream("/turnzero/crisis/crisis-back.png"))

  val nuclearIcon = new ImageIcon(Resource.flag(Flags.Defcon2Penalty.id)(Faction.Neutral).getScaledInstance(25, 25, Image.SCALE_SMOOTH))

  val tokenSpaceUs = ImageIO.read(getClass.getResourceAsStream("/tokens/spaceus.png"))
  val tokenSpaceUssr = ImageIO.read(getClass.getResourceAsStream("/tokens/spaceussr.png"))
  val tokenDefcon = ImageIO.read(getClass.getResourceAsStream("/tokens/defcon.png"))
  val tokenMilitaryUs = ImageIO.read(getClass.getResourceAsStream("/tokens/militaryus.png"))
  val tokenMilitaryUssr = ImageIO.read(getClass.getResourceAsStream("/tokens/militaryussr.png"))
  val tokenActionUs = ImageIO.read(getClass.getResourceAsStream("/tokens/actionus.png"))
  val tokenActionUssr = ImageIO.read(getClass.getResourceAsStream("/tokens/actionussr.png"))

  val buttonClose = ImageIO.read(getClass.getResourceAsStream("/other/close.png"))
  val buttonCloseHover = ImageIO.read(getClass.getResourceAsStream("/other/close-hover.png"))

  val chineseCivilWarBg = ImageIO.read(getClass.getResourceAsStream("/other/ccw-bg.png"))

  val battlefieldTaiwan = ImageIO.read(getClass.getResourceAsStream("/turnzero/other/battlefield-taiwan.png"))

  val textFont = new Font(Lang.heiti, 0, 16)
  val textFont2 = new Font(Lang.lishu, 0, 32)
  val textColor = Color.DARK_GRAY
  val usColor = Color.BLUE
  val usColorInfluenceChange = new Color(128, 128, 255)
  val usColorUI = new Color(230, 230, 255)
  val ussrColor = Color.RED
  val ussrColorInfluenceChange = new Color(255, 128, 128)
  val ussrColorUI = new Color(255, 230, 230)

  val influenceTokenTextFont = new Font(Lang.heiti, Font.BOLD, 32)
  val tokenColor = Color.YELLOW
  val tokenSize = 46

  val crisisWidth = 500
  val crisisHeight = crisisWidth * crisisBack.getHeight / crisisBack.getWidth

  import me.herbix.ts.logic.Region._

  val regionColor = mutable.Map[Region, Color]()
  regionColor += MidEast -> new Color(0xffe8e6f1)
  regionColor += Asia -> new Color(0xfff79740)
  regionColor += Africa -> new Color(0xffffebc8)
  regionColor += MidAmerica -> new Color(0xfff0d9b7)
  regionColor += SouthAmerica -> new Color(0xffc7b793)
  regionColor += WestEurope -> new Color(0xffb38ca9)
  regionColor += EastEurope -> new Color(0xffccb3c8)
  regionColor += SouthEastAsia -> new Color(0xffffc652)

  val countryTitleColor = new Color(0xfffdf5e0)
  val countryStabilityColor = new Color(0xfffbe68d)
  val countryTextColor = Color.BLACK
  val countryCriticalTitleColor = new Color(0xff734977)
  val countryCriticalStabilityColor = new Color(0xffda1124)
  val countryCriticalTextColor = Color.WHITE

  val countryStabilityFont = new Font("Arial", Font.BOLD, 28)
  val countryInfluenceFont = new Font(Lang.heiti, 0, 64)

  val cardTitleEarlyWar = new Color(0xff01abce)
  val cardTitleMidWar = new Color(0xff0088aa)
  val cardTitleLateWar = new Color(0xff282f42)
  val cardTitleSpecial = new Color(0xff282f42)

  val cardStarNeutral1 = Color.WHITE
  val cardStarNeutral2 = new Color(0xfff04b47)

  val cardStarUS1 = new Color(0xff0978bd)
  val cardStarUS2 = Color.WHITE

  val cardStarUSSR1 = new Color(0xffffce00)
  val cardStarUSSR2 = new Color(0xffd62432)

  val cardBackground = new Color(0xffe3f4fe)
  val cardRed = new Color(0xffde1d3a)

  val cardTitleFont = new Font(Lang.lishu, 0, 18)
  val cardOpFont = new Font("Arial", Font.BOLD, 18)
  val cardNameFont = new Font(Lang.lishu, 0, 24)
  val cardDescFont = new Font(Lang.heiti, 0, 13)

  val selectedCountry = new Color(255, 255, 255, 192)
  val changedCountry = Map (
    Neutral -> new Color(255, 255, 128, 120),
    US -> new Color(128, 128, 255, 120),
    USSR -> new Color(255, 128, 128, 120)
  )
  val availableCountry = new Color(128, 255, 128, 120)

  val tokenFont = new Font("Arial", Font.BOLD, 18)
  val tokenFontLarger = new Font("Arial", Font.BOLD, 24)
  val tokenFontSmall = new Font("Arial", Font.PLAIN, 12)

  val mapHighlightColor = new Color(0xffffff00)

  val regionContentColor = new Color(0xfffff7e2)
  val regionContentFont = new Font(Lang.heiti, Font.BOLD, 19)
  val regionContentFont2 = new Font("Arial", Font.BOLD, 19)

  val historyUIFont = new Font(Lang.songti, Font.PLAIN, 10)
}
