package me.herbix.ts.util

import java.awt.{Color, Font}
import javax.imageio.ImageIO

import me.herbix.ts.logic.{Flags, Flag}
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.Faction._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
object Resource {
  val worldMap = ImageIO.read(getClass.getResourceAsStream("/worldmap.jpg"))
  val card = (0 to 110)
    .map(i => (i, ImageIO.read(getClass.getResourceAsStream(f"/cards/$i%03d.png"))))
    .toMap

  Flags.init()
  val flag = (0 to Flag.flagId).map(i => {
    val in = getClass.getResourceAsStream(f"/flags/$i%02d.png")
    if (in != null) {
      val img = ImageIO.read(in)
      (i, Map(US -> img, USSR -> img, Neutral -> img))
    } else {
      val inA = getClass.getResourceAsStream(f"/flags/$i%02dA.png")
      val inS = getClass.getResourceAsStream(f"/flags/$i%02dS.png")
      (i, Map(US -> ImageIO.read(inA), USSR -> ImageIO.read(inS)))
    }
  }).toMap

  val textFont = new Font(Lang.heiti, 0, 16)
  val textFont2 = new Font(Lang.lishu, 0, 32)
  val textColor = Color.DARK_GRAY
  val usColor = Color.BLUE
  val usColorInfluenceChange = new Color(128, 128, 255)
  val ussrColor = Color.RED
  val ussrColorInfluenceChange = new Color(255, 128, 128)

  val influenceTokenTextFont = new Font(Lang.heiti, Font.BOLD, 32)
  val tokenColor = Color.YELLOW
  val tokenSize = 46

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

  val cardStarNeutral1 = Color.WHITE
  val cardStarNeutral2 = new Color(0xfff04b47)

  val cardStarUS1 = new Color(0xff0978bd)
  val cardStarUS2 = Color.WHITE

  val cardStarUSSR1 = new Color(0xffffce00)
  val cardStarUSSR2 = new Color(0xffd62432)

  val cardTitleFont = new Font(Lang.lishu, 0, 18)
  val cardOpFont = new Font("Arial", Font.BOLD, 18)
  val cardNameFont = new Font(Lang.lishu, 0, 24)
  val cardDescFont = new Font(Lang.heiti, 0, 13)

  val selectedCountry = new Color(255, 255, 255, 192)

}
