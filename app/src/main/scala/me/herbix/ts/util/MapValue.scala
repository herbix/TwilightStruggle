package me.herbix.ts.util

import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.{Region, Country}
import me.herbix.ts.logic.Region.Region

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/18.
  */
object MapValue {

  val countrySize = (101, 67)
  val countryPosMap = mutable.Map[String, (Int, Int)]()

  countryPosMap += "Mexico" -> (38, 633)
  countryPosMap += "Guatemala" -> (135, 713)
  countryPosMap += "El Salvador" -> (100, 796)
  countryPosMap += "Honduras" -> (212, 790)
  countryPosMap += "Costa Rica" -> (201, 868)
  countryPosMap += "Cuba" -> (332, 690)
  countryPosMap += "Nicaragua" -> (325, 790)
  countryPosMap += "Haiti" -> (442, 761)
  countryPosMap += "Dominican Rep" -> (552, 760)
  countryPosMap += "Panama" -> (327, 867)

  countryPosMap += "Colombia" -> (399, 960)
  countryPosMap += "Ecuador" -> (282, 994)
  countryPosMap += "Peru" -> (345, 1080)
  countryPosMap += "Chile" -> (400, 1244)
  countryPosMap += "Argentina" -> (438, 1393)
  countryPosMap += "Venezuela" -> (461, 877)
  countryPosMap += "Bolivia" -> (462, 1151)
  countryPosMap += "Paraguay" -> (527, 1235)
  countryPosMap += "Uruguay" -> (562, 1331)
  countryPosMap += "Brazil" -> (656, 1073)

  countryPosMap += "Canada" -> (376, 318)
  countryPosMap += "UK" -> (809, 226)
  countryPosMap += "Benelux" -> (895, 305)
  countryPosMap += "France" -> (875, 396)
  countryPosMap += "Spain/Portugal" -> (794, 504)
  countryPosMap += "Norway" -> (932, 76)
  countryPosMap += "Denmark" -> (957, 154)
  countryPosMap += "W.Germany" -> (1006, 305)
  countryPosMap += "Sweden" -> (1086, 143)
  countryPosMap += "Italy" -> (1025, 462)
  countryPosMap += "Greece" -> (1167, 545)
  countryPosMap += "Turkey" -> (1369, 473)
  countryPosMap += "Finland" -> (1233, 81)
  countryPosMap += "Austria" -> (1055, 383)
  countryPosMap += "E.Germany" -> (1047, 230)
  countryPosMap += "Poland" -> (1164, 230)
  countryPosMap += "Czechoslovakia" -> (1143, 305)
  countryPosMap += "Hungary" -> (1167, 383)
  countryPosMap += "Yugoslavia" -> (1141, 463)
  countryPosMap += "Romania" -> (1279, 383)
  countryPosMap += "Bulgaria" -> (1257, 463)

  countryPosMap += "Lebanon" -> (1304, 549)
  countryPosMap += "Syria" -> (1413, 547)
  countryPosMap += "Israel" -> (1285, 625)
  countryPosMap += "Iraq" -> (1413, 625)
  countryPosMap += "Iran" -> (1523, 626)
  countryPosMap += "Libya" -> (1116, 693)
  countryPosMap += "Egypt" -> (1231, 704)
  countryPosMap += "Jordan" -> (1357, 703)
  countryPosMap += "Gulf States" -> (1484, 701)
  countryPosMap += "Saudi Arabia" -> (1453, 778)

  countryPosMap += "Afghanistan" -> (1654, 575)
  countryPosMap += "Pakistan" -> (1656, 674)
  countryPosMap += "India" -> (1779, 727)
  countryPosMap += "Burma" -> (1916, 741)
  countryPosMap += "Laos/Cambodia" -> (2026, 753)
  countryPosMap += "Thailand" -> (1979, 832)
  countryPosMap += "Vietnam" -> (2092, 833)
  countryPosMap += "Malaysia" -> (2031, 951)
  countryPosMap += "Indonesia" -> (2216, 1044)
  countryPosMap += "Australia" -> (2218, 1180)
  countryPosMap += "Philippines" -> (2257, 830)
  countryPosMap += "Japan" -> (2347, 622)
  countryPosMap += "Taiwan" -> (2209, 712)
  countryPosMap += "S.Korea" -> (2257, 548)
  countryPosMap += "N.Korea" -> (2235, 471)

  countryPosMap += "Tunisia" -> (1051, 604)
  countryPosMap += "Algeria" -> (936, 614)
  countryPosMap += "Morocco" -> (823, 648)
  countryPosMap += "West African States" -> (813, 748)
  countryPosMap += "Saharan States" -> (982, 776)
  countryPosMap += "Sudan" -> (1251, 796)
  countryPosMap += "Ivory Coast" -> (885, 896)
  countryPosMap += "Nigeria" -> (1025, 884)
  countryPosMap += "Ethiopia" -> (1331, 876)
  countryPosMap += "Somalia" -> (1455, 910)
  countryPosMap += "Cameroon" -> (1076, 972)
  countryPosMap += "Zaire" -> (1209, 1010)
  countryPosMap += "Kenya" -> (1343, 977)
  countryPosMap += "Angola" -> (1111, 1102)
  countryPosMap += "Zimbabwe" -> (1247, 1140)
  countryPosMap += "SE African States" -> (1358, 1082)
  countryPosMap += "Botswana" -> (1211, 1221)
  countryPosMap += "South Africa" -> (1158, 1306)

  def getCountryPosSize(country: Country): (Int, Int, Int, Int) = {
    country.name match {
      case "China" => (2074, 545, 134, 106)
      case n: String =>
        val pos = countryPosMap.getOrElse(n, null)
        if (pos == null) {
          (0, 0, -1, -1)
        } else {
          (pos._1, pos._2, countrySize._1, countrySize._2)
        }
      case _ => null
    }
  }

  val turnArea = (1799, 49, 2472, 125)

  val round1 = (1960, 204)
  val round8 = (2414, 204)
  val roundSize = (58, 58)

  val defcon5 = (12, 1230)
  val defcon1 = (277, 1230)
  val defconSize = (58, 58)

  val military5 = (11, 1360)
  val military0 = (344, 1360)
  val militarySize = (58, 58)

  val vpUs20 = (11, 1530)
  val vpUssr20 = (2432, 1530)
  val vpSize = (57, 57)

  val space0 = (1389, 1326)
  val space8 = (2096, 1326)
  val spaceSize = (79, 79)

  val regionSize = (138, 96)
  val regionPos = mutable.Map.empty[Region, (Int, Int)]

  regionPos += Region.Europe -> (1066, 19)
  regionPos += Region.Asia -> (1859, 975)
  regionPos += Region.MidEast -> (1616, 832)
  regionPos += Region.Africa -> (923, 1004)
  regionPos += Region.MidAmerica -> (469, 624)
  regionPos += Region.SouthAmerica -> (725, 1200)
  regionPos += Region.SouthEastAsia -> (1997, 1099)
}
