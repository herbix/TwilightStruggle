package me.herbix.ts.logic

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.Region._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/13.
  */
class WorldMap {

  val links = mutable.Map[String, Set[String]]()
  val countries = mutable.Map[String, Country]()
  val fieldCountries = mutable.Map[Region, Set[Country]]()

  private def addCountry(country: Country, link: Set[String] = Set()): Unit = {
    val name = country.name
    countries(name) = country
    links(name) = link
    for (to <- link) {
      links(to) += name
    }

    val areas = country.regions
    for (area <- areas) {
      if (fieldCountries.contains(area)) {
        fieldCountries(area) += country
      } else {
        fieldCountries(area) = Set(country)
      }
    }
  }

  def modifyInfluence(country: String, faction: Faction, value: Int): Unit = {
    assert(countries.contains(country))
    val countryObj = countries(country)
    var newValue = countryObj.influence(faction) + value
    if (newValue < 0) {
      newValue = 0
    }
    countryObj.influence(faction) = newValue
  }

  addCountry(new Country("US", 50, false, Super))
  addCountry(new Country("USSR", 50, false, Super))

  addCountry(new Country("Mexico", 2, true, MidAmerica), Set("US"))
  addCountry(new Country("Guatemala", 1, false, MidAmerica), Set("Mexico"))
  addCountry(new Country("El Salvador", 1, false, MidAmerica), Set("Guatemala"))
  addCountry(new Country("Honduras", 2, false, MidAmerica), Set("Guatemala", "El Salvador"))
  addCountry(new Country("Costa Rica", 3, false, MidAmerica), Set("Honduras"))
  addCountry(new Country("Cuba", 3, true, MidAmerica), Set("US"))
  addCountry(new Country("Nicaragua", 1, false, MidAmerica), Set("Cuba", "Honduras", "Costa Rica"))
  addCountry(new Country("Haiti", 1, false, MidAmerica), Set("Cuba"))
  addCountry(new Country("Dominican Rep", 1, false, MidAmerica), Set("Haiti"))
  addCountry(new Country("Panama", 2, true, MidAmerica), Set("Costa Rica"))

  addCountry(new Country("Colombia", 1, false, SouthAmerica), Set("Panama"))
  addCountry(new Country("Ecuador", 2, false, SouthAmerica), Set("Colombia"))
  addCountry(new Country("Peru", 2, false, SouthAmerica), Set("Ecuador"))
  addCountry(new Country("Chile", 3, true, SouthAmerica), Set("Peru"))
  addCountry(new Country("Argentina", 2, true, SouthAmerica), Set("Chile"))
  addCountry(new Country("Venezuela", 2, true, SouthAmerica), Set("Colombia"))
  addCountry(new Country("Bolivia", 2, false, SouthAmerica), Set("Peru"))
  addCountry(new Country("Paraguay", 2, false, SouthAmerica), Set("Bolivia", "Argentina"))
  addCountry(new Country("Uruguay", 2, false, SouthAmerica), Set("Paraguay", "Argentina"))
  addCountry(new Country("Brazil", 2, true, SouthAmerica), Set("Venezuela", "Uruguay"))

  addCountry(new Country("Canada", 4, false, WestEuropeSet), Set("US"))
  addCountry(new Country("UK", 5, false, WestEuropeSet), Set("Canada"))
  addCountry(new Country("Benelux", 3, false, WestEuropeSet), Set("UK"))
  addCountry(new Country("France", 3, true, WestEuropeSet), Set("UK"))
  addCountry(new Country("Spain/Portugal", 2, false, WestEuropeSet), Set("France"))
  addCountry(new Country("Norway", 3, false, WestEuropeSet), Set("UK"))
  addCountry(new Country("Denmark", 3, false, WestEuropeSet))
  addCountry(new Country("W.Germany", 4, true, WestEuropeSet), Set("Denmark", "Benelux", "France"))
  addCountry(new Country("Sweden", 4, false, WestEuropeSet), Set("Norway", "Denmark"))
  addCountry(new Country("Italy", 2, true, WestEuropeSet), Set("France", "Spain/Portugal"))
  addCountry(new Country("Greece", 2, false, WestEuropeSet), Set("Italy"))
  addCountry(new Country("Turkey", 2, false, WestEuropeSet), Set("Greece"))
  addCountry(new Country("Finland", 4, false, MidEuropeSet), Set("Sweden", "USSR"))
  addCountry(new Country("Austria", 4, false, MidEuropeSet), Set("W.Germany", "Italy"))
  addCountry(new Country("E.Germany", 3, true, EastEuropeSet), Set("W.Germany", "Austria"))
  addCountry(new Country("Poland", 3, true, EastEuropeSet), Set("E.Germany", "USSR"))
  addCountry(new Country("Czechoslovakia", 3, false, EastEuropeSet), Set("E.Germany", "Poland"))
  addCountry(new Country("Hungary", 3, false, EastEuropeSet), Set("Czechoslovakia", "Austria"))
  addCountry(new Country("Yugoslavia", 3, false, EastEuropeSet), Set("Hungary", "Italy", "Greece"))
  addCountry(new Country("Romania", 3, false, EastEuropeSet), Set("Hungary", "Yugoslavia", "Turkey", "USSR"))
  addCountry(new Country("Bulgaria", 3, false, EastEuropeSet), Set("Greece", "Turkey"))

  addCountry(new Country("Lebanon", 1, false, MidEast))
  addCountry(new Country("Syria", 2, false, MidEast), Set("Lebanon", "Turkey"))
  addCountry(new Country("Israel", 4, true, MidEast), Set("Lebanon", "Syria"))
  addCountry(new Country("Iraq", 3, true, MidEast))
  addCountry(new Country("Iran", 2, true, MidEast), Set("Iraq"))
  addCountry(new Country("Libya", 2, true, MidEast))
  addCountry(new Country("Egypt", 2, true, MidEast), Set("Libya", "Israel"))
  addCountry(new Country("Jordan", 2, false, MidEast), Set("Israel", "Iraq"))
  addCountry(new Country("Gulf States", 3, false, MidEast), Set("Iraq"))
  addCountry(new Country("Saudi Arabia", 3, true, MidEast), Set("Jordan", "Iraq", "Gulf States"))

  addCountry(new Country("Afghanistan", 2, false, Asia), Set("Iran", "USSR"))
  addCountry(new Country("Pakistan", 2, true, Asia), Set("Iran", "Afghanistan"))
  addCountry(new Country("India", 3, true, Asia), Set("Pakistan"))
  addCountry(new Country("Burma", 2, false, SouthEastAsiaSet), Set("India"))
  addCountry(new Country("Laos/Cambodia", 1, false, SouthEastAsiaSet), Set("Burma"))
  addCountry(new Country("Thailand", 2, true, SouthEastAsiaSet), Set("Laos/Cambodia"))
  addCountry(new Country("Vietnam", 1, false, SouthEastAsiaSet), Set("Laos/Cambodia", "Thailand"))
  addCountry(new Country("Malaysia", 2, false, SouthEastAsiaSet), Set("Thailand"))
  addCountry(new Country("Indonesia", 1, false, SouthEastAsiaSet), Set("Malaysia"))
  addCountry(new Country("Australia", 4, false, Asia), Set("Malaysia"))
  addCountry(new Country("Philippines", 2, false, SouthEastAsiaSet), Set("Indonesia"))
  addCountry(new Country("Japan", 4, true, Asia), Set("US", "Philippines"))
  addCountry(new Country("Taiwan", 3, false, Asia), Set("Japan"))
  addCountry(new Country("S.Korea", 3, true, Asia), Set("Taiwan", "Japan"))
  addCountry(new Country("N.Korea", 3, true, Asia), Set("S.Korea", "USSR"))

  addCountry(new Country("Tunisia", 2, false, Africa), Set("Libya"))
  addCountry(new Country("Algeria", 2, true, Africa), Set("France", "Tunisia"))
  addCountry(new Country("Morocco", 3, false, Africa), Set("Spain/Portugal", "Algeria"))
  addCountry(new Country("West African States", 2, false, Africa), Set("Morocco"))
  addCountry(new Country("Saharan States", 1, false, Africa), Set("Algeria"))
  addCountry(new Country("Sudan", 1, false, Africa), Set("Egypt"))
  addCountry(new Country("Ivory Coast", 2, false, Africa), Set("West African States"))
  addCountry(new Country("Nigeria", 1, true, Africa), Set("Ivory Coast", "Saharan States"))
  addCountry(new Country("Ethiopia", 1, false, Africa), Set("Sudan"))
  addCountry(new Country("Somalia", 2, false, Africa), Set("Ethiopia"))
  addCountry(new Country("Cameroon", 1, false, Africa), Set("Nigeria"))
  addCountry(new Country("Zaire", 1, true, Africa), Set("Cameroon"))
  addCountry(new Country("Kenya", 2, false, Africa), Set("Somalia"))
  addCountry(new Country("Angola", 1, true, Africa), Set("Zaire"))
  addCountry(new Country("Zimbabwe", 1, false, Africa), Set("Zaire"))
  addCountry(new Country("SE African States", 1, false, Africa), Set("Kenya", "Zimbabwe"))
  addCountry(new Country("Botswana", 2, false, Africa), Set("Angola", "Zimbabwe"))
  addCountry(new Country("South Africa", 3, true, Africa), Set("Angola", "Botswana"))

  modifyInfluence("US", Faction.US, 100)
  modifyInfluence("USSR", Faction.USSR, 100)

  val normalCountries = countries.filter(e => !e._2.regions.contains(Super))

}
