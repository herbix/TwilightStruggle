package me.herbix.ts.ui

import java.awt.event._
import java.awt.{BorderLayout, Color, Dimension}
import javax.swing._

import me.herbix.ts.agent.Agent
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.SpaceLevel.SpaceLevel
import me.herbix.ts.logic._
import me.herbix.ts.logic.card.Card
import me.herbix.ts.logic.turnzero.Crisis
import me.herbix.ts.util._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/13.
  */
class GameUI(playerId: Int) extends JFrame {

  var game: Game = null
  var agent: Agent = null

  def init(game: Game): Unit = {
    this.game = game

    // UI start
    setLayout(new BorderLayout)

    val leftPanel = new JPanel
    leftPanel.setPreferredSize(new Dimension(200, 100))
    leftPanel.setLayout(new BorderLayout)
    add(leftPanel, BorderLayout.WEST)

    val infoUI = new InfoUI(game)
    infoUI.setPreferredSize(new Dimension(200, 150))
    leftPanel.add(infoUI, BorderLayout.SOUTH)

    val controlUI = new ControlUI(game)
    controlUI.setPreferredSize(new Dimension(200, 200))
    controlUI.operationListeners :+= ((op: Operation) => if (op != null) game.sendNextState(op))
    leftPanel.add(controlUI, BorderLayout.NORTH)

    val historyUI = new HistoryUI(game)
    val historyUIOuter = new JScrollPane(historyUI)
    historyUIOuter.setBorder(null)
    historyUIOuter.getVerticalScrollBar.setUnitIncrement(40)
    leftPanel.add(historyUIOuter)

    val centerPanel = new JPanel
    centerPanel.setLayout(new BorderLayout)
    add(centerPanel)

    val worldMapUI = new WorldMapUI(game)
    val worldMapUIOuter = new JScrollPane(worldMapUI)
    worldMapUI.setOuter(worldMapUIOuter)
    worldMapUI.gameUI = this
    worldMapUIOuter.setPreferredSize(new Dimension(750, 480))
    worldMapUIOuter.setWheelScrollingEnabled(false)
    worldMapUIOuter.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    worldMapUIOuter.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
    centerPanel.add(worldMapUIOuter)

    val handUI = new HandUI(game)
    handUI.setPreferredSize(new Dimension(600, 150))
    centerPanel.add(handUI, BorderLayout.SOUTH)

    val rightPanel = new JPanel
    rightPanel.setPreferredSize(new Dimension(200, 100))
    rightPanel.setLayout(new BorderLayout)
    add(rightPanel, BorderLayout.EAST)

    val detailUI = new DetailUI(game)
    detailUI.setPreferredSize(new Dimension(200, 300))
    rightPanel.add(detailUI, BorderLayout.NORTH)

    val flagsUI = new FlagsUI(game)
    rightPanel.add(flagsUI)

    game.playerId = playerId
    setTitle("Player - " + game.playerId)

    pack()
    // UI end

    var bgSet = false

    val pendingInfluenceChange: mutable.Map[Country, Int] = mutable.Map()
    val pendingCountrySelection: mutable.Set[Country] = mutable.Set()
    val pendingCardSelection: mutable.Set[Card] = mutable.Set()

    game.stateUpdateListeners :+= (() => {
      if (!bgSet) {
        if (game.playerFaction == Faction.US) {
          setUIBackground(Resource.usColorUI)
          bgSet = true
        } else if (game.playerFaction == Faction.USSR) {
          setUIBackground(Resource.ussrColorUI)
          bgSet = true
        }
      }
    })

    controlUI.uiInfluence.pendingInfluenceChange = pendingInfluenceChange
    worldMapUI.pendingInfluenceChange = pendingInfluenceChange

    worldMapUI.countryClickListeners :+= ((country: Country, button: Int) => {
      if (button == MouseEvent.BUTTON1) {
        controlUI.uiType match {
          case controlUI.UIType.Influence => controlUI.uiInfluence.addInfluence(country, 1)
          case controlUI.UIType.SelectCountry => controlUI.uiSelectCountry.toggleCountry(country)
          case _ =>
        }
      }
    })

    handUI.cardClickListeners :+= ((card: Card) => {
      val hasEventCards = handUI.hasEventCards
      if ((handUI.selfHand.isSelected && !hasEventCards) || (handUI.eventCards.isSelected && hasEventCards)) {
        controlUI.uiType match {
          case controlUI.UIType.SelectCard =>
            controlUI.uiSelectCard.setCard(card)
            handUI.repaint()
          case controlUI.UIType.SelectCardAndAction =>
            controlUI.uiSelectCardAndAction.setCard(card)
            handUI.repaint()
          case controlUI.UIType.SelectCardOrCancel =>
            controlUI.uiSelectCardOrCancel.setCard(card)
            handUI.repaint()
          case controlUI.UIType.SelectMultipleCards =>
            controlUI.uiSelectMultipleCards.toggleCard(card)
            handUI.repaint()
          case _ =>
        }
      }
    })

    controlUI.uiInfluence.updateListeners :+= (() => {
      worldMapUI.pendingInfluenceFaction = controlUI.uiInfluence.targetFaction
      worldMapUI.pendingInfluenceIsAdd = controlUI.uiInfluence.isAdd

      worldMapUI.availableCountries =
        game.theWorldMap.countries.values.filter(c => {
          val pendingInfluenceChange = controlUI.uiInfluence.pendingInfluenceChange
          val input = if (pendingInfluenceChange.contains(c)) {
            pendingInfluenceChange + (c -> (pendingInfluenceChange(c) + 1))
          } else {
            pendingInfluenceChange + (c -> 1)
          }
          controlUI.uiInfluence.checkInfluence(input) && c != game.theWorldMap.countryUS && c != game.theWorldMap.countryUSSR
        }).toSet

      worldMapUI.repaint()
    })

    worldMapUI.countryHoverListeners :+= ((country: Country) => {
      detailUI.setCountry(country)
    })

    handUI.cardHoverListeners :+= ((card: Card) => {
      detailUI.setCard(card)
    })

    ControlSubUICard.cardHoverListeners :+= ((card: Card) => {
      detailUI.setCard(card)
    })

    flagsUI.flagHoverListeners :+= ((faction: Faction, flag: Flag, flagData: Any) => {
      detailUI.setFlag(faction, flag, flagData)
    })

    worldMapUI.spaceHoverListeners :+= ((space: SpaceLevel) => {
      detailUI.setSpace(space)
    })

    worldMapUI.regionHoverListeners :+= ((region: Region) => {
      detailUI.setRegion(region)
    })

    worldMapUI.crisisHoverListeners :+= ((crisis: Crisis, effect: Int) => {
      detailUI.setCrisis(crisis, effect)
    })

    controlUI.uiSelectCountry.pendingCountrySelection = pendingCountrySelection
    worldMapUI.pendingCountrySelection = pendingCountrySelection

    controlUI.uiSelectCountry.updateListeners :+= (() => {
      if (controlUI.uiType == controlUI.UIType.SelectCountry) {
        worldMapUI.availableCountries =
          game.theWorldMap.normalCountries.values.filter(c => {
            controlUI.uiSelectCountry.checkSelection(pendingCountrySelection + c)
          }).toSet
        worldMapUI.repaint()
      }
    })

    controlUI.uiUpdateListeners :+= (() => {
      if (controlUI.uiType != controlUI.UIType.SelectCountry && controlUI.uiType != controlUI.UIType.Influence) {
        worldMapUI.availableCountries = Set.empty
      }
    })

    historyUI.historyHoverListeners :+= ((history: History) => {
      val hasChangedCountry = worldMapUI.changedCountries.nonEmpty
      worldMapUI.changedCountries = Set.empty

      history match {
        case h: HistoryCard =>
          detailUI.setCard(h.card)
        case h: HistoryFlag =>
          detailUI.setFlag(h.faction, h.flag, h.data)
        case h: HistoryCountry =>
          worldMapUI.changedCountries = h.countries
          worldMapUI.changedCountriesFaction = h.faction
          worldMapUI.repaint()
        case _ =>
      }

      if (worldMapUI.changedCountries.nonEmpty != hasChangedCountry) {
        worldMapUI.repaint()
      }
    })

    historyUI.historyClickListeners :+= ((history: History) => {
      val snapshot = history.snapshot
      if (snapshot != null && !snapshot.needApproval && snapshot.operatingPlayer == game.playerFaction) {
        game.sendRollBackBeforeHistory(history.id)
      }
    })

    controlUI.uiSelectCard.pendingCardSelection = pendingCardSelection
    controlUI.uiSelectCardAndAction.pendingCardSelection = pendingCardSelection
    controlUI.uiSelectCardOrCancel.pendingCardSelection = pendingCardSelection
    controlUI.uiSelectMultipleCards.pendingCardSelection = pendingCardSelection
    handUI.pendingCardSelection = pendingCardSelection

    flagsUI.flagClickListeners :+= ((faction: Faction, flag: Flag, flagData: Any) => {
      if (faction == game.playerFaction && flag == Flags.CubaMissile && game.stateStack.top != State.end) {
        val op = new OperationCubaMissileRequest(game.playerId, game.playerFaction, false)
        game.sendNextState(op)
      }
    })

    // debug
    addKeyListenerRecursion(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = {
        if (!debugMode) {
          return
        }
        if (e.isControlDown && e.getKeyCode == KeyEvent.VK_D) {
          val cmd = JOptionPane.showInputDialog(GameUI.this, Lang.inputDebugCommand, Lang.twilightStruggle, JOptionPane.PLAIN_MESSAGE)
          if (cmd != null && cmd != "") {
            val params = cmd.substring(1).trim.split(" ")
            cmd.charAt(0) match {
              case 'c' =>
                if (params.nonEmpty) {
                  debugAddCard(params(0).toInt, if (params.length > 1) params(1) else "h")
                }
              case 's' =>
                if (params.nonEmpty) {
                  debugSpace(params(0).toInt)
                }
              case 'r' =>
                if (params.nonEmpty) {
                  debugRound(params(0).toInt)
                }
              case _ =>
            }
          }
        }
      }
    })

    def debugAddCard(id: Int, target: String): Unit = {
      val card = game.theCards.fromId(id)
      if (card == null) return

      val game1 = game
      val game2 = game.anotherGame.asInstanceOf[Game]

      target match {
        case "h" =>
          game1.hand(game.playerFaction).add(card)
          game2.hand(game.playerFaction).add(card)
        case "d" =>
          game1.discards.add(card)
          game2.discards.add(card)
        case "r" =>
          game1.hand(game.playerFaction).remove(card)
          game2.hand(game.playerFaction).remove(card)
        case _ => return
      }

      handUI.updateState()
    }

    def debugSpace(step: Int) = {
      val game1 = game
      val game2 = game.anotherGame.asInstanceOf[Game]
      val space = game1.space(game.playerFaction)

      game1.increaseSpace(game.playerFaction, step)
      game2.increaseSpace(game.playerFaction, step)

      worldMapUI.repaint()
    }

    def debugRound(round: Int) = {
      val game1 = game
      val game2 = game.anotherGame.asInstanceOf[Game]

      game1.round = round
      game2.round = round

      worldMapUI.repaint()
    }

    def addKeyListenerRecursion(listener: KeyListener): Unit = {
      addKeyListener(listener)
      getComponents
        .filter(_.isInstanceOf[JComponent])
        .map(_.asInstanceOf[JComponent])
        .foreach(c => addKeyListenerRecursion2(c, listener))
    }

    def addKeyListenerRecursion2(component: JComponent, listener: KeyListener): Unit = {
      component.addKeyListener(listener)
      component.getComponents
        .filter(_.isInstanceOf[JComponent])
        .map(_.asInstanceOf[JComponent])
        .foreach(c => addKeyListenerRecursion2(c, listener))
    }

    def setUIBackground(color: Color): Unit = {
      controlUI.setBackground(color)
      historyUI.setBackground(color)
      infoUI.setBackground(color)
      handUI.setBackground(color)
      detailUI.setBackground(color)
      flagsUI.setBackground(color)
    }
  }

  var debugMode = false

}
