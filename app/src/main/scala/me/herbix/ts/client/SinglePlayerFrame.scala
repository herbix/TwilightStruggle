package me.herbix.ts.client

import java.awt.Dimension
import java.awt.event.{ActionEvent, ActionListener}
import java.util.Random
import javax.swing._

import me.herbix.ts.agent.{AgentFactory, Agent}
import me.herbix.ts.client.NewRoomDialog.GameVariantDelegate
import me.herbix.ts.logic._
import me.herbix.ts.ui.GameUI
import me.herbix.ts.util.Lang

/**
  * Created by Chaofan on 2016/9/10.
  */
object SinglePlayerFrame extends JFrame {

  var extraInfluence = 0
  var hasOptional = true
  var drawWinner = Faction.US
  var gameVariant = GameVariant.Standard

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  setTitle("冷战热斗")

  val panel = new JPanel()
  panel.setPreferredSize(new Dimension(400, 300))

  add(panel)
  panel.setLayout(null)

  val info = new JLabel()
  showInfo()
  info.setLocation(30, 10)
  info.setSize(220, 80)
  panel.add(info)

  val infoChange = new JButton("游戏设置")
  infoChange.setLocation(280, 30)
  infoChange.setSize(100, 40)
  panel.add(infoChange)

  val labelPlayer1 = new JLabel("玩家一")
  labelPlayer1.setLocation(30, 120)
  labelPlayer1.setSize(60, 25)
  panel.add(labelPlayer1)

  val radio1Player = new JRadioButton("玩家", true)
  radio1Player.setLocation(100, 105)
  radio1Player.setSize(60, 25)
  panel.add(radio1Player)

  val radio1Agent = new JRadioButton("智能")
  radio1Agent.setLocation(100, 135)
  radio1Agent.setSize(60, 25)
  panel.add(radio1Agent)

  val agentList1Model = new DefaultComboBoxModel[AgentClassDelegate]()
  val agentList1 = new JComboBox[AgentClassDelegate](agentList1Model)
  agentList1.setLocation(180, 135)
  agentList1.setSize(160, 25)
  panel.add(agentList1)

  val agentLoad1 = new JButton("...")
  agentLoad1.setLocation(355, 135)
  agentLoad1.setSize(25, 25)
  panel.add(agentLoad1)

  val labelPlayer2 = new JLabel("玩家二")
  labelPlayer2.setLocation(30, 190)
  labelPlayer2.setSize(60, 25)
  panel.add(labelPlayer2)

  val group1 = new ButtonGroup
  group1.add(radio1Player)
  group1.add(radio1Agent)

  val radio2Player = new JRadioButton("玩家", true)
  radio2Player.setLocation(100, 175)
  radio2Player.setSize(60, 25)
  panel.add(radio2Player)

  val radio2Agent = new JRadioButton("智能")
  radio2Agent.setLocation(100, 205)
  radio2Agent.setSize(60, 25)
  panel.add(radio2Agent)

  val agentList2Model = new DefaultComboBoxModel[AgentClassDelegate]()
  val agentList2 = new JComboBox[AgentClassDelegate](agentList2Model)
  agentList2.setLocation(180, 205)
  agentList2.setSize(160, 25)
  panel.add(agentList2)

  val agentLoad2 = new JButton("...")
  agentLoad2.setLocation(355, 205)
  agentLoad2.setSize(25, 25)
  panel.add(agentLoad2)

  val group2 = new ButtonGroup
  group2.add(radio2Player)
  group2.add(radio2Agent)

  val start = new JButton("开始游戏")
  start.setLocation(140, 250)
  start.setSize(120, 45)
  panel.add(start)

  for (agentClass <- AgentFactory.getAllAgentClass) {
    val delegate = new AgentClassDelegate(agentClass)
    agentList1Model.addElement(delegate)
    agentList2Model.addElement(delegate)
  }

  pack()
  setLocationRelativeTo(getOwner)

  setResizable(false)

  def showInfo(): Unit = {
    info.setText("<html><body>" +
      s"游戏变体：${new GameVariantDelegate(gameVariant)}<br/>" +
      s"苏联让点：$extraInfluence<br/>" +
      s"平局胜者：${Lang.getFactionName(drawWinner)}<br/>" +
      s"可选牌：　${if (hasOptional) "有" else "无"}<br/>" +
      "</body></html>"
    )
  }

  infoChange.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      NewRoomDialog.setVisible(true)
      if (NewRoomDialog.isDone) {
        extraInfluence = NewRoomDialog.slider.getValue
        drawWinner = if (NewRoomDialog.us.isSelected) Faction.US else Faction.USSR
        hasOptional = NewRoomDialog.optional.isSelected
        gameVariant = NewRoomDialog.variant.getSelectedItem.asInstanceOf[GameVariantDelegate].gameVariant
        showInfo()
      }
    }
  })

  start.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = startGame()
  })

  def startGame(): Unit = {

    val game1 = GameFactory.createGameByVariant(gameVariant)
    val game2 = GameFactory.createGameByVariant(gameVariant)

    game1.extraInfluence = extraInfluence
    game1.drawGameWinner = drawWinner
    game1.optionalCards = hasOptional
    game1.playerId = 0

    game2.extraInfluence = extraInfluence
    game2.drawGameWinner = drawWinner
    game2.optionalCards = hasOptional
    game2.playerId = 1

    game1.anotherGame = game2
    game2.anotherGame = game1

    val seed = new Random().nextLong()
    game1.setRandomSeed(seed)
    game2.setRandomSeed(seed)

    val player1Agent =
      if (radio1Agent.isSelected && radio2Agent.isSelected) {
        agentList1.getSelectedItem.asInstanceOf[AgentClassDelegate].agentClass
      } else {
        null
      }

    val player2Agent =
      if (radio1Agent.isSelected || radio2Agent.isSelected) {
        if (radio1Agent.isSelected) {
          agentList1.getSelectedItem.asInstanceOf[AgentClassDelegate].agentClass
        } else {
          agentList2.getSelectedItem.asInstanceOf[AgentClassDelegate].agentClass
        }
      } else {
        null
      }

    var gameUI1: GameUI = null

    if (player1Agent != null) {
      AgentFactory.createAgentFromClass(player1Agent, game1, (hint: OperationHint, input: Operation) => {
        SwingUtilities.invokeLater(new Runnable {
          override def run(): Unit = {
            if (hint == game1.getOperationHint) {
              game1.sendNextState(input)
            }
          }
        })
      })

      val gameUI3 = new GameUI(2)
      val game3 = GameFactory.createGameByVariant(gameVariant)

      game3.extraInfluence = extraInfluence
      game3.drawGameWinner = drawWinner
      game3.optionalCards = hasOptional
      game3.playerId = 2
      game3.isSpectator = true

      game3.setRandomSeed(seed)

      game1.anotherGame = new GameCluster(game2, game3)
      game2.anotherGame = new GameCluster(game1, game3)
      game3.anotherGame = new GameCluster(game1, game2)

      gameUI3.init(game3)
      gameUI3.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      gameUI3.setVisible(true)

    } else {
      gameUI1 = new GameUI(0)
      gameUI1.init(game1)
      gameUI1.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      gameUI1.setVisible(true)
    }

    if (player2Agent != null) {
      AgentFactory.createAgentFromClass(player2Agent, game2, (hint: OperationHint, input: Operation) => {
        SwingUtilities.invokeLater(new Runnable {
          override def run(): Unit = {
            if (hint == game2.getOperationHint) {
              game2.sendNextState(input)
            }
          }
        })
      })
    } else {
      val gameUI2 = new GameUI(1)

      gameUI1.debugMode = true
      gameUI2.debugMode = true

      gameUI2.init(game2)
      gameUI2.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      gameUI2.setVisible(true)
    }

    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    setVisible(false)
  }

  class AgentClassDelegate(val agentClass: Class[_ <: Agent]) {
    override def toString = agentClass.getSimpleName
  }

  class GameCluster(games: Game*) extends GameTrait {
    override def nextState(input: Operation): Unit = {
      games.foreach(_.nextState(input))
    }
    override def rollBackBeforeHistory(historyId: Int): Unit = {
      games.foreach(_.rollBackBeforeHistory(historyId))
    }
  }
}
