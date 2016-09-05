package me.herbix.ts.client

import java.awt.{Dimension, BorderLayout}
import javax.swing._

import me.herbix.ts.client.RoomDialog._
import me.herbix.ts.logic.card.{Cards, Card}
import me.herbix.ts.util.Lang

/**
  * Created by Chaofan on 2016/9/5.
  */
object HelpDialog extends JDialog {

  setLayout(new BorderLayout())

  val helpListModel = new DefaultListModel[ListItem]
  val helpList = new JList[ListItem](helpListModel)
  val helpListOuter = new JScrollPane(helpList)

  helpListOuter.setPreferredSize(new Dimension(250, 600))

  add(helpListOuter, BorderLayout.WEST)

  val helpInfo = new JLabel
  val helpInfoOuter = new JScrollPane(helpInfo)

  helpInfoOuter.setPreferredSize(new Dimension(600, 600))

  add(helpInfoOuter)

  setTitle("帮助")

  pack()

  setLocationRelativeTo(getOwner)

  (1 to 110).map(Cards.fromId).foreach(card => helpListModel.addElement(new ListItem(card)))

  class ListItem(val ref: Any) {
    override def toString: String = {
      ref match {
        case card: Card => f"${card.id}%03d ${Lang.cardInfo(card.id)._1}%s"
        case _ => ref.toString
      }
    }
  }

}
