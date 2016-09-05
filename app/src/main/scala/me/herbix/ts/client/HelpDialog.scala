package me.herbix.ts.client

import java.awt.{Font, Dimension, BorderLayout}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import me.herbix.ts.logic.card.{Cards, Card}
import me.herbix.ts.util.{Resource, Lang}

/**
  * Created by Chaofan on 2016/9/5.
  */
object HelpDialog extends JDialog {

  setLayout(new BorderLayout())

  val helpListModel = new DefaultListModel[ListItem]
  val helpList = new JList[ListItem](helpListModel)
  val helpListOuter = new JScrollPane(helpList)

  helpList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  helpListOuter.setPreferredSize(new Dimension(250, 600))

  add(helpListOuter, BorderLayout.WEST)

  val helpInfo = new JLabel
  val helpInfoOuter = new JScrollPane(helpInfo)

  val defaultFont = helpInfo.getFont
  helpInfo.setFont(new Font(defaultFont.getFontName, 0, (defaultFont.getSize * 1.3f).toInt))
  helpInfo.setPreferredSize(new Dimension(10, 10))
  helpInfoOuter.setPreferredSize(new Dimension(700, 650))

  add(helpInfoOuter)

  setTitle("帮助")

  pack()

  setLocationRelativeTo(getOwner)

  (1 to 110).map(Cards.fromId).foreach(card => helpListModel.addElement(new ListItem(card)))

  helpList.addListSelectionListener(new ListSelectionListener {
    override def valueChanged(e: ListSelectionEvent): Unit = {
      if (!e.getValueIsAdjusting) {
        val selected = helpList.getSelectedValue
        if (selected != null) {
          helpInfo.setText(selected.getText)
        }
      }
    }
  })

  class ListItem(val ref: Any) {
    override def toString: String = {
      ref match {
        case card: Card => f"${card.id}%03d ${Lang.cardInfo(card.id)._1}%s"
        case _ => ref.toString
      }
    }

    def getText: String = {
      ref match {
        case card: Card =>
          "<html><body>" +
            f"<center><img border='1' src='${getClass.getResource(f"/cards/${card.id}%03d.png")}'/></center>" +
            f"<center>${Lang.cardInfo(card.id)._1}</center>" +
            f"<p>${Lang.cardInfo(card.id)._2.replaceAll("\n", "</p><p>")}</p>" +
            "</body></html>"
        case _ => ref.toString
      }
    }
  }

}
