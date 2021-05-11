package falstar.util

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Paint

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.WindowConstants
import java.awt.RenderingHints
import java.awt.BasicStroke

object TreePlot {
  type Edges[A] = (Paint, Double, Seq[A])

  def WIDTH = 12
  def HEIGHT = 128
  def PADDING = 12
  def DEPTH = 12

  case class Node(paint: Paint, sub: Seq[Node], width: Int = WIDTH, height: Int = HEIGHT)

  object Node {
    val empty = Node(Color.black, Seq())
  }

  //  val pane = new JScrollPane
  //  pane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS)
  //  pane.add(panel)
  //  frame.setContentPane(pane)
  frame.getContentPane.add(panel)
  frame.pack()

  def plot[A](title: String, root: A, edges: A => Edges[A]) = {
    panel update (root, edges)
    frame.setTitle(title)
    if (!frame.isVisible) frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)

    panel.validate()
    panel.repaint()
  }

  object frame extends JFrame("")

  object panel extends JPanel {
    val lock = new Object

    var root: Node = Node.empty

    def reset(): Unit = lock synchronized {
      this.root = Node.empty
    }

    override def getPreferredSize = {
      new Dimension(800, 600)
    }

    override def paintComponent(_g: Graphics): Unit = lock synchronized {
      val g = _g.asInstanceOf[Graphics2D]
      super.paintComponent(g)

      //      g.setRenderingHint(
      //        RenderingHints.KEY_ANTIALIASING,
      //        RenderingHints.VALUE_ANTIALIAS_ON);

      //      g.setStroke(new BasicStroke(1.5f))

      val x = PADDING
      val y = PADDING + HEIGHT / 2
      paint(x, y, root, g)
    }

    def paint(x: Int, y: Int, node: Node, g: Graphics2D) {

      val nx = x + node.width / 2
      val ny = y

      var offset = x
      for (n <- node.sub) {
        val sx = offset
        val sy = y + HEIGHT
        offset += n.width

        g.setColor(Color.black)
        g.drawLine(nx, ny, sx + n.width / 2, sy)
        paint(sx, sy, n, g)
      }

      val bg = node.paint
      val fg = Color.black

      g.setPaint(bg)
      g.fillOval(nx - 5, ny - 5, 11, 11)
      g.setPaint(fg)
      g.drawOval(nx - 5, ny - 5, 11, 11)
    }

    def update[A](root: A, edges: A => Edges[A]): Unit = lock synchronized {
      def measure(node: A): Node = {
        val (paint, depth, children) = edges(node)

        if (children.isEmpty) {
          val width = WIDTH
          val height = HEIGHT
          Node(paint, Seq(), width, height)
        } else {
          val sub = children map measure
          val widths = sub map (_.width)
          val heights = sub map (_.height)
          Node(paint, sub, widths.sum, heights.max)
        }
      }

      this.root = measure(root)
    }
  }
}