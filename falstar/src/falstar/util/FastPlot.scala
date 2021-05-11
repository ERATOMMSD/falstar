package falstar.util

import java.awt.Graphics2D
import java.awt.Graphics
import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.Paint
import java.awt.Dimension
import falstar.falsification.Results
import falstar.falsification.Result
import javax.swing.WindowConstants

trait Plot {
  def min_x: Double
  def min_y: Double
  def max_x: Double
  def max_y: Double
  def paint(graphics: Graphics2D, transform: (Double, Double) => (Int, Int)): Unit
}

case class Rect(min_x: Double, min_y: Double, max_x: Double, max_y: Double) extends Plot {
  def paint(graphics: Graphics2D, transform: (Double, Double) => (Int, Int)): Unit = {
    val (ax, by) = transform(min_x, min_y)
    val (bx, ay) = transform(max_x, max_y)
    graphics.fillRect(ax, ay, bx - ax, by - ay)
  }
}

case class Scatter(data: Iterable[(Double, Double)], size: (Int, Int) = null) extends Plot {
  assert(!data.isEmpty)

  val min_x = data.minBy(_._1)._1
  val min_y = data.minBy(_._2)._2
  val max_x = data.maxBy(_._1)._1
  val max_y = data.maxBy(_._2)._2

  def paint(graphics: Graphics2D, transform: (Double, Double) => (Int, Int)): Unit = {
    for ((ax, ay) <- data) {
      if (size == null) {
        val (px, py) = transform(ax, ay)
        graphics.fillRect(px - 1, py - 1, 3, 3)
      } else {
        val (aw, ah) = size
        val (px, py) = transform(ax, ay)
        val (pw, ph) = transform(ax + aw, ay + ah)
        graphics.fillRect(px - 1, py - 1, (pw - px) + 3, (ph - py) + 3)
      }
    }
  }
}

case class Tree[A](root: A, succ: A => Seq[A]) extends Plot {
  def min_x: Double = 0
  def min_y: Double = 0
  def max_x: Double = 0
  def max_y: Double = 0

  def paint(graphics: Graphics2D, transform: (Double, Double) => (Int, Int)) {

  }
}

object FastPlot {
  type Series = (Plot, Paint)

  frame.getContentPane().add(panel)
  frame.pack()

  def plot(title: String, series: Series*) = {
    panel update series
    frame.setTitle(title)
    if (!frame.isVisible) frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)

    panel.validate()
    panel.repaint()
  }

  object frame extends JFrame("")

  object panel extends JPanel {
    val lock = new Object

    var series: Seq[Series] = Seq()

    def reset(): Unit = lock synchronized {
      this.series = Seq()
    }

    def update(series: Seq[Series]): Unit = lock synchronized {
      this.series = series
    }

    override def getPreferredSize = {
      new Dimension(800, 600)
    }

    override def paintComponent(_g: Graphics): Unit = lock synchronized {
      if (series.isEmpty) return

      val g = _g.asInstanceOf[Graphics2D]
      super.paintComponent(g)

      val margin = 10
      val width = getWidth - margin * 2
      val height = getHeight - margin * 2

      val min_x = series.map(_._1.min_x).min
      val min_y = series.map(_._1.min_y).min
      val max_x = series.map(_._1.max_x).max
      val max_y = series.map(_._1.max_y).max

      // println(min_x, min_y, max_x, max_y)

      val scale_x = width / (max_x - min_x)
      val scale_y = height / (max_y - min_y)

      // val scale = Math.min(scale_x, scale_y)

      val transform = {
        (ax: Double, ay: Double) =>
          val bx = (ax - min_x) * scale_x
          val by = height - (ay - min_y) * scale_y
          (margin + bx.toInt, margin + by.toInt)
      }

      for ((plot, paint) <- series) {
        g.setPaint(paint)
        plot.paint(g, transform)
      }
    }
  }
}