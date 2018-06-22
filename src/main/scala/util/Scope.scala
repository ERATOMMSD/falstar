package util

import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Paint
import java.awt.Rectangle

import falsification.Result
import hybrid.Signal
import hybrid.Time
import hybrid.System
import hybrid.Trace
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.WindowConstants
import linear.Vector
import scala.collection.mutable.ArrayBuffer
import java.awt.BasicStroke
import mtl.Robustness
import hybrid.ContinuousSystem
import hybrid.Input
import hybrid.State
import java.awt.Dimension

object Scope {
  /* def main(args: Array[String]) {
    val T = 2.0

    val us = Signal((0.0, Vector(1)), (1.0, Vector(-1)))

    val sys = ContinuousSystem(
      "simple",
      Vector(0),
      Seq("u" -> (-1.0, 1.0)),
      Seq("y"),
      (t: Time, x: State, u: Input) => u,
      0.1)

    val Seq(u) = sys.inports
    val Seq(x) = sys.outports

    val tr = sys.sim(us, T)
    val rs = Robustness(x > 0, tr.us, tr.ys)
    val res = Result(tr, rs)
    val scope = new Scope("test", sys, res)
  } */
}

class Scope(title: String, sys: System, _res: Result) extends JFrame(title) {

  val margin = 10
  val textheight = 12

  val t0 = 0
  val T = _res.T

  val dim = if (_res.tr.ys.isEmpty) 0 else _res.tr.ys(0)._2.length

  val res = {
    val Result(Trace(_us, ys), rs) = _res
    val us = new ArrayBuffer[(Time, Vector)]
    val (_, un) = _us.last

    for (i <- 0 until _us.length) {
      val (t0, u0) = _us(i)
      val (t1, u1) = if (i + 1 < _us.length) _us(i + 1) else (T, un)

      us += ((t0, u0))
      us += ((t1, u0))
    }

    Result(Trace(us.toArray, ys), rs)
  }

  val colors = Array(
    Color.red,
    Color.blue,
    Color.green,
    Color.magenta,
    Color.cyan,
    Color.black)

  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  add(panel)
  pack()
  setVisible(true)

  object panel extends JPanel {
    def line(x1: Double, y1: Double, x2: Double, y2: Double, g: Graphics2D) {
      g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }

    def plot(xs: Signal, i: Int, p: Rectangle, g: Graphics2D) {
      if (xs.isEmpty) return

      val sx = if (t0 < T) p.getWidth / (T - t0) else 0

      /*
      val (_, min) = xs.minBy { case (t, x) => x(i) }
      val (_, max) = xs.maxBy { case (t, x) => x(i) }

      val a0 = min(i)
      val a1 = max(i)
      val sy = if (a0 < a1) p.getHeight / (a1 - a0) else Math.abs(a0)
      */

      val a0 = 0.0
      val (_, am) = xs.maxBy { // -0.009116101160866346
        case (t, x) if x(i).isInfinite => 18
        case (t, x) => Math.abs(x(i))
      }

      val py = (-18 to 18).find(Math.abs(am(i)) <= Math.exp(_)).getOrElse(0)
      val sy = p.getHeight / Math.exp(py)

      for (j <- 0 until xs.length - 1) {
        val (t1, a1) = xs(j)
        val ax = (t1 - t0) * sx
        val ay = (p.getHeight - (a1(i) - a0) * sy) / 2

        val (t2, a2) = xs(j + 1)
        val bx = (t2 - t0) * sx
        val by = (p.getHeight - (a2(i) - a0) * sy) / 2

        if (0 <= ay && ay < p.getHeight && 0 <= by && by < p.getHeight) {
          line(p.getX + ax, p.getY + ay, p.getX + bx, p.getY + by, g)
        } else {
          // println("invalid value: " + ay + " or " + by)
        }
      }
    }

    def plot(xs: Signal, ns: Seq[(String, Paint)], p: Rectangle, g: Graphics2D) {
      g.setPaint(Color.white)
      g.fill(p)

      g.setPaint(Color.black)
      g.draw(p)

      g.setPaint(Color.gray)
      g.drawLine(p.x, p.y + p.height / 2, p.x + p.width, p.y + p.height / 2)

      val q = new Rectangle(p.x + margin, p.y + margin, p.width - 2 * margin, p.height - 2 * margin)

      for (((name, paint), i) <- ns.zipWithIndex) {
        g.setPaint(paint)
        plot(xs, i, q, g)

        val tx = p.getX
        val ty = p.getY + p.getHeight + margin + (i + 1) * textheight
        g.drawString(name, tx.toInt, ty.toInt)
      }
    }

    override def paintComponent(_g: Graphics): Unit = {
      val g = _g.asInstanceOf[Graphics2D]

      g.setStroke(new BasicStroke(2));

      val Result(Trace(us, ys), rs) = res
      val zs: Signal = rs.rs map { case (t, r) => (t, Vector(r)) }

      val width = getWidth - 4 * margin
      val height = getHeight - 3 * margin - dim * textheight

      val p1 = new Rectangle(margin, margin, width / 3, height)
      val p2 = new Rectangle(margin + width / 3 + margin, margin, width / 3, height)
      val p3 = new Rectangle(2 * (margin + width / 3) + margin, margin, width / 3, height)

      val in = sys.inports map {
        port =>
          (port.name, colors(port.index % colors.length))
      }

      val out = sys.outports map {
        port =>
          (port.name, colors(port.index % colors.length))
      }

      val rob = Seq("robustness" -> Color.red)

      plot(us, in, p1, g)
      plot(ys, out, p2, g)
      plot(zs, rob, p3, g)
    }
  }
}
