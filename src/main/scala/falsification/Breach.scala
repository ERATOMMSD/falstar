package falsification

import hybrid.Signal
import hybrid.SimulinkSystem
import hybrid.System
import hybrid.Trace
import linear.Vector
import mtl.Always
import mtl.And
import mtl.Const
import mtl.DividedBy
import mtl.Equal
import mtl.Eventually
import mtl.False
import mtl.Formula
import mtl.InPort
import mtl.Less
import mtl.LessEqual
import mtl.Minus
import mtl.NotEqual
import mtl.Or
import mtl.Plus
import mtl.Port
import mtl.Robustness
import mtl.Term
import mtl.Times
import mtl.True
import mtl.Transform
import mtl.Not
import mtl.Implies

object Breach {
  def print(tm: Term): String = tm match {
    case c: Const => c.toString
    case p: Port => p.name + "[t]"
    case Plus(left, right) => "(" + print(left) + " + " + print(right) + ")"
    case Minus(left, right) => "(" + print(left) + " - " + print(right) + ")"
    case Times(left, right) => "(" + print(left) + " * " + print(right) + ")"
    case DividedBy(left, right) => "(" + print(left) + " / " + print(right) + ")"
    case Transform(tm, _, f) if !f.isEmpty => f + "(" + print(tm) + ")"
  }

  def print(phi: Formula): String = phi match {
    case True => "true"
    case False => "false"

    case Less(left, right) => "(" + print(left) + " < " + print(right) + ")"
    case LessEqual(left, right) => "(" + print(left) + " <= " + print(right) + ")"
    case Equal(left, right) => "(" + print(left) + " == " + print(right) + ")"
    case NotEqual(left, right) => "(not (" + print(left) + " == " + print(right) + "))"

    case Not(phi) => "(not " + print(phi) + ")"
    case Or(phi, psi) => "(" + print(phi) + " or " + print(psi) + ")"
    case And(phi, psi) => "(" + print(phi) + " and " + print(psi) + ")"
    case Implies(phi, psi) => "(" + print(phi) + " => " + print(psi) + ")"

    case Always(t0, t1, phi) => "(alw_[" + t0 + "," + t1 + "] " + print(phi) + ")"
    case Eventually(t0, t1, phi) => "(ev_[" + t0 + "," + t1 + "] " + print(phi) + ")"
  }

  case class dummy(cp: Int) extends Falsification {
    def identification = "Breach (print)"
    def params = Seq()

    def search(sys: System, _phi: Formula): (Result, Statistics) = {
      val T = _phi.T
      val in = sys.in
      val phi = print(_phi)

      println("sys = BreachSimulinkSystem('" + sys.name + "');")
      println("phi = STL_Formula('" + phi + "', '" + phi + "')")

      println("gen.type = 'UniStep';")

      println("gen.cp = " + cp + ";")
      println("sys.SetInputGen(gen);")
      println("sys.Sys.tspan = 0:" + T + ";")

      for (k <- 0 until cp) {
        for (InPort(name, i) <- sys.inports) yield {
          println("sys.SetParamRanges({'" + name + "_u" + k + "'}, [" + in.left(i) + " " + in.right(i) + "]);")
        }
      }
      
      println()

      println("problem = FalsificationProblem(sys, phi);")
      println("problem.max_obj_eval = 100;")
      println("problem.max_time = 600; % ten minutes should be enough")
      println("problem.setup_solver('cmaes');")
      println("problem.solver_options.Seed = randi(1000);")
      println("problem.solve();")
      println()

      val us = Signal((0, Vector.zero(sys.inports.length)))
      val ys = Signal((0, Vector.zero(sys.outports.length)))
      val tr = Trace(us, Signal.empty)
      val rs = Robustness(Array((0.0, 0.0)))
      val res = Result(tr, rs)
      val stat = Statistics.empty
      (res, stat)
    }

  }

  case class falsification(controlpoints: Int, solver: String, budget: Int) extends Falsification {
    def identification = "Breach"

    def params = Seq(
      "control points" -> controlpoints,
      "solver" -> solver,
      "budget" -> budget)

    def search(sys: System, _phi: Formula): (Result, Statistics) = sys match {
      case sys @ SimulinkSystem(path, name, _, _, params, vars, load) =>
        val dt = 0.01
        val T = _phi.T

        val in = sys.in
        val inports = sys.inports

        val phi = print(_phi)

        import hybrid.Simulink.eval
        import hybrid.Simulink.get

        // set params and variables
        assert(sys.initialized)

        eval("addpath 'src/main/matlab'")

        eval("model.name = '" + name + "'")
        eval("model.dt = " + dt)

        val inx = for (InPort(name, index) <- inports) yield {
          val min = in.left(index)
          val max = in.right(index)
          val ui = "u" + index
          eval(ui + ".name = '" + name + "'")
          eval(ui + ".range = [" + min + " " + max + "]")
          ui
        }

        eval("inputs = " + inx.mkString("[", " ", "]"))

        eval("phi = STL_Formula('" + phi + "', '" + phi + "')")
        eval("T = " + T)
        eval("solver = '" + solver + "'")
        eval("stages = " + controlpoints)
        eval("samples = " + budget / controlpoints)

        eval("[score, sims, time, t__, u__] = Breach(model, inputs, phi, T, solver, stages, samples)")

        val score: Double = get("score")
        val sims: Double = get("sims")
        val time: Double = get("time")

        val ts: Array[Double] = get("t__")
        val um: Array[Array[Double]] = get("u__")
        val uv = um map (Vector(_: _*))
        val us = ts zip uv

        // fake the result
        val tr = Trace(us, Signal.empty)
        val rs = Robustness(Array((0.0, score)))

        val res = Result(tr, rs)
        val stats = Statistics(sims.toInt, time.toLong, 0)

        (res, stats)

      case _ =>
        throw new Exception("not a simulink model")
    }
  }
}