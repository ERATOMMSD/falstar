package falstar.falsification

import falstar.hybrid.Config
import falstar.hybrid.Constant
import falstar.hybrid.PiecewiseConstant
import falstar.hybrid.Signal
import falstar.hybrid.Signal.SignalOps
import falstar.hybrid.SimulinkSystem
import falstar.hybrid.System
import falstar.hybrid.Trace
import falstar.hybrid.Value
import falstar.linear.Vector
import falstar.mtl.Always
import falstar.mtl.And
import falstar.mtl.Const
import falstar.mtl.DividedBy
import falstar.mtl.Equal
import falstar.mtl.Eventually
import falstar.mtl.False
import falstar.mtl.Formula
import falstar.mtl.Implies
import falstar.mtl.InPort
import falstar.mtl.Less
import falstar.mtl.LessEqual
import falstar.mtl.Minus
import falstar.mtl.Not
import falstar.mtl.NotEqual
import falstar.mtl.Or
import falstar.mtl.Plus
import falstar.mtl.Port
import falstar.mtl.Robustness
import falstar.mtl.Term
import falstar.mtl.Times
import falstar.mtl.Transform
import falstar.mtl.True
import falstar.util.Probability

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

  case class generate(controlpoints: Int, solver: String, budget: Int) extends Falsification {
    def identification = "Breach (generator)"

    def params = Seq(
      "control points" -> controlpoints,
      "solver" -> solver,
      "budget" -> budget)

    def search(sys: System, cfg: Config, phi: Formula): (Result, Statistics) = {
      run(controlpoints, solver, budget, phi, cfg, sys, println)

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

    def search(sys: System, cfg: Config, phi: Formula): (Result, Statistics) = {
      import falstar.hybrid.Simulink.eval
      import falstar.hybrid.Simulink.get
      import falstar.hybrid.Simulink.signal

      // add directory
      // set params and variables
      sys match {
        case sys: SimulinkSystem =>
          assert(sys.initialized)
      }

      run(controlpoints, solver, budget, phi, cfg, sys, eval)

      val score: Double = get("score")
      val sims: Double = get("sims")
      val time: Double = get("time")
      val us = signal("t__", "u__")

      // fake the result
      val tr = Trace(us.collapse, Signal.empty)
      val rs = Robustness(Array((0.0, score)))

      val res = Result(tr, rs)
      val stats = Statistics(sims.toInt, time.toLong, 0)

      (res, stats)
    }
  }

  def run(controlpoints: Int, solver: String, budget: Int, _phi: Formula, cfg: Config, sys: System, eval: String => Unit) = sys match {
    case sys @ SimulinkSystem(path, name, params, inputs, outputs, load) =>
      val dt = 0.01
      val T = _phi.T

      val in = cfg.in(sys.inputs)
      val params = sys.params
      val inports = sys.inports

      val phi = print(_phi)

      eval("InitBreach")

      for (name <- params) {
        cfg.params(name) match {
          case Value(x) =>
            eval(name + " = " + x)
        }
      }

      eval("sys = BreachSimulinkSystem('" + sys.name + "')")
      eval("sys.Sys.tspan = 0:" + T)
      eval("phi = STL_Formula('" + phi + "', '" + phi + "')")

      for (InPort(name, i) <- inports) {
        cfg.inputs(name) match {
          case Value(x) =>
          case Constant(min, max) =>
            eval("gen_" + name + " = constant_signal_gen({'" + name + "'})")
          case PiecewiseConstant(min, max) =>
            eval("gen_" + name + " = fixed_cp_signal_gen({'" + name + "'}, " + controlpoints + ")")
        }
      }

      val gens = inports map ("gen_" + _.name)
      eval("gen = BreachSignalGen(" + gens.mkString("{", ", ", "}") + ")")
      eval("sys.SetInputGen(gen)")

      for (InPort(name, i) <- inports) {
        cfg.inputs(name) match {
          case Value(x) =>
          case Constant(min, max) =>
            eval("sys.SetParamRanges({'" + name + "_u0'}, [" + in.left(i) + " " + in.right(i) + "])")

          case PiecewiseConstant(min, max) =>
            for (k <- 0 until controlpoints) {
              eval("sys.SetParamRanges({'" + name + "_u" + k + "'}, [" + in.left(i) + " " + in.right(i) + "])")
            }
        }
      }

      eval("problem = FalsificationProblem(sys, phi)")
      eval("problem.max_obj_eval = " + budget)
      eval("problem.max_time = 3600") // 1h (can't be 0)
      eval("problem.setup_solver('" + solver + "')")
      eval("problem.solver_options.Seed = " + Probability.seed)
      eval("problem.solve()")

      eval("time = problem.time_spent")
      eval("sims = problem.nb_obj_eval")
      eval("score = problem.obj_best")

      eval("best = problem.BrSet_Best")

    case _ =>
      throw new Exception("not a simulink model")
  }
}
