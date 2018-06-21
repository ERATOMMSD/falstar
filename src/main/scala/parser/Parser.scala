package parser

import java.io.File

import falsification.Adaptive
import falsification.Breach
import falsification.Falsification
import hybrid.Signal
import hybrid.SimulinkSystem
import hybrid.System
import hybrid.Time
import linear.Vector
import mtl.Const
import mtl.Formula
import mtl.Port
import mtl.Term
import mtl.□
import mtl.◇
import mtl.Transform
import falsification.STaliro
import falsification.LaTeX
import falsification.UniformRandom
import hybrid.Config
import hybrid.Value
import hybrid.PiecewiseConstant
import hybrid.Constant

sealed trait Command
case object Flush extends Command
case object Quit extends Command
case class Falsify(search: Falsification, sys: System, cfg: Config, phi: Formula, seed: Option[Long], repeat: Int, log: Option[String]) extends Command
case class Simulate(sys: System, phi: Formula, us: Signal, T: Time) extends Command
case class Robustness(phi: Formula, us: Signal, ys: Signal, T: Time) extends Command

class Parser {
  case class State(
    var search: Falsification,
    var system: System,
    var config: Config,
    var defines: Map[String, Syntax],
    var systems: Map[String, (System, Config)],

    // for experiments
    var seed: Option[Long],
    var repeat: Int,
    var log: Option[String])

  object State {
    def empty = State(null, null, null, Map(), Map(), None, 1, None)
  }

  var stack = List(State.empty)
  def state = stack.head

  object Number {
    def unapply(node: Syntax): Option[Double] = node match {
      case Literal(value: Double) => Some(value)
      case Identifier(name) if state.defines contains name => unapply(state.defines(name))
      case _ => None
    }
  }

  def identifiers(nodes: Seq[Syntax]) = {
    nodes map {
      case Identifier(name) =>
        name
    }
  }

  def splitFilename(name: String) = {
    val dot = name.lastIndexOf('.')
    if (dot > 0) name.substring(0, dot)
    else name
  }

  def configureSystem(sys: System, cfg: Config, _config: Seq[Syntax]) = {
    var params = cfg.params
    var inputs = cfg.inputs
    var options = cfg.options

    _config map {
      case Node(Keyword("constant"), Identifier(name), Number(value)) if sys.params contains name =>
        params = params + (name -> Value(value))
      case Node(Keyword("constant"), Identifier(name), Number(value)) if sys.inputs contains name =>
        inputs = inputs + (name -> Value(value))
      case Node(Keyword("constant"), Identifier(name), Number(min), Number(max)) if sys.params contains name =>
        params = params + (name -> Constant(min, max))
      case Node(Keyword("constant"), Identifier(name), Number(min), Number(max)) if sys.inputs contains name =>
        inputs = inputs + (name -> Constant(min, max))
      case Node(Keyword("piecewise-constant"), Identifier(name), Number(min), Number(max)) if sys.inputs contains name =>
        inputs = inputs + (name -> PiecewiseConstant(min, max))
      case Node(Identifier(name), Literal(value)) =>
        options = options + (name -> value)
    }

    Config(params, inputs, options)
  }

  def defineSystem(name: String, system: Syntax, _params: Seq[Syntax], _inputs: Seq[Syntax], _outputs: Seq[Syntax], _config: Seq[Syntax]) = {
    val params = identifiers(_params)
    val inputs = identifiers(_inputs)
    val outputs = identifiers(_outputs)

    val sys = system match {
      case Node(Keyword("simulink"), Literal(name: String)) =>
        val file = new File(name)
        val path = file.getParent
        val model = splitFilename(file.getName)
        SimulinkSystem(path, model, params, inputs, outputs, Seq())
        
      case Node(Keyword("simulink"), Literal(name: String), Literal(load: String)) =>
        val file = new File(name)
        val path = file.getParent
        val model = splitFilename(file.getName)
        SimulinkSystem(path, model, params, inputs, outputs, Seq(load))
    }

    val cfg = configureSystem(sys, Config.empty, _config)

    assert(!(state.systems contains name))
    state.systems += name -> (sys, cfg)
  }

  def term(ports: Map[String, Port], tm: Syntax): Term = tm match {
    case Node(Keyword("abs"), arg) => Transform(term(ports, arg), Math.abs, "abs")
    case Node(Keyword("+"), left, right) => term(ports, left) + term(ports, right)
    case Node(Keyword("-"), left, right) => term(ports, left) - term(ports, right)
    case Node(Keyword("*"), left, right) => term(ports, left) * term(ports, right)
    case Node(Keyword("/"), left, right) => term(ports, left) / term(ports, right)
    case Identifier(name) if state.defines contains name => term(ports, state.defines(name))
    case Identifier(name) if ports contains name => ports(name)
    case Identifier(name) => sys.error("unknown identifier: " + name + " in " + ports.keys.mkString(", "))
    case Literal(value: Double) => Const(value)
  }

  def formulas(ports: Map[String, Port], phis: Seq[Syntax]): Seq[Formula] = {
    phis map (formula(ports, _))
  }

  def formula(ports: Map[String, Port], phi: Syntax): Formula = phi match {
    case Keyword("true") => mtl.True
    case Keyword("false") => mtl.False

    case Node(Keyword("in"), tm, min, max) => term(ports, tm) in (term(ports, min), term(ports, max))

    case Node(Keyword("<"), left, right) => term(ports, left) < term(ports, right)
    case Node(Keyword(">"), left, right) => term(ports, left) > term(ports, right)
    case Node(Keyword("<="), left, right) => term(ports, left) >= term(ports, right)
    case Node(Keyword(">="), left, right) => term(ports, left) <= term(ports, right)
    case Node(Keyword("=="), left, right) => term(ports, left) === term(ports, right)
    case Node(Keyword("!="), left, right) => term(ports, left) !== term(ports, right)

    case Node(Keyword("!"), phi) => !formula(ports, phi)
    case Node(Keyword("&&"), phis @ _*) => formulas(ports, phis).fold(mtl.True: Formula)(_ && _)
    case Node(Keyword("||"), phis @ _*) => formulas(ports, phis).fold(mtl.False: Formula)(_ || _)
    case Node(Keyword("=>"), phi, psi) => formula(ports, phi) ==> formula(ports, psi)

    case Node(Keyword("not"), phi) => !formula(ports, phi)
    case Node(Keyword("and"), phis @ _*) => formulas(ports, phis).fold(mtl.True: Formula)(_ && _)
    case Node(Keyword("or"), phis @ _*) => formulas(ports, phis).fold(mtl.False: Formula)(_ || _)
    case Node(Keyword("implies"), phi, psi) => formula(ports, phi) ==> formula(ports, psi)

    case Node(Keyword("always"), Node(Number(from), Number(to)), psi) => □(from, to, formula(ports, psi))
    case Node(Keyword("eventually"), Node(Number(from), Number(to)), psi) => ◇(from, to, formula(ports, psi))

    case Identifier(name) if state.defines contains name =>
      formula(ports, state.defines(name))
  }

  def formula(phi: Syntax): Formula = {
    val inports = Map(state.system.inports.map { port => (port.name, port) }: _*)
    val outports = Map(state.system.outports.map { port => (port.name, port) }: _*)
    formula(inports ++ outports, phi)
  }

  def controlpoint(syntax: Syntax) = syntax match {
    case Node(Literal(t: Time), Node(vs @ _*)) =>
      val x = Vector(vs map { case Literal(xi: Double) => xi }: _*)
      (t, x)
  }

  def signal(input: Seq[Syntax]): Signal = {
    Signal((input map controlpoint): _*)
  }

  def top(syntax: Syntax): Seq[Command] = syntax match {
    case Node(Keyword("include"), Literal(file: String)) =>
      val node = read(new File(file))
      parse(node)

    case Node(Keyword("define"), Identifier(name), syntax) =>
      state.defines += name -> syntax
      Seq()

    case Node(Keyword("define-system"), Identifier(name), system, Node(Keyword("parameters"), params @ _*), Node(Keyword("inputs"), inputs @ _*), Node(Keyword("outputs"), outputs @ _*), config @ _*) =>
      defineSystem(name, system, params, inputs, outputs, config)
      Seq()

    case Node(Keyword("select-system"), Identifier(id), _cfg @ _*) =>
      val (sys, cfg) = state.systems(id)
      state.system = sys
      state.config = configureSystem(sys, cfg, _cfg)
      Seq()

    case Node(Keyword("set-solver"), Identifier("random"), Literal(controlpoints: Double), Literal(budget: Double)) =>
      state.search = UniformRandom.falsification(controlpoints.toInt, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("adaptive"), Node(controlpoints @ _*), Literal(exploration: Double), Literal(budget: Double)) =>
      val levels = controlpoints map {
        case Literal(cp: Double) => cp.toInt
      }

      state.search = Adaptive.falsification(levels, exploration, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("breach"), Literal(controlpoints: Double), Identifier(solver), Literal(budget: Double)) =>
      state.search = Breach.falsification(controlpoints.toInt, solver, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("latexprinter")) =>
      state.search = LaTeX.dummy
      Seq()

    case Node(Keyword("set-solver"), Identifier("breachprinter")) =>
      state.search = Breach.dummy
      Seq()

    case Node(Keyword("set-solver"), Identifier("staliroprinter"), Literal(prefix: String)) =>
      state.search = STaliro.dummy(prefix)
      Seq()

    case Node(Keyword("falsify"), phis @ _*) =>
      phis map { phi => Falsify(state.search, state.system, state.config, formula(phi), state.seed, state.repeat, state.log) }

    case Node(Keyword("simulate"), Number(time), phi, input @ _*) =>
      Seq(Simulate(state.system, formula(phi), signal(input), time))

    case Node(Keyword("robustness"), Number(time), phi, input @ _*) =>
      Seq(Robustness(formula(phi), Signal((0: Time, Vector())), signal(input), time))

    case Node(Keyword("set-repeat"), Literal(n: Double)) =>
      state.repeat = n.toInt
      Seq()

    case Node(Keyword("set-seed"), Literal(n: Double)) =>
      state.seed = Some(n.toLong)
      Seq()

    case Node(Keyword("clear-seed")) =>
      state.seed = None
      Seq()

    case Node(Keyword("set-log"), Literal(name: String)) =>
      state.log = Some(name)
      Seq()

    case Node(Keyword("clear-log"), Literal(name: String)) =>
      state.log = None
      Seq()

    case Node(Keyword("flush-log")) =>
      Seq(Flush)
      
    case Node(Keyword("quit")) =>
      Seq(Quit)

    case Node(Keyword("push")) =>
      stack = state.copy() :: stack
      Seq()

    case Node(Keyword("pop")) =>
      val _ :: rest = stack
      stack = rest
      Seq()
  }

  def parse(syntax: Syntax): Seq[Command] = {
    val Node(args @ _*) = syntax
    args flatMap top
  }
}