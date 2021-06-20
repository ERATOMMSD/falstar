package falstar.parser

import java.io.File
import scala.util.DynamicVariable

import falstar.falsification.Adaptive
import falstar.falsification.Breach
import falstar.falsification.Falsification
import falstar.hybrid.Signal
import falstar.hybrid.SimulinkSystem
import falstar.hybrid.System
import falstar.hybrid.Time
import falstar.linear.Vector
import falstar.mtl.Const
import falstar.mtl.Formula
import falstar.mtl.Port
import falstar.mtl.Term
import falstar.mtl.□
import falstar.mtl.◇
import falstar.mtl.○
import falstar.mtl.Transform
import falstar.falsification.STaliro
import falstar.falsification.LaTeX
import falstar.falsification.UniformRandom
import falstar.hybrid.Config
import falstar.hybrid.Value
import falstar.hybrid.PiecewiseConstant
import falstar.hybrid.Constant
import falstar.hybrid.Input
import falstar.hybrid.MatlabSystem
import falstar.falsification.Optimization

sealed trait Command
case object Flush extends Command
case object Quit extends Command

case class Falsify(search: Falsification, sys: System, cfg: Config, phi: Formula, seed: Option[Long], repeat: Int, notes: Seq[(String, Any)], log: Option[String], report: Option[String]) extends Command
case class Validate(log: Option[String], report: Option[String], parser: Parser) extends Command
case class Simulate(sys: System, phi: Formula, ps: Input, us: Signal, T: Time) extends Command
case class Robustness(phi: Formula, us: Signal, ys: Signal, T: Time) extends Command

object Parser {
  case class State(
    var search: Falsification,
    var system: System,
    var config: Config,
    var defines: Map[String, Syntax],
    var macros: Map[String, (Seq[String], Syntax)],
    var systems: Map[String, (System, Config)],
    var requirements: Map[String, Seq[Formula]],

    // for experiments
    var seed: Option[Long],
    var repeat: Int,
    var log: Option[String],
    var report: Option[String],
    var notes: Seq[(String, Any)])

  object State {
    def empty = State(null, null, null, Map(), Map(), Map(), Map(), None, 1, None, None, Seq())
  }
}

class Parser(_directory: String) {
  outer =>
  import Parser._
  object directory extends DynamicVariable(_directory)

  var stack = List(State.empty)
  def state = stack.head

  def copy = new Parser(_directory) { stack = List(outer.state.copy()); }

  def expand(node: Syntax, env: Map[String, Syntax]): Syntax = node match {
    case Identifier(name) if env contains name =>
      env(name)

    case Node(Identifier(name), args @ _*) if state.macros contains name =>
      val (formals, body) = state.macros(name)
      val lex = (formals zip expand(args, env))
      expand(body, env ++ lex)

    case Node(args @ _*) =>
      Node(expand(args, env): _*)

    case _ =>
      node
  }

  def expand(nodes: Seq[Syntax], env: Map[String, Syntax]): Seq[Syntax] = {
    nodes map (expand(_, env))
  }

  object Number {
    def unapply(node: Syntax): Option[Double] = node match {
      case Literal(value: Double) =>
        Some(value)
      case _ =>
        None
    }
  }

  object Path {
    def unapply(node: Syntax): Option[String] = node match {
      case Literal(value: String) =>
        Some(directory.value + "/" + value)
      case _ =>
        None
    }
  }

  def identifiers(nodes: Seq[Syntax]) = {
    nodes map {
      case Identifier(name) =>
        name
      case node =>
        sys.error("not an identifier: " + node)
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
      case Node(Keyword("simulink"), Path(name)) =>
        val file = new File(name)
        val path = file.getParent
        val model = splitFilename(file.getName)
        SimulinkSystem(path, model, params, inputs, outputs, Seq())

      case Node(Keyword("simulink"), Path(name), Literal(load: String)) =>
        val file = new File(name)
        val path = file.getParent
        val model = splitFilename(file.getName)
        SimulinkSystem(path, model, params, inputs, outputs, Seq(load))

      case Node(Keyword("matlab"), Literal(name: String), Path(path), Literal(init: String), Literal(run: String)) =>
        MatlabSystem(path, name, init, run, params, inputs, outputs)
    }

    val cfg = configureSystem(sys, Config.empty, _config)

    assert(!(state.systems contains name))
    state.systems += name -> (sys, cfg)
  }

  def term(ports: Map[String, Port], tm: Syntax): Term = tm match {
    case Literal(value: Double) => Const(value)
    case Node(Keyword("abs"), arg) => Transform(term(ports, arg), Math.abs, "abs")
    case Node(Keyword("+"), left, right) => term(ports, left) + term(ports, right)
    case Node(Keyword("-"), left, right) => term(ports, left) - term(ports, right)
    case Node(Keyword("*"), left, right) => term(ports, left) * term(ports, right)
    case Node(Keyword("/"), left, right) => term(ports, left) / term(ports, right)

    case Identifier(name) if ports contains name =>
      ports(name)

    case Identifier(name) =>
      sys.error("unknown identifier: " + name + " in " + ports.keys.mkString(", "))

    case _ =>
      sys.error("not a term: " + tm)
  }

  def formulas(ports: Map[String, Port], phis: Seq[Syntax]): Seq[Formula] = {
    phis map (formula(ports, _))
  }

  def formula(ports: Map[String, Port], phi: Syntax): Formula = phi match {
    case Keyword("true") => falstar.mtl.True
    case Keyword("false") => falstar.mtl.False

    case Node(Keyword("in"), tm, min, max) => term(ports, tm) in (term(ports, min), term(ports, max))

    case Node(Keyword("<"), left, right) => term(ports, left) < term(ports, right)
    case Node(Keyword(">"), left, right) => term(ports, left) > term(ports, right)
    case Node(Keyword("<="), left, right) => term(ports, left) <= term(ports, right)
    case Node(Keyword(">="), left, right) => term(ports, left) >= term(ports, right)
    case Node(Keyword("=="), left, right) => term(ports, left) === term(ports, right)
    case Node(Keyword("!="), left, right) => term(ports, left) !== term(ports, right)

    case Node(Keyword("!"), phi) => !formula(ports, phi)
    case Node(Keyword("&&"), phis @ _*) => formulas(ports, phis).fold(falstar.mtl.True: Formula)(_ && _)
    case Node(Keyword("||"), phis @ _*) => formulas(ports, phis).fold(falstar.mtl.False: Formula)(_ || _)
    case Node(Keyword("=>"), phi, psi) => formula(ports, phi) ==> formula(ports, psi)

    case Node(Keyword("not"), phi) => !formula(ports, phi)
    case Node(Keyword("and"), phis @ _*) => formulas(ports, phis).fold(falstar.mtl.True: Formula)(_ && _)
    case Node(Keyword("or"), phis @ _*) => formulas(ports, phis).fold(falstar.mtl.False: Formula)(_ || _)
    case Node(Keyword("implies"), phi, psi) => formula(ports, phi) ==> formula(ports, psi)

    case Node(Keyword("next"), phi) => ○(formula(phi))
    case Node(Keyword("always"), Node(Number(from), Number(to)), psi) => □(from, to, formula(ports, psi))
    case Node(Keyword("eventually"), Node(Number(from), Number(to)), psi) => ◇(from, to, formula(ports, psi))

    case Identifier(name) =>
      sys.error("unknown identifier: " + name + " in " + ports.keys.mkString(", "))

    case _ =>
      sys.error("not a formula: " + phi)
  }

  def formula(phi: Syntax): Formula = {
    val inports = Map(state.system.inports.map { port => (port.name, port) }: _*)
    val outports = Map(state.system.outports.map { port => (port.name, port) }: _*)
    formula(inports ++ outports, expand(phi, state.defines))
  }

  def vector(syntax: Syntax) = syntax match {
    case Node(vs @ _*) =>
      Vector(vs map {
        case Literal(xi: Double) => xi
        case v => sys.error("not a number in vector: " + v)
      }: _*)
    case node =>
      sys.error("not a vector" + node)
  }

  def controlpoint(syntax: Syntax) = syntax match {
    case Node(Literal(t: Time), x) =>
      (t, vector(x))
  }

  def signal(input: Seq[Syntax]): Signal = {
    Signal((input map controlpoint): _*)
  }

  def note(syntax: Syntax) = syntax match {
    case Node(Literal(key: String), Literal(value: Integer)) =>
      (key, value)
    case Node(Literal(key: String), Literal(value: String)) =>
      (key, value)
  }

  def top(syntax: Syntax): Seq[Command] = expand(syntax, state.defines) match {
    case Node(Keyword("include"), Path(name)) =>
      val file = new File(name)
      val node = read(file)
      directory.withValue(file.getParent) {
        parse(node)
      }

    case Node(Keyword("define"), Identifier(name), body) =>
      state.defines += name -> body
      Seq()

    case Node(Keyword("define"), Identifier(name), Node(_formals @ _*), body) =>
      val formals = identifiers(_formals)
      state.macros += name -> (formals, body)
      Seq()

    case Node(Keyword("notes"), what @ _*) =>
      state.notes = what map note
      Seq()

    case Node(Keyword("print"), Literal("formula"), what @ _*) =>
      for(item <- what) println(formula(item))
      Seq()
    
    case Node(Keyword("define-system"), Identifier(name), system, Node(Keyword("parameters"), params @ _*), Node(Keyword("inputs"), inputs @ _*), Node(Keyword("outputs"), outputs @ _*), config @ _*) =>
      defineSystem(name, system, params, inputs, outputs, config)
      Seq()

    case Node(Keyword("select-system"), Identifier(id), _cfg @ _*) =>
      val (sys, cfg) = state.systems(id)
      state.system = sys
      state.config = configureSystem(sys, cfg, _cfg)
      Seq()

    case Node(Keyword("set-solver"), Identifier("random"), Number(controlpoints), Number(budget)) =>
      state.search = UniformRandom.falsification(controlpoints.toInt, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("adaptive"), Node(controlpoints @ _*), Number(exploration), Number(budget)) =>
      val levels = controlpoints map {
        case Number(cp) => cp.toInt
      }

      val rest = 1.0 - exploration
      val uniform = rest / 3
      val prefix = rest / 3
      val suffix = rest / 3

      state.search = Adaptive.falsification(levels, exploration, uniform, prefix, suffix, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("adaptive"), Node(controlpoints @ _*), Number(exploration), Number(uniform), Number(prefix), Number(suffix), Number(budget)) =>
      val levels = controlpoints map {
        case Number(cp) => cp.toInt
      }

      state.search = Adaptive.falsification(levels, exploration, uniform, prefix, suffix, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("nelder-mead"), Number(budget)) =>
      state.search = Optimization.falsification(budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("breach"), Number(controlpoints), Identifier(solver), Number(budget)) =>
      state.search = Breach.falsification(controlpoints.toInt, solver, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("breach-generator"), Number(controlpoints), Identifier(solver), Number(budget)) =>
      state.search = Breach.generate(controlpoints.toInt, solver, budget.toInt)
      Seq()

    case Node(Keyword("latex"), phis @ _*) =>
      for (phi <- phis) phi match {
        case Identifier(name) =>
          println(name + " = " + LaTeX.print(formula(phi)))
        case _ =>
          println(LaTeX.print(formula(phi)))
      }
      Seq()

    case Node(Keyword("set-solver"), Identifier("staliro-printer"), Literal(prefix: String)) =>
      state.search = STaliro.dummy(prefix)
      Seq()

    case Node(Keyword("set-requirements"), phis @ _*) =>
      val name = state.system.name
      state.requirements += name -> (phis map formula)
      Seq()

    case Node(Keyword("falsify")) =>
      val name = state.system.name
      state.requirements(name) map { phi => Falsify(state.search, state.system, state.config, phi, state.seed, state.repeat, state.notes, state.log, state.report) }

    case Node(Keyword("falsify"), phis @ _*) =>
      phis map { phi => Falsify(state.search, state.system, state.config, formula(phi), state.seed, state.repeat, state.notes, state.log, state.report) }

    case Node(Keyword("validate")) =>
      val cmd = Validate(state.log, state.report, this.copy)
      Seq(cmd)

    case Node(Keyword("simulate"), Number(time), phi, params, input @ _*) =>
      Seq(Simulate(state.system, formula(phi), vector(params), signal(input), time))

    case Node(Keyword("robustness"), Number(time), phi, input @ _*) =>
      Seq(Robustness(formula(phi), Signal((0: Time, Vector())), signal(input), time))
      
    case Node(Keyword("set-repeat"), Number(n)) =>
      state.repeat = n.toInt
      Seq()

    case Node(Keyword("set-seed"), Number(n)) =>
      state.seed = Some(n.toLong)
      Seq()

    case Node(Keyword("clear-seed")) =>
      state.seed = None
      Seq()

    case Node(Keyword("set-log"), Path(name)) =>
      state.log = Some(name)
      Seq()

    case Node(Keyword("clear-log")) =>
      state.log = None
      Seq()

    case Node(Keyword("flush-log")) =>
      Seq(Flush)

    case Node(Keyword("set-report"), Path(name)) =>
      state.report = Some(name)
      Seq()

    case Node(Keyword("clear-report")) =>
      state.report = None
      Seq()

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