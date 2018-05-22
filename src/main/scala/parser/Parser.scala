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

sealed trait Command
case object Quit extends Command
case class Falsify(search: Falsification, name: String, sys: System, phi: Formula, seed: Option[Long], repeat: Int, log: Option[String]) extends Command
case class Simulate(name: String, sys: System, phi: Formula, us: Signal, T: Time) extends Command
case class Robustness(phi: Formula, us: Signal, ys: Signal, T: Time) extends Command

class Parser {
  // case class State(search: Falsification, system: System, defines: Map[String, Syntax], systems: Map[String, System])
  // var stack: List[State] = Nil

  var search: Falsification = null
  var name: String = null
  var system: System = null
  var defines = Map[String, Syntax]()
  var systems = Map[String, System]()

  // for experiments
  var seed: Option[Long] = None
  var repeat: Int = 1
  var log: Option[String] = None

  object Number {
    def unapply(node: Syntax): Option[Double] = node match {
      case Literal(value: Double) => Some(value)
      case Identifier(name) if defines contains name => unapply(defines(name))
      case _ => None
    }
  }

  def splitFilename(name: String) = {
    val dot = name.lastIndexOf('.')
    if (dot > 0) name.substring(0, dot)
    else name
  }

  def defineSystem(name: String, system: Syntax, inputs: Seq[Syntax], outputs: Seq[Syntax], options: Seq[Syntax]) = {
    val ins = inputs map {
      case Node(Identifier(name), Number(min), Number(max)) =>
        name -> (min, max)
    }

    val outs = outputs map {
      case Node(Identifier(name)) =>
        name
    }

    var params = Seq[(String, String)]()
    var vars = Seq[(String, String)]()
    var load = Seq[String]()

    options map {
      case Node(Keyword("params"), ps @ _*) =>
        params ++= ps map {
          case Node(Identifier(key), Literal(value)) => (key, value.toString)
        }
      case Node(Keyword("vars"), vs @ _*) =>
        vars ++= vs map {
          case Node(Identifier(name), Literal(value)) => (name, value.toString)
        }
      case Node(Keyword("load"), ms @ _*) =>
        load ++= ms map {
          case Literal(file: String) => file
        }
    }

    val sys = system match {
      case Node(Keyword("simulink"), Literal(name: String)) =>
        val file = new File(name)
        val path = file.getParent
        val model = splitFilename(file.getName)
        SimulinkSystem(path, model, ins, outs, params, vars, load)
    }

    assert(!(systems contains name))
    systems += name -> sys
  }

  def term(ports: Map[String, Port], tm: Syntax): Term = tm match {
    case Node(Keyword("abs"), arg) => Transform(term(ports, arg), Math.abs, "abs")
    case Node(Keyword("+"), left, right) => term(ports, left) + term(ports, right)
    case Node(Keyword("-"), left, right) => term(ports, left) - term(ports, right)
    case Node(Keyword("*"), left, right) => term(ports, left) * term(ports, right)
    case Node(Keyword("/"), left, right) => term(ports, left) / term(ports, right)
    case Identifier(name) if defines contains name => term(ports, defines(name))
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

    case Identifier(name) if defines contains name =>
      formula(ports, defines(name))
  }

  def formula(phi: Syntax): Formula = {
    val inports = Map(system.inports.map { port => (port.name, port) }: _*)
    val outports = Map(system.outports.map { port => (port.name, port) }: _*)
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
    case Node(Keyword("define"), Identifier(name), syntax) =>
      defines += name -> syntax
      Seq()

    case Node(Keyword("define-system"), Identifier(name), system, Node(Keyword("inputs"), inputs @ _*), Node(Keyword("outputs"), outputs @ _*), options @ _*) =>
      defineSystem(name, system, inputs, outputs, options)
      Seq()

    case Node(Keyword("set-system"), Identifier(id)) =>
      name = id
      system = systems(id)
      Seq()

    case Node(Keyword("set-solver"), Identifier("adaptive"), Node(controlpoints @ _*), Literal(exploration: Double), Literal(budget: Double)) =>
      val levels = controlpoints map {
        case Literal(cp: Double) => cp.toInt
      }

      search = Adaptive.falsification(levels, exploration, budget.toInt)
      Seq()

    case Node(Keyword("set-solver"), Identifier("breach"), Literal(controlpoints: Double), Identifier(solver), Literal(budget: Double)) =>
      search = Breach.falsification(controlpoints.toInt, solver, budget.toInt)
      Seq()
      
    case Node(Keyword("set-solver"), Identifier("breachprinter")) =>
      search = Breach.dummy
      Seq()

    case Node(Keyword("falsify"), phis @ _*) =>
      phis map { phi => Falsify(search, name, system, formula(phi), seed, repeat, log) }

    case Node(Keyword("simulate"), Number(time), phi, input @ _*) =>
      Seq(Simulate(name, system, formula(phi), signal(input), time))

    case Node(Keyword("robustness"), Number(time), phi, input @ _*) =>
      Seq(Robustness(formula(phi), Signal((0: Time, Vector())), signal(input), time))

    case Node(Keyword("set-repeat"), Literal(n: Double)) =>
      repeat = n.toInt
      Seq()

    case Node(Keyword("set-seed"), Literal(n: Double)) =>
      seed = Some(n.toLong)
      Seq()

    case Node(Keyword("clear-seed")) =>
      seed = None
      Seq()

    case Node(Keyword("set-log"), Literal(name: String)) =>
      log = Some(name)
      Seq()

    case Node(Keyword("clear-log"), Literal(name: String)) =>
      log = None
      Seq()

    case Node(Keyword("quit")) =>
      Seq(Quit)

    /* case Node(Keyword("push")) =>
      val state = State(search, system, defines, systems)
      stack = state :: stack
      Seq()

    case Node(Keyword("pop")) =>
      val state :: rest = stack
      search = state.search
      system = state.system
      defines = state.defines
      systems = state.systems
      stack = rest
      Seq() */
  }

  def parse(syntax: Syntax): Seq[Command] = {
    val Node(args @ _*) = syntax
    args flatMap top
  }
}