package timzh.stricter

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(override val global: Global) extends Plugin {
  override val name = "stricter"
  override val description = "forbids legacy stuff"
  override val components = List[PluginComponent](new Component(global, name))
}

private class Component(override val global: Global, override val phaseName: String) extends PluginComponent {
  import global._

  override val runsAfter = List("typer")

  override def newPhase(prev: Phase) = new StdPhase(prev) {
    override def name = phaseName

    override def apply(unit: CompilationUnit) = {
      handleNulls(unit.body)
      handleDefaultNulls(unit.body)
      handleTypeCasts(unit.body)
    }

    def handleNulls(body: Global#Tree) = {
      for (tree @ Literal(Constant(null)) <- body)
        global.reporter.error(tree.pos, "Unexpected token")
    }

    def handleDefaultNulls(body: Global#Tree) = {
      val notNullDefaults = Set(
        definitions.BooleanClass.tpe,
        definitions.ByteClass.tpe,
        definitions.ShortClass.tpe,
        definitions.CharClass.tpe,
        definitions.IntClass.tpe,
        definitions.LongClass.tpe,
        definitions.FloatClass.tpe,
        definitions.DoubleClass.tpe,
        definitions.UnitClass.tpe)

      for {
        tree @ ValDef(mods, name, t, _) <- body
        if mods.hasFlag(Flag.DEFAULTINIT) && ! notNullDefaults.exists(x => t.tpe <:< x)
      } {
        global.reporter.error(tree.pos, s"Field ${name.toString.trim} hasn't been initialized")
      }
    }

    def handleTypeCasts(body: Global#Tree) = {
      for (tree @ Select(_, TermName(name)) <- body if name == "isInstanceOf" || name == "asInstanceOf") {
        global.reporter.error(tree.pos, s"Invalid method $name")
      }
    }
  }
}
