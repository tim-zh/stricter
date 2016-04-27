package timzh.stricter

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(override val global: Global) extends Plugin {
  override val name = "stricter"
  override val description = "forbids outdated stuff"
  override val components = List[PluginComponent](new Component(global, name))
}

private class Component(override val global: Global, override val phaseName: String) extends PluginComponent {
  import global._

  override val runsAfter = List("typer")

  override def newPhase(prev: Phase) = new NpePhase(prev)

  class NpePhase(prev: Phase) extends StdPhase(prev) {
    private val notNullDefaults = Set(
      definitions.BooleanClass.tpe,
      definitions.ByteClass.tpe,
      definitions.ShortClass.tpe,
      definitions.CharClass.tpe,
      definitions.IntClass.tpe,
      definitions.LongClass.tpe,
      definitions.FloatClass.tpe,
      definitions.DoubleClass.tpe,
      definitions.UnitClass.tpe)

    override def name = phaseName
    override def apply(unit: CompilationUnit) = {
      for (tree @ Literal(Constant(null)) <- unit.body)
        global.reporter.error(tree.pos, "Unexpected token")

      for {
        tree @ ValDef(mods, name, t, _) <- unit.body
        if mods.hasFlag(Flag.DEFAULTINIT) && ! notNullDefaults.exists(x => t.tpe <:< x)
      } {
        global.reporter.error(tree.pos, s"Field ${name.toString.trim} hasn't been initialized")
      }
    }
  }
}
