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

    override def apply(unit: CompilationUnit) = TreeTraverser(unit.body)

    object TreeTraverser extends Traverser {
      override def traverse(tree: global.Tree) = {
        tree match {
          case x @ Literal(Constant(null)) =>
            reporter.error(x.pos, "Unexpected token")

          case x @ ValDef(mods, name, t, _) =>
            if (mods.hasFlag(Flag.DEFAULTINIT) && !notNullDefaults.exists(x => t.tpe <:< x))
              reporter.error(x.pos, s"Field ${name.toString.trim} hasn't been initialized")
            if (topClasses.exists(x => t.tpe =:= x))
              reporter.warning(x.pos, "Unconstrained type")

          case x @ DefDef(m, _, _, _, t, _) =>
            if (! m.hasAccessorFlag && topClasses.exists(x => t.tpe =:= x))
              reporter.warning(x.pos, "Unconstrained type")

          case x @ Select(_, TermName(name)) if name == "isInstanceOf" || name == "asInstanceOf" =>
            reporter.error(x.pos, s"Invalid method $name")

          case x @ Return(_) =>
            reporter.warning(x.pos, "Unexpected token")

          case _ =>
        }
        super.traverse(tree)
      }

      val topClasses = Set(
        definitions.AnyTpe,
        definitions.AnyValTpe,
        definitions.AnyRefTpe)

      val notNullDefaults = Set(
        definitions.BooleanTpe,
        definitions.ByteTpe,
        definitions.ShortTpe,
        definitions.CharTpe,
        definitions.IntTpe,
        definitions.LongTpe,
        definitions.FloatTpe,
        definitions.DoubleTpe,
        definitions.UnitTpe)
    }
  }
}
