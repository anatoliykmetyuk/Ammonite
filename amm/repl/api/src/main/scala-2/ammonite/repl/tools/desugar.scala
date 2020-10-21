package ammonite.repl.tools

import ownerUtil._
import scala.language.experimental.macros

object desugar{
  def transformer(c: Context)(expr: c.Expr[Any]): c.Expr[Desugared] = {
    import c.universe._
    c.Expr[Desugared](
      q"ammonite.repl.api.SourceBridge.value.desugarImpl(${c.universe.showCode(expr.tree)})"
    )
  }

  def apply(expr: Any): Desugared = macro transformer
}