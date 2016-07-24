package me.herbix.ts.logic.card

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * Created by Chaofan on 2016/7/24.
  */
abstract class StepAnnotation(val step: Int, val state: Any, val meta: Any*) extends StaticAnnotation

@compileTimeOnly("")
class prepare extends StepAnnotation(0, null) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implPrepare
}

@compileTimeOnly("")
class step1(state: Any, meta: Any*) extends StepAnnotation(1, state, meta) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implStep1
}

@compileTimeOnly("")
class step2(state: Any, meta: Any*) extends StepAnnotation(2, state, meta) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implStep2
}

@compileTimeOnly("")
class step3(state: Any, meta: Any*) extends StepAnnotation(3, state, meta) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implStep3
}

@compileTimeOnly("")
class step4(state: Any, meta: Any*) extends StepAnnotation(4, state, meta) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implStep4
}

@compileTimeOnly("")
class step5(state: Any, meta: Any*) extends StepAnnotation(5, state, meta) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implStep5
}

@compileTimeOnly("")
class step6(state: Any, meta: Any*) extends StepAnnotation(6, state, meta) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implStep6
}

@compileTimeOnly("")
class step7(state: Any, meta: Any*) extends StepAnnotation(7, state, meta) {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.implStep7
}

object StepAnnotationMacro {

  def implPrepare(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(0)(c)(annottees).asInstanceOf[c.Expr[Any]]
  def implStep1(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(1)(c)(annottees).asInstanceOf[c.Expr[Any]]
  def implStep2(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(2)(c)(annottees).asInstanceOf[c.Expr[Any]]
  def implStep3(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(3)(c)(annottees).asInstanceOf[c.Expr[Any]]
  def implStep4(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(4)(c)(annottees).asInstanceOf[c.Expr[Any]]
  def implStep5(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(5)(c)(annottees).asInstanceOf[c.Expr[Any]]
  def implStep6(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(6)(c)(annottees).asInstanceOf[c.Expr[Any]]
  def implStep7(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = implStep(7)(c)(annottees).asInstanceOf[c.Expr[Any]]

  def implStep(step: Int)(c: whitebox.Context)(annottees: Seq[c.Expr[Any]]): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList

    val annotationParams = c.prefix.tree match {
      case q"new $annotationName(..$annotationParams)" => annotationParams
      case q"new $annotationName()" => Seq()
    }

    val result = inputs match {
      case (method: DefDef) :: rest => method match {
        case q"def $name(): $returnType = { ..$body }" =>
          q"""
            addStepInfo($step, $name, ..$annotationParams)
            def $name(game: Game, faction: Faction, input: Operation): Int = {
              ..$body
              ..${if (returnType.toString != "Int") Seq(q"${step + 1}") else Seq()}
            }
            """
        case q"def $name($game: Game): $returnType = { ..$body }" =>
          q"""
            addStepInfo($step, $name, ..$annotationParams)
            def $name($game: Game, faction: Faction, input: Operation): Int = {
              ..$body
              ..${if (returnType.toString != "Int") Seq(q"${step + 1}") else Seq()}
            }
            """
        case q"def $name($game: Game, $input: Operation): $returnType = { ..$body }" =>
          q"""
            addStepInfo($step, $name, ..$annotationParams)
            def $name($game: Game, faction: Faction, input: Operation): Int = {
              ..$body
              ..${if (returnType.toString != "Int") Seq(q"${step + 1}") else Seq()}
            }
            """
        case q"def $name($game: Game, $faction: Faction, $input: Operation): $returnType = { ..$body }" =>
          q"""
            addStepInfo($step, $name, ..$annotationParams)
            def $name($game: Game, $faction: Faction, $input: Operation): Int = {
              ..$body
              ..${if (returnType.toString != "Int") Seq(q"${step + 1}") else Seq()}
            }
            """
        case _ => c.abort(c.enclosingPosition, "@prepare or @stepN Method param error")
      }
      case _ => c.abort(c.enclosingPosition, "@prepare or @stepN Can only apply to methods")
    }
    c.Expr[Any](result)
  }
}
