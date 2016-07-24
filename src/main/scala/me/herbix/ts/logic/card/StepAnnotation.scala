package me.herbix.ts.logic.card

import me.herbix.ts.logic.State._

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/**
  * Created by Chaofan on 2016/7/24.
  */
@compileTimeOnly("")
abstract class StepAnnotation(val step: Int, val state: State, val meta: Any*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro StepAnnotationMacro.impl
}
@compileTimeOnly("")
class prepare extends StepAnnotation(0, null)
@compileTimeOnly("")
class step1(state: State, meta: Any*) extends StepAnnotation(1, state, meta)
@compileTimeOnly("")
class step2(state: State, meta: Any*) extends StepAnnotation(2, state, meta)
@compileTimeOnly("")
class step3(state: State, meta: Any*) extends StepAnnotation(3, state, meta)
@compileTimeOnly("")
class step4(state: State, meta: Any*) extends StepAnnotation(4, state, meta)
@compileTimeOnly("")
class step5(state: State, meta: Any*) extends StepAnnotation(5, state, meta)
@compileTimeOnly("")
class step6(state: State, meta: Any*) extends StepAnnotation(6, state, meta)
@compileTimeOnly("")
class step7(state: State, meta: Any*) extends StepAnnotation(7, state, meta)

object StepAnnotationMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val (annottee, expandees) = inputs match {
      case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
      case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
      case _ => (EmptyTree, inputs)
    }
    println((annottee, expandees))
    val outputs = expandees
    //Block(outputs, Literal(Constant(())))
    c.Expr[Any](q"println(a)")
  }
}
