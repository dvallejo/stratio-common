/*
 * Copyright (C) 2015 Stratio (http://stratio.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.stratio.common.utils.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class LogFunction extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro LogFunctionImpl.impl
}

object LogFunctionImpl {

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def logMessage(methodName: TermName): String =
      s"Calling method '$methodName' with arguments: "

    def argumentsMap(args: List[List[ValDef]]): List[(String, TermName)] =
      args.flatten.map { case arg =>
        (arg.name.toString, arg.name)
      }

    def argumentsToString(args: List[List[ValDef]]): Tree =
      q"""
        ${argumentsMap(args)}.map { case (key, value) =>
          key + " = " + value
        }
        .mkString(", ")
      """

    def modifiedFunction(funcDecl: DefDef): c.Expr[Any] = funcDecl match {
      case q"$mods def $methodName[..$tpes](...$args): $returnType = { ..$body }" =>
        c.Expr[Any](
          q"""
            $mods def $methodName[..$tpes](...$args): $returnType = {
              logger.trace(${logMessage(methodName)} + ${argumentsToString(args)})
              ..$body
            }
          """
        )
      case _ => c.abort(c.enclosingPosition, s" Match error with $funcDecl")
    }

    annottees.map(_.tree).toList match {
      case (funcDecl: DefDef) :: Nil => modifiedFunction(funcDecl)
      case _ => c.abort(c.enclosingPosition, "Invalid annottee. Only can be used with methods")
    }

  }
}
