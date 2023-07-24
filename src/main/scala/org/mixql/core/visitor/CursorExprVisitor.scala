package org.mixql.core.visitor

import org.mixql.core.context.Context
import org.antlr.v4.runtime.TokenStream
import org.mixql.core.generated.sql
import org.mixql.core.context.gtype.Type
import scala.collection.JavaConverters._

import scala.util.Try

class CursorExprVisitor(ctx: Context, tokens: TokenStream)
    extends MainVisitor(ctx, tokens) {

  override def executeOther(stmt: String,
                            engine: sql.Choose_engineContext): Try[Type] =
    Try {
      if (engine) {
        // execute on custom engine
        // get engine name
        val engineName =
          if (engine.expr)
            visit(engine.expr).toString
          else
            visit(engine.ident).toString
        if (engine.engine_params) {
          // execute with additional params
          val params =
            engine.engine_params.ident.asScala.map(visit(_).toString)
              .zip(engine.engine_params.expr.asScala.map(visit)).toMap
          context.execute(stmt, engineName, params, true)
        } else {
          // execute with current params
          context.execute(stmt, engineName, true)
        }
      } else {
        // execute on current engine
        context.execute(stmt, true)
      }
    }
}
