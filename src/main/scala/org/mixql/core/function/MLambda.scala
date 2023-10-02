package org.mixql.core.function

import org.mixql.core.context.mtype._
import org.mixql.core.generated.sql.BlockContext
import org.mixql.core.visitor.BaseVisitor
import org.mixql.core.context.Context
import org.mixql.core.visitor.MainVisitor
import org.antlr.v4.runtime.TokenStream

final class MLambda(paramNames: List[String], body: BlockContext, tokens: TokenStream) extends MType {

  def apply(context: Context, params: Any*): Any = {
    if (paramNames.size > params.size)
      throw new IllegalArgumentException("not enough arguments")
    else if (paramNames.size < params.size)
      throw new IllegalArgumentException("too many arguments")

    val visitor = new MainVisitor(context, tokens)
    visitor.context.pushScope()
    paramNames.zip(params).foreach(param => visitor.context.setVar(param._1, pack(param._2)))
    val res = visitor.visit(body)
    visitor.context.popScope()
    unpack(res)
  }
}
