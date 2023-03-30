package org.mixql.core.function

import org.mixql.core.context.gtype._
import org.mixql.core.generated.sql.BlockContext
import org.mixql.core.visitor.BaseVisitor

final class SqlLambda(
  paramNames: List[String],
  body: BlockContext,
  visitor: BaseVisitor
) extends Type {
  def apply(params: Any*): Any = {
    if (paramNames.size > params.size)
      throw new IllegalArgumentException("not enough arguments")
    else if (paramNames.size < params.size)
      throw new IllegalArgumentException("too many arguments")
    visitor.context.push_scope()
    paramNames
      .zip(params)
      .foreach(param => visitor.context.setVar(param._1, pack(param._2)))
    val res = visitor.visit(body)
    visitor.context.pop_scope()
    unpack(res)
  }
}
