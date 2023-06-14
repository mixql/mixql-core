package org.mixql.core.context.gtype;

import org.antlr.v4.runtime.TokenStream;
import org.mixql.core.context.Context;
import org.mixql.core.generated.sql;
import org.mixql.core.visitor.CursorExprVisitor;

public class gcursor extends cursor {
//    Context gCtx = null;
//    TokenStream tokens = null;

    sql.ExprContext ctx = null;

    CursorExprVisitor exprVisitor = null;

    Type source = null;

    @Override
    public bool open() {
        if (source != null)
            return new bool(true);
        else {
            source = exprVisitor.visit(ctx);
            return new bool(true);
        }
    }

    @Override
    public bool close() {
        return new bool(true);
    }

    @Override
    public Type fetch() {
        return new Null();
    }

    public gcursor(Type source) {
        this.source = source;
    }

    public gcursor(Context gCtx, TokenStream tokens, sql.ExprContext ctx) {
        this.ctx = ctx;
        this.exprVisitor = new CursorExprVisitor(gCtx, tokens);
    }
}
