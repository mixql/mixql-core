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

    Boolean openWasTriggered = false;

    @Override
    public bool open() {
        openWasTriggered = true;
        if (source != null) {
            return new bool(true);
        } else {
            source = exprVisitor.visit(ctx);
            if (source instanceof cursor) {
                return ((cursor) source).open();
            }
            return new bool(true);
        }
    }

    @Override
    public bool close() {
        if (source instanceof cursor) {
            return ((cursor) source).close();
        }
        return new bool(true);
    }

    Integer arr_index = -1;

    Type[] keySet = null;
    Integer keySetIndex = -1;

    @Override
    public Type fetch() throws Exception {
        if (!openWasTriggered)
            throw new Exception("Can not fetch from cursor, when open was not called");
        if (source instanceof cursor) {
            return ((cursor) source).fetch();
        }

        if (source instanceof array) {
            array arr = (array) source;
            gInt arr_size = arr.size();
            if (arr_size.value == 0)
                return new nothing();

            if (arr_index == -1) {
                arr_index = 0;
            }

            if (arr_index < arr_size.value) {
                var elem = arr.apply(new gInt(arr_index++));
                return elem;
            }

            return new nothing();
        }

        if (source instanceof map) {
            map m = (map) source;
            int mSize = m.size().value;
            if (keySet == null) {

                keySet = new Type[mSize];
                keySet = m.getMap().keySet().toArray(keySet);
            }

            if (mSize == 0)
                return new nothing();

            if (keySetIndex == -1) {
                keySetIndex = 0;
            }

            if (keySetIndex < mSize){
                return new array(
                        new Type[]{keySet[keySetIndex], m.apply(keySet[keySetIndex++])}
                );
            }
            return new nothing();
        }

        throw new Exception("Expexted source of type array or map for cursor, not type: " +
                source.getClass().getName()
        );
    }

    public gcursor(Type source) {
        this.source = source;
    }

    public gcursor(Context gCtx, TokenStream tokens, sql.ExprContext ctx) {
        this.ctx = ctx;
        this.exprVisitor = new CursorExprVisitor(gCtx, tokens);
    }
}
