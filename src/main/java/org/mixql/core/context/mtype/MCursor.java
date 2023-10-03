package org.mixql.core.context.mtype;

import org.antlr.v4.runtime.TokenStream;
import org.mixql.core.context.Context;
import org.mixql.core.generated.sql;
import org.mixql.core.visitor.CursorExprVisitor;

public class MCursor extends MCursorBase {
//    Context gCtx = null;
//    TokenStream tokens = null;

    sql.ExprContext ctx = null;

    CursorExprVisitor exprVisitor = null;

    MType source = null;

    Boolean openWasTriggered = false;

    @Override
    public MBool open() {
        openWasTriggered = true;
        if (source != null) {
            return new MBool(true);
        } else {
            source = exprVisitor.visit(ctx);
            if (source instanceof MCursorBase) {
                return ((MCursorBase) source).open();
            }
            return new MBool(true);
        }
    }

    @Override
    public MBool close() {
        if (source instanceof MCursorBase) {
            return ((MCursorBase) source).close();
        }
        return new MBool(true);
    }

    Integer arr_index = -1;

    MType[] keySet = null;
    Integer keySetIndex = -1;

    @Override
    public MType fetch() throws Exception {
        if (!openWasTriggered)
            throw new Exception("Can not fetch from cursor, when open was not called");
        if (source instanceof MCursorBase) {
            return ((MCursorBase) source).fetch();
        }

        if (source instanceof MArray) {
            MArray arr = (MArray) source;
            MInt arr_size = arr.size();
            if (arr_size.value == 0)
                return MNone.get();

            if (arr_index == -1) {
                arr_index = 0;
            }

            if (arr_index < arr_size.value) {
                MType elem = arr.apply(new MInt(arr_index++));
                return elem;
            }

            return MNone.get();
        }

        if (source instanceof MMap) {
            MMap m = (MMap) source;
            int mSize = (int) m.size().value;
            if (keySet == null) {

                keySet = new MType[mSize];
                keySet = m.getMap().keySet().toArray(keySet);
            }

            if (mSize == 0)
                return MNone.get();

            if (keySetIndex == -1) {
                keySetIndex = 0;
            }

            if (keySetIndex < mSize){
                return new MArray(
                        new MType[]{keySet[keySetIndex], m.apply(keySet[keySetIndex++])}
                );
            }
            return MNone.get();
        }

        throw new Exception("Expexted source of type array or map for cursor, not type: " +
                source.getClass().getName()
        );
    }

    public MCursor(MType source) {
        this.source = source;
    }

    public MCursor(Context gCtx, TokenStream tokens, sql.ExprContext ctx) {
        this.ctx = ctx;
        this.exprVisitor = new CursorExprVisitor(gCtx, tokens);
    }
}
