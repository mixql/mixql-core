package org.mixql.core.context.gtype;

import java.util.ArrayList;

public class Null extends Type {
    @Override
    public String toString() {
        return "null";
    }

    @Override
    public Type Add(Type other) {
        if (other instanceof array) {
            Type[] t = ((array) other).getArr();
            return new array(t).Add(this);
        } else
            return super.Add(other);
    }
}
