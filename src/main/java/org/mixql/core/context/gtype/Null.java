package org.mixql.core.context.gtype;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Null extends Type {
    @Override
    public String toString() {
        return "null";
    }

    @Override
    public Type Add(Type other) {
        if (other instanceof array) {
            Type[] _t = ((array) other).getArr();
            ArrayList<Type> t = new ArrayList<>(Arrays.asList(_t));
            t.add(0, this);
            return (new array(t.toArray(_t)));
        } else
            return super.Add(other);
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof Null) {
            return new bool(true);
        }
        return new bool(false);
    }

    @Override
    public Type NotEqual(Type other) {
        if (other instanceof Null) {
            return new bool(false);
        }
        return new bool(true);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Null)
            return true;
        else
            return false;
    }
}
