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
}
