package org.mixql.core.context.gtype;

public class nothing extends Type {

    @Override
    public String toString() {
        return "nothing";
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof nothing) {
            return new bool(true);
        }
        return new bool(false);
    }

    @Override
    public Type NotEqual(Type other) {
        if (other instanceof nothing) {
            return new bool(false);
        }
        return new bool(true);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof nothing)
            return true;
        else
            return false;
    }

}
