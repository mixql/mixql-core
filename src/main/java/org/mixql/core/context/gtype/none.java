package org.mixql.core.context.gtype;

public class none extends Type {

    @Override
    public String toString() {
        return "none";
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof none) {
            return new bool(true);
        }
        return new bool(false);
    }

    @Override
    public Type NotEqual(Type other) {
        if (other instanceof none) {
            return new bool(false);
        }
        return new bool(true);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof none)
            return true;
        else
            return false;
    }

}
