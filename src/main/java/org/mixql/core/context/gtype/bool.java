package org.mixql.core.context.gtype;

import java.util.ArrayList;
import java.util.Arrays;

public class bool extends Type {
    boolean value;

    public boolean getValue(){
        return value;
    }

    public bool(boolean value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }

    @Override
    public Type Add(Type other) {
        if (other instanceof string) {
            string oval = (string) other;
            return new string(value + other.toString(), oval.quote);
        }

        if (other instanceof array) {
            Type[] _t = ((array) other).getArr();
            ArrayList<Type> t = new ArrayList<>(Arrays.asList(_t));
            t.add(0, this);
            return (new array(t.toArray(_t)));
        }

        return super.Add(other);
    }

    @Override
    public Type Not() {
        return new bool(!value);
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof bool) {
            return value == ((bool) other).value;
        }
        if (other instanceof Boolean) {
            return value == (Boolean) other;
        }
        if (other instanceof Null) {
            return false;
        }
        if (other instanceof nothing) {
            return false;
        }
        return super.equals(other);
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof bool) {
            return new bool(this.equals(other));
        }
        if (other instanceof Null) {
            return new bool(false);
        }
        if (other instanceof nothing) {
            return new bool(false);
        }
        return super.Equal(other);
    }

    @Override
    public Type NotEqual(Type other) {
        if (other instanceof bool) {
            return new bool(value != ((bool) other).value);
        }
        if (other instanceof Null) {
            return new bool(true);
        }
        if (other instanceof nothing) {
            return new bool(true);
        }
        return super.NotEqual(other);
    }

    @Override
    public Type Or(Type other) {
        if (other instanceof bool) {
            return new bool(value || ((bool) other).value);
        }
        return super.Or(other);
    }

    @Override
    public Type And(Type other) {
        if (other instanceof bool) {
            return new bool(value && ((bool) other).value);
        }
        return super.And(other);
    }
}
