package org.mixql.core.context.gtype;

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
            Type[] t = ((array) other).getArr();
            return (new array(t)).Add(this);
        }

        return super.Add(other);
    }

    @Override
    public Type Not() {
        return new bool(!value);
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof bool) {
            return new bool(value == ((bool) other).value);
        }
        return super.Equal(other);
    }

    @Override
    public Type NotEqual(Type other) {
        if (other instanceof bool) {
            return new bool(value != ((bool) other).value);
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
