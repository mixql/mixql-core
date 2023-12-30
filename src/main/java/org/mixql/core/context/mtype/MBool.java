package org.mixql.core.context.mtype;

import java.util.ArrayList;
import java.util.Arrays;

public class MBool extends MType {

    boolean value;

    private static MBool _true = new MBool(true);
    private static MBool _false = new MBool(false);

    static public MBool get(boolean value) {
        if (value)
            return True();
        else
            return False();
    }

    static public MBool True() {
        return _true;
    }

    static public MBool False() {
        return _false;
    }

    private MBool(boolean value) {
        this.value = value;
    }

    public boolean getValue(){
        return value;
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }

    @Override
    public MType Add(MType other) {
        if (other instanceof MString) {
            MString oval = (MString) other;
            return new MString(value + other.toString(), oval.quote);
        }

        if (other instanceof MArray) {
            MType[] _t = ((MArray) other).getArr();
            ArrayList<MType> t = new ArrayList<>(Arrays.asList(_t));
            t.add(0, this);
            return (new MArray(t.toArray(_t)));
        }

        return super.Add(other);
    }

    @Override
    public MType Not() {
        return get(!value);
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof MBool) {
            return value == ((MBool) other).value;
        }
        if (other instanceof Boolean) {
            return value == (Boolean) other;
        }
        if (other instanceof MNull) {
            return false;
        }
        if (other instanceof MNone) {
            return false;
        }
        return super.equals(other);
    }

    @Override
    public MType Equal(MType other) {
        if (other instanceof MBool) {
            return get(this.equals(other));
        }
        return False();
    }

    @Override
    public MType Or(MType other) {
        if (other instanceof MBool) {
            return get(value || ((MBool) other).value);
        }
        return super.Or(other);
    }

    @Override
    public MType And(MType other) {
        if (other instanceof MBool) {
            return get(value && ((MBool) other).value);
        }
        return super.And(other);
    }
}
