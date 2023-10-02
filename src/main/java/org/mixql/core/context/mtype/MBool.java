package org.mixql.core.context.mtype;

import java.util.ArrayList;
import java.util.Arrays;

public class MBool extends MType {
    boolean value;

    public boolean getValue(){
        return value;
    }

    public MBool(boolean value) {
        this.value = value;
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
        return new MBool(!value);
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
            return new MBool(this.equals(other));
        }
        if (other instanceof MNull) {
            return new MBool(false);
        }
        if (other instanceof MNone) {
            return new MBool(false);
        }
        return super.Equal(other);
    }

    @Override
    public MType NotEqual(MType other) {
        if (other instanceof MBool) {
            return new MBool(value != ((MBool) other).value);
        }
        if (other instanceof MNull) {
            return new MBool(true);
        }
        if (other instanceof MNone) {
            return new MBool(true);
        }
        return super.NotEqual(other);
    }

    @Override
    public MType Or(MType other) {
        if (other instanceof MBool) {
            return new MBool(value || ((MBool) other).value);
        }
        return super.Or(other);
    }

    @Override
    public MType And(MType other) {
        if (other instanceof MBool) {
            return new MBool(value && ((MBool) other).value);
        }
        return super.And(other);
    }
}
