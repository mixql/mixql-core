package org.mixql.core.context.mtype;

import java.util.ArrayList;
import java.util.Arrays;

public class MNull extends MType {

    private MNull() {}

    private static MNull value = new MNull();

    public static MNull get() {
        return value;
    }

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public MType Add(MType other) {
        if (other instanceof MArray) {
            MType[] _t = ((MArray) other).getArr();
            ArrayList<MType> t = new ArrayList<>(Arrays.asList(_t));
            t.add(0, this);
            return (new MArray(t.toArray(_t)));
        } else
            return super.Add(other);
    }

    @Override
    public MType Equal(MType other) {
        if (other instanceof MNull) {
            return MBool.True();
        }
        return MBool.False();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MNull)
            return true;
        else
            return false;
    }
}
