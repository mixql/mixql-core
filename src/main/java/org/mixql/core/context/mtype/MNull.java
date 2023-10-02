package org.mixql.core.context.mtype;

import java.util.ArrayList;
import java.util.Arrays;

public class MNull extends MType {
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
            return new MBool(true);
        }
        return new MBool(false);
    }

    @Override
    public MType NotEqual(MType other) {
        if (other instanceof MNull) {
            return new MBool(false);
        }
        return new MBool(true);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MNull)
            return true;
        else
            return false;
    }
}
