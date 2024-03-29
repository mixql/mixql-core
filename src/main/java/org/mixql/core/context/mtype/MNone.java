package org.mixql.core.context.mtype;

public class MNone extends MType {

    private MNone() {}

    private static MNone value = new MNone();

    public static MNone get() {
        return value;
    }

    @Override
    public String toString() {
        return "none";
    }

    @Override
    public MType Equal(MType other) {
        if (other instanceof MNone) {
            return MBool.True();
        }
        return MBool.False();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MNone)
            return true;
        else
            return false;
    }

}
