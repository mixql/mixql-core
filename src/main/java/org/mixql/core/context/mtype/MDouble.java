package org.mixql.core.context.mtype;

import java.util.ArrayList;
import java.util.Arrays;

public class MDouble extends MType {

    double value;

    public double getValue(){
        return value;
    }

    public MDouble(double value) {
        this.value = value;
    }

    public MDouble(String value) {
        this.value = Double.parseDouble(value);
    }

    @Override
    public String toString() {
        return Double.toString(value);
    }

    @Override
    public MType Add(MType other) {
        if (other instanceof MInt) {
            return new MDouble(value + ((MInt) other).value);
        }

        if (other instanceof MDouble) {
            return new MDouble(value + ((MDouble) other).value);
        }

        if (other instanceof MString) {
            MString otherStr = (MString) other;
            return new MString(value + otherStr.value, otherStr.quote);
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
    public MType Subtract(MType other) {
        if (other instanceof MInt) {
            return new MDouble(value - ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MDouble(value - ((MDouble) other).value);
        }
        return super.Subtract(other);
    }

    @Override
    public MType Multiply(MType other) {
        if (other instanceof MInt) {
            return new MDouble(value * ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MDouble(value * ((MDouble) other).value);
        }
        return super.Multiply(other);
    }

    @Override
    public MType Divide(MType other) {
        if (other instanceof MInt) {
            return new MDouble(value / ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MDouble(value / ((MDouble) other).value);
        }
        return super.Divide(other);
    }

    @Override
    public MType MoreThen(MType other) {
        if (other instanceof MInt) {
            return new MBool(value > ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MBool(value > ((MDouble) other).value);
        }
        return super.MoreThen(other);
    }

    @Override
    public MType MoreEqualThen(MType other) {
        if (other instanceof MInt) {
            return new MBool(value >= ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MBool(value >= ((MDouble) other).value);
        }
        return super.MoreEqualThen(other);
    }

    @Override
    public MType LessThen(MType other) {
        if (other instanceof MInt) {
            return new MBool(value < ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MBool(value < ((MDouble) other).value);
        }
        return super.LessThen(other);
    }

    @Override
    public MType LessEqualThen(MType other) {
        if (other instanceof MInt) {
            return new MBool(value <= ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MBool(value <= ((MDouble) other).value);
        }
        return super.LessEqualThen(other);
    }

    @Override
    public MType Equal(MType other) {
        if (other instanceof MInt) {
            return new MBool(value == ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MBool(value == ((MDouble) other).value);
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
        if (other instanceof MInt) {
            return new MBool(value != ((MInt) other).value);
        }
        if (other instanceof MDouble) {
            return new MBool(value != ((MDouble) other).value);
        }
        if (other instanceof MNull) {
            return new MBool(true);
        }
        if (other instanceof MNone) {
            return new MBool(true);
        }
        return super.NotEqual(other);
    }
}
