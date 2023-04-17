package org.mixql.core.context.gtype;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class gDouble extends Type {
    double value;

    public double getValue(){
        return value;
    }


    public gDouble(double value) {
        this.value = value;
    }

    public gDouble(String value) {
        this.value = Double.parseDouble(value);
    }

    @Override
    public String toString() {
        return Double.toString(value);
    }

    @Override
    public Type Add(Type other) {
        if (other instanceof gInt) {
            return new gDouble(value + ((gInt) other).value);
        }

        if (other instanceof gDouble) {
            return new gDouble(value + ((gDouble) other).value);
        }

        if (other instanceof string) {
            string otherStr = (string) other;
            return new string(value + otherStr.value, otherStr.quote);
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
    public Type Subtract(Type other) {
        if (other instanceof gInt) {
            return new gDouble(value - ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new gDouble(value - ((gDouble) other).value);
        }
        return super.Subtract(other);
    }

    @Override
    public Type Multiply(Type other) {
        if (other instanceof gInt) {
            return new gDouble(value * ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new gDouble(value * ((gDouble) other).value);
        }
        return super.Multiply(other);
    }

    @Override
    public Type Divide(Type other) {
        if (other instanceof gInt) {
            return new gDouble(value / ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new gDouble(value / ((gDouble) other).value);
        }
        return super.Divide(other);
    }

    @Override
    public Type MoreThen(Type other) {
        if (other instanceof gInt) {
            return new bool(value > ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new bool(value > ((gDouble) other).value);
        }
        return super.MoreThen(other);
    }

    @Override
    public Type MoreEqualThen(Type other) {
        if (other instanceof gInt) {
            return new bool(value >= ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new bool(value >= ((gDouble) other).value);
        }
        return super.MoreEqualThen(other);
    }

    @Override
    public Type LessThen(Type other) {
        if (other instanceof gInt) {
            return new bool(value < ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new bool(value < ((gDouble) other).value);
        }
        return super.LessThen(other);
    }

    @Override
    public Type LessEqualThen(Type other) {
        if (other instanceof gInt) {
            return new bool(value <= ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new bool(value <= ((gDouble) other).value);
        }
        return super.LessEqualThen(other);
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof gInt) {
            return new bool(value == ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new bool(value == ((gDouble) other).value);
        }
        return super.Equal(other);
    }

    @Override
    public Type NotEqual(Type other) {
        if (other instanceof gInt) {
            return new bool(value != ((gInt) other).value);
        }
        if (other instanceof gDouble) {
            return new bool(value != ((gDouble) other).value);
        }
        return super.NotEqual(other);
    }
}
