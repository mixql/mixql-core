package org.mixql.core.context.mtype;

import java.util.ArrayList;
import java.util.Arrays;

public class MString extends MType {

    String value;

    public String getValue() {
        return value;
    }

    String quote = "";

    public String getQuote() {
        return quote;
    }

    public MString(String value) {
        this.value = value;
    }

    public MString(String value, String quote) {
        this.value = value;
        this.quote = quote;
    }

    @Override
    public String toString() {
        return value;
    }

    public String quoted() {
        return quote + value + quote;
    }

    public String asLiteral() {
        String q = "\"";
        if (quote != "") q = quote;
        return q + value + q;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MString) {
            int res = ((MString) obj).value.compareTo(value);
            if (res == 0)
                return true;
            else
                return false;
        }
        if (obj instanceof String) {
            int res = ((String) obj).compareTo(value);
            if (res == 0)
                return true;
            else
                return false;
        }
        return false;
    }

    @Override
    public MType Add(MType other) {
        if (other instanceof MString) {
            MString otherStr = (MString) other;
            String q = otherStr.quote;
            if (quote != "") q = quote;
            return new MString(value + otherStr.value, q);
        }

        if (other instanceof MArray) {
            MType[] _t = ((MArray) other).getArr();
            ArrayList<MType> t = new ArrayList<>(Arrays.asList(_t));
            t.add(0, this);
            return (new MArray(t.toArray(_t)));
        }

        return new MString(value + other.toString(), quote);
    }

    @Override
    public MType Equal(MType other) {
        return MBool.get(this.equals(other));
    }

    @Override
    public MType NotEqual(MType other) {
        return this.Equal(other).Not();
    }
}
