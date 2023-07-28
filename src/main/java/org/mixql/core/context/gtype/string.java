package org.mixql.core.context.gtype;

import java.util.ArrayList;
import java.util.Arrays;

public class string extends Type {
    String value;

    public String getValue() {
        return value;
    }

    String quote = "";

    public String getQuote() {
        return quote;
    }

    public string(String value) {
        this.value = value;
    }

    public string(String value, String quote) {
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
        if (obj instanceof string) {
            int res = ((string) obj).value.compareTo(value);
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
    public Type Add(Type other) {
        if (other instanceof string) {
            string otherStr = (string) other;
            String q = otherStr.quote;
            if (quote != "") q = quote;
            return new string(value + otherStr.value, q);
        }

        if (other instanceof array) {
            Type[] _t = ((array) other).getArr();
            ArrayList<Type> t = new ArrayList<>(Arrays.asList(_t));
            t.add(0, this);
            return (new array(t.toArray(_t)));
        }

        return new string(value + other.toString(), quote);
    }

    @Override
    // TODO attention do we need type check?
    public Type Equal(Type other) {
        return new bool(this.equals(other));
    }


    // TODO attention do we need type check?
    @Override
    public Type NotEqual(Type other) {
        return new bool(value != other.toString());
    }
}
