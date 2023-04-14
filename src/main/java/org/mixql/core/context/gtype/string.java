package org.mixql.core.context.gtype;

import java.util.ArrayList;

public class string extends Type {
    String value;

    public String getValue(){
        return value;
    }

    String quote = "";

    public String getQuote(){
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
            return ((string) obj).value.equals(value);
        }
        return false;// TODO mb String too?
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
            Type[] otherList = ((array) other).getArr();
            return new array(otherList).Add(this);
        }

        return new string(value + other.toString(), quote);
    }

    @Override
    // TODO attention do we need type check?
    public Type Equal(Type other) {
        return new bool(value == other.toString());
    }


    // TODO attention do we need type check?
    @Override
    public Type NotEqual(Type other) {
        return new bool(value != other.toString());
    }
}
