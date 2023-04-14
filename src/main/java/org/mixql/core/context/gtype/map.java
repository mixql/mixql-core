package org.mixql.core.context.gtype;

import java.util.HashMap;
import java.util.Map;

public class map extends collection {
    Map<Type, Type> m = new HashMap<>();

    public Map<Type, Type> getMap(){
        return m;
    }

    public map(Map<Type, Type> m) {
        this.m = m;
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof map) {
            return new bool(m.equals(((map)other).m));
        }
        return new bool(false);
    }

    @Override
    public Type apply(Type index) {
        return m.get(index);
    }

    @Override
    public void update(Type index, Type value) {
        m.put(index, value);
    }

    @Override
    public gInt size() {
        return new gInt(m.size());
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("{");
        for (Type key : m.keySet()) {
            if (key instanceof string)
                buffer.append(((string) key).asLiteral());
            else
                buffer.append(key.toString());
            buffer.append(": ");
            Type value = m.get(key);
            if (value instanceof string)
                buffer.append(((string) value).asLiteral());
            else
                buffer.append(value.toString());
        }
        buffer.append("}");
        return buffer.toString();
    }
}
