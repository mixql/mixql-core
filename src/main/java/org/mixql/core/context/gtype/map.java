package org.mixql.core.context.gtype;

import java.util.HashMap;
import java.util.Map;

public class map extends collection {
    Map<Type, Type> m = new HashMap<>();

    public Map<Type, Type> getMap() {
        return m;
    }

    public map(Map<Type, Type> m) {
        this.m = m;
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof map) {
            return new bool(m.equals(((map) other).m));
        }
        return new bool(false);
    }

    @Override
    public Type NotEqual(Type other) {
        if (other instanceof map) {
            return new bool(!m.equals(((map) other).m));
        }
        return new bool(true);
    }

    @Override
    public Type apply(Type index) {
        for (Map.Entry<Type, Type> entry : m.entrySet()) {
            try {
                if (((bool) entry.getKey().Equal(index)).value)
                    return entry.getValue();
            } catch (Exception e) {
            }
        }
        return new Null();
    }

    @Override
    public void update(Type index, Type value) {
        boolean keyIsPresent = false;
        for (Map.Entry<Type, Type> entry : m.entrySet()) {
            try {
                if (((bool) entry.getKey().Equal(index)).value) {
                    keyIsPresent = true;
                    entry.setValue(value);
                    break;
                }
            } catch (Exception e) {
            }
        }
        if (!keyIsPresent)
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
        for (Map.Entry<Type, Type> entry : m.entrySet()) {
            if (entry.getKey() instanceof string)
                buffer.append(((string) entry.getKey()).asLiteral());
            else
                buffer.append(entry.getKey().toString());
            buffer.append(": ");
            Type value = entry.getValue();
            if (value instanceof string)
                buffer.append(((string) value).asLiteral());
            else
                buffer.append(value.toString());
            buffer.append(", ");
        }
        buffer.append("}");
        buffer.replace(buffer.length() - 3, buffer.length(), "}");
        return buffer.toString();
    }
}
