package org.mixql.core.context.mtype;

import java.util.HashMap;
import java.util.Map;

public class MMap extends MCollection {
    Map<MType, MType> m = new HashMap<>();

    public Map<MType, MType> getMap() {
        return m;
    }

    public MMap(Map<MType, MType> m) {
        this.m = m;
    }

    @Override
    public MType Equal(MType other) {
        if (other instanceof MMap) {
            return new MBool(m.equals(((MMap) other).m));
        }
        return new MBool(false);
    }

    @Override
    public MType NotEqual(MType other) {
        if (other instanceof MMap) {
            return new MBool(!m.equals(((MMap) other).m));
        }
        return new MBool(true);
    }

    @Override
    public MType apply(MType index) {
        for (Map.Entry<MType, MType> entry : m.entrySet()) {
            try {
                if (((MBool) entry.getKey().Equal(index)).value)
                    return entry.getValue();
            } catch (Exception e) {
            }
        }
        return new MNull();
    }

    @Override
    public void update(MType index, MType value) {
        boolean keyIsPresent = false;
        for (Map.Entry<MType, MType> entry : m.entrySet()) {
            try {
                if (((MBool) entry.getKey().Equal(index)).value) {
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
    public MInt size() {
        return new MInt(m.size());
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("{");
        for (Map.Entry<MType, MType> entry : m.entrySet()) {
            if (entry.getKey() instanceof MString)
                buffer.append(((MString) entry.getKey()).asLiteral());
            else
                buffer.append(entry.getKey().toString());
            buffer.append(": ");
            MType value = entry.getValue();
            if (value instanceof MString)
                buffer.append(((MString) value).asLiteral());
            else
                buffer.append(value.toString());
            buffer.append(", ");
        }
        buffer.append("}");
        buffer.replace(buffer.length() - 3, buffer.length(), "}");
        return buffer.toString();
    }
}
