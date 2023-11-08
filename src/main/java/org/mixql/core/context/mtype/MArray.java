package org.mixql.core.context.mtype;

import java.util.ArrayList;
import java.util.Arrays;

public class MArray extends MCollection {

    private ArrayList<MType> arr = new ArrayList<>();

    public MArray(MType[] arr) {
        setArr(arr);
    }

    public MType[] getArr() {
        MType[] simpleArray = new MType[arr.size()];
        return arr.toArray(simpleArray);
    }

    public void setArr(MType[] arr) {
        this.arr.addAll(Arrays.asList(arr));
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("[");
        for (int i = 0; i <= arr.size() - 1; i++) {
            MType a = arr.get(i);
            if (a instanceof MString)
                buffer.append(((MString) a).asLiteral());
            else
                buffer.append(a.toString());
            if (i != arr.size() - 1) {
                buffer.append(", ");
            }
        }
        buffer.append("]");
        return buffer.toString();
    }

    @Override
    public MType Add(MType other) {
        MType[] simpleArray = new MType[arr.size()];
        simpleArray = arr.toArray(simpleArray);
        ArrayList<MType> simpleList = new ArrayList<>(Arrays.asList(simpleArray));
        if (other instanceof MArray) {
            simpleList.addAll(Arrays.asList(((MArray) other).getArr()));
            return new MArray(simpleList.toArray(simpleArray));
        } else {
            simpleList.add(other);
            return new MArray(simpleList.toArray(simpleArray));
        }
    }

    @Override
    public MType Equal(MType other) {
        if (other instanceof MArray) {
            MType[] otherArr = ((MArray) other).getArr();
            return MBool.get(Arrays.equals(getArr(), otherArr));
        }
        return MBool.False();
    }

    @Override
    public MType NotEqual(MType other) {
        return Equal(other).Not();
    }

    @Override
    public MInt size() {
        return new MInt(arr.size());
    }

    @Override
    public MType apply(MType index) {
        if (index instanceof MInt)
            return arr.get((int) ((MInt) index).value);
        else
            throw new IllegalArgumentException("array index must be int");
    }

    @Override
    public void update(MType index, MType value) {
        if (index instanceof MInt)
            arr.set((int) ((MInt) index).value, value);
        else
            throw new IllegalArgumentException("array index must be int");
    }
}
