package org.mixql.core.context.gtype;

import java.util.ArrayList;
import java.util.Arrays;

public class array extends collection {

    private ArrayList<Type> arr = new ArrayList<>();

    public array(Type[] arr) {
        setArr(arr);
    }

    public Type[] getArr() {
        Type[] simpleArray = new Type[arr.size()];
        return arr.toArray(simpleArray);
    }

    public void setArr(Type[] arr) {
        this.arr.addAll(Arrays.asList(arr));
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("[");
        for (int i = 0; i <= arr.size() - 1; i++) {
            Type a = arr.get(i);
            if (a instanceof string)
                buffer.append(((string) a).asLiteral());
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
    public Type Add(Type other) {
        Type[] simpleArray = new Type[arr.size()];
        simpleArray = arr.toArray(simpleArray);
        ArrayList<Type> simpleList = new ArrayList<>(Arrays.asList(simpleArray));
        if (other instanceof array) {
            simpleList.addAll(Arrays.asList(((array) other).getArr()));
            return new array(simpleList.toArray(simpleArray));
        } else {
            simpleList.add(other);
            return new array(simpleList.toArray(simpleArray));
        }
    }

    @Override
    public Type Equal(Type other) {
        if (other instanceof array) {
            Type[] otherArr = ((array) other).getArr();
            return new bool(Arrays.equals(getArr(), otherArr));
        }
        return new bool(false);
    }

    @Override
    public gInt size() {
        return new gInt(arr.size());
    }

    @Override
    public Type apply(Type index) {
        if (index instanceof gInt)
            return arr.get(((gInt) index).value);
        else
            throw new IllegalArgumentException("array index must be int");
    }

    @Override
    public void update(Type index, Type value) {
        if (index instanceof gInt)
            arr.set(((gInt) index).value, value);
        else
            throw new IllegalArgumentException("array index must be int");
    }
}
