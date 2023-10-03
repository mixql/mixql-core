package org.mixql.core.context.mtype;

public abstract class MCollection extends MType {

    public abstract MType apply(MType index);

    public abstract void update(MType index, MType value);

    public abstract MInt size();
}
