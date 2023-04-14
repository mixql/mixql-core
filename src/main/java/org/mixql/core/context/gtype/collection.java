package org.mixql.core.context.gtype;

public abstract class collection extends Type {
    public abstract Type apply(Type index);

    public abstract void update(Type index, Type value);

    public abstract gInt size();
}
