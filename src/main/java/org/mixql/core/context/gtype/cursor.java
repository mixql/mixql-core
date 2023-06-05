package org.mixql.core.context.gtype;

public abstract class cursor extends Type {
    public abstract bool open();
    
    public abstract bool close();

    public abstract Type fetch();

}
