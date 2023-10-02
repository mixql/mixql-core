package org.mixql.core.context.mtype;

public abstract class cursor extends MType {
    public abstract MBool open();
    
    public abstract MBool close();

    public abstract MType fetch() throws Exception;
}
