package org.mixql.core.context.mtype;

public abstract class MCursorBase extends MType {

    public abstract MBool open();
    
    public abstract MBool close();

    public abstract MType fetch() throws Exception;
}
