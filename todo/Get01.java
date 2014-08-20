package com.ugos.JIProlog.engine;
import java.io.*;
import java.util.*;

class Get01 extends Notify2
{
    public final boolean unify(final Hashtable varsTbl)
    {
        return getParam(1).unify(Expression.createNumber(readNextChar()), varsTbl);
    }
    
    protected final int readNextChar()
    {
        InputStream ins = getJIPEngine().getCurrentInputStream();
        
        if("user".equals(getJIPEngine().getCurrentInputStreamName()))
            notifyEvent(JIPEvent.ID_WAITFORUSERINPUT, getPredicate());
        
        try
        {
            int c = ins.read();
                        
            Thread.currentThread().yield();
            
            if("user".equals(getJIPEngine().getCurrentInputStreamName()))
                getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());
            
            return c;
        }
        catch(IOException ex)
        {
            throw new JIPJVMException(ex);
        }
    }
}
