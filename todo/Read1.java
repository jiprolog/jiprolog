package com.ugos.JIProlog.engine;

import java.io.IOException;
import java.io.*;
import java.util.*;

final class Read1 extends Notify2
{
    public final boolean unify(final Hashtable varsTbl)
    {
        InputStream ins = getJIPEngine().getCurrentInputStream();
        
        if("user".equals(getJIPEngine().getCurrentInputStreamName()))
            notifyEvent(JIPEvent.ID_WAITFORUSERINPUT, getPredicate());
        
        PrologParser parser = new PrologParser((ParserInputStream)ins, getJIPEngine().getOperatorManager());//
        
        PrologObject term = parser.parseNext();
        if(term == null)
        {
            term = Atom.createAtom("end_of_file");
        }
        
        // per permettere all'input stream di ricevere il byte
        Thread.currentThread().yield();
        
        if("user".equals(getJIPEngine().getCurrentInputStreamName()))
            notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate());

        return getParam(1).unify(term, varsTbl);
    }
}
