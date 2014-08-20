package com.ugos.JIProlog.engine;

import java.io.IOException;
import java.io.InputStream;

final class Get1 extends Get0
{
    public final boolean match()
    {
        int c = readNextChar();
        
        while(c <= 32 && c != -1)
            c = readNextChar();
        
        if(c == -1)
            m_term = Atom.createAtom("end_of_file");
        else
            m_term = Expression.createNumber(c);
                        
        return getParam(1).match(m_term);
    }
}
