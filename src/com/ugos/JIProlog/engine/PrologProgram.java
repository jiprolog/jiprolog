package com.ugos.JIProlog.engine;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;

public class PrologProgram implements Serializable 
{
	private String program;
	private String programName;
	
	/**
     * Creates a theory getting its source text from an input stream
     *
     * @param is the input stream acting as source
     */
    public PrologProgram(InputStream is) throws IOException {
        byte[] b = new byte[is.available()];
        is.read(b);
        program = new String(b);
        programName = "" + program.hashCode();
        is.close();
    }

    /**
     * Creates a theory from its source text
     *
     * @param theory the source text
     * @throws s InvalidTheoryException if theory is null
     */
    public PrologProgram(String program)
    {
        this.program=program;
        programName = "" + program.hashCode();

    }
    
    public PrologProgram(File programFile) throws FileNotFoundException, IOException
    {
        this(new FileInputStream(programFile));
        this.programName = programFile.getName();
    }
    
//    PrologProgram() {
//        this.program = "";
//    }
}
