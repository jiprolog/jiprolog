/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico - http://www.ugochirico.com
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the Affero GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Affero GNU General Public License for more details.
 *
 * You should have received a copy of the Affero GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */


package com.ugos.jiprolog.engine;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.nio.charset.Charset;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import com.ugos.util.ClassLoaderEx;
import com.ugos.util.JARClassProvider;



/**
 * JIPEngine implements the JIProlog interpeter.<br>
 * See the section <i>"How to call JIProlog interpreter from Java"</i> in the Reference Manual.
 * @version 4.1.2.x
 * @author Ugo Chirico<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPQuery
 */
public class JIPEngine implements Serializable
{
    private final static long serialVersionUID = 300000001L;

    // resource path
    static final String RESOURCEPATH = "/com/ugos/jiprolog/resources/";

    public static final int major = 4;
    public static final int minor = 1;
    public static final int build = 2;
    public static final int revision = 8;

    public static final int USER_INPUT_HANDLE = -1;
    public static final int USER_OUTPUT_HANDLE = -2;
    public static final int USER_ERROR_HANDLE = -4;

    private static final String VERSION = new StringBuilder().append(major).append(".").append(minor).append(".").append(build).append(".").append(revision).toString();

    private static JARClassProvider s_classProvider;
    private static ClassLoader      s_classLoader;

    private static GlobalDB s_globalDB;

    private BuiltInFactory  m_builtInFactory;
    private Hashtable<String, Object>       m_envVarTbl;
    private EventNotifier   m_eventNotifier;
    private Hashtable<Integer, AsyncWAMManager>       m_prologTable;
    private JIPTermParser   m_termParser;
    private OperatorManager m_opManager;

    private boolean         m_bTrace;

    private String          m_strSearchPath;
    private GlobalDB        m_globalDB;

    private static JIPEngine defaultEngine;

    /** Returns the JIProlog version
     */
    public static final String getVersion()
    {
        return VERSION;
    }

    /** Returns JIProlog info
     */
    public static final String getInfo()
    {
        return new StringBuilder("JIProlog v").append(VERSION).append(" Copyright (c) 1999-2015 By Ugo Chirico - http://www.jiprolog.com. All Right Reserved").toString();
    }

    /** Returns Copyright
     */
    public static final String getCopyrightInfo()
    {
        return "Copyright (c) 1999-2015 By Ugo Chirico - http://www.jiprolog.com. All Rights Reserved";
    }

    /** Returns JIProlog license info
     */
    public static final String getLicenseInfo()
    {
        return "JIProlog is released under AGPL v3 or under Commercial License. Visit www.jiprolog.com for more info";
    }

    static JIPEngine getDefaultEngine()
    {
    	return defaultEngine;
    }

    /** Constructs a new instance of JIPEngine with default values search path, input and output stream.
     * Search path is the path where this instance of JIPEngine searches for files.
     * The default value is the value of the environment variable user.dir<br>
     * Input and Output stream are the streams where this instance of JIPEngine reads and writes.
     * The default values are respectively System.in and System.out.<br>
     */
    public JIPEngine()
    {
    	if(defaultEngine == null)
    	{
    		defaultEngine = this;
            s_globalDB = new GlobalDB(this);
    	}

        m_bTrace         = false;

        m_prologTable    = new Hashtable<Integer, AsyncWAMManager>(10);
        m_builtInFactory = new BuiltInFactory(this);
        m_globalDB       = s_globalDB.newInstance(this);
        m_eventNotifier  = new EventNotifier(this);
        m_envVarTbl      = new Hashtable<String, Object>(10);
        m_opManager      = new OperatorManager();
        m_termParser     = new JIPTermParser(m_opManager, this, Charset.defaultCharset().toString());

        // avvia il notifier
        m_eventNotifier.setEnabled(true);

        m_strSearchPath  = "LOC://";

        try
        {
            File file = new File(".");
            setSearchPath(file.getCanonicalPath());
        }
        catch(SecurityException ex)
        {
            m_strSearchPath  = null;
        }
        catch(IOException ex)
        {
            m_strSearchPath  = null;
        }

        // default streams
        setUserOutputStream(System.out);

        setUserInputStream(System.in);

        setEncoding(Charset.defaultCharset().name());

        setEnvVariable("char_conversion", "off");
        setEnvVariable("double_quotes", "codes");
        setEnvVariable("back_quotes", "atom");
        setEnvVariable("unknown", "warning");
        setEnvVariable("syntax_error", "error");
        setEnvVariable("os_error", "error");
        setEnvVariable("debug", JIPDebugger.debug ? "on" : "off");
        setEnvVariable("update_semantics", "logical");

        setEnvVariable("enable_clause_check", "false");


        try
        {
			consultFile("INTERNAL://com/ugos/jiprolog/resources/x.pl");
		}
        catch (JIPSyntaxErrorException e)
        {
			e.printStackTrace();
		}
        catch (IOException e)
        {
			e.printStackTrace();
		}

        setEnvVariable("enable_clause_check", "true");

    }

    /** Sets the update semantics
     * ISO Prolog specifies that a prolog system should implement 'logical update
	 * semantics' on the internal database. The ISO standard imposes a static state
	 * of the database at and during the time of a query/goal.
	 * By 'static' it means that new clauses/entries to the database are not visible in
	 * back-tracking.
	 * The following example illustrates the difference between the two semantics:
	 *
	 * :- dynamic a/1.
	 * JIP:- assert(a(1)).
	 * JIP:- retract(a(X)), X1 is X + 1, assertz(a(X)).
	 *
	 * With 'immediate update semantics', the first calls to retract/1 and assertz/1
	 * will succeed. On back-tracking, the system will retry retract/1 and the newly
	 * asserted goal, which is visible to retract/1, can be retracted from the
	 * database. (This example will continue generating integers forever.) On back-tracking with
	 * 'logical update semantics' the call to retract/1 will not see the update performed by
	 * the assertz/1 and the query will have a single solution.
	 *
     * @param immediate true - immediate update semantics, false - logical update semantics (default)
     */
    public void setImmediateUpdateSemantics(boolean immediate)
    {
    	setEnvVariable("update_semantics", immediate ? "immediate" : "logical");
    }

    /** Gets the update semantics
     * @return true if immediate update semantics,  false if logical update semantics (default)
     */
    public boolean isImmediateUpdateSemantics()
    {
    	return getEnvVariable("update_semantics").equals("immediate");
    }

    /** Sets debug flag.
     */
    public void setDebug(boolean debug)
    {
    	JIPDebugger.debug = debug;
        setEnvVariable("debug", debug ? "on" : "off");
    }

    /** gets debug flag.
     */
    public boolean getDebug()
    {
        return "on".equals(getEnvVariable("debug"));
    }

    /** Returns true if the given functor is a system predicate.
     */
    public final boolean isSystem(final String strName)
    {
        return m_globalDB.isSystem(strName);
    }

    /** Returns true if the given functor is a system predicate.
     */
    public final boolean isSystem(JIPFunctor funct)
    {
        return  m_globalDB.isSystem((Functor)funct.getRealTerm());
    }

    /** Returns true if the given functor is an internal predicate.
     */
    public final boolean isInternal(final String strName)
    {
        return m_globalDB.isInternal(strName);
    }

    /** Returns true if the given functor is an internal predicate.
     */
    public final boolean isInternal(JIPFunctor funct)
    {
        return  m_globalDB.isInternal(funct.getName());
    }

    /** Load an extensions library.<br>
     * The library should be a valid .jar file containing a set of extension classes.<br>
     * If in the .jar there is a file named init.pl it is consulted as any other prolog file.
     * For more information see the section <i>"Load an extension library"</i> in the Reference Manual.<br>
     */
    public final void loadLibrary(final String strPath) throws IOException
    {
        // se il classloader non è stato inizializzato
        if(s_classLoader == null)
        {
            try
            {
                s_classProvider = new JARClassProvider();
                s_classLoader   = new ClassLoaderEx(s_classProvider);
            }
            catch(Throwable th)
            {
                throw new JIPJVMException(th);
            }
        }

        String strFileName[] = new String[1];
        String strCurDir[] = new String[1];

        InputStream ins1 = StreamManager.getStreamManager().getInputStream(strPath, getSearchPath(), strFileName, strCurDir);
        ins1.close();

//        System.out.println("strFileName " + strFileName[0]);  //DBG
//        System.out.println("strCurDir " + strFileName[0]);  //DBG

        final File libf = new File(strPath);
        //final String strBasePath = (libf.getParent() != null ? libf.getParent() : File.separator);
        final String strBasePath = strCurDir[0];
        final ZipFile library = new ZipFile(libf);

//        System.out.println("strBasePath " + strBasePath);  //DBG

        Enumeration en = library.entries();
        ZipEntry entry;
        while(en.hasMoreElements())
        {
            entry = (ZipEntry)en.nextElement();
            if(entry.getName().endsWith(".pl"))
            {
                String strCurSarchPath = "";
                final InputStream ins = library.getInputStream(entry);
                try
                {
//                    System.out.println("strBasePath " + strBasePath);  //DBG
                    strCurSarchPath = getSearchPath();
                    setSearchPath(strBasePath);
                    Consult1.consult(ins, strPath, this, 0, getEnvVariable("enable_clause_check").equals("true"));
                    setSearchPath(strCurSarchPath);
                }
                catch(RuntimeException ex)
                {
//                    ex.printStackTrace();  // DBG

                    setSearchPath(strCurSarchPath);
                    library.close();
                    throw ex;
                }
                catch(IOException ex)
                {
//                    ex.printStackTrace();  // DBG

                    setSearchPath(strCurSarchPath);
                    library.close();
                    throw ex;
                }
            }
        }

        library.close();

        s_classProvider.addJarPath(strPath);
    }

    /** Adds the specified JIPEventListener object
     * @param listener JIPEventListener object to add
     * @see com.ugos.jiprolog.engine.JIPEventListener
     */
    public void addEventListener(final JIPEventListener listener)
    {
        m_eventNotifier.addEventListener(listener);
    }

    /** Removes the specified JIPEventListener object
     * @param listener JIPEventListener object to remove
     * @see com.ugos.jiprolog.engine.JIPEventListener
     */
    public void removeEventListener(final JIPEventListener listener)
    {
        m_eventNotifier.removeEventListener(listener);
    }

    /** Gets the list of JIPEventListeners
     * @return Vector containing the registered JIPEventListener objects
     * @see com.ugos.jiprolog.engine.JIPEventListener
     */
    public Vector getEventListeners()
    {
        return m_eventNotifier.getEventListeners();
    }

    /** Adds the specified JIPTraceListener object
     * @param traceListener JIPTraceListener object to add
     * @see com.ugos.jiprolog.engine.JIPTraceListener
     */
    public void addTraceListener(JIPTraceListener traceListener)
    {
        m_eventNotifier.addTraceListener(traceListener);
    }

    /** Removes the specified JIPTraceListener object
     * @param traceListener JIPTraceListener object to remove
     * @see com.ugos.jiprolog.engine.JIPTraceListener
     */
    public void removeTraceListener(JIPTraceListener traceListener)
    {
        m_eventNotifier.removeTraceListener(traceListener);
    }


    /** Gets the list of JIPTraceListener
     * @return Vector of listeners
     * @see com.ugos.jiprolog.engine.JIPTraceListener
     */
    public Vector getTraceListeners()
    {
        return m_eventNotifier.getTraceListeners();
    }

    /** Sets the search path of this instance of JIPEngine.
     * The Search path is the path where this instance of JIPEngine searches for files.<br>
     * It can refer to:<br>
     * - a directory on hard disk (d:\myapp, file:/user/myapp)<br>
     * - an URL (http://www.mysite.com)<br>
     * - a jar file (jar://user/myprologfile.jar)<br>
     * @param strSearchPath Search path
     * @see com.ugos.jiprolog.engine.JIPEngine#getSearchPath
     */
    public synchronized final void setSearchPath(final String strSearchPath) throws IOException
    {
        m_strSearchPath = SearchPath1.getValidSearchPath(strSearchPath, "");
    }

    /** returns the search path of this instance of JIPEngine.
     * The Search path is the path where this instance of JIPEngine searches for files.<br>
     * @return the search path of this JIPEngine object.
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public synchronized final String getSearchPath()
    {
        return m_strSearchPath;
    }

    /** Sets the trace flag. If the flag is set all JIPTraceListeners are notified during execution.
     * @param bTrace Trace flag
     * @see com.ugos.jiprolog.engine.JIPEngine#getTrace
     */
    public synchronized final void setTrace(final boolean bTrace)
    {
        m_bTrace = bTrace;
        if(bTrace)
        	setEnvVariable("__trace__", "__trace__");
        else
        	removeEnvVariable("__trace__");
    }

    /** Returns the trace flag.
     * @see com.ugos.jiprolog.engine.JIPEngine#setTrace
     */
    public synchronized final boolean getTrace()
    {
        return m_bTrace;
    }

    /** Sets user OutputStream
     * @param outs the user output stream to set.
     * @see com.ugos.jiprolog.engine.JIPEngine#getUserOutputStream
     */
    public final void setUserOutputStream(final OutputStream outs)
    {
        if(outs == null)
        {
            removeEnvVariable("___userout___");
            setCurrentOutputStream(null, USER_OUTPUT_HANDLE);
        }
        else
        {
            setEnvVariable("___userout___", outs);
            setCurrentOutputStream(outs, USER_OUTPUT_HANDLE);
        }
    }

    /** Gets user OutputStream
     * @return The user output stream.
     * @see com.ugos.jiprolog.engine.JIPEngine#setUserOutputStream
     */
    public final OutputStream getUserOutputStream()
    {
        return (OutputStream)getEnvVariable("___userout___");
    }

    /** Sets user InputStream
     * @param ins the user input stream to set
     * @see com.ugos.jiprolog.engine.JIPEngine#getUserInputStream
     */
    public final void setUserInputStream(final InputStream ins)
    {
        if(ins == null)
        {
            removeEnvVariable("___userin___");
            setCurrentInputStream(null, USER_INPUT_HANDLE);
        }
        else
        {
            setEnvVariable("___userin___", ins);
            setCurrentInputStream(ins, USER_INPUT_HANDLE);
        }
    }

    /** Gets user InputStream
     * @return The user input stream
     * @see com.ugos.jiprolog.engine.JIPEngine#setUserInputStream
     */
    public final InputStream getUserInputStream()
    {
        return (InputStream)getEnvVariable("___userin___");
    }

    /** Gets current OutputStream
     * @return The current output stream
     * @see com.ugos.jiprolog.engine.JIPEngine#setCurrentOutputStream
     */
    public final OutputStream getCurrentOutputStream()
    {
        return (OutputStream)getEnvVariable("___currentout___");
    }

    /** Sets current OutputStream
     * @param outs the current output stream to set
     * @param streamHandle the stream handle
     * @see com.ugos.jiprolog.engine.JIPEngine#getUserOutputStream
     */
    public final void setCurrentOutputStream(final OutputStream outs, int streamHandle)
    {
        if(outs == null)
        {
            removeEnvVariable("___currentout___");
            removeEnvVariable("___CurrentOutStreamName___");
        }
        else
        {
            setEnvVariable("___currentout___", outs);
            setEnvVariable("___CurrentOutStreamName___", streamHandle);
        }
    }

    /** Sets current InputStream
     * @param ins the current input stream to set
     * @param streamHandle the handle of the input stream
     * @see com.ugos.jiprolog.engine.JIPEngine#getUserInputStream
     */
    public final void setCurrentInputStream(final InputStream ins, int streamHandle)
    {
        if(ins == null)
        {
            removeEnvVariable("___currentin___");
            removeEnvVariable("___CurrentInStreamHandle___");
        }
        else
        {
            setEnvVariable("___currentin___", ins);
            setEnvVariable("___CurrentInStreamHandle___", streamHandle);
        }
    }

    /** Gets current InputStream
     * @return The current input stream
     * @see com.ugos.jiprolog.engine.JIPEngine#setCurrentInputStream
     */
    public final InputStream getCurrentInputStream()
    {
        return (InputStream)getEnvVariable("___currentin___");
    }

    /** Gets current InputStream Name
     * @return The current input stream handle.
     */
    public final int getCurrentInputStreamHandle()
    {
        return (Integer)getEnvVariable("___CurrentInStreamHandle___");
    }

    /** Gets current OutputStream Name
     * @return The current output stream name.
     */
    public final int getCurrentOutputStreamHandle()
    {
        return (Integer)getEnvVariable("___CurrentOutStreamName___");
    }

    /** Gets current encoding
     * @return The current encoding.
     */
    public final String getEncoding()
    {
        return (String)getEnvVariable("___CurrentEncoding___");
    }

    /** Sets current encoding
     * @param encoding the encoding
     */
    public final void setEncoding(String encoding)
    {
    	Charset.forName(encoding);
    	m_termParser.setEncoding(encoding);
        setEnvVariable("___CurrentEncoding___", encoding);
    }


    /** Closes all pending queries, reset the prolog interpreter and clear the database
     */
    public void reset()
    {
        closeAllQueries();
        synchronized(m_globalDB)
        {
            m_globalDB = s_globalDB.newInstance(this);
            m_opManager.reset();

            try {
    			consultFile("INTERNAL://com/ugos/jiprolog/resources/x.pl");
    		} catch (JIPSyntaxErrorException e) {
    			e.printStackTrace();
    		} catch (IOException e) {
    			e.printStackTrace();
    		}
        }
    }

    /** Closes all pending queries and release all threads and resources.
     */
    public void releaseAllResources()
    {
        finalize();
    }

    /** Consults a file.
     * If in the file there are some no multifile predicates already consulted/loaded in another file file/stream the
     * interpreter raises an exception.<br>
     * @param strFileName the name of the file to consult. It can refer to: <br>
     * - a file on hard disk (d:\myapp/myfile.pl, file:/user/myapp/myfile.pl)<br>
     * - an URL (http://www.mysite.com/myfile.pl)<br>
     * - a file contained in a jar file (jar://user/myprologfile.jar#myfile.pl)<br>
     * - a path relative to the search path.
     * @exception com.ugos.jiprolog.engine.JIPSyntaxErrorException, java.io.IOException
     * @see com.ugos.jiprolog.engine.JIPEngine#loadFile
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public final void consultFile(final String strFileName) throws JIPSyntaxErrorException, IOException
    {
        Consult1.consult(strFileName, this, 0);
    }

    /** Consults a Stream.
     * If in the stream there are some no multifile predicates already consulted/loaded in another file/stream the
     * interpreter raises an exception.<br>
     * @param ins is the stream to consult
     * @param strStreamName is the symbolic name of the stream
     * @exception com.ugos.jiprolog.engine.JIPSyntaxErrorException, java.io.IOException
     * @see com.ugos.jiprolog.engine.JIPEngine#loadStream
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public final void consultStream(final InputStream ins, final String strStreamName) throws JIPSyntaxErrorException
    {
        Consult1.consult(ins, strStreamName, this, 0, getEnvVariable("enable_clause_check").equals("true"));
    }

    /** Unconsults/unloads a file. Deletes from the prolog database all clauses previously consulted or loaded with to the specified file.
     * Note: If in the file consulted/loaded there was nested consult/load the predicates defined in the other files are not deleted.
     * @param strFileName the name of the file to unconsult.
     * @see com.ugos.jiprolog.engine.JIPEngine#loadFile
     * @see com.ugos.jiprolog.engine.JIPEngine#consultFile
     */
    public final void unconsultFile(final String strFileName)
    {
        final String[] strCurDir = new String[1];
        final String[] strFName = new String[1];

        try
        {
            final InputStream ins = StreamManager.getStreamManager().getInputStream(strFileName, m_strSearchPath, strFName, strCurDir);
            ins.close();
        }
        catch(IOException ex)
        {
        }

        synchronized(m_globalDB)
        {
            m_globalDB.unconsult(strFName[0]);
        }
    }

    /** Unconsults/unloads a stream. Deletes from the prolog database all clauses previously consulted or loaded with to the specified stream.
     * Note: If in the file consulted/loaded there was nested consult/load the predicates defined in the other files are not deleted.
     * @param strStreamName the name of the stream to unconsult.
     * @see com.ugos.jiprolog.engine.JIPEngine#loadStream
     * @see com.ugos.jiprolog.engine.JIPEngine#consultStream
     */
    public final void unconsultStream(final String strStreamName)
    {
        unconsultFile(strStreamName);
    }

    /** Compiles the specified file. As result a file with .jip extension is produced<br>
     * @param strFileName the name of the file to compile. It can refer to: <br>
     * - a file on hard disk (d:\myapp/myfile.pl, file:/user/myapp/myfile.pl)<br>
     * - a path relative to the search path.
     * @exception com.ugos.jiprolog.engine.JIPSyntaxErrorException, java.io.IOException
     * @see com.ugos.jiprolog.engine.JIPEngine#loadFile
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public final void compileFile(final String strFileName) throws JIPSyntaxErrorException
    {
        Compile2.compile(strFileName, null, this);
    }

    /** Compiles the specified file. As result a file with .jip extension is produced in the destination folder<br>
     * @param strFileName the name of the file to compile. It can refer to: <br>
     * - a file on hard disk (d:\myapp/myfile.pl, file:/user/myapp/myfile.pl)<br>
     * - a path relative to the search path.
     * @param strDestinationFolder the destination folder wher the compiled .jip file is saved<br>
     * @exception com.ugos.jiprolog.engine.JIPSyntaxErrorException, java.io.IOException
     * @see com.ugos.jiprolog.engine.JIPEngine#loadFile
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public final void compileFile(final String strFileName, final String strDestinationFolder) throws JIPSyntaxErrorException
    {
        Compile2.compile(strFileName, strDestinationFolder, this);
    }

    /** Compiles and Packs the specified files. As result the destination file with .jip extension is generated.<br>
     * @param strFileNames the array of file names to compile and pack. Each file can refer to: <br>
     * - a file on hard disk (d:\myapp/myfile.pl, file:/user/myapp/myfile.pl)<br>
     * - a path relative to the search path.
     * @param strDestinationFile the destination file where the packed .jip file is saved<br>
     * @exception com.ugos.jiprolog.engine.JIPSyntaxErrorException, java.io.IOException
     * @throws IOException
     * @throws FileNotFoundException
     * @see com.ugos.jiprolog.engine.JIPEngine#loadFile
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public final void packFiles(final String[] strFileNames, final String strDestinationFile) throws JIPSyntaxErrorException, FileNotFoundException, IOException
    {
    	List fileList = List.NIL;

    	for(String file : strFileNames)
    	{
    		fileList = new List(Atom.createAtom(file), fileList);
    	}

    	Pack2.pack(fileList, strDestinationFile, this);
    }

    /** Loads a file. The file must contain a valid JIProlog compiled code
     * If in the file there are some no multifile predicates already consulted/loaded in another file the
     * interpreter raises an exception.<br>
     * @param strFileName the name of the file to load. It can refer to: <br>
     * - a file on hard disk (d:\myapp/myfile.pl, file:/user/myapp/myfile.pl)<br>
     * - an URL (http://www.mysite.com/myfile.pl)<br>
     * - a file contained in a jar file (jar://user/myprologfile.jar#myfile.pl)<br>
     * - a path relative to the search path.
     * @exception java.io.IOException
     * @see com.ugos.jiprolog.engine.JIPEngine#consultFile
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public final void loadFile(final String strFileName) throws IOException
    {
        try
        {
            Load1.load(strFileName, this);
        }
        catch(ClassNotFoundException ex)
        {
        	throw new JIPTypeException(JIPTypeException.LIBRARY, Atom.createAtom(strFileName));
//            throw JIPRuntimeException.create(28, strFileName);
        }
    }

    /** Loads a stream. The stream must contain a valid JIProlog compiled code
     * If in the stream there are some no multifile predicates already consulted/loaded in another file the
     * interpreter raises an exception.<br>
     * @param ins the inpustream to load
     * @param strStreamName the name of the file to load. It can refer to: <br>
     * - a file on hard disk (d:\myapp/myfile.pl, file:/user/myapp/myfile.pl)<br>
     * - an URL (http://www.mysite.com/myfile.pl)<br>
     * - a file contained in a jar file (jar://user/myprologfile.jar#myfile.pl)<br>
     * - a path relative to the search path.
     * @exception java.io.IOException
     * @see com.ugos.jiprolog.engine.JIPEngine#loadFile
     * @see com.ugos.jiprolog.engine.JIPEngine#setSearchPath
     */
    public final void loadStream(final InputStream ins, String strStreamName) throws IOException
    {
        try
        {
            Load1.load(ins, strStreamName, this);
        }
        catch(ClassNotFoundException ex)
        {
        	throw new JIPTypeException(JIPTypeException.LIBRARY, Atom.createAtom(strStreamName));
//            throw JIPRuntimeException.create(28, strStreamName);
        }
    }

    /** It is equivalent to asserta/1 predicate.
     * Add a clause to the interpreter database at the beginning of the clauses associated to the given one.
     * @param term the term to assert.
     */
    public final void asserta(final JIPTerm term)
    {
        synchronized(m_globalDB)
        {
            m_globalDB.asserta(Clause.getClause(term.getTerm(), getEnvVariable("enable_clause_check").equals("true")), null, true);
        }
    }

    /** It is equivalent to retract/1 predicate.
     * Retracts a fact or a clause that unifies with the given term from the interpreter database
     * and, as side-effect, unifies the given term the the one retracted.
     * @param term the term to retract.
     */
    public final boolean retract(final JIPTerm term)
    {
        synchronized(m_globalDB)
        {
            final Clause clause = Clause.getClause(term.getTerm(), getEnvVariable("enable_clause_check").equals("true"));
            final Clause retractedClause = m_globalDB.retract(clause);

            if(retractedClause == null)
            {
                return false;
            }

            return clause.unify(retractedClause, new Hashtable(10));
        }
    }

    /** It is equivalent to assert/1 predicate.
     * Add a term to the interpreter database
     * @param term the term to assert.
     */
    public final void assertz(final JIPTerm term)
    {
        synchronized(m_globalDB)
        {
            m_globalDB.assertz(Clause.getClause(term.getTerm(),getEnvVariable("enable_clause_check").equals("true")), null, true);
        }
    }

    /** It is equivalent to abolish/1 predicate.
     * abolish a term in the interpreter database
     * @param term the term to abolish.
     */
    public final void abolish(final JIPTerm term)
    {
        synchronized(m_globalDB)
        {
            m_globalDB.abolish(term.getTerm());
        }
    }

    /** Returns a reference to a synchronous query.
     * @param strQuery Query as a text string
     * @return an instantiated JIPQuery object
     * @exception JIPSyntaxErrorException
     * @see com.ugos.jiprolog.engine.JIPQuery
     * @see com.ugos.jiprolog.engine.JIPEngine#openQuery(String)
     */
    public JIPQuery openSynchronousQuery(final String strQuery) throws JIPSyntaxErrorException
    {
        return openSynchronousQuery(m_termParser.parseTerm(strQuery));
    }

    /** Returns a reference to a synchronous query.
     * @param jipquery Query to open
     * @return an instantiated JIPQuery object
     * @see com.ugos.jiprolog.engine.JIPQuery
     * @see com.ugos.jiprolog.engine.JIPEngine#openQuery(JIPTerm)
     */
    public synchronized JIPQuery openSynchronousQuery(final JIPTerm jipquery)
    {
        PrologObject query = jipquery.getTerm();

        // controlla se si tratta di una query o di un termine
        if(query instanceof Functor)
        {
            if(((Functor)query).getName().equals("?-/1"))
                query = ((Functor)query).getParams();
        }

        if(m_bTrace)
            return new JIPQuery(query, new WAMTrace(this));
        else
            return new JIPQuery(query, new WAM(this));
    }

    /** Opens a query. <br>
     * The registered JIPEventListeners will be notified in the openNotified method.<br>
     * When a solution is found the JIPEventListeners will be notified in solutionNotified.<br>
     * If an error occurs the JIPEventListeners will be notified in errorNotified.<br>
     * @param strQuery Query as a text string
     * @return The handle of the opened query
     * @exception JIPSyntaxErrorException
     * @see com.ugos.jiprolog.engine.JIPEngine#nextSolution
     * @see com.ugos.jiprolog.engine.JIPEngine#closeQuery
     * @see com.ugos.jiprolog.engine.JIPEventListener#openNotified
     */
    public int openQuery(final String strQuery) throws JIPSyntaxErrorException
    {
        return openQuery(m_termParser.parseTerm(strQuery));
    }

    /** Opens a query.<br>
     * The registered JIPEventListeners will be notified in the openNotified method.<br>
     * When an event occurs in the interpreter the registered JIPEventListeners will be notified in the corresponding <i>notified</i> method.<br>
     * If an error occurs the registered JIPEventListeners will be notified in errorNotified.<br>
     * @param jipquery Query as a JIPTerm
     * @return The handle of the opened query
     * @see com.ugos.jiprolog.engine.JIPTermParser#parseTerm
     * @see com.ugos.jiprolog.engine.JIPEngine#nextSolution
     * @see com.ugos.jiprolog.engine.JIPEngine#closeQuery
     * @see com.ugos.jiprolog.engine.JIPEventListener
     */
    public synchronized int openQuery(final JIPTerm jipquery)
    {
        // get query
        PrologObject query = jipquery.getTerm();

        // controlla se si tratta di una query o di un termine
        if(query instanceof Functor)
        {
            if(((Functor)query).getName().equals("?-/1"))
                query = ((Functor)query).getParams();
        }

        // crea una nuova WAM
        final WAM wam =
            m_bTrace ?
            new WAMTrace(this)
            :
            new WAM(this);

        // store query
        final AsyncWAMManager container = new AsyncWAMManager(wam, query, this);
        m_prologTable.put(new Integer(container.getHandle()), container);

        // notify open
        notifyOpen(container.getHandle());

        // la sincronizzazione qui non è necessaria
        // il chiamamante dovrebbe sincronizzare la chiamata e il listener
        container.start();

        return container.getHandle();
    }

    /** Searches for another soution.
     * The registered JIPEventListeners will be notified in the openNotified method.<br>
     * When an event occurs in the interpreter the registered JIPEventListeners will be notified in the corresponding <i>notified</i> method.<br>
     * If an error occurs the registered JIPEventListeners will be notified in errorNotified.<br>
     * If the query specified in the parameter is still running it raises a JIPIsRunningException
     * If the query specified in the parameter is not found it raises a JIPInvalidHandleException
     * @param nQueryHandle the handle of the query
     * @see com.ugos.jiprolog.engine.JIPEngine#openQuery(JIPTerm)
     * @see com.ugos.jiprolog.engine.JIPEngine#closeQuery
     * @see com.ugos.jiprolog.engine.JIPEventListener
     */
    public synchronized void nextSolution(final int nQueryHandle)
    {
        if (!m_prologTable.containsKey(new Integer(nQueryHandle)))
            throw new JIPInvalidHandleException();

        final AsyncWAMManager container =
            (AsyncWAMManager)m_prologTable.get(new Integer(nQueryHandle));

        if (container.isRunning())
        {
            throw new JIPIsRunningException();
        }

        notifyMore(nQueryHandle);
        synchronized(container)
        {
            container.next();
        }
    }

    /** Closes a query. If the query is still running it is closed.<br>
     * The registered JIPEventListeners will be notified in closeNotify.
     * If the query specified in the parameter is not found it raises a JIPInvalidHandleException
     * @param nQueryHandle the handle of the query to close
     * @see com.ugos.jiprolog.engine.JIPEngine#nextSolution
     * @see com.ugos.jiprolog.engine.JIPEngine#closeQuery
     * @see com.ugos.jiprolog.engine.JIPEventListener#closeNotified
     */
    public synchronized void closeQuery(final int nQueryHandle)
    {
        if (!m_prologTable.containsKey(new Integer(nQueryHandle)))
            throw new JIPInvalidHandleException();

        final AsyncWAMManager container =
            (AsyncWAMManager)m_prologTable.get(new Integer(nQueryHandle));

        // rimuove il container dalla tabella
        m_prologTable.remove(new Integer(nQueryHandle));

        // chiude container e query
        container.close();

        notifyClose(nQueryHandle);
    }

    /** Returns true if the query has more choice points on backtracking.<br>
     * If the query specified in nQueryHandle is not found it raised a JIPInvalidHandleException.<br>
     * If the query specified in nQueryHandle is still running it raises a JIPIsRunningException.<br>
     * Note: You should not call isDeterministic until the query has no reached the goal.<br>
     * @param nQueryHandle the handle of the query
     * @return true if the query has more choice points, false otherwise.
     * @see com.ugos.jiprolog.engine.JIPEngine#openQuery(JIPTerm)
     * @see com.ugos.jiprolog.engine.JIPEngine#nextSolution
     * @see com.ugos.jiprolog.engine.JIPEngine#closeQuery
     */
    public synchronized boolean hasMoreChoicePoints(final int nQueryHandle)
    {
        if (!m_prologTable.containsKey(new Integer(nQueryHandle)))
            return false;//throw new JIPInvalidHandleException();

        final AsyncWAMManager container =
            (AsyncWAMManager)m_prologTable.get(new Integer(nQueryHandle));

        // N.B. se la query è stata chiusa il container non compare nella tabella

        if (container.isRunning())
            throw new JIPIsRunningException();

        return container.hasMoreChoicePoints();
    }

    /** Closes all pending queries
     * @see com.ugos.jiprolog.engine.JIPEngine#openQuery(JIPTerm)
     * @see com.ugos.jiprolog.engine.JIPEngine#nextSolution
     * @see com.ugos.jiprolog.engine.JIPEngine#closeQuery
     */
    public synchronized void closeAllQueries()
    {
        final Enumeration en = m_prologTable.keys();
        Integer nHandle;
        while (en.hasMoreElements())
        {
            nHandle = (Integer)en.nextElement();
            final AsyncWAMManager container = (AsyncWAMManager)(m_prologTable.remove(nHandle));
            container.close();
        }
    }

    /** Sets the value of a custom environment variable.<br>
     * Environment variables may be useful in custom predicates.
     * @param varName the name of the variable
     * @param value the value of the variable
     * @see com.ugos.jiprolog.engine.JIPEngine#getEnvVariable
     * @see com.ugos.jiprolog.engine.JIPEngine#removeEnvVariable
     */
    public synchronized void setEnvVariable(final String varName, final Object value)
    {
        m_envVarTbl.put(varName, value);
    }

    /** Gets the value of a custom environment variable. <br>
     * Environment variables may be useful in custom predicates
     * @param varName the name of the variable
     * @see com.ugos.jiprolog.engine.JIPEngine#setEnvVariable
     * @see com.ugos.jiprolog.engine.JIPEngine#removeEnvVariable
     */
    public synchronized Object getEnvVariable(final String varName)
    {
        return m_envVarTbl.get(varName);
    }

    /** Removes a custom environment variable. <br>
     * Environment variables may be useful in custom predicates
     * @param varName the name of the variable to remove
     * @see com.ugos.jiprolog.engine.JIPEngine#setEnvVariable
     * @see com.ugos.jiprolog.engine.JIPEngine#getEnvVariable
     */
    public final synchronized Object removeEnvVariable(final String varName)
    {
        return m_envVarTbl.remove(varName);
    }

    /** Gets the enumeration of all custom environment variable name. <br>
     * @see com.ugos.jiprolog.engine.JIPEngine#setEnvVariable
     * @see com.ugos.jiprolog.engine.JIPEngine#removeEnvVariable
     */
    public synchronized Enumeration<String> getEnvVariableNames()
    {
        return m_envVarTbl.keys();
    }

    /** Notifies a JIPEvent. <br>
     * @param nID the ID of the event to notify
     * @param term the term attached to the event
     * @param nQueryHandle the handle of the query which notifies the event
     * @see com.ugos.jiprolog.engine.JIPEvent
     * @see com.ugos.jiprolog.engine.JIPTraceEvent
     */
    public final void notifyEvent(final int nID, final JIPTerm term, final int nQueryHandle)
    {
        notifyEvent(nID, term.getTerm(), nQueryHandle);
    }

    /** Returns the instance of JIPTermParser attached to this JIPEngine object<br>
     * Use JIPTermParser to parse prolog terms
     * @see com.ugos.jiprolog.engine.JIPTermParser
     */
    public final JIPTermParser getTermParser()
    {
        return m_termParser;
    }

    final synchronized void update(final AsyncWAMManager container)
    {
        // Serve per consentire alla open query di tornare il valore.
        // problema riscontrato su Mozilla e Linux
        // può essere eliminato poichè la sincronizzazione dovrebbe essere
        // lasciata al chimante me preferisco lasciarlo per
        // mantere la compatibiilità con le vecchie versioni
        Thread.yield();
        // credo si risolva così

        final Object obj = container.m_result;

        if(obj instanceof Throwable)
        {
            //((Throwable)obj).printStackTrace();  // DBG
            notifyException((Throwable)obj, container.getHandle());
        }
        else if(obj != null)
        {
            PrologObject solution;
            try
            {
                // la copia va fatta prima di chiedere deterministic?
                // potrebbe lanciare StackOverflowError
                solution = container.m_query.copy(true);
//                System.out.println("sol.copy() " + solution.copy());

//                System.out.println("obj " + obj);
                PrologObject cobj = ((PrologObject)obj).copy(true);

                solution.unify(cobj, new Hashtable(10));

//                System.out.println("obj.copy() " + ((PrologObject)obj).copy());
//                System.out.println("solution " + solution);
            }
            catch(Throwable er)
            {
                notifyException(new JIPJVMException(er), container.getHandle());
                return;
            }

            notifySolution(solution, container.getHandle());
        }
        else
        {
            // end reached
            notifyEnd(container.getHandle());
        }
    }

    final BuiltInFactory getBuiltInFactory()
    {
        return m_builtInFactory;
    }

    final GlobalDB getGlobalDB()
    {
        return m_globalDB;
    }

    final EventNotifier getEventNotifier()
    {
        return m_eventNotifier;
    }

    final OperatorManager getOperatorManager()
    {
        return m_opManager;
    }

    final void notifyEvent(final int nID, final PrologObject solution, final int nQueryHandle)
    {
        m_eventNotifier.notifyEvent(nID, solution, nQueryHandle);
    }

    private final void notifySolution(final PrologObject solution, final int nQueryHandle)
    {
        m_eventNotifier.notifyEvent(JIPEvent.ID_SOLUTION, solution, nQueryHandle);
    }

    private final void notifyOpen(final int nQueryHandle)
    {
        final AsyncWAMManager container =
            (AsyncWAMManager)m_prologTable.get(new Integer(nQueryHandle));

        m_eventNotifier.notifyEvent(JIPEvent.ID_OPEN, container.m_query, nQueryHandle);
    }

    private final void notifyMore(final int nQueryHandle)
    {
        final AsyncWAMManager container =
            (AsyncWAMManager)m_prologTable.get(new Integer(nQueryHandle));

        m_eventNotifier.notifyEvent(JIPEvent.ID_MORE, container.m_query, nQueryHandle);
    }

    private final void notifyEnd(final int nQueryHandle)
    {
        final AsyncWAMManager container =
            (AsyncWAMManager)m_prologTable.get(new Integer(nQueryHandle));

        m_eventNotifier.notifyEvent(JIPEvent.ID_END, container.m_query, nQueryHandle);
    }

    private final void notifyClose(final int nQueryHandle)
    {
        m_eventNotifier.notifyEvent(JIPEvent.ID_CLOSE, null, nQueryHandle);
    }

    final void notifyException(final Throwable err, final int nQueryHandle)
    {
//        if(err instanceof UndefinedPredicateException)
//        {
//            notifyUndefPredicate(((UndefinedPredicateException)err).getTerm().getTerm(), nQueryHandle);
//        }
//      else
        if(err instanceof JIPRuntimeException)
        {
            m_eventNotifier.notifyErrorEvent(nQueryHandle, (JIPRuntimeException)err);
        }
        else
        {
            m_eventNotifier.notifyErrorEvent(nQueryHandle, new JIPJVMException(err));
        }
    }

//    private final void notifyUndefPredicate(final PrologObject term, final int nQueryHandle)
//    {
//        m_eventNotifier.notifyEvent(JIPEvent.ID_UNDEFPREDICATE, term.copy(), nQueryHandle);
//    }

    /** Releases the resources used by JIProlog
     * Developers should not call it directly.
     */
    protected void finalize()
    {
        m_eventNotifier.setEnabled(false);
        m_eventNotifier = null;
        closeAllQueries();
    }


    static final synchronized ClassLoader getClassLoader()
    {
        return s_classLoader;
    }
}
