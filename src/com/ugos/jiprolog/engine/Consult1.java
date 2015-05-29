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

import java.io.*;
import java.util.*;

import com.ugos.io.PushbackLineNumberInputStream;

class Consult1 extends BuiltIn
{
    public boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        final PrologObject path = getRealTerm(getParam(1));

        String strPath;

        try
        {
            if(path instanceof Atom)
            {
                strPath = ((Atom)path).getName();
                consult(strPath, getJIPEngine(), getQueryHandle());
            }
            else if(path instanceof PString)
            {
                strPath = ((PString)path).getString();
                consult(strPath, getJIPEngine(), getQueryHandle());
            }
            else if(path instanceof List)
            {
                PrologObject head = getRealTerm(((List)path).getHead());
                PrologObject tail = getRealTerm(((List)path).getTail());
                while(head != null)
                {
                    if(head instanceof Atom)
                    {
                        strPath = ((Atom)head).getName();
                    }
                    else if(head instanceof PString)
                    {
                        strPath = ((PString)head).getString();
                    }
                    else
                    {
                        throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, head);
                    }

                    consult(strPath, getJIPEngine(), getQueryHandle());

                    if(tail != null)
                    {
                        head = getRealTerm(((List)tail).getHead());
                        tail = getRealTerm(((List)tail).getTail());
                    }
                    else
                        head = null;
                }
            }
            else
            {
                throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, path);
            }
        }
        catch(IOException ex)
        {
            throw new JIPJVMException(ex);
        }

        return true;
    }

    static final void consult(String strPath, JIPEngine engine, int nQueryHandle) throws IOException
    {
    	boolean enableClauseChecks = engine.getEnvVariable("enable_clause_check").equals("true");

        String strOldSearchPath = null;
        try
        {
            String strFileName[] = new String[1];
            String strCurDir[] = new String[1];

            InputStream ins;
            try
            {
                ins = StreamManager.getStreamManager().getInputStream(strPath, engine.getSearchPath(), strFileName, strCurDir);
            }
            catch(IOException ex)
            {
                if(strPath.indexOf(".") == -1)//endsWith(".pl"))
                {
                    strPath += ".pl";
                    consult(strPath, engine, nQueryHandle);
                    return;
                }
                else
                    throw ex;
            }

            //System.out.println(strFileName[0]);
            strOldSearchPath = engine.getSearchPath();
            engine.setSearchPath(strCurDir[0]);
            Vector<PrologObject> initializationVector = consult(ins, strFileName[0], engine, nQueryHandle, enableClauseChecks);
            engine.setSearchPath(strOldSearchPath);

            ins.close();

            final WAM wam = new WAM(engine);

            for(PrologObject goal : initializationVector)
            {
            	// chiama la wam
                if(!wam.query(goal))
                {
                    wam.closeQuery();
                    throw JIPRuntimeException.createRuntimeException(27, strFileName[0] + ". goal: " + goal.toString(engine));
                }

                wam.closeQuery();
            }

        }
        catch(SecurityException ex)
        {
            if(strOldSearchPath != null)
                engine.setSearchPath(strOldSearchPath);

            throw new JIPPermissionException("access", "source_sink", Atom.createAtom(strPath));
//            throw JIPRuntimeException.create(9, "consult " + strPath);
        }
        catch(JIPRuntimeException ex)
        {
        	ex.printStackTrace();
            if(strOldSearchPath != null)
                engine.setSearchPath(strOldSearchPath);
            throw ex;
        }
    }

    static final Vector<PrologObject> consult(InputStream ins, String strStreamName, JIPEngine engine, int nQueryHandle, boolean enableClauseChecks)
    {
//        System.out.println("consult");

        int strOldInputStreamName = 0;
        InputStream oldins = null;
        try
        {
        	Vector<PrologObject> initializationVector = new Vector<PrologObject>();

            oldins = engine.getCurrentInputStream();
            strOldInputStreamName = engine.getCurrentInputStreamHandle();
            engine.setCurrentInputStream(ins, strStreamName.hashCode());

            // unconsult the file
            engine.getGlobalDB().unconsult(strStreamName);

            try
            {
//                System.out.println(engine.getCurrentEncoding());

                ParserReader pins = new ParserReader(new PushbackLineNumberInputStream(ins));
                PrologParser parser = new PrologParser(pins, engine.getOperatorManager(), engine, strStreamName);

                PrologObject term;
                final Hashtable<String, String> exportTbl = new Hashtable<String, String>(20);

                exportTbl.put("#module", GlobalDB.USER_MODULE);
                final WAM wam = new WAM(engine);
                while ((term = parser.parseNext()) != null)
                {
                    //System.out.println(term);
                    _assert(term, engine, strStreamName, pins, exportTbl, initializationVector, wam, enableClauseChecks);

                    Hashtable<String, Variable> singletonVars = parser.getSingletonVariables();
                    //System.out.println(singletonVars);
                    if(!singletonVars.isEmpty())
                        notifySingletonVars(singletonVars, pins, engine, nQueryHandle);
                }

                //pins.close();
                ins.close();
            }
            catch(IOException ex)
            {
                engine.setCurrentInputStream(oldins, strOldInputStreamName);
                try
                {
                    ins.close();
                }
                catch(IOException ex1)
                {}

                JIPJVMException jvmex = new JIPJVMException(ex);
                jvmex.m_engine = engine;
                throw jvmex;
                //throw new JIPRuntimeException("Unable to consult " + strStreamName + ": " + ex.toString());
            }

            engine.setCurrentInputStream(oldins, strOldInputStreamName);

            return initializationVector;
        }
        catch(SecurityException ex)
        {
            if(oldins != null)
                engine.setCurrentInputStream(oldins, strOldInputStreamName);

            try
            {
                ins.close();
            }
            catch(IOException ex1)
            {}

            throw new JIPPermissionException("access", "source_sink", Atom.createAtom(strStreamName), engine);
        }
        catch(JIPSyntaxErrorException ex)
        {
            if(oldins != null)
                engine.setCurrentInputStream(oldins, strOldInputStreamName);
            ex.m_strFileName = strStreamName;

            try
            {
                ins.close();
            }
            catch(IOException ex1)
            {}

            ex.m_engine = engine;

            throw ex;
        }
        catch(JIPRuntimeException ex)
        {
            if(oldins != null)
                engine.setCurrentInputStream(oldins, strOldInputStreamName);
            ex.m_strFileName = strStreamName;

            try
            {
                ins.close();
            }
            catch(IOException ex1)
            {}

            ex.m_engine = engine;

            throw ex;
        }
    }

    protected final static void _assert(PrologObject pred, JIPEngine engine, String strPath, ParserReader pins, Hashtable<String, String> exportTbl, Vector<PrologObject> initializationVector, WAM wam, boolean enableClauseChecks)
    {
//        System.out.println("ASSERT");  //DBG
//        System.out.println(pred);  //DBG
//
        try
        {
        	GlobalDB globalDB = engine.getGlobalDB();

            String strModuleName = (String)exportTbl.get("#module");

            // directive
            if(pred instanceof Functor &&
              (((Functor)pred).getName().equals(":-/1") || ((Functor)pred).getName().equals("?-/1")))
            {
                // si tratta di una direttiva o una query
                // controlla se si tratta di :-module(moduleName,  [exportList]).
                Functor funct = ((Functor)pred);
                PrologObject first = getRealTerm(funct.getParams().getHead());

                // :-module
                if(first instanceof Functor && ((Functor)first).getName().equals("module/2"))
                {
                    if(exportTbl.size() > 1)
                        throw JIPRuntimeException.createRuntimeException(24, strPath);

                    ConsCell params = ((Functor)first).getParams();

                    strModuleName = ((Atom)params.getHead()).getName();
                    exportTbl.put("#module", strModuleName);
                    List exportList = (List)((ConsCell)params.getTail()).getHead();
                    while(exportList != null)
                    {
                        PrologObject head = getRealTerm(exportList.getHead());
                        //System.out.println(head);  //DBG
                        //System.out.println("((Functor)head).getName() " + ((Functor)head).getName());  //DBG
                        if(head instanceof Functor && ((Functor)head).getAtom().equals(Atom.SLASHSLASH))
                        {
                            ConsCell parms = ((Functor )head).getParams();
                            String strPredDef = new StringBuilder(((Atom)parms.getHead()).getName()).append('/').append(((ConsCell)parms.getTail()).getHead()).toString();
                            //System.out.println("strPredDef " + strPredDef);  //DBG
                            exportTbl.put(strPredDef, strModuleName);
                            globalDB.setExported(strPredDef);
                        }
                        else
                        {
                            throw JIPRuntimeException.createRuntimeException(47, new StringBuilder(strPath).append("-").append(head.toString(engine)).toString());
                            //throw new JIPTypeException(1, head);
                        }

                        exportList = (List)exportList.getTail();
                    }
                }
                // :-initialization/1
                else if(first instanceof Functor && ((Functor)first).getName().equals("initialization/1"))
                {
                    ConsCell goal = ((Functor)first).getParams();
                    initializationVector.add(goal);
                }
                else  // :-goal
                {
                    // chiama la wam
                    if(!wam.query(funct.getParams()))
                    {
                        wam.closeQuery();
                        throw JIPRuntimeException.createRuntimeException(47, new StringBuilder(strPath).append("-").append(funct.toString(engine)).toString());
                    }

                    wam.closeQuery();
                }
            }
            else
            {
                Clause clause = Clause.getClause(pred, strModuleName, enableClauseChecks);
                clause.setFileName(strPath);
                if(pins != null)
                {
//                	System.out.println("line number " + pins.getLineNumber());

//                    clause.setLineNumber(pins.getLineNumber());
//                    clause.setPosition(pins.getRead());
                }

                if(exportTbl.containsKey(((Functor)clause.getHead()).getName()))//.getAtom()))
                {
//                        System.out.println("Exported");
                    clause.setExported();


//                    clause.setModuleName(GlobalDB.USER_MODULE);
                }

//                  System.out.println("ASSERT: " + clause);  //DBG
                globalDB.assertzNoCopy(clause, strPath, false);
            }
        }
        catch(ClassCastException ex)
        {
//            ex.printStackTrace();
            throw JIPRuntimeException.createRuntimeException(21, strPath + "-" + pred.toString(engine));
        }
//        catch(JIPTypeException ex)
//        {
//            throw JIPRuntimeException.create(1, pred.toString(engine) + " at line " + pins.getLineNumber());
//        }
    }

    private static void notifySingletonVars(Hashtable singletonVars, ParserReader pins, JIPEngine engine, int nQueryHandle)
    {
        Enumeration en = singletonVars.keys();
        ConsCell cons = null;
        while (en.hasMoreElements())
        {
            cons = new ConsCell(Atom.createAtom((String)en.nextElement()), cons);
        }

        cons = cons.reverse();
        cons = new ConsCell(Expression.createNumber(pins.getLineNumber()), cons);

        engine.notifyEvent(JIPEvent.ID_SINGLETONVARS, cons, nQueryHandle);
    }
}



