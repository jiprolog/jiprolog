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
                        throw new JIPParameterTypeException(1,JIPParameterTypeException.ATOM_OR_STRING);
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
                throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM_OR_STRING);
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
            consult(ins, strFileName[0], engine, nQueryHandle);
            engine.setSearchPath(strOldSearchPath);
        
            ins.close();
        }
        catch(SecurityException ex)
        {
            if(strOldSearchPath != null)
                engine.setSearchPath(strOldSearchPath);
            
            throw JIPRuntimeException.create(9, "consult " + strPath);
        }
        catch(JIPRuntimeException ex)
        {
            if(strOldSearchPath != null)
                engine.setSearchPath(strOldSearchPath);
            throw ex;
        }
    }
    
    static final void consult(InputStream ins, String strStreamName, JIPEngine engine, int nQueryHandle)
    {
//        System.out.println("consult");
        
        String strOldInputStreamName = null;
        InputStream oldins = null;
        try
        {
            oldins = engine.getCurrentInputStream();
            strOldInputStreamName = engine.getCurrentInputStreamName();
            engine.setCurrentInputStream(ins, strStreamName);
            // unconsult the file
            engine.getGlobalDB().unconsult(strStreamName);
            
            //ParserInputStream pins = new ParserInputStream(ins);
            //PrologParser parser = new PrologParser(pins, engine.getOperatorManager(), strStreamName);
            
            try
            {
//                System.out.println(engine.getCurrentEncoding());
                
                ParserReader pins = new ParserReader(new InputStreamReader(ins, engine.getEncoding()));
                PrologParser parser = new PrologParser(pins, engine.getOperatorManager(), strStreamName);
                
                PrologObject term;
                final Hashtable<String, String> exportTbl = new Hashtable<String, String>(20);
                exportTbl.put("#module", GlobalDB.USER_MODULE);
                final WAM wam = new WAM(engine);
                while ((term = parser.parseNext()) != null)
                {
                    //System.out.println(term);
                    _assert(term, engine, strStreamName, pins, exportTbl, wam);
                    
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
                
                throw new JIPJVMException(ex);
                //throw new JIPRuntimeException("Unable to consult " + strStreamName + ": " + ex.toString());
            }
            
            engine.setCurrentInputStream(oldins, strOldInputStreamName);
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
            
            throw JIPRuntimeException.create(9, "consult " + strStreamName);
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
            
            throw ex;
        }
    }
            
    protected final static void _assert(PrologObject pred, JIPEngine engine, String strPath, ParserReader pins, Hashtable exportTbl, WAM wam)
    {
//        System.out.println("ASSERT");  //DBG
//        System.out.println(pred);  //DBG
//
        try
        {
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
                        throw JIPRuntimeException.create(24, strPath);
                        
                    ConsCell params = ((Functor)first).getParams();
                        
                    strModuleName = ((Atom)params.getHead()).getName();
                    exportTbl.put("#module", strModuleName);
                    List exportList = (List)((ConsCell)params.getTail()).getHead();
                    while(exportList != null)
                    {
                        PrologObject head = getRealTerm(exportList.getHead());
                        //System.out.println(head);  //DBG
                        //System.out.println("((Functor)head).getName() " + ((Functor)head).getName());  //DBG
                        if(head instanceof Functor && ((Functor)head).getName().equals("//2"))
                        {
                            ConsCell parms = ((Functor )head).getParams();
                            String strPredDef = ((Atom)parms.getHead()).getName() + "/" + ((ConsCell)parms.getTail()).getHead();
                            //System.out.println("strPredDef " + strPredDef);  //DBG
                            exportTbl.put(strPredDef, strModuleName);
                        }
                        else
                        {
                            throw JIPRuntimeException.create(47, strPath + "-" + head.toString(engine));
                            //throw new JIPParameterTypeException(1, head);
                        }
                        
                        exportList = (List)exportList.getTail();
                    }
                }
                else  // :-goal
                {
                    // chiama la wam
                    if(!wam.query(funct.getParams()))
                    {
                        wam.closeQuery();
                        throw JIPRuntimeException.create(27, strPath + "-" + funct.toString(engine));
                    }

                    wam.closeQuery();
                }
            }
            else
            {
                Clause clause = Clause.getClause(pred, strModuleName);
                clause.setFileName(strPath);
                if(pins != null)
                {
                    clause.setLineNumber(pins.getLineNumber());
                    clause.setPosition(pins.getRead());
                }

                if(exportTbl.containsKey(((Functor)clause.getHead()).getName()))//.getAtom()))
                {
//                        System.out.println("Exported");
                    clause.setExported();
                }
                
//                  System.out.println("ASSERT: " + clause);  //DBG
                engine.getGlobalDB().assertz(clause, strPath);
            }
        }
        catch(ClassCastException ex)
        {
//            ex.printStackTrace();
            throw JIPRuntimeException.create(21, strPath + "-" + pred.toString(engine));
        }
        catch(JIPParameterTypeException ex)
        {
            throw JIPRuntimeException.create(1, pred.toString(engine) + " at line " + pins.getLineNumber());
        }
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



