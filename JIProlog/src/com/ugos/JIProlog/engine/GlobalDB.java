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

package com.ugos.JIProlog.engine;

import java.util.*;
import java.io.*;

final class GlobalDB extends Object// implements Cloneable //Serializable
{
	private static final String[] INTERNAL_MODULES = {"jipxlist", "jipsys", "jipxdb", "jipxexception", "jipxio", "jipxreflect", "jipxsets", "jipxsystem", "jipxterms", "jipxxml"};
	
    // Database
    private Hashtable<String, JIPClausesDatabase> m_clauseTable;
    // associazione tra predicati e file
    private Hashtable m_pred2FileMap;
    
    private Hashtable m_moduleTransparentTbl;

    static final String SYSTEM_MODULE = "$system";
    static final String USER_MODULE   = "$user";
    static final String KERNEL_MODULE = "$kernel";
    
    boolean m_bCheckDisabled = false;
    
    //#ifndef _MIDP
    private GlobalDB(GlobalDB gdb)
    {
        m_clauseTable            = (Hashtable<String, JIPClausesDatabase>)gdb.m_clauseTable.clone();
        m_pred2FileMap           = (Hashtable)gdb.m_pred2FileMap.clone();
        m_moduleTransparentTbl   = (Hashtable)gdb.m_moduleTransparentTbl.clone();
    }
    
    public final GlobalDB newInstance()
    {
        return new GlobalDB(this);
    }
    
    //#endif
    
    public GlobalDB()
    {
        m_clauseTable            = new Hashtable<String, JIPClausesDatabase>(100);
        m_pred2FileMap           = new Hashtable(100);
        m_moduleTransparentTbl   = new Hashtable(100);
        loadKernel(this);
    }
    
    
    
    final boolean isMultifile(final String strPredName)
    {
        if(m_clauseTable.containsKey(USER_MODULE + ":" + strPredName))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(USER_MODULE + ":" + strPredName)).isMultifile();
        }
        
        return false;
    }
    
    final void multifile(final String strPredName)
    {
        int nPos = strPredName.lastIndexOf('/');
        if(nPos < 0)
            throw new JIPParameterTypeException(1, JIPParameterTypeException.PREDICATE_INDICATOR);
        
        final String strDef = USER_MODULE + ":" + strPredName;
        JIPClausesDatabase db;
        if(m_clauseTable.containsKey(strDef))
        {
            db = ((JIPClausesDatabase)m_clauseTable.get(strDef));
        }
        else
        {
            final String strName = strPredName.substring(0, nPos);
            final int nArity = Integer.parseInt(strPredName.substring(nPos + 1));
            db = new DefaultClausesDatabase(strName, nArity);
            //db = new DefaultClausesDatabase();
            // Aggiunge il vettore alla tabella
            m_clauseTable.put(strDef, db);
        }
        
        db.setMultifile();
    }
        
    final void moduleTransparent(final String strPredName)
    {
        if(isSystem(strPredName) && !m_bCheckDisabled)
            throw JIPRuntimeException.create(46, Atom.createAtom(strPredName));
        
        m_moduleTransparentTbl.put(strPredName, strPredName);
        
        JIPClausesDatabase db = (JIPClausesDatabase)m_clauseTable.get(USER_MODULE + ":" + strPredName);
        if(db == null)
            db = (JIPClausesDatabase)m_clauseTable.get(SYSTEM_MODULE + ":" + strPredName);
        
        if(db != null)
            db.setModuleTransparent();
            
        
//      final String strDef = strModule + ":" + strPredName;
//        JIPClausesDatabase db;
//        if(m_clauseTable.containsKey(strDef))
//        {
//            db = ((JIPClausesDatabase)m_clauseTable.get(strDef));
//        }
//        else
//        {
//            db = new DefaultClausesDatabase();
//            // Aggiunge il vettore alla tabella
//            m_clauseTable.put(strDef, db);
//        }
//
//        db.setModuleTransparent();
    }
    
    final boolean isModuleTransparent(final String strPredName)
    {
        if(m_clauseTable.containsKey(USER_MODULE + ":" + strPredName))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(USER_MODULE + ":" + strPredName)).isModuleTransparent();
        }
        
        return false;
    }

    public final boolean isDynamic(final String strPredName)
    {
        if(m_clauseTable.containsKey(USER_MODULE + ":" + strPredName))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(USER_MODULE + ":" + strPredName)).isDynamic();
        }
        
        return false;
    }
    
    final void dynamic(final String strPredName)
    {
        int nPos = strPredName.lastIndexOf('/');
        if(nPos < 0)
            throw new JIPParameterTypeException(1, JIPParameterTypeException.PREDICATE_INDICATOR);
        
        final String strDef = USER_MODULE + ":" + strPredName;
        JIPClausesDatabase db;
        if(m_clauseTable.containsKey(strDef))
        {
            db = ((JIPClausesDatabase)m_clauseTable.get(strDef));
        }
        else
        {
            final String strName = strPredName.substring(0, nPos);
            final int nArity = Integer.parseInt(strPredName.substring(nPos + 1));
            db = new DefaultClausesDatabase(strName, nArity);
            //db = new DefaultClausesDatabase();
            // Aggiunge il vettore alla tabella
            m_clauseTable.put(strDef, db);
        }
        
        db.setDynamic();
    }

    public final boolean isExternal(final String strPredName)
    {
        if(m_clauseTable.containsKey(USER_MODULE + ":" + strPredName))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(USER_MODULE + ":" + strPredName)).isExternal();
        }
        
        return false;
    }
    
    final void external(final String strPredName)
    {
        int nPos = strPredName.lastIndexOf('/');
        if(nPos < 0)
            throw new JIPParameterTypeException(1, JIPParameterTypeException.PREDICATE_INDICATOR);
        
        final String strDef = USER_MODULE + ":" + strPredName;
        JIPClausesDatabase db;
        if(m_clauseTable.containsKey(strDef))
        {
            db = ((JIPClausesDatabase)m_clauseTable.get(strDef));
        }
        else
        {
            final String strName = strPredName.substring(0, nPos);
            final int nArity = Integer.parseInt(strPredName.substring(nPos + 1));
            db = new DefaultClausesDatabase(strName, nArity);
            //db = new DefaultClausesDatabase();
            // Aggiunge il vettore alla tabella
            m_clauseTable.put(strDef, db);
        }
        
        db.setExternal();
    }
    
    public final boolean isSystem(final String strName)
    {
        return  m_clauseTable.containsKey(SYSTEM_MODULE + ":" + strName) ||
        		m_clauseTable.containsKey(KERNEL_MODULE + ":" + strName) || 
                BuiltInFactory.isBuiltIn(strName) ||
                strName.equals(",/2");
    }
    
    final boolean isSystem(final Clause clause)
    {
        return isSystem((Functor)clause.getHead());
    }
    
    final boolean isSystem(final Functor funct)
    {
        return isSystem(funct.getName());
    }
    
    final boolean isInternal(final String funct)
    {
    	if(isSystem(funct))
    	{
    		return true;
    	}
    	else
    	{
    		for(String module : INTERNAL_MODULES)
    		{
    			if(m_clauseTable.containsKey(module + ":" + funct))
    				return true;
    		}
    	}
		
		return false;
    }
    
    final String getFile(final String strName)
    {
        if(m_pred2FileMap.containsKey(strName))
            return (String)m_pred2FileMap.get(strName);
        else if(m_pred2FileMap.containsKey(USER_MODULE + ":" + strName))
            return (String)m_pred2FileMap.get(USER_MODULE + ":" + strName);
        else if(m_pred2FileMap.containsKey(SYSTEM_MODULE + ":" + strName))
               return (String)m_pred2FileMap.get(SYSTEM_MODULE + ":" + strName);
        else if(m_pred2FileMap.containsKey(KERNEL_MODULE + ":" + strName))
            return (String)m_pred2FileMap.get(KERNEL_MODULE + ":" + strName);
        else
            return null;
    }
    
    final void assertz(final Clause clause, String strFile)
    {
        addPredicate((Clause)fixTerm(clause.copy()), false, strFile);
    }
    
    final void asserta(final Clause clause, String strFile)
    {
        addPredicate((Clause)fixTerm(clause.copy()), true, strFile);
    }
    
    final Clause retract(Clause clause)
    {
//      System.out.println("retract");
                
        Functor functor = (Functor)clause.getHead();
        
//      System.out.println("functor " + functor);  // DBG
        
        if(isSystem(functor.getName()))
            throw JIPRuntimeException.create(13, functor);
        
        JIPClausesDatabase db;
        db = search(functor, clause.getModuleName());

        if(db == null)
            return null;

        Enumeration en = db.clauses();

        if(!en.hasMoreElements())
            return null;

//      System.out.println("more");
        boolean bFound = false;
        Clause currentClause = null;
        while(en.hasMoreElements() && !bFound)
        {
            currentClause = ((Clause)en.nextElement());//.getHead();
            
            if(clause.getTail() == null)
            {
                // si tratta solo di funtore
                bFound = functor.unifiable(currentClause.getHead());
            }
            else
            {
                // si tratta di clausola
                bFound = clause.unifiable(currentClause);
            }
        }

        if (bFound)
        {
            db.removeClause(new JIPClause(currentClause));
            return currentClause;
        }
        else
        {
            return null;
        }
    }

    final void abolish(PrologObject pred)
    {
        if(pred instanceof Variable)
            pred = ((Variable)pred).getObject();
        
        if(!(pred instanceof List))
            pred = new ConsCell(pred, null);
        
        String strModuleName = USER_MODULE;
        PrologObject head = pred;
        try
        {
            while (pred != null)
            {
                head = ((ConsCell)pred).getHead();
                if(head instanceof Variable)
                    head = ((Variable)head).getObject();
                        
                // controlla se :/2
                if(head instanceof Functor && ((Functor)head).getName().equals(":/2"))
                {
                    strModuleName = ((Atom)((Functor)head).getParams().getHead()).getName();
                    head = ((ConsCell)((Functor)head).getParams().getTail()).getHead();
                }
                
                String strPredDef;
                // head deve essere instanza di funtore /2 del tipo name/arity
                if(head instanceof Functor && ((Functor)head).getName().equals("//2"))
                {
                    ConsCell params = ((Functor )head).getParams();
                    strPredDef = ((Atom)params.getHead()).getName() + "/" + ((ConsCell)params.getTail()).getHead();
                }
                else
                {
                    throw new JIPParameterTypeException(1, JIPParameterTypeException.PREDICATE_INDICATOR);
                }
                
    
    //            if(head instanceof Atom)  // atom deve essere un preddef
    //                head = new Functor(((Atom)head).getName(), null);
                            
//                  if(isSystem(((Functor)head).getName()))
//                      throw JIPRuntimeException.create(18, head, null);
                    if(isSystem(strPredDef))
                        throw JIPRuntimeException.create(12, head);
            
                // aggiungere il modulo
                
                //m_clauseTable.remove(strModuleName + ":" + ((Functor)head).getName());
                m_clauseTable.remove(strModuleName + ":" + strPredDef);
                
                pred = ((ConsCell)pred).getTail();
            }
        }
        catch(ClassCastException ex)
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.PREDICATE_INDICATOR);
        }
    }

    // called by assert
    private final synchronized void addPredicate(final Clause clause, final boolean bFirst, final String strFile)
    {
//      System.out.println("addpredicate");
//      System.out.println(clause.getHead());
//      System.out.println(clause.getModuleName());
      
      //System.out.println(clause);
        if(!m_bCheckDisabled && isSystem((Functor)clause.getHead()))
            throw JIPRuntimeException.create(15, ((Functor)clause.getHead()).getName());
      
       
        // Estrae il nome del funtore
        PrologObject head = clause.getHead();
        
        // qui controllare se il funtore è nella export list corrente.
        // se si controllare se l'eventuale modulo specificato corrisponde a
        // quello di definizione. in tal caso la specifica di modulo va ignorata
        String strFunctName = clause.isExported() ? GlobalDB.USER_MODULE : clause.getModuleName();
                
        if (head instanceof Functor)
        {
            strFunctName += ":" + ((Functor)head).getName();
        }
        else
        {
            throw JIPRuntimeException.create(11, head);
        }
        
        JIPClausesDatabase db;
        
//        System.out.println("ASSERT:"); //DEBUG
//        System.out.println(strFunctName); //DEBUG
//        System.out.println(clause); //DEBUG
//        System.out.println("File: " + strFile); //DEBUG
        
        
        // verifica se il predicato è stato già asserted in un altro file
        if(strFile != null)
        {
            if(m_pred2FileMap.containsKey(strFunctName))
            {
//             System.out.println("contains: "); //DEBUG
                String strFileName = (String)m_pred2FileMap.get(strFunctName);
                if(!strFileName.equals(strFile) && !isMultifile(((Functor)head).getName()))
                {
                    throw JIPRuntimeException.create(5, strFunctName);
                }
            }
        }

        // Verifica l'esistenza del funtore
        if(m_clauseTable.containsKey(strFunctName))
        {
//            System.out.println("database: " + strFunctName);
            
            // Estrae il database
            db = (JIPClausesDatabase)(m_clauseTable.get(strFunctName));
            
            if(bFirst)
            {
                // Aggiunge il predicato
                if(!db.addClauseAt(0, new JIPClause(clause)))
                    throw JIPRuntimeException.create(10, clause);
            }
            else
            {
                // Aggiunge il predicato
                if(!db.addClause(new JIPClause(clause)))
                    throw JIPRuntimeException.create(10, clause);
            }
        }
        else
        {
//            System.out.println("new database: " + strFunctName);
            // Crea un nuovo database
            db = new DefaultClausesDatabase(((Functor)head).getFriendlyName(), ((Functor)head).getArity());
            
            // Aggiunge il predicato
            if(!db.addClause(new JIPClause(clause)))
                throw JIPRuntimeException.create(10, clause);
            
            // Aggiunge il vettore alla tabella
            m_clauseTable.put(strFunctName, db);
        }
        
        if(strFile != null)
            m_pred2FileMap.put(strFunctName, strFile);
        
        if(m_moduleTransparentTbl.containsKey(strFunctName))
            db.setModuleTransparent();
    }
                
    final synchronized void addClausesDatabase(final JIPClausesDatabase db, final String strModuleName, final String strFuncName)
    {
        // qui va inserita con modulo user
        m_clauseTable.put(strModuleName + ":" + strFuncName, db);
    }
    
    // called by rulesenumeration
    final synchronized JIPClausesDatabase search(final Functor funct, final String strModule)
    {
//        System.out.println("SearchFor: " + strModule + ":" + funct.getName());  // DBG
        
        JIPClausesDatabase db = (JIPClausesDatabase)m_clauseTable.get(strModule + ":" + funct.getName());
        
        if(db == null)
        {
//            System.out.println("not found in " + strModule);
//            System.out.println("search in:" + USER_MODULE );  // DBG);
            
            db = (JIPClausesDatabase)m_clauseTable.get(USER_MODULE + ":" + funct.getName());
            if(db == null)
            {
//                System.out.println("not found in " + USER_MODULE);
//                System.out.println("search in:" + SYSTEM_MODULE);  // DBG);

                return (JIPClausesDatabase)m_clauseTable.get(SYSTEM_MODULE + ":" + funct.getName());
            }
        }
                
        return db;
    }
    
    private static final synchronized void loadKernel(GlobalDB gdb)
    {
        //BuiltInFactory.removeStatic();

        try
        {
            InputStream ins = gdb.getClass().getResourceAsStream(JIPEngine.RESOURCEPATH + "JIPKernel.txt");
            
            PrologParser parser = new PrologParser(new ParserReader(new InputStreamReader(ins)), new OperatorManager(), "JIPKernel.txt");
            
            gdb.m_bCheckDisabled = true;
            
            PrologObject term;
            
            while ((term = parser.parseNext()) != null)
            {
                //System.out.println(term);
                gdb.assertz(Clause.getClause(term), "__KERNEL__");
            }
            
            
            ins.close();
            
            gdb.moduleTransparent("\\+/1");
            gdb.moduleTransparent("not/1");
            gdb.moduleTransparent(";/2");
            gdb.moduleTransparent("->/2");
            gdb.moduleTransparent("^/2");
            
            gdb.m_bCheckDisabled = false;
        }
        catch(IOException ex)
        {
            gdb.m_bCheckDisabled = false;
            throw new JIPRuntimeException("Unable to load Kernel: " + ex.toString());
        }
        catch(JIPSyntaxErrorException ex)
        {
            gdb.m_bCheckDisabled = false;
            throw new JIPRuntimeException("Unable to load Kernel: " + ex.toString());
        }
        catch(NullPointerException ex)
        {
            //ex.printStackTrace();
            gdb.m_bCheckDisabled = false;
            throw new JIPRuntimeException("JIProlog Kernel is not found. JIPKernel.txt should be in the classpath.");
        }
    }
        
//    final Vector listing()
//    {
//        Vector vect = new Vector(m_clauseTable.size());
//
//        Enumeration enum = m_clauseTable.keys();
//
//        while(enum.hasMoreElements())
//        {
//            String strPredDef = (String)enum.nextElement();
//            if(strPredDef.startsWith(SYSTEM_MODULE) || strPredDef.startsWith(USER_MODULE))
//            {
//                vect.addElement(strPredDef.substring(strPredDef.indexOf(':') + 1));
//            }
//        }
//
//        return vect;
//    }
    
    Enumeration<JIPClausesDatabase> databases()
    {
        return m_clauseTable.elements();
    }

    private static final PrologObject fixTerm(final PrologObject term)
    {
//      System.out.println("fixterm");
//      System.out.println(term);
        
        if(term instanceof BuiltInPredicate)
        {
            //final PrologObject newParam = fixTerm(((BuiltInPredicate)term).getParams());
            
            return new BuiltInPredicate(((Functor)term).getAtom(), (ConsCell)fixTerm(((BuiltInPredicate)term).getParams()));
        }
        else if(term instanceof Functor)
        {
            //final PrologObject newParam = fixTerm(((Functor)term).getParams());
            
            return new Functor(((Functor)term).getAtom(), (ConsCell)fixTerm(((Functor)term).getParams()));
        }
        else if (term instanceof PString)
        {
            //System.out.println("ConsCell");

            return new PString(new List(fixTerm(((ConsCell)term).getHead()), fixTerm(((ConsCell)term).getTail())));
        }
        else if (term instanceof List)
        {
            //System.out.println("ConsCell");
            
            if(term == List.NIL)
                return term;
            else
                return new List(fixTerm(((ConsCell)term).getHead()), fixTerm(((ConsCell)term).getTail()));
        }
//        else if (term instanceof DCGClause)
//        {
//            //System.out.println("Clause");
//
//            return new DCGClause(fixTerm(((ConsCell)term).getHead()), (ConsCell)fixTerm(((ConsCell)term).getTail()));
//        }
        else if (term instanceof Clause)
        {
            //System.out.println("Clause");
            Clause clause = new Clause(((Clause)term).getModuleName(), (Functor)fixTerm(((ConsCell)term).getHead()), (ConsCell)fixTerm(((ConsCell)term).getTail()));
            if(((Clause)term).isExported())
                clause.setExported();
            
            clause.setFileName(((Clause)term).getFileName());
            clause.setLineNumber(((Clause)term).getLineNumber());
            clause.setPosition(((Clause)term).getPosition());
            
            return clause;
        }
        else if (term instanceof ConsCell)
        {
            if(term == ConsCell.NIL)
                return term;
            else
                return new ConsCell(fixTerm(((ConsCell)term).getHead()), fixTerm(((ConsCell)term).getTail()));
        }
        else if(term instanceof Variable)
        {
            //return term;
            if(((Variable)term).isBounded())
            {
                return fixTerm(((Variable)term).getObject());
            }
            else
            {
                return ((Variable)term).getLastVariable();
            }
        }
        
        return term;
    }
    
      // unconsult non funziona sui predicati multifile
    final void unconsult(final String strFileName)
    {
        Enumeration en = m_pred2FileMap.keys();
        while(en.hasMoreElements())
        {
            String strPredName = (String)en.nextElement();
            String strFile = (String)m_pred2FileMap.get(strPredName);
            if(strFile.equals(strFileName))
            {
                if(m_clauseTable.containsKey(strPredName))
                {
                    m_clauseTable.remove(strPredName);
                    if(!isMultifile(strPredName))
                        m_pred2FileMap.remove(strPredName);
                }
            }
        }
    }
}

