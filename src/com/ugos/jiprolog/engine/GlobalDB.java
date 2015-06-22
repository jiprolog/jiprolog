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

import java.util.*;
import java.io.*;

import com.ugos.io.PushbackLineNumberInputStream;
import com.ugos.util.StringBuilderEx;

final class GlobalDB extends Object// implements Cloneable //Serializable
{
	private static final String[] INTERNAL_MODULES = {"jipxlist", "jipsys", "jipxdb", "jipxexception", "jipxio", "jipxreflect", "jipxsets", "jipxsystem", "jipxterms", "jipxxml"};

    // Database
    private Hashtable<String, JIPClausesDatabase> m_clauseTable;

    // associazione tra predicati e file
    private Hashtable<String, String> m_pred2FileMap;
    private Hashtable<String, String> m_moduleTransparentTbl;
    private Hashtable<String, String> m_exportedTable;
    private Hashtable<String, Vector<String>> m_file2PredMap;

    private static final String KERNEL_DEBUG = JIPEngine.RESOURCEPATH + "jipkernel.txt";
    private static final String KERNEL_RELEASE = JIPEngine.RESOURCEPATH + "jipkernel.jip";

    static final String SYSTEM_MODULE = "$system";
    static final String USER_MODULE   = "$user";
    static final String KERNEL_MODULE = "$kernel";

    public static final StringBuilderEx sbUSER_MODULE_AUX = new StringBuilderEx(USER_MODULE).append(":").setInitial();
    public static final StringBuilderEx sbUSER_MODULE = new StringBuilderEx(USER_MODULE).append(":").setInitial();
    public static final StringBuilderEx sbSYSTEM_MODULE = new StringBuilderEx(SYSTEM_MODULE).append(":").setInitial();
    public static final StringBuilderEx sbKERNEL_MODULE = new StringBuilderEx(KERNEL_MODULE).append(":").setInitial();

//    public static final StringBuilderEx defaultStringBuilder = new StringBuilderEx();

    boolean m_bCheckDisabled = false;

    private JIPEngine jipEngine;

    @SuppressWarnings("unchecked")
	private GlobalDB(GlobalDB gdb)
    {
        m_clauseTable            = (Hashtable<String, JIPClausesDatabase>)gdb.m_clauseTable.clone();
        m_pred2FileMap           = (Hashtable<String, String>)gdb.m_pred2FileMap.clone();
        m_moduleTransparentTbl   = (Hashtable<String, String>)gdb.m_moduleTransparentTbl.clone();
        m_exportedTable 		 = (Hashtable<String, String>)gdb.m_exportedTable.clone();
        m_file2PredMap			 = (Hashtable<String, Vector<String>>)gdb.m_file2PredMap.clone();

        jipEngine 				 = gdb.jipEngine;

    }

    public final GlobalDB newInstance(JIPEngine engine)
    {
    	GlobalDB gdb = new GlobalDB(this);
        gdb.jipEngine = engine;
        return gdb;
    }

    public GlobalDB(JIPEngine engine)
    {
        m_clauseTable            = new Hashtable<String, JIPClausesDatabase>();
        m_pred2FileMap           = new Hashtable<String, String>();
        m_moduleTransparentTbl   = new Hashtable<String, String>();
        m_exportedTable 		 = new Hashtable<String, String>();
        m_file2PredMap			 = new Hashtable<String, Vector<String>>();
        jipEngine				 = engine;

        loadKernel(this);
    }



    final boolean isMultifile(final String strPredName)
    {
    	String def = sbUSER_MODULE.resetToInitialValue().append(strPredName).toString();

        if(m_clauseTable.containsKey(def))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(def)).isMultifile();
        }
        else if(m_clauseTable.containsKey(strPredName))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(strPredName)).isMultifile();
        }

        return false;
    }

    final void multifile(final String strPredName)
    {
        if(isSystem(strPredName) && !m_bCheckDisabled)
        	throw new JIPPermissionException("modify", "static_procedure", Functor.getPredicateIndicator(strPredName), jipEngine);

        int nPos = strPredName.lastIndexOf('/');
        if(nPos < 0)
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, Functor.getPredicateIndicator(strPredName), jipEngine);

    	final String def = sbUSER_MODULE.resetToInitialValue().append(strPredName).toString();
        JIPClausesDatabase db;
        if(m_clauseTable.containsKey(def))
        {
            db = ((JIPClausesDatabase)m_clauseTable.get(def));
        }
        else
        {
            final String strName = strPredName.substring(0, nPos);
            final int arity = Integer.parseInt(strPredName.substring(nPos + 1));
        	if(arity == 0)
        		db = new ZeroArityDefaultClausesDatabase(strName);
        	else
        		db = new NotIndexedDefaultClausesDatabase(strName, arity, def, this);

            db.setJIPEngine(jipEngine);

            // Aggiunge il vettore alla tabella
            m_clauseTable.put(def, db);
        }

        if(db instanceof DefaultClausesDatabase)
        	db.setMultifile();
        else
        	throw new JIPPermissionException("modify", "extern_procedure", Functor.getPredicateIndicator(strPredName));

    }

    final void moduleTransparent(final String strPredName)
    {
        if(isSystem(strPredName) && !m_bCheckDisabled)
        	throw new JIPPermissionException("modify", "static_procedure", Functor.getPredicateIndicator(strPredName));
//            throw JIPRuntimeException.create(46, Atom.createAtom(strPredName));

        m_moduleTransparentTbl.put(strPredName, strPredName);

        JIPClausesDatabase db = (JIPClausesDatabase)m_clauseTable.get(sbUSER_MODULE.resetToInitialValue().append(strPredName).toString());
        if(db == null)
            db = (JIPClausesDatabase)m_clauseTable.get(sbSYSTEM_MODULE.resetToInitialValue().append(strPredName).toString());

        if(db != null)
            db.setModuleTransparent();


    }

    final boolean isModuleTransparent(final String strPredName)
    {
    	final String def = sbUSER_MODULE.resetToInitialValue().append(strPredName).toString();

        if(m_clauseTable.containsKey(def))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(def)).isModuleTransparent();
        }

        return false;
    }

    public final boolean isDynamic(final String strPredName)
    {
    	final String def = sbUSER_MODULE.resetToInitialValue().append(strPredName).toString();

        if(m_clauseTable.containsKey(def))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(def)).isDynamic();
        }

        return false;
    }

    public final boolean isUser(final String strPredName)
    {
        return m_clauseTable.containsKey(sbUSER_MODULE.resetToInitialValue().append(strPredName).toString());
    }

    final boolean isUser(final Clause clause)
    {
        return isUser((Functor)clause.getHead());
    }

    final boolean isUser(final Functor funct)
    {
        return isUser(funct.getName());
    }

    final void makeIndexed(NotIndexedDefaultClausesDatabase db)
    {
    	IndexedDefaultClausesDatabase db1 = new IndexedDefaultClausesDatabase(db);
        m_clauseTable.put(db.getFullName(), db1);
    }

    final void dynamic(final String strPredName)
    {
        int nPos = strPredName.lastIndexOf('/');
        if(nPos < 0)
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, Functor.getPredicateIndicator(strPredName), jipEngine);

        final String def = sbUSER_MODULE.resetToInitialValue().append(strPredName).toString();
//        System.out.println("strDef " + strDef);
        JIPClausesDatabase db;
        if(m_clauseTable.containsKey(def))
        {
//        	System.out.println("found " + strDef);
            db = ((JIPClausesDatabase)m_clauseTable.get(def));
        }
        else
        {
//        	System.out.println("not found " + strDef);
            final String strName = strPredName.substring(0, nPos);
            final int arity = Integer.parseInt(strPredName.substring(nPos + 1));
            // Crea un nuovo database
            if(arity == 0)
        		db = new ZeroArityDefaultClausesDatabase(strName);
        	else
        		db = new NotIndexedDefaultClausesDatabase(strName, arity, def, this);

            db.setJIPEngine(jipEngine);
            // Aggiunge il vettore alla tabella
            m_clauseTable.put(def, db);
        }

        db.setDynamic();
    }

    public final boolean isExternal(final String strPredName)
    {
    	final String def = sbUSER_MODULE.resetToInitialValue().append(strPredName).toString();

        if(m_clauseTable.containsKey(def))
        {
            return ((JIPClausesDatabase)m_clauseTable.get(def)).isExternal();
        }

        return false;
    }

    final void external(final String strPredName)
    {
        int nPos = strPredName.lastIndexOf('/');
        if(nPos < 0)
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, Functor.getPredicateIndicator(strPredName));

        final String def = sbUSER_MODULE.resetToInitialValue().append(strPredName).toString();
        JIPClausesDatabase db;
        if(m_clauseTable.containsKey(def))
        {
            db = ((JIPClausesDatabase)m_clauseTable.get(def));
        }
        else
        {
            final String strName = strPredName.substring(0, nPos);
            final int arity = Integer.parseInt(strPredName.substring(nPos + 1));
            if(arity == 0)
        		db = new ZeroArityDefaultClausesDatabase(strName);
        	else
        		db = new NotIndexedDefaultClausesDatabase(strName, arity, def, this);

            db.setJIPEngine(jipEngine);
            // Aggiunge il vettore alla tabella
            m_clauseTable.put(def, db);
        }

        db.setExternal();
    }

    public final boolean isSystem(final String strName)
    {
        return  m_clauseTable.containsKey(sbSYSTEM_MODULE.resetToInitialValue().append(strName).toString()) ||
        		m_clauseTable.containsKey(sbKERNEL_MODULE.resetToInitialValue().append(strName).toString()) ||
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
    			if(m_clauseTable.containsKey(new StringBuilder(module).append(':').append(funct).toString()))
    				return true;
    		}
    	}

		return false;
    }

    final String getFile(final String strName)
    {
    	String def;
        if(m_pred2FileMap.containsKey(strName))
            return (String)m_pred2FileMap.get(strName);
        else if(m_pred2FileMap.containsKey(def = sbUSER_MODULE.resetToInitialValue().append(strName).toString()))
            return (String)m_pred2FileMap.get(def);
        else if(m_pred2FileMap.containsKey(def = sbSYSTEM_MODULE.resetToInitialValue().append(strName).toString()))
               return (String)m_pred2FileMap.get(def);
        else if(m_pred2FileMap.containsKey(def = sbKERNEL_MODULE.resetToInitialValue().append(strName).toString()))
            return (String)m_pred2FileMap.get(def);
        else
            return null;
    }

    final void assertz(final Clause clause, String strFile, boolean dynamic)
    {
		addClause((Clause)fixTerm(clause.copy(true)), false, strFile, dynamic);
    }

    final void asserta(final Clause clause, String strFile, boolean dynamic)
    {
    	addClause((Clause)fixTerm(clause.copy(true)), true, strFile, dynamic);
    }

    final void assertzNoCopy(final Clause clause, String strFile, boolean dynamic)
    {
		addClause(clause, false, strFile, dynamic);
    }

    final Clause retract(Clause clause)
    {
//      System.out.println("retract");

        Functor functor = (Functor)clause.getHead();

        String strPredDef;

        if(functor.getAtom().equals(Atom.COLONDASH))
            strPredDef = Functor.getFunctor(functor.getParams().getHead()).getName();
		else
            strPredDef = functor.getName();

//      System.out.println("functor " + functor);  // DBG

        if(isSystem(strPredDef) || isUser(strPredDef) && !isDynamic(strPredDef))
        	throw new JIPPermissionException("modify", "static_procedure", functor.getPredicateIndicator());
//            throw JIPRuntimeException.create(13, functor);

        JIPClausesDatabase db;
        db = search(functor, clause.getModuleName());

        if(db == null)
            return null;

        Enumeration en = db.clauses(functor);

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

        if(pred == null)
        	throw new JIPInstantiationException();

        if(pred instanceof Atom)
        	throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, pred);

        if(!(pred instanceof List))
            pred = new ConsCell(pred, null);

        StringBuilderEx strModuleName = sbUSER_MODULE_AUX.resetToInitialValue();
        PrologObject head = pred;
        try
        {
            while (pred != null)
            {
                head = ((ConsCell)pred).getHead();
                if(head instanceof Variable)
                    head = ((Variable)head).getObject();

                // controlla se :/2
                if(head instanceof Functor && ((Functor)head).getAtom().equals(Atom.COLON))
                {
                    strModuleName = new StringBuilderEx(((Atom)((Functor)head).getParams().getHead()).getName()).append(":");
                    head = ((ConsCell)((Functor)head).getParams().getTail()).getHead();
                }

                String strPredDef;
                String name;
                PrologObject arity;
                // head deve essere instanza di funtore /2 del tipo name/arity
                if(head instanceof Functor && ((Functor)head).getAtom().equals(Atom.SLASHSLASH))
                {

                    ConsCell params = ((Functor )head).getParams();
                    PrologObject h = params.getHead();
                    if(h instanceof Variable)
                    	h = ((Variable)h).getObject();

                    if(h == null)
                    	throw new JIPInstantiationException(1);

                    if(!(h instanceof Atom))
                    	throw new JIPTypeException(JIPTypeException.ATOM, h);

                    name = ((Atom)h).getName();
                    PrologObject t = ((ConsCell)params.getTail());

                    if(t instanceof Variable)
                    	t = ((Variable)t).getObject();

                    if(t == null)
                    	throw new JIPInstantiationException();

                    if(!(t instanceof ConsCell))
                    {
                    	throw new JIPTypeException(JIPTypeException.INTEGER, t);
                    }

                    arity = ((ConsCell)t).getHead();

                    if(arity instanceof Variable)
                    	arity = ((Variable)arity).getObject();

                    if(arity == null)
                    	throw new JIPInstantiationException();

                    if(!(arity instanceof Expression))
                    	throw new JIPTypeException(JIPTypeException.INTEGER, arity);

                    int valArity =  (int)((Expression)arity).getValue();
                    if(valArity < 0)
                    	throw new JIPDomainException("not_less_than_zero", arity);

                    strPredDef = new StringBuilder(name).append('/').append((int)((Expression)arity).getValue()).toString();

                }
                else
                {
                    throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, head);
                }

                if(isSystem(strPredDef) || isUser(strPredDef) && !isDynamic(strPredDef))
                	throw new JIPPermissionException("modify", "static_procedure", head);

                String key = strModuleName.append(strPredDef).toString();

               	m_clauseTable.remove(key);

                pred = ((ConsCell)pred).getTail();
            }
        }
        catch(ClassCastException ex)
        {
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, head);
        }
    }

    // called by assert
    private final synchronized void addClause(final Clause clause, final boolean bFirst, final String strFile, boolean dynamic)
    {
        if(!m_bCheckDisabled && isSystem((Functor)clause.getHead()))
        	throw new JIPPermissionException("modify", "static_procedure", ((Functor)clause.getHead()).getPredicateIndicator(), jipEngine);

        // Estrae il nome del funtore
        PrologObject head = clause.getHead();

        // qui controllare se il funtore � nella export list corrente.
        // se si controllare se l'eventuale modulo specificato corrisponde a
        // quello di definizione. in tal caso la specifica di modulo va ignorata
        StringBuilder functName = new StringBuilder(clause.isExported() ? USER_MODULE : clause.getModuleName());

        if (head instanceof Functor)
        {
        	functName.append(":").append(((Functor)head).getName());
        }
        else
        {
        	JIPTypeException ex = new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, head);
        	ex.m_engine = jipEngine;
        	throw ex;
        }

        String strFunctName = functName.toString();

        // verifica se il predicato � stato gi� asserted in un altro file
        if(strFile != null)
        {
            if(m_pred2FileMap.containsKey(strFunctName))
            {
                String strFileName = (String)m_pred2FileMap.get(strFunctName);
                if(!strFileName.equals(strFile) && !isMultifile(((Functor)head).getName()))
                {
                	throw new JIPPermissionException("modify", "static_procedure", Functor.getPredicateIndicator(strFunctName), jipEngine);
                }
            }
        }

//        clause.setDynamic(dynamic);

        JIPClausesDatabase db;

        // Verifica l'esistenza del funtore
        if(m_clauseTable.containsKey(strFunctName))
        {
//            System.out.println("database: " + strFunctName);

            // Estrae il database
            db = (JIPClausesDatabase)(m_clauseTable.get(strFunctName));

            if(dynamic && !db.isDynamic())
            {
            	throw new JIPPermissionException("modify", "static_procedure", ((Functor)clause.getHead()).m_head, jipEngine);
            }

            if(bFirst)
            {
                // Aggiunge il predicato
                if(!db.addClauseAtFirst(new JIPClause(clause)))
                	throw new JIPPermissionException("modify", "static_procedure", ((Functor)clause.getHead()).m_head, jipEngine);
            }
            else
            {
                // Aggiunge il predicato
                if(!db.addClause(new JIPClause(clause)))
                	throw new JIPPermissionException("modify", "static_procedure", ((Functor)clause.getHead()).m_head, jipEngine);
//                    throw JIPRuntimeException.create(10, clause);
            }
        }
        else
        {
        	int arity = ((Functor)head).getArity();
            // Crea un nuovo database
        	if(arity == 0)
        		db = new ZeroArityDefaultClausesDatabase(((Functor)head).getFriendlyName());
        	else
        		db = new NotIndexedDefaultClausesDatabase(((Functor)head).getFriendlyName(), arity, strFunctName, this);

            db.setJIPEngine(jipEngine);

            if(dynamic)
            	db.setDynamic();

            // Aggiunge il predicato
            if(!db.addClause(new JIPClause(clause)))
            	throw new JIPPermissionException("modify", "static_procedure", ((Functor)clause.getHead()).m_head, jipEngine);

            // Aggiunge il vettore alla tabella
            m_clauseTable.put(strFunctName, db);
        }

        if(strFile != null)
        {
            m_pred2FileMap.put(strFunctName, strFile);
            if(m_file2PredMap.containsKey(strFile))
            {
            	Vector<String> preds = m_file2PredMap.get(strFile);
            	if(!preds.contains(strFunctName))
            		preds.add(strFunctName);
            }
            else
            {
              	Vector<String> preds = new Vector<String>();
           		preds.add(strFunctName);
           		m_file2PredMap.put(strFile, preds);
            }
        }

        if(m_moduleTransparentTbl.containsKey(strFunctName))
            db.setModuleTransparent();
    }

    final synchronized void addClausesDatabase(final JIPClausesDatabase db, final String strModuleName, final String strFuncName)
    {
    	db.setDynamic();

        // qui va inserita con modulo user
        m_clauseTable.put(new StringBuilder(strModuleName).append(':').append(strFuncName).toString(), db);
    }

    final synchronized JIPClausesDatabase search(final Functor funct, final Stack<String> moduleStack)
    {
    	JIPClausesDatabase db;

    	StringBuilder moduleFunName = new StringBuilder(":").append(funct.getName());
    	for(String module : moduleStack)
    	{
    		db = (JIPClausesDatabase)m_clauseTable.get(new StringBuilder(module).append(moduleFunName).toString());
    		if(db != null)
    			return db;
    	}

        db = (JIPClausesDatabase)m_clauseTable.get(sbUSER_MODULE.resetToInitialValue().append(funct.getName()).toString());//":" + funct.getName());
        if(db == null)
        {
        	db = (JIPClausesDatabase)m_clauseTable.get(sbSYSTEM_MODULE.resetToInitialValue().append(funct.getName()).toString());//":" + funct.getName());
        	if(db == null)
        	{
//        		db = (JIPClausesDatabase)m_clauseTable.get(KERNEL_MODULE + ":" + funct.getName());
//            	if(db == null)
//            	{
//            		System.out.println("funct: " + funct.getName());
//            	}
//            	System.out.println("\nmodule stack: " + moduleStack);

        	}
        }

    	return db;
    }

    // called by rulesenumeration
    final synchronized JIPClausesDatabase search(final Functor funct, final String strModule)
    {
//        System.out.println("SearchFor: " + strModule + ":" + funct.getName());  // DBG

        JIPClausesDatabase db = (JIPClausesDatabase)m_clauseTable.get(new StringBuilder(strModule).append(':').append(funct.getName()).toString());

        if(db == null)
        {
//            System.out.println("not found in " + strModule);
//            System.out.println("search in:" + USER_MODULE );  // DBG);

            db = (JIPClausesDatabase)m_clauseTable.get(sbUSER_MODULE.resetToInitialValue().append(funct.getName()).toString());
            if(db == null)
            {
//                System.out.println("not found in " + USER_MODULE);
//                System.out.println("search in:" + SYSTEM_MODULE);  // DBG);

                return (JIPClausesDatabase)m_clauseTable.get(sbSYSTEM_MODULE.resetToInitialValue().append(funct.getName()));
            }
        }

        return db;
    }

    private static final synchronized void loadKernel(GlobalDB gdb)
    {
    	try
        {
    		gdb.m_bCheckDisabled = true;

        	if(JIPDebugger.debug)
        	{
        		System.out.println("GlobalDebug");

        		InputStream ins = gdb.getClass().getResourceAsStream(KERNEL_DEBUG);

	            PrologParser parser = new PrologParser(new ParserReader(new PushbackLineNumberInputStream(ins)), new OperatorManager(), gdb.jipEngine, "jipkernel.txt");

	            PrologObject term;
	            while ((term = parser.parseNext()) != null)
	            {
	                //System.out.println(term);
	                gdb.assertzNoCopy(Clause.getClause(term, false), "__KERNEL__", false);
	            }

	            ins.close();
        	}
        	else
        	{
	             InputStream ins = gdb.getClass().getResourceAsStream(KERNEL_RELEASE);

		    	 final ObjectInputStream oins = new ObjectInputStream(ins);

		    	 PrologObject obj;
		    	 try
		    	 {
			         while((obj = (PrologObject)oins.readObject()) != null)
			    	 {
			        	 gdb.assertzNoCopy(Clause.getClause(obj, false), "__KERNEL__", false);
			    	 }
		    	 }
		    	 catch(EOFException ex)
		    	 {

		    	 }
		         oins.close();
		         ins.close();
        	}

        	// ','/2 caluse
        	Variable x = new Variable("X");
        	Variable y = new Variable("Y");

        	Functor comma = new Functor(Atom.COMMA, new ConsCell(x, new ConsCell(y, null)));
        	Functor commaWithModule = new Functor(Atom.COLON, new ConsCell(Atom.createAtom("$system"), new ConsCell(comma, null)));
        	Functor clause = new Functor(Atom.COLONDASH, new ConsCell(commaWithModule, new ConsCell(x, new ConsCell(y, null))));
        	gdb.assertzNoCopy(Clause.getClause(clause, false), "__KERNEL__", false);

	        gdb.moduleTransparent("\\+/1");
	        gdb.moduleTransparent("not/1");
	        gdb.moduleTransparent(";/2");
	        gdb.moduleTransparent("->/2");
	        gdb.moduleTransparent("^/2");

	        gdb.m_bCheckDisabled = false;
	     }
	     catch(Exception ex)
	     {
	    	 ex.printStackTrace();
	         gdb.m_bCheckDisabled = false;
	         throw new JIPRuntimeException("Unable to load Kernel: " + ex.toString());
	     }
//	     catch(JIPSyntaxErrorException ex)
//	     {
//	    	 ex.printStackTrace();
//	         gdb.m_bCheckDisabled = false;
//	         throw new JIPRuntimeException("Unable to load Kernel: " + ex.toString());
//	     }
//	     catch(NullPointerException ex)
//	     {
//	         ex.printStackTrace();
//	         gdb.m_bCheckDisabled = false;
//	         throw new JIPRuntimeException("JIProlog Kernel is not found. JIPKernel.txt should be in the classpath.");
//	     }
//		catch (ClassNotFoundException e)
//		{
//			 e.printStackTrace();
//	         gdb.m_bCheckDisabled = false;
//	         throw new JIPRuntimeException("Unable to load Kernel: " + e.toString());
//		}
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

    private final PrologObject fixTerm(final PrologObject term)
    {
//      System.out.println("fixterm");
//      System.out.println(term);

        if(term instanceof BuiltInPredicate)
        {
            return new BuiltInPredicate(((Functor)term).getAtom(), (ConsCell)fixTerm(((BuiltInPredicate)term).getParams()));
        }
        else if(term instanceof Functor)
        {
            return new Functor(((Functor)term).getAtom(), (ConsCell)fixTerm(((Functor)term).getParams()));
        }
//        else if (term instanceof PString)
//        {
//            //System.out.println("ConsCell");
//
//            return new PString(new List(fixTerm(((ConsCell)term).getHead()), fixTerm(((ConsCell)term).getTail())), this.jipEngine);
//        }
        else if (term instanceof List)
        {
            //System.out.println("ConsCell");

            if(term == List.NIL)
                return term;
            else
                return new List(fixTerm(((ConsCell)term).getHead()), fixTerm(((ConsCell)term).getTail()));
        }
        else if (term instanceof Clause)
        {
            //System.out.println("Clause");
            Clause clause = new Clause(((Clause)term).getModuleName(), (Functor)fixTerm(((ConsCell)term).getHead()), (ConsCell)fixTerm(((ConsCell)term).getTail()));
            if(((Clause)term).isExported() || isExported((Functor)clause.getHead()))
                clause.setExported();

            clause.setFileName(((Clause)term).getFileName());
            clause.setPosition(term.getLine(), term.getColumn(), term.getPosition());
//            clause.setLine(((Clause)term).getLine());
//            clause.setPosition(((Clause)term).getPosition());

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
                return ((Variable)term).lastVariable();
            }
        }

        return term;
    }

    final void unconsult(final String strFileName)
    {
    	Vector<String> predVector = m_file2PredMap.get(strFileName);
    	if(predVector == null)
    		return;

    	for(String strPredName : predVector)
        {
            if(m_clauseTable.containsKey(strPredName))
            {
            	if(isMultifile(strPredName))
            	{
            		JIPClausesDatabase db =
            				m_clauseTable.get(strPredName);

            		Enumeration en1 = db.clauses();
            		Vector<Clause> clausesToRemove = new Vector<Clause>();

            		while(en1.hasMoreElements())
            	    {
            			Clause clause = (Clause)en1.nextElement();
            			String f = clause.getFileName();
            			if(strFileName.equalsIgnoreCase(f))
            			{
            				clausesToRemove.add(clause);
            			}
            	    }

            		for(Clause c : clausesToRemove)
            		{
            			db.removeClause(new JIPClause(c));
            		}
            	}
            	else
            	{
                    m_pred2FileMap.remove(strPredName);
                    m_clauseTable.remove(strPredName);
            	}
            }
        }



//        Enumeration<String> en = m_pred2FileMap.keys();
//        while(en.hasMoreElements())
//        {
//            String strPredName = en.nextElement();
//            String strFile = m_pred2FileMap.get(strPredName);
//            if(strFile.equals(strFileName))
//            {
//                if(m_clauseTable.containsKey(strPredName))
//                {
//                	if(isMultifile(strPredName))
//                	{
//                		JIPClausesDatabase db =
//                				m_clauseTable.get(strPredName);
//
//                		Enumeration en1 = db.clauses();
//                		while(en1.hasMoreElements())
//                	    {
//                			Clause clause = (Clause)en1.nextElement();
//                			if(strFileName.equalsIgnoreCase(clause.getFileName()))
//                			{
//                				db.removeClause(new JIPClause(clause));
//                			}
//                	    }
//                	}
//                	else
//                	{
//                        m_pred2FileMap.remove(strPredName);
//                        m_clauseTable.remove(strPredName);
//                	}
//                }
//            }
//        }
    }

    final ArrayList<String> getFiles()
    {
    	ArrayList<String> files = new ArrayList<String>();

        Enumeration<String> en = m_pred2FileMap.keys();
        while(en.hasMoreElements())
        {
            String strPredName = en.nextElement();
            String strFile = m_pred2FileMap.get(strPredName);
            files.add(strFile);
        }

        return files;
    }

    void setExported(String functor)
    {
//    	String func = functor.getName();
    	m_exportedTable.put(functor, functor);
    }

    boolean isExported(Functor functor)
    {
    	String func = functor.getName();
    	return m_exportedTable.containsKey(func);
    }

//    public void exportDB(OutputStream outs) throws IOException
//    {
//    	final ObjectOutputStream out = new ObjectOutputStream(outs);
//
//    	Enumeration<String> en = m_clauseTable.keys();
//    	JIPClausesDatabase db;
//    	PrologObject term;
//    	for(String key : m_clauseTable.keySet() )
//    	{
//    		db = m_clauseTable.get(key);
//    		if(!db.isExternal())
//    		{
//    			Enumeration clausesEnum = db.clauses();
//
//    			while(clausesEnum.hasMoreElements())
//    			{
//    				term = ((JIPClause)clausesEnum.nextElement()).getTerm();
//   	                out.writeObject(term);
//    			}
//    		}
//    	}
//    }
//
//    public void importDB(InputStream ins) throws IOException, ClassNotFoundException
//    {
//    	final ObjectInputStream oins = new ObjectInputStream(ins);
//
//    	Clause clause;
//        while((clause = (Clause)oins.readObject()) != null)
//		{
//        	addClause(clause, false, clause.getFileName(), clause.isDynamic());
//		}
//    }

}

