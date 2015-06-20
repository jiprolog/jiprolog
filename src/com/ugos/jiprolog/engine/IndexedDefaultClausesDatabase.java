package com.ugos.jiprolog.engine;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

final class IndexedDefaultClausesDatabase extends DefaultClausesDatabase
{
    private final Vector<Clause> m_clausesVarVector;
    private final Hashtable<PrologObject, Vector<Clause>> m_clausesAtomTable;
    private final Hashtable<PrologObject, Vector<Clause>> m_clausesExpressionTable;
    private final Hashtable<Atom, Vector<Clause>> m_clausesFunctorTable;

    private final Vector<Clause> m_clausesConsVector;
    private final Vector<Clause> m_clausesListVector;

    public IndexedDefaultClausesDatabase(DefaultClausesDatabase db)
    {
    	super(db.getFunctorName(), db.getArity());

    	if(db.isDynamic())
    		setDynamic();

    	if(db.isExternal())
    		setExternal();

    	if(db.isModuleTransparent())
    		setModuleTransparent();

    	if(db.isMultifile())
    		setMultifile();

    	setJIPEngine(db.getJIPEngine());

        m_clausesVarVector = new Vector<Clause>();
        m_clausesAtomTable = new Hashtable<PrologObject, Vector<Clause>>();
        m_clausesExpressionTable = new Hashtable<PrologObject, Vector<Clause>>();
        m_clausesFunctorTable = new Hashtable<Atom, Vector<Clause>>();

        m_clausesConsVector = new Vector<Clause>();
        m_clausesListVector = new Vector<Clause>();

        for(Clause clause : db.m_clausesVector)
        {
        	addClause(new JIPClause(clause));
        }
    }

    @Override
    public void setIndex(int index)
    {
    	// TODO
    	//reindex all
//    	if(m_clausesVector.size()>0)
    }

    public final synchronized boolean addClauseAtFirst(final JIPClause clause)
    {
        m_clausesVector.add(0, (Clause)clause.getTerm());

        Functor funct = (Functor)((Clause)clause.getTerm()).getHead();
        PrologObject key = funct.getParams().getTerm(getIndex()).getRealTerm();

        if(key == null)
        {
        	// variable
        	addClauseWithVariable((Clause)clause.getTerm(), true);
        }
//	        else if(key instanceof PString)
//	        {
//	        	if(m_clausesTable.containsKey(key))
//		        {
//		        	m_clausesTable.get(key).add(0, (Clause)clause.getTerm());
//		        }
//	        	else
//	        	{
//		        	Vector<Clause> clauseVector = new Vector<Clause>();
//		        	m_clausesTable.put(key, clauseVector);
//		        	clauseVector.add((Clause)clause.getTerm());
//		        	addVariablesToVector(clauseVector);
//	        	}
//	        }
        else if(key instanceof List)
        {
        	if(key == List.NIL)
        	{
        		if(m_clausesAtomTable.containsKey(key))
        		{
        			m_clausesAtomTable.get(key).add(0, (Clause)clause.getTerm());
	        	}
            	else
    	        {
    	        	Vector<Clause> clauseVector = new Vector<Clause>();
    	        	m_clausesAtomTable.put(key, clauseVector);
    	        	clauseVector.add((Clause)clause.getTerm());
    	        	addVariablesToVector(clauseVector);
    	        }
        	}

        	m_clausesListVector.add(0,(Clause)clause.getTerm());
        }
        else if(key instanceof Functor)
        {
        	Atom atom = ((Functor)key).getAtom();

        	if(m_clausesFunctorTable.containsKey(atom))
	        {
        		m_clausesFunctorTable.get(atom).add(0, (Clause)clause.getTerm());
	        }
        	else
	        {
	        	Vector<Clause> clauseVector = new Vector<Clause>();
	        	m_clausesFunctorTable.put(atom, clauseVector);
	        	clauseVector.add((Clause)clause.getTerm());
	        	addVariablesToVector(clauseVector);
	        }
        }
        else if(key instanceof ConsCell)
        {
        	m_clausesConsVector.add(0,(Clause)clause.getTerm());
        }
        else if(key instanceof Expression)
        {
        	if(m_clausesExpressionTable.containsKey(key))
	        {
	        	m_clausesExpressionTable.get(key).add(0, (Clause)clause.getTerm());
	        }
        	else
	        {
	        	Vector<Clause> clauseVector = new Vector<Clause>();
	        	m_clausesExpressionTable.put(key, clauseVector);
	        	clauseVector.add((Clause)clause.getTerm());
	        	addVariablesToVector(clauseVector);
	        }
        }
        else if(key instanceof Atom)
        {
        	if(m_clausesAtomTable.containsKey(key))
	        {
	        	m_clausesAtomTable.get(key).add(0, (Clause)clause.getTerm());
	        }
        	else
	        {
	        	Vector<Clause> clauseVector = new Vector<Clause>();
	        	m_clausesAtomTable.put(key, clauseVector);
	        	clauseVector.add((Clause)clause.getTerm());
	        	addVariablesToVector(clauseVector);
	        }
        }
        else
        {
        	return false;
        }

        return true;
    }

    public final synchronized boolean addClause(final JIPClause clause)
    {
        m_clausesVector.add((Clause)clause.getTerm());

        Functor funct = (Functor)((Clause)clause.getTerm()).getHead();
        PrologObject key = funct.getParams().getTerm(getIndex()).getRealTerm();

        if(key == null)
        {
        	// variable
        	addClauseWithVariable((Clause)clause.getTerm(), false);
        }
//	        else if(key instanceof PString)
//	        {
//	        	if(m_clausesTable.containsKey(key))
//		        {
//		        	m_clausesTable.get(key).add((Clause)clause.getTerm());
//		        }
//	        	else
//	        	{
//		        	Vector<Clause> clauseVector = new Vector<Clause>();
//		        	m_clausesTable.put(key, clauseVector);
//		        	addVariablesToVector(clauseVector);
//		        	clauseVector.add((Clause)clause.getTerm());
//	        	}
//	        }
        else if(key instanceof List)
        {
    		if(m_clausesAtomTable.containsKey(key))
    		{
    			m_clausesAtomTable.get(key).add(0, (Clause)clause.getTerm());
        	}
        	else
	        {
	        	Vector<Clause> clauseVector = new Vector<Clause>();
	        	m_clausesAtomTable.put(key, clauseVector);
	        	addVariablesToVector(clauseVector);
	        	clauseVector.add((Clause)clause.getTerm());
	        }

        	m_clausesListVector.add((Clause)clause.getTerm());
        }
        else if(key instanceof Functor)
        {
        	Atom atom = ((Functor)key).getAtom();

        	if(m_clausesFunctorTable.containsKey(atom))
	        {
        		m_clausesFunctorTable.get(atom).add((Clause)clause.getTerm());
	        }
        	else
	        {
	        	Vector<Clause> clauseVector = new Vector<Clause>();
	        	m_clausesFunctorTable.put(atom, clauseVector);
	        	addVariablesToVector(clauseVector);
	        	clauseVector.add((Clause)clause.getTerm());
	        }
        }
        else if(key instanceof ConsCell)
        {
        	m_clausesConsVector.add((Clause)clause.getTerm());
        }
        else if(key instanceof Expression)
        {
        	if(m_clausesExpressionTable.containsKey(key))
	        {
	        	m_clausesExpressionTable.get(key).add((Clause)clause.getTerm());
	        }
        	else
	        {
	        	Vector<Clause> clauseVector = new Vector<Clause>();
	        	m_clausesExpressionTable.put(key, clauseVector);
	        	addVariablesToVector(clauseVector);
	        	clauseVector.add((Clause)clause.getTerm());
	        }
        }
        else if(key instanceof Atom)
        {
        	if(m_clausesAtomTable.containsKey(key))
	        {
	        	m_clausesAtomTable.get(key).add((Clause)clause.getTerm());
	        }
        	else
	        {
	        	Vector<Clause> clauseVector = new Vector<Clause>();
	        	m_clausesAtomTable.put(key, clauseVector);
	        	addVariablesToVector(clauseVector);
	        	clauseVector.add((Clause)clause.getTerm());
	        }
        }
        else
        {
//        	System.out.println("######");

        	return false;
        }

        return true;
    }

    private void addClauseWithVariable(Clause clause, boolean first)
    {
    	if(first)
    	{
    		m_clausesVarVector.add(0, clause);

        	for(Vector<Clause> clauseVector : m_clausesAtomTable.values())
        	{
        		clauseVector.add(0, clause);
        	}

        	for(Vector<Clause> clauseVector : m_clausesFunctorTable.values())
        	{
        		clauseVector.add(0, clause);
        	}

        	for(Vector<Clause> clauseVector : m_clausesExpressionTable.values())
        	{
        		clauseVector.add(0, clause);
        	}

        	m_clausesListVector.add(0,clause);
        	m_clausesConsVector.add(0,clause);
    	}
    	else
    	{
        	m_clausesVarVector.add(clause);

	    	for(Vector<Clause> clauseVector : m_clausesAtomTable.values())
	    	{
	    		clauseVector.add(clause);
	    	}

	    	for(Vector<Clause> clauseVector : m_clausesExpressionTable.values())
        	{
        		clauseVector.add(clause);
        	}

	    	for(Vector<Clause> clauseVector : m_clausesFunctorTable.values())
        	{
        		clauseVector.add(clause);
        	}

        	m_clausesListVector.add(clause);
        	m_clausesConsVector.add(clause);
    	}
    }

    private void addVariablesToVector(Vector<Clause> clauseVector)
    {
    	for(Clause clause : m_clausesVarVector)
    	{
    		clauseVector.add(clause);
    	}
    }

    private void removeClauseWithVariable(Clause clause)
    {
    	m_clausesVarVector.removeElement(clause);

    	for(Vector<Clause> clauseVector : m_clausesAtomTable.values())
    	{
    		clauseVector.removeElement(clause);
    	}

    	for(Vector<Clause> clauseVector : m_clausesFunctorTable.values())
    	{
    		clauseVector.removeElement(clause);
    	}

    	for(Vector<Clause> clauseVector : m_clausesExpressionTable.values())
    	{
    		clauseVector.removeElement(clause);
    	}

    	m_clausesListVector.removeElement(clause);
    	m_clausesConsVector.removeElement(clause);
    }

    public final synchronized boolean removeClause(final JIPClause clause)
    {
    	boolean removed = m_clausesVector.removeElement(clause.getTerm());

        Functor funct = (Functor)((Clause)clause.getTerm()).getHead();
        PrologObject key = funct.getParams().getTerm(getIndex()).getRealTerm();

        if(key == null)
        {
        	// variable
        	removeClauseWithVariable((Clause)clause.getTerm());
        }
//	    else if(key instanceof PString)
//	    {
//	    }
        else if(key instanceof List)
        {
        	if(key == List.NIL && m_clausesAtomTable.containsKey(key))
        		removed = m_clausesAtomTable.get(key).removeElement(clause.getTerm());

        	removed = m_clausesListVector.removeElement(clause.getTerm());
        }
        else if(key instanceof Functor)
        {
        	Atom atom = ((Functor)key).getAtom();

        	if(m_clausesFunctorTable.containsKey(atom))
        		removed = m_clausesFunctorTable.get(atom).removeElement((Clause)clause.getTerm());
        }
        else if(key instanceof ConsCell)
        {
        	removed = m_clausesConsVector.removeElement(clause.getTerm());
        }
        else if(key instanceof Expression)
        {
        	if(m_clausesExpressionTable.containsKey(key))
        		removed = m_clausesExpressionTable.get(key).removeElement((Clause)clause.getTerm());
        }
        else if(key instanceof Atom)
        {
        	if(m_clausesAtomTable.containsKey(key))
        		removed = m_clausesAtomTable.get(key).removeElement((Clause)clause.getTerm());
        }
        else
        {
        	return false;
        }

        return removed;
    }

    public final synchronized Enumeration clauses(JIPFunctor functor)
    {
    	Vector<Clause> clausesVector = null;

        Functor funct = (Functor)(functor.getTerm());
        PrologObject key = funct.getParams().getTerm(getIndex()).getRealTerm();

        if(key == null)
        {
        	clausesVector = m_clausesVector;
        }
//	        else if(key instanceof PString)
//	        {
//		        if(m_clausesTable.containsKey(key))
//		        {
//		        	clausesVector = m_clausesTable.get(key);
//		        }
//		        else
//		        {
//		        	clausesVector = m_clausesVarVector;
//		        }
//	        }
        else if(key instanceof List)
        {
        	clausesVector = m_clausesListVector;
        }
        else if(key instanceof Functor)
        {
        	Atom atom = ((Functor)key).getAtom();
        	if(m_clausesFunctorTable.containsKey(atom))
        	{
        		clausesVector = m_clausesFunctorTable.get(atom);
        	}
        	else
        	{
	        	clausesVector = m_clausesVarVector;
        	}
        }
        else if(key instanceof ConsCell)
        {
        	clausesVector = m_clausesConsVector;
        }
        else if(key instanceof Expression)
        {
        	if(m_clausesExpressionTable.containsKey(key))
        	{
        		clausesVector = m_clausesExpressionTable.get(key);
        	}
        	else
        	{
	        	clausesVector = m_clausesVarVector;
        	}
        }
        else if(key instanceof Atom)
        {
	        if(m_clausesAtomTable.containsKey(key))
        	{
        		clausesVector = m_clausesAtomTable.get(key);
        	}
        	else
        	{
	        	clausesVector = m_clausesVarVector;
        	}
        }
        else
        {
        	clausesVector = m_clausesVarVector;
        }

//    	System.out.println(clausesVector);

    	if(!isDynamic() || getJIPEngine().isImmediateUpdateSemantics())
    		return clausesVector.elements();
    	else
        	return ((Vector<Clause>)clausesVector.clone()).elements();
    }
}