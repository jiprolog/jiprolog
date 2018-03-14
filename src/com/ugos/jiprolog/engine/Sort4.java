package com.ugos.jiprolog.engine;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Stack;
import java.util.Vector;

final class Sort4 extends BuiltIn
{
	private static final Vector<Atom> opVect;
	
	static 
	{
		opVect = new Vector<Atom>();
		
		opVect.add(Atom.createAtom("<"));
		opVect.add(Atom.createAtom("=<"));
		opVect.add(Atom.createAtom(">"));
		opVect.add(Atom.createAtom(">="));
		opVect.add(Atom.createAtom("@<"));
		opVect.add(Atom.createAtom("@=<"));
		opVect.add(Atom.createAtom("@>"));
		opVect.add(Atom.createAtom("@>="));
	}
	
    public final boolean unify(final Hashtable varsTbl)
    {
        final PrologObject keyIndex  = getRealTerm(getParam(1));
        PrologObject operator = getRealTerm(getParam(2));
        PrologObject input  = getRealTerm(getParam(3));
        PrologObject output  = getParam(4);

        if(keyIndex == null)
            throw new JIPInstantiationException(1);

        if(!(keyIndex instanceof Expression) || !((Expression)keyIndex).isInteger())        
        	throw new JIPTypeException(JIPTypeException.INTEGER, keyIndex);
        
        final int ki = (int)((Expression)keyIndex).getValue();
        
        if(operator == null)
            throw new JIPInstantiationException(2);

        if(!(operator instanceof Atom))        
        	throw new JIPTypeException(JIPTypeException.ATOM, operator);
    
        if(!opVect.contains(operator))        
        	throw new JIPTypeException(JIPTypeException.ATOM, operator);
    
        final int opVal = opVect.indexOf(operator);
        
        if(input == null)
            throw new JIPInstantiationException(3);
        
		if(!(input instanceof List))
			throw new JIPTypeException(JIPTypeException.LIST, input);
		
		if((((ConsCell)input).isPartial()))
			throw new JIPInstantiationException(1);

		if(!(((ConsCell)input).isClosedOrPartial()))
			throw new JIPTypeException(JIPTypeException.LIST, input);

		PrologObject o = getRealTerm(output); 
		if(o != null)
		{
			if(!(o instanceof List))
				throw new JIPTypeException(JIPTypeException.LIST, output);
		
			if((((ConsCell)o).isPartial()))
				throw new JIPInstantiationException(2);
			
			if(!(((ConsCell)o).isClosedOrPartial()))
					throw new JIPTypeException(JIPTypeException.LIST, output);		
		}
		
		java.util.List<PrologObject> terms = ((List)input).getTerms();
		
		Collections.sort(terms, new Comparator<PrologObject>() {

			public int compare(PrologObject o1, PrologObject o2) {

				if(ki > 0)
				{
					if(o1 instanceof Functor)
					{
						o1 = ((Functor)o1).getTerm(ki + 1);
					}
					else if(o1 instanceof ConsCell)
					{
						o1 = ((ConsCell)o1).getTerm(ki);
					}
					else
					{
						throw new JIPTypeException(JIPTypeException.COMPOUND, o1);
					}
					
					if(o2 instanceof Functor)
					{
						o2 = ((Functor)o2).getTerm(ki + 1);
					}
					else if(o1 instanceof ConsCell)
					{
						o2 = ((ConsCell)o2).getTerm(ki);
					}
					else
					{
						throw new JIPTypeException(JIPTypeException.COMPOUND, o2);
					}
				}
				
				switch(opVal)
				{
					case 0:
					case 1:
					case 4:
					case 5:
						return o1.lessThen(o2) ? -1 : o1.termEquals(o2) ? 0 : 1;
						
					default:
						return o1.lessThen(o2) ? 1 : o1.termEquals(o2) ? 0 : -1;						
				}
				
				
			}
		});

		for(int i = 0; i < terms.size(); i++)
		{
			PrologObject term = terms.get(i);
			
			int j = i + 1;
			while(j < terms.size() && term.termEquals(terms.get(j)))
				terms.remove(j);
		}
		
		List sortedList = List.create(terms);
		
        return sortedList.unify(output, varsTbl);
    }

    public final boolean isDeterministic()
    {
        return true;
    }
}
