package com.ugos.jiprolog.engine;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Stack;
import java.util.Vector;

final class KeySort2 extends BuiltIn
{
    public final boolean unify(final Hashtable varsTbl)
    {
        PrologObject input  = getRealTerm(getParam(1));
        PrologObject output  = getParam(2);
        
        if(input == null)
            throw new JIPInstantiationException(1);
        
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
		
		if(terms.size() == 1)
		{
			PrologObject o1 = getRealTerm(terms.get(0));
			if(o1 instanceof Functor)
			{
				if(!((Functor)o1).getName().equals("-/2"))
					throw new JIPTypeException(JIPTypeException.PAIR, o1);	
				
				o1 = ((Functor)o1).getTerm(2);
			}
			else if(o1 == null)
			{
				throw new JIPInstantiationException();
			}
			else
			{
				throw new JIPTypeException(JIPTypeException.COMPOUND, o1);
			}
		} 
		
		Collections.sort(terms, new Comparator<PrologObject>() {

			public int compare(PrologObject o1, PrologObject o2) {
				
				o1 = getRealTerm(o1);
				
				if(o1 instanceof Functor)
				{
					if(!((Functor)o1).getName().equals("-/2"))
						throw new JIPTypeException(JIPTypeException.PAIR, o1);	
					
					o1 = ((Functor)o1).getTerm(2);
				}
				else if(o1 == null)
				{
					throw new JIPInstantiationException();
				}
				else
				{
					throw new JIPTypeException(JIPTypeException.COMPOUND, o1);
				}
				
				o2 = getRealTerm(o2);
				if(o2 instanceof Functor)
				{
					if(!((Functor)o2).getName().equals("-/2"))
						throw new JIPTypeException(JIPTypeException.PAIR, o2);
					
					o2 = ((Functor)o2).getTerm(2);
				}
				else if(o2 == null)
				{
					throw new JIPInstantiationException();
				}
				else
				{
					throw new JIPTypeException(JIPTypeException.COMPOUND, o2);
				}
				
				return o1.lessThen(o2) ? -1 : o1.termEquals(o2) ? 0 : 1;
				
			}
		});
		
		List sortedList = List.create(terms);
		
        return sortedList.unify(output, varsTbl);
    }

    public final boolean isDeterministic()
    {
        return true;
    }
}
