package com.ugos.jiprolog.engine;

import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;

final class MSort2 extends BuiltIn
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
		
		Collections.sort(terms, new Comparator<PrologObject>() {

			public int compare(PrologObject o1, PrologObject o2) {
				
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
