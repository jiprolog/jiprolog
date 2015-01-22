package com.ugos.util;
    
import java.util.Enumeration;
import java.util.Vector;


/**
 * SortedCollection <p> This class is free
 * @author Ugo Chirico 1998 <p>
 * @version 1.0.0
 * Home Page: http:\\www.geocities.com\ResearchTriangle\3219
 * Implements a Sorted Collection of generic Objects.
 * You can access directly to the objects using a Key as in an Hashtable or using the methods
 * elements or keys that return a sorted enumeration of objects or keys respectively
 */
public class SortedVector extends Vector
{
    //private int m_nOrder = 0;
    private final Order m_order;

    /** Constructor
     * Construct a sorted collection
     * @parm order
     */
    public SortedVector(final Order order)
    {
        if (order == null)
            throw new IllegalArgumentException("order not defined");

        m_order = order;
    }
    
    /** Constructor
     * Construct a sorted collection given an hashtable
     * @parm order
     */
    public SortedVector(final Order order, final Vector vect)
    {
        if (order == null)
            throw new IllegalArgumentException("order not defined");

        m_order = order;
        
        Object val;
        
        Enumeration e = vect.elements();
        while(e.hasMoreElements())
        {
            val = e.nextElement();
            addElement(val);
        }
    }
    
   /** Add an object in the predefined order 
    * @parm obj to put in
    */
    public synchronized void addElement(Object obj)
    {
        // search for previous element
        boolean bFound = false;
        int i = 0;
        while(!bFound && i < size())
        {
            bFound = m_order.lessThen(obj, elementAt(i));
            i++;
        }
        
        if(bFound)
        {
            insertElementAt(obj, i - 1);
        }
        else
        {
            super.addElement(obj);
        }
    }
}
