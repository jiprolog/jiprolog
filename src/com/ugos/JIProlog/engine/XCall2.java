/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico
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

//import com.ugos.debug.*;
import java.util.*;

final class XCall2 extends BuiltIn
{
    private JIPXCall      m_exObj;
    
    // Called by prolog engine when it tries to unify the goal
    // (in this case the goal is a call to a built in predicate)
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        if (m_exObj == null)  // Called for the first time
        {
            //#ifndef _MIDP
            // se prima volta mostra finestra di shareware
//            if("Personal".equals(JIPEngine.getLicenseType()))
//            {
//                qwerty();
//            }
            //#endif
            
            // Get JIPXCall class Name (first parameter)
            final PrologObject exClass = getRealTerm(getParam(1));
            
            // extract the Atom related to the class name
            String strXClassName;
            if(exClass instanceof PString)
                strXClassName = ((PString)exClass).getString();
            else if(exClass instanceof Atom)
                strXClassName = ((Atom)exClass).getName();
            else
                throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM_OR_STRING);
                      
            // Create an instance of JIPXCall class
            m_exObj = createXCall(strXClassName);
            
            // Set current JIPEngine instance
            m_exObj.init(this);
        }
        
        final PrologObject params = getRealTerm(getParam(2));
            if(!(params instanceof List))
                throw new JIPParameterTypeException(2, JIPParameterTypeException.LIST);
            
        JIPCons exParams = new JIPCons(((List)params).getConsCell());
            
        Hashtable<JIPVariable, JIPVariable> jipVarsTable = new Hashtable<JIPVariable, JIPVariable>();
        // Invoke JIPXCall class
        boolean unify = m_exObj.unify(exParams, jipVarsTable);
        
        if(unify)
        {
        	Variable var;
        	for(JIPVariable jvar : jipVarsTable.values())
        	{
        		var = (Variable)jvar.getTerm();
        		varsTbl.put(var, var);
        	}
        }
        
        return unify;
    }
    
    
    // return true if the JIPXCall class is deterministic
    public final boolean hasMoreChoicePoints()
    {
        return m_exObj == null ? true : m_exObj.hasMoreChoicePoints();
    }
    
    // Create an instance of JIPXCall class
    protected static final JIPXCall createXCall(String strXClassName)
    {
        try
        {
            //System.out.println(strXClassName);
            // Get the correct class name
            if(strXClassName.charAt(0) == 39 || strXClassName.charAt(0) == 34)
            {
                strXClassName = strXClassName.substring(1, strXClassName.length() - 1);
            }
            
           JIPXCall exObj;
//            Debug.traceln(JIPEngine.getClassLoader(), 1);
//            Debug.traceln(JIPEngine.getClassLoader().getClass(), 1);
            //////////
//            Object obj = (JIPEngine.getClassLoader().loadClass(strXClassName)).newInstance();
//            Debug.traceln(obj, 1);
//            Debug.traceln(obj.getClass(), 1);
//            Debug.traceln(obj.getClass().getClassLoader(),1);
//            Debug.traceln(Class.forName("com.ugos.JIProlog.engine.JIPXCall").getClassLoader(),1);
//            Debug.traceln("Obj: " + obj.getClass(), 1);
//            Debug.traceln("JIPXCall: " + Class.forName("com.ugos.JIProlog.engine.JIPXCall"),1);
//            Debug.traceln("Obj: " + obj.getClass().getPackage(), 1);
//            Debug.traceln("Obj: " + obj.getClass().getSuperclass(), 1);
//            Debug.traceln("Obj: " + obj.getClass().getSuperclass().getPackage(), 1);
//            Debug.traceln("JIPXCall: " + Class.forName("com.ugos.JIProlog.engine.JIPXCall").getPackage(),1);
//            Debug.traceln("JIPXCall: " + Class.forName("com.ugos.JIProlog.engine.JIPXCall").getSuperclass(),1);
//            Debug.traceln("JIPXCall: " + Class.forName("com.ugos.JIProlog.engine.JIPXCall").getSuperclass().getPackage(),1);
//            Debug.traceln("Test: " + Class.forName("com.ugos.JIProlog.engine.JIPXCall").isAssignableFrom(obj.getClass()),1);
//
            ///////////////
           //#ifndef _MIDP
            if(JIPEngine.getClassLoader() != null)
                exObj = (JIPXCall)(JIPEngine.getClassLoader().loadClass(strXClassName)).newInstance();  //obj;
            else
            //#endif
                exObj = (JIPXCall)Class.forName(strXClassName).newInstance();
            
            return exObj;
        }
        catch(ClassNotFoundException ex)
        {
            //ex.printStackTrace();
            throw JIPRuntimeException.create(41, strXClassName);
        }
        //#ifndef _MIDP
        catch(NoClassDefFoundError ex)
        {
            throw JIPRuntimeException.create(41, strXClassName);
        }
        //#endif
        catch(IllegalAccessException ex)
        {
            throw JIPRuntimeException.create(42, strXClassName);
        }
        catch(InstantiationException ex)
        {
            throw JIPRuntimeException.create(43, strXClassName);
        }
        catch(ClassCastException ex)
        {
//            ex.printStackTrace();  // Debug
            throw JIPRuntimeException.create(44, strXClassName);
        }
    }
}
