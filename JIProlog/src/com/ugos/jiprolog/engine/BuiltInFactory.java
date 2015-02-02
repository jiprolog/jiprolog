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

package com.ugos.jiprolog.engine;

import java.util.Hashtable;

final class BuiltInFactory extends Object
{
    static final Hashtable<String, BuiltIn> m_BuiltInTable = new Hashtable<String, BuiltIn>(100);
    private JIPEngine       m_jipEngine   = null;
    // Inizializzazione BuiltInTable
    static
    {
        // 3.0.0-0
        // debugging/tracing

        //#ifndef _MIDP
        m_BuiltInTable.put("trace/0",           new Trace0());
        m_BuiltInTable.put("notrace/0",         new Notrace0());
        m_BuiltInTable.put("spy/1",             new Spy1());
        m_BuiltInTable.put("nospy/1",           new Nospy1());
        m_BuiltInTable.put("nodebug/0",         new Nodebug0());
        m_BuiltInTable.put("debugging/0",       new Debugging0());
//      #endif

        m_BuiltInTable.put("write/1",           new Write1());
        m_BuiltInTable.put("write_canonical/1", new WriteCanonical1());
        m_BuiltInTable.put("nl/0",              new NL0());

        m_BuiltInTable.put("length/2",          new Length2());
        m_BuiltInTable.put("ground/1",          new Ground1());
        m_BuiltInTable.put("ensure_loaded/1",   new EnsureLoaded1());
        m_BuiltInTable.put("ver/1",             new Ver1());
        m_BuiltInTable.put("assert/1",          new Assert1());
        m_BuiltInTable.put("asserta/1",         new Asserta1());
        m_BuiltInTable.put("retract/1",         new Retract1());
        m_BuiltInTable.put("retractall/1",      new RetractAll1());
        m_BuiltInTable.put("abolish/1",         new Abolish1());
        m_BuiltInTable.put("consult/1",         new Consult1());
        m_BuiltInTable.put("reconsult/1",         new Consult1());
        m_BuiltInTable.put("==/2",              new TermEquals2());
        m_BuiltInTable.put("garbage_collect/0", new GarbageCollect0());
        m_BuiltInTable.put("abort/0",           new Abort0());

        //#ifndef _MIDP
        m_BuiltInTable.put("compile/1",         new Compile1());
        m_BuiltInTable.put("load_library/1",    new LoadLibrary1());
        m_BuiltInTable.put("load/1",            new Load1());
        m_BuiltInTable.put("extern/3",          new Extern3());
//      #endif

        m_BuiltInTable.put("current_op/3",      new CurrentOp3());
        m_BuiltInTable.put("current_atom/1",      new CurrentAtom1());
        m_BuiltInTable.put("current_functor/2", new CurrentFunctor2());
        m_BuiltInTable.put("compare/3",         new Compare3());
        m_BuiltInTable.put("clause/2",          new Clause2());
        m_BuiltInTable.put("unconsult/1",       new Unconsult1());
        m_BuiltInTable.put("multifile/1",       new Multifile1());
        m_BuiltInTable.put("module_transparent/1", new ModuleTransparent1());
        m_BuiltInTable.put("dynamic/1",         new Dynamic1());
        m_BuiltInTable.put("predicate_properties/2", new PredicateProperties2());
        m_BuiltInTable.put("xcall/2",           new XCall2());



        m_BuiltInTable.put("atom/1",            new Atom1());
        m_BuiltInTable.put("chars/1",           new Chars1());
        m_BuiltInTable.put("integer/1",         new Integer1());
        m_BuiltInTable.put("float/1",           new Float1());
        m_BuiltInTable.put("var/1",             new Var1());
        m_BuiltInTable.put("functor/3",         new Functor3());
//        m_BuiltInTable.put("arg/3",             new Arg3());
        m_BuiltInTable.put("=../2",             new Univ2());
        m_BuiltInTable.put("notify/2",          new Notify2());
        m_BuiltInTable.put("chdir/1",      	new SearchPath1());
        m_BuiltInTable.put("is/2",              new Is2());
        m_BuiltInTable.put(">/2",               new Great2());
        m_BuiltInTable.put("</2",               new Less2());
        m_BuiltInTable.put("=:=/2",             new Equal2());
        m_BuiltInTable.put("!/0",               new Cut0());
        m_BuiltInTable.put("!]/0",              new CloseSnip0());
        m_BuiltInTable.put("fail/0",            new Fail0());
        m_BuiltInTable.put("halt/1",            new Halt1());
        m_BuiltInTable.put("encoding/1",        new Encoding1());

        m_BuiltInTable.put("copyright/1",        new Copyright1());
        m_BuiltInTable.put("integer_bounds/2",   new IntegerBounds2());
        m_BuiltInTable.put("env/2",   			 new Env2());
        m_BuiltInTable.put("set_env/2",   		 new SetEnv2());
        m_BuiltInTable.put("ver/4",   		 	 new Ver4());
        m_BuiltInTable.put("acyclic_term/1", 		 new AcyclicTerm1());


        //m_BuiltInTable.put("bagof_with_duplicates/3", new BagOf3());
        //m_BuiltInTable.put("findall/3",         new Findall3());

        // system
        m_BuiltInTable.put("$op/3",              new Op3());
        m_BuiltInTable.put("$current_query_handle/1",      new CurrentQueryHandle1());
        m_BuiltInTable.put("$error/1",      new Error1());
        m_BuiltInTable.put("$free/1",       new Free1());
        m_BuiltInTable.put("$custom_built_in/1", new Custom1());
        m_BuiltInTable.put("$!/0", new SCut0());

        m_BuiltInTable.put("pid/1",      new Pid1());

        //m_BuiltInTable.put("wait/1",            new Wait1());
    }

    static final boolean isBuiltIn(final String strName)
    {
        return m_BuiltInTable.containsKey(strName);
    }

//    static final Vector listing()
//    {
//        final Vector vect = new Vector(m_BuiltInTable.size());
//
//        final Enumeration enum = m_BuiltInTable.keys();
//
//        while(enum.hasMoreElements())
//        {
//            vect.addElement(enum.nextElement());
//        }
//
//        return vect;
//    }

    BuiltInFactory(final JIPEngine jipEngine)
    {
        m_jipEngine        = jipEngine;
    }

    BuiltInFactory(final BuiltInFactory factory)
    {
        this(factory.m_jipEngine);
    }

    final void notifyEvent(final int nIDEvent, final PrologObject term, final int nQueryHandle)
    {
        m_jipEngine.notifyEvent(nIDEvent, term, nQueryHandle);
    }

    final JIPEngine getJIPEngine()
    {
        return m_jipEngine;
    }

    final BuiltIn getInstance(final String strName, final BuiltInPredicate pred, final WAM wam)
    {
        try
        {
            BuiltIn builtIn =
                (BuiltIn)(m_BuiltInTable.get(strName).getClass().newInstance());

            builtIn.init(m_jipEngine, pred, wam);

            return builtIn;
        }
        catch(IllegalAccessException ex)
        {
            throw new JIPJVMException(ex);
        }
        catch(InstantiationException ex)
        {
            throw new JIPJVMException(ex);
        }
    }
}
