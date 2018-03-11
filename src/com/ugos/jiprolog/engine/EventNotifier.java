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

import java.util.Vector;

class EventNotifier extends Object implements Runnable
{
    private final Vector           m_EventListenerVect = new Vector(5);;
    private final Vector           m_TraceListenerVect = new Vector(1);
    private final Vector           m_eventsVect = new Vector();;
    private JIPEngine              m_jipEngine;

    private Thread m_workerThread;

    private boolean m_bAvailable;
//    private boolean m_bEnabled;

    EventNotifier(final JIPEngine jipEngine)
    {
        m_jipEngine = jipEngine;

//        m_workerThread = new Thread(this);
//        m_workerThread.setDaemon(true);
//        m_workerThread.start();
    }

    //#ifndef _MIDP
    protected final void finalize() throws Throwable
    {
        //System.out.println("Finalize");
        m_workerThread = null;
    }
    //#endif

    public final synchronized void addEventListener(final JIPEventListener listener)
    {
        if(!m_EventListenerVect.contains(listener))
            m_EventListenerVect.insertElementAt(listener, 0);
    }

    public final synchronized void removeEventListener(final JIPEventListener listener)
    {
        if(!m_EventListenerVect.contains(listener))
            m_EventListenerVect.removeElement(listener);
    }

    public final synchronized Vector getEventListeners()
    {
        return m_EventListenerVect;
    }

//  #ifndef _MIDP
    public final synchronized void addTraceListener(final JIPTraceListener listener)
    {
        if(!m_TraceListenerVect.contains(listener))
            m_TraceListenerVect.insertElementAt(listener, 0);
    }

    public final synchronized void removeTraceListener(final JIPTraceListener listener)
    {
        if(m_TraceListenerVect.contains(listener))
            m_TraceListenerVect.removeElement(listener);
    }

    public final synchronized Vector getTraceListeners()
    {
        return m_TraceListenerVect;
    }

    //#endif

    public final synchronized void setEnabled(final boolean bEnabled)
    {
        if(bEnabled && m_workerThread == null)
        {
            m_workerThread = new Thread(this);
            m_workerThread.setDaemon(true);
            m_workerThread.setName("JIProlog event notifier");
            m_workerThread.start();
        }
        else if(!bEnabled && m_workerThread != null)
        {
            notify();
            //m_workerThread.stop();
            m_workerThread = null;
        }

        //m_bEnabled = bEnabled;
    }

    public final JIPEvent notifyEvent(final int nID, final PrologObject term, final int queryHandle)
    {
        final JIPEvent e = new JIPEvent(nID, term, m_jipEngine, queryHandle);
//        System.out.println("Notify Event ");

        notify(e);

//        System.out.println("Notify Event 2");

        return e;

    }

//  #ifndef _MIDP
    public final JIPTraceEvent notifyTraceEvent(final int nID, final PrologObject term, int nQueryHandle,  final WAMTrace wam, final int nLevel)
    {
        final JIPTraceEvent e = new JIPTraceEvent(nID, term, m_jipEngine, nQueryHandle, wam, nLevel);
        notify(e);
        return e;

    }
    //#endif

    public final JIPErrorEvent notifyErrorEvent(final int queryHandle, final JIPRuntimeException err)
    {
        final JIPErrorEvent e = new JIPErrorEvent(m_jipEngine, queryHandle, err);
        notify(e);
        return e;
    }

    private final synchronized void notify(final JIPEvent e)
    {
//        System.out.println("notify " + e.getID());
//        System.out.println("notify " + e.getTerm());
        m_eventsVect.addElement(e);
//        System.out.println("notify 2");
        if(!m_bAvailable)
        {
            m_bAvailable = true;
            notify();
//            System.out.println("notify 3");
        }

//        System.out.println("notify 4");
    }

    private final synchronized JIPEvent getEvent()
    {
        if(!m_bAvailable)
        {
            try
            {
                wait();
            }
            catch(InterruptedException ex)
            {
                throw new JIPJVMException(ex);
                //ex.printStackTrace();
            }
        }

        if(m_eventsVect.size() > 0)
        {
	        final JIPEvent e = (JIPEvent)m_eventsVect.elementAt(0);
	        m_eventsVect.removeElementAt(0);

	        m_bAvailable = m_eventsVect.size() > 0;

	        return e;
        }
        else
        {
            return null;
        }
    }

    public final void run()
    {
        while(m_workerThread != null)
        {
            final JIPEvent e = getEvent();

            if(e == null)
            {
//                System.out.println("Thead ends");
                return; // thread ends
            }

//            if(m_bEnabled)
//            {
                switch(e.getID())
                {
                case JIPEvent.ID_OPEN:
                    {
                        JIPEventListener listener;
                        for(int i = 0; i < m_EventListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPEventListener)m_EventListenerVect.elementAt(i);
                            listener.openNotified(e);
                        }
                    }
                    break;

                case JIPEvent.ID_CLOSE:
                    {
                        JIPEventListener listener;
                        for(int i = 0; i < m_EventListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPEventListener)m_EventListenerVect.elementAt(i);
                            listener.closeNotified(e);
                        }
                    }
                    break;


                case JIPEvent.ID_END:
                    {
                        JIPEventListener listener;
                        for(int i = 0; i < m_EventListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPEventListener)m_EventListenerVect.elementAt(i);
                            listener.endNotified(e);
                        }
                    }
                    break;

                case JIPEvent.ID_SOLUTION:
                    {
                        JIPEventListener listener;
                        for(int i = 0; i < m_EventListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPEventListener)m_EventListenerVect.elementAt(i);
                            listener.solutionNotified(e);
                        }
                    }
                    break;

                case JIPEvent.ID_MORE:
                    {
                        JIPEventListener listener;
                        for(int i = 0; i < m_EventListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPEventListener)m_EventListenerVect.elementAt(i);
                            listener.moreNotified(e);
                        }
                    }
                        break;

                case JIPErrorEvent.ID_ERROR:
                    {
                        JIPEventListener listener;
                        for(int i = 0; i < m_EventListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPEventListener)m_EventListenerVect.elementAt(i);
                            listener.errorNotified((JIPErrorEvent)e);
                        }
                    }
                    break;
//              #ifndef _MIDP
                case JIPTraceEvent.ID_CALL:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.callNotified((JIPTraceEvent)e);
                        }
                    }
                    break;

                case JIPTraceEvent.ID_REDO:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.redoNotified((JIPTraceEvent)e);
                        }
                    }
                    break;

                case JIPTraceEvent.ID_FOUND:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.foundNotified((JIPTraceEvent)e);
                        }
                    }
                    break;

                case JIPTraceEvent.ID_BIND:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.bindNotified((JIPTraceEvent)e);
                        }
                    }
                    break;

                case JIPTraceEvent.ID_FAIL:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.failNotified((JIPTraceEvent)e);
                        }
                    }
                    break;

                    case JIPTraceEvent.ID_EXIT:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.exitNotified((JIPTraceEvent)e);
                        }
                    }
                    break;

                    case JIPTraceEvent.ID_START:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.startNotified((JIPTraceEvent)e);
                        }
                    }
                    break;

                    case JIPTraceEvent.ID_STOP:
                    {
                        JIPTraceListener listener;
                        for(int i = 0; i < m_TraceListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPTraceListener)m_TraceListenerVect.elementAt(i);
                            listener.stopNotified((JIPTraceEvent)e);
                        }
                    }
                    break;
                //#endif

                default:
                    {
                        JIPEventListener listener;
                        for(int i = 0; i < m_EventListenerVect.size() && !e.consumed(); i++)
                        {
                            listener = (JIPEventListener)m_EventListenerVect.elementAt(i);
                            listener.termNotified(e);
                        }
                    }
                    break;
                }
//            }
        }

//        System.out.println("Thread notifier OUT");
    }
 }
