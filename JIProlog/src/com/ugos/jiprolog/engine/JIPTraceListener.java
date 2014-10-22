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

/**
 * JIPTraceListener receives JIPTraceEvent objects in one of its methods depending on the type of the events
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTraceEvent
 */
public interface JIPTraceListener
{
  /** Notifies a Call event
    * @param e event occurred
    * @see com.ugos.jiprolog.engine.JIPTraceEvent
    */
    public void callNotified(JIPTraceEvent e);
    
  /** Notifies a Found event
    * @param e event occurred
    * @see com.ugos.jiprolog.engine.JIPTraceEvent
    */
    public void foundNotified(JIPTraceEvent e);
    
  /** Notifies a Bind event
    * @param e event occurred
    * @see com.ugos.jiprolog.engine.JIPTraceEvent
    */
    public void bindNotified(JIPTraceEvent e);

    /** Notifies a Fail event
      * @param e event occurred
      * @see com.ugos.jiprolog.engine.JIPTraceEvent
      */
    public void failNotified(JIPTraceEvent e);

  /** Notifies a Redo event
    * @param e event occurred
    * @see com.ugos.jiprolog.engine.JIPTraceEvent
    */
    public void redoNotified(JIPTraceEvent e);
    
    /** Notifies an Exit event
    * @param e event occurred
    * @see com.ugos.jiprolog.engine.JIPTraceEvent
    */
    public void exitNotified(JIPTraceEvent e);
    
    /** Notifies a Start event
    * @param e event occurred
    * @see com.ugos.jiprolog.engine.JIPTraceEvent
    */
    public void startNotified(JIPTraceEvent e);
    
    /** Notifies a Stop event
    * @param e event occurred
    * @see com.ugos.jiprolog.engine.JIPTraceEvent
    */
    public void stopNotified(JIPTraceEvent e);
    
}
