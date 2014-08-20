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

package com.ugos.JIProlog;

import java.awt.Frame;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Properties;

import com.ugos.JIProlog.engine.JIPEngine;
import com.ugos.JIProlog.engine.JIPQuery;
import com.ugos.JIProlog.engine.JIPRuntimeException;
import com.ugos.JIProlog.engine.JIPSyntaxErrorException;
import com.ugos.JIProlog.engine.JIPTerm;


/**
 * JIProlog implements the stand alone prolog interpreter.<br>
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 */
public class JIProlog
{
    private static final String VERSION = "2.2";
    
    /** Entry point of the stand alone prolog interpreter
     * @param args the first element of the array must be the file to consult.<br>
     * Note that such a file must define main/0 predicate.
     */
    public static void main(String args[])
    {
        System.out.println("*************************************************");
        System.out.println("** JIProlog Standalone Interpreter v" + VERSION);
        System.out.println("** Based on JIProlog v" + JIPEngine.getVersion());
        System.out.println("** Copyright (C) 1999-2007 By Ugo Chirico. All rights reserved");
        System.out.println("** http://www.ugochirico.com");
        System.out.println("*************************************************");
            
        if(args.length < 1)
        {
            showMessage("JIProlog Standalone Interpreter requires a Prolog file to run");
            System.exit(0);
        }
        
        // New instance of prolog engine
        final JIPEngine jip = new JIPEngine();
        
        System.out.println(jip.getSearchPath());
        
        // load autoload file
        String strKey;
        String strValue = null;
        try
        {
            InputStream ins;    
            ins = new FileInputStream("jipautoload.ini");
            
            Properties props = new Properties();
            props.load(ins);
            Enumeration en = props.keys();
            while(en.hasMoreElements())
            {
                strKey = (String)en.nextElement();
                strValue = (String)props.get(strKey);
                try
                {
                    if(strKey.startsWith("plfile"))
                    {
                        jip.consultFile(strValue);
                        System.out.println(strValue + " consulted");
                    }
                    else if(strKey.startsWith("cplfile"))
                    {
                        jip.loadFile(strValue);
                        System.out.println(strValue + " loaded");
                    }
                    else if(strKey.startsWith("xlibrary"))
                    {
                        jip.loadLibrary(strValue);
                        System.out.println(strValue + " xloaded");
                    }
                }
                catch(IOException ex)
                {
                    System.out.println("WARNING, " + (strValue != null ? strValue : "") + " in autoload not completed: " + ex.toString() );
                }
                catch(JIPSyntaxErrorException ex)
                {
                    System.out.println("WARNING, autoload not completed: " + ex.toString() + " in " + ex.getFileName());
                }
                catch(JIPRuntimeException ex)
                {
                    System.out.println("WARNING, " + (strValue != null ? strValue : "") + " in autoload not completed: " + ex.toString() );
                    //ex.printStackTrace();
                }
            }
            
            ins.close();
        }
        catch(FileNotFoundException ex)
        {
            System.out.println("WARNING, jipautoload.ini not found");
        }
        catch(IOException ex)
        {
            System.out.println("WARNING, autoload not completed: " + ex.toString() );
        }

        try
        {
            jip.consultFile(args[0]);
            
            JIPQuery query = jip.openSynchronousQuery("?-main.");
            JIPTerm term = query.nextSolution();
            if(term == null)
            {
                showMessage("The predicate main/0 wasn't found in the Prolog file specified");
                System.exit(0);
            }
            
            System.out.println("***************************************************");
            System.out.println("** Thanks for using JIProlog                     **");
            System.out.println("***************************************************");
        }
        catch(IOException ex)
        {
            showMessage(ex.getMessage());
        }
        catch(JIPSyntaxErrorException ex)
        {
            showMessage(ex.getMessage());
        }
        catch(JIPRuntimeException ex)
        {
            showMessage(ex.getMessage());
        }
                        
        System.exit(0);
    }
    
//    private static void showMessage(String strMsg)
//    {
//        System.out.println("\n" + strMsg);
//        System.out.println("Press any key");
//        try
//        {
//            System.in.read();
//        }
//        catch(IOException ex)
//        {
//        }
//    }

    private static void showMessage(String strMsg)
    {
        //TextMessageDialog dialog = new TextMessageDialog(new Frame(), "JIProlog Standalone Interpreter", strMsg);
//        dialog.add(new Label(strMsg, Label.CENTER));
        //dialog.setSize(400, 80);

//        // Centra la dialog
//        Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
//
//        int nChar = strMsg.length();
//        int nWidth = nChar * TextMessageDialog.CHAR_WEIGHT;
//
//        int nTop  = (d.height - 80) / 2;
//        int nLeft = (d.width  - 400)  / 2;
//
//        dialog.setBounds(nLeft, nTop, nWidth, 80);
       // dialog.show();
    }
}

