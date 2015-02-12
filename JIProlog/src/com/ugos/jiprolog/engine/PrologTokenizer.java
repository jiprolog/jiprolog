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

import java.io.*;
import java.math.BigInteger;
import java.util.*;

import com.ugos.util.*;

class PrologTokenizer
{
    static final String UPPERCASE_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    static final String LOWERCASE_CHARS = "abcdefghijklmnopqrstuvwxyz";
    static final String SPECIAL_CHARS   = "+-*/~^<>:?@#$&=.\\";
    static final String SINGLETON_CHARS = "!\"()|{}[];,";
    static final String NUMBER_CHARS    = "0123456789";
    static final String EXPONENT_CHARS    = "+-eE";
    //static final String WHITESPACE_CHARS = "\n\r\t\b\f";
    static final String SIGN_CHARS   = "-+";

    static final char   LINECOMMENT_CHAR = '%';
    static final String OPENCOMMENT_CHAR = "/*";
    static final String CLOSECOMMENT_CHAR = "*/";
    static final char   QUOTE_CHAR = '\'';
    static final char   DOUBLEQUOTE_CHAR = '\"';

    private static final int STATE_END          = -1;
    private static final int STATE_NONE         = 0;
    private static final int STATE_ATOM         = 1;
    private static final int STATE_SPECIAL_ATOM = 2;
    private static final int STATE_NUMBER       = 3;
    private static final int STATE_EXPONENT     = 4;
    private static final int STATE_INTEGER      = 5;
    private static final int STATE_SIGN       	= 6;
    private static final int STATE_LINECOMMENT  = 7;
    private static final int STATE_COMMENT      = 8;
    private static final int STATE_QUOTE        = 9;
    private static final int STATE_DOUBLEQUOTE  = 10;
    private static final int STATE_VARIABLE     = 11;
    private static final int STATE_DECIMAL      = 12;
    private static final int STATE_ASCII        = 13;

    static final int TOKEN_UNKNOWN      = -1;
    static final int TOKEN_ATOM         = 1;
    static final int TOKEN_SPECIAL_ATOM = 2;
    static final int TOKEN_NUMBER       = 3;
    static final int TOKEN_SINGLETON    = 4;
    static final int TOKEN_SIGN         = 5;
    static final int TOKEN_VARIABLE     = 6;
    static final int TOKEN_PREDDEF      = 7;
    static final int TOKEN_DBLQUOTE     = 8;
    static final int TOKEN_QUOTE        = 9;
    static final int TOKEN_WHITESPACE   = 10;

    private Token m_nextToken;

    private ParserReader m_lnReader;
    private String m_strFileName;

    PrologTokenizer(ParserReader lnReader, String strFileName)
    {
        m_lnReader = lnReader;
        m_nextToken = null;
        m_strFileName = strFileName;
    }

    Token getNextToken() throws IOException, JIPSyntaxErrorException
    {
        // sostituisce il lex
        String strTerm = "";
        int curChar = -1;
        int nState = STATE_NONE;
        int nTokenType = TOKEN_UNKNOWN;

        if(m_nextToken != null)
        {
            Token token = m_nextToken;
            m_nextToken = null;
            return token;
        }

        while(nState != STATE_END)
        {
            //m_lnReader.mark(2);
            //System.out.println("reading");
            curChar = m_lnReader.read();

            //System.out.println("read " + (char)curChar);
            //System.out.println("read " + curChar);

            if(curChar == -1) // EOF
            {
                //System.out.println("EOF");
                nState = STATE_END;
                m_lnReader.pushback();
            }
            else
            {
                switch(nState)
                {
                    case STATE_NONE:

                        if((curChar == LINECOMMENT_CHAR))
                        {
                            nState = STATE_LINECOMMENT;
                        }
                        else if((curChar == QUOTE_CHAR))
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nState = STATE_QUOTE;
                        }
                        else if((curChar == DOUBLEQUOTE_CHAR))
                        {
                            strTerm += (char)curChar;
                            nState = STATE_DOUBLEQUOTE;
                        }
                        else if((LOWERCASE_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_ATOM;
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nTokenType = TOKEN_ATOM;
                        }
                        else if((UPPERCASE_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_VARIABLE;
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nTokenType = TOKEN_VARIABLE;
                        }
//                        else if((SIGN_CHARS.indexOf(curChar) > -1))
//                        {
//                            nState = STATE_SIGN;
//                            strTerm += (char)curChar;//String.valueOf((char)curChar);
//                            nTokenType = TOKEN_SIGN;
//                        }
                        else if((SPECIAL_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_SPECIAL_ATOM;
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nTokenType = TOKEN_SPECIAL_ATOM;
                        }
                        else if((NUMBER_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_NUMBER;
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nTokenType = TOKEN_NUMBER;
                        }
                        else if(curChar <= 0x20) // whitespace char
                            //if((WHITESPACE_CHARS.indexOf(curChar) > -1))
                        {
                            //System.out.println("TOKEN_WHITESPACE");
                            nTokenType = TOKEN_WHITESPACE;
                            nState = STATE_END;
                            strTerm = " ";
                        }
//                        else if(curChar < 0x20) // whitespace char
//                            //if((WHITESPACE_CHARS.indexOf(curChar) > -1))
//                        {
//                            nState = STATE_NONE;
//                        }
                        else if((SINGLETON_CHARS.indexOf(curChar) > -1))
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);

                            int c = m_lnReader.read();
                            if(c == -1)
                            {
                                //System.out.println("SINGLETON_CHARS EOF");

                                nState = STATE_END;
                                m_lnReader.pushback();
                            }
                            else
                            {
                                if((strTerm + (char)c).equals("!>"))
                                    strTerm += (char)c;//String.valueOf((char)c);
                                else
                                    m_lnReader.pushback();
                            }

                            nTokenType = TOKEN_SINGLETON;
                            nState = STATE_END;

////                                System.out.println("Singleton " + strTerm);
//                                nTokenType = TOKEN_SINGLETON;
////                            }

//                            nState = STATE_END;
                        }
                        else
                        {
                            throw syntaxError("invalid_character('" + (char)curChar + "')");
                        }
                        break;

                    case STATE_ATOM:
                        if((UPPERCASE_CHARS.indexOf(curChar) > -1) ||
                               (LOWERCASE_CHARS.indexOf(curChar) > -1) ||
                               (NUMBER_CHARS.indexOf(curChar) > -1))
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                        }
                        else
                        {
                            nTokenType = TOKEN_ATOM;
                            nState = STATE_END;
                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_VARIABLE:
                        if((UPPERCASE_CHARS.indexOf(curChar) > -1) ||
                               (LOWERCASE_CHARS.indexOf(curChar) > -1) ||
                               (NUMBER_CHARS.indexOf(curChar) > -1))
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                        }
                        else
                        {
                            nTokenType = TOKEN_VARIABLE;
                            nState = STATE_END;
                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_SPECIAL_ATOM:
                        if((strTerm + (char)curChar).equals("<!") || (strTerm + (char)curChar).equals("!>"))
                    	{
                        	strTerm += (char)curChar;//String.valueOf((char)c);
                        	nTokenType = TOKEN_SPECIAL_ATOM;
                            nState = STATE_END;
                    	}
                        else if((SPECIAL_CHARS.indexOf(curChar) > -1))
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            if(strTerm.equals(OPENCOMMENT_CHAR))
                            {
                                nState = STATE_COMMENT;
                                strTerm = "";
                                nTokenType = TOKEN_UNKNOWN;
                            }
                        }
                        else
                        {
                            nTokenType = TOKEN_SPECIAL_ATOM;
                            nState = STATE_END;
                            m_lnReader.pushback();
                        }
                        break;

//                    case STATE_SIGN:
//                    	if((SPECIAL_CHARS.indexOf(curChar) > -1))
//                        {
//                    		nState = STATE_SPECIAL_ATOM;
//                            strTerm += (char)curChar;//String.valueOf((char)curChar);
//                            nTokenType = TOKEN_SPECIAL_ATOM;
//                        }
//                        else
//                        {
//                            nTokenType = TOKEN_SIGN;
//                            nState = STATE_END;
//                            m_lnReader.pushback();
//                        }
//                        break;

                    case STATE_NUMBER:
                        if(NUMBER_CHARS.indexOf(curChar) > -1)
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                        }
                        else if(curChar == '.')
                        {
                            //m_lnReader.mark(2);
                            int c = m_lnReader.read();
                            m_lnReader.pushback();
                            if(NUMBER_CHARS.indexOf(c) == -1)
                            {
                                m_nextToken = new Token();

                                m_nextToken.m_strToken = ".";
                                m_nextToken.m_nType = TOKEN_SPECIAL_ATOM;//&&TOKEN_DOT;
                                nTokenType = TOKEN_NUMBER;
                                nState = STATE_END;
                            }
                            else
                            {
                                strTerm += (char)curChar;//String.valueOf((char)curChar);
                                nState = STATE_DECIMAL;
                            }
                        }
                        else if(curChar == 'e' || curChar == 'E')
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nState = STATE_EXPONENT;
                        }
                        else if(curChar == '\'')
                        {
                            if(strTerm.equals("0"))
                                nState = STATE_ASCII;
                            else
                                throw syntaxError("invalid_character('''')");// + (char)curChar + "')");
                        }
                        else
                        {
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_EXPONENT:
                        if(curChar == '+' || curChar == '-')
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nState = STATE_INTEGER;
                        }
                        else
                        {
                            throw syntaxError("invalid_character('" + (char)curChar + "')");
                        }
                        break;

                    case STATE_DECIMAL:
                        if(NUMBER_CHARS.indexOf(curChar) > -1)
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                        }
                        else if(curChar == 'e' || curChar == 'E')
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                            nState = STATE_EXPONENT;
                        }
                        else
                        {
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_INTEGER:
                        if(NUMBER_CHARS.indexOf(curChar) > -1)
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                        }
                        else
                        {
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_ASCII:
                        if(curChar == '\\')
                        {
                            strTerm = "";
                            curChar = m_lnReader.read();
                            switch(curChar)
                            {
                                case 'a':  // \a bell
                                    strTerm += Integer.toString(7);
                                    break;

                                case 'b':  // \b backspace
                                    strTerm += Integer.toString(8);
                                    break;

                                case 'f':  // \f form feed
                                    strTerm += Integer.toString(12);
                                    break;

                                case 'n':  // \n line feed
                                    strTerm += Integer.toString(10);
                                    break;

                                case 'r':  // \r carriage return
                                    strTerm += Integer.toString(13);
                                    break;

                                case 't':  // \t tab
                                    strTerm += Integer.toString(9);
                                    break;

                                case 'v':  // \v vtab
                                    strTerm += Integer.toString(11);
                                    break;

                                case 'x':  // \xHX
                                    // legge il prossimo byte
                                    int d1 = m_lnReader.read();
                                    // legge il prossimo byte
                                    int d2 = m_lnReader.read();
                                    String strHexNum = (char)d1 + "" + (char)d2;

                                    try
                                    {
                                        byte[] val = ValueEncoder.hexStringToBytes(strHexNum);
                                        strTerm += Integer.toString(val[0]);
                                    }
                                    catch(NumberFormatException ex)
                                    {
                                        throw syntaxError("bad_escape_sequence('\\x" + strHexNum + "')");
                                    }

                                    // legge il prossimo byte
                                    d2 = m_lnReader.read();
                                    if(d2 != '\\')  // ISO def
                                        m_lnReader.pushback();
                                    break;

                                default: // ignora \
                                    strTerm += Integer.toString(curChar);
                            }

                        }
                        else
                        {
                            strTerm = Integer.toString(curChar);
                        }

                        nTokenType = TOKEN_NUMBER;
                        nState = STATE_END;
                        break;

                    case STATE_LINECOMMENT:
                        int c = curChar;
                        do
                        {
                            if((c == '\r') || (c == '\n'))
                                break;

                            c = m_lnReader.read();
                        }
                        while(c != -1);

                        nState = STATE_NONE;
                        break;

                    case STATE_COMMENT:
                        {
                            c = curChar;
                            int c1;
                            do
                            {
                                c1 = c;
                                c = m_lnReader.read();
                                if((c1 == '*') && (c == '/'))
                                    break;
                            }
                            while(c != -1);

                            // torna allo stato iniziale
                            nState = STATE_NONE;
                        }
                        break;

                    case STATE_QUOTE:
                    case STATE_DOUBLEQUOTE:
//                        System.out.println(curChar);
                        if(curChar == DOUBLEQUOTE_CHAR)
                        {
                        	if(nState == STATE_DOUBLEQUOTE)
                        	{
	                            c = m_lnReader.read();
	                            if(c == DOUBLEQUOTE_CHAR)
	                            {
	                                strTerm += "\"";
	                            }
	                            else
	                            {
	                                // fine quoted atom
	                                m_lnReader.pushback();
	                                strTerm += (char)curChar;//String.valueOf((char)curChar);
	                                nTokenType = (nState == STATE_QUOTE) ? TOKEN_QUOTE : TOKEN_DBLQUOTE;
	                                nState = STATE_END;
	                            }
                        	}
                            else
                            {
                            	strTerm += (char)curChar;
                            }
                        }
                        else if(curChar == QUOTE_CHAR)
                        {
                        	if(nState == STATE_QUOTE)
                        	{
	                            c = m_lnReader.read();
	                            if(c == QUOTE_CHAR)
	                            {
	                                strTerm += "'";
	                            }
	                            else
	                            {
	                                // fine quoted atom
	                                m_lnReader.pushback();
	                                strTerm += (char)curChar;//String.valueOf((char)curChar);
	                                nTokenType = (nState == STATE_QUOTE) ? TOKEN_QUOTE : TOKEN_DBLQUOTE;
	                                nState = STATE_END;
	                            }
                        	}
                            else
                            {
                            	strTerm += (char)curChar;
                            }
                        }
                        else if(nState == STATE_QUOTE && (curChar == '\r' || curChar == '\n'))
                        {
                            throw syntaxError("carriage_return_in_quoted_atom('" + strTerm + "')");
                        }
//                        else if(curChar == '~')  // edimburgh prolog
//                        {
//                            c = m_lnReader.read();
//                            if(c == '~')
//                            {
//                                strTerm += (char)c;//String.valueOf((char)c);
//                            }
//                            else if(c == '\'')
//                            {
////                              fine quoted atom
//                                m_lnReader.pushback();
//                                //strTerm += (char)(c - '@');//String.valueOf((char)c - '@');
//                            }
//                            else if(c >= '@')
//                            {
//                                strTerm += (char)(c - '@');//String.valueOf((char)c - '@');
//                            }
//
//                            else
//                                throw syntaxError("bad_escape_sequence('~" + (char)c + "')");
//                        }
                        else if(curChar == '\\') // ISO prolog
                        {
                            c = m_lnReader.read();
                            if(c == '\\')
                            {
                                strTerm += (char)c;//String.valueOf((char)c);
                            }
                            else if(NUMBER_CHARS.indexOf(c) > -1)
                            {
                            	String strNum = "" + (char)c;
                            	// legge i prossimi numeri
                                int d1 = m_lnReader.read();
                                while(NUMBER_CHARS.indexOf(d1) > -1)
                                {
                                	strNum += "" + (char)d1;
                                	d1 = m_lnReader.read();
                                }

                                // legge il prossimo byte
                                if(d1 != '\\')  // ISO def
                                    m_lnReader.pushback();

                                try
                                {
                                	BigInteger bival = new BigInteger(strNum, 8);

                                    byte val = bival.byteValue();// Byte.parseByte(strNum);
                                    strTerm += (char)val;
                                }
                                catch(NumberFormatException ex)
                                {
                                    throw syntaxError("bad_escape_sequence('\\x" + strNum + "')");
                                }

                            }
                            else// if(c >= 'a')
                            {
                                switch(c)
                                {
                                    case 'a':  // \a bell
                                        strTerm += (char)(7);
                                        break;

                                    case 'b':  // \b backspace
                                        strTerm += (char)(8);
                                        break;

                                    case 'c':  // \c empty
                                        //strTerm += (char)(8);
                                        break;

                                    case 'f':  // \f form feed
                                        strTerm += (char)(12);
                                        break;

                                    case 'n':  // \n line feed
                                        strTerm += (char)(10);
                                        break;

                                    case 'r':  // \r carriage return
                                        strTerm += (char)(13);
                                        break;

                                    case 't':  // \t tab
                                        strTerm += (char)(9);
                                        break;

                                    case 'v':  // \v vtab
                                        strTerm += (char)(11);
                                        break;

                                    case 'x':  // \xHX
                                        // legge il prossimo byte
                                        int d1 = m_lnReader.read();
                                        // legge il prossimo byte
                                        int d2 = m_lnReader.read();
                                        String strHexNum = (char)d1 + "" + (char)d2;

                                        try
                                        {
                                            byte[] val = ValueEncoder.hexStringToBytes(strHexNum);
                                            strTerm += (char)(val[0]);
                                        }
                                        catch(NumberFormatException ex)
                                        {
                                            throw syntaxError("bad_escape_sequence('\\x" + strHexNum + "')");
                                        }

                                        // legge il prossimo byte
                                        d2 = m_lnReader.read();
                                        if(d2 != '\\')  // ISO def
                                            m_lnReader.pushback();
                                        break;

                                    default: // ignora \
                                        strTerm += (char)(c);
                                }
                            }
//                            else
//                                throw syntaxError("bad_escape_sequence('\\" + (char)c + "')");
                        }
                        else
                        {
                            strTerm += (char)curChar;//String.valueOf((char)curChar);
                        }
                        break;

//                    case STATE_DOUBLEQUOTE:
//                        if(curChar == DOUBLEQUOTE_CHAR)
//                        {
//                            strTerm += (char)curChar;
//                            nTokenType = TOKEN_DBLQUOTE;
//                            nState = STATE_END;
//                        }
//                        else if(curChar == '\r' || curChar == '\n')
//                        {
//                            throw syntaxError("carriage_return_in_string");
//                        }
//                        else if(curChar == '~')  // edimburgh prolog
//                        {
//                            c = m_lnReader.read();
//                            if(c == '~')
//                            {
//                                strTerm += (char)c;//String.valueOf((char)c);
//                            }
//                            else if(c == '"')
//                            {
//                                m_lnReader.pushback();
//                                //strTerm += (char)(c - '@');//String.valueOf((char)c - '@');
//                            }
//                            else if(c > '@')
//                            {
//                                strTerm += (char)(c - '@');//String.valueOf((char)c - '@');
//                            }
//                            else
//                                throw syntaxError("bad_escape_sequence('~" + (char)c + "')");
//                        }
//                        else if(curChar == '\\') // ISO prolog
//                        {
//                            c = m_lnReader.read();
//                            if(c == '\\')
//                            {
//                                strTerm += (char)c;//String.valueOf((char)c);
//                            }
//                            else if(c >= 'a')
//                            {
//                                switch(c)
//                                {
//                                    case 'a':  // \a bell
//                                        strTerm += (char)(7);
//                                        break;
//
//                                    case 'b':  // \b backspace
//                                        strTerm += (char)(8);
//                                        break;
//
//                                    case 'f':  // \f form feed
//                                        strTerm += (char)(12);
//                                        break;
//
//                                    case 'n':  // \n line feed
//                                        strTerm += (char)(10);
//                                        break;
//
//                                    case 'r':  // \r carriage return
//                                        strTerm += (char)(13);
//                                        break;
//
//                                    case 't':  // \t tab
//                                        strTerm += (char)(9);
//                                        break;
//
//                                    case 'v':  // \v vtab
//                                        strTerm += (char)(11);
//                                        break;
//
//                                    case 'x':  // \xHX
//                                        // legge il prossimo byte
//                                        int d1 = m_lnReader.read();
//                                        // legge il prossimo byte
//                                        int d2 = m_lnReader.read();
//                                        try
//                                        {
//                                            String strHexNum = (char)d1 + "" + (char)d2;
//
//                                            byte[] val = ValueEncoder.hexStringToBytes(strHexNum);
//                                            strTerm += (char)(val[0]);
//                                        }
//                                        catch(NumberFormatException ex)
//                                        {
//                                            throw syntaxError("bad_escape_sequence('\\x')");
//                                        }
//
//                                        // legge il prossimo byte
//                                        d2 = m_lnReader.read();
//                                        if(d2 != '\\')  // ISO def
//                                            m_lnReader.pushback();
//                                        break;
//
//                                    default: // ignora \
//                                        strTerm += (char)(c);
//                                }
//                            }
//                        }
//                        else
//                        {
//                            strTerm += (char)curChar;//String.valueOf((char)curChar);
//                        }
//                        break;
                }
            }
        }


        if(!strTerm.equals(""))
        {
            Token tok = new Token();
            tok.m_strToken = strTerm;
            tok.m_nType = nTokenType;
//            if(strTerm.equals("."))
//                tok.m_nType = TOKEN_DOT;
//            else
//                tok.m_nType = nTokenType;

            return tok;
        }
        else
        {
            return null;
        }
    }

    class Token
    {
        String m_strToken;
        int m_nType;
    }

    JIPSyntaxErrorException syntaxError(String strMsg)
    {
        return new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), strMsg);
    }
}
