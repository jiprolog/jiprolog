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
    static final String EXPONENT_CHARS  = "+-eE";
    //static final String WHITESPACE_CHARS = "\n\r\t\b\f";
    static final String SIGN_CHARS      = "-+";
    static final String NUMBER_BINARY_CHARS      = "01";
    static final String NUMBER_OCTAL_CHARS       = "01234567";
    static final String NUMBER_HEXADECIMAL_CHARS = "0123456789abcdefABCDEF";

    static final char   LINECOMMENT_CHAR = '%';
    static final String OPENCOMMENT_CHAR = "/*";
    static final String CLOSECOMMENT_CHAR = "*/";
    static final char   QUOTE_CHAR = '\'';
    static final char   DOUBLEQUOTE_CHAR = '\"';

    private static final int STATE_END          = -1;
    private static final int STATE_NONE         =  0;
    private static final int STATE_ATOM         =  1;
    private static final int STATE_SPECIAL_ATOM =  2;
    private static final int STATE_NUMBER       =  3;
    private static final int STATE_EXPONENT     =  4;
    private static final int STATE_INTEGER      =  5;
    private static final int STATE_SIGN         =  6;
    private static final int STATE_LINECOMMENT  =  7;
    private static final int STATE_COMMENT      =  8;
    private static final int STATE_QUOTE        =  9;
    private static final int STATE_DOUBLEQUOTE  = 10;
    private static final int STATE_VARIABLE     = 11;
    private static final int STATE_DECIMAL      = 12;
    private static final int STATE_ASCII        = 13;
    private static final int STATE_BINARY       = 14;
    private static final int STATE_OCTAL        = 15;
    private static final int STATE_HEXADECIMAL  = 16;

    static final int TOKEN_UNKNOWN            = -1;
    static final int TOKEN_ATOM               =  1;
    static final int TOKEN_SPECIAL_ATOM       =  2;
    static final int TOKEN_NUMBER             =  3;
    static final int TOKEN_SINGLETON          =  4;
    static final int TOKEN_SIGN               =  5;
    static final int TOKEN_VARIABLE           =  6;
    static final int TOKEN_PREDDEF            =  7;
    static final int TOKEN_DBLQUOTE           =  8;
    static final int TOKEN_QUOTE              =  9;
    static final int TOKEN_WHITESPACE         = 10;

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
//        String strTerm = "";
        StringBuilder sbTerm = new StringBuilder();
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
//                m_lnReader.pushback();
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
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nState = STATE_QUOTE;
                        }
                        else if((curChar == DOUBLEQUOTE_CHAR))
                        {
                            sbTerm.append((char)curChar);
                            nState = STATE_DOUBLEQUOTE;
                        }
                        else if((LOWERCASE_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_ATOM;
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nTokenType = TOKEN_ATOM;
                        }
                        else if((UPPERCASE_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_VARIABLE;
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nTokenType = TOKEN_VARIABLE;
                        }
//                        else if((SIGN_CHARS.indexOf(curChar) > -1))
//                        {
//                            nState = STATE_SIGN;
//                            sbTerm.append( (char)curChar;//String.valueOf((char)curChar);
//                            nTokenType = TOKEN_SIGN;
//                        }
                        else if((SPECIAL_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_SPECIAL_ATOM;
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nTokenType = TOKEN_SPECIAL_ATOM;
                        }
                        else if((NUMBER_CHARS.indexOf(curChar) > -1))
                        {
                            nState = STATE_NUMBER;
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nTokenType = TOKEN_NUMBER;
                        }
                        else if(curChar <= 0x20) // whitespace char
                            //if((WHITESPACE_CHARS.indexOf(curChar) > -1))
                        {
                            //System.out.println("TOKEN_WHITESPACE");
                            nTokenType = TOKEN_WHITESPACE;
                            nState = STATE_END;
                            sbTerm = new StringBuilder(" ");
//                            strTerm = " ";
                        }
//                        else if(curChar < 0x20) // whitespace char
//                            //if((WHITESPACE_CHARS.indexOf(curChar) > -1))
//                        {
//                            nState = STATE_NONE;
//                        }
                        else if((SINGLETON_CHARS.indexOf(curChar) > -1))
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);

                            int c = m_lnReader.read();
                            if(c == -1)
                            {
                                //System.out.println("SINGLETON_CHARS EOF");

                                nState = STATE_END;
//                                m_lnReader.pushback();
                            }
                            else
                            {
                                if(c == '>' && sbTerm.charAt(sbTerm.length() - 1) == '!')
//                                if((strTerm + (char)c).equals("!>"))
                                    sbTerm.append((char)c);//String.valueOf((char)c);
                                else
                                    m_lnReader.unread(c);
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
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                        }
                        else
                        {
                            nTokenType = TOKEN_ATOM;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
                        }
                        break;

                    case STATE_VARIABLE:
                        if((UPPERCASE_CHARS.indexOf(curChar) > -1) ||
                               (LOWERCASE_CHARS.indexOf(curChar) > -1) ||
                               (NUMBER_CHARS.indexOf(curChar) > -1))
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                        }
                        else
                        {
                            nTokenType = TOKEN_VARIABLE;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
                        }
                        break;

                    case STATE_SPECIAL_ATOM:
                        char lastChar = sbTerm.charAt(sbTerm.length() - 1);
                        if((curChar == '!' && lastChar == '<') || (curChar == '>' && lastChar == '!'))
//                        if((strTerm + (char)curChar).equals("<!") || (strTerm + (char)curChar).equals("!>"))
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)c);
                            nTokenType = TOKEN_SPECIAL_ATOM;
                            nState = STATE_END;
                        }
                        else if((SPECIAL_CHARS.indexOf(curChar) > -1))
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            if(sbTerm.toString().equals(OPENCOMMENT_CHAR))
                            {
                                nState = STATE_COMMENT;
                                sbTerm = new StringBuilder("");
                                nTokenType = TOKEN_UNKNOWN;
                            }
                        }
                        else
                        {
                            nTokenType = TOKEN_SPECIAL_ATOM;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
//                            m_lnReader.pushback();
                        }
                        break;

//                    case STATE_SIGN:
//                        if((SPECIAL_CHARS.indexOf(curChar) > -1))
//                        {
//                            nState = STATE_SPECIAL_ATOM;
//                            sbTerm.append( (char)curChar;//String.valueOf((char)curChar);
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
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                        }
                        else if(curChar == '.')
                        {
                            //m_lnReader.mark(2);
                            int c = m_lnReader.read();
                            m_lnReader.unread(c);
//                            m_lnReader.pushback();
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
                                sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                                nState = STATE_DECIMAL;
                            }
                        }
                        else if(curChar == 'e' || curChar == 'E')
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nState = STATE_EXPONENT;
                        }
                        else if(curChar == '\'')
                        {
                            if(sbTerm.charAt(0) == '0')
//                            if(strTerm.equals("0"))
                                nState = STATE_ASCII;
                            else
                                throw syntaxError("invalid_character('''')");// + (char)curChar + "')");
                        }
                        else if(curChar == 'b')
                        {
                            if(sbTerm.charAt(0) == '0')
//                            if(strTerm.equals("0"))
                                nState = STATE_BINARY;
                            else
                                throw syntaxError("invalid_character('''')");// + (char)curChar + "')");
                        }
                        else if(curChar == 'o')
                        {
                            if(sbTerm.charAt(0) == '0')
//                            if(strTerm.equals("0"))
                                nState = STATE_OCTAL;
                            else
                                throw syntaxError("invalid_character('''')");// + (char)curChar + "')");
                        }
                        else if(curChar == 'x')
                        {
                            if(sbTerm.charAt(0) == '0')
//                            if(strTerm.equals("0"))
                                nState = STATE_HEXADECIMAL;
                            else
                                throw syntaxError("invalid_character('''')");// + (char)curChar + "')");
                        }
                        else
                        {
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
//                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_EXPONENT:
                        if(curChar == '+' || curChar == '-')
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nState = STATE_INTEGER;
                        }
                        else if(NUMBER_CHARS.indexOf(curChar) > -1)
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
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
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                        }
                        else if(curChar == 'e' || curChar == 'E')
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                            nState = STATE_EXPONENT;
                        }
                        else
                        {
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
//                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_INTEGER:
                        if(NUMBER_CHARS.indexOf(curChar) > -1)
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                        }
                        else
                        {
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
//                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_ASCII:
                        if(curChar == '\\')
                        {
                            sbTerm = new StringBuilder();
//                            strTerm = "";
                            curChar = m_lnReader.read();
                            switch(curChar)
                            {
                                case 'a':  // \a bell
                                    sbTerm.append(Integer.toString(7));
                                    break;

                                case 'b':  // \b backspace
                                    sbTerm.append( Integer.toString(8));
                                    break;

                                case 'd':  // \d delete
                                    sbTerm.append( Integer.toString(127));
                                    break;

                                case 'e':  // \e escape
                                    sbTerm.append( Integer.toString(27));
                                    break;

                                case 'f':  // \f form feed
                                    sbTerm.append( Integer.toString(12));
                                    break;

                                case 'n':  // \n line feed
                                    sbTerm.append( Integer.toString(10));
                                    break;

                                case 'r':  // \r carriage return
                                    sbTerm.append( Integer.toString(13));
                                    break;

                                case 's':  // \s space
                                    sbTerm.append( Integer.toString(32));
                                    break;

                                case 't':  // \t tab
                                    sbTerm.append( Integer.toString(9));
                                    break;

                                case 'v':  // \v vtab
                                    sbTerm.append( Integer.toString(11));
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
                                        sbTerm.append( Integer.toString(val[0]));
                                    }
                                    catch(NumberFormatException ex)
                                    {
                                        throw syntaxError("bad_escape_sequence('\\x" + strHexNum + "')");
                                    }

                                    // legge il prossimo byte
                                    d2 = m_lnReader.read();
                                    if(d2 != '\\')  // ISO def
                                        m_lnReader.unread(d2);
//                                        m_lnReader.pushback();
                                    break;

                                default: // ignora \
                                    sbTerm.append( Integer.toString(curChar));
                            }

                        }
                        else
                        {
                            sbTerm = new StringBuilder(Integer.toString(curChar));
//                            strTerm = Integer.toString(curChar);
                        }

                        nTokenType = TOKEN_NUMBER;
                        nState = STATE_END;
                        break;

                    case STATE_BINARY:
                        if(NUMBER_BINARY_CHARS.indexOf(curChar) > -1)
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                        }
                        else
                        {
                            sbTerm = new StringBuilder().append(Integer.parseInt(sbTerm.toString(), 2));
//                            strTerm = "" + Integer.parseInt(strTerm, 2);
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
//                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_OCTAL:
                        if(NUMBER_OCTAL_CHARS.indexOf(curChar) > -1)
                        {
                            sbTerm.append((char)curChar);//String.valueOf((char)curChar);
                        }
                        else
                        {
                            sbTerm = new StringBuilder().append(Integer.parseInt(sbTerm.toString(), 8));
//                            strTerm = "" + Integer.parseInt(strTerm, 8);
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
//                            m_lnReader.pushback();
                        }
                        break;

                    case STATE_HEXADECIMAL:
                        if(NUMBER_HEXADECIMAL_CHARS.indexOf(curChar) > -1)
                        {
                            sbTerm.append( (char)curChar);//String.valueOf((char)curChar);
                        }
                        else
                        {
                            sbTerm = new StringBuilder().append(Integer.parseInt(sbTerm.toString(), 16));
//                            strTerm = "" + Integer.parseInt(strTerm, 16);
                            nTokenType = TOKEN_NUMBER;
                            nState = STATE_END;
                            m_lnReader.unread(curChar);
//                            m_lnReader.pushback();
                        }
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
                                    sbTerm.append('"');
                                }
                                else
                                {
                                    // fine quoted atom
                                    m_lnReader.unread(c);
//                                    m_lnReader.pushback();
                                    sbTerm.append( (char)curChar);//String.valueOf((char)curChar);
                                    nTokenType = (nState == STATE_QUOTE) ? TOKEN_QUOTE : TOKEN_DBLQUOTE;
                                    nState = STATE_END;
                                }
                            }
                            else
                            {
                                sbTerm.append( (char)curChar);
                            }
                        }
                        else if(curChar == QUOTE_CHAR)
                        {
                            if(nState == STATE_QUOTE)
                            {
                                c = m_lnReader.read();
                                if(c == QUOTE_CHAR)
                                {
                                    sbTerm.append('\'');
                                }
                                else
                                {
                                    // fine quoted atom
                                    m_lnReader.unread(c);
//                                    m_lnReader.pushback();
                                    sbTerm.append( (char)curChar);//String.valueOf((char)curChar);
                                    nTokenType = (nState == STATE_QUOTE) ? TOKEN_QUOTE : TOKEN_DBLQUOTE;
                                    nState = STATE_END;
                                }
                            }
                            else
                            {
                                sbTerm.append( (char)curChar);
                            }
                        }
                        else if(nState == STATE_QUOTE && (curChar == '\r' || curChar == '\n'))
                        {
                            throw syntaxError("carriage_return_in_quoted_atom('" + sbTerm.toString() + "')");
                        }
//                        else if(curChar == '~')  // edimburgh prolog
//                        {
//                            c = m_lnReader.read();
//                            if(c == '~')
//                            {
//                                sbTerm.append( (char)c;//String.valueOf((char)c);
//                            }
//                            else if(c == '\'')
//                            {
////                              fine quoted atom
//                                m_lnReader.pushback();
//                                //sbTerm.append( (char)(c - '@');//String.valueOf((char)c - '@');
//                            }
//                            else if(c >= '@')
//                            {
//                                sbTerm.append( (char)(c - '@');//String.valueOf((char)c - '@');
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
                                sbTerm.append((char)c);//String.valueOf((char)c);
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
                                    m_lnReader.unread(d1);
//                                    m_lnReader.pushback();

                                try
                                {
                                    BigInteger bival = new BigInteger(strNum, 8);

                                    byte val = bival.byteValue();// Byte.parseByte(strNum);
                                    sbTerm.append((char)val);
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
                                        sbTerm.append( (char)(7));
                                        break;

                                    case 'b':  // \b backspace
                                        sbTerm.append( (char)(8));
                                        break;

                                    case 'c':  // \c empty
                                        //sbTerm.append( (char)(8);
                                        break;

                                    case 'd':  // \d delete
                                        sbTerm.append( (char)(127));
                                        break;
    
                                    case 'e':  // \e escape
                                        sbTerm.append( (char)(27));
                                        break;

                                    case 'f':  // \f form feed
                                        sbTerm.append( (char)(12));
                                        break;

                                    case 'n':  // \n line feed
                                        sbTerm.append( (char)(10));
                                        break;

                                    case 'r':  // \r carriage return
                                        sbTerm.append( (char)(13));
                                        break;

                                    case 's':  // \s space
                                        sbTerm.append( (char)(32));
                                        break;

                                    case 't':  // \t tab
                                        sbTerm.append( (char)(9));
                                        break;

                                    case 'v':  // \v vtab
                                        sbTerm.append( (char)(11));
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
                                            sbTerm.append( (char)(val[0]));
                                        }
                                        catch(NumberFormatException ex)
                                        {
                                            throw syntaxError("bad_escape_sequence('\\x" + strHexNum + "')");
                                        }

                                        // legge il prossimo byte
                                        d2 = m_lnReader.read();
                                        if(d2 != '\\')  // ISO def
                                            m_lnReader.unread(d2);
//                                            m_lnReader.pushback();
                                        break;

                                    case '\r':
                                    case '\n':
                                        break;

                                    default: // ignora \
                                        sbTerm.append( (char)(c));
                                }
                            }
//                            else
//                                throw syntaxError("bad_escape_sequence('\\" + (char)c + "')");
                        }
                        else
                        {
                            sbTerm.append( (char)curChar);//String.valueOf((char)curChar);
                        }
                        break;

//                    case STATE_DOUBLEQUOTE:
//                        if(curChar == DOUBLEQUOTE_CHAR)
//                        {
//                            sbTerm.append( (char)curChar;
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
//                                sbTerm.append( (char)c;//String.valueOf((char)c);
//                            }
//                            else if(c == '"')
//                            {
//                                m_lnReader.pushback();
//                                //sbTerm.append( (char)(c - '@');//String.valueOf((char)c - '@');
//                            }
//                            else if(c > '@')
//                            {
//                                sbTerm.append( (char)(c - '@');//String.valueOf((char)c - '@');
//                            }
//                            else
//                                throw syntaxError("bad_escape_sequence('~" + (char)c + "')");
//                        }
//                        else if(curChar == '\\') // ISO prolog
//                        {
//                            c = m_lnReader.read();
//                            if(c == '\\')
//                            {
//                                sbTerm.append( (char)c;//String.valueOf((char)c);
//                            }
//                            else if(c >= 'a')
//                            {
//                                switch(c)
//                                {
//                                    case 'a':  // \a bell
//                                        sbTerm.append( (char)(7);
//                                        break;
//
//                                    case 'b':  // \b backspace
//                                        sbTerm.append( (char)(8);
//                                        break;
//
//                                    case 'd':  // \d delete
//                                        sbTerm.append( (char)(127));
//                                        break;
//
//                                    case 'e':  // \e escape
//                                        sbTerm.append( (char)(27));
//                                        break;
//
//                                    case 'f':  // \f form feed
//                                        sbTerm.append( (char)(12);
//                                        break;
//
//                                    case 'n':  // \n line feed
//                                        sbTerm.append( (char)(10);
//                                        break;
//
//                                    case 'r':  // \r carriage return
//                                        sbTerm.append( (char)(13);
//                                        break;
//
//                                    case 't':  // \t tab
//                                        sbTerm.append( (char)(9);
//                                        break;
//
//                                    case 'v':  // \v vtab
//                                        sbTerm.append( (char)(11);
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
//                                            sbTerm.append( (char)(val[0]);
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
//                                        sbTerm.append( (char)(c);
//                                }
//                            }
//                        }
//                        else
//                        {
//                            sbTerm.append( (char)curChar;//String.valueOf((char)curChar);
//                        }
//                        break;
                }
            }
        }

        if(sbTerm.length() > 0)
        {
            Token tok = new Token();
            tok.m_strToken = sbTerm.toString();
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
