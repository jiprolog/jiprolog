/*
 * 15/10/2002
 *
 * Copyright (C) 2002 Ugo Chirico
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


package com.ugos.jiprolog.extensions.xml;

import com.ugos.jiprolog.engine.*;

import java.io.*;
import java.util.*;

import org.xml.sax.*;
import org.w3c.dom.*;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;

public class XMLRead1 extends JIPXCall
{
    public boolean unify(JIPCons input, Hashtable varsTbl)
    {
        JIPTerm term = input.getNth(1);
        if(term instanceof JIPVariable)
            if(((JIPVariable)term).isBounded())
                term = ((JIPVariable)term).getValue();


        //System.out.println(getJIPEngine().getCurrentInputStreamName());
        //m_xmlDoc = createXMLTerm(new InputStreamReader(getJIPEngine().getCurrentInputStream()));
        JIPTerm xmlDoc = createXMLTerm(String.valueOf(getJIPEngine().getCurrentInputStreamHandle()));

        return term.unify(xmlDoc, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }

    //static JIPTerm createXMLTerm(Reader ins)
    JIPTerm createXMLTerm(String strStreamName)
    //static JIPTerm createXMLTerm(String strFile)
    {
        Document xmldoc;
        try
        {
        	DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
    		DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();

//            //Debug.traceln("Parser: " + parser, 1);
            xmldoc = dBuilder.parse(new InputSource(strStreamName));


            // get version
            String strVer;
            try
            {
                strVer = xmldoc.getXmlEncoding();
            }
            catch(Throwable ex)
            {
                strVer = "1.0";
            }

            // get encoding
            String strEnc;
            try
            {
                strEnc = xmldoc.getInputEncoding();
            }
            catch(Throwable ex)
            {
                strEnc  = "UTF-8";
            }

            // create prolog structure
            String strProlog = "[[version = '" + strVer + "'";

            if(strEnc != null)
                strProlog += ", encoding = '" + strEnc + "'";

            strProlog += "]";

            // get doctype
            DocumentType docType = xmldoc.getDoctype();
            if(docType != null)
            {
//                //Debug.traceln("DocType: " + docType, 1);
                String strDTD = createXMLTerm(docType);
                strProlog += ", " + strDTD;
            }
            else
            {
                strProlog += ", []";
            }

            // close prolog
            strProlog += "]";

//            //Debug.traceln("Doc Prolog: " + strProlog, 1);

            // create xml doc
            Element doc = xmldoc.getDocumentElement();
            //Debug.traceln("Node type: " + doc.getNodeType() + " - " + doc, 1);

            // create root
            String strRoot = createXMLTerm(doc);
            //Debug.traceln("Root: " + strRoot, 1);

            // create prolog xml doc
            String strDoc = "xml_document(" + strProlog + ", " + strRoot + ")";
            //Debug.traceln("Doc: " + strDoc, 1);
            //System.out.println(strDoc);
            JIPTerm xmlDocTerm = getJIPEngine().getTermParser().parseTerm(strDoc);
            //Debug.traceln("Prolog XML Doc: " + xmlDocTerm, 1);
            return xmlDocTerm;
        }
        catch(JIPSyntaxErrorException ex)
        {
            throw new JIPRuntimeException(4002, ex.getMessage());
        }
        catch(SAXException ex)
        {
            //ex.printStackTrace();
            throw new JIPRuntimeException(4003, "SAX Exception: " + ex.getMessage());
        }
        catch(Exception ex)
        {
            //ex.printStackTrace();
            throw new JIPJVMException(ex);
        }
    }

    private static String doubleQuote(String str)
    {
        //double '
        int nPos = 0;
        int nBegin = 0;
        while(nPos > -1)
        {
            nPos = str.indexOf('\'', nBegin);
            if(nPos != -1)
            {
                str = str.substring(0, nPos) + '\'' + str.substring(nPos, str.length());
                nBegin = nPos + 2;
            }
        }

        return str;
    }

    private static String createXMLTerm(Node n) throws SAXException
    {
        //Debug.traceln("Node: " + n, 1);
        int type = n.getNodeType();
        switch (type)
        {
            case Node.CDATA_SECTION_NODE:
                //Debug.traceln("CDATA", 1);
                CDATASection cdata = (CDATASection)n;
                if(cdata.getData().equals("\n"))
                    return null;
                else
                    return "xml_cdata('" + doubleQuote(cdata.getData().trim()) + "')";

            case Node.TEXT_NODE:
                //Debug.traceln("TEXT", 1);
                Text text = (Text)n;
                if(text.getData().equals("\n"))
                    return null;
                else
                    return "xml_text('" + doubleQuote(text.getData().trim()) + "')";

            case Node.COMMENT_NODE:
                //Debug.traceln("COMMENT", 1);
                Comment comment = (Comment)n;
                if(comment.getData().equals("\n"))
                    return null;
                else
                    return "xml_comment('" + doubleQuote(comment.getData().trim()) + "')";

            case Node.ELEMENT_NODE:
                //Debug.traceln("ELEMENT", 1);
                Element elem = (Element)n;
                String strName = elem.getNodeName();

                String strElement = "xml_element('" + strName + "'";

                // get element attrs
                NamedNodeMap attrs = elem.getAttributes();
                if(attrs != null)
                    strElement += ", [" + createAttrList(attrs) + "], ";
                else
                    strElement += ", [], ";

                // get children
                String strChildren = "";
                for (Node child = n.getFirstChild(); child != null;
                         child = child.getNextSibling())
                {
                    //Debug.traceln("Child:" + child, 1);
                    String strChild = createXMLTerm(child);
                    if(strChild != null)
                    {
                        strChildren += (strChildren.equals("") ? "" : ", ") + createXMLTerm(child);
                    }
                }

                strElement += "[" + strChildren + "])";

                return strElement;

            case Node.PROCESSING_INSTRUCTION_NODE:
                ProcessingInstruction pi = (ProcessingInstruction)n;
                if(pi.getData().equals("\n"))
                    return null;
                else
                    return "xml_pi('" + pi.getNodeName() + "', '" + doubleQuote(pi.getData().trim()) + "')";

            case Node.DOCUMENT_TYPE_NODE:
                //Debug.traceln("DOC_TYPE", 1);
                DocumentType docType = (DocumentType)n;
                //Debug.traceln("Val:" + docType.getNodeValue(), 1);
                //Debug.traceln("Attrs:" + docType.getAttributes(), 1);
                //Debug.traceln("Children:" + docType.getChildNodes() + " - " + docType.getChildNodes().getLength(), 1);
                //Debug.traceln("Entities:" + docType.getEntities() + " - " + docType.getEntities().getLength(), 1);
                //Debug.traceln("InternalSubset:" + docType.getInternalSubset(), 1);
                //Debug.traceln("Notations:" + docType.getNotations(), 1);
                //Debug.traceln("PublicId:" + docType.getPublicId(), 1);
                //Debug.traceln("SystemId:" + docType.getSystemId(), 1);

                strElement = "xml_doctype('" + docType.getNodeName() + "', [";

                if(docType.getPublicId() != null)
                {
                    strElement += " 'PUBLIC' = '" + doubleQuote(docType.getPublicId()) + "'";
                }

                if(docType.getSystemId() != null)
                {
                    if(docType.getPublicId() != null)
                        strElement += ", ";

                    strElement += " 'SYSTEM' = '" + doubleQuote(docType.getSystemId()) + "'";
                }

                strElement += "], ";

                String strInternalSubset = docType.getInternalSubset();

                if(strInternalSubset != null)
                {
                    int nBegin = 0;
                    int nPos = strInternalSubset.indexOf('\'');
                    String strAux = "";

                    while(nPos > -1)
                    {
                        strAux += strInternalSubset.substring(nBegin, nPos) + "'";
                        nBegin = nPos;
                        nPos = strInternalSubset.indexOf('\'', nBegin + 1);
                        //Debug.traceln("strInternalSubset: " + strAux, 1);
                    }

                    strAux += strInternalSubset.substring(nBegin);

                    strElement += "['" + strAux + "']";
                }
                else
                {
                    strElement += "[]";
                }

                strElement += ")";

                return strElement;

            default:
                throw new JIPRuntimeException(4001, "Unsupported Tag: " + n.toString());
        }
    }

    private static String createAttrList(NamedNodeMap attrMap)
    {
        String strAttrs = "";
        for (int i = 0; i < attrMap.getLength(); i++)
        {
            Attr attr = (Attr)attrMap.item(i);
            strAttrs += "xml_attribute('" + attr.getName() + "', '" + doubleQuote(attr.getValue()) + "')";
            if(i < attrMap.getLength() - 1)
                strAttrs += ", ";
        }

        return strAttrs;
    }
}
