/***************************************************************
 * 15/10/2002
 *
 * Copyright (C) 2002 Ugo Chirico
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 ****************************************************************/

:-module(jipxxml,
         [xml_write_document/1, xml_write_document/2, xml_read_document/1, xml_read_document/2, xml_object/1,
         xml_child/1, xml_object_name/2, xml_object_value/2, xml_object_type/2, xml_document/2, xml_document/5,
         xml_document_root/2, xml_document_version/2, xml_document_encoding/2,xml_doctype/3,xml_doctype/4,
         xml_doctype_id/2, xml_element/3, xml_element/4, xml_element_attributes/2, xml_element_attribute_by_name/3,
         xml_element_children/2, xml_element_child_by_name/3, xml_append_attribute/3, xml_remove_attribute/3,
         xml_append_child/3, xml_remove_child/3, xml_attribute/2, xml_attribute/3, xml_cdata/1, xml_cdata/2,
         xml_text/1, xml_text/2, xml_comment/1, xml_comment/2, xml_pi/1, xml_pi/2]).

:-assert(ver(jipxxml, '3.0.0')).

/**********************
 * XML I/O predicates
 ***********************/

xml_write_document(xml_document(Prolog, Root)):-
    !,
    write_prolog(Prolog),
    write_elements([Root]).

xml_write_document(Handle, xml_document(Prolog, Root)):-
    !,
    write_prolog(Handle, Prolog),
    write_elements(Handle, [Root]).

xml_read_document(X):-
    xcall('com.ugos.jiprolog.extensions.xml.XMLRead1', [X]).

xml_read_document(Handle, X):-
    xcall('com.ugos.jiprolog.extensions.xml.XMLRead2', [Handle, X]).

/***************************************
 * Common predicates about DOM objects
 ***************************************/

xml_object(Object):-
    Object =.. [xml_document, _Prolog, _Root] ;
    Object =.. [xml_doctype, _Name, _Id, _Content] ;
    Object =.. [xml_element, _Name, _Attrs, _Children] ;
    Object =.. [xml_text, _Content] ;
    Object =.. [xml_cdata, _Content] ;
    Object =.. [xml_comment, _Content] ;
    Object =.. [xml_attribute, _Name, _Value] ;
    Object =.. [xml_pi, _Name, _Value].

xml_child(Object):-
    Object =.. [xml_element, _Name, _Attrs, _Children] ;
    Object =.. [xml_text, _Content] ;
    Object =.. [xml_cdata, _Content] ;
    Object =.. [xml_comment, _Content] ;
    Object =.. [xml_pi, _Name, _Value].

xml_object_type(Element, Type):-
    xml_object(Element),
    Element =.. [Type | _Args].

xml_object_name(xml_doctype(Name, _Id, _Content), Name).
xml_object_name(xml_element(Name, _Attributes, _Children), Name).
xml_object_name(xml_attribute(Name, _Value), Name).
xml_object_name(xml_pi(Name, _Data), Name).

xml_object_value(xml_attribute(_Name, Value), Value).
xml_object_value(xml_cdata(Content), Content).
xml_object_value(xml_text(Content), Content).
xml_object_value(xml_comment(Content), Content).
xml_object_value(xml_pi(_Name, Data), Data).

/***************************************
 * predicates about Document object
 ***************************************/

xml_document(Version, [], DocType, Root, xml_document([[version = Version], DocType], Root)):-!.
xml_document(Version, Encoding, DocType, Root, xml_document([[version = Version, encoding = Encoding], DocType], Root)).

xml_document_root(xml_document(_Prolog, Root), Root).

xml_document_version(xml_document([[version = Version|_Rest]|_DocType], _Root), Version).

xml_document_encoding(xml_document([[_Version, encoding = Encoding]|_Rest], _Root), Encoding).

/***************************************
 * predicates about DocumentType object
 ***************************************/

xml_doctype(Name, Id, Content, xml_doctype(Name, Id, Content)).

xml_doctype_id(xml_doctype(_Name, Id, _Content), Id).

/***************************************
 * predicates about Element object
 ***************************************/

xml_element(Name, Attributes, Children, xml_element(Name, Attributes, Children)).

xml_element_children(xml_element(_Name, _Attrs, Children), Children).

xml_element_child_by_name(Name, xml_element(_Name, _Attrs, Children), Child):-
    xml_element_by_name(Name, Children, Child).

xml_element_attributes(xml_element(_Name, Attributes, _Children), Attributes).

xml_element_attribute_by_name(Name, xml_element(_Name, Attributes, _Children), xml_attribute(Name, Value)):-
    member(xml_attribute(Name,Value), Attributes).

xml_append_attribute(xml_attribute(AttName, Value), xml_element(Name, Attributes, Children), xml_element(Name, NewAttributes, Children)):-
    append(Attributes, [xml_attribute(AttName,Value)], NewAttributes).

xml_remove_attribute(xml_attribute(AttName, Value), xml_element(Name, Attributes, OldChildren), xml_element(Name, NewAttributes, Children)):-
    remove(xml_attribute(AttName, Value), Attributes, NewAttributes).

xml_append_child(Child, xml_element(Name, Attributes, Children), xml_element(Name, Attributes, NewChildren)):-
    xml_child(Child),
    append(Children, [Child], NewChildren).

xml_remove_child(xml_element(CName, CAttrs, CChildren), xml_element(Name, Attributes, Children), xml_element(Name, Attributes, NewChildren)):-
    remove(xml_element(CName, CAttrs, CChildren), Children, NewChildren).

/***************************************
 * predicates about Attr object
 ***************************************/

xml_attribute(Name, Value, xml_attribute(Name, Value)).

/***************************************
 * predicates about CDATA object
 ***************************************/

xml_cdata(Content, xml_cdata(Content)).

/***************************************
 * predicates about Text object
 ***************************************/

xml_text(Content, xml_text(Content)).

/***************************************
 * predicates about Comment object
 ***************************************/

xml_comment(Content, xml_comment(Content)).

/***************************************
 * predicates about ProcessingInstruction object
 ***************************************/

xml_pi(Name, Data, xml_pi(Name, Data)).

/***************************************
 * Utility predicates
 ***************************************/

xml_element_by_name(Name, Elements, xml_element(Name, Attrs, Children)):-
    member(xml_element(Name, Attrs, Children), Elements).

/***************************************
 * private predicates
 ***************************************/

write_prolog([Attributes, []]):-
    !,
    write('<?xml'),
    write_attributes(Attributes),
    write('?>'), nl.

write_prolog([Attributes, DocType]):-
    write('<?xml'),
    write_attributes(Attributes),
    write(' ?>'), nl,
    write_doctype(DocType).

write_attributes([Name = Value | Rest]):-
    !,
    write(' '),
    write(Name),
    write('="'),
    write(Value),
    write('"'),
    write_attributes(Rest).

write_attributes([xml_attribute(Name, Value) | Rest]):-
    !,
    write(' '),
    write(Name),
    write('="'),
    write(Value),
    write('"'),
    write_attributes(Rest).

write_attributes([]).

write_elements([xml_element(Name, Attributes, []) | Rest]):-
    !,
    write('<'),
    write(Name),
    write_attributes(Attributes),
    write('/>'), nl,
    write_elements(Rest).

write_elements([xml_element(Name, Attributes, Children) | Rest]):-
    !,
    write('<'),
    write(Name),
    write_attributes(Attributes),
    write('>'), nl,
    write_elements(Children),
    write('</'),
    write(Name),
    write('>'),nl,
    write_elements(Rest).


write_elements([xml_cdata(Content) | Rest]):-
    !,
    write('<![CDATA['),
    write(Content),
    write(']]>'), nl,
    write_elements(Rest).

write_elements([xml_text(Content) | Rest]):-
    !,
    write(Content), nl,
    write_elements(Rest).

write_elements([xml_comment(Text) | Rest]):-
    !,
    write('<-- '),
    write(Text),
    write('-->'), nl,
    write_elements(Rest).

write_elements([xml_pi(Name, Content) | Rest]):-
    !,
    write('<?'),
    write(Name),
    write(' '),
    write(Content),
    write(' ?>'), nl,
    write_elements(Rest).

write_elements([]).

write_doctype(xml_doctype(Name, [], [])):-
    !,
    write('<!DOCTYPE '),
    write(Name),
    write('>'), nl.

write_doctype(xml_doctype(Name, Id, [])):-
    !,
    write('<!DOCTYPE '),
    write(Name),
    write(' '),
    write_id(Id),
    write('>'), nl.

write_doctype(xml_doctype(Name, [], Content)):-
    !,
    write('<!DOCTYPE '),
    write(Name),
    write(' ['),
    write(Content), nl,
    write(']>'), nl.

write_doctype(xml_doctype(Name, Id, Content)):-
    !,
    write('<!DOCTYPE '),
    write(Name),
    write(' '),
    write_id(Id),
    write(' ['),
    write(Content), nl,
    write(']>'), nl.

write_id([Name = Value | Rest]):-
    !,
    write(' '),
    write(Name),
    write(' '),
    write(Value),
    write_id(Rest).

write_id([]).


write_prolog(Handle, [Attributes, []]):-
    !,
    write(Handle, '<?xml'),
    write_attributes(Handle, Attributes),
    write(Handle, '?>'), nl(Handle).

write_prolog(Handle, [Attributes, DocType]):-
    !,
    write(Handle, '<?xml'),
    write_attributes(Handle, Attributes),
    write(Handle, ' ?>'), nl(Handle),
    write_doctype(Handle, DocType).

write_attributes(Handle, [Name = Value | Rest]):-
    !,
    write(Handle, ' '),
    write(Handle, Name),
    write(Handle, '="'),
    write(Handle, Value),
    write(Handle, '"'),
    write_attributes(Handle, Rest).

write_attributes(Handle, [xml_attribute(Name, Value) | Rest]):-
    !,
    write(Handle, ' '),
    write(Handle, Name),
    write(Handle, '="'),
    write(Handle, Value),
    write(Handle, '"'),
    write_attributes(Handle, Rest).

write_attributes(Handle, []).

write_elements(Handle, [xml_element(Name, Attributes, Children) | Rest]):-
    !,
    write(Handle, '<'),
    write(Handle, Name),
    write_attributes(Handle, Attributes),
    write(Handle, '>'), nl(Handle),
    write_elements(Handle, Children),
    write(Handle, '</'),
    write(Handle, Name),
    write(Handle, '>'),nl(Handle),
    write_elements(Handle, Rest).

write_elements(Handle, [xml_cdata(Content) | Rest]):-
    !,
    write(Handle, '<![CDATA['),
    write(Handle, Content),
    write(Handle, ']]>'), nl(Handle),
    write_elements(Handle, Rest).

write_elements(Handle, [xml_text(Content) | Rest]):-
    !,
    write(Handle, Content), nl(Handle),
    write_elements(Handle, Rest).

write_elements(Handle, [xml_comment(Text) | Rest]):-
    !,
    write(Handle, '<!-- '),
    write(Handle, Text),
    write(Handle, '-->'), nl(Handle),
    write_elements(Handle, Rest).

write_elements(Handle, [xml_pi(Name, Content) | Rest]):-
    !,
    write(Handle, '<?'),
    write(Handle, Name),
    write(Handle, ' '),
    write(Handle, Content),
    write(Handle, ' ?>'), nl(Handle),
    write_elements(Handle, Rest).

write_elements(Handle, []).

write_doctype(Handle, xml_doctype(Name, [], [])):-
    !,
    write(Handle, '<!DOCTYPE '),
    write(Handle, Name),
    write(Handle, '>'), nl(Handle).

write_doctype(Handle, xml_doctype(Name, Id, [])):-
    !,
    write(Handle, '<!DOCTYPE '),
    write(Handle, Name),
    write(Handle, ' '),
    write_id(Handle, Id),
    write(Handle, '>'), nl(Handle).

write_doctype(Handle, xml_doctype(Name, [], Content)):-
    !,
    write(Handle, '<!DOCTYPE '),
    write(Handle, Name),
    write(Handle, ' ['),
    write(Handle, Content), nl(Handle),
    write(Handle, ']>'), nl(Handle).

write_doctype(Handle, xml_doctype(Name, Id, Content)):-
    !,
    write(Handle, '<!DOCTYPE '),
    write(Handle, Name),
    write(Handle, ' '),
    write_id(Handle, Id),
    write(Handle, ' ['),
    write(Handle, Content), nl(Handle),
    write(Handle, ']>'), nl(Handle).

write_id(Handle, [Name = Value | Rest]):-
    !,
    write(Handle, ' '),
    write(Handle, Name),
    write(Handle, ' '),
    write(Handle, Value),
    write_id(Handle, Rest).

write_id(Handle, []).



