/**
 *
 */
package com.ugos.jiprolog.engine;

/**
 * @author UgoChirico
 *
 A domain error occurs when the type of an argument is correct but its value is outside the expected domain. ErrorTerm has the following form: domain_error(Domain, Culprit) where Domain is the expected domain and Culprit the argument which caused the error. Domain is one of:

atom_property
buffering_mode
character_code_list
close_option
date_time
eof_action
fd_labeling_option
flag_value
format_control_sequence
g_array_index
io_mode
non_empty_list
not_less_than_zero
operator_priority
operator_specifier
order
os_file_permission
os_file_property
os_path
predicate_property
prolog_flag
read_option
selectable_item
socket_address
socket_domain
source_sink
statistics_key
statistics_value
stream
stream_option
stream_or_alias
stream_position
stream_property
stream_seek_method
stream_type
term_stream_or_alias
var_binding_option
write_option
 */
public class JIPDomainException extends JIPRuntimeException {

	private String domain;
	private String culprit;

	public JIPDomainException(String domain, String culprit)
	{
		this.domain = domain;
		this.culprit = culprit;
	}

	@Override
	public JIPTerm getTerm()
	{
    	return getTerm(new Functor("domain_error/2", new ConsCell (Atom.createAtom(domain), new ConsCell(Atom.createAtom(culprit),  null))));

//    	return getTerm("domain_error(" + domain + ", " + culprit + ")");
	}

}
