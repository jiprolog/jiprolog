% load external pls

jip_init_modules :-
	env(debug, 'on'),
	!,
	use_module('INTERNAL://com/ugos/jiprolog/resources/flags.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/list.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/sys.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/xsets.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/setof.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/xio.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/xdb.pl'),
	%use_module('INTERNAL://com/ugos/jiprolog/resources/xexception.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/xreflect.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/xsystem.pl'),
	%use_module('INTERNAL://com/ugos/jiprolog/resources/xxml.pl'),
	use_module('INTERNAL://com/ugos/jiprolog/resources/xterm.pl').



jip_init_modules :-
	use_module('INTERNAL://com/ugos/jiprolog/resources/flags.jip'), %write('flags.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/list.jip'), %write('list.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/sys.jip'), %write('sys.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/xsets.jip'), %write('xsets.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/setof.jip'), %write('xsets.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/xio.jip'), %write('xio.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/xdb.jip'), %write('xdb.jip'), nl,
	%use_module('INTERNAL://com/ugos/jiprolog/resources/xexception.jip'), %write('xexception.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/xreflect.jip'), %write('xreflect.jip'), nl,
	use_module('INTERNAL://com/ugos/jiprolog/resources/xsystem.jip'), %write('xsystems.jip'), nl,
	%use_module('INTERNAL://com/ugos/jiprolog/resources/xxml.jip'), %write('xxml.jip'), nl.
	use_module('INTERNAL://com/ugos/jiprolog/resources/xterm.jip'). %write('xterm.jip'), nl.



:-jip_init_modules.
