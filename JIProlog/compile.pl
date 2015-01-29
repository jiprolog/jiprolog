

compile:-
	write('compilation start'), nl,
	compile('src/com/ugos/jiprolog/resources/jipkernel.txt'),
	compile('src/com/ugos/jiprolog/resources/list.pl'),
	compile('src/com/ugos/jiprolog/resources/sys.pl'),
	compile('src/com/ugos/jiprolog/resources/xsets.pl'),
	compile('src/com/ugos/jiprolog/resources/xio.pl'),
	compile('src/com/ugos/jiprolog/resources/xdb.pl'),
	compile('src/com/ugos/jiprolog/resources/xexception.pl'),
	compile('src/com/ugos/jiprolog/resources/xreflect.pl'),
	compile('src/com/ugos/jiprolog/resources/xsystem.pl'),
	compile('src/com/ugos/jiprolog/resources/xterm.pl'),
	compile('src/com/ugos/jiprolog/resources/xxml.pl'),
	write('compilation done'), nl.


:- compile.

