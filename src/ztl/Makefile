all: ztl_scanner.erl

%.erl: %.slex
	erl -noinput -eval "slex_compiler:compile('$<', \
		[{target, erl}, {out_dir, '.'}])" \
		-s init stop
