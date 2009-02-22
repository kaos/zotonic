ERL          ?= erl
ERLC          = erlc
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := zophrenic
PARSER        =src/erlydtl/erlydtl_parser

all: epgsql epgsql_pool mochiweb webmachine gen $(PARSER).erl erl ebin/$(APP).app 

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'code:load_file(gen_scomp), case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

gen:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:files(["src/behaviours/gen_scomp"]) of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl src/erlydtl/erlydtl_parser.yrl

epgsql:
	(cd deps/epgsql; make)

epgsql_pool:
	(cd deps/epgsql_pool; make)

mochiweb:
	(cd deps/mochiweb; make)

webmachine:
	(cd deps/webmachine; make)

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	(cd deps/epgsql; make clean)
	(cd deps/epgsql_pool; make clean)
	(cd deps/mochiweb; make clean)
	(cd deps/webmachine; make clean)
	@rm -fv ebin/*.beam ebin/*.app
	@rm -fv erl_crash.dump $(PARSER).erl
	@rm -fv priv/log/*

ebin/$(APP).app:
	@cp -v src/$(APP).app $@

