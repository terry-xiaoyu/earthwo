REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

ct: compile
	$(REBAR) as test ct -v

eunit: compile
	$(REBAR) as test eunit

rel: prod

prod:
	$(REBAR) as prod tar

xref:
	$(REBAR) xref

clean: clean

distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock
