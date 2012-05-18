PROJECT=release_h
REBAR=./rebar

all:
	@$(REBAR) get-deps compile


eunit:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

release:
	rm -rf rel
	mkdir rel
	cd rel && ../rebar create-node nodeid=$(PROJECT)
	$(REBAR) generate
