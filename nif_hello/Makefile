PROJECT=nif_test
PREVIOUS_RELEASE_VERSION=0.1PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
REBAR=./rebar
REBAR_UP=../rebar
ERL_ROOT=/usr/lib/erlang/
.PHONY:test

compile:
	gcc -fPIC -shared -o ebin/hello_c.so c_src/hello_c.c -I $(ERL_ROOT)/usr/include/
	@$(REBAR) get-deps compile
edoc:
	@$(REBAR) doc
test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
build_plt:
	@$(REBAR) build-plt
dialyzer:
	@$(REBAR) dialyze
app:
	@$(REBAR) create-app dest=$(DEST) appid=$(PROJECT)

createnode:
	@-mkdir rel
	cd rel && $(REBAR_UP) create-node nodeid=$(PROJECT)
create-node:createnode
cn:createnode

generate:
	$(REBAR) generate
gen:generate
generatef:
	$(REBAR) generate -f

appup:
	$(REBAR) generate-appups previous_release=$(PROJECT)-$(PREVIOUS_RELEASE_VERSION)
upgrade:
	$(REBAR) generate-upgrade previous_release=$(PROJECT)-$(PREVIOUS_RELEASE_VERSION)
up:appup upgrade
