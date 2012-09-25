PROJECT=port
PREVIOUS_RELEASE_VERSION=0.1PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
REBAR=./rebar
REBAR_UP=../rebar
.PHONY:test

compile:
	@$(REBAR) get-deps compile
	@-cp src/$(PROJECT).appup.src ebin/$(PROJECT).appup
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
