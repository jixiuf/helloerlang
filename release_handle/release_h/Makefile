PROJECT=release_h
PREVIOUS_RELEASE_VERSION=0.1
REBAR=./rebar
REBAR_UP=../rebar

compile:
	@$(REBAR) get-deps compile
	@-cp src/$(PROJECT).appup.src ebin/$(PROJECT).appup

clean:
	@$(REBAR) clean

# 不太明白为什么起的名字叫create-node
# 但是要想运行 rebar generate
# ./rel/目录下必须有rebar create-node nodeid=$(PROJECT) 命令生成的
# 一系列文件才行，比如 reltool.config
createnode:
	@-mkdir rel
	cd rel && ../rebar create-node nodeid=$(PROJECT)

create-node:createnode

cn:createnode

# 生成release版本,生成的文件是 ./rel/$(PROJECT)目录下
# 在生成之前，需要编辑 ./rel/reltool.config文件，修改成相应的版本，配好各种信息
# reltool.config生成的模版直接用，未必可用， 一些参数需要调整。
generate:
	$(REBAR) generate

gen:generate

generatef:
	$(REBAR) generate -f
# rebar  generate-appups 指令与 appup文件有关，
# 如果 ebin/目录下有一个 appup 文件，则直接使用这个文件，不为其自动生成一个
# 如果没有，则为其自动生成一个,
# 生成的appup文件，会放一份在 ./rel/$(PROJECT)/lib/$(PROJECT)-new-version/ebin/目录下
appup:
	$(REBAR) generate-appups previous_release=$(PROJECT)-$(PREVIOUS_RELEASE_VERSION)
# 会在 ./rel目录下生成一个tar.gz压缩包，
upgrade:
	$(REBAR) generate-upgrade previous_release=$(PROJECT)-$(PREVIOUS_RELEASE_VERSION)

up:appup upgrade
