# make compile  编译本application
# make compile-deps 编译 application/deps/*
# make compile-all

#make install-deps  copy applications in  deps/ to /usr/lib/erlang/lib/目录下
#make install     copy 本applicatoin to /usr/lib/erlang/lib/目录下
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
REBAR=./rebar
DEPS=./deps/*
MAKE=make
all: compile compile-deps
clean-all: clean clean-deps
compile:
	@$(REBAR) get-deps compile
compile-deps:
	for d in $(DEPS); do (cd $$d; $(MAKE)  ); done
edoc:
	@$(REBAR) doc
test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
clean-deps:
	@$(REBAR) clean
	for d in $(DEPS); do (cd $$d; $(MAKE) clean  ); done

install-deps:compile compile-deps
# 如果 /usr/lib/erlang/lib/下没有 projects/deps/subprojects 子模块的名，则copy subprojects to /usr/lib/erlang/lib/
	@for dep in $(DEPS);do \
	f=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`/`basename $$dep`; \
	 if [ ! -d $$f ]; then \
	    cp -r $$dep $$f ;\
	 fi \
	done
install:compile
	cp -r $(CURDIR) $(LIBDIR)
