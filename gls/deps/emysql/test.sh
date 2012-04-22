#!/usr/bin/sh
./rebar compile
erlc hello.erl
erl -pa ./ebin -s hello run -s init stop -noshell
