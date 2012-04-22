#!/bin/sh
./rebar compile
erlc test_transaction.erl
erl -pa ebin/ -config log.config -boot start_sasl -s emysql start -s test_transaction run -s init stop
