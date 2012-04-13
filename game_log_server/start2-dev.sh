#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin \
	-name emacs2@192.168.0.44 \
	-rsh ssh \
	-config slave.config \
	-s start_app
