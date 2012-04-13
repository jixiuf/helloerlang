#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ./ebin ./deps/*/ebin \
	-name emacs@192.168.0.44 \
	-rsh ssh \
	-config master.config \
	-s start_app
