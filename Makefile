#
# Copyright (c) 2017 Dmitry Poroh
# All rights reserved.
# Distributed under the terms of the MIT License. See the LICENSE file.
#
# SIP message buffer
#

SHELL=/bin/bash

tests:
	export ERL_FLAGS=$(ERL_FLAGS) ; rebar3 do eunit -v --cover, proper -n 10000 --constraint_tries 500 -c, cover | sed 's/^_build\/test\/lib\/ersip\///' ; exit "$${PIPESTATUS[0]}"

proper:
	rebar3 do proper -n 100000 --constraint_tries 500
