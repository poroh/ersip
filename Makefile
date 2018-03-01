#
# Copyright (c) 2017 Dmitry Poroh
# All rights reserved.
# Distributed under the terms of the MIT License. See the LICENSE file.
#
# SIP message buffer
#

SHELL=/bin/bash

tests:
	export ERL_FLAGS=$(ERL_FLAGS) ; rebar3 do eunit -v --cover, cover | sed 's/^_build\/test\/lib\/ersip\///' ; exit "$${PIPESTATUS[0]}"
