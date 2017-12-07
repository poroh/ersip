[![Build Status](https://travis-ci.org/poroh/ersip.svg?branch=master)](https://travis-ci.org/poroh/ersip) [![Coverage Status](https://coveralls.io/repos/github/poroh/ersip/badge.svg?branch=master)](https://coveralls.io/github/poroh/ersip?branch=master)

Erlang SIP
==========

Goals
-----

Goal of the project to implement lightweight scalable SIP stack.

I beleive that Erlang may become great telephony platform again :).

Story decomposion
-----------------

Defined layers:

   - Parser
   - Transport
   - Transaction layer
   - Dialog layer


# Parser and low level messages

Low level parser operates with items:

   - Request Linee (for requests)
   - Status Lines (for responses)
   - Headers in generic manner (without parsing content)
   - Body (without interpret it)
   
Low level parser is defined by module
https://github.com/poroh/ersip/blob/master/src/ersip_parser.erl and
generates low level message
https://github.com/poroh/ersip/blob/master/src/ersip_msg.erl.

