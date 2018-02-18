
[![Build Status](https://travis-ci.org/poroh/ersip.svg?branch=master)](https://travis-ci.org/poroh/ersip) [![Coverage Status](https://coveralls.io/repos/github/poroh/ersip/badge.svg?branch=master)](https://coveralls.io/github/poroh/ersip?branch=master)

Erlang SIP
==========

Some time ago I found that there are no opensource SIP library
that does not force you to some software design. All libraries
provides some framework that required in project if you want to use
SIP.

Roadmap
------

    - Basic low-level parser (completed)
    - Essential headers support (in-progress)
    - Tranacation support
      - Transaction table
      - Non-INVITE transaction
      - INVITE transaction
    - Dialog support
    - High-level UA support
    - SIP proxy support 

Basics
-----

Idea of this project to provide SIP stack that:

    - can be integrated anywhere in any other infratructure
    - has 100% test coveage
    - fully conformant with IETF standards and BCPs

To do this this library provides high-level building blocks:

    - SIP parser
    - SIP message
    - Transactions
    - UAs

All layers and objects has well-defined interface and well-defined
side effects that need to be implemented.


Side effects
------

Following side effects must be implemented to use library

    - Timer set
    - Send message
    - UA's side effects

Transport-layer interaction
------

When transport layer receives new portion of data it need to feed
buffer inside the UA.

When UA needs to send new message it returns appropriate side-effect
return code.
