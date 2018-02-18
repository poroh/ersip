
[![Build Status](https://travis-ci.org/poroh/ersip.svg?branch=master)](https://travis-ci.org/poroh/ersip) [![Coverage Status](https://coveralls.io/repos/github/poroh/ersip/badge.svg?branch=master)](https://coveralls.io/github/poroh/ersip?branch=master)

# Erlang SIP
==========

Some time ago I found that there are no opensource SIP library
that does not force you to some software design. All libraries
provides some framework that required in project if you want to use
SIP.

## About Author
------

Dmitry Poroh:

Has huge (17 years) exprience in telecommunication areas. Among other projects I have developed:

  + SMTP protocol (RFC2821)
  + HTTP/FTP proxy (RFC2616, RFC0959)
  + OSPF protocol (RFC2328)
  + ZigBee/IEEE 802.15.4 protocol stack
  + HTTP/HTTPS implementation (RFC2616)
  + DiffServ implementation
  + VPN (IPSec/ESP) tunnels implementation
  + RTP/RTCP implementation (RFC3550/RFC3551)

Also I have experience in VOIP area:

  + Private PBX implementation based on FreeSWITCH
  + Multipoint Control Unit (developed from scratch)
  + MSML-capable Audio Media Gateway
  + Session Border Controller

I try to contribute all my experience to this project because I
beleive that SIP protocol is instrument the must be free and
available.

## Roadmap
------

  + Basic low-level parser (completed)
  + Essential headers support (in-progress)
  + Tranacation support
     - Transaction table
     - Non-INVITE transaction
     - INVITE transaction
  + Dialog support
  + High-level UA support
  + SIP proxy support 

## Basics
-----

Idea of this project to provide SIP stack that:

  + can be integrated anywhere in any other infratructure
  + has 100% test coveage
  + fully conformant with IETF standards and BCPs

To do this this library provides high-level building blocks:

  + SIP parser
  + SIP message
  + Transactions
  + UAs

All layers and objects has well-defined interface and well-defined
side effects that need to be implemented.


## Side effects
------

Following side effects must be implemented to use library

  + Timer set
  + Send message
  + UA's side effects

## Transport-layer interaction
------

When transport layer receives new portion of data it need to feed
buffer inside the UA.

When UA needs to send new message it returns appropriate side-effect
return code.
