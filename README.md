
[![Build Status](https://travis-ci.org/poroh/ersip.svg?branch=master)](https://travis-ci.org/poroh/ersip) [![Coverage Status](https://coveralls.io/repos/github/poroh/ersip/badge.svg?branch=master)](https://coveralls.io/github/poroh/ersip?branch=master)

# Erlang SIP

Some time ago I found that there are no opensource SIP library that
does not force you to some software design. All libraries provides
some framework that is required in your project if you want to use
SIP. I beleive that this is wrong way.

## About Author

Dmitry Poroh:

Has huge (17 years) exprience in telecommunication areas. Among other
projects I have developed:

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
  + MSML-capable Audio Media Gateway (developed from scratch)
  + Session Border Controller (developed from scratch)

I try to contribute all my experience to this project because I
beleive that SIP protocol is instrument the must be free and
available.

## Roadmap

  + Basic low-level parser (completed)
  + Essential headers support (completed)
  + High-level SIP message (completed)
  + Stateless proxy support (in-progress)
  + Transaction support
     - Transaction table
     - Non-INVITE transaction
     - INVITE transaction
  + Statefull proxy support
  + Dialog support
  + Authorization and proxy authorization
  + High-level UA support
  + SIP proxy support
  + Detailed documentation and tutorials

## Basics

Idea of this project to provide SIP stack that:

  + can be integrated anywhere in any other infrastructure
  + has 100% test coverage
  + fully conformant with IETF standards and BCPs

To do this this library provides high-level building blocks:

  + SIP parser
  + SIP message
  + Proxy functions
  + Stateless proxy functions
  + Statefull proxy object description
  + Transactions
  + UAs

All layers and objects has well-defined interface and well-defined
side effects/callbacks that need to be implemented.

## SIP message representation

There is two levels of messages available:

  + Low level message (ersip_msg:message())
  + SIP message (ersip_sipmsg:sip_msg()

### Low level message

Provides simplies level of parsing and lowlevel header manipulation.
It consist of 

  + First line parsed data
  + Set of low-level headers
  + Not parsed body

## SIP message

SIP message has low level message under the hood but also has some set
of parsed headers. It provides interface for higher level manipulation
of SIP messages. For example you can easy change some headers with
your values. Code example:


```
NewRURI = ersip_uri:make(<<"sip:alice@atlanta.com">>),
NewSipMsg = ersip_sipmsg:set_ruri(NewRURI, SipMsg)
```

After serialization message will contain this RURI instead previous
one.

