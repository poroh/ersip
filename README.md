
[![Build Status](https://travis-ci.org/poroh/ersip.svg?branch=master)](https://travis-ci.org/poroh/ersip) [![Coverage Status](https://coveralls.io/repos/github/poroh/ersip/badge.svg?branch=master)](https://coveralls.io/github/poroh/ersip?branch=master)

# Erlang SIP

Some time ago I found that there are no opensource SIP library that
does not force you to some software design. All libraries provides
some framework that is required in your project if you want to use
SIP. I believe that this is wrong way. Goal of this project is to
provide high-level primitives that implements all SIP functions but
leave to user how to organize these primitives in his/her software.

## Roadmap

  + Basic low-level parser (completed)
  + Essential headers support (completed)
  + High-level SIP message (completed)
  + Stateless proxy support (completed)
     - Request passing (completed)
     - Response passing (completed)
  + Transaction support (completed)
     - Transaction idenification (completed)
     - Non-INVITE transaction (completed)
     - INVITE transaction (completed)
  + Registrar support (completed)
  + Parser limits enforcement (completed)
  + Statefull proxy support (completed)
  + Common dialog support (completed)
  + INVITE dialog support (in-progress)
  + Authorization and proxy authorization
  + High-level UA support
  + Detailed documentation and tutorials

## Realworld examples

Working examples:

  + Sateful SIP proxy: https://github.com/poroh/ersip_proxy

## Provided primitives

Primitives that provided by SIP stack:

  + Connection parser split streams and retrieve low level SIP messages (ersip_conn)
  + Low level SIP message processing (ersip_msg)
  + Lazy high-level SIP message processing (ersip_sipmsg)
  + Transactions support (ersip_trans)
  + Basic UAS support (ersip_uas)
  + Registrar function support (ersip_registrar)
  + SIP stateful proxy function support (ersip_proxy)
  + SIP common dialog support (ersip_dialog)

## Supported RFC

RFC supported in this SIP stack:

  + RFC 2543 SIP: Session Initiation Protocol (backward compatibility)
  + RFC 3261 SIP: Session Initiation Protocol (partially - in progress)
  + RFC 6026 Correct Transaction Handling for 2xx Responses to Session Initiation Protocol (SIP) INVITE Requests
  + RFC 3581 An Extension to the Session Initiation Protocol (SIP) for Symmetric Response Routing (rport)
  + RFC 4475 Session Initiation Protocol (SIP) Torture Test Messages
  + RFC 5118 Session Initiation Protocol (SIP) Torture Test Messages for Internet Protocol Version 6 (IPv6)

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

