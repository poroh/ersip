
[![Build Status](https://travis-ci.org/poroh/ersip.svg?branch=master)](https://travis-ci.org/poroh/ersip) [![Coverage Status](https://coveralls.io/repos/github/poroh/ersip/badge.svg?branch=master)](https://coveralls.io/github/poroh/ersip?branch=master)

# Erlang SIP and SDP library

## Approach to design

What you cannot find in ersip:
  + Supervisors
  + Servers
  + ETS
  + Dependencies

You can find only pure functions that are compliant to SIP specification.

## Real world examples

Working examples:

  + Stateful SIP proxy: https://github.com/poroh/ersip_proxy
  + Hunt group service (B2BUA) example: https://github.com/poroh/piraha

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
  + RFC 3261 SIP: Session Initiation Protocol (partially):
      - 7 SIP Messages
      - 8.2 UAS Behavior
      - 9 Canceling a Request
      - 10 Registration - Registrar part only
      - 12 Dialogs
      - 16 Proxy Behavior
      - 17 Transactions
      - 18 Transport
      - 19 Common Message Components
  + RFC 3262 Reliability of Provisional Responses in the Session Initiation Protocol (SIP)
      - 6 Definition of the PRACK Method
      - 7 Header Field Definitions
      - 7.1 RSeq
      - 7.2 RAck
  + RFC 6026 Correct Transaction Handling for 2xx Responses to Session Initiation Protocol (SIP) INVITE Requests
  + RFC 3581 An Extension to the Session Initiation Protocol (SIP) for Symmetric Response Routing (rport)
  + RFC 4475 Session Initiation Protocol (SIP) Torture Test Messages
  + RFC 5118 Session Initiation Protocol (SIP) Torture Test Messages for Internet Protocol Version 6 (IPv6)
  + RFC 4566 SDP: Session Description Protocol (partially)
  + RFC 3515 The Session Initiation Protocol (SIP) Refer Method (header/method only):
      - 2.1  The Refer-To Header Field
  + RFC 6665 SIP-Specific Event Notification (header/method only)
      - 8.2.1. "Event" Header Field
      - 8.2.3. "Subscription-State" Header Field
  + RFC 3891 The Session Initiation Protocol (SIP) "Replaces" Header (header only)
      - 6.1. The Replaces Header

## License

MIT

## Roadmap

  + Basic low-level parser (completed)
  + Essential headers support (completed)
  + High-level SIP message (completed)
  + Stateless proxy support (completed)
     - Request passing (completed)
     - Response passing (completed)
  + Transaction support (completed)
     - Transaction identification (completed)
     - Non-INVITE transaction (completed)
     - INVITE transaction (completed)
  + Registrar support (completed)
  + Parser limits enforcement (completed)
  + Stateful proxy support (completed)
  + Common dialog support (completed)
  + SDP support (completed)
  + Detailed documentation and tutorials
  + Authorization and proxy authorization
  + INVITE dialog support
  + High-level UA support
