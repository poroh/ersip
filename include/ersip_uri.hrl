%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP URI
%%

-record(uri, { scheme = sip       :: sip | sips,
               %% user: The identifier of a particular resource at the host being
               %%    addressed.  The term "host" in this context frequently refers
               %%    to a domain.  The "userinfo" of a URI consists of this user
               %%    field, the password field, and the @ sign following them.  The
               %%    userinfo part of a URI is optional and MAY be absent when the
               %%    destination host does not have a notion of users or when the
               %%    host itself is the resource being identified.  If the @ sign is
               %%    present in a SIP or SIPS URI, the user field MUST NOT be empty.
               user   = undefined :: undefined  
                                   | { user, binary() },
               %% host: The host providing the SIP resource.  The host part contains
               %%    either a fully-qualified domain name or numeric IPv4 or IPv6
               %%    address.  Using the fully-qualified domain name form is
               %%    RECOMMENDED whenever possible.
               host               :: ersip_host:host(),
                %% port: The port number where the request is to be sent.
               port   = 5060      :: 1..65535,
               %% URI parameters: Parameters affecting a request constructed from
               %% the URI.
               params = #{}       :: #{ transport => ersip_transport:transport(),
                                        maddr     => ersip_host:host()
                                      }
             }).

-type uri() :: #uri{}.
