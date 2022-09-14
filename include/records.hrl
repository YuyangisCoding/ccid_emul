%% ccid_emul
%%
%% Copyright 2022, The University of Queensland
%% Author: Alex Wilson <alex@uq.edu.au>
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-define(URELAY_STATUS,      16#80).
-define(URELAY_CTRL,        16#01).
-define(URELAY_CTRL_RESP,   16#81).
-define(URELAY_DATA,        16#02).
-define(URELAY_DATA_RESP,   16#82).
-define(URELAY_RESET,       16#03).
-define(URELAY_INIT,        16#04).
-define(URELAY_INTR,        16#05).

-record(urelay_reset, {}).

-record(urelay_status, {
    errno :: integer()
    }).

-record(urelay_ctrl, {
    req :: integer(),
    req_type :: integer(),
    value :: integer(),
    index :: integer(),
    length :: integer()
    }).

-record(urelay_ctrl_resp, {
    errcode = 0 :: integer(),
    rc = 0 :: integer(),
    blen = 0 :: integer(),
    bdone = 0 :: integer(),
    data = <<>> :: binary()
    }).

-define(URELAY_DIR_IN,  16#11).
-define(URELAY_DIR_OUT, 16#22).

-record(urelay_data, {
    dir :: ?URELAY_DIR_IN | ?URELAY_DIR_OUT,
    ep :: integer(),
    remain :: integer(),
    data :: binary()
    }).

-record(urelay_data_resp, {
    errcode = 0 :: integer(),
    rc = 0 :: integer(),
    blen = 0 :: integer(),
    bdone = 0 :: integer(),
    data = <<>> :: binary()
    }).

-record(urelay_intr, {
    dir :: ?URELAY_DIR_IN | ?URELAY_DIR_OUT,
    ep :: integer()
    }).
