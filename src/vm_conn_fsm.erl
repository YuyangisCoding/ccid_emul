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

-module(vm_conn_fsm).

-behaviour(gen_statem).
-include("include/records.hrl").
-include("include/usb.hrl").

-compile([{parse_transform, lager_transform}]).

-export([pretty_print/1]).

-export([
    start_link/2
    ]).

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    accept/3,
    await_init/3,
    connect/3,
    connected/3
    ]).

-spec start_link(gen_tcp:socket(), pid()) -> {ok, pid()} | {error, term()}.
start_link(LSock, Sup) ->
    gen_statem:start_link(?MODULE, [LSock, Sup], []).

-record(?MODULE, {
    lsock :: gen_tcp:socket(),
    sup :: pid(),
    sock :: undefined | gen_tcp:socket(),
    vm :: undefined | binary(),
    fsm :: undefined | pid(),
    mref :: undefined | reference(),
    reqs = gen_server:reqids_new() :: gen_server:request_id_collection(),
    seq = 0 :: integer(),
    waiters = #{} :: #{integer() => gen_statem:from()}
    }).

-define(pp(Rec),
pretty_print(Rec, N) ->
    N = record_info(size, Rec) - 1,
    record_info(fields, Rec)).

pretty_print(Record) ->
    io_lib_pretty:print(Record, [
        {record_print_fun, fun pretty_print/2},
        {line_length, 9999}]).
?pp(?MODULE);
pretty_print(Rec, N) ->
    ccid:pretty_print(Rec, N).

init([LSock, Sup]) ->
    {ok, accept, #?MODULE{lsock = LSock, sup = Sup}}.

terminate(_Why, _State, #?MODULE{}) ->
    ok.

callback_mode() -> [state_functions, state_enter].

accept(enter, _PrevState, #?MODULE{}) ->
    {keep_state_and_data, [{state_timeout, 0, accept}]};
accept(state_timeout, accept, S0 = #?MODULE{lsock = LSock, sup = Sup}) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    vm_conn_sup:start_child(Sup),
    inet:setopts(Sock, [{active, once}]),
    S1 = S0#?MODULE{sock = Sock},
    {next_state, await_init, S1}.

await_init(enter, _PrevState, #?MODULE{}) ->
    keep_state_and_data;
await_init(info, {tcp, Sock, Data}, S0 = #?MODULE{sock = Sock}) ->
    case Data of
        <<Seq:32/big, ?URELAY_INIT, NameZero:256/binary>> ->
            [Name | _] = binary:split(NameZero, [<<0>>]),
            lager:md([{vm_name, Name}]),
            gen_tcp:send(Sock, <<Seq:32/big, ?URELAY_STATUS, 0>>),
            S1 = S0#?MODULE{vm = Name},
            inet:setopts(Sock, [{active, once}]),
            {next_state, connect, S1};
        _ ->
            lager:debug("weird data while waiting for init: ~p", [Data]),
            inet:setopts(Sock, [{active, once}]),
            keep_state_and_data
    end;
await_init(info, {tcp_error, Sock, Why}, S0 = #?MODULE{sock = Sock}) ->
    lager:debug("socket error: ~p", [Why]),
    {stop, normal, S0};
await_init(info, {tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    {stop, normal, S0}.

connect(enter, _PrevState, #?MODULE{vm = Name}) ->
    lager:debug("handling connection from ~p", [Name]),
    {keep_state_and_data, [{state_timeout, 0, connect}]};
connect(state_timeout, connect, S0 = #?MODULE{vm = VM}) ->
    {ok, Fsm} = ccid_fsm:open(VM),
    lager:debug("connected to ccid fsm ~p", [Fsm]),
    MRef = erlang:monitor(process, Fsm),
    S1 = S0#?MODULE{fsm = Fsm, mref = MRef},
    ok = ccid_fsm:take_control(Fsm),
    {next_state, connected, S1};
connect(info, {tcp, Sock, _Data}, #?MODULE{sock = Sock}) ->
    {keep_state_and_data, [postpone]};
connect(info, {tcp_error, Sock, _Why}, #?MODULE{sock = Sock}) ->
    {keep_state_and_data, [postpone]};
connect(info, {tcp_closed, Sock}, #?MODULE{sock = Sock}) ->
    {keep_state_and_data, [postpone]}.

connected(enter, _PrevState, #?MODULE{}) ->
    keep_state_and_data;
connected(info, {'DOWN', MRef, process, Pid, _Why},
                                    S0 = #?MODULE{fsm = Pid, mref = MRef}) ->
    {next_state, connect, S0};
connected(info, {tcp, Sock, Data}, S0 = #?MODULE{sock = Sock, reqs = Reqs0,
                                                 fsm = Fsm}) ->
    case Data of
        % A response to our intr request
        <<Seq:32/big, ?URELAY_STATUS, Errno>> ->
            #?MODULE{waiters = W0} = S0,
            Msg = #urelay_status{errno = Errno},
            {From, W1} = maps:take(Seq, W0),
            gen_statem:reply(From, Msg),
            S1 = S0#?MODULE{waiters = W1},
            inet:setopts(Sock, [{active, once}]),
            {keep_state, S1};

        % Requests from the VM
        <<Seq:32/big, ?URELAY_RESET>> ->
            Msg = #urelay_reset{},
            Reqs1 = gen_server:send_request(Fsm, Msg, Seq, Reqs0),
            S1 = S0#?MODULE{reqs = Reqs1},
            inet:setopts(Sock, [{active, once}]),
            {keep_state, S1};

        <<Seq:32/big, ?URELAY_CTRL, Req, ReqType, Value:16/big, Index:16/big,
          Length:16/big>> ->
            Msg = #urelay_ctrl{req = Req, req_type = ReqType, value = Value,
                               index = Index, length = Length},
            Reqs1 = gen_server:send_request(Fsm, Msg, Seq, Reqs0),
            S1 = S0#?MODULE{reqs = Reqs1},
            inet:setopts(Sock, [{active, once}]),
            {keep_state, S1};

        <<Seq:32/big, ?URELAY_DATA, Dir, EP, Rem:32/big, RData/binary>> ->
            Msg = #urelay_data{dir = Dir, ep = EP, remain = Rem, data = RData},
            Reqs1 = gen_server:send_request(Fsm, Msg, Seq, Reqs0),
            S1 = S0#?MODULE{reqs = Reqs1},
            inet:setopts(Sock, [{active, once}]),
            {keep_state, S1};

        _ ->
            lager:debug("weird data: ~p", [Data]),
            inet:setopts(Sock, [{active, once}]),
            keep_state_and_data
    end;
connected(info, {tcp_error, Sock, Why}, S0 = #?MODULE{sock = Sock}) ->
    lager:debug("socket error: ~p", [Why]),
    {stop, normal, S0};
connected(info, {tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    lager:debug("socket closed"),
    {stop, normal, S0};
connected({call, From}, #urelay_intr{dir = Dir, ep = EP}, S0 = #?MODULE{}) ->
    #?MODULE{sock = Sock, seq = Seq0, waiters = W0} = S0,
    Pkt = <<Seq0:32/big, ?URELAY_INTR, Dir, EP>>,
    ok = gen_tcp:send(Sock, Pkt),
    W1 = W0#{Seq0 => From},
    S1 = S0#?MODULE{seq = Seq0 + 1, waiters = W1},
    {keep_state, S1};
connected(info, Msg, S0 = #?MODULE{reqs = Reqs0, sock = Sock}) ->
    {{reply, Resp}, Seq, Reqs1} = gen_server:check_response(Msg, Reqs0, true),
    S1 = S0#?MODULE{reqs = Reqs1},
    case Resp of
        #urelay_status{errno = Errno} ->
            Pkt = <<Seq:32/big, ?URELAY_STATUS, Errno>>,
            ok = gen_tcp:send(Sock, Pkt),
            {keep_state, S1};
        #urelay_ctrl_resp{errcode = ErrCode, rc = RC, blen = BLen,
                          bdone = BDone, data = Data} ->
            Pkt = <<Seq:32/big, ?URELAY_CTRL_RESP, ErrCode,
                    RC:32/big, BLen:32/big, BDone:32/big, Data/binary>>,
            ok = gen_tcp:send(Sock, Pkt),
            {keep_state, S1};
        #urelay_data_resp{errcode = ErrCode, rc = RC, blen = BLen,
                          bdone = BDone, data = Data} ->
            Pkt = <<Seq:32/big, ?URELAY_DATA_RESP, ErrCode,
                    RC:32/big, BLen:32/big, BDone:32/big, Data/binary>>,
            ok = gen_tcp:send(Sock, Pkt),
            {keep_state, S1}
    end.
