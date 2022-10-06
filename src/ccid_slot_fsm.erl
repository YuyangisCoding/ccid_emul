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

-module(ccid_slot_fsm).

-behaviour(gen_statem).
-include("include/records.hrl").
-include("include/usb.hrl").
-include("include/iso7816.hrl").

-compile([{parse_transform, lager_transform}]).

-export([pretty_print/1]).

-export([
    start_link/3,
    remove_card/1,
    insert_card/1
    ]).

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    empty/3,
    pwr_off/3,
    pwr_on/3,
    wait_abort/3
    %mute/3
    ]).

-spec start_link(string(), integer(), pid()) -> {ok, pid()} | {error, term()}.
start_link(Name, SlotIdx, Sup) ->
    gen_statem:start_link(?MODULE, [Name, SlotIdx, Sup], []).

-spec remove_card(pid()) -> ok | {error, term()}.
remove_card(Pid) ->
    gen_statem:call(Pid, remove_card).

-spec insert_card(pid()) -> ok | {error, term()}.
insert_card(Pid) ->
    gen_statem:call(Pid, insert_card).

-record(?MODULE, {
    sup :: pid(),
    name :: string(),
    slotidx :: integer(),
    last_cmd :: undefined | ccid:host_msg(),
    aborter :: undefined | gen_statem:from(),
    abort_to :: undefined | atom(),
    card :: pid(),
    cmref :: reference()
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

init([Name, SlotIdx, Sup]) ->
    lager:md([{vm_name, Name}, {slot_idx, SlotIdx}]),
    lager:debug("slot fsm for ~p slot ~B (~p)", [Name, SlotIdx, Sup]),
    {ok, Card} = ccid_card_fsm:open(Name, SlotIdx),
    MRef = erlang:monitor(process, Card),
    ok = gen_statem:call(Card, reset),
    {ok, pwr_off, #?MODULE{name = Name, slotidx = SlotIdx, sup = Sup,
                           card = Card, cmref = MRef}}.

terminate(_Why, _State, #?MODULE{}) ->
    ok.

callback_mode() -> [state_functions, state_enter].

empty(enter, _PrevState, S0 = #?MODULE{name = Name, slotidx = Slot}) ->
    lager:debug("[~s/~B] empty", [Name, Slot]),
    {keep_state, S0};

empty(info, {'DOWN', MRef, process, Card, _Why},
                                    S0 = #?MODULE{card = Card, cmref = MRef}) ->
    #?MODULE{name = Name, slotidx = SlotIdx} = S0,
    {ok, NewCard} = ccid_card_fsm:open(Name, SlotIdx),
    NewMRef = erlang:monitor(process, NewCard),
    ok = gen_statem:call(Card, reset),
    {keep_state, S0#?MODULE{card = NewCard, cmref = NewMRef}};

empty({call, From}, reset, #?MODULE{card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    keep_state_and_data;
empty({call, From}, insert_card, S0 = #?MODULE{card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    {next_state, pwr_off, S0};
empty({call, From}, remove_card, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;

empty({call, From}, abort, #?MODULE{last_cmd = A = #ccid_pc_to_rdr_abort{},
                                    aborter = AbortFrom}) ->
    gen_statem:reply(From, ok),
    #ccid_pc_to_rdr_abort{slot = Slot, seq = Seq} = A,
    Msg = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = not_present},
                                      clock = stopped},
    gen_statem:reply(AbortFrom, Msg),
    keep_state_and_data;
empty({call, From}, abort, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, wait_abort, S0};
empty({call, From}, X = #ccid_pc_to_rdr_abort{}, S0 = #?MODULE{}) ->
    {keep_state, S0#?MODULE{last_cmd = X, aborter = From}};

empty({call, From}, X = #ccid_pc_to_rdr_getslotstatus{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = not_present},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
empty({call, From}, Cmd = #ccid_pc_to_rdr_xfrblock{}, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = not_present,
                                          cmd = failed,
                                          error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}};
empty({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweron{}, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = not_present,
                                          cmd = failed,
                                          error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}};
empty({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = not_present},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}};

empty({call, From}, X = #ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = not_present},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
empty({call, From}, X = #ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = not_present},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
empty({call, From}, X = #ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = not_present,
                                                  cmd = failed,
                                                  error = 10},
                                  params = none},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};

empty({call, From}, Cmd, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = not_present,
                                          cmd = failed,
                                          error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}}.


pwr_off(enter, _PrevState, #?MODULE{name = Name, slotidx = Slot}) ->
    lager:debug("[~s/~B] powered off", [Name, Slot]),
    keep_state_and_data;

pwr_off(info, {'DOWN', MRef, process, Card, _Why},
                                    S0 = #?MODULE{card = Card, cmref = MRef}) ->
    #?MODULE{name = Name, slotidx = SlotIdx} = S0,
    {ok, NewCard} = ccid_card_fsm:open(Name, SlotIdx),
    NewMRef = erlang:monitor(process, NewCard),
    ok = gen_statem:call(Card, reset),
    {keep_state, S0#?MODULE{card = NewCard, cmref = NewMRef}};

pwr_off({call, From}, reset, #?MODULE{card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    keep_state_and_data;
pwr_off({call, From}, insert_card, #?MODULE{card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    keep_state_and_data;
pwr_off({call, From}, remove_card, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, empty, S0};

pwr_off({call, From}, abort, #?MODULE{last_cmd = A = #ccid_pc_to_rdr_abort{},
                                      aborter = AbortFrom,
                                      name = Name, slotidx = Slot}) ->
    gen_statem:reply(From, ok),
    lager:debug("[~s/~B] using stashed abort cmd", [Name, Slot]),
    #ccid_pc_to_rdr_abort{slot = Slot, seq = Seq} = A,
    Msg = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = inactive},
                                      clock = stopped},
    gen_statem:reply(AbortFrom, Msg),
    keep_state_and_data;
pwr_off({call, From}, abort, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, wait_abort, S0};
pwr_off({call, From}, X = #ccid_pc_to_rdr_abort{}, S0 = #?MODULE{}) ->
    #?MODULE{name = Name, slotidx = Slot} = S0,
    lager:debug("[~s/~B] stashing abort cmd", [Name, Slot]),
    {keep_state, S0#?MODULE{last_cmd = X, aborter = From}};

pwr_off({call, From}, X = #ccid_pc_to_rdr_getslotstatus{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = inactive},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, Cmd = #ccid_pc_to_rdr_xfrblock{}, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = inactive,
                                          cmd = failed,
                                          error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}};
pwr_off({call, From}, X = #ccid_pc_to_rdr_iccpoweron{slot = Slot, seq = Seq},
                                                    S0 = #?MODULE{card = C}) ->
    {ok, ATR} = gen_statem:call(C, get_atr),
    Resp = #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, data = ATR},
    gen_statem:reply(From, Resp),
    {next_state, pwr_on, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = inactive},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}};

pwr_off({call, From}, X = #ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = inactive},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, X = #ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = inactive},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, X = #ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = inactive,
                                                  cmd = failed,
                                                  error = 10},
                                  params = none},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};

pwr_off({call, From}, Cmd, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = inactive,
                                          cmd = failed,
                                          error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}}.

wait_abort(enter, PrevState, S0 = #?MODULE{name = Name, slotidx = Slot}) ->
    lager:debug("[~s/~B] awaiting abort cmd", [Name, Slot]),
    {keep_state, S0#?MODULE{abort_to = PrevState}};
wait_abort({call, From}, reset, S0 = #?MODULE{abort_to = State, card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    {next_state, State, S0};
wait_abort({call, From}, X = #ccid_pc_to_rdr_abort{slot = Slot, seq = Seq},
                                            S0 = #?MODULE{abort_to = State}) ->
    % XXX: the state should decide what err and clock we put here?
    Msg = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                     err = #ccid_err{icc = inactive},
                                     clock = stopped},
    gen_statem:reply(From, Msg),
    {next_state, State, S0#?MODULE{last_cmd = X}};
wait_abort({call, From}, Cmd, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = inactive,
                                          cmd = failed,
                                          error = ?CCID_CMD_ABORTED}),
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}}.

pwr_on(enter, _PrevState, #?MODULE{name = Name, slotidx = Slot}) ->
    lager:debug("[~s/~B] powered on", [Name, Slot]),
    keep_state_and_data;

pwr_on({call, From}, reset, S0 = #?MODULE{card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    {next_state, pwr_off, S0};

pwr_on({call, From}, abort, #?MODULE{last_cmd = A = #ccid_pc_to_rdr_abort{},
                                     aborter = AbortFrom}) ->
    gen_statem:reply(From, ok),
    #ccid_pc_to_rdr_abort{slot = Slot, seq = Seq} = A,
    Msg = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = active},
                                      clock = running},
    gen_statem:reply(AbortFrom, Msg),
    keep_state_and_data;
% other abort cases handled by pwr_off

pwr_on(info, {'DOWN', MRef, process, Card, _Why},
                                    S0 = #?MODULE{card = Card, cmref = MRef}) ->
    #?MODULE{name = Name, slotidx = SlotIdx} = S0,
    {ok, NewCard} = ccid_card_fsm:open(Name, SlotIdx),
    NewMRef = erlang:monitor(process, NewCard),
    % treat this as a card hardware error I guess?
    ok = gen_statem:call(Card, reset),
    {next_state, pwr_off, S0#?MODULE{card = NewCard, cmref = NewMRef}};

pwr_on({call, From}, X = #ccid_pc_to_rdr_getslotstatus{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = active},
                                      clock = running},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};

pwr_on({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = inactive},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {next_state, pwr_off, S0#?MODULE{last_cmd = Cmd}};
pwr_on({call, From}, X = #ccid_pc_to_rdr_iccpoweron{slot = Slot, seq = Seq},
                                                    S0 = #?MODULE{card = C}) ->
    {ok, ATR} = gen_statem:call(C, get_atr),
    ok = gen_statem:call(C, reset),
    Resp = #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, data = ATR},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};

pwr_on({call, From}, X = #ccid_pc_to_rdr_xfrblock{slot = Slot, seq = Seq},
                        S0 = #?MODULE{name = Name, slotidx = Slot, card = C}) ->
    #ccid_pc_to_rdr_xfrblock{data = APDUData, chain = one} = X,
    APDU = iso7816:decode_apdu_cmd(APDUData),
    lager:debug("[~s/~B] APDU >> ~s", [Name, Slot, ccid:pretty_print(APDU)]),
    RAPDU = case APDU of
        #apdu_cmd{} ->
            gen_statem:call(C, APDU);
        {error, Why} ->
            lager:debug("apdu decode failed: ~p", [Why]),
            #apdu_reply{sw = {error, general_failure}}
    end,
    lager:debug("[~s/~B] APDU << ~s", [Name, Slot, ccid:pretty_print(RAPDU)]),
    RepData = iso7816:encode_apdu_reply(RAPDU),
    Resp = #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, data = RepData},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};

pwr_on({call, From}, X = #ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = active},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
pwr_on({call, From}, X = #ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = active},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};
pwr_on({call, From}, X = #ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = active,
                                                  cmd = failed,
                                                  error = 10},
                                  params = none},
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = X}};

pwr_on({call, From}, X, S0 = #?MODULE{}) ->
    % fall through to pwr_off handling
    pwr_off({call, From}, X, S0).
