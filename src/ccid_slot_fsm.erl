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

-vsn({2, 0}).

-export([pretty_print/1]).

-export([
    start_link/3,
    remove_card/1,
    insert_card/1,
    config_err/4
    ]).

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4,
    empty/3,
    pwr_off/3,
    pwr_on/3,
    wait_abort/3,
    busy/3
    %mute/3
    ]).

-type err_scenario() :: power_on | apdu.
%% Scenario for error injection.
-type err_type() :: long_sleep | hw_error | parity | ripped.
%% Types of error which are allowed to be injected at random.
-type p_err() :: integer().
%% 0-100, percent probability of an error
-type err_config() :: #{err_scenario() => {p_err(), [err_type()]}}.

-spec start_link(string(), integer(), pid()) -> {ok, pid()} | {error, term()}.
start_link(Name, SlotIdx, Sup) ->
    gen_statem:start_link(?MODULE, [Name, SlotIdx, Sup], []).

-spec remove_card(pid()) -> ok | {error, term()}.
remove_card(Pid) ->
    gen_statem:call(Pid, remove_card).

-spec insert_card(pid()) -> ok | {error, term()}.
insert_card(Pid) ->
    gen_statem:call(Pid, insert_card).

-spec config_err(pid(), err_scenario(), p_err(), [err_type()]) -> ok | {error, term()}.
config_err(Pid, Scenario, PErr, Types) ->
    gen_statem:call(Pid, {config_err, Scenario, {PErr, Types}}).

-record(?MODULE, {
    sup :: pid(),
    name :: string(),
    slotidx :: integer(),
    last_cmd :: undefined | ccid:host_msg(),
    aborter :: undefined | gen_statem:from(),
    abort_to :: undefined | atom(),
    abort_seq :: undefined | integer(),
    idle_to :: undefined | atom(),
    card_req :: undefined | gen_statem:request_id(),
    waiter :: undefined | gen_statem:from(),
    card :: pid(),
    cmref :: reference(),
    err = #{} :: err_config()
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

code_change(_OldVsn, OldState, S0, _Extra) when (element(1, S0) =:= ?MODULE) and
                                                (tuple_size(S0) == 9) ->
    L0 = tuple_to_list(S0),
    S1 = list_to_tuple(L0 ++ [#{}]),
    #?MODULE{} = S1,
    {ok, OldState, S1}.

callback_mode() -> [state_functions, state_enter].

-spec inject_err(err_scenario(), [err_type()], #?MODULE{}) -> {error, err_type()} | ok.
inject_err(Scenario, AllowTypes, #?MODULE{err = E, name = Name, slotidx = Slot}) ->
    {PErr, Types} = maps:get(Scenario, E, {0, []}),
    R = rand:uniform(100),
    if
        (R =< PErr) ->
            PickTypes = list_to_tuple(sets:to_list(sets:intersection(
                sets:from_list(AllowTypes),
                sets:from_list(Types)))),
            Idx = rand:uniform(tuple_size(PickTypes)),
            Type = element(Idx, PickTypes),
            lager:debug("[~s/~B] !! injecting error: ~p => ~p", [Name, Slot,
                Scenario, Type]),
            {error, Type};
        true ->
            ok
    end.

cancel_waiter(S0 = #?MODULE{card_req = undefined}) ->
    S0;
cancel_waiter(S0 = #?MODULE{idle_to = PrevState, last_cmd = X, waiter = From}) ->
    IccState = case PrevState of
        pwr_off -> inactive;
        pwr_on -> active;
        _ -> not_present
    end,
    Reply = ccid:error_resp(X, #ccid_err{icc = IccState,
                                         cmd = failed,
                                         error = ?CCID_CMD_ABORTED}),
    gen_statem:reply(From, Reply),
    S0#?MODULE{card_req = undefined, waiter = undefined}.

busy(enter, PrevState, S0 = #?MODULE{idle_to = undefined}) ->
    {keep_state, S0#?MODULE{idle_to = PrevState},
     [{state_timeout, 4000, time_ext}]};
busy(enter, _PrevState, #?MODULE{}) ->
    {keep_state_and_data, [{state_timeout, 4000, time_ext}]};

busy(state_timeout, time_ext, #?MODULE{card_req = undefined}) ->
    keep_state_and_data;
busy(state_timeout, time_ext, #?MODULE{sup = Sup, last_cmd = X,
                                       slotidx = Slot}) ->
    Reply = ccid:error_resp(X, #ccid_err{icc = active, cmd = time_ext,
                                         error = 1}),
    gen_server:cast(Sup, {interim_reply, self(), Slot, Reply}),
    {keep_state_and_data, [{state_timeout, 4000, time_ext}]};

busy(info, {'DOWN', MRef, process, Card, _Why},
                                    S0 = #?MODULE{card = Card, cmref = MRef,
                                                  card_req = undefined}) ->
    #?MODULE{name = Name, slotidx = SlotIdx} = S0,
    {ok, NewCard} = ccid_card_fsm:open(Name, SlotIdx),
    NewMRef = erlang:monitor(process, NewCard),
    ok = gen_statem:call(NewCard, reset),
    {keep_state, S0#?MODULE{card = NewCard, cmref = NewMRef}};
busy(info, {'DOWN', MRef, process, Card, _Why},
                                    S0 = #?MODULE{card = Card, cmref = MRef,
                                                  waiter = From, last_cmd = X,
                                                  idle_to = RetState}) ->
    #?MODULE{name = Name, slotidx = SlotIdx} = S0,
    {ok, NewCard} = ccid_card_fsm:open(Name, SlotIdx),
    NewMRef = erlang:monitor(process, NewCard),
    ok = gen_statem:call(NewCard, reset),
    S1 = S0#?MODULE{card = NewCard, cmref = NewMRef},
    IccState = case RetState of
        pwr_off -> inactive;
        pwr_on -> active;
        _ -> not_present
    end,
    #?MODULE{name = Name, slotidx = Slot} = S0,
    lager:debug("[~s/~B] killed command due to card crash", [Name, Slot]),
    Resp = ccid:error_resp(X, #ccid_err{icc = IccState,
                                        cmd = failed,
                                        error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    {keep_state, S1};
busy(info, _Msg, #?MODULE{card_req = undefined}) ->
    keep_state_and_data;
busy(info, Msg, S0 = #?MODULE{card_req = ReqId, waiter = From}) ->
    case gen_statem:check_response(Msg, ReqId) of
        {reply, RAPDU = #apdu_reply{}} ->
            #?MODULE{name = Name, slotidx = Slot, last_cmd = X} = S0,
            #ccid_pc_to_rdr_xfrblock{slot = Slot, seq = Seq} = X,
            lager:debug("[~s/~B] APDU << ~s", [Name, Slot,
                ccid:pretty_print(RAPDU)]),
            RepData = iso7816:encode_apdu_reply(RAPDU),
            Resp = #ccid_rdr_to_pc_datablock{slot = Slot,
                                             seq = Seq,
                                             data = RepData},
            gen_statem:reply(From, Resp),
            {keep_state, S0#?MODULE{card_req = undefined, waiter = undefined}};
        no_reply ->
            keep_state_and_data
    end;

busy({call, From}, {finish_read, Seq}, S0 = #?MODULE{card_req = undefined,
                                                     last_cmd = Cmd,
                                                     slotidx = Slot}) ->
    case ccid:slot_seq(Cmd) of
        {Slot, Seq} ->
            #?MODULE{idle_to = PrevState} = S0,
            gen_statem:reply(From, ok),
            {next_state, PrevState, S0#?MODULE{idle_to = undefined}};
        {Slot, _OtherSeq} ->
            gen_statem:reply(From, ok),
            keep_state_and_data
    end;
busy({call, From}, {finish_read, _}, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;

busy({call, From}, stall_clear, S0 = #?MODULE{idle_to = PrevState}) ->
    gen_statem:reply(From, ok),
    S1 = cancel_waiter(S0#?MODULE{idle_to = undefined}),
    {next_state, PrevState, S1};

busy({call, From}, reset, S0 = #?MODULE{card = Card, idle_to = NewState}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    S1 = cancel_waiter(S0#?MODULE{idle_to = undefined}),
    {next_state, NewState, S1};
busy({call, From}, {abort, Seq}, S0 = #?MODULE{idle_to = NewState}) ->
    gen_statem:reply(From, ok),
    S1 = cancel_waiter(S0#?MODULE{idle_to = undefined}),
    {next_state, wait_abort, S1#?MODULE{abort_seq = Seq,
                                        abort_to = NewState}};
busy({call, _From}, Cmd, S0 = #?MODULE{}) when is_atom(Cmd) ->
    {keep_state, S0, [postpone]};
busy({call, _From}, {config_err, _, _}, S0 = #?MODULE{}) ->
    {keep_state, S0, [postpone]};
busy({call, From}, Cmd, S0 = #?MODULE{idle_to = RetState}) ->
    IccState = case RetState of
        pwr_off -> inactive;
        pwr_on -> active;
        _ -> not_present
    end,
    #?MODULE{name = Name, slotidx = Slot} = S0,
    lager:debug("[~s/~B] new command while busy!", [Name, Slot]),
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = IccState,
                                          cmd = failed,
                                          error = ?CCID_CMD_SLOT_BUSY}),
    gen_statem:reply(From, Resp),
    % don't update last_cmd, we use that!
    {keep_state, S0}.

empty(enter, _PrevState, S0 = #?MODULE{sup = Sup, name = Name, slotidx = Slot}) ->
    gen_server:cast(Sup, {slot_presence, self(), Slot, not_present}),
    lager:debug("[~s/~B] empty", [Name, Slot]),
    {keep_state, S0};

empty(info, {'DOWN', MRef, process, Card, _Why},
                                    S0 = #?MODULE{card = Card, cmref = MRef}) ->
    #?MODULE{name = Name, slotidx = SlotIdx} = S0,
    {ok, NewCard} = ccid_card_fsm:open(Name, SlotIdx),
    NewMRef = erlang:monitor(process, NewCard),
    ok = gen_statem:call(NewCard, reset),
    {keep_state, S0#?MODULE{card = NewCard, cmref = NewMRef}};

empty(info, _, #?MODULE{}) ->
    keep_state_and_data;

empty({call, From}, {finish_read, _Seq}, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
empty({call, From}, stall_clear, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
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
empty({call, From}, {config_err, Scenario, Config}, S0 = #?MODULE{err = E0}) ->
    E1 = E0#{Scenario => Config},
    gen_statem:reply(From, ok),
    {keep_state, S0#?MODULE{err = E1}};

empty({call, From}, {abort, Seq}, #?MODULE{last_cmd = #ccid_pc_to_rdr_abort{seq = Seq},
                                           aborter = AbortFrom, slotidx = Slot}) ->
    gen_statem:reply(From, ok),
    Msg = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                     err = #ccid_err{icc = not_present},
                                     clock = stopped},
    gen_statem:reply(AbortFrom, Msg),
    keep_state_and_data;
empty({call, From}, {abort, Seq}, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, wait_abort, S0#?MODULE{abort_seq = Seq}};
empty({call, From}, X = #ccid_pc_to_rdr_abort{}, S0 = #?MODULE{}) ->
    {keep_state, S0#?MODULE{last_cmd = X, aborter = From}};

empty({call, From}, X = #ccid_pc_to_rdr_getslotstatus{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = not_present},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
empty({call, From}, Cmd = #ccid_pc_to_rdr_xfrblock{}, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = not_present,
                                          cmd = failed,
                                          error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd}};
empty({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweron{}, S0 = #?MODULE{}) ->
    timer:sleep(1000),
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = not_present,
                                          cmd = failed,
                                          error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd}};
empty({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = not_present},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd}};

empty({call, From}, X = #ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = not_present},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
empty({call, From}, X = #ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = not_present},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
empty({call, From}, X = #ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = not_present,
                                                  cmd = failed,
                                                  error = 10},
                                  params = none},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};

empty({call, From}, Cmd, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = not_present,
                                          cmd = failed,
                                          error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd}}.


pwr_off(enter, _PrevState, #?MODULE{name = Name, slotidx = Slot, sup = Sup}) ->
    gen_server:cast(Sup, {slot_presence, self(), Slot, present}),
    lager:debug("[~s/~B] powered off", [Name, Slot]),
    keep_state_and_data;

pwr_off(info, {'DOWN', MRef, process, Card, _Why},
                                    S0 = #?MODULE{card = Card, cmref = MRef}) ->
    #?MODULE{name = Name, slotidx = SlotIdx} = S0,
    {ok, NewCard} = ccid_card_fsm:open(Name, SlotIdx),
    NewMRef = erlang:monitor(process, NewCard),
    ok = gen_statem:call(NewCard, reset),
    {keep_state, S0#?MODULE{card = NewCard, cmref = NewMRef}};
pwr_off(info, _, #?MODULE{}) ->
    keep_state_and_data;

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
pwr_off({call, From}, {config_err, Scenario, Config}, S0 = #?MODULE{err = E0}) ->
    E1 = E0#{Scenario => Config},
    gen_statem:reply(From, ok),
    {keep_state, S0#?MODULE{err = E1}};
pwr_off({call, From}, {finish_read, _Seq}, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
pwr_off({call, From}, stall_clear, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;

pwr_off({call, From}, {abort, Seq}, #?MODULE{last_cmd = #ccid_pc_to_rdr_abort{seq = Seq},
                                             aborter = AbortFrom,
                                             name = Name, slotidx = Slot}) ->
    gen_statem:reply(From, ok),
    lager:debug("[~s/~B] using stashed abort cmd", [Name, Slot]),
    Msg = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                     err = #ccid_err{icc = inactive},
                                     clock = stopped},
    gen_statem:reply(AbortFrom, Msg),
    keep_state_and_data;
pwr_off({call, From}, {abort, Seq}, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, wait_abort, S0#?MODULE{abort_seq = Seq}};
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
    {next_state, busy, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, X = #ccid_pc_to_rdr_xfrblock{data = <<>>},
                                S0 = #?MODULE{name = Name, slotidx = Slot}) ->
    Resp = ccid:error_resp(X, #ccid_err{icc = active, cmd = failed,
                                        error = ?CCID_LENGTH_INVALID}),
    lager:debug("[~s/~B] sent zero-length xfrblock", [Name, Slot]),
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, Cmd = #ccid_pc_to_rdr_xfrblock{}, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = inactive,
                                          cmd = failed,
                                          error = ?CCID_ICC_MUTE}),
    timer:sleep(500),
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd}};
pwr_off({call, From}, X = #ccid_pc_to_rdr_iccpoweron{slot = Slot, seq = Seq},
                                                    S0 = #?MODULE{card = C}) ->
    case inject_err(power_on, [long_sleep, hw_error, parity], S0) of
        {error, long_sleep} ->
            timer:sleep(30000),
            Resp = ccid:error_resp(X, #ccid_err{icc = inactive,
                                                  cmd = failed,
                                                  error = ?CCID_ICC_MUTE}),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X}};
        {error, hw_error} ->
            timer:sleep(100),
            Resp = ccid:error_resp(X, #ccid_err{icc = inactive,
                                                  cmd = failed,
                                                  error = ?CCID_HW_ERROR}),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X}};
        {error, parity} ->
            timer:sleep(500),
            Resp = ccid:error_resp(X, #ccid_err{icc = inactive,
                                                  cmd = failed,
                                                  error = ?CCID_XFR_PARITY_ERR}),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X}};
        ok ->
            {ok, ATR} = gen_statem:call(C, get_atr),
            Resp = #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, data = ATR},
            timer:sleep(500),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X, idle_to = pwr_on}}
    end;
pwr_off({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = inactive},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd}};

pwr_off({call, From}, X = #ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = inactive},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, X = #ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = inactive},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
pwr_off({call, From}, X = #ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = inactive,
                                                  cmd = failed,
                                                  error = 10},
                                  params = none},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};

pwr_off({call, From}, Cmd, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = inactive,
                                          cmd = failed,
                                          error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd}}.

wait_abort(enter, PrevState, S0 = #?MODULE{name = Name, slotidx = Slot,
                                           abort_to = undefined}) ->
    lager:debug("[~s/~B] awaiting abort cmd (return to ~p)", [Name, Slot,
        PrevState]),
    {keep_state, S0#?MODULE{abort_to = PrevState}};
wait_abort(enter, PrevState, #?MODULE{name = Name, slotidx = Slot,
                                      abort_to = FinalState}) ->
    lager:debug("[~s/~B] awaiting abort cmd (return to ~p, from ~p)",
        [Name, Slot, FinalState, PrevState]),
    keep_state_and_data;
wait_abort({call, From}, reset, S0 = #?MODULE{abort_to = State, card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    {next_state, State, S0#?MODULE{abort_to = undefined}};
wait_abort({call, From}, stall_clear, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
wait_abort({call, _From}, Cmd, #?MODULE{}) when is_atom(Cmd) ->
    {keep_state_and_data, [postpone]};
wait_abort({call, _From}, {config_err, _, _}, #?MODULE{}) ->
    {keep_state_and_data, [postpone]};
wait_abort({call, From}, {abort, NewSeq}, S0 = #?MODULE{name = Name,
                                                        slotidx = Slot}) ->
    gen_statem:reply(From, ok),
    lager:debug("[~s/~B] aborting to seq ~B now instead", [Name, Slot, NewSeq]),
    {keep_state, S0#?MODULE{abort_seq = NewSeq}};
wait_abort({call, From}, X = #ccid_pc_to_rdr_abort{slot = Slot, seq = Seq},
                                            S0 = #?MODULE{abort_to = State,
                                                          abort_seq = Seq}) ->
    {IccState, Clock} = case State of
        pwr_off -> {inactive, stopped};
        pwr_on -> {active, running};
        _ -> {not_present, stopped}
    end,
    Msg = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                     err = #ccid_err{icc = IccState},
                                     clock = Clock},
    gen_statem:reply(From, Msg),
    S1 = S0#?MODULE{last_cmd = X, abort_to = undefined, idle_to = State},
    {next_state, busy, S1};
wait_abort({call, From}, Cmd, S0 = #?MODULE{}) ->
    Resp = ccid:error_resp(Cmd, #ccid_err{icc = inactive,
                                          cmd = failed,
                                          error = ?CCID_CMD_ABORTED}),
    gen_statem:reply(From, Resp),
    {keep_state, S0#?MODULE{last_cmd = Cmd}};
wait_abort(info, _Msg, #?MODULE{}) ->
    {keep_state_and_data, [postpone]}.

pwr_on(enter, _PrevState, #?MODULE{name = Name, slotidx = Slot}) ->
    lager:debug("[~s/~B] powered on", [Name, Slot]),
    keep_state_and_data;

pwr_on({call, From}, reset, S0 = #?MODULE{card = Card}) ->
    ok = gen_statem:call(Card, reset),
    gen_statem:reply(From, ok),
    {next_state, pwr_off, S0};

pwr_on({call, From}, {abort, Seq}, #?MODULE{last_cmd = #ccid_pc_to_rdr_abort{seq = Seq},
                                            aborter = AbortFrom, slotidx = Slot}) ->
    gen_statem:reply(From, ok),
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
    ok = gen_statem:call(NewCard, reset),
    {next_state, pwr_off, S0#?MODULE{card = NewCard, cmref = NewMRef}};

pwr_on({call, From}, X = #ccid_pc_to_rdr_getslotstatus{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = active},
                                      clock = running},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};

pwr_on({call, From}, Cmd = #ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq,
                                      err = #ccid_err{icc = inactive},
                                      clock = stopped},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = Cmd, idle_to = pwr_off}};
pwr_on({call, From}, X = #ccid_pc_to_rdr_iccpoweron{slot = Slot, seq = Seq},
                                                    S0 = #?MODULE{card = C}) ->
    {ok, ATR} = gen_statem:call(C, get_atr),
    ok = gen_statem:call(C, reset),
    Resp = #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, data = ATR},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};

pwr_on({call, From}, X = #ccid_pc_to_rdr_xfrblock{data = <<>>},
                                S0 = #?MODULE{name = Name, slotidx = Slot}) ->
    Resp = ccid:error_resp(X, #ccid_err{icc = active, cmd = failed,
                                        error = ?CCID_LENGTH_INVALID}),
    lager:debug("[~s/~B] sent zero-length xfrblock", [Name, Slot]),
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};

pwr_on({call, From}, X = #ccid_pc_to_rdr_xfrblock{slot = Slot, seq = Seq},
                        S0 = #?MODULE{name = Name, slotidx = Slot, card = C}) ->
    #ccid_pc_to_rdr_xfrblock{data = APDUData, chain = one} = X,
    timer:sleep(5 + byte_size(APDUData) div 2),
    case inject_err(apdu, [long_sleep, hw_error, parity, ripped], S0) of
        {error, long_sleep} ->
            timer:sleep(30000),
            Resp = ccid:error_resp(X, #ccid_err{icc = active,
                                                cmd = failed,
                                                error = ?CCID_ICC_MUTE}),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X}};
        {error, hw_error} ->
            timer:sleep(100),
            Resp = ccid:error_resp(X, #ccid_err{icc = active,
                                                cmd = failed,
                                                error = ?CCID_HW_ERROR}),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X}};
        {error, parity} ->
            timer:sleep(100),
            Resp = ccid:error_resp(X, #ccid_err{icc = active,
                                                cmd = failed,
                                                error = ?CCID_XFR_PARITY_ERR}),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X}};
        {error, ripped} ->
            timer:sleep(500),
            Resp = ccid:error_resp(X, #ccid_err{icc = inactive,
                                                cmd = failed,
                                                error = ?CCID_ICC_MUTE}),
            gen_statem:reply(From, Resp),
            {next_state, busy, S0#?MODULE{last_cmd = X, idle_to = empty}};
        ok ->
            APDU = iso7816:decode_apdu_cmd(APDUData),
            lager:debug("[~s/~B] APDU >> ~s", [Name, Slot,
                ccid:pretty_print(APDU)]),
            case APDU of
                #apdu_cmd{} ->
                    ReqId = gen_statem:send_request(C, APDU),
                    {next_state, busy, S0#?MODULE{last_cmd = X,
                                                  card_req = ReqId,
                                                  waiter = From}};
                {error, Why} ->
                    lager:debug("apdu decode failed: ~p", [Why]),
                    RAPDU = #apdu_reply{sw = {error, general_failure}},
                    lager:debug("[~s/~B] APDU << ~s", [Name, Slot,
                        ccid:pretty_print(RAPDU)]),
                    RepData = iso7816:encode_apdu_reply(RAPDU),
                    Resp = #ccid_rdr_to_pc_datablock{slot = Slot,
                                                     seq = Seq,
                                                     data = RepData},
                    timer:sleep(5 + byte_size(RepData) div 4),
                    gen_statem:reply(From, Resp),
                    {next_state, busy, S0#?MODULE{last_cmd = X}}
            end
    end;

pwr_on({call, From}, X = #ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = active},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
pwr_on({call, From}, X = #ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = active},
                                  params = #ccid_t0_params{}},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};
pwr_on({call, From}, X = #ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq},
                                                            S0 = #?MODULE{}) ->
    Resp = #ccid_rdr_to_pc_params{slot = Slot, seq = Seq,
                                  err = #ccid_err{icc = active,
                                                  cmd = failed,
                                                  error = 10},
                                  params = none},
    gen_statem:reply(From, Resp),
    {next_state, busy, S0#?MODULE{last_cmd = X}};

pwr_on(info, Msg, S0 = #?MODULE{}) ->
    % fall through to pwr_off handling
    pwr_off(info, Msg, S0);

pwr_on({call, From}, X, S0 = #?MODULE{}) ->
    % fall through to pwr_off handling
    pwr_off({call, From}, X, S0).
