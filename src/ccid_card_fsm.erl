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

-module(ccid_card_fsm).

-behaviour(gen_statem).
-include("include/iso7816.hrl").

-compile([{parse_transform, lager_transform}]).

-export([pretty_print/1]).

-export([
    start_link/2,
    open/2
    ]).

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    root_applet/3,
    piv/3
    ]).

-define(NIST_RID, 16#A0, 0, 0, 3, 8).
-define(PIV_PIX, 0, 0, 16#10, 0).
-define(PIV_AID, ?NIST_RID, ?PIV_PIX).

-define(PIV_GETDATA_TAGMAP, #{
    16#5C => tag
}).

-define(PIV_APT_INVTAGMAP, #{
    apt => {16#61, [
        {aid, 16#4F},
        {alloc_auth, {16#79, #{
            aid => 16#4F
        }}},
        {app_label, 16#50},
        {uri, 16#5F50},
        {algos, [{16#AC, [
            {algo, [16#80]},
            {oid, 16#06}
        ]}]}
    ]},
    unknown => 16#7F66
}).

-define(PIV_CHUID_INVTAGMAP, #{
    value => {16#53, [
        {buffer_len, 16#EE},
        {fascn, 16#30},
        {org_id, 16#32},
        {duns, 16#33},
        {guid, 16#34},
        {expiry, 16#35},
        {chuuid, 16#36},
        {signature, 16#3E},
        {lrc, 16#FE}
    ]}
}).

-spec start_link(string(), integer()) -> {ok, pid()} | {error, term()}.
start_link(Name, SlotIdx) ->
    gen_statem:start_link(?MODULE, [Name, SlotIdx, self()], []).

-spec open(string(), integer()) -> {ok, pid()} | {error, term()}.
open(Name, SlotIdx) ->
    case ccid_card_db:lookup(Name, SlotIdx) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            case ccid_card_sup:start_child(Name, SlotIdx) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, already_registered} ->
                    open(Name, SlotIdx);
                Err ->
                    Err
            end
    end.

-record(?MODULE, {
    sup :: pid(),
    name :: string(),
    slotidx :: integer()
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
    case ccid_card_db:register(Name, SlotIdx) of
        ok ->
            lager:md([{vm_name, Name}, {slot_idx, SlotIdx}]),
            lager:debug("card fsm for ~p slot ~B (~p)", [Name, SlotIdx, Sup]),
            {ok, root_applet, #?MODULE{name = Name, slotidx = SlotIdx, sup = Sup}};
        {error, already_registered} ->
            {stop, already_registered};
        {error, Why} ->
            {stop, {register_failed, Why}}
    end.

terminate(_Why, _State, #?MODULE{}) ->
    ok.

callback_mode() -> [state_functions, state_enter].

root_applet(enter, _PrevState, #?MODULE{}) ->
    keep_state_and_data;
root_applet({call, From}, reset, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
root_applet({call, From}, get_atr, #?MODULE{}) ->
    gen_statem:reply(From,
        {ok, base64:decode(<<"O/0TAACBMf4VgHPAIcBXWXViaUtleUA=">>)}),
    keep_state_and_data;
root_applet({call, From},
        #apdu_cmd{cla = iso, ins = select, p1 = 4, p2 = 0, data = AID},
        S0 = #?MODULE{}) ->
    case AID of
        <<?PIV_AID, _/binary>> ->
            #?MODULE{name = Name, slotidx = SlotIdx} = S0,
            APT = #{
                apt => #{
                    aid => <<?PIV_PIX, 1:16/little>>,
                    alloc_auth => #{
                        aid => <<?NIST_RID>>
                    },
                    app_label => iolist_to_binary([
                        "COMP3301 ", Name, " slot ",
                        integer_to_list(SlotIdx)]),
                    uri => <<"https://my.uq.edu.au/programs-courses/"
                        "course.html?course_code=comp3301">>
                }
            },
            Data = iso7816:encode_ber_tlvs_map(APT, ?PIV_APT_INVTAGMAP),
            timer:sleep(100),
            gen_statem:reply(From, #apdu_reply{data = Data}),
            {next_state, piv, S0};
        _ ->
            Reply = #apdu_reply{sw = {error, {file, not_found}}},
            gen_statem:reply(From, Reply),
            keep_state_and_data
    end;
root_applet({call, From}, #apdu_cmd{}, #?MODULE{}) ->
    Reply = #apdu_reply{sw = {error, func_not_supported}},
    gen_statem:reply(From, Reply),
    keep_state_and_data.

piv(enter, _PrevState, #?MODULE{}) ->
    keep_state_and_data;
piv({call, From}, reset, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, root_applet, S0};
piv({call, From}, get_atr, S0 = #?MODULE{}) ->
    root_applet({call, From}, get_atr, S0);
piv({call, From}, Cmd = #apdu_cmd{cla = iso, ins = select, p1 = 4, p2 = 0},
                                                        S0 = #?MODULE{}) ->
    root_applet({call, From}, Cmd, S0);
piv({call, From},
        #apdu_cmd{cla = iso, ins = get_data, p1 = 16#3F, p2 = 16#FF,
                  data = Data},
        S0 = #?MODULE{}) ->
    {ok, Tags} = iso7816:decode_ber_tlvs_map(Data, ?PIV_GETDATA_TAGMAP),
    Reply = case Tags of
        #{tag := <<16#5F, 16#C1, 16#02>>} ->
            #?MODULE{name = Name, slotidx = SlotIdx} = S0,
            Hash = crypto:hash(sha256, [Name, <<SlotIdx>>]),
            Chuid = #{
                value => #{guid => binary:part(Hash, 0, 16)}
            },
            OutData = iso7816:encode_ber_tlvs_map(Chuid, ?PIV_CHUID_INVTAGMAP),
            #apdu_reply{data = OutData};
        _ ->
            #apdu_reply{sw = {error, {file, not_found}}}
    end,
    gen_statem:reply(From, Reply),
    keep_state_and_data;
piv({call, From}, #apdu_cmd{cla = iso, ins = 16#FD, p1 = 0, p2 = 0},
                                                            #?MODULE{}) ->
    gen_statem:reply(From, #apdu_reply{data = <<33, 0, 1>>}),
    keep_state_and_data;
piv({call, From}, #apdu_cmd{}, #?MODULE{}) ->
    Reply = #apdu_reply{sw = {error, func_not_supported}},
    gen_statem:reply(From, Reply),
    keep_state_and_data.
