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
    pwr_on/3
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
    cmd :: undefined | #ccid_bulk{}
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
    {ok, pwr_off, #?MODULE{name = Name, slotidx = SlotIdx, sup = Sup}}.

terminate(_Why, _State, #?MODULE{}) ->
    ok.

callback_mode() -> [state_functions, state_enter].

empty(enter, _PrevState, #?MODULE{}) ->
    lager:debug("empty"),
    keep_state_and_data;
empty({call, From}, insert_card, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, pwr_off, S0};
empty({call, From}, remove_card, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
empty({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_GetSlotStatus},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = not_present}, <<1>>, <<>>),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
empty({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_Escape},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = not_present,
                                            cmd = failed,
                                            error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
empty({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_XfrBlock},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = not_present,
                                            cmd = failed,
                                            error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
empty({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_Mechanical},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = not_present,
                                            cmd = failed,
                                            error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
empty({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_Secure},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = not_present,
                                            cmd = failed,
                                            error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
empty({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_SetParams},
                                                            S0 = #?MODULE{}) ->
    % always say the first param is not changeable
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = not_present,
                                            cmd = failed,
                                            error = 10}),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
empty({call, From}, X = #ccid_bulk{}, S0 = #?MODULE{}) ->
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = not_present,
                                            cmd = failed,
                                            error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    keep_state_and_data.

pwr_off(enter, _PrevState, #?MODULE{}) ->
    lager:debug("powered off"),
    keep_state_and_data;

pwr_off({call, From}, insert_card, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
pwr_off({call, From}, remove_card, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, empty, S0};

pwr_off({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_GetSlotStatus},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = inactive}, <<1>>, <<>>),
    gen_statem:reply(From, Resp),
    keep_state_and_data;

pwr_off({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_IccPowerOff},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = inactive}, <<1>>, <<>>),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
pwr_off({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_IccPowerOn},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = active}, <<0>>,
        base64:decode(<<"O/0TAACBMf4VgHPAIcBXWXViaUtleUA=">>)),
    gen_statem:reply(From, Resp),
    {next_state, pwr_on, S0};

pwr_off({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_XfrBlock},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = inactive,
                                            cmd = failed,
                                            error = ?CCID_ICC_MUTE}),
    gen_statem:reply(From, Resp),
    keep_state_and_data;

pwr_off({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_SetParams},
                                                            S0 = #?MODULE{}) ->
    % always say the first param is not changeable
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = inactive,
                                            cmd = failed,
                                            error = 10}),
    gen_statem:reply(From, Resp),
    keep_state_and_data;

pwr_off({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_GetParams},
                                                            S0 = #?MODULE{}) ->
    Params = <<0>>, % T=0
    Fidx = 1, % Fi = 372, f(max) = 5MHz
    Didx = 1, % Di = 1,
    RespData = <<Fidx:4, Didx:4,    % bmFindexDindex
                 0:6, 0:1, 0:1,     % bmTCCKST0 (direct convention)
                 0,                 % bGuardTimeT0
                 0,                 % bWaitingIntegerT0
                 3>>,               % bClockStop = either high or low
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = active}, Params, RespData),
    gen_statem:reply(From, Resp),
    keep_state_and_data;
pwr_off({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_ResetParams},
                                                            S0 = #?MODULE{}) ->
    pwr_off({call, From}, X#ccid_bulk{type = ?CCID_PC_to_RDR_GetParams}, S0);

pwr_off({call, From}, X = #ccid_bulk{}, S0 = #?MODULE{}) ->
    Resp = ccid_fsm:error_resp(X, #ccid_err{icc = inactive,
                                            cmd = failed,
                                            error = ?CCID_CMD_UNSUPPORTED}),
    gen_statem:reply(From, Resp),
    keep_state_and_data.

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

pwr_on(enter, _PrevState, #?MODULE{}) ->
    lager:debug("powered on"),
    keep_state_and_data;

pwr_on({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_GetSlotStatus},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = active}, <<0>>, <<>>),
    gen_statem:reply(From, Resp),
    keep_state_and_data;

pwr_on({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_IccPowerOff},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = inactive}, <<1>>, <<>>),
    gen_statem:reply(From, Resp),
    {next_state, pwr_off, S0};
pwr_on({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_IccPowerOn},
                                                            S0 = #?MODULE{}) ->
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = active}, <<0>>,
        base64:decode(<<"O/0TAACBMf4VgHPAIcBXWXViaUtleUA=">>)),
    gen_statem:reply(From, Resp),
    keep_state_and_data;

pwr_on({call, From}, X = #ccid_bulk{type = ?CCID_PC_to_RDR_XfrBlock},
                                                            S0 = #?MODULE{}) ->
    #ccid_bulk{params = Params, data = CmdData} = X,
    <<BWI, Level:16/little>> = Params,
    0 = Level,
    APDU = iso7816:decode_apdu_cmd(CmdData),
    lager:debug(">> ~p", [APDU]),
    RAPDU = case APDU of
        #apdu_cmd{cla = iso, ins = select, p1 = 4, p2 = 0, data = AID} ->
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
                    #apdu_reply{data = Data};
                _ ->
                    #apdu_reply{sw = {error, {file, not_found}}}
            end;
        #apdu_cmd{cla = iso, ins = get_data, p1 = 16#3F, p2 = 16#FF,
                  data = Data} ->
            {ok, Tags} = iso7816:decode_ber_tlvs_map(Data, ?PIV_GETDATA_TAGMAP),
            case Tags of
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
            end;
        #apdu_cmd{cla = iso, ins = 16#FD, p1 = 0, p2 = 0} ->
            #apdu_reply{data = <<33, 0, 1>>};
        #apdu_cmd{} ->
            #apdu_reply{sw = {error, func_not_supported}};
        {error, Why} ->
            lager:debug("apdu decode failed: ~p", [Why]),
            #apdu_reply{sw = {error, general_failure}}
    end,
    lager:debug("<< ~p", [RAPDU]),
    RepData = iso7816:encode_apdu_reply(RAPDU),
    Resp = ccid_fsm:data_resp(X, #ccid_err{icc = active}, <<0>>, RepData),
    gen_statem:reply(From, Resp),
    keep_state_and_data;

pwr_on({call, From}, X = #ccid_bulk{}, S0 = #?MODULE{}) ->
    % fall through to pwr_off handling
    pwr_off({call, From}, X, S0).
