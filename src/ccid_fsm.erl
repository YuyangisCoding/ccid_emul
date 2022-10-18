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

-module(ccid_fsm).

-behaviour(gen_server).
-include("include/records.hrl").
-include("include/usb.hrl").

-vsn({1,0}).

-compile([{parse_transform, lager_transform}]).

-export([pretty_print/1]).

-export([
    start_link/1,
    open/1,
    take_control/1,
    get_slot/2,
    force_reset/1
    ]).

-export([
    init/1,
    terminate/2,
    code_change/3,
    handle_call/3,
    handle_info/2,
    handle_cast/2
    ]).

-spec start_link(string()) -> {ok, pid()} | {error, term()}.
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

-spec open(string()) -> {ok, pid()} | {error, term()}.
open(Name) ->
    case ccid_fsm_db:lookup(Name) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            case ccid_fsm_sup:start_child(Name) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, already_registered} ->
                    open(Name);
                Err ->
                    Err
            end
    end.

-spec take_control(pid()) -> ok | {error, term()}.
take_control(Pid) ->
    gen_server:call(Pid, {set_vm_conn, self()}, infinity).

-spec get_slot(pid(), integer()) -> {ok, pid()} | {error, term()}.
get_slot(Pid, N) ->
    gen_server:call(Pid, {get_slot, N}, infinity).

-spec force_reset(pid()) -> ok | {error, term()}.
force_reset(Pid) ->
    case gen_server:call(Pid, #urelay_reset{}, infinity) of
        #urelay_status{errno = 0} -> ok;
        #urelay_status{errno = Eno} -> {error, {errno, Eno}};
        Other -> Other
    end.

-type slot() :: integer().
-type seq() :: integer().

-record(?MODULE, {
    name :: string(),
    slots = #{} :: #{slot() => pid()},
    slast = #{} :: #{slot() => present | not_present},
    nslots = 0 :: integer(),
    reqs = gen_statem:reqids_new() :: gen_statem:request_id_collection(),
    conn_fsm :: undefined | pid(),
    mref :: undefined | reference(),

    % usb vendor/product id
    uvid = 16#3301 :: integer(),
    upid = 16#0010 :: integer(),

    % currently active config #
    actcfg = 1 :: integer(),

    % endpoint numbers
    epin = 2 :: integer(),
    epout = 1 :: integer(),
    epint = 3 :: integer(),

    % bulk-out buffer (commands from host)
    cmdlen :: undefined | integer(),
    cmdbuf = [] :: [binary()],

    % bulk-in buffer (responses to host)
    rq = [] :: [{slot(), seq(), binary()}],
    rslot = undefined :: undefined | slot(),
    rseq = undefined :: undefined | seq(),
    rbuf = <<>> :: binary(),
    rpos = 0 :: integer(),

    % interrupts
    iq = [] :: [binary()],

    rwait = false :: boolean(),     % bulk-in transfer waiting
    iwait = false :: boolean(),     % interrupt transfer waiting

    % ccid seq
    seq :: undefined | integer()
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

init([Name]) ->
    case ccid_fsm_db:register(Name) of
        ok ->
            <<UPid:16/little>> = crypto:strong_rand_bytes(2),
            S0 = #?MODULE{name = Name, upid = UPid bor 16#4000},
            lager:md([{vm_name, Name}]),
            S1 = case crypto:hash(sha256, Name) of
                <<_, 0:2, _/bitstring>> ->
                    S0#?MODULE{epin = 1, epout = 2, epint = 3};
                <<_, 1:2, _/bitstring>> ->
                    S0#?MODULE{epin = 2, epout = 1, epint = 3};
                <<_, 2:2, _/bitstring>> ->
                    S0#?MODULE{epin = 2, epout = 2, epint = 1};
                <<_, 3:2, _/bitstring>> ->
                    S0#?MODULE{epin = 1, epout = 1, epint = 2}
            end,
            <<_:5, NSlots0:3, _/bitstring>> = crypto:hash(sha256, Name),
            NSlots = lists:min([NSlots0 + 2, 5]),
            {ok, start_slots(NSlots - 1, S1#?MODULE{nslots = NSlots})};
        {error, already_registered} ->
            {stop, already_registered};
        {error, Why} ->
            {stop, {register_failed, Why}}
    end.

start_slots(-1, S0 = #?MODULE{}) -> S0;
start_slots(N, S0 = #?MODULE{name = Name, slots = Slots0}) ->
    {ok, Slot} = ccid_slot_fsm:start_link(Name, N, self()),
    Slots1 = Slots0#{N => Slot},
    start_slots(N - 1, S0#?MODULE{slots = Slots1}).

terminate(_Why, #?MODULE{}) ->
    ok.

code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.

ctrl_reply(#urelay_ctrl{length = Len}, RC) ->
    #urelay_ctrl_resp{rc = RC, blen = Len, bdone = 0}.

ctrl_reply_data(#urelay_ctrl{length = Len}, Data) ->
    if
        (byte_size(Data) < Len) ->
            #urelay_ctrl_resp{rc = ?USB_ERR_SHORT_XFER,
                              blen = Len - byte_size(Data),
                              bdone = byte_size(Data),
                              data = Data};
        (byte_size(Data) >= Len) ->
            #urelay_ctrl_resp{rc = ?USB_ERR_NORMAL_COMPLETION,
                              blen = 0,
                              bdone = Len,
                              data = binary:part(Data, 0, Len)}
    end.

handle_call({get_slot, N}, _From, S0 = #?MODULE{slots = Slots}) ->
    case Slots of
        #{N := Pid} -> {reply, {ok, Pid}, S0};
        _ -> {reply, {error, not_found}, S0}
    end;

handle_call(#urelay_reset{}, From, S0 = #?MODULE{name = Name, slots = Slots0}) ->
    lager:debug("[~s] reset", [Name]),
    maps:foreach(fun (_Number, Pid) ->
        ok = gen_statem:call(Pid, reset),
        {Pid, idle}
    end, Slots0),
    S1 = S0#?MODULE{reqs = gen_statem:reqids_new(),
                    cmdlen = undefined,
                    cmdbuf = [],
                    rq = [],
                    rbuf = <<>>,
                    rpos = 0,
                    rslot = undefined,
                    rseq = undefined,
                    rwait = false,
                    iwait = false,
                    iq = []},
    gen_server:reply(From, #urelay_status{errno = 0}),
    {noreply, S1};

handle_call(X = #urelay_ctrl{}, From, S0 = #?MODULE{name = Name}) ->
    #urelay_ctrl{req = Req, req_type = ReqType, value = Val, index = Idx} = X,
    {Resp, S1} = case {ReqType, Req} of
        {?UT_READ_DEVICE, ?UR_GET_DESCRIPTOR} ->
            case (Val bsr 8) of
                ?UDESC_DEVICE ->
                    #?MODULE{uvid = Vid, upid = Pid} = S0,
                    D0 = udescr:pack([
                        #usb_device_descr{
                            bcdUSB = {2, 0},
                            bMaxPacketSize = 8,
                            idVendor = Vid,
                            idProduct = Pid,
                            bcdDevice = 16#00,
                            iManufacturer = 1,
                            iProduct = 2,
                            iSerialNumber = 3,
                            bNumConfigurations = 1
                        }
                    ]),
                    {ctrl_reply_data(X, D0), S0};
                ?UDESC_CONFIG ->
                    #?MODULE{epin = EpIn, epout = EpOut, epint = EpInt,
                             nslots = NSlots} = S0,
                    Eps = [{EpIn, in}, {EpOut, out}, {EpInt, int}],
                    EpDescrs = lists:map(fun
                        ({N, in}) ->
                            #usb_endpoint_descr{
                                bEndpointAddress = {in, N},
                                bmAttributes = bulk,
                                wMaxPacketSize = 64,
                                bInterval = 0
                            };
                        ({N, out}) ->
                            #usb_endpoint_descr{
                                bEndpointAddress = {out, N},
                                bmAttributes = bulk,
                                wMaxPacketSize = 64,
                                bInterval = 0
                            };
                        ({N, int}) ->
                            #usb_endpoint_descr{
                                bEndpointAddress = {in, N},
                                bmAttributes = interrupt,
                                wMaxPacketSize = 8,
                                bInterval = 32
                            }
                    end, lists:sort(Eps)),
                    D0 = udescr:pack([
                        #usb_config_descr{
                            bNumInterface = 1,
                            bConfigurationValue = 1,
                            iConfiguration = 4,
                            bmAttributes = [bus_powered, remote_wakeup],
                            bMaxPower = 0
                        },
                        #usb_intf_descr{
                            bInterfaceNumber = 0,
                            bAlternateSetting = 0,
                            bNumEndpoints = length(EpDescrs),
                            bInterfaceClass = 16#0B,
                            bInterfaceSubClass = 16#00,
                            bInterfaceProtocol = 16#00,
                            iInterface = 5
                        },
                        #usb_ccid_descr{
                            bcdCCID = {1, 10},
                            bMaxSlotIndex = NSlots - 1,
                            bVoltageSupport = ['5v', '3v', '1.8v'],
                            dwProtocols = [t1, t0],
                            dwDefaultClock = 3580,
                            dwMaximumClock = 3580,
                            bNumClockSupported = 0,
                            dwDataRate = 9600,
                            dwMaxDataRate = 115200,
                            bNumDataRatesSupported = 0,
                            dwMaxIFSD = 255,
                            dwSynchProtocols = [],
                            dwMechanical = [],
                            dwFeatures = {short_apdu, [auto_atr, auto_voltage,
                                auto_clock, auto_baud, auto_ifsd, auto_pps]},
                            dwMaxCCIDMessageLength = 1034,
                            bClassGetResponse = 16#FF,
                            bClassEnvelope = 16#FF,
                            wLcdLayout = none,
                            bPINSupport = [],
                            bMaxCCIDBusySlots = NSlots
                        }
                    ] ++ EpDescrs),
                    {ctrl_reply_data(X, D0), S0};
                ?UDESC_STRING ->
                    StringIdx = Val band 16#FF,
                    StrVal = case StringIdx of
                        0 -> {raw, <<9, 4>>};
                        1 -> "COMP3301";
                        2 -> "CCID Simulator";
                        3 -> "01";
                        4 -> "Simulated CCID device for A2";
                        5 -> "CCID";
                        _ -> ""
                    end,
                    D0 = udescr:pack([
                        #usb_string_descr{string = StrVal}
                    ]),
                    {ctrl_reply_data(X, D0), S0};
                ?UDESC_BOS ->
                    D0 = udescr:pack([
                        #usb_bos_descr{
                            bNumDeviceCaps = 1
                        },
                        #usb_devcap_ss_descr{
                            bmAttributes            = 0,
                            wSpeedsSupported        = 16#08,
                            bFunctionalitySupport   = 3,
                            bU1DevExitLat           = 16#0a,
                            wU2DevExitLat           = 16#0020
                        }
                    ]),
                    {ctrl_reply_data(X, D0), S0};
                _ ->
                    {ctrl_reply(X, ?USB_ERR_STALLED), S0}
            end;

        {?UT_READ_DEVICE, ?UR_GET_CONFIG} ->
            {ctrl_reply_data(X, <<(S0#?MODULE.actcfg)>>), S0};
        {?UT_READ_INTERFACE, ?UR_GET_INTERFACE} ->
            {ctrl_reply_data(X, <<0>>), S0};

        {?UT_READ_DEVICE, ?UR_GET_STATUS} ->
            {ctrl_reply_data(X, <<0:16/big>>), S0};
        {?UT_READ_INTERFACE, ?UR_GET_STATUS} ->
            {ctrl_reply_data(X, <<0:16/big>>), S0};
        {?UT_READ_ENDPOINT, ?UR_GET_STATUS} ->
            {ctrl_reply_data(X, <<0:16/big>>), S0};

        {?UT_WRITE_ENDPOINT, ?UR_CLEAR_FEATURE} when (Val == ?UF_ENDPOINT_HALT) ->
            EpAddr = case <<Idx:8>> of
                <<0:1, 0:3, 0:4>> -> control;
                <<0:1, 0:3, Num:4>> -> {out, Num};
                <<1:1, 0:3, Num:4>> -> {in, Num}
            end,
            lager:debug("[~s] clearing stall on ~p", [Name, EpAddr]),
            SS1 = case {EpAddr, S0} of
                {control, _} -> S0;
                {{out, OutEp}, #?MODULE{epout = OutEp}} ->
                    % clearing a stall on the bulk-out
                    % we'll drop all our command buffers
                    S0#?MODULE{cmdlen = undefined, cmdbuf = []};
                {{in, InEp}, #?MODULE{epin = InEp, slots = Slots0}} ->
                    % clearing a stall on bulk-in
                    % drop all queued messages on the floor?
                    maps:foreach(fun (_SlotNum, Pid) ->
                        ok = gen_statem:call(Pid, stall_clear)
                    end, Slots0),
                    S0#?MODULE{rq = [],
                               rslot = undefined,
                               rseq = undefined,
                               rbuf = <<>>,
                               rpos = 0};
                {{in, IntEp}, #?MODULE{epint = IntEp}} ->
                    % clearing a stall on intr-in
                    S0#?MODULE{iq = []}
            end,
            {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION), SS1};

        {?UT_WRITE_DEVICE, ?UR_SET_CONFIG} ->
            lager:debug("[~s] set config ~B", [Name, Val]),
            case Val of
                1 ->
                    {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION),
                     S0#?MODULE{actcfg = 1}};
                _ ->
                    {ctrl_reply(X, ?USB_ERR_STALLED), S0}
            end;

        {?UT_WRITE_INTERFACE, ?UR_SET_INTERFACE} ->
            {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION), S0};

        {?UT_WRITE_DEVICE, ?UR_SET_DESCRIPTOR} ->
            {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION), S0};
        {?UT_WRITE_DEVICE, ?UR_SET_FEATURE} ->
            {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION), S0};
        {?UT_WRITE_DEVICE, ?UR_CLEAR_FEATURE} ->
            {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION), S0};

        {?UT_WRITE_CLASS_INTERFACE, ?CCID_CTRL_ABORT} ->
            Slot = Val band 16#FF,
            AbortSeq = (Val bsr 8) band 16#FF,
            #?MODULE{slots = Slots0} = S0,
            case Slots0 of
                #{Slot := Fsm} ->
                    lager:debug("[~s] ABORT: slot ~B, seq ~B",
                        [Name, Slot, AbortSeq]),
                    ok = gen_statem:call(Fsm, {abort, AbortSeq}),
                    SS1 = drop_slot_resps(Slot, S0),
                    {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION), SS1};
                _ ->
                    lager:debug("[~s] abort non-existent slot ~B",
                        [Name, Slot]),
                    {ctrl_reply(X, ?USB_ERR_STALLED), S0}
            end;
        {?UT_READ_CLASS_INTERFACE, ?CCID_CTRL_GET_CLOCK} ->
            lager:debug("[~s] reading clock speeds", [Name]),
            Data = <<3580:32/little>>,
            {ctrl_reply_data(X, Data), S0};
        {?UT_READ_CLASS_INTERFACE, ?CCID_CTRL_GET_BAUD} ->
            lager:debug("[~s] reading baud rates", [Name]),
            Data = <<9600:32/little, 115200:32/little>>,
            {ctrl_reply_data(X, Data), S0};

        {0, ?UR_SET_SEL} ->
            {ctrl_reply(X, ?USB_ERR_NORMAL_COMPLETION), S0};

        _ ->
            lager:debug("[~s] stalled due to unknown ctrl xfer: ~s", [Name,
                ccid:pretty_print(X)]),
            {ctrl_reply(X, ?USB_ERR_STALLED), S0}
    end,
    gen_statem:reply(From, Resp),
    {noreply, S1};

handle_call(X = #urelay_data{dir = ?URELAY_DIR_OUT, ep = EpOut}, From,
                            S0 = #?MODULE{cmdlen = undefined, epout = EpOut}) ->
    #urelay_data{remain = Rem, data = Data} = X,
    SS0 = S0#?MODULE{cmdbuf = [Data]},
    {Resp, S1} = case Data of
        <<_MsgType, Len:32/little, _Rest/binary>> when (Len =< 65536) ->
            SS1 = SS0#?MODULE{cmdlen = 10 + Len, cmdbuf = [Data]},
            {#urelay_data_resp{bdone = Rem}, SS1};
        _ ->
            #?MODULE{name = Name} = S0,
            lager:debug("[~s] stalled due to invalid header: ~p", [Name, Data]),
            {#urelay_data_resp{errcode = ?USB_STALL,
                               rc = ?USB_ERR_STALLED,
                               blen = Rem,
                               bdone = 0}, S0}
    end,
    gen_statem:reply(From, Resp),
    {noreply, check_cmd(S1)};

handle_call(X = #urelay_data{dir = ?URELAY_DIR_OUT, ep = EpOut}, From,
                                S0 = #?MODULE{cmdbuf = Buf0, epout = EpOut}) ->
    #urelay_data{remain = Rem, data = Data} = X,
    Buf1 = [Data | Buf0],
    S1 = S0#?MODULE{cmdbuf = Buf1},
    Resp = #urelay_data_resp{bdone = Rem},
    gen_statem:reply(From, Resp),
    {noreply, check_cmd(S1)};

handle_call(X = #urelay_data{dir = ?URELAY_DIR_IN, ep = EpIn}, From,
                                    S0 = #?MODULE{rbuf = <<>>, rq = [],
                                                  epin = EpIn}) ->
    #urelay_data{remain = Rem} = X,
    Resp = #urelay_data_resp{blen = Rem,
                             errcode = ?USB_NAK,
                             rc = ?USB_ERR_CANCELLED},
    S1 = S0#?MODULE{rwait = true},
    gen_statem:reply(From, Resp),
    {noreply, S1};

handle_call(X = #urelay_data{dir = ?URELAY_DIR_IN, ep = EpIn}, From,
                                    S0 = #?MODULE{rbuf = <<>>, rq = RQ0,
                                                  epin = EpIn}) ->
    [{Slot, Seq, Next} | RQ1] = RQ0,
    S1 = S0#?MODULE{rbuf = Next, rpos = 0, rq = RQ1, rslot = Slot, rseq = Seq},
    handle_call(X, From, S1);

handle_call(X = #urelay_data{dir = ?URELAY_DIR_IN, ep = EpIn}, From,
                        S0 = #?MODULE{rbuf = Buf, rpos = Pos0, epin = EpIn}) ->
    #urelay_data{remain = Rem} = X,
    ActualRem = byte_size(Buf) - Pos0,
    {RC, EC, Chunk} = if
        (Rem > ActualRem) ->
            {?USB_ERR_SHORT_XFER, ?USB_SHORT,
             binary:part(Buf, {Pos0, ActualRem})};
        (ActualRem >= Rem) ->
            {?USB_ERR_NORMAL_COMPLETION, ?USB_ACK,
             binary:part(Buf, {Pos0, Rem})}
    end,
    Resp = #urelay_data_resp{rc = RC,
                             errcode = EC,
                             bdone = byte_size(Chunk),
                             blen = Rem - byte_size(Chunk),
                             data = Chunk},
    Pos1 = Pos0 + byte_size(Chunk),
    S1 = if
        (Pos1 >= byte_size(Buf)) ->
            #?MODULE{rslot = RSlot, rseq = Seq, slots = Slots0} = S0,
            case maps:get(RSlot, Slots0, none) of
                none ->
                    % this should be an INVALID_SLOT response
                    #?MODULE{name = Name} = S0,
                    lager:debug("[~s] read from a non-existent slot??", [Name]),
                    S0#?MODULE{rpos = 0, rbuf = <<>>, rseq = undefined,
                               rslot = undefined};
                Fsm ->
                    ok = gen_statem:call(Fsm, {finish_read, Seq}),
                    S0#?MODULE{rpos = 0, rbuf = <<>>, rslot = undefined,
                               rseq = undefined}
            end;
        true ->
            S0#?MODULE{rpos = Pos1}
    end,
    gen_statem:reply(From, Resp),
    {noreply, S1};

handle_call(X = #urelay_data{dir = ?URELAY_DIR_IN, ep = EpIntr}, From,
                                    S0 = #?MODULE{epint = EpIntr, iq = []}) ->
    #urelay_data{remain = Rem} = X,
    Resp = #urelay_data_resp{blen = Rem,
                             errcode = ?USB_NAK,
                             rc = ?USB_ERR_CANCELLED},
    S1 = S0#?MODULE{iwait = true},
    gen_statem:reply(From, Resp),
    {noreply, S1};
handle_call(X = #urelay_data{dir = ?URELAY_DIR_IN, ep = EpIntr}, From,
                                    S0 = #?MODULE{epint = EpIntr, iq = IQ0}) ->
    #urelay_data{remain = Rem} = X,
    [Buf | IQ1] = IQ0,
    ActualRem = byte_size(Buf),
    {RC, EC, Chunk} = if
        (Rem > ActualRem) ->
            {?USB_ERR_SHORT_XFER, ?USB_SHORT,
             binary:part(Buf, {0, ActualRem})};
        (ActualRem >= Rem) ->
            {?USB_ERR_NORMAL_COMPLETION, ?USB_ACK,
             binary:part(Buf, {0, Rem})}
    end,
    Resp = #urelay_data_resp{rc = RC,
                             errcode = EC,
                             bdone = byte_size(Chunk),
                             blen = Rem - byte_size(Chunk),
                             data = Chunk},
    S1 = S0#?MODULE{iq = IQ1},
    gen_statem:reply(From, Resp),
    {noreply, S1};

handle_call(X = #urelay_data{}, From, S0 = #?MODULE{name = Name}) ->
    lager:debug("[~s] stalled due to unhandled xfer: ~s", [Name,
        ccid:pretty_print(X)]),
    #urelay_data{remain = Rem} = X,
    Resp = #urelay_data_resp{blen = Rem,
                             errcode = ?USB_STALL,
                             rc = ?USB_ERR_STALLED},
    gen_statem:reply(From, Resp),
    {noreply, S0};

handle_call({set_vm_conn, Pid}, From, S0 = #?MODULE{name = Name,
                                                    conn_fsm = OldPid,
                                                    mref = OldMRef}) ->
    case OldMRef of
        undefined -> ok;
        _ ->
            lager:debug("conn fsm ~p stealing control of ccid ~p from ~p",
                [Pid, Name, OldPid]),
            erlang:demonitor(OldMRef, [flush])
    end,
    MRef = erlang:monitor(process, Pid),
    S1 = S0#?MODULE{conn_fsm = Pid, mref = MRef},
    gen_server:reply(From, ok),
    {noreply, S1};

handle_call(_Msg, _From, _S0 = #?MODULE{}) ->
    error(no_call).

handle_info({'DOWN', MRef, process, _Pid, _Why}, S0 = #?MODULE{mref = MRef}) ->
    S1 = S0#?MODULE{conn_fsm = undefined, mref = undefined},
    {noreply, S1};

handle_info(Msg, S0 = #?MODULE{reqs = Reqs0, slots = Slots0}) ->
    case gen_statem:check_response(Msg, Reqs0, true) of
        {{reply, _Resp}, intr, Reqs1} ->
            % thanks for letting us know
            {noreply, S0#?MODULE{reqs = Reqs1}};
        {{reply, Resp}, Slot, Reqs1} ->
            #?MODULE{name = Name} = S0,
            {Slot, _Seq} = ccid:slot_seq(Resp),
            lager:debug("[~s/~B] << ~s", [Name, Slot, ccid:pretty_print(Resp)]),
            #{Slot := Fsm} = Slots0,
            true = is_pid(Fsm),
            S1 = S0#?MODULE{reqs = Reqs1},
            {noreply, send_bulk_resp(Resp, S1)};
        _ ->
            {noreply, S0}
    end.

handle_cast({slot_presence, Fsm, Slot, Pres},
                            S0 = #?MODULE{slots = Slots0, slast = Last0}) ->
    #?MODULE{name = Name} = S0,
    #{Slot := Fsm} = Slots0,
    case maps:get(Slot, Last0, not_present) of
        Pres ->
            {noreply, S0};
        _ ->
            lager:debug("[~s/~B] changing to ~p", [Name, Slot, Pres]),
            M0 = maps:map(fun (SlotNum, _) ->
                {maps:get(SlotNum, Last0, not_present), no_change}
            end, Slots0),
            M1 = M0#{Slot => {Pres, change}},
            Intr = #ccid_rdr_to_pc_notifyslotchange{slots = M1},
            lager:debug("[~s/~B] INTR << ~s",
                [Name, Slot, ccid:pretty_print(Intr)]),
            Last1 = Last0#{Slot => Pres},
            S1 = S0#?MODULE{slast = Last1},
            S2 = send_intr(Intr, S1),
            {noreply, S2}
    end;

handle_cast(_, #?MODULE{}) ->
    error(no_cast).

send_intr(Msg, S0 = #?MODULE{name = Name, epint = EpInt,
                             iwait = Waiting, iq = []}) ->
    Bin = ccid:encode_intr(Msg),
    S1 = if
        Waiting ->
            #?MODULE{conn_fsm = Fsm, reqs = Reqs0} = S0,
            lager:debug("[~s] sending intr for intr-in", [Name]),
            Cmd = #urelay_intr{dir = ?URELAY_DIR_IN, ep = EpInt},
            Reqs1 = gen_statem:send_request(Fsm, Cmd, intr, Reqs0),
            S0#?MODULE{reqs = Reqs1};
        true ->
            S0
    end,
    S1#?MODULE{iq = [Bin], iwait = false};
send_intr(Msg, S0 = #?MODULE{iq = IQ0}) ->
    Bin = ccid:encode_intr(Msg),
    S0#?MODULE{iq = IQ0 ++ [Bin]}.

send_bulk_resp(Msg, S0 = #?MODULE{rbuf = <<>>, rq = [], name = Name,
                                  epin = EpIn, rwait = Waiting}) ->
    {Slot, Seq} = ccid:slot_seq(Msg),
    Bin = ccid:encode_msg(Msg),
    S1 = if
        Waiting ->
            #?MODULE{conn_fsm = Fsm, reqs = Reqs0} = S0,
            lager:debug("[~s] sending intr for bulk-in", [Name]),
            Cmd = #urelay_intr{dir = ?URELAY_DIR_IN, ep = EpIn},
            Reqs1 = gen_statem:send_request(Fsm, Cmd, intr, Reqs0),
            S0#?MODULE{reqs = Reqs1};
        true ->
            S0
    end,
    S1#?MODULE{rbuf = Bin, rpos = 0, rwait = false, rslot = Slot, rseq = Seq};
send_bulk_resp(Msg, S0 = #?MODULE{rq = RQ0}) ->
    {Slot, Seq} = ccid:slot_seq(Msg),
    Bin = ccid:encode_msg(Msg),
    S0#?MODULE{rq = RQ0 ++ [{Slot, Seq, Bin}]}.

drop_slot_resps(Slot, S0 = #?MODULE{name = Name, rq = RQ0}) ->
    S1 = case S0 of
        #?MODULE{rslot = Slot, rpos = 0} ->
            S0#?MODULE{rslot = undefined, rseq = undefined, rbuf = <<>>};
        _ -> S0
    end,
    RQ1 = lists:filter(fun
        ({TheSlot, _Bin}) when (TheSlot =:= Slot) ->
            lager:debug("[~s] ABORT: killing queued resp", [Name]),
            false;
        ({_OtherSlot, _Bin}) ->
            true
    end, RQ0),
    S1#?MODULE{rq = RQ1}.

handle_cmd(Cmd, S0 = #?MODULE{slots = Slots0, reqs = Reqs0,
                              name = Name}) ->
    {Slot, _Seq} = ccid:slot_seq(Cmd),
    lager:debug("[~s/~B] >> ~s", [Name, Slot, ccid:pretty_print(Cmd)]),
    case Slots0 of
        #{Slot := Fsm} ->
            Reqs1 = gen_statem:send_request(Fsm, Cmd, Slot, Reqs0),
            S0#?MODULE{reqs = Reqs1};
        _ ->
            lager:debug("[~s/~B] dropping, invalid slot num", [Name, Slot]),
            Resp = ccid:error_resp(Cmd, #ccid_err{icc = not_present,
                                                  cmd = failed,
                                                  error = ?CCID_SLOT_INVALID}),
            send_bulk_resp(Resp, S0)
    end.

check_cmd(S0 = #?MODULE{cmdlen = undefined}) -> S0;
check_cmd(S0 = #?MODULE{cmdbuf = Buf, cmdlen = Len, seq = Seq0, name = Name}) ->
    HaveLen = iolist_size(Buf),
    if
        (HaveLen >= Len) ->
            <<Cmd:Len/binary, Rest/binary>> = iolist_to_binary(
                lists:reverse(Buf)),
            S1 = case Rest of
                <<_MsgType, NextLen:32/little, _Rest/binary>> when (NextLen =< 65536) ->
                    S0#?MODULE{cmdbuf = [Rest], cmdlen = 10 + NextLen};
                <<>> ->
                    S0#?MODULE{cmdbuf = [], cmdlen = undefined};
                _ ->
                    lager:debug("[~s] dropping trailing garbage: ~p",
                        [Name, Rest]),
                    S0#?MODULE{cmdbuf = [], cmdlen = undefined}
            end,
            case ccid:decode_msg(Cmd) of
                {ok, Rec} ->
                    {_Slot, Seq} = ccid:slot_seq(Rec),
                    if
                        (Seq < Seq0) ->
                            lager:debug("[~s] sequence number reset!", [Name]);
                        true -> ok
                    end,
                    S2 = S1#?MODULE{seq = Seq},
                    check_cmd(handle_cmd(Rec, S2));
                Err ->
                    lager:debug("[~s] failed to decode ccid command ~p: ~p", [
                        Name, Cmd, Err]),
                    check_cmd(S1)
            end;
        true ->
            S0
    end.
