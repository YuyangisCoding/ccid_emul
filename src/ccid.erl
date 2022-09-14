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

-module(ccid).

-include("include/records.hrl").
-include("include/usb.hrl").
-include("include/iso7816.hrl").

-export([pretty_print/1, pretty_print/2]).

-export([decode_msg/1, encode_msg/1, error_resp/2]).

-export_type([
    msg/0, host_msg/0, device_msg/0
    ]).

-type msg() :: host_msg() | device_msg().
-type host_msg() :: #ccid_generic_msg{} | #ccid_pc_to_rdr_iccpoweron{}.
-type device_msg() :: #ccid_generic_msg{} | #ccid_rdr_to_pc_datablock{} |
    #ccid_rdr_to_pc_escape{} | #ccid_rdr_to_pc_params{} |
    #ccid_rdr_to_pc_slotstatus{} | #ccid_rdr_to_pc_baudclock{}.

-define(pp(Rec),
pretty_print(Rec, N) ->
    N = record_info(size, Rec) - 1,
    record_info(fields, Rec)).

pretty_print(Record) ->
    io_lib_pretty:print(Record, [
        {record_print_fun, fun pretty_print/2},
        {line_length, 9999}]).
?pp(urelay_reset);
?pp(urelay_status);
?pp(urelay_ctrl);
?pp(urelay_ctrl_resp);
?pp(urelay_data);
?pp(urelay_data_resp);
?pp(urelay_intr);
?pp(apdu_cmd);
?pp(apdu_reply);
?pp(ccid_err);
?pp(ccid_pc_to_rdr_iccpoweron);
?pp(ccid_pc_to_rdr_iccpoweroff);
?pp(ccid_pc_to_rdr_getslotstatus);
?pp(ccid_pc_to_rdr_xfrblock);
?pp(ccid_pc_to_rdr_getparams);
?pp(ccid_pc_to_rdr_resetparams);
?pp(ccid_pc_to_rdr_setparams);
?pp(ccid_t0_params);
?pp(ccid_t1_params);
?pp(ccid_pc_to_rdr_escape);
?pp(ccid_pc_to_rdr_iccclock);
?pp(ccid_pc_to_rdr_t0apdu);
?pp(ccid_pc_to_rdr_mechanical);
?pp(ccid_pc_to_rdr_abort);
?pp(ccid_pc_to_rdr_setbaudclock);
?pp(ccid_rdr_to_pc_datablock);
?pp(ccid_rdr_to_pc_slotstatus);
?pp(ccid_rdr_to_pc_params);
?pp(ccid_rdr_to_pc_escape);
?pp(ccid_rdr_to_pc_baudclock);
?pp(ccid_pc_to_rdr_secure);
?pp(ccid_generic_msg);
?pp(usb_ccid_descr);
?pp(usb_endpoint_descr);
?pp(usb_intf_descr);
?pp(usb_config_descr);
?pp(usb_device_descr);
?pp(usb_string_descr);
pretty_print(_Rec, _N) ->
    no.

decode_msg(<<MsgType, DataLen:32/little, Slot, Seq, Params:3/binary,
             Data:DataLen/binary>>) ->
    case (catch decode_msg(MsgType, Slot, Seq, Params, Data)) of
        {'EXIT', Why} -> {error, Why};
        Else -> {ok, Else}
    end;
decode_msg(_) ->
    {error, invalid_header}.

decode_msg(?CCID_PC_to_RDR_IccPowerOn, Slot, Seq, <<PSel, _, _>>, <<>>) ->
    PowerSel = case PSel of
        16#00 -> auto;
        16#01 -> '5v';
        16#02 -> '3v';
        16#03 -> '1.8v'
    end,
    #ccid_pc_to_rdr_iccpoweron{slot = Slot, seq = Seq, powersel = PowerSel};

decode_msg(?CCID_PC_to_RDR_IccPowerOff, Slot, Seq, <<_:24>>, <<>>) ->
    #ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq};

decode_msg(?CCID_PC_to_RDR_GetSlotStatus, Slot, Seq, <<_:24>>, <<>>) ->
    #ccid_pc_to_rdr_getslotstatus{slot = Slot, seq = Seq};

decode_msg(?CCID_PC_to_RDR_XfrBlock, Slot, Seq,
                                <<BWI, ChainParam:16/little>>, Data) ->
    Chain = case ChainParam of
        16#0000 -> one;
        16#0001 -> first;
        16#0002 -> last;
        16#0003 -> continue;
        16#0010 -> get_next
    end,
    #ccid_pc_to_rdr_xfrblock{slot = Slot, seq = Seq, bwi = BWI,
                             chain = Chain, data = Data};

decode_msg(?CCID_PC_to_RDR_GetParams, Slot, Seq, <<_:24>>, <<>>) ->
    #ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq};

decode_msg(?CCID_PC_to_RDR_ResetParams, Slot, Seq, <<_:24>>, <<>>) ->
    #ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq};

decode_msg(?CCID_PC_to_RDR_SetParams, Slot, Seq, <<ProtoNum, _:16>>, ProtoD) ->
    Proto = case ProtoNum of
        16#00 ->
            <<FIdx:4, DIdx:4,
              _:6, ConvBit:1, _:1,
              GuardTime,
              WI,
              0:6, StopHigh:1, StopLow:1>> = ProtoD,
            Conv = ifbit(ConvBit, inverse, direct),
            Stop = collect_bits([{StopHigh, high}, {StopLow, low}]),
            #ccid_t0_params{fidx = FIdx, didx = DIdx,
                            convention = Conv,
                            guardtime = GuardTime,
                            wi = WI,
                            clockstop = Stop};
        16#01 ->
            <<FIdx:4, DIdx:4,
              _:6, ConvBit:1, ChecksumBit:1,
              GuardTime,
              BWI:4, CWI:4,
              _:6, StopHigh:1, StopLow:1,
              IFSC,
              NAD>> = ProtoD,
            Conv = ifbit(ConvBit, inverse, direct),
            Checksum = ifbit(ChecksumBit, crc, lrc),
            Stop = collect_bits([{StopHigh, high}, {StopLow, low}]),
            #ccid_t1_params{fidx = FIdx, didx = DIdx,
                            convention = Conv,
                            checksum = Checksum,
                            guardtime = GuardTime,
                            bwi = BWI, cwi = CWI,
                            clockstop = Stop,
                            ifsc = IFSC, nad = NAD}
    end,
    #ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq, params = Proto};

decode_msg(?CCID_PC_to_RDR_Escape, Slot, Seq, <<_:24>>, Data) ->
    #ccid_pc_to_rdr_escape{slot = Slot, seq = Seq, data = Data};

decode_msg(?CCID_PC_to_RDR_IccClock, Slot, Seq, <<CmdI, _:16>>, <<>>) ->
    Cmd = case CmdI of
        16#00 -> restart;
        16#01 -> stop
    end,
    #ccid_pc_to_rdr_iccclock{slot = Slot, seq = Seq, cmd = Cmd};

decode_msg(?CCID_PC_to_RDR_T0APDU, Slot, Seq, Params, <<>>) ->
    <<_:6, SetEnv:1, SetGetResp:1, GetRespV, EnvV>> = Params,
    GetResp = case SetGetResp of
        1 -> case GetRespV of 16#FF -> echo; _ -> GetRespV end;
        0 -> default
    end,
    Env = case SetEnv of
        1 -> case EnvV of 16#FF -> echo; _ -> EnvV end;
        0 -> default
    end,
    #ccid_pc_to_rdr_t0apdu{slot = Slot, seq = Seq,
                           cls_get_response = GetResp,
                           cls_envelope = Env};

decode_msg(?CCID_PC_to_RDR_Secure, Slot, Seq,
                                <<BWI, ChainParam:16/little, _>>, Data) ->
    Chain = case ChainParam of
        16#0000 -> one;
        16#0001 -> first;
        16#0002 -> last;
        16#0003 -> continue;
        16#0010 -> get_next
    end,
    % todo: decode this further?
    #ccid_pc_to_rdr_secure{slot = Slot, seq = Seq, bwi = BWI,
                           chain = Chain, data = Data};

decode_msg(?CCID_PC_to_RDR_Mechanical, Slot, Seq, <<FuncI, _:16>>, <<>>) ->
    Func = case FuncI of
        16#01 -> accept;
        16#02 -> eject;
        16#03 -> capture;
        16#04 -> lock;
        16#05 -> unlock
    end,
    #ccid_pc_to_rdr_mechanical{slot = Slot, seq = Seq, func = Func};

decode_msg(?CCID_PC_to_RDR_Abort, Slot, Seq, <<_:24>>, <<>>) ->
    #ccid_pc_to_rdr_abort{slot = Slot, seq = Seq};

decode_msg(?CCID_PC_to_RDR_SetBaudClock, Slot, Seq, <<_:24>>, Data) ->
    <<Freq:32/little, Baud:32/little>> = Data,
    #ccid_pc_to_rdr_setbaudclock{slot = Slot, seq = Seq, freq = Freq,
                                 baud = Baud};

decode_msg(MsgType, Slot, Seq, Params, Data) ->
    #ccid_generic_msg{type = MsgType, slot = Slot, seq = Seq,
                      params = Params, data = Data}.

encode_err(#ccid_err{icc = IccState, cmd = CmdState, error = ErrCode}) ->
    BmIccStatus = case IccState of
        active      -> 0;
        inactive    -> 1;
        not_present -> 2
    end,
    BmCmdStatus = case CmdState of
        ok          -> 0;
        failed      -> 1;
        time_ext    -> 2
    end,
    <<BmCmdStatus:2, 0:4, BmIccStatus:2, ErrCode>>.

encode_msg(#ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, err = Err,
                                     chain = Chain, data = Data}) ->
    ChainParam = case Chain of
        one      -> 16#00;
        first    -> 16#01;
        last     -> 16#02;
        continue -> 16#03;
        get_next -> 16#10
    end,
    Params = <<(encode_err(Err))/binary, ChainParam>>,
    encode_msg(?CCID_RDR_to_PC_DataBlock, Slot, Seq, Params, Data);

encode_msg(#ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq, err = Err,
                                      clock = Clock}) ->
    ClockI = case Clock of
        running      -> 16#00;
        stopped_low  -> 16#01;
        stopped_high -> 16#02;
        stopped      -> 16#03
    end,
    Params = <<(encode_err(Err))/binary, ClockI>>,
    encode_msg(?CCID_RDR_to_PC_SlotStatus, Slot, Seq, Params, <<>>);

encode_msg(#ccid_rdr_to_pc_params{slot = Slot, seq = Seq, err = Err,
                                  params = TParams}) ->
    {ProtoNum, Data} = case TParams of
        none ->
            {16#00, <<>>};
        #ccid_t0_params{fidx = FIdx, didx = DIdx, convention = Conv,
                        guardtime = GuardTime, wi = WI, clockstop = Stop} ->
            ConvBit = unifbit(Conv, inverse, direct),
            [StopLow, StopHigh] = uncollect_bits(Conv, [low, high]),
            PD = <<FIdx:4, DIdx:4,
                   0:6, ConvBit:1, 0:1,
                   GuardTime,
                   WI,
                   0:6, StopHigh:1, StopLow:1>>,
            {16#00, PD};
        #ccid_t1_params{fidx = FIdx, didx = DIdx, convention = Conv,
                        checksum = Checksum, guardtime = GuardTime, bwi = BWI,
                        cwi = CWI, clockstop = Stop, ifsc = IFSC, nad = NAD} ->
            ConvBit = unifbit(Conv, inverse, direct),
            ChecksumBit = unifbit(Checksum, crc, lrc),
            [StopLow, StopHigh] = uncollect_bits(Conv, [low, high]),
            PD = <<FIdx:4, DIdx:4,
                   0:6, ConvBit:1, ChecksumBit:1,
                   GuardTime,
                   BWI:4, CWI:4,
                   0:6, StopHigh:1, StopLow:1,
                   IFSC,
                   NAD>>,
            {16#01, PD}
    end,
    Params = <<(encode_err(Err))/binary, ProtoNum>>,
    encode_msg(?CCID_RDR_to_PC_Params, Slot, Seq, Params, Data);

encode_msg(#ccid_rdr_to_pc_escape{slot = Slot, seq = Seq, err = Err,
                                  data = Data}) ->
    Params = <<(encode_err(Err))/binary, 0>>,
    encode_msg(?CCID_RDR_to_PC_Escape, Slot, Seq, Params, Data);

encode_msg(#ccid_rdr_to_pc_baudclock{slot = Slot, seq = Seq, err = Err,
                                     freq = Freq, baud = Baud}) ->
    Params = <<(encode_err(Err))/binary, 0>>,
    Data = <<Freq:32/little, Baud:32/little>>,
    encode_msg(?CCID_RDR_to_PC_BaudClock, Slot, Seq, Params, Data);

encode_msg(#ccid_generic_msg{type = MsgType, slot = Slot, seq = Seq,
                             params = Params, data = Data}) ->
    encode_msg(MsgType, Slot, Seq, Params, Data).

encode_msg(MsgType, Slot, Seq, Params, Data) ->
    <<MsgType, (byte_size(Data)):32/little, Slot, Seq, Params/binary,
      Data/binary>>.

collect_bits([{1, Atom} | Rest]) ->
    [Atom | collect_bits(Rest)];
collect_bits([{0, Atom} | Rest]) ->
    collect_bits(Rest);
collect_bits([]) -> [].

ifbit(1, True, _False) -> True;
ifbit(0, _True, False) -> False.

unifbit(Atom, True, _False) when Atom =:= True -> 1;
unifbit(Atom, _True, False) when Atom =:= False -> 0.

uncollect_bits(Set, [Atom | Rest]) ->
    case lists:member(Atom, Set) of
        true -> [1 | uncollect_bits(Set, Rest)];
        false -> [0 | uncollect_bits(Set, Rest)]
    end;
uncollect_bits(_Set, []) -> [].

error_resp(#ccid_pc_to_rdr_iccpoweron{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, err = Err, data = <<>>};
error_resp(#ccid_pc_to_rdr_iccpoweroff{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq, err = Err};
error_resp(#ccid_pc_to_rdr_getslotstatus{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq, err = Err};
error_resp(#ccid_pc_to_rdr_xfrblock{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, err = Err, data = <<>>};
error_resp(#ccid_pc_to_rdr_getparams{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_params{slot = Slot, seq = Seq, params = none};
error_resp(#ccid_pc_to_rdr_resetparams{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_params{slot = Slot, seq = Seq, params = none};
error_resp(#ccid_pc_to_rdr_setparams{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_params{slot = Slot, seq = Seq, params = none};
error_resp(#ccid_pc_to_rdr_escape{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_escape{slot = Slot, seq = Seq, err = Err, data = <<>>};
error_resp(#ccid_pc_to_rdr_iccclock{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq, err = Err};
error_resp(#ccid_pc_to_rdr_t0apdu{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq, err = Err};
error_resp(#ccid_pc_to_rdr_secure{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_datablock{slot = Slot, seq = Seq, err = Err, data = <<>>};
error_resp(#ccid_pc_to_rdr_mechanical{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq, err = Err};
error_resp(#ccid_pc_to_rdr_abort{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_slotstatus{slot = Slot, seq = Seq, err = Err};
error_resp(#ccid_pc_to_rdr_setbaudclock{slot = Slot, seq = Seq},
           #ccid_err{} = Err) ->
    #ccid_rdr_to_pc_baudclock{slot = Slot, seq = Seq, err = Err,
                              freq = 0, baud = 0}.
