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

-module(udescr).

-include("include/usb.hrl").

-export([
    pack/1
    ]).

-type descriptor() ::
    #usb_string_descr{} | #usb_bos_descr{} | #usb_devcap_ss_descr{} |
    #usb_device_descr{} | #usb_config_descr{} | #usb_intf_descr{} |
    #usb_endpoint_descr{} | #usb_ccid_descr{}.

-spec pack([descriptor()]) -> binary().
pack(Descrs) ->
    iolist_to_binary(packl(Descrs)).

-spec bit(atom(), [atom()]) -> 1 | 0.
bit(Atom, List) ->
    case lists:member(Atom, List) of
        true -> 1;
        false -> 0
    end.

bcd_to_bin({A, B}) ->
    <<(bcd_to_bin(B))/binary, (bcd_to_bin(A))/binary>>;
bcd_to_bin(X) when is_integer(X) ->
    X0 = X div 10,
    X1 = X rem 10,
    <<X0:4, X1:4>>.

packl([#usb_config_descr{bNumInterface = BNumInterface,
                         bConfigurationValue = BConfigValue,
                         iConfiguration = IConfig,
                         bmAttributes = Attrs,
                         bMaxPower = BMaxPower} | Rest]) ->
    RestEnc = packl(Rest),
    BusPowered = bit(bus_powered, Attrs),
    SelfPowered = bit(self_powered, Attrs),
    RemoteWake = bit(remote_wakeup, Attrs),
    BmAttributes = <<BusPowered:1, SelfPowered:1, RemoteWake:1, 0:5>>,
    AfterLen = <<BNumInterface, BConfigValue, IConfig, BmAttributes/binary,
                 BMaxPower>>,
    ZeroVer = pack_descr(?UDESC_CONFIG, <<0:16/little, AfterLen/binary>>),
    TotalLen = iolist_size([ZeroVer | RestEnc]),
    [pack_descr(?UDESC_CONFIG, <<TotalLen:16/little, AfterLen/binary>>) | RestEnc];

packl([#usb_bos_descr{bNumDeviceCaps = BNumDeviceCaps} | Rest]) ->
    RestEnc = packl(Rest),
    AfterLen = <<BNumDeviceCaps>>,
    ZeroVer = pack_descr(?UDESC_BOS, <<0:16/little, AfterLen/binary>>),
    TotalLen = iolist_size([ZeroVer | RestEnc]),
    [pack_descr(?UDESC_BOS, <<TotalLen:16/little, AfterLen/binary>>) | RestEnc];

packl([#usb_device_descr{bcdUSB = BcdUSB,
                         bDeviceClass = BDeviceClass,
                         bDeviceSubClass = BDeviceSubClass,
                         bDeviceProtocol = BDeviceProtocol,
                         bMaxPacketSize = BMaxPacketSize,
                         idVendor = IdVendor,
                         idProduct = IdProduct,
                         bcdDevice = BcdDevice,
                         iManufacturer = IManufacturer,
                         iProduct = IProduct,
                         iSerialNumber = ISerialNum,
                         bNumConfigurations = BNumConfigs} | Rest]) ->
    [pack_descr(?UDESC_DEVICE, <<(bcd_to_bin(BcdUSB))/binary, BDeviceClass,
        BDeviceSubClass, BDeviceProtocol, BMaxPacketSize, IdVendor:16/little,
        IdProduct:16/little, BcdDevice:16/little, IManufacturer, IProduct,
        ISerialNum, BNumConfigs>>) | packl(Rest)];

packl([#usb_intf_descr{bInterfaceNumber = BInterfaceNum,
                       bAlternateSetting = BAlternateSetting,
                       bNumEndpoints = BNumEndpoints,
                       bInterfaceClass = BInterfaceClass,
                       bInterfaceSubClass = BInterfaceSubClass,
                       bInterfaceProtocol = BInterfaceProtocol,
                       iInterface = IInterface} | Rest]) ->
    [pack_descr(?UDESC_INTERFACE, <<BInterfaceNum, BAlternateSetting,
        BNumEndpoints, BInterfaceClass, BInterfaceSubClass, BInterfaceProtocol,
        IInterface>>) | packl(Rest)];

packl([#usb_endpoint_descr{bEndpointAddress = EPAddr,
                           bmAttributes = Attrs,
                           wMaxPacketSize = MaxPacketSize,
                           bInterval = BInterval} | Rest]) ->
    EPAddrBin = case EPAddr of
        control ->      <<0:1, 0:3, 0:4>>;
        {out, Num} ->   <<0:1, 0:3, Num:4>>;
        {in, Num} ->    <<1:1, 0:3, Num:4>>
    end,
    AttrBin = case Attrs of
        control ->   <<0:6, 2#00:2>>;
        bulk ->      <<0:6, 2#10:2>>;
        interrupt -> <<0:6, 2#11:2>>;

        {isochronous, SyncType, UsageType} ->
            SyncTypeN = case SyncType of
                none ->     2#00;
                async ->    2#01;
                adaptive -> 2#10;
                sync ->     2#11
            end,
            UsageTypeN = case UsageType of
                data ->     2#00;
                feedback -> 2#01;
                implicit -> 2#10
            end,
            <<0:2, UsageTypeN:2, SyncTypeN:2, 2#01:2>>
    end,
    <<WMaxPacketSize:16/big>> = case MaxPacketSize of
        {Micros, Sz} -> <<0:4, Micros:2, Sz:10/big>>;
        Sz ->           <<0:4, 0:2, Sz:10/big>>
    end,
    [pack_descr(?UDESC_ENDPOINT, <<EPAddrBin/binary, AttrBin/binary,
        WMaxPacketSize:16/little, BInterval>>) | packl(Rest)];

packl([#usb_ccid_descr{bcdCCID = BcdCCID,
                       bMaxSlotIndex = BMaxSlotIndex,
                       bVoltageSupport = VoltageSupport,
                       dwProtocols = Protocols,
                       dwDefaultClock = DwDefaultClock,
                       dwMaximumClock = DwMaximumClock,
                       bNumClockSupported = BNumClockSupported,
                       dwDataRate = DwDataRate,
                       dwMaxDataRate = DwMaxDataRate,
                       bNumDataRatesSupported = BNumDataRatesSupported,
                       dwMaxIFSD = DwMaxIFSD,
                       dwSynchProtocols = SynchProtocols,
                       dwMechanical = Mechanical,
                       dwFeatures = Features,
                       dwMaxCCIDMessageLength = DwMaxCCIDMessageLength,
                       bClassGetResponse = BClassGetResponse,
                       bClassEnvelope = BClassEnvelope,
                       wLcdLayout = LcdLayout,
                       bPINSupport = PINSupport,
                       bMaxCCIDBusySlots = BMaxCCIDBusySlots} | Rest]) ->
    V18 = bit('1.8v', VoltageSupport),
    V3 = bit('3v', VoltageSupport),
    V5 = bit('5v', VoltageSupport),
    BVoltageSupport = <<0:5, V18:1, V3:1, V5:1>>,
    T0 = bit(t0, Protocols),
    T1 = bit(t1, Protocols),
    <<DwProtocols:32/big>> = <<0:24,0:6,T1:1,T0:1>>,
    W2 = bit('2wire', SynchProtocols),
    W3 = bit('3wire', SynchProtocols),
    I2C = bit(i2c, SynchProtocols),
    <<DwSynchProtocols:32/big>> = <<0:24,0:5,W2:1,W3:1,I2C:1>>,
    Accept = bit(accept, Mechanical),
    Eject = bit(eject, Mechanical),
    Capture = bit(capture, Mechanical),
    Lock = bit(lock, Mechanical),
    <<DwMechanical:32/big>> = <<0:24,0:4,Lock:1,Capture:1,Eject:1,Accept:1>>,
    {Level, Feats} = Features,
    LevelN = case Level of
        char -> 16#0;
        tpdu -> 16#1;
        short_apdu -> 16#2;
        ext_apdu -> 16#4
    end,
    USBWake = bit(usb_wake, Feats),
    AutoIFSD = bit(auto_ifsd, Feats),
    NADNZ = bit(nad_nz, Feats),
    ClockStop = bit(clock_stop, Feats),
    AutoPPS = bit(auto_pps, Feats),
    AutoParams = bit(auto_params, Feats),
    AutoBaud = bit(auto_baud, Feats),
    AutoClock = bit(auto_clock, Feats),
    AutoVolt = bit(auto_voltage, Feats),
    AutoActivate = bit(auto_activate, Feats),
    AutoATR = bit(auto_atr, Feats),
    <<DwFeatures:32/big>> = <<0:4,
                              0:4,
                              0:3, USBWake:1,
                              LevelN:4,
                              0:4,
                              0:1, AutoIFSD:1, NADNZ:1, ClockStop:1,
                              AutoPPS:1, AutoParams:1, AutoBaud:1, AutoClock:1,
                              AutoVolt:1, AutoActivate:1, AutoATR:1, 0:1>>,
    <<WLcdLayout:16/big>> = case LcdLayout of
        none -> <<0:16>>;
        {Lines, Chars} -> <<Lines, Chars>>
    end,
    Modify = bit(modify, PINSupport),
    Verify = bit(verify, PINSupport),
    BPINSupport = <<0:6, Modify:1, Verify:1>>,
    [pack_descr(?UDESC_CCID, <<(bcd_to_bin(BcdCCID))/binary, BMaxSlotIndex,
        BVoltageSupport/binary, DwProtocols:32/little, DwDefaultClock:32/little,
        DwMaximumClock:32/little, BNumClockSupported, DwDataRate:32/little,
        DwMaxDataRate:32/little, BNumDataRatesSupported,
        DwMaxIFSD:32/little, DwSynchProtocols:32/little, DwMechanical:32/little,
        DwFeatures:32/little, DwMaxCCIDMessageLength:32/little,
        BClassGetResponse, BClassEnvelope, WLcdLayout:16/little,
        BPINSupport/binary, BMaxCCIDBusySlots>>) | packl(Rest)];

packl([#usb_string_descr{string = Str} | Rest]) ->
    D = case Str of
        {raw, Bin} -> Bin;
        _ -> unicode:characters_to_binary(Str, utf8, {utf16, little})
    end,
    [pack_descr(?UDESC_STRING, D) | packl(Rest)];

packl([#usb_devcap_ss_descr{bmAttributes = BmAttributes,
                            wSpeedsSupported = WSpeedsSupported,
                            bFunctionalitySupport = BFunctionalitySupport,
                            bU1DevExitLat = BU1DevExitLat,
                            wU2DevExitLat = WU2DevExitLat} | Rest]) ->
    [pack_descr(?UDESC_DEVICE_CAPABILITY, <<3, BmAttributes,
        WSpeedsSupported:16/little, BFunctionalitySupport,
        BU1DevExitLat, WU2DevExitLat:16/little>>) | packl(Rest)];
packl([]) -> [].

pack_descr(Type, Data) ->
    Len = byte_size(Data) + 2,
    [<<Len, Type>>, Data].
