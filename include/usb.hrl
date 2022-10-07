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

-define(UR_GET_STATUS,                          16#00).
-define(UR_CLEAR_FEATURE,                       16#01).
-define(UR_SET_FEATURE,                         16#03).
-define(UR_SET_ADDRESS,                         16#05).
-define(UR_GET_DESCRIPTOR,                      16#06).
-define(UDESC_DEVICE,                           16#01).
-define(UDESC_CONFIG,                           16#02).
-define(UDESC_STRING,                           16#03).
-define(USB_LANGUAGE_TABLE,                     16#00).
-define(UDESC_INTERFACE,                        16#04).
-define(UDESC_ENDPOINT,                         16#05).
-define(UDESC_DEVICE_QUALIFIER,                 16#06).
-define(UDESC_OTHER_SPEED_CONFIGURATION,        16#07).
-define(UDESC_INTERFACE_POWER,                  16#08).
-define(UDESC_OTG,                              16#09).
-define(UDESC_DEBUG,                            16#0A).
-define(UDESC_IFACE_ASSOC,                      16#0B).
-define(UDESC_BOS,                              16#0F).
-define(UDESC_DEVICE_CAPABILITY,                16#10).
-define(UDESC_CS_DEVICE,                        16#21).
-define(UDESC_CS_CONFIG,                        16#22).
-define(UDESC_CS_STRING,                        16#23).
-define(UDESC_CS_INTERFACE,                     16#24).
-define(UDESC_CS_ENDPOINT,                      16#25).
-define(UDESC_HUB,                              16#29).
-define(UDESC_SS_HUB,                           16#2A).
-define(UDESC_ENDPOINT_SS_COMP,                 16#30).
-define(UR_SET_DESCRIPTOR,                      16#07).
-define(UR_GET_CONFIG,                          16#08).
-define(UR_SET_CONFIG,                          16#09).
-define(UR_GET_INTERFACE,                       16#0a).
-define(UR_SET_INTERFACE,                       16#0b).
-define(UR_SYNCH_FRAME,                         16#0c).
-define(UR_SET_SEL,                             16#30).
-define(UR_ISOCH_DELAY,                         16#31).

-define(UF_ENDPOINT_HALT,                       16#00).
-define(UF_DEVICE_REMOTE_WAKEUP,                16#01).
-define(UF_TEST_MODE,                           16#02).

-define(UDESC_CCID,                             16#21).

-define(UT_WRITE,                               16#00).
-define(UT_READ,                                16#80).
-define(UT_STANDARD,                            16#00).
-define(UT_CLASS,                               16#20).
-define(UT_VENDOR,                              16#40).
-define(UT_DEVICE,                              16#00).
-define(UT_INTERFACE,                           16#01).
-define(UT_ENDPOINT,                            16#02).
-define(UT_OTHER,                               16#03).

-define(UT_READ_DEVICE,                 (?UT_READ  bor ?UT_STANDARD bor ?UT_DEVICE)).
-define(UT_READ_INTERFACE,              (?UT_READ  bor ?UT_STANDARD bor ?UT_INTERFACE)).
-define(UT_READ_ENDPOINT,               (?UT_READ  bor ?UT_STANDARD bor ?UT_ENDPOINT)).
-define(UT_WRITE_DEVICE,                (?UT_WRITE bor ?UT_STANDARD bor ?UT_DEVICE)).
-define(UT_WRITE_INTERFACE,             (?UT_WRITE bor ?UT_STANDARD bor ?UT_INTERFACE)).
-define(UT_WRITE_ENDPOINT,              (?UT_WRITE bor ?UT_STANDARD bor ?UT_ENDPOINT)).
-define(UT_READ_CLASS_DEVICE,           (?UT_READ  bor ?UT_CLASS    bor ?UT_DEVICE)).
-define(UT_READ_CLASS_INTERFACE,        (?UT_READ  bor ?UT_CLASS    bor ?UT_INTERFACE)).
-define(UT_READ_CLASS_OTHER,            (?UT_READ  bor ?UT_CLASS    bor ?UT_OTHER)).
-define(UT_READ_CLASS_ENDPOINT,         (?UT_READ  bor ?UT_CLASS    bor ?UT_ENDPOINT)).
-define(UT_WRITE_CLASS_DEVICE,          (?UT_WRITE bor ?UT_CLASS    bor ?UT_DEVICE)).
-define(UT_WRITE_CLASS_INTERFACE,       (?UT_WRITE bor ?UT_CLASS    bor ?UT_INTERFACE)).
-define(UT_WRITE_CLASS_OTHER,           (?UT_WRITE bor ?UT_CLASS    bor ?UT_OTHER)).
-define(UT_WRITE_CLASS_ENDPOINT,        (?UT_WRITE bor ?UT_CLASS    bor ?UT_ENDPOINT)).
-define(UT_READ_VENDOR_DEVICE,          (?UT_READ  bor ?UT_VENDOR   bor ?UT_DEVICE)).
-define(UT_READ_VENDOR_INTERFACE,       (?UT_READ  bor ?UT_VENDOR   bor ?UT_INTERFACE)).
-define(UT_READ_VENDOR_OTHER,           (?UT_READ  bor ?UT_VENDOR   bor ?UT_OTHER)).
-define(UT_READ_VENDOR_ENDPOINT,        (?UT_READ  bor ?UT_VENDOR   bor ?UT_ENDPOINT)).
-define(UT_WRITE_VENDOR_DEVICE,         (?UT_WRITE bor ?UT_VENDOR   bor ?UT_DEVICE)).
-define(UT_WRITE_VENDOR_INTERFACE,      (?UT_WRITE bor ?UT_VENDOR   bor ?UT_INTERFACE)).
-define(UT_WRITE_VENDOR_OTHER,          (?UT_WRITE bor ?UT_VENDOR   bor ?UT_OTHER)).
-define(UT_WRITE_VENDOR_ENDPOINT,       (?UT_WRITE bor ?UT_VENDOR   bor ?UT_ENDPOINT)).

-define(USB_ERR_NORMAL_COMPLETION,      0).
-define(USB_ERR_PENDING_REQUESTS,       1).
-define(USB_ERR_NOT_STARTED,            2).
-define(USB_ERR_INVAL,                  3).
-define(USB_ERR_NOMEM,                  4).
-define(USB_ERR_CANCELLED,              5).
-define(USB_ERR_BAD_ADDRESS,            6).
-define(USB_ERR_BAD_BUFSIZE,            7).
-define(USB_ERR_BAD_FLAG,               8).
-define(USB_ERR_NO_CALLBACK,            9).
-define(USB_ERR_IN_USE,                 10).
-define(USB_ERR_NO_ADDR,                11).
-define(USB_ERR_NO_PIPE,                12).
-define(USB_ERR_ZERO_NFRAMES,           13).
-define(USB_ERR_ZERO_MAXP,              14).
-define(USB_ERR_SET_ADDR_FAILED,        15).
-define(USB_ERR_NO_POWER,               16).
-define(USB_ERR_TOO_DEEP,               17).
-define(USB_ERR_IOERROR,                18).
-define(USB_ERR_NOT_CONFIGURED,         19).
-define(USB_ERR_TIMEOUT,                20).
-define(USB_ERR_SHORT_XFER,             21).
-define(USB_ERR_STALLED,                22).
-define(USB_ERR_INTERRUPTED,            23).
-define(USB_ERR_DMA_LOAD_FAILED,        24).
-define(USB_ERR_BAD_CONTEXT,            25).
-define(USB_ERR_NO_ROOT_HUB,            26).
-define(USB_ERR_NO_INTR_THREAD,         27).
-define(USB_ERR_NOT_LOCKED,             28).

-define(USB_ACK,        0).
-define(USB_NAK,        1).
-define(USB_STALL,      2).
-define(USB_NYET,       3).
-define(USB_ERR,        4).
-define(USB_SHORT,      5).

-define(CCID_CTRL_ABORT,        16#01).
-define(CCID_CTRL_GET_CLOCK,    16#02).
-define(CCID_CTRL_GET_BAUD,     16#03).

-define(CCID_PC_to_RDR_IccPowerOn,      16#62).
-define(CCID_PC_to_RDR_IccPowerOff,     16#63).
-define(CCID_PC_to_RDR_GetSlotStatus,   16#65).
-define(CCID_PC_to_RDR_XfrBlock,        16#6F).
-define(CCID_PC_to_RDR_GetParams,       16#6C).
-define(CCID_PC_to_RDR_ResetParams,     16#6D).
-define(CCID_PC_to_RDR_SetParams,       16#61).
-define(CCID_PC_to_RDR_Escape,          16#6B).
-define(CCID_PC_to_RDR_IccClock,        16#6E).
-define(CCID_PC_to_RDR_T0APDU,          16#6A).
-define(CCID_PC_to_RDR_Secure,          16#69).
-define(CCID_PC_to_RDR_Mechanical,      16#71).
-define(CCID_PC_to_RDR_Abort,           16#72).
-define(CCID_PC_to_RDR_SetBaudClock,    16#73).

-define(CCID_RDR_to_PC_DataBlock,       16#80).
-define(CCID_RDR_to_PC_SlotStatus,      16#81).
-define(CCID_RDR_to_PC_Params,          16#82).
-define(CCID_RDR_to_PC_Escape,          16#83).
-define(CCID_RDR_to_PC_BaudClock,       16#84).

-define(CCID_CMD_ABORTED,               -1).
-define(CCID_ICC_MUTE,                  -2).
-define(CCID_XFR_PARITY_ERR,            -3).
-define(CCID_XFR_OVERRUN,               -4).
-define(CCID_HW_ERROR,                  -5).
-define(CCID_BAD_ATR_TS,                -8).
-define(CCID_BAD_ATR_TCK,               -9).
-define(CCID_ICC_PROTO_UNSUPPORTED,     -10).
-define(CCID_ICC_CLASS_UNSUPPORTED,     -11).
-define(CCID_PROC_BYTE_CONFLICT,        -12).
-define(CCID_DEACT_PROC,                -13).
-define(CCID_BUSY_AUTO_SEQ,             -14).
-define(CCID_PIN_TIMEOUT,               -16).
-define(CCID_PIN_CANCELLED,             -17).
-define(CCID_CMD_SLOT_BUSY,             -32).
-define(CCID_SLOT_INVALID,              5).
-define(CCID_LENGTH_INVALID,            1).
-define(CCID_PARAMS_INVALID,            7).
-define(CCID_CMD_UNSUPPORTED,           0).

-record(usb_string_descr, {
    string :: {raw, binary()} | string()
    }).

-record(usb_bos_descr, {
    bNumDeviceCaps :: integer()
    }).

-record(usb_devcap_ss_descr, {
    bmAttributes :: integer(),
    wSpeedsSupported :: integer(),
    bFunctionalitySupport :: integer(),
    bU1DevExitLat :: integer(),
    wU2DevExitLat :: integer()
    }).

-record(usb_device_descr, {
    bcdUSB = {3,0} :: {integer(), integer()},
    bDeviceClass = 0 :: integer(),
    bDeviceSubClass = 0 :: integer(),
    bDeviceProtocol = 0 :: integer(),
    bMaxPacketSize :: integer(),
    idVendor :: integer(),
    idProduct :: integer(),
    bcdDevice :: integer(),
    iManufacturer :: integer(),
    iProduct :: integer(),
    iSerialNumber :: integer(),
    bNumConfigurations :: integer()
    }).

-record(usb_config_descr, {
    bNumInterface :: integer(),
    bConfigurationValue :: integer(),
    iConfiguration :: integer(),
    bmAttributes = [] :: [bus_powered | self_powered | remote_wakeup],
    bMaxPower :: integer()
    }).

-record(usb_intf_descr, {
    bInterfaceNumber :: integer(),
    bAlternateSetting :: integer(),
    bNumEndpoints :: integer(),
    bInterfaceClass :: integer(),
    bInterfaceSubClass :: integer(),
    bInterfaceProtocol :: integer(),
    iInterface :: integer()
    }).

-record(usb_endpoint_descr, {
    bEndpointAddress :: control | {out, integer()} | {in, integer()},
    bmAttributes :: control | bulk | interrupt |
        {isochronous, none | async | adaptive | sync,
            data | feedback | implicit},
    wMaxPacketSize :: integer() | {integer(), integer()},
    bInterval :: integer()
    }).

-record(usb_ccid_descr, {
    bcdCCID :: {integer(), integer()},
    bMaxSlotIndex :: integer(),
    bVoltageSupport :: ['5v' | '3v' | '1.8v'],
    dwProtocols :: [t1 | t0],
    dwDefaultClock :: integer(),
    dwMaximumClock :: integer(),
    bNumClockSupported :: integer(),
    dwDataRate :: integer(),
    dwMaxDataRate :: integer(),
    bNumDataRatesSupported :: integer(),
    dwMaxIFSD :: integer(),
    dwSynchProtocols :: ['2wire' | '3wire' | i2c],
    dwMechanical :: [accept | eject | capture | lock],
    dwFeatures :: {char | tpdu | short_apdu | ext_apdu, [auto_atr |
        auto_activate | auto_voltage | auto_clock | auto_baud | auto_params |
        auto_pps | clock_stop | nad_nz | auto_ifsd | usb_wake]},
    dwMaxCCIDMessageLength :: integer(),
    bClassGetResponse :: integer(),
    bClassEnvelope :: integer(),
    wLcdLayout :: none | {integer(), integer()},
    bPINSupport :: [verify | modify],
    bMaxCCIDBusySlots :: integer()
    }).

-record(ccid_generic_msg, {
    slot :: integer(),
    seq :: integer(),
    type :: integer(),
    params = <<0:24>> :: binary(),
    data = <<>> :: binary()
    }).

-record(ccid_err, {
    icc = active :: active | inactive | not_present,
    cmd = ok :: ok | failed | time_ext,
    error = 0 :: integer()
    }).

-record(ccid_pc_to_rdr_iccpoweron, {
    slot :: integer(),
    seq :: integer(),
    powersel :: auto | '5v' | '3v' | '1.8v'
    }).

-record(ccid_pc_to_rdr_iccpoweroff, {
    slot :: integer(),
    seq :: integer()
    }).

-record(ccid_pc_to_rdr_getslotstatus, {
    slot :: integer(),
    seq :: integer()
    }).

-record(ccid_pc_to_rdr_xfrblock, {
    slot :: integer(),
    seq :: integer(),
    bwi = 0 :: integer(),
    chain = one :: one | first | last | continue | get_next,
    data :: binary()
    }).

-record(ccid_pc_to_rdr_getparams, {
    slot :: integer(),
    seq :: integer()
    }).

-record(ccid_pc_to_rdr_resetparams, {
    slot :: integer(),
    seq :: integer()
    }).

-record(ccid_t0_params, {
    fidx = 1 :: integer(),
    didx = 1 :: integer(),
    convention = direct :: direct | inverse,
    guardtime = 0 :: integer(),
    wi = 0 :: integer(),
    clockstop = [high, low] :: [high | low]
    }).

-record(ccid_t1_params, {
    fidx = 1 :: integer(),
    didx = 1 :: integer(),
    checksum = lrc :: lrc | crc,
    convention = direct :: direct | inverse,
    guardtime = 0 :: integer(),
    bwi = 0 :: integer(),
    cwi = 0 :: integer(),
    clockstop = [high, low] :: [high | low],
    ifsc = 16#fe :: integer(),
    nad = 0 :: integer()
    }).

-record(ccid_pc_to_rdr_setparams, {
    slot :: integer(),
    seq :: integer(),
    params :: #ccid_t0_params{} | #ccid_t1_params{}
    }).

-record(ccid_pc_to_rdr_escape, {
    slot :: integer(),
    seq :: integer(),
    data :: binary()
    }).

-record(ccid_pc_to_rdr_iccclock, {
    slot :: integer(),
    seq :: integer(),
    cmd = restart :: restart | stop
    }).

-record(ccid_pc_to_rdr_t0apdu, {
    slot :: integer(),
    seq :: integer(),
    cls_get_response :: default | echo | integer(),
    cls_envelope :: default | echo | integer()
    }).

-record(ccid_pc_to_rdr_mechanical, {
    slot :: integer(),
    seq :: integer(),
    func :: accept | eject | capture | lock | unlock
    }).

-record(ccid_pc_to_rdr_abort, {
    slot :: integer(),
    seq :: integer()
    }).

-record(ccid_pc_to_rdr_setbaudclock, {
    slot :: integer(),
    seq :: integer(),
    freq :: integer(),
    baud :: integer()
    }).

-record(ccid_rdr_to_pc_datablock, {
    slot :: integer(),
    seq :: integer(),
    err = #ccid_err{} :: #ccid_err{},
    chain = one :: one | first | last | continue | get_next,
    data :: binary()
    }).

-record(ccid_rdr_to_pc_slotstatus, {
    slot :: integer(),
    seq :: integer(),
    err = #ccid_err{} :: #ccid_err{},
    clock = stopped :: running | stopped_low | stopped_high | stopped
    }).

-record(ccid_rdr_to_pc_params, {
    slot :: integer(),
    seq :: integer(),
    err = #ccid_err{} :: #ccid_err{},
    params :: none | #ccid_t0_params{} | #ccid_t1_params{}
    }).

-record(ccid_rdr_to_pc_escape, {
    slot :: integer(),
    seq :: integer(),
    err = #ccid_err{} :: #ccid_err{},
    data :: binary()
    }).

-record(ccid_rdr_to_pc_baudclock, {
    slot :: integer(),
    seq :: integer(),
    err = #ccid_err{} :: #ccid_err{},
    freq :: integer(),
    baud :: integer()
    }).

-record(ccid_pc_to_rdr_secure, {
    slot :: integer(),
    seq :: integer(),
    bwi = 0 :: integer(),
    chain = one :: one | first | last | continue | get_next,
    data :: binary()    % not yet decoded
    }).
