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

-module(admin_cli).

-export([
    help/1,
    dump/1,
    list/1,
    reset/1,
    eject/1,
    insert/1
    ]).

help(_) ->
    io:format("usage: ccidemuladm <cmd> [args]\n"
              "\n"
              "COMMANDS\n"
              "    ccidemuladm list <prefix>\n"
              "                dump <name> [slot]\n"
              "                reset <name>\n"
              "                eject <name> <slot>\n"
              "                insert <name> <slot>\n").

list([Prefix]) ->
    {ok, Results} = ccid_fsm_db:lookup_prefix(Prefix),
    lists:foreach(fun ({K, Pid}) ->
        io:format("~20.. s  ~p\n", [K, Pid])
    end, Results);
list(_) -> help([]).

reset([Name]) ->
    {ok, Pid} = ccid_fsm_db:lookup(list_to_binary(Name)),
    ccid_fsm:force_reset(Pid);
reset(_) -> help([]).

eject([Name, SlotIdxStr]) ->
    SlotIdx = list_to_integer(SlotIdxStr),
    {ok, Pid} = ccid_fsm_db:lookup(list_to_binary(Name)),
    {ok, SlotPid} = ccid_fsm:get_slot(Pid, SlotIdx),
    ccid_slot_fsm:remove_card(SlotPid);
eject(_) -> help([]).

insert([Name, SlotIdxStr]) ->
    SlotIdx = list_to_integer(SlotIdxStr),
    {ok, Pid} = ccid_fsm_db:lookup(list_to_binary(Name)),
    {ok, SlotPid} = ccid_fsm:get_slot(Pid, SlotIdx),
    ccid_slot_fsm:insert_card(SlotPid);
insert(_) -> help([]).

dump([Name]) ->
    {ok, Pid} = ccid_fsm_db:lookup(list_to_binary(Name)),
    io:format("~s\n", [ccid_fsm:pretty_print(sys:get_state(Pid))]);
dump([Name, SlotIdxStr]) ->
    SlotIdx = list_to_integer(SlotIdxStr),
    {ok, Pid} = ccid_fsm_db:lookup(list_to_binary(Name)),
    {ok, SlotPid} = ccid_fsm:get_slot(Pid, SlotIdx),
    io:format("~s\n", [ccid_slot_fsm:pretty_print(sys:get_state(SlotPid))]);
dump(_) -> help([]).
