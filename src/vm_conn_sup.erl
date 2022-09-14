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

-module(vm_conn_sup).

-behaviour(supervisor).

-export([
    start_link/1,
    init/1,
    start_child/1,
    initial_listeners/1
    ]).

-spec start_link(Socket :: string()) -> {ok, pid()} | {error, term()}.
start_link(Socket) ->
    supervisor:start_link(?MODULE, [Socket]).

start_child(Sup) ->
    supervisor:start_child(Sup, []).

initial_listeners(Sup) ->
    [start_child(Sup) || _ <- lists:seq(1,8)],
    ok.

init([SockPath]) ->
    _ = file:delete(SockPath),
    {ok, ListenSock} = gen_tcp:listen(0, [
        {ip, {local, SockPath}},
        binary,
        {packet, 2}
    ]),
    spawn_link(?MODULE, initial_listeners, [self()]),
    ChildSpec = #{
        id          => vm_conn_fsm,
        start       => {vm_conn_fsm, start_link, [ListenSock, self()]},
        restart     => temporary
    },
    Flags = #{
        strategy    => simple_one_for_one,
        intensity   => 60,
        period      => 60
    },
    {ok, {Flags, [ChildSpec]}}.

