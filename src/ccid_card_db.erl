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

-module(ccid_card_db).

-behaviour(gen_server).

-export([
    start_link/0,
    lookup/2,
    register/2
    ]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_info/2,
    handle_cast/2
    ]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec lookup(string(), integer()) ->
    {ok, pid()} | {error, not_found} | {error, term()}.
lookup(Name, Idx) ->
    gen_server:call(?MODULE, {lookup, Name, Idx}, infinity).

-spec register(string(), integer()) ->
    ok | {error, already_registered} | {error, term()}.
register(Name, Idx) ->
    gen_server:call(?MODULE, {register, Name, Idx, self()}, infinity).

-record(?MODULE, {
    fsms = #{} :: #{string() => pid()},
    mons = #{} :: #{reference() => string()}
    }).

init([]) ->
    {ok, #?MODULE{}}.

terminate(_Why, #?MODULE{}) ->
    ok.

handle_call({lookup, Name, Idx}, _From, S0 = #?MODULE{fsms = R0}) ->
    case R0 of
        #{{Name, Idx} := Pid} ->
            {reply, {ok, Pid}, S0};
        _ ->
            {reply, {error, not_found}, S0}
    end;

handle_call({register, Name, Idx, Pid}, _From, S0 = #?MODULE{fsms = R0,
                                                         mons = M0}) ->
    case R0 of
        #{{Name, Idx} := _} ->
            {reply, {error, already_registered}, S0};
        _ ->
            R1 = R0#{{Name, Idx} => Pid},
            MRef = erlang:monitor(process, Pid),
            M1 = M0#{MRef => {Name, Idx}},
            S1 = S0#?MODULE{fsms = R1, mons = M1},
            {reply, ok, S1}
    end.

handle_info({'DOWN', MRef, process, Pid, _Why}, S0 = #?MODULE{fsms = R0,
                                                             mons = M0}) ->
    case M0 of
        #{MRef := Key} ->
            #{Key := Pid} = R0,
            R1 = maps:remove(Key, R0),
            M1 = maps:remove(MRef, M0),
            S1 = S0#?MODULE{fsms = R1, mons = M1},
            {noreply, S1};
        _ ->
            {noreply, S0}
    end.

handle_cast(_, #?MODULE{}) ->
    error(no_cast).
