%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Luis Rascão.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(erlqueue).

-on_load(init/0).

-export([new/2,
         delete/1,
         get/1,
         queue/2,
         dequeue/1,
         byte_size/1,
         stats/1,
         info/1,
         inspect/2]).

-define(BINARY_MARKER, "/b").

-type new_opts() :: proplists:proplist().
-export_type([new_opts/0]).

-spec new(Name :: atom(),
          Opts :: new_opts()) ->
        {error, already_exists | shmem_creation_failed} | {ok, atom()}.
new(Name, Opts) ->
    Size = proplists:get_value(size, Opts),
    nif_new(Name, Size).

-spec delete(Name :: atom()) -> {error, not_found} | {ok, atom()}.
delete(Name) ->
    nif_delete(Name).

-spec get(Name :: atom()) -> {error, no_queue} | {ok, atom()}.
get(Name) ->
    nif_get(Name).

-spec queue(Name :: atom(), Term :: term()) -> {error, no_queue} | ok.
queue(Name, Bin) when is_binary(Bin) ->
    nif_queue(Name, << ?BINARY_MARKER, Bin/binary>>);
queue(Name, Term) ->
    nif_queue(Name, term_to_binary(Term)).

-spec dequeue(Name :: atom()) -> {error, no_queue | not_found} | {ok, term()}.
dequeue(Name) ->
    case nif_dequeue(Name) of
        {error, _} = Error -> Error;
        << ?BINARY_MARKER, Bin/binary>> ->
            {ok, Bin};
        T -> {ok, binary_to_term(T)}
    end.

-spec byte_size(Term :: term()) -> {ok, non_neg_integer()} | {error, invalid}.
byte_size(Term) ->
    nif_byte_size(term_to_binary(Term)).

-spec stats(Name :: atom()) -> {error, not_found} | {ok, proplists:proplist()}.
stats(Name) ->
    case nif_stats(Name) of
        {error, _} = Error -> Error;
        T -> {ok, T}
    end.

-spec info(Name :: atom()) -> {error, not_found} | {ok, proplists:proplist()}.
info(Name) ->
    case nif_info(Name) of
        {error, _} = Error -> Error;
        T -> {ok, T}
    end.

-spec inspect(Name :: atom(),
              Position :: non_neg_integer()) -> {error, not_found} | {ok, proplists:proplist()}.
inspect(Name, Position) ->
    case nif_inspect(Name, Position) of
        {error, _} = Error -> Error;
        T -> {ok, T}
    end.

nif_new(_, _) ->
    erlang:nif_error({error, not_loaded}).

nif_delete(_) ->
    erlang:nif_error({error, not_loaded}).

nif_get(_) ->
    erlang:nif_error({error, not_loaded}).

nif_queue(_, _) ->
    erlang:nif_error({error, not_loaded}).

nif_dequeue(_) ->
    erlang:nif_error({error, not_loaded}).

nif_byte_size(_) ->
    erlang:nif_error({error, not_loaded}).

nif_stats(_) ->
    erlang:nif_error({error, not_loaded}).

nif_info(_) ->
    erlang:nif_error({error, not_loaded}).

nif_inspect(_, _) ->
    erlang:nif_error({error, not_loaded}).

init() ->
    case code:priv_dir(erlqueue) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename), "../priv", "erlqueue"]);
                _ ->
                    SoName = filename:join("../priv", "erlqueue")
            end;
        Dir ->
            SoName = filename:join(Dir, "erlqueue")
    end,
    erlang:load_nif(SoName, 0).
