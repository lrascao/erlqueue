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
-module(erlqueue_test).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{<<"New works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024}]))
        end},
       {<<"Detection of double creation works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024}])),
            ?assertEqual({error, already_exists}, erlqueue:new(test, [{size, 1024}]))
        end},
       {<<"Queue/Dequeue works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024}])),
            ?assertEqual(ok, erlqueue:queue(test, hello)),
            ?assertEqual({ok, hello}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:queue(test, <<"hello">>)),
            ?assertEqual({ok, <<"hello">>}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:queue(test, <<"b/hello">>)),
            ?assertEqual({ok, <<"b/hello">>}, erlqueue:dequeue(test))
        end},
       {<<"Empty Dequeue works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024}])),
            ?assertEqual({error, not_found}, erlqueue:dequeue(test))
        end},
       {<<"Info works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024}])),
            ?assertEqual({ok, [{name, "test"},
                               {head, 0},
                               {tail, 0},
                               {size, 1024}]}, erlqueue:info(test))
        end},
       {<<"Inspect works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024}])),
            ?assertEqual({ok, [{valid, false},
                               {read, true}]}, erlqueue:inspect(test, 0)),
            ?assertEqual(ok, erlqueue:queue(test, hello)),
            ?assertEqual({ok, [{valid, true},
                               {read, false}]}, erlqueue:inspect(test, 0))
        end},
       {<<"Full queue works">>,
        fun() ->
            ?assertEqual({ok, test},  erlqueue:new(test, [{size, 64}])),
            ?assertEqual(ok, erlqueue:queue(test, hellohello)),
            ?assertEqual(ok, erlqueue:queue(test, hellohello)),
            ?assertEqual({error, queue_is_full}, erlqueue:queue(test, hellohello))
        end},
       {<<"Circular buffer works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024 * 2}])),
            N = 50000,
            lists:foreach(fun(I) ->
                            ?assertEqual(ok, erlqueue:queue(test, I)),
                            ?assertEqual({ok, I}, erlqueue:dequeue(test))
                          end, lists:seq(0, N))
        end},
       {<<"Roll around the buffer shouldn't crash">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 64}])),
            ?assertEqual(ok, erlqueue:queue(test, a1)),
            ?assertEqual(ok, erlqueue:queue(test, a12)),
            ?assertEqual({ok, a1}, erlqueue:dequeue(test)),
            ?assertEqual({ok, a12}, erlqueue:dequeue(test)),
            ?assertEqual({error, not_found}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:queue(test, a1234)),
            ?assertEqual({ok, a1234}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:queue(test, a12345)),
            ?assertEqual({ok, a12345}, erlqueue:dequeue(test)),
            ?assertEqual({error, not_found}, erlqueue:dequeue(test))
        end},
       {<<"Proper order">>,
        fun() ->
          ?assertEqual({ok, test}, erlqueue:new(test, [{size, 1024 * 2}])),
          L = lists:seq(0, 50),
          lists:foreach(fun(N) ->
                          ?assertEqual(ok, erlqueue:queue(test, N))
                        end, L),
          Output = lists:map(fun(_) ->
                              N = case erlqueue:dequeue(test) of
                                    {ok, N0} -> N0;
                                    {error, not_found} -> undefined
                                  end,
                              N
                             end, L),
          ?assertEqual(L, Output)
        end},
       {<<"Tricky queue full detection">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 64}])),
            ?assertEqual(ok, erlqueue:queue(test, a1)),
            ?assertEqual(ok, erlqueue:queue(test, a1)),
            ?assertEqual(ok, erlqueue:queue(test, a1)),
            ?assertEqual({error, queue_is_full}, erlqueue:queue(test, a2222222222222)),
            ?assertEqual({ok, a1}, erlqueue:dequeue(test)),
            ?assertEqual({ok, a1}, erlqueue:dequeue(test)),
            ?assertEqual({ok, a1}, erlqueue:dequeue(test)),
            ?assertEqual({error, not_found}, erlqueue:dequeue(test))
        end},
       {<<"Exceeding the total queue size doesn't crash">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, [{size, 64}])),
            ?assertEqual(ok, erlqueue:queue(test, an_atom)),
            ?assertEqual(ok, erlqueue:queue(test, <<"a binary">>)),
            ?assertEqual({error, queue_is_full}, erlqueue:queue(test, [{a, proplist},
                                                              {containing, <<"binaries">>}]))
        end}
       ]
    }.

setup() -> ok.

teardown(_) ->
  ?assertEqual(ok, erlqueue:delete(test)),
  ok.
