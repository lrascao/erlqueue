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
     [
       {<<"New works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, 1024)),
            ?assertEqual(ok, erlqueue:delete(test))
        end},
       {<<"Detection of double creation works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, 1024)),
            ?assertEqual({error, already_exists}, erlqueue:new(test, 1024)),
            ?assertEqual(ok, erlqueue:delete(test))
        end},
       {<<"Queue/Dequeue works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, 1024)),
            ?assertEqual(ok, erlqueue:queue(test, ola)),
            ?assertEqual({ok, ola}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:delete(test))
        end},
       {<<"Circular buffer works">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, 1024)),
            lists:foreach(fun(N) ->
                            ?assertEqual(ok, erlqueue:queue(test, N))
                          end, lists:seq(0, 400)),
            ?assertEqual(ok, erlqueue:delete(test))
        end},
       {<<"Roll around the buffer shouldn't crash">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, 64)),
            ?assertEqual(ok, erlqueue:queue(test, a1)),
            ?assertEqual(ok, erlqueue:queue(test, a12)),
            ?assertEqual(ok, erlqueue:queue(test, a123)),
            ?assertEqual({ok, a1}, erlqueue:dequeue(test)),
            ?assertEqual({ok, a12}, erlqueue:dequeue(test)),
            ?assertEqual({ok, a123}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:queue(test, a1234)),
            ?assertEqual({ok, a1234}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:queue(test, a12345)),
            ?assertEqual({ok, a12345}, erlqueue:dequeue(test)),
            ?assertEqual(ok, erlqueue:delete(test))
        end},
       {<<"Big bang">>,
        fun() ->
            ?assertEqual({ok, test}, erlqueue:new(test, 1024)),
            big:bang(100),
            ?assertEqual(ok, erlqueue:delete(test))
        end}
       ]
    }.

setup() -> ok.

teardown(_) -> ok.
