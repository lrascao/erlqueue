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
-module(big).

-export([bang/3]).

launch(N0) ->
    spawn(fun() ->
        lists:foreach(fun(N) ->
                        spawn(fun() ->
                            _ = erlqueue:queue(test, base64:encode(crypto:strong_rand_bytes(N)))
                        end)
                      end,
                      [crypto:rand_uniform(0, 200) || _X <- lists:seq(0, N0)]) end),
    spawn(fun() ->
        lists:foreach(fun(_) ->
                        spawn(fun() ->
                            _ = erlqueue:dequeue(test)
                        end)
                      end,
                      [crypto:rand_uniform(0, 200) || _X <- lists:seq(0, N0)])
    end).

bang(N, T, SecondsSleep) ->
    lists:foreach(fun(_) ->
                    launch(N)
                  end, lists:seq(1, T)),
    timer:sleep(SecondsSleep * 1000),
    ok.
