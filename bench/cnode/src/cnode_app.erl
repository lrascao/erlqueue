-module(cnode_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([config_change/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cnode_sup:start_link().

stop(_State) ->
    ok.

%% http://www.erlang.org/doc/design_principles/release_handling.html
%% After the installation, the application controller compares the old and new
%% configuration parameters for all running applications and calls the callback function:
config_change(Changed, New, Removed) ->
    lager:debug("changed: ~p", [Changed]),
    lager:debug("new: ~p", [New]),
    lager:debug("removed: ~p", [Removed]),
    ok.
