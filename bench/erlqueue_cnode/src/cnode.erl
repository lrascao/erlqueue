% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(cnode).
-behaviour(gen_server).

-export([start_link/0]).
-export([connect/2,
         ready/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(CNODE, "priv/cnode").
-define(INIT_TIMEOUT, 3000).

-record(state, {
    port,
    cnode,
    init_ref,
    init_timer = undefined,
    initialized = false
}).

%% ----------------------------------------------------------
%% Public API
%% ----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect(Direction, {queue, Id}) ->
    gen_server:call(?MODULE, {qconnect, {Direction, Id}});
connect(Direction, {semaphore, Id}) ->
    gen_server:call(?MODULE, {sconnect, {Direction, Id}}).

ready() ->
    case whereis(?MODULE) of
        undefined -> false;
        _ ->
            gen_server:call(?MODULE, ready)
    end.

%% ------------------------------------------------------------------
%% Behaviour Function Exports
%% ------------------------------------------------------------------
init(_) ->
    CNodeName = "cnode" ++ integer_to_list(crypto:rand_uniform(1, 10000)),
    CNodeHost = lists:nth(2, string:tokens(atom_to_list(node()), [$@])),
    CNodeFullName = list_to_atom(CNodeName ++ "@" ++ CNodeHost),
    Port = erlang:open_port({spawn_executable, [filename:join(code:lib_dir(erlqueue_cnode), ?CNODE)]},
    % Port = erlang:open_port({spawn_executable, [?CNODE]},
                             [binary,
                              {args, [CNodeName, CNodeHost,
                                      erlang:get_cookie()]}]),
    erlang:process_flag(trap_exit, true),
    true = erlang:link(Port),
    %% cast a message to self for initialization purposes
    Ref = erlang:make_ref(),
    Timer = erlang:send_after(?INIT_TIMEOUT, self(), init_timeout),
    gen_server:cast(self(), {init, {Ref}}),
    {ok, #state { port = Port,
                  cnode = CNodeFullName,
                  init_ref = Ref,
                  init_timer = Timer
                }}.

handle_cast({reply, _From, {inited, Ref}},
            #state{ cnode = Cnode,
                    init_timer = Timer,
                    init_ref = Ref } = State) ->
    lager:debug("cnode ~p has been initialized", [Cnode]),
    _ = erlang:cancel_timer(Timer),
    {noreply, State#state{ init_timer = undefined,
                           initialized = true }};
handle_cast({reply, From, Reply}, State) ->
    gen_server:reply(From, Reply),
    {noreply, State};
handle_cast({init, _Ref} = Msg, #state{cnode = Cnode} = State) ->
    call(Cnode, undefined, Msg),
    {noreply, State};
handle_cast(Msg, #state{cnode = Cnode} = State) ->
    % lager:debug("cast to cnode ~p: ~p",
    %     [Cnode, Msg]),
    cast(Cnode, Msg),
    {noreply, State}.

handle_call(ready = _Msg, _From,
            #state{initialized = Initialized} = State) ->
    {reply, Initialized, State};
handle_call(Msg, From, #state{cnode = Cnode} = State) ->
    % lager:debug("call to cnode ~p: ~p",
    %     [Cnode, Msg]),
    call(Cnode, From, Msg),
    {noreply, State}.

handle_info(init_timeout, #state{cnode = Cnode} = State) ->
    lager:debug("timed out waiting for ~p initialization, re-requesting",
        [Cnode]),
    Ref = erlang:make_ref(),
    Timer = erlang:send_after(?INIT_TIMEOUT, self(), init_timeout),
    gen_server:cast(self(), {init, {Ref}}),
    {noreply, State#state { init_ref = Ref,
                            init_timer = Timer }};
handle_info({'EXIT', Port, normal}, #state{port = Port} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) -> ok;
terminate(Reason, _State) ->
    lager:error("cnode died with reason ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cast(Cnode, Msg) ->
    {undefined, Cnode} ! {cast, Msg}.

call(Cnode, From, Msg) ->
    {undefined, Cnode} ! {call, From, Msg}.
