-module(basho_bench_driver_erlqueue).

-export([init/0,
         new/1,
         run/4]).

-record(state, {
            queue :: atom()
        }).

-define(SHARED_MEMORY_QUEUE_SIZE, 10 * 1024 * 1024).

init() ->
    application:ensure_all_started(erlqueue).

new(Id) ->
    Queue = list_to_atom("test" ++ integer_to_list(Id)),
    case erlqueue:get(Queue) of
        {error, no_queue} ->
            lager:debug("creating queue: ~p", [Queue]),
            {ok, Queue} = erlqueue:new(Queue, [{size, ?SHARED_MEMORY_QUEUE_SIZE}]);
        _ ->
            lager:debug("queue already created: ~p", [Queue]),
            ok
    end,
    {ok, #state{queue = Queue}}.

run(get, _KeyGen, _ValueGen,
    #state{queue = Queue} = State) ->
    case erlqueue:dequeue(Queue) of
        {ok, _} -> {ok, State};
        not_found -> {error, not_found, State}
    end;
run(put, _KeyGen, ValueGen,
    #state{queue = Queue} = State) ->
    Value = ValueGen(),
    case erlqueue:queue(Queue, Value) of
        ok -> {ok, State};
        queue_is_full -> {error, queue_is_full, State}
    end;
run(delete, _KeyGen, _ValueGen,
    #state{queue = Queue} = State) ->
    erlqueue:delete(Queue),
    {ok, State}.
