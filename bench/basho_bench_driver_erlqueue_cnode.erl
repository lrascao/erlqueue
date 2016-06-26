-module(basho_bench_driver_erlqueue_cnode).

-export([init/0,
         new/1,
         run/4]).

-record(state, {
            in_queue :: atom(),
            in_semaphore :: atom(),
            out_queue :: atom(),
            out_semaphore :: atom()
        }).

-define(SHARED_MEMORY_QUEUE_SIZE, 10 * 1024 * 1024).

init() ->
    application:ensure_all_started(cnode),
    init(cnode:ready()).

init(true) -> ok;
init(false) ->
    timer:sleep(500),
    init(cnode:ready()).

new(Id) ->
    %% create our in queue
    InQueue = list_to_atom("test_queue_in" ++ integer_to_list(Id)),
    lager:debug("creating queue: ~p", [InQueue]),
    {ok, InQueue} = erlqueue:new(InQueue, [{size, ?SHARED_MEMORY_QUEUE_SIZE}]),
    %% ask the cnode to connect to the in queue which for it is the out queue
    ok = cnode:connect(out, {queue, InQueue}),
    %% create our out queue
    OutQueue = list_to_atom("test_queue_out" ++ integer_to_list(Id)),
    lager:debug("creating queue: ~p", [OutQueue]),
    {ok, OutQueue} = erlqueue:new(OutQueue, [{size, ?SHARED_MEMORY_QUEUE_SIZE}]),
    %% ask the cnode to connect to the out queue which for it is the in queue
    ok = cnode:connect(in, {queue, OutQueue}),

    %% create the in semaphore
    InSemaphore = list_to_atom("test_sem_in" ++ integer_to_list(Id)),
    lager:debug("creating semaphore: ~p", [InSemaphore]),
    {ok, InSemaphore} = erlsemaphore:new(InSemaphore),
    %% ask the cnode to connect to the in semaphore which for it is the in semaphore
    ok = cnode:connect(out, {semaphore, InSemaphore}),
    %% create the out semaphore
    OutSemaphore = list_to_atom("test_sem_out" ++ integer_to_list(Id)),
    lager:debug("creating semaphore: ~p", [OutSemaphore]),
    {ok, OutSemaphore} = erlsemaphore:new(OutSemaphore),
    %% ask the cnode to connect to the in semaphore which for it is the in semaphore
    ok = cnode:connect(in, {semaphore, OutSemaphore}),

    {ok, #state{ in_queue = InQueue,
                 out_queue = OutQueue,
                 in_semaphore = InSemaphore,
                 out_semaphore = OutSemaphore }}.

run(wait, _KeyGen, _ValueGen,
    #state{in_queue = InQueue,
           in_semaphore = InSemaphore} = State) ->
    %% wait until a resource becomes available
    case erlsemaphore:wait(InSemaphore, 1, [{no_wait, true}]) of
        ok ->
            case erlqueue:dequeue(InQueue) of
                {ok, _} ->
                    {ok, State};
                not_found -> {error, not_found, State}
            end;
        {error, would_block} ->
            {error, would_block, State}
    end;
run(signal, _KeyGen, ValueGen,
    #state{out_queue = OutQueue,
           out_semaphore = OutSemaphore} = State) ->
    Value = ValueGen(),
    case erlqueue:queue(OutQueue, Value) of
        ok ->
            %% make a resource available
            ok = erlsemaphore:signal(OutSemaphore, 1),
            {ok, State};
        queue_is_full ->
            {error, queue_is_full, State}
    end.
