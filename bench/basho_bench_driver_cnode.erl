-module(basho_bench_driver_cnode).

-export([init/0,
         new/1,
         run/4]).

-record(state, {
            bucket :: atom()
        }).

init() ->
    {ok, _} = application:ensure_all_started(cnode),
    init(cnode:ready()).

init(true) -> ok;
init(false) ->
    timer:sleep(500),
    init(cnode:ready()).

new(Id) ->
    Bucket = list_to_atom("bucket" ++ integer_to_list(Id)),
    ok = cnode:new(Bucket),
    {ok, #state{bucket = Bucket}}.

run(get, KeyGen, _ValueGen,
    #state{bucket = Bucket} = State) ->
    Key = KeyGen(),
    case cnode:get(Bucket, Key) of
        {ok, _} -> {ok, State};
        not_found -> {error, not_found, State}
    end;
run(put, KeyGen, ValueGen,
    #state{bucket = Bucket} = State) ->
    Key = KeyGen(),
    Value = ValueGen(),
    case cnode:put(Bucket, Key, Value) of
        ok -> {ok, State};
        bucket_is_full -> {error, bucket_is_full, State}
    end;
run(delete, _KeyGen, _ValueGen,
    #state{bucket = Bucket} = State) ->
    cnode:delete(Bucket),
    {ok, State}.
