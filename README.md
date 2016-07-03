# erlqueue

[erlqueue](https://github.com/lrascao/erlqueue) is a multiple writer, multiple reader bounded shared memory lock-free queue API for Erlang (through NIF) and C/C++.

[![Build Status](https://travis-ci.org/lrascao/erlqueue.svg?branch=develop)](https://travis-ci.org/lrascao/erlqueue)

## Introduction

Communication between the Erlang VM and [Cnodes](http://erlang.org/doc/tutorial/cnode.html) and [Cports](http://erlang.org/doc/tutorial/c_port.html) is usually through a TCP connection (in the case of Cnodes) or pipes (in the case of Cports), this package offers another alternative with a significant speedup relative to the pipe/TCP alternative.

## Build

    $ git clone https://github.com/lrascao/erlqueue.git
    $ cd erlqueue
    $ make

## Getting Started

Start an Erlang console with `erlqueue` running:

    $ make shell

The Erlang API is as follows:

```erlang
    %%
    %%
    -spec erlqueue:new(Name :: atom(), Opts :: proplists:proplist())
                -> {ok, Name :: atom()} | .
    * erlqueue:queue(Name :: atom(), Data :: term()) -> ok.
    * erlqueue:dequeue(Name :: atom()) -> {ok, term()}.
```

```erlang
% create a new queue named test with 1024 bytes
> erlqueue:new(test, [{size, 1024}]).
{ok,test}
```

Now that we have our shared memory queue of size 1024 bytes, let's queue some data on it:

```erlang
> erlqueue:queue(test, an_atom).
ok
> erlqueue:queue(test, <<"a binary">>).
ok
> erlqueue:queue(test, [{a, proplist}, {containing, <<"binaries">>}]).
ok
```

And of course, dequeue it:

```erlang
> erlqueue:dequeue(test).
{ok,an_atom}
> erlqueue:dequeue(test).
{ok,<<"a binary">>}
> erlqueue:dequeue(test).
{ok,[{a,proplist},{containing,<<"binaries">>}]}
% the next dequeue will return `not_found` since the queue is now empty
> erlqueue:dequeue(test).
{error,not_found}
%% finally delete the queue
> erlqueue:delete(test).
ok
```

Let's create a smaller queue to see what happens when space runs out:

```erlang
% request a small 64 byte sized queue
> erlqueue:new(test, [{size, 64}]).
{ok,test}
% determine the byte size of a term
> erlqueue:byte_size(an_atom).
{ok,19}
> erlqueue:queue(test, an_atom1).
ok
> erlqueue:queue(test, an_atom2).
ok
> erlqueue:queue(test, an_atom3).
ok
> erlqueue:queue(test, an_atom4).
{error,queue_is_full}
```

As expected we were able to insert 3 blocks of 19 bytes each (19 * 3 = 57) and unable to
insert a fourth, moving on:

```erlang
% as expected we get an_atom1 which is at the top of the queue
> erlqueue:dequeue(test).
{ok, an_atom1}
% we're now able to write since dequeue freed some space for us
> erlqueue:queue(test, an_atom4).
ok
% dequeue the rest of the elements in the expected order
> erlqueue:dequeue(test).
{ok, an_atom2}
> erlqueue:dequeue(test).
{ok, an_atom3}
> erlqueue:dequeue(test).
{ok, an_atom4}
> erlqueue:dequeue(test).
{error,not_found}
> erlqueue:queue(test, <<"a binary">>).
ok
```

On the C side of things (Cnode or Cport) a similar API is available by linking with liblqueue.a:

```c

```


## Benchmarks

## Copyright and License

Copyright (c) 2016 Luis Miguel Mourato Rascão

**erlqueue** source code is licensed under the [Apache License 2.0](LICENSE.md).
