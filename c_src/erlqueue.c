/* -------------------------------------------------------------------
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
%% ------------------------------------------------------------------- */
#include "erl_nif.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "uthash.h"
#include "lstats.h"
#include "lqueue.h"

#define MAX_QUEUE_NAME 64

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_NOT_FOUND;
static ERL_NIF_TERM ATOM_ALREADY_EXISTS;
static ERL_NIF_TERM ATOM_SHMEM_CREATION_FAILED;
static ERL_NIF_TERM ATOM_NO_QUEUE;
static ERL_NIF_TERM ATOM_QUEUE_FULL;
static ERL_NIF_TERM ATOM_INVALID;

typedef struct {
  char name[64];
  lqueue_t *q;
  UT_hash_handle hh; /* makes this structure hashable */
} lqueue_hashed;

lqueue_hashed *qs = NULL;

/*********************************************************************/

static ERL_NIF_TERM
erl_mk_atom_prop_value(ErlNifEnv* env, const char *atom, ERL_NIF_TERM value) {
    ERL_NIF_TERM key = enif_make_atom(env, atom);
    return enif_make_tuple(env, 2, key, value);
}

/*********************************************************************/

static ERL_NIF_TERM
nif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2 || !enif_is_atom(env, argv[0]) ||
        !enif_is_number(env, argv[1])) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);
    int size;
    enif_get_int(env, argv[1], &size);

    /* first make sure there isn't already one queue with the same name */
    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed != NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_ALREADY_EXISTS);

    lqueue_t *q = lqueue_create(name, size);
    if (q == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_SHMEM_CREATION_FAILED);

    q_hashed = (lqueue_hashed *) malloc(sizeof(lqueue_hashed));
    q_hashed->q = q;
    strcpy(q_hashed->name, name);
    HASH_ADD_STR(qs, name, q_hashed);

    return enif_make_tuple2(env, ATOM_OK, argv[0]);
}

static ERL_NIF_TERM
nif_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1 || !enif_is_atom(env, argv[0])) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);

    /* first make sure there is a queue with the same name */
    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed == NULL)
      return ATOM_ERROR;

    HASH_DEL(qs, q_hashed);
    lqueue_free(q_hashed->q);
    free(q_hashed);

    return ATOM_OK;
}

static ERL_NIF_TERM
nif_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1 || !enif_is_atom(env, argv[0])) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);

    /* first make sure there isn't already one queue with the same name */
    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed != NULL)
      return enif_make_tuple2(env, ATOM_OK, argv[0]);

    lqueue_t *q = lqueue_connect(name);
    if (q == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NO_QUEUE);

    q_hashed = (lqueue_hashed *) malloc(sizeof(lqueue_hashed));
    q_hashed->q = q;
    strcpy(q_hashed->name, name);
    HASH_ADD_STR(qs, name, q_hashed);

    return enif_make_tuple2(env, ATOM_OK, argv[0]);
}

static ERL_NIF_TERM
nif_queue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (argc != 2 || !enif_is_atom(env, argv[0]) ||
        !enif_inspect_binary(env, argv[1], &bin)) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);

    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NO_QUEUE);

    if (lqueue_queue(q_hashed->q, (void*) bin.data, bin.size) == 1)
      return ATOM_QUEUE_FULL;
    return ATOM_OK;
}

static ERL_NIF_TERM
nif_dequeue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1 || !enif_is_atom(env, argv[0])) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);

    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NO_QUEUE);

    void *v;
    size_t size = 0;
    if (lqueue_dequeue(q_hashed->q, &v, &size) == 1)
      return ATOM_NOT_FOUND;

    ErlNifBinary bin;
    enif_alloc_binary(size, &bin);
    memcpy(bin.data, v, size);
    // ask the lqueue layer to release the pointer
    // it gave us
    lqueue_release(v, size);
    return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM
nif_byte_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (argc != 1 || !enif_inspect_binary(env, argv[0], &bin)) {
      return enif_make_badarg(env);
    }

    // get the actual byte size that this term takes up in the lqueue
    size_t byte_size = lqueue_byte_size(bin.size);
    if (byte_size == -1)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_INVALID);

    ERL_NIF_TERM byte_size_term = enif_make_int(env, byte_size);
    return enif_make_tuple(env, 2, ATOM_OK, byte_size_term);
}

static ERL_NIF_TERM
nif_stats(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef LSTATS
   if (argc != 1 || !enif_is_atom(env, argv[0])) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);

    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NO_QUEUE);

    lstats_t *stats = lqueue_stats(q_hashed->q);
    if (stats == NULL)
      return ATOM_NOT_FOUND;

    // convert the lstats_t struct to a proplist
    ERL_NIF_TERM prop_value0 = erl_mk_atom_prop_value(env, "queues",
                                                      enif_make_int(env, stats->n_queues));
    ERL_NIF_TERM prop_value1 = erl_mk_atom_prop_value(env, "dequeues",
                                                      enif_make_int(env, stats->n_dequeues));
    ERL_NIF_TERM prop_value2 = erl_mk_atom_prop_value(env, "overflows",
                                                      enif_make_int(env, stats->n_overflows));
    ERL_NIF_TERM prop_value3 = erl_mk_atom_prop_value(env, "queue_tries",
                                                      enif_make_int(env, stats->n_queue_tries));
    ERL_NIF_TERM prop_value4 = erl_mk_atom_prop_value(env, "dequeue_tries",
                                                      enif_make_int(env, stats->n_dequeue_tries));
    ERL_NIF_TERM prop_value5 = erl_mk_atom_prop_value(env, "queue_time",
                                                      enif_make_int(env, stats->queue_time_micros));
    ERL_NIF_TERM prop_value6 = erl_mk_atom_prop_value(env, "dequeue_time",
                                                      enif_make_int(env, stats->dequeue_time_micros));
    ERL_NIF_TERM prop_value7 = erl_mk_atom_prop_value(env, "max_queue_time",
                                                      enif_make_int(env, stats->max_queue_time_micros));
    ERL_NIF_TERM prop_value8 = erl_mk_atom_prop_value(env, "max_dequeue_time",
                                                      enif_make_int(env, stats->max_dequeue_time_micros));

    return enif_make_list(env, N_STATS, prop_value0, prop_value1, prop_value2, prop_value3,
                                        prop_value4, prop_value5, prop_value6, prop_value7,
                                        prop_value8);
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_INVALID);
#endif
}

static ERL_NIF_TERM
nif_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   if (argc != 1 || !enif_is_atom(env, argv[0])) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);

    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NO_QUEUE);

    lqueue_t *q = q_hashed->q;

    // convert some fields of the lqueue_t struct to a proplist
    ERL_NIF_TERM prop_value0 = erl_mk_atom_prop_value(env, "name",
                                                      enif_make_string(env, q->name, ERL_NIF_LATIN1));
    ERL_NIF_TERM prop_value1 = erl_mk_atom_prop_value(env, "head",
                                                      enif_make_int(env, q->head));
    ERL_NIF_TERM prop_value2 = erl_mk_atom_prop_value(env, "tail",
                                                      enif_make_int(env, q->tail));
    // deduct the header size that was added in lqueue_create to show consistency
    // to the user
    ERL_NIF_TERM prop_value3 = erl_mk_atom_prop_value(env, "size",
                                                      enif_make_int(env, q->size - sizeof(header_t)));

    return enif_make_list(env, N_INFO_FIELDS, prop_value0, prop_value1, prop_value2, prop_value3);
}

static ERL_NIF_TERM
nif_inspect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   if (argc != 2 || !enif_is_atom(env, argv[0])
                 || !enif_is_number(env, argv[1])) {
      return enif_make_badarg(env);
    }
    char name[MAX_QUEUE_NAME];
    enif_get_atom(env, argv[0], name, MAX_QUEUE_NAME, ERL_NIF_LATIN1);
    int position;
    enif_get_int(env, argv[1], &position);

    lqueue_hashed *q_hashed = NULL;
    HASH_FIND_STR(qs, name, q_hashed);
    if (q_hashed == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NO_QUEUE);

    marker_t marker = 0;
    lqueue_inspect(q_hashed->q, position, &marker);

    ERL_NIF_TERM prop_value0 = erl_mk_atom_prop_value(env, "valid",
                                    enif_make_atom(env,
                                                   IS_VALID(marker, position) ? "true" : "false"));
    ERL_NIF_TERM prop_value1 = erl_mk_atom_prop_value(env, "read",
                                    enif_make_atom(env,
                                                   IS_READ(marker) ? "true" : "false"));
    return enif_make_list(env, 2, prop_value0, prop_value1);
}

/*********************************************************************/

static void init(ErlNifEnv *env)
{
  ATOM_OK = enif_make_atom(env, "ok");
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_NOT_FOUND = enif_make_atom(env, "not_found");
  ATOM_ALREADY_EXISTS = enif_make_atom(env, "already_exists");
  ATOM_SHMEM_CREATION_FAILED = enif_make_atom(env, "shmem_creation_failed");
  ATOM_NO_QUEUE = enif_make_atom(env, "no_queue");
  ATOM_QUEUE_FULL = enif_make_atom(env, "queue_is_full");
  ATOM_INVALID = enif_make_atom(env, "invalid");
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  init(env);
  return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
                      ERL_NIF_TERM load_info)
{
  init(env);
  return 0;
}

static void on_unload(ErlNifEnv *env, void *priv_data)
{
}

/*********************************************************************/

static ErlNifFunc nif_funcs[] = {
  {"nif_new", 2, nif_new},
  {"nif_delete", 1, nif_delete},
  {"nif_get", 1, nif_get},
  {"nif_queue", 2, nif_queue},
  {"nif_dequeue", 1, nif_dequeue},
  {"nif_byte_size", 1, nif_byte_size},
  {"nif_stats", 1, nif_stats},
  {"nif_info", 1, nif_info},
  {"nif_inspect", 2, nif_inspect}
};

ERL_NIF_INIT(erlqueue, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
