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
/**
 * C11 lock-free bounded shared memory queue. Supports multiple writers and multiple
 * readers. To simplify memory management queue users, data offered to
 * the queue are copied into the queue's buffers and copied back out
 * on retrieval.
 *
 * Queue functions return non-zero if the queue is full/empty.
 */
#ifndef LQUEUE_H
#define LQUEUE_H

#include <stddef.h>

#define N_INFO_FIELDS 4 // name, head, tail, size in lqueue_t

typedef struct {
#ifdef LSTATS
    lstats_t stats;
#endif
    char name[64];
    _Atomic unsigned int head;
    _Atomic unsigned int tail;
    size_t size;
    char buffer[];
} lqueue_t;

typedef unsigned short marker_t;

typedef struct {
    _Atomic marker_t marker;
    _Atomic unsigned int size;
} header_t;

#define VALID_MASK(position) (((marker_t)~position) >> 1)
#define IS_VALID(marker, position) ((marker & VALID_MASK(position)) == VALID_MASK(position))
#define SET_VALID(marker, position) (marker | VALID_MASK(position))

#define UNREAD_MASK (1 << ((sizeof(marker_t) * 8) - 1))
#define IS_UNREAD(marker) ((marker & UNREAD_MASK) == UNREAD_MASK)
#define IS_READ(marker) !IS_UNREAD(marker)
#define SET_UNREAD(marker) (marker | UNREAD_MASK)
#define SET_READ(marker) (marker & ~UNREAD_MASK)

lqueue_t *
lqueue_connect(char *name);

lqueue_t *
lqueue_create(char *name, size_t size);

void
lqueue_free(lqueue_t *);

int
lqueue_queue(lqueue_t *q, void *v, size_t size);

int
lqueue_dequeue(lqueue_t *q, void **v, size_t *size);

size_t
lqueue_byte_size(size_t size);

lstats_t *
lqueue_stats(lqueue_t *q);

void
lqueue_inspect(lqueue_t *q, unsigned int position, marker_t *marker);

#endif
