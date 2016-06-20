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
#include <stdio.h>
#include <sys/time.h>

#include "lstats.h"

void
lstats_init(lstats_t *stats)
{
    stats->n_queues = 0;
    stats->n_dequeues = 0;
    stats->n_overflows = 0;
    stats->n_queue_tries = 0;
    stats->n_dequeue_tries = 0;
    stats->queue_time_micros = 0;
    stats->dequeue_time_micros = 0;
    stats->max_queue_time_micros = 0;
    stats->max_dequeue_time_micros = 0;
}

void
lstats_score(short stat, short value, lstats_t *stats)
{
    switch (stat) {
        case STAT_QUEUE:
            stats->n_queues += value;
            break;
        case STAT_DEQUEUE:
            stats->n_dequeues += value;
            break;
        case STAT_OVERFLOW:
            stats->n_overflows += value;
            break;
        case STAT_QUEUE_TRY:
            stats->n_queue_tries += value;
            break;
        case STAT_DEQUEUE_TRY:
            stats->n_dequeue_tries += value;
            break;
        case STAT_QUEUE_TIME_MICROS:
            stats->queue_time_micros += value;
            break;
        case STAT_DEQUEUE_TIME_MICROS:
            stats->dequeue_time_micros += value;
            break;
        case STAT_MAX_QUEUE_TIME_MICROS:
            if (value > stats->max_queue_time_micros)
                stats->max_queue_time_micros = value;
            break;
        case STAT_MAX_DEQUEUE_TIME_MICROS:
            if (value > stats->max_dequeue_time_micros)
                stats->max_dequeue_time_micros = value;
            break;
        default:
            break;
    }
}

unsigned long
lstats_time()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (tv.tv_sec * 1000000LL) + tv.tv_usec;
}

short
lstats_time_diff(unsigned long t0, unsigned long t1)
{
    return t1 - t0;
}

void
lstats_print(lstats_t *stats)
{
    printf("# queues: %d\n", stats->n_queues);
    printf("# dequeues: %d\n", stats->n_dequeues);
    printf("# overflows: %d\n", stats->n_overflows);
    printf("# queue try: %d\n", stats->n_queue_tries);
    printf("# dequeue try: %d\n", stats->n_dequeue_tries);
    printf("queue time (us): %ld\n", stats->queue_time_micros);
    printf("dequeue time (us): %ld\n", stats->dequeue_time_micros);
    printf("max queue time (us): %d\n", stats->max_queue_time_micros);
    printf("max dequeue time (us): %d\n", stats->max_dequeue_time_micros);
}
