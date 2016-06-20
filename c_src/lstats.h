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

#define STAT_QUEUE 0
#define STAT_DEQUEUE 1
#define STAT_OVERFLOW 2
#define STAT_QUEUE_TRY 3
#define STAT_DEQUEUE_TRY 4
#define STAT_QUEUE_TIME_MICROS 5
#define STAT_DEQUEUE_TIME_MICROS 6
#define STAT_MAX_QUEUE_TIME_MICROS 7
#define STAT_MAX_DEQUEUE_TIME_MICROS 8

#ifdef LSTATS
#   define STAT_SCORE(stat, stats) lstats_score(stat, 1, stats)
#   define STAT_VALUE_SCORE(stat, value, stats) lstats_score(stat, value, stats)
#   define STAT_TIME() unsigned long __stats_start = lstats_time()
#   define STAT_TIME_DIFF() lstats_time_diff(__stats_start, lstats_time())
#else
#   define STAT_SCORE(stat, stats)
#   define STAT_VALUE_SCORE(stat, value, stats)
#   define STAT_TIME()
#   define STAT_TIME_DIFF()
#endif

#define N_STATS 9
typedef struct {
    unsigned int n_queues;
    unsigned int n_dequeues;
    unsigned int n_overflows;
    unsigned int n_queue_tries;
    unsigned int n_dequeue_tries;
    unsigned long queue_time_micros;
    unsigned long dequeue_time_micros;
    unsigned short max_queue_time_micros;
    unsigned short max_dequeue_time_micros;
} lstats_t;

void
lstats_init(lstats_t *stats);

void
lstats_score(short stat, short value, lstats_t *stats);

unsigned long
lstats_time();

short
lstats_time_diff(unsigned long t0, unsigned long t1);

void
lstats_print(lstats_t *stats);
