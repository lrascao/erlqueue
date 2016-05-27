#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "lqueue.h"

#define SHMEM_PREFIX "/tmp/lqueue.shm."

lqueue *
lqueue_create(char *name, size_t size)
{
    char filename[256];
    strcpy(filename, SHMEM_PREFIX);
    strcat(filename, name);
    FILE *fp = fopen(filename, "ab+");
    fclose(fp);
    int shmid = shmget(ftok(filename, 1), size, IPC_CREAT | 0666);
    if (shmid == -1)
        return NULL;
    lqueue *q = shmat(shmid, NULL, 0);
    if (q == (void *) -1)
        return NULL;

    q->head = ATOMIC_VAR_INIT(0);
    q->tail = ATOMIC_VAR_INIT(0);
    q->size = size;
    return q;
}

lqueue *
lqueue_connect(char *name)
{
    char filename[256];
    strcpy(filename, SHMEM_PREFIX);
    strcat(filename, name);
    int shmid = shmget(ftok(filename, 1), 0, 0);
    if (shmid == -1)
        return NULL;
    lqueue *q = shmat(shmid, NULL, 0);
    if (q == (void *) -1)
        return NULL;
    return q;
}

void
lqueue_free(lqueue *q)
{
    shmdt(q);
}

int
lqueue_queue(lqueue *q, void *v, size_t size)
{
    size_t tail = atomic_load(&q->tail);
    size_t next_tail;
    unsigned int tries = 0;
    unsigned short overflow;
    do {
        overflow = 0;
        tries++;
        next_tail = tail + sizeof(size_t) + size;
        /* if this write would exceed the buffer limits
           then reset and start from the top
        */
        if ((next_tail + sizeof(size_t)) > q->size) {
            next_tail = 0;
            overflow = 1;
        }
    } while (!atomic_compare_exchange_weak(&q->tail, &tail, next_tail));
    if (overflow) {
        /* set the full size of the buffer as this block's size
           so dequeue will see it and reset to the beginning
        */
        memcpy(q->buffer + tail, &q->size, sizeof(size_t));
        /* still we have to deal with this queue request
           so just try again
        */
        return lqueue_queue(q, v, size);
    } else {
        memcpy(q->buffer + tail, &size, sizeof(size_t));
        memcpy(q->buffer + tail + sizeof(size_t), v, size);
    }
    return 0;
}

int
lqueue_dequeue(lqueue *q, void **v, size_t *size)
{
    size_t head = atomic_load(&q->head);
    size_t tail = atomic_load(&q->tail);
    size_t next_head;
    unsigned int tries = 0;
    unsigned short overflow;
    do {
        if (head == tail)
            return 1;

        overflow = 0;
        tries++;
        memcpy(size, q->buffer + head, sizeof(size_t));
        next_head = head + sizeof(size_t) + *size;
        /* if this read would exceed the buffer limits
           then reset and start from the top
        */
        if (next_head > q->size) {
            next_head = 0;
            overflow = 1;
        }
    } while (!atomic_compare_exchange_weak(&q->head, &head, next_head));
    if (overflow)
        return lqueue_dequeue(q, v, size);
    else {
        *v = q->buffer + head + sizeof(size_t);
        return 0;
    }
}
