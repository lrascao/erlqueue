/**
 * C11 lock-free bounded queue. Supports multiple writers and multiple
 * readers. To simplify memory management queue users, data offered to
 * the queue are copied into the queue's buffers and copied back out
 * on retrieval.
 *
 * Queue functions return non-zero if the queue is full/empty.
 */
#ifndef LQUEUE_H
#define LQUEUE_H

#include <stddef.h>

typedef struct {
    _Atomic size_t head;
    _Atomic size_t tail;
    size_t size;
    char buffer[];
} lqueue;

lqueue *lqueue_connect(char *name);
lqueue *lqueue_create(char *name, size_t size);
void lqueue_free(lqueue *);
int lqueue_queue(lqueue *q, void *v, size_t size);
int lqueue_dequeue(lqueue *q, void **v, size_t *size);

#endif
