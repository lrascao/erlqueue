/* cnode.c */
// {any, 'test2@Luis-Rascao-MPT.miniclip.global'} ! ping
// [begin {any, 'test2@Luis-Rascao-MPT.miniclip.global'} ! ping,
//                receive pong -> ok end end || N <- lists:seq(0, 2000)].

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <unistd.h>
#include <poll.h>
#include <sys/timeb.h>
#include <stdarg.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <pthread.h>

#include "erl_interface.h"
#include "ei.h"

#include "uthash.h"
#include "lqueue.h"

#define SEMAPHORE_PREFIX "/tmp/erlsemaphore.sem."

typedef ETERM* (*command_fptr_)(ETERM *, ETERM *);
typedef struct {
    char name[64];
    command_fptr_ command;
    UT_hash_handle hh; /* makes this structure hashable */
} command_t;

typedef struct {
    int fd;
    ErlConnect *conn;
    UT_hash_handle hh; /* makes this structure hashable */
} client_t;

typedef struct {
    int eventfd;
} worker_init_t;

client_t *clients = NULL;
client_t *current_client = NULL;
command_t *commands = NULL;

lqueue_t *queue_in = NULL;
lqueue_t *queue_out = NULL;
int semaphore_in = 0;
int semaphore_out = 0;
worker_init_t worker_init;

#define BUFSIZE 2048

/* uncomment the next line to enable debug print */
#define DEBUG

#ifdef DEBUG
#   undef DEBUGF
#   define DEBUGF(P) debugf P
#else
#   define DEBUGF(P)
#endif

void debugf(char *str, ...)
{
  va_list ap;
  va_start(ap,str);
  fprintf(stderr,"[cnode] : ");
  vfprintf(stderr,str, ap);
  fprintf(stderr,"\r\n");
  va_end(ap);
}

static ETERM *init(ETERM *fromp, ETERM *argp);
static ETERM *qconnect(ETERM *fromp, ETERM *argp);
static ETERM *sconnect(ETERM *fromp, ETERM *argp);
static void *queue_worker(void *arg);

static void init_commands() {
    command_t _commands[] = {
        { .name = "init", .command = init },
        { .name = "qconnect", .command = qconnect },
        { .name = "sconnect", .command = sconnect },
        { .name = "", .command = NULL }
    };

    int i = 0;
    while (1) {
        /* end of the command array */
        if (_commands[i].command == NULL)
            break;

        command_t *cmd = (command_t *) malloc(sizeof(command_t));
        memcpy(cmd, &_commands[i], sizeof(command_t));
        HASH_ADD_STR(commands, name, cmd);
        i++;
    }
}

ETERM *erl_mk_reply(ETERM *fromp, ETERM *reply) {

    ETERM *atom_reply = erl_mk_atom("reply");
    ETERM **reply_array = (ETERM **) malloc(sizeof(ETERM*) * 3);
    reply_array[0] = atom_reply;
    reply_array[1] = fromp;
    reply_array[2] = reply;
    ETERM *reply_tuple = erl_mk_tuple(reply_array, 3);
    free(reply_array);
    return reply_tuple;
}

ETERM *erl_mk_gen_cast(ETERM *arg) {
    ETERM *gen_cast_atom = erl_mk_atom("$gen_cast");
    ETERM **gen_cast_tuple_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    gen_cast_tuple_array[0] = gen_cast_atom;
    gen_cast_tuple_array[1] = arg;
    ETERM *gen_cast_tuple = erl_mk_tuple(gen_cast_tuple_array, 2);
    free(gen_cast_tuple_array);
    return gen_cast_tuple;
}

int tcp_listen(int *port) {
    int fd;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    int on = 1;

    if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        return -1;

    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

    memset((void*) &addr, 0, (size_t) sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(*port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
        return -1;

    if (getsockname(fd, (struct sockaddr *)&addr, &len) == -1)
        return -1;

    *port = ntohs(addr.sin_port);

    listen(fd, 50);

    return fd;
}

void add_client(int fd, ErlConnect *conn) {
    client_t *client = (client_t *) malloc(sizeof(client_t));
    client->fd = fd;
    client->conn = conn;
    HASH_ADD_INT(clients, fd, client);
}

void remove_client(client_t *client) {
    HASH_DEL(clients, client);
    free(client->conn);
    free(client);
}

struct pollfd *
get_clients_to_poll(int listen_fd, int event_fd, int *nfds) {
    static struct pollfd *poll_fds = NULL;
    static int n_poll_fds = 0;

    // total number of clients plus the accept and event socket
    int n_clients = HASH_COUNT(clients) + 2;
    if (n_poll_fds != n_clients) {
        if (poll_fds != NULL)
            free(poll_fds);
        poll_fds = malloc(sizeof(struct pollfd) * n_clients);
        n_poll_fds = n_clients;
    }

    // the zero slot is reserved for the accept socket
    poll_fds[0].fd = listen_fd;
    poll_fds[0].events = POLLIN;
    // the next slot is reserved for the event descriptor
    poll_fds[1].fd = event_fd;
    poll_fds[1].events = POLLIN;
    int n = 2;
    for(client_t *client = clients; client != NULL; client = client->hh.next) {
        poll_fds[n].fd = client->fd;
        poll_fds[n].events = POLLIN;
        n++;
    }

    *nfds = n_clients;
    return poll_fds;
}

void
set_current_client(client_t *client) {
    current_client = client;
}

static ETERM* run_command(char *name, ETERM *fromp, ETERM *argp) {
    command_t *c;

    HASH_FIND_STR(commands, name, c);
    if (c)
        return (c->command)(fromp, argp);
    return NULL;
}

static ETERM *command(ETERM *command, ETERM *fromp, ETERM *argp)
{
    ETERM *resp = NULL;
    char *command_str = NULL;

    command_str = ERL_ATOM_PTR(command);
    resp = run_command(command_str, fromp, argp);

    return resp;
}

void handle_cast(ETERM *message) {

    ETERM *commandp = erl_element(1, message);
    ETERM *argp = erl_element(2, message);
    command(commandp, NULL, argp);
}

ETERM *handle_call(ETERM *fromp, ETERM *message) {

    ETERM *commandp = erl_element(1, message);
    ETERM *argp = erl_element(2, message);
    return command(commandp, fromp, argp);
}

void
handle_queued_message()
{
    void *v;
    size_t size = 0;

    if (lqueue_dequeue(queue_in, &v, &size) != 0)
        return;

    // DEBUGF(("dequeued %d bytes", size));

    // now enqueue the same data in our out queue
    if (lqueue_queue(queue_out, v, size) != 0)
        return;

    // scrub the data that we just read
    // this is an important bit since it's thie srubbing
    // that will prevent a future queue to end up somewhere
    // along this buffer and mistakenly interpret random bytes
    // as a valid header
    memset(v, 0, size);

    // and signal the semaphore
     // http://www.tldp.org/LDP/lpg/node52.html#SECTION00743400000000000000
    struct sembuf op;
    op.sem_num = 0;
    // If sem_op is negative, then its value is subtracted from the semaphore.
    // This correlates with obtaining resources that the semaphore controls or monitors access of.
    // If IPC_NOWAIT is not specified, then the calling process sleeps until the requested
    // amount of resources are available in the semaphore (another process has released some).
    op.sem_op = 1;
    op.sem_flg = 0;
    if (semop(semaphore_out, &op, 1) == -1) {
        DEBUGF(("failed to signal semaphore %d", semaphore_in));
        return;
    }
}

void
handle_message(client_t *client) {
    int got;                                 /* Result of receive */
    unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
    ErlMessage emsg;                         /* Incoming message */

    got = erl_receive_msg(client->fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
        // DEBUGF(("tick"));
        /* ignore */
        return;
    }

    if (got == ERL_ERROR) {
        DEBUGF(("error receiving message"));
        remove_client(client);
        return;
    }

    if (emsg.type != ERL_REG_SEND) {
        DEBUGF(("got something other than a ERL_REG_SEND: %d\n", emsg.type));
        return;
    }

    set_current_client(client);

    /* {cast, {Command, Args}} | {call, From, {Command, Args}} */
    char *message_type = ERL_ATOM_PTR(erl_element(1, emsg.msg));

    if (strcmp(message_type, "cast") == 0) {
        ETERM *message = erl_element(2, emsg.msg);
        handle_cast(message);
    } else if (strcmp(message_type, "call") == 0) {
        ETERM *from = erl_element(2, emsg.msg);
        ETERM *message = erl_element(3, emsg.msg);
        ETERM *resp = handle_call(from, message);
        if (resp != NULL) {
            if (erl_send(client->fd, emsg.from, resp) != 1) {
                DEBUGF(("failed to send reply to client %d\n", client->fd));
            }
            erl_free_compound(resp);
        }
    }

    erl_free_compound(emsg.from);
    erl_free_compound(emsg.msg);
}

void
handle_clients(int listen_fd, int event_fd, struct pollfd *fds, int nfds, int n_set_fds, ei_cnode *ec) {

    int n_checked_fds = 0;
    for(int i = 0; i < nfds; i++) {
        // only care about data coming in
        if (!fds[i].revents & POLLIN)
            continue;

        if (fds[i].fd == listen_fd) {
            // accept the connection and add to the client list
            ErlConnect *conn = malloc(sizeof(ErlConnect));
            int fd = ei_accept(ec, listen_fd, conn);
            if (fd == ERL_ERROR) {
                DEBUGF(("ei_accept failed: %d", fd));
                return;
            }
            DEBUGF(("Connected to %s", conn->nodename));
            add_client(fd, conn);
        }
        else if (fds[i].fd == event_fd) {
            // read off the data
            unsigned char signal;
            if (read(event_fd, &signal, sizeof(signal)) != sizeof(signal)) {
                DEBUGF(("error reading from event descriptor"));
            }
            handle_queued_message();
        } else {
            // fds[i].fd is set
            client_t *client = NULL;
            HASH_FIND_INT(clients, &fds[i].fd, client);

            handle_message(client);
        }
        n_checked_fds++;

        if (n_checked_fds == n_set_fds)
            return;
    }
}

static ETERM *init(ETERM *fromp, ETERM *argp) {
    ETERM **inited_array;
    ETERM *ref, *atom_inited, *inited_tuple;

    ref = erl_element(1, argp);
    if (! ERL_IS_REF(ref))
        return NULL;

    atom_inited = erl_mk_atom("inited");

    inited_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    inited_array[0] = atom_inited;
    inited_array[1] = ref;
    inited_tuple = erl_mk_tuple(inited_array, 2);
    free(inited_array);

    ETERM *reply_tuple = erl_mk_reply(fromp, inited_tuple);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    DEBUGF(("init has succeeded"));
    return gen_cast_tuple;
}

static ETERM *qconnect(ETERM *fromp, ETERM *argp) {
    ETERM *atom_ok = erl_mk_atom("ok");
    char name[64];
    short queue_in_direction = 0;

    ETERM *term_direction = erl_element(1, argp);
    if (strcmp(ERL_ATOM_PTR(term_direction), "in") == 0)
        queue_in_direction = 1;
    ETERM *term_queue = erl_element(2, argp);
    if (ERL_IS_ATOM(term_queue))
        sprintf(name, "%s", ERL_ATOM_PTR(term_queue));

    lqueue_t *q = lqueue_connect(name);
    if (q == NULL)
        return NULL;

    if (queue_in_direction)
        queue_in = q;
    else
        queue_out = q;

    ETERM *reply_tuple = erl_mk_reply(fromp, atom_ok);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    return gen_cast_tuple;
}

static ETERM *sconnect(ETERM *fromp, ETERM *argp) {
    ETERM *atom_ok = erl_mk_atom("ok");
    char name[64];
    short semaphore_in_direction = 0;

    ETERM *term_direction = erl_element(1, argp);
    if (strcmp(ERL_ATOM_PTR(term_direction), "in") == 0)
        semaphore_in_direction = 1;
    ETERM *term_semaphore = erl_element(2, argp);
    if (ERL_IS_ATOM(term_semaphore))
        sprintf(name, "%s", ERL_ATOM_PTR(term_semaphore));

    char filename[256];
    strcpy(filename, SEMAPHORE_PREFIX);
    strcat(filename, name);
    FILE *fp = fopen(filename, "ab+");
    fclose(fp);

    int semid = semget(ftok(filename, 1), 0, 0);
    if (semid == -1)
        return NULL;

    if (semaphore_in_direction) {
        semaphore_in = semid;

        // start up the thread that will be waiting on the semaphore
        pthread_t queue_thread;
        pthread_create(&queue_thread, NULL, queue_worker, &worker_init);
    }
    else
        semaphore_out = semid;

    ETERM *reply_tuple = erl_mk_reply(fromp, atom_ok);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    return gen_cast_tuple;
}

static void *
queue_worker(void *arg)
{
    worker_init_t *worker_init = arg;
    DEBUGF(("starting up queue worker"));
     // http://www.tldp.org/LDP/lpg/node52.html#SECTION00743400000000000000
    struct sembuf op;
    op.sem_num = 0;
    // If sem_op is negative, then its value is subtracted from the semaphore.
    // This correlates with obtaining resources that the semaphore controls or monitors access of.
    // If IPC_NOWAIT is not specified, then the calling process sleeps until the requested
    // amount of resources are available in the semaphore (another process has released some).
    op.sem_op = -1;
    op.sem_flg = 0;
    for (;;) {
        if (semop(semaphore_in, &op, 1) == -1) {
            DEBUGF(("failed to wait on semaphore %d", semaphore_in));
            return NULL;
        }

        unsigned char signal = '\0';
        if (write(worker_init->eventfd, &signal, sizeof(signal)) != sizeof(signal)) {
            DEBUGF(("failed to write on event descriptor %d", worker_init->eventfd));
            return NULL;
        }
    }
    return NULL;
}

int main(int argc, char **argv) {
    struct in_addr addr;                     /* 32-bit IP number of host */
    int listen_fd;                              /* Listen socket */
    int port = 0;                            /* port number */
    ei_cnode ec;
    char *node_name, *node_host, *cookie;
    char full_node_name[2048];

    node_name = argv[1];
    node_host = argv[2];
    cookie = argv[3];

    DEBUGF(("args:"));
    DEBUGF(("   [1] node_name: %s", node_name));
    DEBUGF(("   [2] node_host: %s", node_host));
    DEBUGF(("   [3] cookie: %s", cookie));

    erl_init(NULL, 0);

    node_name = argv[1];
    node_host = argv[2];
    cookie = argv[3];
    full_node_name[0] = '\0';
    strcat(full_node_name, node_name);
    strcat(full_node_name, "@");
    strcat(full_node_name, node_host);

    addr.s_addr = inet_addr("0.0.0.0");
    if (ei_connect_xinit(&ec,
                         node_host, node_name,
                         full_node_name,
                         &addr,
                         cookie, 0) == -1)
        erl_err_quit("cnode : ei_connect_xinit failed");

    /* Make a listen socket */
    if ((listen_fd = tcp_listen(&port)) <= 0)
        erl_err_quit("cnode : tcp_listen failed");

    if (ei_publish(&ec, port) == -1)
        erl_err_quit("cnode : ei_publish failed");

    DEBUGF(("published port: %d", port));

    init_commands();

    // create the event descriptor
    int pipefd[2];
    if (pipe(pipefd) != 0) {
        erl_err_quit("cnode : pipe creation failed");
    }
    // initialize the worker init structure that will be used
    // for the thread
    worker_init.eventfd = pipefd[1];

    while (1) {
        int nfds = 1;
        int timeout = 1000;

        struct pollfd *fds = get_clients_to_poll(listen_fd, pipefd[0], &nfds);
        int n_set_fds = poll(fds, nfds, timeout);

        handle_clients(listen_fd, pipefd[0], fds, nfds, n_set_fds, &ec);
    }
    return 0;
}
