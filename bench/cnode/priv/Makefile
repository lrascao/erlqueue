#
# 'make'        build executable file 'mycc'
# 'make clean'  removes all .o and executable files
#

ERL_TOP=${_KERL_ACTIVE_DIR}

# define the C compiler to use
CC = gcc

# define any compile-time flags
CFLAGS = -c -Wall -g -std=c99

ERL_INTERFACE_VERSION=3.7.20

# define any directories containing header files other than /usr/include
#
# the two include paths are for you compile the project on it's own or as a dep
LQUEUE_INCLUDES=-I../../../c_src
UTHASH_INCLUDES=-I../../uthash/src -I../deps/uthash/src
INCLUDES = -I${ERL_TOP}/usr/include -I${ERL_TOP}/lib/erl_interface-$(ERL_INTERFACE_VERSION)/include $(UTHASH_INCLUDES) $(LQUEUE_INCLUDES)

# define library paths in addition to /usr/lib
#   if I wanted to include libraries not in /usr/lib I'd specify
#   their path using -Lpath, something like:
LFLAGS = -L${ERL_TOP}/lib/erl_interface-$(ERL_INTERFACE_VERSION)/lib -L${ERL_TOP}/usr/lib

# define any libraries to link into executable:
#   if I want to link in libraries (libx.so or libx.a) I use the -llibname
#   option, something like (this will link in libmylib.so and libm.so:
LIBS = -lerl_interface -lei -lerts_r -lpthread -ldl -lm

# define the C source files
CNODE_SRCS = cnode.c

# define the C object files
#
# This uses Suffix Replacement within a macro:
#   $(name:string1=string2)
#         For each word in 'name' replace 'string1' with 'string2'
# Below we are replacing the suffix .c of all words in the macro SRCS
# with the .o suffix
#
CNODE_OBJS = $(CNODE_SRCS:.c=.o)

# define the executable file
CNODE = cnode

#
# The following part of the makefile is generic; it can be used to
# build any executable just by changing the definitions above and by
# deleting dependencies appended to the file from 'make depend'
#

.PHONY: clean

all: $(CNODE)

$(CNODE): $(CNODE_OBJS)
	$(CC) -o $(CNODE) $(CNODE_OBJS) $(LFLAGS) $(LIBS)

# this is a suffix replacement rule for building .o's from .c's
# it uses automatic variables $<: the name of the prerequisite of
# the rule(a .c file) and $@: the name of the target of the rule (a .o file)
# (see the gnu make manual section about automatic variables)
.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<  -o $@

.erl.beam:
	$(ERLC) -o $@ $<

clean:
	$(RM) *.o *~ $(CNODE)
