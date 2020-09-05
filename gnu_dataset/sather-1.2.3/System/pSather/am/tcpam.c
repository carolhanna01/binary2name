#ifdef TCP
/*
 * This file was originally part of the TCP/IP AM distribution.
 * It has been adapted to work on Solaris with Threads by Claudio Fleiner
 * at the International Computer Science Institute
 *
 *                                                                      tab:2
 * tcpam.c - Active Messages on top of TCP/IP
 *
 * "Copyright (c) 1994 by Lok Tin Liu and The Regents of the University 
 * of California.  All rights reserved."
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Author:                      Lok Tin Liu
 * Version:                     456
 * Creation Date:       Fri Jul  8 13:55:26 1994
 * Filename:            tcpam.c
 * History:
 *	LTL	1	Fri Nov 11 15:49:16 1994
 *		Incorporated fixes by Chad and Alan
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <values.h>
#include "/usr/include/string.h"
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <signal.h>
#include <sys/select.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include "am.h"
#include "am_int.h"
#include <fcntl.h>
#include <errno.h>

#ifdef DEBUG4
#define TRS(a)	printf("%s:%d %d starting handler %x\n",__FILE__,__LINE__,thr_self(),(int)a)
#define TRE	printf("%s:%d %d handler done\n",__FILE__,__LINE__,thr_self())
#else
#define TRS(a)
#define TRE
#endif

/* we need the correct sleep(3) in this file */
#undef sleep

#define DEBUG 0
#define DEBUG2 0
#define DEBUG3 0

#define MAX_RETRY 60
#undef SB_MAX
#define SB_MAX 1024 /* had some problems with incorrect SB_MAX on Solaris 2.4, CMF */

#define QUICK 1

#ifndef OPEN_MAX
#define OPEN_MAX 16
#endif

#define _BSD 44									/* for AIX */

/* maximum length of hostname */
#define MAX_HOSTNAME_LEN 256

/* default configuration file name */
#define TCPAM_CONFIG "./tcpam_config"

/* message tags */
#define TCPAM_AM_INT_PKT 0
#define TCPAM_AM_DOUBLE_PKT 1
#define TCPAM_STORE_PKT 2
#define TCPAM_GET_REQUEST_PKT 3
#ifdef AM_THREADS
#define TCPAM_THREAD_INT_PKT 4
#define TCPAM_THREAD_DOUBLE_PKT 5
#endif

#define WAIT_HEADER 0
#define WAIT_DATA   1

/*Number of connections between peers */
/*One for send and one for receive... */
#define TCPAM_NUM_CONNECTIONS 2
#define REQ_CONN 0
#define REPLY_CONN 1
/* Hardcoded for now, but these N connections are the building blocks
 * for MT-Safe GAM... COY
 */
#ifndef MAX
#define MAX(a,b) (a>b)?a:b;
#endif

static volatile int debug_am=1;

int aborting()
{
	abort();
}
typedef struct {
	int tag;
	vnn_t calling_node;
	handler_t handler;
	long arg1, arg2, arg3;
	union {
		long arg4, arg5;
		double d_arg4;
	} opt_arg;
} TCPAM_pkt;

typedef struct {
	int tag;
	handler_t endfunc;
	long arg1, arg2, arg3, arg4;
	void *addr;
	int len;
} TCPAM_xfer_header;

int am_my_cluster_id;
int am_clusters;
int AM_errno;

/* server port to initiate connections */
int TCPAM_server_port[TCPAM_NUM_CONNECTIONS];

/* used with select() */
struct timeval TCPAM_timeval;

/* maximum file descriptor number per connection */
int TCPAM_max_sockfd[TCPAM_NUM_CONNECTIONS];

/* bit vector used with select() */
static fd_set TCPAM_fd_set[TCPAM_NUM_CONNECTIONS], TCPAM_all_fd_set;

/* can have as many processors as the max. number of file open */
struct sockaddr_in host_entries[OPEN_MAX];

/* corresponding socket descriptor to each node */
int node_sockfd[OPEN_MAX][TCPAM_NUM_CONNECTIONS];

#ifdef AM_THREADS
/* locks to serialise access to the sockets */
# ifdef SPINLOCK_LOCK
   spinlock_t node_sock_locks[OPEN_MAX][TCPAM_NUM_CONNECTIONS];
#  define NODE_SOCK_LOCK_CREATE 0
#  define NODE_SOCK_LOCK(a,b) SPINLOCK_LOCK(node_sock_locks[a][b])
#  define NODE_SOCK_UNLOCK(a,b) SPINLOCK_UNLOCK(node_sock_locks[a][b])
# else
   lock_t node_sock_locks[OPEN_MAX][TCPAM_NUM_CONNECTIONS];
#  define NODE_SOCK_LOCK_CREATE LCK_CREATE
#  define NODE_SOCK_LOCK(a,b) LCK_LOCK(node_sock_locks[a][b])
#  define NODE_SOCK_UNLOCK(a,b) LCK_UNLOCK(node_sock_locks[a][b])
# endif
#endif

/* message headers for request and reply sockets to each node */
TCPAM_pkt pkt_headers[OPEN_MAX][TCPAM_NUM_CONNECTIONS];

/* message state for request and reply sockets to each node */
int message_state[OPEN_MAX][TCPAM_NUM_CONNECTIONS];

/* pending data bytes for request and reply sockets to each node */
int msg_bytes_left[OPEN_MAX][TCPAM_NUM_CONNECTIONS];

/* pending data bytes for message headers ... */
int hdr_bytes_read[OPEN_MAX][TCPAM_NUM_CONNECTIONS];

/* hostname of each node */
char node_hostname[OPEN_MAX][MAX_HOSTNAME_LEN];

int am_reply_xfer(vnn_t dest, handler_t request_handler, void *handler_arg, void *lva, void *rva, int nbytes);
void am_exit(int code);
int retry = 0;

/* read hostnames in the configuration file: the first line is the
 * number of nodes; the second line is the port number; subsequent
 * lines contain names of each node
 */
void TCPAM_read_hostnames(char * config_path, int num_procs)
{
	int i;
	FILE *config_file;
	char config_filename[512];
	char hostname[MAX_HOSTNAME_LEN];
	char my_hostname[MAX_HOSTNAME_LEN];
	unsigned long inaddr, my_inaddr;
	extern int gethostname(char *,int);
	struct hostent *hp;

	strcpy(config_filename,config_path);
	strcat(config_filename,"/hosts");
	config_file=fopen(config_filename,"r");
	if(config_file==NULL) {
		fprintf(stderr,"tcpam: cannot open configfile %s\n",config_filename);
		abort();
	}
	/* figure out IP address of local host */
	if (gethostname(my_hostname, MAX_HOSTNAME_LEN) != 0) {
		perror("Cannot get hostname");
		fclose(config_file);
		exit(-1);
	}
#ifdef VERBOSE
	printf("local hostname is %s\n", my_hostname);
#endif
	hp = gethostbyname(my_hostname);
	if (hp->h_addr == 0) {
		fprintf(stderr,"Cannot get IP address of local host %s\n", my_hostname);
		fclose(config_file);
		exit(-1);
	}
	my_inaddr = *((unsigned int *) hp->h_addr);

	/* read in the hostnames and store the IP address */
	i = 0;
	am_my_cluster_id = -1;
	am_clusters = num_procs;


	if (num_procs * TCPAM_NUM_CONNECTIONS > OPEN_MAX) {
		fprintf(stderr,"Cannot start program with %d hosts and %d connections (%d max)\n",
		    num_procs, TCPAM_NUM_CONNECTIONS, OPEN_MAX);
		fclose(config_file);
		exit(-1);
	}
	
	while (!feof(config_file) && i < num_procs) {

		/*
		 * Read fast network name. Messages go over the fast network.
		 * First try to convert the hostname as
		 * a dotted-decimal number. If this fails, call gethostbyname().
		 */
		fscanf(config_file, "%s", hostname);
		if ((inaddr = inet_addr(hostname)) != -1) {
			/* hostname is dotted-decimal */
			host_entries[i].sin_addr.s_addr = inaddr;
		} else {
			hp = gethostbyname(hostname);
			if (hp->h_addr == 0) {
				fprintf(stderr,"Invalid hostname %s", hostname);
				fclose(config_file);
				exit(-1);
			}
			/* found it by name */
			host_entries[i].sin_addr.s_addr = *((unsigned long *) hp->h_addr);
		}
		host_entries[i].sin_family = AF_INET;
		host_entries[i].sin_port = 0;
		strcpy(node_hostname[i], hostname);

		/*
		 * Read slow network name. First try to convert the hostname as
		 * a dotted-decimal number. If this fails, call gethostbyname().
		 */
		fscanf(config_file, "%s", hostname);
		if ((inaddr = inet_addr(hostname)) == -1) {
			/* hostname is not dotted-decimal */
			hp = gethostbyname(hostname);
			if (hp->h_addr == 0) {
				fprintf(stderr,"Invalid hostname %s", hostname);
				fclose(config_file);
				exit(-1);
			}
			/* found it by name */
			inaddr = *((unsigned long *) hp->h_addr);
		}
		/* determine local node number */
		if (my_inaddr == inaddr)
			am_my_cluster_id = i;
#ifdef VERBOSE
		printf("%s read hostname %s\n", my_hostname, node_hostname[i]);
#endif
		if(++i==am_clusters) break;
	}
	fclose(config_file);

	if (am_clusters != i) {
		fprintf(stderr,"Error: trying to start program on %d nodes", num_procs);
		fprintf(stderr," but found only %d nodes in configuration file\n", i);
		fclose(config_file);
		exit(-1);
	}
	if (am_my_cluster_id < 0) {
		fprintf(stderr,"Error: local host %s is not defined in configuration file\n",
		    node_hostname[am_my_cluster_id]);
		fclose(config_file);
		exit(-1);
	}
	fclose(config_file);
}


/* set various socket options */
void TCPAM_set_socket(int sockfd)
{
	int opt = 1;
	int opt_len = sizeof(opt);

	/* don't delay send to buffer packets */
	if (setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt TCP_NODELAY failed");
		close(sockfd);
		exit(-1);
	}
	/* allow local address to be reused */
	if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt SO_REUSEADDR failed");
		close(sockfd);
		exit(-1);
	}
	opt = /* SB_MAX */  1024;
	/* set send and receive buffer size to maximum */
	if (setsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt SO_RCVBUF failed");
		close(sockfd);
		exit(-1);
	}
	if (setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt SO_SNDBUF failed");
		close(sockfd);
		exit(-1);

	}
}


/* search for the corresponding node using the IP address and store the
 * socket file descriptor to the connection_num
 */
void TCPAM_store_sockfd(struct sockaddr_in *client_addr, int client_sockfd,
int connection_num)
{
	int i, found;

	found = 0;
	/* use linear search -- hope it won't be too slow */
	i = 0;
	while ((i < am_clusters) && (!found)) {
		if (host_entries[i].sin_addr.s_addr == client_addr->sin_addr.s_addr)
			found = 1;
		else
			++i;
	}
	if (found)
		node_sockfd[i][connection_num] = client_sockfd;
	else {
		fprintf(stderr,"Error: cannot find IP address %s in host table\n",
		    inet_ntoa(client_addr->sin_addr));
		am_exit(-1);
	}
#ifdef VERBOSE
	printf("match socket %d for node %d (connection %d)\n", client_sockfd, i,
	    connection_num);
#endif
}


/* open known server port socket for receiving connection requests 
 */
int TCPAM_open_server(int conn)
{
	int my_sockfd;
	struct sockaddr_in my_addr;

	/* first open TCPAM_server_port as server port to set up connections */
	if ((my_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		perror("Cannot open server stream socket");
		exit(-1);
	}
	my_addr.sin_family = AF_INET;
	my_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	my_addr.sin_port = htons(TCPAM_server_port[conn]);

	TCPAM_set_socket(my_sockfd);
#if DEBUG
	printf("my_sockfd = %d\n", my_sockfd);
	printf("s_addr = %ld\n", my_addr.sin_addr.s_addr);
	printf("s_port = %d\n", my_addr.sin_port);
	printf("TCPAM_server_port = %d\n", TCPAM_server_port[conn]);
#endif

	if (bind(my_sockfd, (struct sockaddr *) &my_addr,
	    sizeof(my_addr)) < 0) {

		perror("Cannot bind local address to server port");
		perror("");
		close(my_sockfd);
		exit(-1);
	}
	return (my_sockfd);
}


/* open sockets and connect to all other nodes on connection number 
   connection_num.  Use my_sockfd to make the connection.
 */
void TCPAM_open_sockets(int num_procs, int connection_num, int my_sockfd)
{
	int i, addr_len, connected, retry;
	struct sockaddr_in server_addr, client_addr;
	int client_sockfd;

	/* initialize socket descriptor table */
	for (i = 0; i < OPEN_MAX; ++i) {
		node_sockfd[i][connection_num] = -1;
#ifdef AM_THREADS
		node_sock_locks[i][connection_num]=NODE_SOCK_LOCK_CREATE;
#endif
	}

	TCPAM_max_sockfd[connection_num] = -1;
	addr_len = sizeof(struct sockaddr);


	node_sockfd[am_my_cluster_id][connection_num] = my_sockfd;		/*for loopback? */

	listen(my_sockfd, 5);

	/* make connections to all nodes with node number < am_my_cluster_id */
	for (i = 0; i < am_my_cluster_id; ++i) {
		server_addr.sin_family = AF_INET;
		server_addr.sin_addr.s_addr = host_entries[i].sin_addr.s_addr;
		server_addr.sin_port = htons(TCPAM_server_port[connection_num]);
		if ((client_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			printf("Cannot open client stream socket\n");
			am_exit(-1);
		}
		TCPAM_set_socket(client_sockfd);
		retry = 0;
		connected = 0;
		/* Keep retrying if server is not ready; can we livelock here? */
		while ((!connected) && (retry < MAX_RETRY)) {
			if (connect(client_sockfd, (struct sockaddr *) &server_addr,
			    sizeof(server_addr)) < 0) {
				close(client_sockfd);
				sleep(1);
				server_addr.sin_family = AF_INET;
				server_addr.sin_addr.s_addr = host_entries[i].sin_addr.s_addr;
				server_addr.sin_port = htons(TCPAM_server_port[connection_num]);
				if ((client_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
					printf("Cannot open client stream socket\n");
					am_exit(-1);
				}
				++retry;
			} else
				connected = 1;

		}

		if (!connected) {
			printf("Cannot connect to %s after %d seconds timeout",
			    node_hostname[i], MAX_RETRY);
			perror("");
			close(client_sockfd);
			am_exit(-1);
		}
		node_sockfd[i][connection_num] = client_sockfd;
#ifdef VERBOSE
		printf("open socket %d for node %d, connection_num %d\n", client_sockfd, i, connection_num);
#endif
		if (client_sockfd > TCPAM_max_sockfd[connection_num])
			TCPAM_max_sockfd[connection_num] = client_sockfd;
	}

	/* make connections to all nodes with node number > am_my_cluster_id */
	for (i = 1; i <= (am_clusters - am_my_cluster_id - 1); ++i) {
		client_sockfd = accept(my_sockfd, (struct sockaddr *) &client_addr,
		    &addr_len);

		if (client_sockfd == -1) {
			printf("Error: cannot accept connection on %s\n",
			    node_hostname[am_my_cluster_id]);
			perror("accept");
			am_exit(-1);
		}
		TCPAM_set_socket(client_sockfd);

		/* store the socket file descriptor of the corresponding node */
		TCPAM_store_sockfd(&client_addr, client_sockfd, connection_num);
		if (client_sockfd > TCPAM_max_sockfd[connection_num])
			TCPAM_max_sockfd[connection_num] = client_sockfd;
	}
	++TCPAM_max_sockfd[connection_num];		/*Make the max strictly greater than */
	/*Why do this? COY */
}


static void check_lock()
{
	FILE *f;
	int i,j;
	char buf[10];
	f=fopen("/tmp/TCPIP_AM","r");
	if(f!=NULL) {
		fread(buf,1,10,f);
		i=atoi(buf);
		j=kill(i,SIGKILL);
		if(j<0 && errno==EPERM) {
			fprintf(stderr,"There seems to be another process that uses\n");
			fprintf(stderr,"the LAM package (pid %d). I cannot kill this\n",i);
			fprintf(stderr,"process. If this is wrong, remove\n");
			fprintf(stderr,"the file /tmp/TCPIP_AM and try again.\n");
			exit(-1);
		}
		fclose(f);
	}
	close(creat("/tmp/TCPIP_AM",0666));
	chmod("/tmp/TCPIP_AM",0666);
	f=fopen("/tmp/TCPIP_AM","w");
	fprintf(f,"%d",(int)getpid());
	fclose(f);
}

static void remove_lock()
{
	unlink("/tmp/TCPIP_AM");
}

/* set up the TCPAM layer

 * 1. read in the configuration file
 * 2. open sockets and connect to all other nodes
 * 3. initialize data structures
 */
void am_enable(int nodes,int argc,char *argv[])
{
	int i, ns, flags;
	char *config_path;
	char config_filename[FILENAME_MAX];
	FILE *config_file;
	int num_procs;
	int my_sockfd[TCPAM_NUM_CONNECTIONS];
	char sather_home[512];


	/* read in the configuration file */

	/* if user does not specify full path of configuration file */
	if ((config_path = getenv("TCPAM_CONFIG")) == NULL) {
		/* use the current directory by default */
		if((config_path=getenv("SATHER_HOME"))==NULL) { 
			config_path="./";
		} else {
			strcpy(sather_home,config_path);
			strcat(sather_home,"/System/Platforms/old/tcpip");
			config_path=sather_home;
		}
	}

	num_procs=nodes;
	strcpy(config_filename,config_path);
	strcat(config_filename,"/port");

	config_file = fopen(config_filename, "r");
	if (config_file == NULL) {
		printf("Cannot open configuration file %s\n", config_filename);
		exit(-1);
	}
	if (fscanf(config_file, "%d", &TCPAM_server_port[0]) == EOF) {
		printf("Cannot read server port number in configuration file\n");
		exit(-1);
	}
	fclose(config_file);
	for (ns = 1; ns < TCPAM_NUM_CONNECTIONS; ns++)
		TCPAM_server_port[ns] = TCPAM_server_port[0] + 1000*ns;
	TCPAM_read_hostnames(config_path, num_procs);
	check_lock();

        if(am_my_cluster_id==0) {
		char exec[1024];
                sprintf(exec,"%s/start_nodes %d ",config_path,nodes);
                for(i=0;i<argc;i++) {
                        strcat(exec," \"");
                        strcat(exec,argv[i]);
                        strcat(exec,"\"");
                }
                system(exec);
        }

	/*open the server port for receiving connection requests */
	for (ns = 0; ns < TCPAM_NUM_CONNECTIONS; ns++) {
		my_sockfd[ns] = TCPAM_open_server(ns);
	}

	/* open sockets and connect to all other processors; this also acts as
	 * an implicit barrier for the setup phase
	 */
	for (ns = 0; ns < TCPAM_NUM_CONNECTIONS; ns++) {
		TCPAM_open_sockets(num_procs, ns, my_sockfd[ns]);
		/* set up descriptor bit vector for select() */
		FD_ZERO(&TCPAM_fd_set[ns]);
		for (i = 0; i < am_clusters; ++i) {
			FD_SET(node_sockfd[i][ns], &TCPAM_fd_set[ns]);
#ifdef QUICK
			FD_SET(node_sockfd[i][ns], &TCPAM_all_fd_set);
#endif
		}
	}

	/* use non-blocking select() */
	TCPAM_timeval.tv_sec = 0;
	TCPAM_timeval.tv_usec = 0;

	for (ns = 0; ns < TCPAM_NUM_CONNECTIONS; ns++) {
		for (i = 0; i < am_clusters; ++i) {
			if (node_sockfd[i][ns] != -1) {
				flags = fcntl(node_sockfd[i][ns], F_GETFL);
				flags |= O_NONBLOCK;
				fcntl(node_sockfd[i][ns], F_SETFL, flags);
			}
		}
	}
	if(getenv("DEBUG_AM") || getenv("DEBUG_PSATHER")) {
		char com[200];
		if(getenv("DEBUG_AM_COMMAND"))
			sprintf(com,getenv("DEBUG_AM_COMMAND"),argv[0],getpid());
		else if(getenv("DEBUG_PSATHER_COMMAND"))  
			sprintf(com,getenv("DEBUG_PSATHER_COMMAND"),argv[0],getpid());
		else
			sprintf(com,"xterm -fn 7x13 -T \"%d ($hostname) %s\" -e gdb %s %d&\n",am_my_cluster_id,argv[0],argv[0],(int)getpid());
		system(com);
		while(debug_am);
	}
#ifdef AM_THREADS
	T_INIT_MACHINE;
	T_INIT_CLUSTER;
	AM_INIT_THREAD;
#endif
}

/* close all sockets */
void am_disable()
{
	int i, ns;

	/*spin for a while before exit; otherwise some nodes will hang on the
	 *last barrier
	 */
	for (i = 0; i < 100000; ++i);

	/* close all sockets */
	for (ns = 0; ns < TCPAM_NUM_CONNECTIONS; ns++)
		for (i = 0; i < am_clusters; ++i) {
			if (node_sockfd[i][ns] != -1)
				close(node_sockfd[i][ns]);
		}
	remove_lock();
}

/* close all sockets before exit */
void am_exit(int code)
{
	am_disable();
	exit(code);
}

/*Optimization--just go up to max */
/*OR two sets together */
#define OR_FD_SET(a,b)\
{\
   int i;\
   for(i=0; i< howmany(FD_SETSIZE, NFDBITS); i++)\
     a.fds_bits[i] |= b.fds_bits[i];\
}


#if 0
/*
 * Caller blocks until 1 or more messages arrives.  Messages
 * are not actually processed.  This function is used for event
 * driven applications.
 */
void am_sleep()
{
	fd_set TCPAM_temp_fd_set;
	int max;
	int num_pending;

#ifdef QUICK
	/*This  value is precomputed for quicker response times--yeah, right*/
	TCPAM_temp_fd_set = TCPAM_all_fd_set;
#else
	TCPAM_temp_fd_set= TCPAM_fd_set[REQ_CONN];
	OR_FD_SET(TCPAM_temp_fd_set, TCPAM_fd_set[REPLY_CONN]);
#endif

	max = MAX(TCPAM_max_sockfd[REQ_CONN],TCPAM_max_sockfd[REPLY_CONN]);

	/* 
     * check whether there are messages pending (note that
     * select() will put result in TCPAM_temp_fd_set) -- uses
     * blocking select() call, to block caller until msg arrives
     */
	/*could we get stuck if a socket connection breaks ? */

	do {
		num_pending = select(max /*COY*/,
		    &TCPAM_temp_fd_set,
		    (fd_set *)0,
		    (fd_set *)0, 0);

		if(num_pending == -1) {
			AM_errno = errno;
			am_exit(-1);
		}

	} while(num_pending == 0);

	return;
}
#endif

#ifdef AM_THREADS
static AM_START_THREAD_DCL(TCPAM_pkt *pp)
{
	AM_INIT_THREAD;
	switch(pp->tag) {
	case TCPAM_THREAD_INT_PKT:
		((handler_4_t)pp->handler)(pp->calling_node,
		    pp->arg1, pp->arg2, pp->arg3, pp->opt_arg.arg4);
		break;
	case TCPAM_THREAD_DOUBLE_PKT:
		((handler_df_t)pp->handler)(pp->calling_node,
		    pp->arg1, pp->arg2, pp->opt_arg.d_arg4);
		break;
	}
	/* free(pp); XXXX */
}

#endif /* AM_THREADS */
/*
 * Poll request and reply networks. Returns -1 on encountering 
 * any error and may set AM_errno, and returns 0 otherwise.
 */
void am_poll()
{
	char *cp;
	TCPAM_pkt *pp;
	TCPAM_xfer_header *hp;
	fd_set TCPAM_temp_fd_set;

	int i, n, m, sn;
	int max, sockfd;

#ifdef AM_THREADS
	if(!thread_may_poll()) { YIELD;return; }
#endif
#if DEBUG
	printf("am_poll enter\n");
#endif

#ifdef QUICK
	/*This  value is precomputed for quicker response times--yeah, right*/
	TCPAM_temp_fd_set = TCPAM_all_fd_set;
#else
	TCPAM_temp_fd_set= TCPAM_fd_set[REQ_CONN];
	OR_FD_SET(TCPAM_temp_fd_set, TCPAM_fd_set[REPLY_CONN]);
#endif
	max = MAX(TCPAM_max_sockfd[REQ_CONN], TCPAM_max_sockfd[REPLY_CONN]);

#ifdef AM_THREADS
	if(select(max, &TCPAM_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, NULL) == -1) {
#else
	if(select(max, &TCPAM_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, &TCPAM_timeval) == -1) {
#endif
		AM_errno = errno;
		am_exit(-1);
	}

	for(i = 0; i < am_clusters; i++) {
		for(sn = 0; sn < TCPAM_NUM_CONNECTIONS; sn++) {
			sockfd = node_sockfd[i][sn];

			if(FD_ISSET(sockfd, &TCPAM_temp_fd_set)) {
				pp = &pkt_headers[i][sn];

				/* check if waiting for message header */
				if(message_state[i][sn] == WAIT_HEADER) {
#if DEBUG3
					printf("am_poll: read header...");
#endif
					n = sizeof(TCPAM_pkt) - hdr_bytes_read[i][sn];
					if((m = read(sockfd, pp + hdr_bytes_read[i][sn], n)) == -1) {
						if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
							AM_errno = errno;
							printf("node %d am_poll: read header error", am_my_cluster_id);
							perror("am_poll: read header error");
							am_exit(-1);
						}
						retry++;

					} else {
						retry = 0;
						hdr_bytes_read[i][sn] += m;

						if(hdr_bytes_read[i][sn] == sizeof(TCPAM_pkt)) {
							hdr_bytes_read[i][sn] = 0;
#if DEBUG2
							printf("am_poll %x %x %x %x %x %x %x %x\n", 
							    (int)pp->tag, (int)pp->handler, (int)pp->calling_node, (int)pp->arg1, 
							    (int)pp->arg2, (int)pp->arg3, (int)pp->opt_arg.arg4,(int)pp->opt_arg.arg5);
#endif
							/* check if active message with integer arguments */
							if (pp->tag == TCPAM_AM_INT_PKT) {
								TRS(pp->handler);
								((handler_4_t)pp->handler)(pp->calling_node,
								    pp->arg1,
								    pp->arg2,
								    pp->arg3,
								    pp->opt_arg.arg4);
							        TRE;

								/* check if active message with floating point arguments */
							} else if (pp->tag == TCPAM_AM_DOUBLE_PKT) {
								TRS(pp->handler);
								((handler_df_t)pp->handler)(pp->calling_node,
								    pp->arg1, pp->arg2, pp->opt_arg.d_arg4);
								TRE;

								/* check if am_get request and do reply back */
#ifdef AM_THREADS
							} else if (pp->tag == TCPAM_THREAD_INT_PKT || pp->tag == TCPAM_THREAD_DOUBLE_PKT) {
								TCPAM_pkt *pp1;
								pp1=malloc(sizeof(*pp1));
								memcpy(pp1,pp,sizeof(*pp1));
								AM_START_THREAD(pp1);
#endif
							} else if (pp->tag == TCPAM_GET_REQUEST_PKT) {
								am_reply_xfer(pp->calling_node,
								    pp->handler /* handler */,
								    (void *)pp->arg1 /* handler arg*/,
								    (void *)pp->arg2 /* src addr */,
								    (void *)pp->arg3 /* dst addr */,
								    pp->opt_arg.arg4 /* nbytes */);

								/* check if am_store header, and change state */
							} else if (pp->tag == TCPAM_STORE_PKT) {
#if DEBUG3
								printf("reading to read data\n");
#endif
								message_state[i][sn] = WAIT_DATA;

								hp = (TCPAM_xfer_header *) &pkt_headers[i][sn];
								msg_bytes_left[i][sn] = hp->len;
								/* invoke callback function if address is not 0 */
								if(msg_bytes_left[i][sn] == 0) {
									message_state[i][sn] = WAIT_HEADER;
									TRS(hp->endfunc);
									if(hp->endfunc)
										((handler_mem_t)hp->endfunc)
										    (hp->arg2, hp->addr,
										    hp->len, (void *)(hp->arg1));
									TRE;
								}

								/* uh oh */
							} else {
								/* corrupted data stream */
								printf("Error: unknown packet type\n");
								am_exit(-1);
							}
						}
					}

					/* we've received hdr, waiting for data */
				} else {

					hp = (TCPAM_xfer_header *) &pkt_headers[i][sn];
					n = msg_bytes_left[i][sn];
					cp = (char *) hp->addr;
#if DEBUG3
					printf("am_poll: data read...\n");
#endif
					if((m = read(sockfd, &cp[hp->len-n], n)) == -1) {
						if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
							AM_errno = errno;
							printf("node %d am_poll: read data error", am_my_cluster_id);
							perror("am_poll: read data error");
							am_exit(-1);
						}
						retry++;

					}
					else {
#if DEBUG3
						printf("done (%d))\n", m);
#endif
						retry = 0;
						msg_bytes_left[i][sn] -= m;

						/* invoke callback function if address is not 0 */
						if(msg_bytes_left[i][sn] == 0) {
							message_state[i][sn] = WAIT_HEADER;

							TRS(hp->endfunc);
							if(hp->endfunc)
								((handler_mem_t)hp->endfunc)
								    (hp->arg2, hp->addr,
								    hp->len, (void *)(hp->arg1));
							TRE;
						}
					}
				}
			}
		}
	}

#if DEBUG
	printf("am_poll exit\n");
#endif		

	return;
}

/*
 * Poll request and reply networks.  Returns -1 on encountering 
 * any error and may set AM_errno, and returns 0 otherwise.
 */
int am_reply_poll()
{
	char *cp;
	TCPAM_pkt *pp;
	TCPAM_xfer_header *hp;
	fd_set TCPAM_temp_fd_set;

	int i, n, m;
	int max, sockfd;

#ifdef AM_THREADS
	if(!thread_may_poll()) { YIELD;return 0; }
#endif
#if DEBUG
	printf("am_reply_poll enter\n");
#endif

	TCPAM_temp_fd_set= TCPAM_fd_set[REPLY_CONN];
	max = TCPAM_max_sockfd[REPLY_CONN];

#ifdef AM_THREADS
	if(select(max, &TCPAM_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, NULL) == -1) {
#else
	if(select(max, &TCPAM_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, &TCPAM_timeval) == -1) {
#endif
		AM_errno = errno;
		am_exit(-1);
	}

	for(i = 0; i < am_clusters; i++) {
		sockfd = node_sockfd[i][REPLY_CONN];

		if(FD_ISSET(sockfd, &TCPAM_temp_fd_set)) {
			pp = &pkt_headers[i][REPLY_CONN];

			/* check if waiting for message header */
			if(message_state[i][REPLY_CONN] == WAIT_HEADER) {
#if DEBUG3
				printf("am_reply_poll: read header...");
#endif
				n = sizeof(TCPAM_pkt) - hdr_bytes_read[i][REPLY_CONN];
				if((m = read(sockfd, pp + hdr_bytes_read[i][REPLY_CONN], n)) == -1) {
					if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
						AM_errno = errno;
						printf("node %d am_poll: read data error", am_my_cluster_id);
						perror("am_poll: read data error");
						am_exit(-1);
					}
					retry++;

				} else {
					retry = 0;
					hdr_bytes_read[i][REPLY_CONN] += m;

					if(hdr_bytes_read[i][REPLY_CONN] == sizeof(TCPAM_pkt)) {
						hdr_bytes_read[i][REPLY_CONN] = 0;
#if DEBUG2
						printf("am_reply_poll %x %x %x %x %x %x %x %x\n", 
						    (int)pp->tag, (int)pp->handler, (int)pp->calling_node, (int)pp->arg1, 
						    (int)pp->arg2, (int)pp->arg3, (int)pp->opt_arg.arg4,(int)pp->opt_arg.arg5);
#endif
						/* check if active message with integer arguments */
						if (pp->tag == TCPAM_AM_INT_PKT) {
							TRS(pp->handler);
							((handler_4_t)pp->handler)(pp->calling_node,
							    pp->arg1,
							    pp->arg2,
							    pp->arg3,
							    pp->opt_arg.arg4);
							TRE;

							/* check if active message with floating point arguments */
						} else if (pp->tag == TCPAM_AM_DOUBLE_PKT) {
							TRS(pp->handler);
							((handler_df_t)pp->handler)(pp->calling_node,
							    pp->arg1, pp->arg2, pp->opt_arg.d_arg4);
							TRE;

							/* check if am_get request and do reply back */
						} else if (pp->tag == TCPAM_GET_REQUEST_PKT) {
							am_reply_xfer(pp->calling_node,
							    pp->handler /* handler */,
							    (void *)pp->arg1 /* handler arg*/,
							    (void *)pp->arg2 /* src addr */,
							    (void *)pp->arg3 /* dst addr */,
							    pp->opt_arg.arg4 /* nbytes */);

							/* check if am_store header, and change state */
						} else if (pp->tag == TCPAM_STORE_PKT) {
#if DEBUG3
							printf("ready to read data\n");
#endif
							message_state[i][REPLY_CONN] = WAIT_DATA;

							hp = (TCPAM_xfer_header *) &pkt_headers[i][REPLY_CONN];
							msg_bytes_left[i][REPLY_CONN] = hp->len;
							/* invoke callback function if address is not 0 */
							if(msg_bytes_left[i][REPLY_CONN] == 0 && hp->endfunc) {
								message_state[i][REPLY_CONN] = WAIT_HEADER;

								TRS(hp->endfunc);
								((handler_mem_t)hp->endfunc)(hp->arg2, hp->addr,
								    hp->len, (void *)(hp->arg1));
								TRE;
							}

							/* uh oh */
						} else {
							/* corrupted data stream */
							printf("Error: unknown packet type\n");
							am_exit(-1);
						}
					}
				}

				/* we've received hdr, waiting for data */
			} else {

				hp = (TCPAM_xfer_header *) &pkt_headers[i][REPLY_CONN];
				n = msg_bytes_left[i][REPLY_CONN];
				cp = (char *) hp->addr;
#if DEBUG3
				printf("am_reply_poll: data read...\n");
#endif
				if((m = read(sockfd, &cp[hp->len-n], n)) == -1) {
					if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
						AM_errno = errno;
						printf("node %d am_poll: read data error", am_my_cluster_id);
						perror("am_poll: read data error");
						am_exit(-1);
					}
					retry++;


				}
				else {
#if DEBUG3
					printf("done (%d)\n", m);
#endif
					retry = 0;
					msg_bytes_left[i][REPLY_CONN] -= m;

					/* invoke callback function if address is not 0 */
					if(msg_bytes_left[i][REPLY_CONN] == 0 && hp->endfunc) {
						message_state[i][REPLY_CONN] = WAIT_HEADER;

						TRS(hp->endfunc);
						((handler_mem_t)hp->endfunc)(hp->arg2, hp->addr,
						    hp->len, (void *)(hp->arg1));
						TRE;
					}
				}
			}
		}
	}

#if DEBUG
	printf("am_reply_poll exit\n");
#endif		

	return(0);
}

/*
 * while *flag != value, poll for messages.  If poll 
 * encounters any errors, am_wait returns -1 and does
 * not modify the flag.
 */
int am_wait(volatile int *flag, 
int value)
{
	while (*flag < value) am_poll();
	(*flag)-=value;
	return 0;
}

/*
 * poll for messages until *flag == value.  If poll encounters
 * any errors, am_poll_wait returns -1 and does not modify the
 * flag.
 */
int am_poll_wait(volatile int *flag, 
int value)
{
	do { am_poll(); } while(*flag < value);
	(*flag)-=value;
	return 0;
}

#ifdef AM_THREADS
/*
 * Starts a new thread on dest with upto 4 interger arguments. Returns -1 on
 * encountering any error and sets AM_errno, return 0 otherwise.
 */
int thr_create_0(vnn_t dest,handler_0_t h)     			   { return thr_create_4(dest,(handler_4_t)h,111,222,333,444); }
int thr_create_1(vnn_t dest,handler_1_t h,long a1)     		   { return thr_create_4(dest,(handler_4_t)h,a1,222,333,444); }
int thr_create_2(vnn_t dest,handler_2_t h,long a1,long a2)     	   { return thr_create_4(dest,(handler_4_t)h,a1,a2,333,444); }
int thr_create_3(vnn_t dest,handler_3_t h,long a1,long a2,long a3) { return thr_create_4(dest,(handler_4_t)h,a1,a2,a3,444); }
int thr_create_4(vnn_t dest,
handler_4_t fun,
long arg1, long arg2,
long arg3, long arg4)
{
	char *bp;
	TCPAM_pkt TCPAM_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
	int sigs;

	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;

	if(dest != am_my_cluster_id) {
		bp = (char *)&TCPAM_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPAM_sbuf.tag = TCPAM_THREAD_INT_PKT;
		TCPAM_sbuf.calling_node = am_my_cluster_id;
		TCPAM_sbuf.handler = (handler_t)fun;
		TCPAM_sbuf.arg1 = arg1;
		TCPAM_sbuf.arg2 = arg2;
		TCPAM_sbuf.arg3 = arg3;
		TCPAM_sbuf.opt_arg.arg4 = arg4;

		byte_left = sizeof(TCPAM_pkt);
		NODE_SOCK_LOCK(dest,REQ_CONN);
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("thr_create_4 writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_poll();
			}
		}
		NODE_SOCK_UNLOCK(dest,REQ_CONN);

	} else {
		/* loopback */
		TCPAM_pkt *pp1;
		pp1=malloc(sizeof(*pp1));
		pp1->tag = TCPAM_THREAD_INT_PKT;
		pp1->calling_node = am_my_cluster_id;
		pp1->handler = (handler_t)fun;
		pp1->arg1 = arg1;
		pp1->arg2 = arg2;
		pp1->arg3 = arg3;
		pp1->opt_arg.arg4 = arg4;
		AM_START_THREAD(pp1);
	}
	if(sigs) THR_ENABLE_SIGNAL;

	return(0);
}
/*
 * Starts thread with 2 interger arguments and 1 double argument.
 * Returns -1 on encountering any error and sets AM_errno, return 0 
 * otherwise.
 */
int thr_create_df(vnn_t dest, handler_df_t fun,long arg2,long arg3,double arg1)
{
	char *bp;
	TCPAM_pkt TCPAM_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;

	if(dest != am_my_cluster_id) {
		bp = (char *)&TCPAM_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPAM_sbuf.tag = TCPAM_THREAD_DOUBLE_PKT;
		TCPAM_sbuf.calling_node = am_my_cluster_id;
		TCPAM_sbuf.handler = (handler_t)fun;
		TCPAM_sbuf.arg1 = arg2;
		TCPAM_sbuf.arg2 = arg3;
		TCPAM_sbuf.opt_arg.d_arg4 = arg1;

		byte_left = sizeof(TCPAM_pkt);
		NODE_SOCK_LOCK(dest,REQ_CONN);
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("thr_create_df writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_poll();
			}
		}
		NODE_SOCK_UNLOCK(dest,REQ_CONN);

	} else {
		/* loopback */
		TCPAM_pkt *pp1;
		pp1=malloc(sizeof(*pp1));
		pp1->tag = TCPAM_THREAD_DOUBLE_PKT;
		pp1->calling_node = am_my_cluster_id;
		pp1->handler = (handler_t)fun;
		pp1->arg1 = arg2;
		pp1->arg2 = arg3;
		pp1->opt_arg.d_arg4 = arg1;
		AM_START_THREAD(pp1);
		am_poll();
	}
	if(sigs) THR_ENABLE_SIGNAL;
	return(0);
}

#endif /* AM_THREADS */


/*
 * Sends request with 4 interger arguments. Returns -1 on
 * encountering any error and sets AM_errno, return 0 otherwise.
 */
int am_request_0(vnn_t dest,handler_0_t h)     			   { return am_request_4(dest,(handler_4_t)h,1,2,3,4); }
int am_request_1(vnn_t dest,handler_1_t h,long a1)     		   { return am_request_4(dest,(handler_4_t)h,a1,2,3,4); }
int am_request_2(vnn_t dest,handler_2_t h,long a1,long a2)     	   { return am_request_4(dest,(handler_4_t)h,a1,a2,3,4); }
int am_request_3(vnn_t dest,handler_3_t h,long a1,long a2,long a3) { return am_request_4(dest,(handler_4_t)h,a1,a2,a3,4); }
int am_request_4(vnn_t dest,
handler_4_t fun,
long arg1, long arg2,
long arg3, long arg4)
{
	char *bp;
	TCPAM_pkt TCPAM_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef AM_THREADS
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif

#if DEBUG
	printf("starting am_request_4\n");fflush(stdout);
#endif				
	if(dest != am_my_cluster_id) {
		bp = (char *)&TCPAM_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPAM_sbuf.tag = TCPAM_AM_INT_PKT;
		TCPAM_sbuf.calling_node = am_my_cluster_id;
		TCPAM_sbuf.handler = (handler_t)fun;
		TCPAM_sbuf.arg1 = arg1;
		TCPAM_sbuf.arg2 = arg2;
		TCPAM_sbuf.arg3 = arg3;
		TCPAM_sbuf.opt_arg.arg4 = arg4;
#if DEBUG2
		printf("am_request %x %x %x %x %x %x %x %x %x\n", 
		    dest,(int)TCPAM_sbuf.tag, (int)TCPAM_sbuf.handler, (int)TCPAM_sbuf.calling_node, (int)TCPAM_sbuf.arg1, 
		    (int)TCPAM_sbuf.arg2, (int)TCPAM_sbuf.arg3, (int)TCPAM_sbuf.opt_arg.arg4,(int)TCPAM_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPAM_pkt);
#ifdef AM_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_request_4 writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_poll();
			}
		}
#ifdef AM_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		TRS(fun);
		((handler_4_t)fun)(am_my_cluster_id, 
		    arg1, arg2,
		    arg3, arg4);
		TRE;
		am_poll();
	}
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
#if DEBUG
	printf("am_request_4 done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Sends request with 2 interger arguments and 1 double argument.
 * Returns -1 on encountering any error and sets AM_errno, return 0 
 * otherwise.
 */
int am_request_df(vnn_t dest, handler_df_t fun,long arg2,long arg3,double arg1)
{
	char *bp;
	TCPAM_pkt TCPAM_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef AM_THREADS
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
#if DEBUG
	printf("starting am_request_df\n");fflush(stdout);
#endif				

	if(dest != am_my_cluster_id) {
		bp = (char *)&TCPAM_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPAM_sbuf.tag = TCPAM_AM_DOUBLE_PKT;
		TCPAM_sbuf.calling_node = am_my_cluster_id;
		TCPAM_sbuf.handler = (handler_t)fun;
		TCPAM_sbuf.arg1 = arg2;
		TCPAM_sbuf.arg2 = arg3;
		TCPAM_sbuf.opt_arg.d_arg4 = arg1;
#if DEBUG2
		printf("am_request_df %x %x %x %x %x %x %x %x %x\n", 
		    dest,(int)TCPAM_sbuf.tag, (int)TCPAM_sbuf.handler, (int)TCPAM_sbuf.calling_node, (int)TCPAM_sbuf.arg1, 
		    (int)TCPAM_sbuf.arg2, (int)TCPAM_sbuf.arg3, (int)TCPAM_sbuf.opt_arg.arg4,(int)TCPAM_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPAM_pkt);
#ifdef AM_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_request_df writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_poll();
			}
		}
#ifdef AM_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		TRS(fun);
		((handler_df_t)fun)(am_my_cluster_id, arg2, arg3, arg1);
		TRE;
		am_poll();
	}
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
#if DEBUG
	printf("am_request_df done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Sends a reply message with 4 integer arguments.  Returns -1 on
 * encountering any error and sets AM_errno, return 0 otherwise.
 */
int am_reply_0(vnn_t dest,handler_0_t h)     			   { return am_reply_4(dest,(handler_4_t)h,1,2,3,4); }
int am_reply_1(vnn_t dest,handler_1_t h,long a1)     		   { return am_reply_4(dest,(handler_4_t)h,a1,2,3,4); }
int am_reply_2(vnn_t dest,handler_2_t h,long a1,long a2)     	   { return am_reply_4(dest,(handler_4_t)h,a1,a2,3,4); }
int am_reply_3(vnn_t dest,handler_3_t h,long a1,long a2,long a3)   { return am_reply_4(dest,(handler_4_t)h,a1,a2,a3,4); }
int am_reply_4(vnn_t dest, handler_4_t fun, long arg1, long arg2, long arg3, long arg4)
{
	char *bp;
	TCPAM_pkt TCPAM_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef AM_THREADS
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
#if DEBUG
	printf("starting am_reply_4\n");fflush(stdout);
#endif				

	if(dest != am_my_cluster_id) {
		bp = (char *)&TCPAM_sbuf;
		dest_sockfd = node_sockfd[dest][REPLY_CONN];
		TCPAM_sbuf.tag = TCPAM_AM_INT_PKT;
		TCPAM_sbuf.calling_node = am_my_cluster_id;
		TCPAM_sbuf.handler = (handler_t)fun;
		TCPAM_sbuf.arg1 = arg1;
		TCPAM_sbuf.arg2 = arg2;
		TCPAM_sbuf.arg3 = arg3;
		TCPAM_sbuf.opt_arg.arg4 = arg4;
#if DEBUG2
		printf("am_reply %x, %x %x %x %x %x %x %x %x\n", 
		    dest, (int)TCPAM_sbuf.tag, (int)TCPAM_sbuf.handler, (int)TCPAM_sbuf.calling_node, (int)TCPAM_sbuf.arg1, 
		    (int)TCPAM_sbuf.arg2, (int)TCPAM_sbuf.arg3, (int)TCPAM_sbuf.opt_arg.arg4,(int)TCPAM_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPAM_pkt);
#ifdef AM_THREADS
/*		NODE_SOCK_LOCK(dest,REPLY_CONN); */
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_reply_4 writes %d bytes\n", byte_written);
#endif							
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_reply_poll();
			}
		}
#ifdef AM_THREADS
/*		NODE_SOCK_UNLOCK(dest,REPLY_CONN); */
#endif

	} else {
		/* loopback */
		TRS(fun);
		((handler_4_t)fun)(am_my_cluster_id, 
		    arg1, arg2,
		    arg3, arg4);
		TRE;
		am_reply_poll();
	}
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
#if DEBUG
	printf("am_reply_4 done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Sends a reply message with 3 integer arguments and 1 
 * double-precision floating point argument.  Returns -1 on
 * encountering any error and sets AM_errno, return 0 otherwise.
 */
int am_reply_df(vnn_t dest, handler_df_t fun,long arg2,long arg3,double arg1)
{
	register char *bp;
	TCPAM_pkt TCPAM_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef AM_THREADS
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
#if DEBUG
	printf("starting am_reply_df\n");fflush(stdout);
#endif				

	if(dest != am_my_cluster_id) {
		bp = (char *)&TCPAM_sbuf;
		dest_sockfd = node_sockfd[dest][REPLY_CONN];
		TCPAM_sbuf.tag = TCPAM_AM_DOUBLE_PKT;
		TCPAM_sbuf.calling_node = am_my_cluster_id;
		TCPAM_sbuf.handler = (handler_t)fun;
		TCPAM_sbuf.arg1 = arg2;
		TCPAM_sbuf.arg2 = arg3;
		TCPAM_sbuf.opt_arg.d_arg4 = arg1;
#if DEBUG2
		printf("am_reply_df %x %x %x %x %x %x %x %x %x\n", 
		    dest, (int)TCPAM_sbuf.tag, (int)TCPAM_sbuf.handler, (int)TCPAM_sbuf.calling_node, (int)TCPAM_sbuf.arg1, 
		    (int)TCPAM_sbuf.arg2, (int)TCPAM_sbuf.arg3, (int)TCPAM_sbuf.opt_arg.arg4,(int)TCPAM_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPAM_pkt);
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_reply_df writes %d bytes\n", byte_written);
#endif							
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_reply_poll();
			}
		}

	} else {
		/* loopback */
		TRS(fun);
		((handler_df_t)fun)(am_my_cluster_id, arg2, arg3, arg1);
		TRE;
		am_reply_poll();
	}
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
#if DEBUG
	printf("am_reply_df done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Copy nbytes bytes of data from address lva on calling node to address
 * rva on dest node. Invoke handler with handler_arg on dest node when all
 * data are received on dest node.  Returns -1 on encountering any error and
 * sets AM_errno, returns 0 otherwise.
 */
int am_store(vnn_t dest, void *lva, void *rva, int nbytes, handler_mem_t request_handler, void *handler_arg)
{
	char *bp;
	TCPAM_xfer_header header;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef AM_THREADS
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif

	if(dest != am_my_cluster_id) {
		header.tag = TCPAM_STORE_PKT;
		header.len = nbytes;
		header.addr = rva;
		header.endfunc = (handler_t)request_handler;
		header.arg1 = (int)handler_arg;
		header.arg2 = am_my_cluster_id;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		bp = (char *)&header;

		/* send the header */
		byte_left = sizeof(TCPAM_xfer_header);
#ifdef AM_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_store writes %d bytes of header\n", byte_written);
#endif			
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_poll();
			}
		}
		/* send the buffer */
		bp = lva;
		byte_left = nbytes;
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG3
			printf("am_store %d/%d\n", byte_written, nbytes);
#endif
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_poll();
			}
		}
#ifdef AM_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		/* was memmove, but changed it to memcpy (am_store does not specify
	   the behaviour in cases of overlaping memory */
		memcpy(rva, lva, nbytes);

		/* invoke callback function if address is not 0 */
		TRS(request_handler);
		if(request_handler)
			((handler_mem_t)request_handler)(am_my_cluster_id, 
			    rva, nbytes,
			    handler_arg);
		TRE;
		am_poll();
	}

#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
	return(0);
}
/*
 * am_store_async is supposed to return immediatly, we cheat */
int am_store_async(vnn_t dest, void *lva, void *rva, int nbytes, handler_mem_t request_handler, void *handler_arg,handler_mem_t endfunc,void *earg)
{
	int i;
	i=am_store(dest,lva,rva,nbytes,request_handler,handler_arg);
	TRS(endfunc);
	if(i==0 && endfunc!=NULL) (*endfunc)(dest,lva,nbytes,earg);
	TRE;
	return i;
}
/*
 * Like am_store, but used for the reply bulk transfer for am_gets.
 * Also, am_reply_xfer only polls the reply network.  Returns -1 on 
 * encountering any errors and sets AM_errno, and 0 otherwise.
 */
int am_reply_xfer(vnn_t dest, 
handler_t request_handler,
void *handler_arg,
void *lva,
void *rva, 
int nbytes)
{
	char *bp;
	TCPAM_xfer_header header;

	int byte_left;
	int dest_sockfd;
	int byte_written;

	if(dest != am_my_cluster_id) {
		header.tag = TCPAM_STORE_PKT;
		header.endfunc = request_handler;
		header.arg1 = (int)handler_arg;
		header.arg2 = am_my_cluster_id;
		header.addr = rva;
		header.len = nbytes;
		dest_sockfd = node_sockfd[dest][REPLY_CONN];
		bp = (char *) &header;

		/* send the header */
		byte_left = sizeof(TCPAM_xfer_header);
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_reply_xfer writes %d bytes of header\n", byte_written);
#endif			
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_reply_poll();
			}
		}

		/* send the data buffer */
		bp = lva;
		byte_left = nbytes;

		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_reply_xfer writes %d bytes of buffer\n", byte_written);
#endif						
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_reply_poll();
			}
		}

	} else {
		/* loopback */
		/* see comments on memcpy in am_store */
		memcpy(rva, lva, nbytes);

		/* invoke callback function if address is not 0 */
		TRS(request_handler);
		if(request_handler)
			((handler_mem_t)request_handler)(am_my_cluster_id,
			    rva, nbytes,
			    handler_arg);
		TRE;
		am_reply_poll();
	}

	return(0);
}

/*
 * Retrive nbytes bytes of data from address rva on dest node to address
 * lva on calling node.  Invokes handler with handler_arg on calling node 
 * when all data are received on calling node.  Return -1 on encountering
 * any errors and sets AM_errno, and 0 otherwise.
 */
int am_get(vnn_t dest, void *sva, void *dva, int nbytes, handler_mem_t reply_handler, void *handler_arg)
{
	char *bp;
	TCPAM_pkt TCPAM_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef AM_THREADS
	int sigs;
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif

	if(dest != am_my_cluster_id) {
		bp = (char *) &TCPAM_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPAM_sbuf.tag = TCPAM_GET_REQUEST_PKT;
		TCPAM_sbuf.calling_node = am_my_cluster_id;
		TCPAM_sbuf.handler = (handler_t)reply_handler;
		TCPAM_sbuf.arg1 = (long)handler_arg;
		TCPAM_sbuf.arg2 = (long)sva;
		TCPAM_sbuf.arg3 = (long)dva;
		TCPAM_sbuf.opt_arg.arg4 = nbytes;
		byte_left = sizeof(TCPAM_pkt);

#ifdef AM_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("am_get writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					AM_errno = errno;
					am_exit(-1);
				}
				am_reply_poll();
			}
		}
#ifdef AM_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		/* see comments regarding memcpy in am_store() */
		memcpy(dva, sva, nbytes);

		/* invoke callback function if address is not 0 */
		TRS(reply_handler);
		if(reply_handler)
			((handler_mem_t)reply_handler)(am_my_cluster_id, 
			    dva, nbytes,
			    handler_arg);
		TRE;
		am_reply_poll();
	}
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif

	return(0);
}

/*
 * am_isalive returns TRUE if communications via GAM
 * to the specified vnn are operating "normally".  For
 * TCPAM this means that we can do a select() on the fd's
 * associated with the sockets to that specified VNN, and
 * select returns normally.
 */
int am_isalive(vnn_t vnn)
{
#if 0
	fd_set fs;
#endif
	char msg_byte;

#if 0
	FD_SET(node_sockfd[vnn][REQ_CONN], &fs);
#endif

	if(recv(node_sockfd[vnn][REQ_CONN],&msg_byte,1,MSG_PEEK)==-1)
		if (errno == EIO) return (0);

#if 0
	if(select(1, &fs, (fd_set *) 0, (fd_set *) 0, &TCPAM_timeval) == -1)
		return(0);
	FD_SET(node_sockfd[vnn][REPLY_CONN], &fs);
#endif

	if(recv(node_sockfd[vnn][REPLY_CONN],&msg_byte,1,MSG_PEEK)==-1)
		if (errno == EIO) return (0);
#if 0
	if(select(1, &fs, (fd_set *) 0, (fd_set *) 0, &TCPAM_timeval) == -1)
		return(0);
#endif

	return(1);
}

/*
 * Returns virtual node number 
 */
int am_my_proc()
{
	return am_my_cluster_id;
}

/*
 * Returns the number of nodes
 */
int am_procs()
{
	return am_clusters;
}



/*
 * Returns AM_errno
 */
int am_errno()
{
	return(AM_errno);
}

/* Return maximum buffer size in bytes for put and get */
int am_max_size()
{
	return (SB_MAX/2);
}

#endif /* TCP */
