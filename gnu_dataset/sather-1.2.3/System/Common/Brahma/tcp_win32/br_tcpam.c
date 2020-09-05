/*
 * This file was originally part of the TCP/IP AM distribution.
 * It has been adapted to work on Solaris with Threads by Claudio Fleiner
 * at the International Computer Science Institute

 * It has been later modified to use Brahma interface for synchronization
 * by Boris Vaysman (ICSI)
 * There were some other more or less random hacks, so it is know quite
 * a bit different from the original version.
 *
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

/* The following thread and synchronization facilities need to be provided
 * by the Brahma interface layer for this extended thread safe GAM 
 * implementation to work:

 BR_FORK_LOCAL_1(func,arg) - forks a local thread
 Brahma locks
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
#include "br_tcpam.h"
#include "brahma.h"
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

/* Make sure that exit doesn't call the blocking version of exit() */
#define EXIT _exit

#define DEBUG 0
#define DEBUG2 0
#define DEBUG3 0

#define MAX_RETRY 128
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
#define TCPQQ_CONFIG "./tcpqq_config"

/* message tags */
#define TCPQQ_QQ_INT_PKT 0
#define TCPQQ_QQ_DOUBLE_PKT 1
#define TCPQQ_STORE_PKT 2
#define TCPQQ_GET_REQUEST_PKT 3
#ifdef QQ_THREADS
#define TCPQQ_THREAD_INT_PKT 4
#define TCPQQ_THREAD_DOUBLE_PKT 5
#endif

#define WAIT_HEADER 0
#define WAIT_DATA   1

/*Number of connections between peers */
/*One for send and one for receive... */
#define TCPQQ_NUM_CONNECTIONS 2
#define REQ_CONN 0
#define REPLY_CONN 1
/* Hardcoded for now, but these N connections are the building blocks
 * for MT-Safe GAM... COY
 */
#ifndef MAX
#define MAX(a,b) (a>b)?a:b
#endif

#ifndef MIN
#define MIN(a,b) (a<b)?a:b
#endif


static volatile int debug_am=1;

int aborting()
{
	abort();
}
typedef struct {
	int tag;
	qqvnn_t calling_node;
	handler_t handler;
	long arg1, arg2, arg3;
	union {
		long arg4, arg5;
		double d_arg4;
	} opt_arg;
} TCPQQ_pkt;

typedef struct {
	int tag;
	handler_t endfunc;
	long arg1, arg2, arg3, arg4;
	void *addr;
	int len;
} TCPQQ_xfer_header;

int qq_my_cluster_id;
int qq_clusters;
int QQ_errno;

/* server port to initiate connections */
int TCPQQ_server_port[TCPQQ_NUM_CONNECTIONS];

/* used with select() */
struct timeval TCPQQ_timeval;

/* maximum file descriptor number per connection */
int TCPQQ_max_sockfd[TCPQQ_NUM_CONNECTIONS];

/* bit vector used with select() */
static fd_set TCPQQ_fd_set[TCPQQ_NUM_CONNECTIONS], TCPQQ_all_fd_set;

/* can have as many processors as the max. number of file open */
struct sockaddr_in host_entries[OPEN_MAX];

/* corresponding socket descriptor to each node */
int node_sockfd[OPEN_MAX][TCPQQ_NUM_CONNECTIONS];

#ifdef QQ_THREADS
/* locks to serialise access to the sockets */
   BR_lock_t node_sock_locks[OPEN_MAX][TCPQQ_NUM_CONNECTIONS];
#  define NODE_SOCK_LOCK_CREATE BR_LOCK_CREATE()
#  define NODE_SOCK_LOCK(a,b) BR_LOCK(node_sock_locks[a][b])
#  define NODE_SOCK_UNLOCK(a,b) BR_UNLOCK(node_sock_locks[a][b])
#endif

/* message headers for request and reply sockets to each node */
TCPQQ_pkt pkt_headers[OPEN_MAX][TCPQQ_NUM_CONNECTIONS];

/* message state for request and reply sockets to each node */
int message_state[OPEN_MAX][TCPQQ_NUM_CONNECTIONS];

/* pending data bytes for request and reply sockets to each node */
int msg_bytes_left[OPEN_MAX][TCPQQ_NUM_CONNECTIONS];

/* pending data bytes for message headers ... */
int hdr_bytes_read[OPEN_MAX][TCPQQ_NUM_CONNECTIONS];

/* hostname of each node */
char node_hostname[OPEN_MAX][MAX_HOSTNAME_LEN];

int qq_reply_xfer(qqvnn_t dest, handler_t request_handler, void *handler_arg, void *lva, void *rva, int nbytes);
void qq_exit(int code);
int retry = 0;


/*
 * Gets an array of host names and checks if they are valid 
 * Then creates the host data structures and initalizes them properly; 
 * node number is the the cluster number for the node that excutes this
 */
void TCPQQ_set_hosts(int num_hosts, char** hosts)
{
	int i,j;
	char *hostname;
	unsigned long inaddr;
	extern int gethostname(char *,int);
	struct hostent *hp;

	/* store the IP address of hosts */
	i = 0;

	if (qq_clusters * TCPQQ_NUM_CONNECTIONS > OPEN_MAX) {
		fprintf(stderr,"Cannot start program with %d clusters and %d connections (%d max)\n",
		    qq_clusters, TCPQQ_NUM_CONNECTIONS, OPEN_MAX);
		EXIT(-1);
	}
	
	/* 
	 * The number of clusters could be less, the same, or greater than
	 * the number of hosts. In the first two cases, the only a subset
	 * of hosts is used. In the third case, clusters are wrapped
	 * around hosts 
	 */
	 
	j=0;  /* host name index */
	while (i < qq_clusters) {
	  hostname = hosts[j];
	  if ((inaddr = inet_addr(hostname)) != -1) {
		 /* hostname is dotted-decimal */
		 host_entries[i].sin_addr.s_addr = inaddr;
	  } else {
		 hp = gethostbyname(hostname);
		 if (hp->h_addr == 0) {
			fprintf(stderr,"Invalid hostname %s", hostname);
			EXIT(-1);
		 }
		 /* found it by name */
		 host_entries[i].sin_addr.s_addr = *((unsigned long *) hp->h_addr);
	  }
	  host_entries[i].sin_family = AF_INET;
	  host_entries[i].sin_port = 0;
	  strcpy(node_hostname[i], hostname);
	  
	  j = (j+1)%num_hosts;
	  if(++i==qq_clusters) break; 
	}

	/*	if (qq_clusters != i) {
	  fprintf(stderr,"Error: trying to start program on %d nodes", num_hosts);
	  fprintf(stderr," but passed only %d nodes to am_enable()\n", i);
	  EXIT(-1);
	}
	*/
}

/* read hostnames in the configuration file: the first line is the
 * number of nodes; the second line is the port number; subsequent
 * lines contain names of each node
 */
void TCPQQ_read_hostnames(char * config_path, int num_procs)
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
		EXIT(-1);
	}
#ifdef VERBOSE
	printf("local hostname is %s\n", my_hostname);
#endif
	hp = gethostbyname(my_hostname);
	if (hp->h_addr == 0) {
		fprintf(stderr,"Cannot get IP address of local host %s\n", my_hostname);
		fclose(config_file);
		EXIT(-1);
	}
	my_inaddr = *((unsigned int *) hp->h_addr);

	/* read in the hostnames and store the IP address */
	i = 0;
	qq_my_cluster_id = -1;
	qq_clusters = num_procs;


	if (num_procs * TCPQQ_NUM_CONNECTIONS > OPEN_MAX) {
		fprintf(stderr,"Cannot start program with %d hosts and %d connections (%d max)\n",
		    num_procs, TCPQQ_NUM_CONNECTIONS, OPEN_MAX);
		fclose(config_file);
		EXIT(-1);
	}
	
	while (!feof(config_file) && i < num_procs) {

		/*
		 * Read fast network name. Messages go over the fast network.
		 * First try to convert the hostname as
		 * a dotted-decimal number. If this fails, call gethostbyname().
		 */
		fscanf(config_file, "%s", hostname);
#ifdef VERBOSE
		printf("read fast %s\n", hostname);
#endif
		if ((inaddr = inet_addr(hostname)) != -1) {
			/* hostname is dotted-decimal */
			host_entries[i].sin_addr.s_addr = inaddr;
		} else {
			hp = gethostbyname(hostname);
			if (hp->h_addr == 0) {
				fprintf(stderr,"Invalid hostname %s", hostname);
				fclose(config_file);
				EXIT(-1);
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
#ifdef VERBOSE
		printf("read slow %s\n", hostname);
#endif
		if ((inaddr = inet_addr(hostname)) == -1) {
			/* hostname is not dotted-decimal */
			hp = gethostbyname(hostname);
			if (hp->h_addr == 0) {
				fprintf(stderr,"Invalid hostname %s", hostname);
				fclose(config_file);
				EXIT(-1);
			}
			/* found it by name */
			inaddr = *((unsigned long *) hp->h_addr);
		}
		/* determine local node number */
		if (my_inaddr == inaddr)
			qq_my_cluster_id = i;
#ifdef VERBOSE
		printf("%s read hostname %s\n", my_hostname, node_hostname[i]);
#endif
		if(++i==qq_clusters) break;
	}
	fclose(config_file);

	if (qq_clusters != i) {
		fprintf(stderr,"Error: trying to start program on %d nodes", num_procs);
		fprintf(stderr," but found only %d nodes in configuration file\n", i);
		fclose(config_file);
		EXIT(-1);
	}
	if (qq_my_cluster_id < 0) {
		fprintf(stderr,"Error: local host %s is not defined in configuration file\n",
		    node_hostname[qq_my_cluster_id]);
		fclose(config_file);
		EXIT(-1);
	}
	fclose(config_file);
}


/* set various socket options */
void TCPQQ_set_socket(int sockfd)
{
	int opt = 1;
	int opt_len = sizeof(opt);

	/* don't delay send to buffer packets */
	if (setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt TCP_NODELAY failed");
		close(sockfd);
		EXIT(-1);
	}
	/* allow local address to be reused */
	if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt SO_REUSEADDR failed");
		close(sockfd);
		EXIT(-1);
	}
	opt = /* SB_MAX */  1024;
	/* set send and receive buffer size to maximum */
	if (setsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt SO_RCVBUF failed");
		close(sockfd);
		EXIT(-1);
	}
	if (setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, (char *)&opt,
	    opt_len) < 0) {
		perror("setsockopt SO_SNDBUF failed");
		close(sockfd);
		EXIT(-1);

	}
}


/* search for the corresponding node using the IP address and store the
 * socket file descriptor to the connection_num
 */
void TCPQQ_store_sockfd(struct sockaddr_in *client_addr, int client_sockfd,
int connection_num)
{
	int i, found;

	found = 0; /* use linear search -- hope it won't be too slow */ 
	i = 0; 
	while ((i < qq_clusters) && (!found)) { 
	  if (host_entries[i].sin_addr.s_addr == client_addr->sin_addr.s_addr)
		 found = 1; 
	  else ++i; 
	} 
	if (found) 
	  node_sockfd[i][connection_num] = client_sockfd; 
	else { 
	  fprintf(stderr,"Error: cannot find IP address %s in host table\n", 
				 inet_ntoa(client_addr->sin_addr)); qq_exit(-1); }
#ifdef VERBOSE 
	printf("match socket %d for node %d (connection %d)\n",
			 client_sockfd, i, connection_num); 
#endif 
}


/* open known server port socket for receiving connection requests 
 */
int TCPQQ_open_server(int conn)
{
	int my_sockfd;
	struct sockaddr_in my_addr;

	/* first open TCPQQ_server_port as server port to set up connections */
	if ((my_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		perror("Cannot open server stream socket");
		EXIT(-1);
	}
	my_addr.sin_family = AF_INET;
	my_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	/* Own port is computed as: base_port + cluster_id */
	my_addr.sin_port = htons(TCPQQ_server_port[conn]+qq_my_cluster_id);

	TCPQQ_set_socket(my_sockfd);
#if DEBUG
	printf("my_sockfd = %d\n", my_sockfd);
	printf("s_addr = %ld\n", my_addr.sin_addr.s_addr);
	printf("s_port = %d\n", my_addr.sin_port);
	printf("TCPQQ_server_port = %d\n", TCPQQ_server_port[conn]);
#endif

	if (bind(my_sockfd, (struct sockaddr *) &my_addr,
	    sizeof(my_addr)) < 0) {

		perror("Cannot bind local address to server port");
		perror("");
		close(my_sockfd);
		EXIT(-1);
	}
	return (my_sockfd);
}


/* open sockets and connect to all other nodes on connection number 
   connection_num.  Use my_sockfd to make the connection.
 */
void TCPQQ_open_sockets(int num_procs, int connection_num, int my_sockfd)
{
	int i, addr_len, connected, retry;
	struct sockaddr_in server_addr, client_addr;
	int client_sockfd;

	/* initialize socket descriptor table */
	for (i = 0; i < OPEN_MAX; ++i) {
		node_sockfd[i][connection_num] = -1;
#ifdef QQ_THREADS
		node_sock_locks[i][connection_num]=NODE_SOCK_LOCK_CREATE;
#endif
	}

	TCPQQ_max_sockfd[connection_num] = -1;
	addr_len = sizeof(struct sockaddr);


	node_sockfd[qq_my_cluster_id][connection_num] = my_sockfd;		/*for loopback? */

	listen(my_sockfd, 5);

	/* make connections to all nodes with node number < qq_my_cluster_id */
	for (i = 0; i < qq_my_cluster_id; ++i) {
		server_addr.sin_family = AF_INET;
		server_addr.sin_addr.s_addr = host_entries[i].sin_addr.s_addr;
		/* Other host ports are computed as: base_port + cluster_id */
		server_addr.sin_port = htons(TCPQQ_server_port[connection_num]+i);
		if ((client_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			printf("Cannot open client stream socket\n");
			qq_exit(-1);
		}
		TCPQQ_set_socket(client_sockfd);
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
				/* Other host ports are comuted as: base_port + cluster_id */
				server_addr.sin_port = htons(TCPQQ_server_port[connection_num]+i);
				if ((client_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
					printf("Cannot open client stream socket\n");
					qq_exit(-1);
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
			qq_exit(-1);
		}
		node_sockfd[i][connection_num] = client_sockfd;
#ifdef VERBOSE
		printf("open socket %d for node %d, connection_num %d\n", client_sockfd, i, connection_num);
#endif
		if (client_sockfd > TCPQQ_max_sockfd[connection_num])
			TCPQQ_max_sockfd[connection_num] = client_sockfd;
	}

	/* make connections to all nodes with node number > qq_my_cluster_id */
	for (i = 1; i <= (qq_clusters - qq_my_cluster_id - 1); ++i) {
		client_sockfd = accept(my_sockfd, (struct sockaddr *) &client_addr,
		    &addr_len);

		if (client_sockfd == -1) {
			printf("Error: cannot accept connection on %s\n",
			    node_hostname[qq_my_cluster_id]);
			perror("accept");
			qq_exit(-1);
		}
		TCPQQ_set_socket(client_sockfd);

		/* store the socket file descriptor of the corresponding node */
		TCPQQ_store_sockfd(&client_addr, client_sockfd, connection_num);
		if (client_sockfd > TCPQQ_max_sockfd[connection_num])
			TCPQQ_max_sockfd[connection_num] = client_sockfd;
	}
	++TCPQQ_max_sockfd[connection_num];		/*Make the max strictly greater than */
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
			EXIT(-1);
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
void qq_enable(int nodes, int my_cluster, int num_hosts,char *host_names[])
{
	int i, ns, flags;
	char *config_path;
	char config_filename[FILENAME_MAX];
	FILE *config_file;
	int num_procs;
	int my_sockfd[TCPQQ_NUM_CONNECTIONS];
	char sather_home[512];


	/* read in the configuration file */

	/* if user does not specify full path of configuration file */
	if ((config_path = getenv("TCPQQ_CONFIG")) == NULL) {
		/* use the current directory by default */
		if((config_path=getenv("SATHER_HOME"))==NULL) { 
			config_path="./";
		} else {
			strcpy(sather_home,config_path);
			strcat(sather_home,"/System/Common/Brahma/tcp_solaris");
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
	if (fscanf(config_file, "%d", &TCPQQ_server_port[0]) == EOF) {
		printf("Cannot read server port number in configuration file\n");
		EXIT(-1);
	}

	qq_my_cluster_id = my_cluster;
	qq_clusters = nodes;
	

	for (ns = 1; ns < TCPQQ_NUM_CONNECTIONS; ns++)
	  TCPQQ_server_port[ns] = TCPQQ_server_port[0] + 1000*ns;
	
	/*	TCPQQ_read_hostnames(config_path, num_procs); */
	TCPQQ_set_hosts(num_hosts, host_names);

	check_lock(); 

	/*open the server port for receiving connection requests */
	for (ns = 0; ns < TCPQQ_NUM_CONNECTIONS; ns++) {
		my_sockfd[ns] = TCPQQ_open_server(ns);
	}

	/* open sockets and connect to all other processors; this also acts as
	 * an implicit barrier for the setup phase
	 */
	for (ns = 0; ns < TCPQQ_NUM_CONNECTIONS; ns++) {
		TCPQQ_open_sockets(num_procs, ns, my_sockfd[ns]);
		/* set up descriptor bit vector for select() */
		FD_ZERO(&TCPQQ_fd_set[ns]);
		for (i = 0; i < qq_clusters; ++i) {
			FD_SET(node_sockfd[i][ns], &TCPQQ_fd_set[ns]);
#ifdef QUICK
			FD_SET(node_sockfd[i][ns], &TCPQQ_all_fd_set);
#endif
		}
	}

	/* use non-blocking select() */
	TCPQQ_timeval.tv_sec = 0;
	TCPQQ_timeval.tv_usec = 0;

	for (ns = 0; ns < TCPQQ_NUM_CONNECTIONS; ns++) {
		for (i = 0; i < qq_clusters; ++i) {
			if (node_sockfd[i][ns] != -1) {
				flags = fcntl(node_sockfd[i][ns], F_GETFL);
				flags |= O_NONBLOCK;
				fcntl(node_sockfd[i][ns], F_SETFL, flags);
			}
		}
	}
	/*	if(getenv("DEBUG_AM") || getenv("DEBUG_PSATHER")) {
		char com[200];
		if(getenv("DEBUG_QQ_COMMAND"))
		sprintf(com,getenv("DEBUG_QQ_COMMAND"),argv[0],getpid());
		else if(getenv("DEBUG_PSATHER_COMMAND"))  
			sprintf(com,getenv("DEBUG_PSATHER_COMMAND"),argv[0],getpid());
		else
			sprintf(com,"xterm -fn 7x13 -T \"%d ($hostname) %s\" -e gdb %s %d&\n",qq_my_cluster_id,argv[0],argv[0],(int)getpid());
		system(com);
		while(debug_am);
		}
		*/
}

/* close all sockets */
void qq_disable()
{
	int i, ns;

	/*spin for a while before exit; otherwise some nodes will hang on the
	 *last barrier
	 */
	for (i = 0; i < 100000; ++i);

	/* close all sockets */
	for (ns = 0; ns < TCPQQ_NUM_CONNECTIONS; ns++)
		for (i = 0; i < qq_clusters; ++i) {
			if (node_sockfd[i][ns] != -1)
				close(node_sockfd[i][ns]);
		}
	remove_lock();
}

/* close all sockets before exit */
void qq_exit(int code)
{
	qq_disable();
	EXIT(code);
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
void qq_sleep()
{
	fd_set TCPQQ_temp_fd_set;
	int max;
	int num_pending;

#ifdef QUICK
	/*This  value is precomputed for quicker response times--yeah, right*/
	TCPQQ_temp_fd_set = TCPQQ_all_fd_set;
#else
	TCPQQ_temp_fd_set= TCPQQ_fd_set[REQ_CONN];
	OR_FD_SET(TCPQQ_temp_fd_set, TCPQQ_fd_set[REPLY_CONN]);
#endif

	max = MAX(TCPQQ_max_sockfd[REQ_CONN],TCPQQ_max_sockfd[REPLY_CONN]);

	/* 
     * check whether there are messages pending (note that
     * select() will put result in TCPQQ_temp_fd_set) -- uses
     * blocking select() call, to block caller until msg arrives
     */
	/*could we get stuck if a socket connection breaks ? */

	do {
		num_pending = select(max /*COY*/,
		    &TCPQQ_temp_fd_set,
		    (fd_set *)0,
		    (fd_set *)0, 0);

		if(num_pending == -1) {
			QQ_errno = errno;
			qq_exit(-1);
		}

	} while(num_pending == 0);

	return;
}
#endif

#ifdef QQ_THREADS
/* This simply executes a handler supplied to qq_thr_create* 
 * The new context thread must have been already forked off 
 * when this is executed */

static void qq_start_fork_handler(TCPQQ_pkt *pp)
{
  /* nothing special to do for Brahma 
	  QQ_INIT_THREAD; */
	switch(pp->tag) {
	case TCPQQ_THREAD_INT_PKT:
		((handler_4_t)pp->handler)(pp->calling_node,
		    pp->arg1, pp->arg2, pp->arg3, pp->opt_arg.arg4);
		break;
	case TCPQQ_THREAD_DOUBLE_PKT:
		((handler_df_t)pp->handler)(pp->calling_node,
		    pp->arg1, pp->arg2, pp->opt_arg.d_arg4);
		break;
	}
	/* free(pp); XXXX */
}

#endif /* QQ_THREADS */
/*
 * Poll request and reply networks. Returns -1 on encountering 
 * any error and may set QQ_errno, and returns 0 otherwise.
 *
 * If block is nonzero, it blocks until a message is received.
 */
 

void qq_poll(int block)
{
	char *cp;
	TCPQQ_pkt *pp;
	TCPQQ_xfer_header *hp;
	fd_set TCPQQ_temp_fd_set;

	int i, n, m, sn;
	int max, sockfd;
	int num_fd_ready;

#if DEBUG
	printf("qq_poll enter\n");
#endif

	/*	if (!BR_thread_may_poll()) {
		
		return;
		}*/

#ifdef QUICK
	/*This  value is precomputed for quicker response times--yeah, right*/
	TCPQQ_temp_fd_set = TCPQQ_all_fd_set;
#else
	TCPQQ_temp_fd_set= TCPQQ_fd_set[REQ_CONN];
	OR_FD_SET(TCPQQ_temp_fd_set, TCPQQ_fd_set[REPLY_CONN]);
#endif
	max = MAX(TCPQQ_max_sockfd[REQ_CONN], TCPQQ_max_sockfd[REPLY_CONN]);

#ifdef QQ_THREADS
	if (block) {
		/* Select blocks until something arrives */
		num_fd_ready =  select(max, &TCPQQ_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, NULL);
	}
	else {
		num_fd_ready =  select(max, &TCPQQ_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, &TCPQQ_timeval);
	}		
	if(num_fd_ready == -1) {
#else
	if((num_fd_ready=select(max, &TCPQQ_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, &TCPQQ_timeval)) == -1) {
#endif
		QQ_errno = errno;
		qq_exit(-1);
	}

	if (!num_fd_ready){
	  return;
	}

	for(i = 0; i < qq_clusters; i++) {
		for(sn = 0; sn < TCPQQ_NUM_CONNECTIONS; sn++) {
/*
#ifdef QQ_THREADS
			NODE_SOCK_LOCK(i,sn);
#endif
*/
			sockfd = node_sockfd[i][sn];

			if(FD_ISSET(sockfd, &TCPQQ_temp_fd_set)) {
				pp = &pkt_headers[i][sn];

				/* check if waiting for message header */
				if(message_state[i][sn] == WAIT_HEADER) {
#if DEBUG3
					printf("qq_poll: read header...");
#endif
					n = sizeof(TCPQQ_pkt) - hdr_bytes_read[i][sn];
					if((m = read(sockfd, pp + hdr_bytes_read[i][sn], n)) == -1) {
						if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
							QQ_errno = errno;
							printf("node %d qq_poll: read header error %d", qq_my_cluster_id, QQ_errno);
							perror("qq_poll: read header error");
							qq_exit(-1);
						}
						retry++; 

					} else {
						retry = 0;
						hdr_bytes_read[i][sn] += m;

						if(hdr_bytes_read[i][sn] == sizeof(TCPQQ_pkt)) {
							hdr_bytes_read[i][sn] = 0;
#if DEBUG2
							printf("qq_poll %x %x %x %x %x %x %x %x\n", 
							    (int)pp->tag, (int)pp->handler, (int)pp->calling_node, (int)pp->arg1, 
							    (int)pp->arg2, (int)pp->arg3, (int)pp->opt_arg.arg4,(int)pp->opt_arg.arg5);
#endif
							/* check if active message with integer arguments */
							if (pp->tag == TCPQQ_QQ_INT_PKT) {
								TRS(pp->handler);
								((handler_4_t)pp->handler)(pp->calling_node,
								    pp->arg1,
								    pp->arg2,
								    pp->arg3,
								    pp->opt_arg.arg4);
							        TRE;

								/* check if active message with floating point arguments */
							} else if (pp->tag == TCPQQ_QQ_DOUBLE_PKT) {
								TRS(pp->handler);
								((handler_df_t)pp->handler)(pp->calling_node,
								    pp->arg1, pp->arg2, pp->opt_arg.d_arg4);
								TRE;

								/* check if qq_get request and do reply back */
#ifdef QQ_THREADS
							} else if (pp->tag == TCPQQ_THREAD_INT_PKT || pp->tag == TCPQQ_THREAD_DOUBLE_PKT) {
								TCPQQ_pkt *pp1;
								pp1=malloc(sizeof(*pp1));
								memcpy(pp1,pp,sizeof(*pp1));
								BR_FORK_LOCAL_1(qq_start_fork_handler,pp1);
#endif
							} else if (pp->tag == TCPQQ_GET_REQUEST_PKT) {
								qq_reply_xfer(pp->calling_node,
								    pp->handler /* handler */,
								    (void *)pp->arg1 /* handler arg*/,
								    (void *)pp->arg2 /* src addr */,
								    (void *)pp->arg3 /* dst addr */,
								    pp->opt_arg.arg4 /* nbytes */);

								/* check if qq_store header, and change state */
							} else if (pp->tag == TCPQQ_STORE_PKT) {
#if DEBUG3
								printf("reading to read data\n");
#endif
								message_state[i][sn] = WAIT_DATA;

								hp = (TCPQQ_xfer_header *) &pkt_headers[i][sn];
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
								qq_exit(-1);
							}
						}
					}

					/* we've received hdr, waiting for data */
				} else {

					hp = (TCPQQ_xfer_header *) &pkt_headers[i][sn];
					n = msg_bytes_left[i][sn];
					cp = (char *) hp->addr;
#if DEBUG3
					printf("qq_poll: data read...\n");
#endif
					if((m = read(sockfd, &cp[hp->len-n], n)) == -1) {
					  if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
						 QQ_errno = errno;
							printf("node %d qq_poll: read data error", qq_my_cluster_id);
							perror("qq_poll: read data error");
							qq_exit(-1);
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
			/*
			  #ifdef QQ_THREADS
			  NODE_SOCK_UNLOCK(i,sn);
			  #endif		
			  */		  
		}
	}

#if DEBUG
	printf("qq_poll exit\n");
#endif		

	return;
}
	
	/*
	 * Poll request and reply networks.  Returns -1 on encountering 
 * any error and may set QQ_errno, and returns 0 otherwise.
 */
int qq_reply_poll()
{
	char *cp;
	TCPQQ_pkt *pp;
	TCPQQ_xfer_header *hp;
	fd_set TCPQQ_temp_fd_set;

	int i, n, m;
	int max, sockfd;
	int num_fd_ready;

#if DEBUG
	printf("qq_reply_poll enter\n");
#endif

	if (!BR_thread_may_poll()) {
	  /* Yield to let the thread manager kick in and drain the queues */
	  /*BR_THREAD_YIELD; */
	  return 0;
	}

	TCPQQ_temp_fd_set= TCPQQ_fd_set[REPLY_CONN];
	max = TCPQQ_max_sockfd[REPLY_CONN];

#ifdef QQ_THREADS
	if((num_fd_ready=select(max, &TCPQQ_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, &TCPQQ_timeval)) == -1) {
#else
	if((num_fd_ready=select(max, &TCPQQ_temp_fd_set, (fd_set *) 0,
	    (fd_set *) 0, &TCPQQ_timeval)) == -1) {
#endif
		QQ_errno = errno;
		qq_exit(-1);
	}

	if (!num_fd_ready){
	  return 0;
	}

	for(i = 0; i < qq_clusters; i++) {
	  /*#ifdef QQ_THREADS
		 NODE_SOCK_LOCK(i,REPLY_CONN);
		 #endif */
		sockfd = node_sockfd[i][REPLY_CONN];

		if(FD_ISSET(sockfd, &TCPQQ_temp_fd_set)) {
			pp = &pkt_headers[i][REPLY_CONN];

			/* check if waiting for message header */
			if(message_state[i][REPLY_CONN] == WAIT_HEADER) {
#if DEBUG3
				printf("qq_reply_poll: read header...");
#endif
				n = sizeof(TCPQQ_pkt) - hdr_bytes_read[i][REPLY_CONN];
				if((m = read(sockfd, pp + hdr_bytes_read[i][REPLY_CONN], n)) == -1) {
					if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
						QQ_errno = errno;
						printf("node %d qq_poll: read data error", qq_my_cluster_id);
						perror("qq_poll: read data error");
						qq_exit(-1);
					}
					retry++;

				} else {
					retry = 0;
					hdr_bytes_read[i][REPLY_CONN] += m;

					if(hdr_bytes_read[i][REPLY_CONN] == sizeof(TCPQQ_pkt)) {
						hdr_bytes_read[i][REPLY_CONN] = 0;
#if DEBUG2
						printf("qq_reply_poll %x %x %x %x %x %x %x %x\n", 
						    (int)pp->tag, (int)pp->handler, (int)pp->calling_node, (int)pp->arg1, 
						    (int)pp->arg2, (int)pp->arg3, (int)pp->opt_arg.arg4,(int)pp->opt_arg.arg5);
#endif
						/* check if active message with integer arguments */
						if (pp->tag == TCPQQ_QQ_INT_PKT) {
							TRS(pp->handler);
							((handler_4_t)pp->handler)(pp->calling_node,
							    pp->arg1,
							    pp->arg2,
							    pp->arg3,
							    pp->opt_arg.arg4);
							TRE;

							/* check if active message with floating point arguments */
						} else if (pp->tag == TCPQQ_QQ_DOUBLE_PKT) {
							TRS(pp->handler);
							((handler_df_t)pp->handler)(pp->calling_node,
							    pp->arg1, pp->arg2, pp->opt_arg.d_arg4);
							TRE;

							/* check if qq_get request and do reply back */
						} else if (pp->tag == TCPQQ_GET_REQUEST_PKT) {
							qq_reply_xfer(pp->calling_node,
							    pp->handler /* handler */,
							    (void *)pp->arg1 /* handler arg*/,
							    (void *)pp->arg2 /* src addr */,
							    (void *)pp->arg3 /* dst addr */,
							    pp->opt_arg.arg4 /* nbytes */);

							/* check if qq_store header, and change state */
						} else if (pp->tag == TCPQQ_STORE_PKT) {
#if DEBUG3
							printf("ready to read data\n");
#endif
							message_state[i][REPLY_CONN] = WAIT_DATA;

							hp = (TCPQQ_xfer_header *) &pkt_headers[i][REPLY_CONN];
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
							qq_exit(-1);
						}
					}
				}

				/* we've received hdr, waiting for data */
			} else {

				hp = (TCPQQ_xfer_header *) &pkt_headers[i][REPLY_CONN];
				n = msg_bytes_left[i][REPLY_CONN];
				cp = (char *) hp->addr;
#if DEBUG3
				printf("qq_reply_poll: data read...\n");
#endif
				if((m = read(sockfd, &cp[hp->len-n], n)) == -1) {
					if (((errno	!= EAGAIN) && (errno != 0)) || (retry >=  MAX_RETRY))	 {
						QQ_errno = errno;
						printf("node %d qq_poll: read data error", qq_my_cluster_id);
						perror("qq_poll: read data error");
						qq_exit(-1);
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
		/*#ifdef QQ_THREADS
		  NODE_SOCK_UNLOCK(i,REPLY_CONN);
		  #endif*/
	}

#if DEBUG
	printf("qq_reply_poll exit\n");
#endif		

	return(0);
}

/*
 * while *flag != value, poll for messages.  If poll 
 * encounters any errors, qq_wait returns -1 and does
 * not modify the flag.
 */
int qq_wait(volatile int *flag, 
int value)
{
	while (*flag < value) qq_poll(0);
	(*flag)-=value;
	return 0;
}

/*
 * poll for messages until *flag == value.  If poll encounters
 * any errors, qq_poll_wait returns -1 and does not modify the
 * flag.
 */
int qq_poll_wait(volatile int *flag, 
int value)
{
	do { qq_poll(0); } while(*flag < value);
	(*flag)-=value;
	return 0;
}

#ifdef QQ_THREADS
/*
 * Starts a new thread on dest with upto 4 interger arguments. Returns -1 on
 * encountering any error and sets QQ_errno, return 0 otherwise.
 */
int thr_create_0(qqvnn_t dest,handler_0_t h)     			   { return thr_create_4(dest,(handler_4_t)h,111,222,333,444); }
int thr_create_1(qqvnn_t dest,handler_1_t h,long a1)     		   { return thr_create_4(dest,(handler_4_t)h,a1,222,333,444); }
int thr_create_2(qqvnn_t dest,handler_2_t h,long a1,long a2)     	   { return thr_create_4(dest,(handler_4_t)h,a1,a2,333,444); }
int thr_create_3(qqvnn_t dest,handler_3_t h,long a1,long a2,long a3) { return thr_create_4(dest,(handler_4_t)h,a1,a2,a3,444); }
int thr_create_4(qqvnn_t dest,
handler_4_t fun,
long arg1, long arg2,
long arg3, long arg4)
{
	char *bp;
	TCPQQ_pkt TCPQQ_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
	/* int sigs;

		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */
	
	if(dest != qq_my_cluster_id) {
		bp = (char *)&TCPQQ_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPQQ_sbuf.tag = TCPQQ_THREAD_INT_PKT;
		TCPQQ_sbuf.calling_node = qq_my_cluster_id;
		TCPQQ_sbuf.handler = (handler_t)fun;
		TCPQQ_sbuf.arg1 = arg1;
		TCPQQ_sbuf.arg2 = arg2;
		TCPQQ_sbuf.arg3 = arg3;
		TCPQQ_sbuf.opt_arg.arg4 = arg4;

		byte_left = sizeof(TCPQQ_pkt);
		NODE_SOCK_LOCK(dest,REQ_CONN);
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("thr_create_4 writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
			  byte_left -= byte_written;
			  bp += byte_written;
			  
			} 
			else {
			  if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
				 QQ_errno = errno;
				 qq_exit(-1);
			  }
			  qq_poll(0);
			}
		}
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
		
	} else {
		/* loopback */
		TCPQQ_pkt *pp1;
		pp1=malloc(sizeof(*pp1));
		pp1->tag = TCPQQ_THREAD_INT_PKT;
		pp1->calling_node = qq_my_cluster_id;
		pp1->handler = (handler_t)fun;
		pp1->arg1 = arg1;
		pp1->arg2 = arg2;
		pp1->arg3 = arg3;
		pp1->opt_arg.arg4 = arg4;
		BR_FORK_LOCAL_1(qq_start_fork_handler,pp1);
	}
	/*	if(sigs) THR_ENABLE_SIGNAL; */

	return(0);
}
/*
 * Starts thread with 2 interger arguments and 1 double argument.
 * Returns -1 on encountering any error and sets QQ_errno, return 0 
 * otherwise.
 */
int thr_create_df(qqvnn_t dest, handler_df_t fun,long arg2,long arg3,double arg1)
{
	char *bp;
	TCPQQ_pkt TCPQQ_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
	/* int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */

	if(dest != qq_my_cluster_id) {
		bp = (char *)&TCPQQ_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPQQ_sbuf.tag = TCPQQ_THREAD_DOUBLE_PKT;
		TCPQQ_sbuf.calling_node = qq_my_cluster_id;
		TCPQQ_sbuf.handler = (handler_t)fun;
		TCPQQ_sbuf.arg1 = arg2;
		TCPQQ_sbuf.arg2 = arg3;
		TCPQQ_sbuf.opt_arg.d_arg4 = arg1;

		byte_left = sizeof(TCPQQ_pkt);
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
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_poll(0);
			}
		}
		NODE_SOCK_UNLOCK(dest,REQ_CONN);

	} else {
		/* loopback */
		TCPQQ_pkt *pp1;
		pp1=malloc(sizeof(*pp1));
		pp1->tag = TCPQQ_THREAD_DOUBLE_PKT;
		pp1->calling_node = qq_my_cluster_id;
		pp1->handler = (handler_t)fun;
		pp1->arg1 = arg2;
		pp1->arg2 = arg3;
		pp1->opt_arg.d_arg4 = arg1;
		BR_FORK_LOCAL_1(qq_start_fork_handler,pp1);
		qq_poll(0);
	}
	/*	if(sigs) THR_ENABLE_SIGNAL; */
	return(0);
}

#endif /* QQ_THREADS */


/*
 * Sends request with 4 interger arguments. Returns -1 on
 * encountering any error and sets QQ_errno, return 0 otherwise.
 */
int qq_request_0(qqvnn_t dest,handler_0_t h)     			   { return qq_request_4(dest,(handler_4_t)h,1,2,3,4); }
int qq_request_1(qqvnn_t dest,handler_1_t h,long a1)     		   { return qq_request_4(dest,(handler_4_t)h,a1,2,3,4); }
int qq_request_2(qqvnn_t dest,handler_2_t h,long a1,long a2)     	   { return qq_request_4(dest,(handler_4_t)h,a1,a2,3,4); }
int qq_request_3(qqvnn_t dest,handler_3_t h,long a1,long a2,long a3) { return qq_request_4(dest,(handler_4_t)h,a1,a2,a3,4); }
int qq_request_4(qqvnn_t dest,
handler_4_t fun,
long arg1, long arg2,
long arg3, long arg4)
{
	char *bp;
	TCPQQ_pkt TCPQQ_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef QQ_THREADS
	/* int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */
#endif

#if DEBUG
	printf("starting qq_request_4\n");fflush(stdout);
#endif				
	if(dest != qq_my_cluster_id) {
		bp = (char *)&TCPQQ_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPQQ_sbuf.tag = TCPQQ_QQ_INT_PKT;
		TCPQQ_sbuf.calling_node = qq_my_cluster_id;
		TCPQQ_sbuf.handler = (handler_t)fun;
		TCPQQ_sbuf.arg1 = arg1;
		TCPQQ_sbuf.arg2 = arg2;
		TCPQQ_sbuf.arg3 = arg3;
		TCPQQ_sbuf.opt_arg.arg4 = arg4;
#if DEBUG2
		printf("qq_request %x %x %x %x %x %x %x %x %x\n", 
		    dest,(int)TCPQQ_sbuf.tag, (int)TCPQQ_sbuf.handler, (int)TCPQQ_sbuf.calling_node, (int)TCPQQ_sbuf.arg1, 
		    (int)TCPQQ_sbuf.arg2, (int)TCPQQ_sbuf.arg3, (int)TCPQQ_sbuf.opt_arg.arg4,(int)TCPQQ_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPQQ_pkt);
#ifdef QQ_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_request_4 writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_poll(0);
			}
		}
#ifdef QQ_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		TRS(fun);
		((handler_4_t)fun)(qq_my_cluster_id, 
		    arg1, arg2,
		    arg3, arg4);
		TRE;
		qq_poll(0);
	}
#ifdef QQ_THREADS
	/*	if(sigs) THR_ENABLE_SIGNAL; */
#endif
#if DEBUG
	printf("qq_request_4 done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Sends request with 2 interger arguments and 1 double argument.
 * Returns -1 on encountering any error and sets QQ_errno, return 0 
 * otherwise.
 */
int qq_request_df(qqvnn_t dest, handler_df_t fun,long arg2,long arg3,double arg1)
{
	char *bp;
	TCPQQ_pkt TCPQQ_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef QQ_THREADS
	/* int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */
#endif
#if DEBUG
	printf("starting qq_request_df\n");fflush(stdout);
#endif				

	if(dest != qq_my_cluster_id) {
		bp = (char *)&TCPQQ_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPQQ_sbuf.tag = TCPQQ_QQ_DOUBLE_PKT;
		TCPQQ_sbuf.calling_node = qq_my_cluster_id;
		TCPQQ_sbuf.handler = (handler_t)fun;
		TCPQQ_sbuf.arg1 = arg2;
		TCPQQ_sbuf.arg2 = arg3;
		TCPQQ_sbuf.opt_arg.d_arg4 = arg1;
#if DEBUG2
		printf("qq_request_df %x %x %x %x %x %x %x %x %x\n", 
		    dest,(int)TCPQQ_sbuf.tag, (int)TCPQQ_sbuf.handler, (int)TCPQQ_sbuf.calling_node, (int)TCPQQ_sbuf.arg1, 
		    (int)TCPQQ_sbuf.arg2, (int)TCPQQ_sbuf.arg3, (int)TCPQQ_sbuf.opt_arg.arg4,(int)TCPQQ_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPQQ_pkt);
#ifdef QQ_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_request_df writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_poll(0);
			}
		}
#ifdef QQ_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		TRS(fun);
		((handler_df_t)fun)(qq_my_cluster_id, arg2, arg3, arg1);
		TRE;
		qq_poll(0);
	}
#ifdef QQ_THREADS
	/* 	if(sigs) THR_ENABLE_SIGNAL; */
#endif
#if DEBUG
	printf("qq_request_df done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Sends a reply message with 4 integer arguments.  Returns -1 on
 * encountering any error and sets QQ_errno, return 0 otherwise.
 */
int qq_reply_0(qqvnn_t dest,handler_0_t h)     			   { return qq_reply_4(dest,(handler_4_t)h,1,2,3,4); }
int qq_reply_1(qqvnn_t dest,handler_1_t h,long a1)     		   { return qq_reply_4(dest,(handler_4_t)h,a1,2,3,4); }
int qq_reply_2(qqvnn_t dest,handler_2_t h,long a1,long a2)     	   { return qq_reply_4(dest,(handler_4_t)h,a1,a2,3,4); }
int qq_reply_3(qqvnn_t dest,handler_3_t h,long a1,long a2,long a3)   { return qq_reply_4(dest,(handler_4_t)h,a1,a2,a3,4); }
int qq_reply_4(qqvnn_t dest, handler_4_t fun, long arg1, long arg2, long arg3, long arg4)
{
	char *bp;
	TCPQQ_pkt TCPQQ_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef QQ_THREADS
	/* int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */
#endif
#if DEBUG
	printf("starting qq_reply_4\n");fflush(stdout);
#endif				

	if(dest != qq_my_cluster_id) {
		bp = (char *)&TCPQQ_sbuf;
		dest_sockfd = node_sockfd[dest][REPLY_CONN];
		TCPQQ_sbuf.tag = TCPQQ_QQ_INT_PKT;
		TCPQQ_sbuf.calling_node = qq_my_cluster_id;
		TCPQQ_sbuf.handler = (handler_t)fun;
		TCPQQ_sbuf.arg1 = arg1;
		TCPQQ_sbuf.arg2 = arg2;
		TCPQQ_sbuf.arg3 = arg3;
		TCPQQ_sbuf.opt_arg.arg4 = arg4;
#if DEBUG2
		printf("qq_reply %x, %x %x %x %x %x %x %x %x\n", 
		    dest, (int)TCPQQ_sbuf.tag, (int)TCPQQ_sbuf.handler, (int)TCPQQ_sbuf.calling_node, (int)TCPQQ_sbuf.arg1, 
		    (int)TCPQQ_sbuf.arg2, (int)TCPQQ_sbuf.arg3, (int)TCPQQ_sbuf.opt_arg.arg4,(int)TCPQQ_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPQQ_pkt);
#ifdef QQ_THREADS
/*		NODE_SOCK_LOCK(dest,REPLY_CONN); */
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_reply_4 writes %d bytes\n", byte_written);
#endif							
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_reply_poll();
			}
		}
#ifdef QQ_THREADS
/*		NODE_SOCK_UNLOCK(dest,REPLY_CONN); */
#endif

	} else {
		/* loopback */
		TRS(fun);
		((handler_4_t)fun)(qq_my_cluster_id, 
		    arg1, arg2,
		    arg3, arg4);
		TRE;
		qq_reply_poll();
	}
#ifdef QQ_THREADS
	/*	if(sigs) THR_ENABLE_SIGNAL; */
#endif
#if DEBUG
	printf("qq_reply_4 done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Sends a reply message with 3 integer arguments and 1 
 * double-precision floating point argument.  Returns -1 on
 * encountering any error and sets QQ_errno, return 0 otherwise.
 */
int qq_reply_df(qqvnn_t dest, handler_df_t fun,long arg2,long arg3,double arg1)
{
	register char *bp;
	TCPQQ_pkt TCPQQ_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef QQ_THREADS
	/* int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */
#endif
#if DEBUG
	printf("starting qq_reply_df\n");fflush(stdout);
#endif				

	if(dest != qq_my_cluster_id) {
		bp = (char *)&TCPQQ_sbuf;
		dest_sockfd = node_sockfd[dest][REPLY_CONN];
		TCPQQ_sbuf.tag = TCPQQ_QQ_DOUBLE_PKT;
		TCPQQ_sbuf.calling_node = qq_my_cluster_id;
		TCPQQ_sbuf.handler = (handler_t)fun;
		TCPQQ_sbuf.arg1 = arg2;
		TCPQQ_sbuf.arg2 = arg3;
		TCPQQ_sbuf.opt_arg.d_arg4 = arg1;
#if DEBUG2
		printf("qq_reply_df %x %x %x %x %x %x %x %x %x\n", 
		    dest, (int)TCPQQ_sbuf.tag, (int)TCPQQ_sbuf.handler, (int)TCPQQ_sbuf.calling_node, (int)TCPQQ_sbuf.arg1, 
		    (int)TCPQQ_sbuf.arg2, (int)TCPQQ_sbuf.arg3, (int)TCPQQ_sbuf.opt_arg.arg4,(int)TCPQQ_sbuf.opt_arg.arg5);
#endif

		byte_left = sizeof(TCPQQ_pkt);
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_reply_df writes %d bytes\n", byte_written);
#endif							
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_reply_poll();
			}
		}

	} else {
		/* loopback */
		TRS(fun);
		((handler_df_t)fun)(qq_my_cluster_id, arg2, arg3, arg1);
		TRE;
		qq_reply_poll();
	}
#ifdef QQ_THREADS
	/*	if(sigs) THR_ENABLE_SIGNAL; */
#endif
#if DEBUG
	printf("qq_reply_df done\n");fflush(stdout);
#endif

	return(0);
}

/*
 * Copy nbytes bytes of data from address lva on calling node to address
 * rva on dest node. Invoke handler with handler_arg on dest node when all
 * data are received on dest node.  Returns -1 on encountering any error and
 * sets QQ_errno, returns 0 otherwise.
 */
int qq_store(qqvnn_t dest, void *lva, void *rva, int nbytes, handler_mem_t request_handler, void *handler_arg)
{
	char *bp;
	TCPQQ_xfer_header header;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef QQ_THREADS
	/* int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */
#endif

	if(dest != qq_my_cluster_id) {
		header.tag = TCPQQ_STORE_PKT;
		header.len = nbytes;
		header.addr = rva;
		header.endfunc = (handler_t)request_handler;
		header.arg1 = (int)handler_arg;
		header.arg2 = qq_my_cluster_id;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		bp = (char *)&header;

		/* send the header */
		byte_left = sizeof(TCPQQ_xfer_header);
#ifdef QQ_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_store writes %d bytes of header\n", byte_written);
#endif			
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_poll(0);
			}
		}
		/* send the buffer */
		bp = lva;
		byte_left = nbytes;
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG3
			printf("qq_store %d/%d\n", byte_written, nbytes);
#endif
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_poll(0);
			}
		}
#ifdef QQ_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		/* was memmove, but changed it to memcpy (qq_store does not specify
	   the behaviour in cases of overlaping memory */
		memcpy(rva, lva, nbytes);

		/* invoke callback function if address is not 0 */
		TRS(request_handler);
		if(request_handler)
			((handler_mem_t)request_handler)(qq_my_cluster_id, 
			    rva, nbytes,
			    handler_arg);
		TRE;
		qq_poll(0);
	}

#ifdef QQ_THREADS
	/*	if(sigs) THR_ENABLE_SIGNAL; */
#endif
	return(0);
}
/*
 * qq_store_async is supposed to return immediatly, we cheat */
int qq_store_async(qqvnn_t dest, void *lva, void *rva, int nbytes, handler_mem_t request_handler, void *handler_arg,handler_mem_t endfunc,void *earg)
{
	int i;
	i=qq_store(dest,lva,rva,nbytes,request_handler,handler_arg);
	TRS(endfunc);
	if(i==0 && endfunc!=NULL) (*endfunc)(dest,lva,nbytes,earg);
	TRE;
	return i;
}
/*
 * Like qq_store, but used for the reply bulk transfer for qq_gets.
 * Also, qq_reply_xfer only polls the reply network.  Returns -1 on 
 * encountering any errors and sets QQ_errno, and 0 otherwise.
 */
int qq_reply_xfer(qqvnn_t dest, 
handler_t request_handler,
void *handler_arg,
void *lva,
void *rva, 
int nbytes)
{
	char *bp;
	TCPQQ_xfer_header header;

	int byte_left;
	int dest_sockfd;
	int byte_written;

	if(dest != qq_my_cluster_id) {
		header.tag = TCPQQ_STORE_PKT;
		header.endfunc = request_handler;
		header.arg1 = (int)handler_arg;
		header.arg2 = qq_my_cluster_id;
		header.addr = rva;
		header.len = nbytes;
		dest_sockfd = node_sockfd[dest][REPLY_CONN];
		bp = (char *) &header;

		/* send the header */
		byte_left = sizeof(TCPQQ_xfer_header);
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_reply_xfer writes %d bytes of header\n", byte_written);
#endif			
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_reply_poll();
			}
		}

		/* send the data buffer */
		bp = lva;
		byte_left = nbytes;

		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_reply_xfer writes %d bytes of buffer\n", byte_written);
#endif						
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_reply_poll();
			}
		}

	} else {
		/* loopback */
		/* see comments on memcpy in qq_store */
		memcpy(rva, lva, nbytes);

		/* invoke callback function if address is not 0 */
		TRS(request_handler);
		if(request_handler)
			((handler_mem_t)request_handler)(qq_my_cluster_id,
			    rva, nbytes,
			    handler_arg);
		TRE;
		qq_reply_poll();
	}

	return(0);
}

/*
 * Retrive nbytes bytes of data from address rva on dest node to address
 * lva on calling node.  Invokes handler with handler_arg on calling node 
 * when all data are received on calling node.  Return -1 on encountering
 * any errors and sets QQ_errno, and 0 otherwise.
 */
int qq_get(qqvnn_t dest, void *sva, void *dva, int nbytes, handler_mem_t reply_handler, void *handler_arg)
{
	char *bp;
	TCPQQ_pkt TCPQQ_sbuf;

	int byte_left;
	int dest_sockfd;
	int byte_written;
#ifdef QQ_THREADS
	/* int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL; */
#endif

	if(dest != qq_my_cluster_id) {
		bp = (char *) &TCPQQ_sbuf;
		dest_sockfd = node_sockfd[dest][REQ_CONN];
		TCPQQ_sbuf.tag = TCPQQ_GET_REQUEST_PKT;
		TCPQQ_sbuf.calling_node = qq_my_cluster_id;
		TCPQQ_sbuf.handler = (handler_t)reply_handler;
		TCPQQ_sbuf.arg1 = (long)handler_arg;
		TCPQQ_sbuf.arg2 = (long)sva;
		TCPQQ_sbuf.arg3 = (long)dva;
		TCPQQ_sbuf.opt_arg.arg4 = nbytes;
		byte_left = sizeof(TCPQQ_pkt);

#ifdef QQ_THREADS
		NODE_SOCK_LOCK(dest,REQ_CONN);
#endif
		while(byte_left > 0) {
			byte_written = write(dest_sockfd, bp, byte_left);
#if DEBUG
			printf("qq_get writes %d bytes\n", byte_written);
#endif				
			if(byte_written > 0) {
				byte_left -= byte_written;
				bp += byte_written;

			} else {
				if(errno != EWOULDBLOCK && errno != EAGAIN && errno != 0) {
					QQ_errno = errno;
					qq_exit(-1);
				}
				qq_reply_poll();
			}
		}
#ifdef QQ_THREADS
		NODE_SOCK_UNLOCK(dest,REQ_CONN);
#endif

	} else {
		/* loopback */
		/* see comments regarding memcpy in qq_store() */
		memcpy(dva, sva, nbytes);

		/* invoke callback function if address is not 0 */
		TRS(reply_handler);
		if(reply_handler)
			((handler_mem_t)reply_handler)(qq_my_cluster_id, 
			    dva, nbytes,
			    handler_arg);
		TRE;
		qq_reply_poll();
	}
#ifdef QQ_THREADS
	/*	if(sigs) THR_ENABLE_SIGNAL; */
#endif

	return(0);
}

/*
 * qq_isalive returns TRUE if communications via GAM
 * to the specified vnn are operating "normally".  For
 * TCPAM this means that we can do a select() on the fd's
 * associated with the sockets to that specified VNN, and
 * select returns normally.
 */
int qq_isalive(qqvnn_t vnn)
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
	if(select(1, &fs, (fd_set *) 0, (fd_set *) 0, &TCPQQ_timeval) == -1)
		return(0);
	FD_SET(node_sockfd[vnn][REPLY_CONN], &fs);
#endif

	if(recv(node_sockfd[vnn][REPLY_CONN],&msg_byte,1,MSG_PEEK)==-1)
		if (errno == EIO) return (0);
#if 0
	if(select(1, &fs, (fd_set *) 0, (fd_set *) 0, &TCPQQ_timeval) == -1)
		return(0);
#endif

	return(1);
}

/*
 * Returns virtual node number 
 */
qqvnn_t qq_my_proc()
{
	return qq_my_cluster_id;
}

/*
 * Returns the number of nodes
 */
int qq_procs()
{
	return qq_clusters;
}



/*
 * Returns QQ_errno
 */
int qq_errno()
{
	return(QQ_errno);
}

/* Return maximum buffer size in bytes for put and get */
int qq_max_size()
{
	return (SB_MAX/2);
}

