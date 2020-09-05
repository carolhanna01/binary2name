/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1995/96 by International Computer Science Institute         */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

/* 
 * Shared Memory Active Message Library
 *
 * Version 1.0 (released for Sather 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>
#include <assert.h>
#include <memory.h>
#include <string.h>
#include <sys/types.h>

#ifdef __CYGWIN32__
# include <fcntl.h>
# include <sys/mman.h>
# define MAP_FAILED ((caddr_t) -1)
#else
# include <sys/ipc.h>
# include <sys/shm.h>
#endif
 
#include <errno.h>

#include "am.h"
#include "am_int.h"

#if !defined(TCP) && !defined(LANAI_NET) && !defined(MEIKO)
static volatile int debug_am=1;

#ifdef SUNOS4
void *shmat(int,const void*,int);
int shmdt(char *);
int shmget(key_t,size_t,int);
int shmctl(int,int,struct shmid_ds *);
int fprintf(FILE*,const char *,...);
int printf(const char *,...);
int fflush(FILE *);
void perror(char *);
#endif

int am_clusters;
int am_my_cluster_id;

static int chpids[AM_MAX_NODES];
static int sid;
AM_MSG_POOL am_pool;
#ifdef SPINLOCK_LOCK
   static spinlock_t am_lock;
   static spinlock_t am_write_lock[AM_MAX_NODES];
#  define AM_LOCK 		SPINLOCK_LOCK(am_lock);
#  define AM_UNLOCK		SPINLOCK_UNLOCK(am_lock);
#  define AM_LOCK_CREATE 	0
#  define AM_WRITE_LOCK(a)	SPINLOCK_LOCK(am_write_lock[a])
#  define AM_WRITE_UNLOCK(a)	SPINLOCK_UNLOCK(am_write_lock[a])
#  define AM_WRITE_CREATE	0
#else
   static lock_t am_lock;
   static lock_t am_write_lock[AM_MAX_NODES];
#  define AM_LOCK 		LCK_LOCK(am_lock);
#  define AM_UNLOCK 		LCK_UNLOCK(am_lock);
#  define AM_LOCK_CREATE 	LCK_CREATE
#  define AM_WRITE_LOCK(a)	LCK_LOCK(am_write_lock[a])
#  define AM_WRITE_UNLOCK(a)	LCK_UNLOCK(am_write_lock[a])
#  define AM_WRITE_CREATE	LCK_CREATE
#endif

struct AM_MSG_QUEUE_struct {
	struct AM_MSG_QUEUE_struct *next;
	int size;
	AM_MSG msg[1];
} AM_MSG_QUEUE;
#define MMIN(a,b) ((a)<(b)?(a):(b))

static int am_do_store(unsigned long type,vnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *arg,handler_mem_t endfunc,void *e_arg);

int wait();
static void kill_all_children()
{
	int i;
	if(am_my_cluster_id!=-1) exit(1);
	for(i=0;i<am_clusters;i++) kill(chpids[i],SIGKILL);
	
	while(wait(NULL)!=-1); /* wait for all children */
	exit(1);
}

void am_disable()
{
#ifdef AM_STAT
	print_am_stat();
#endif
	if(am_clusters>1)
		am_pool[am_my_cluster_id].dead=1;
}

static void wait_for_children()
{
	int children_dead=0;
	int i,j;
	struct sigaction act;

/* Nobbi: was
#ifdef SUNOS5
	sigemptyset(&act.sa_mask);
#else
	act.sa_mask=0;
#endif
*/
	sigemptyset(&act.sa_mask);

	act.sa_flags=0;
	act.sa_handler=(void (*)(int))kill_all_children;
	assert(sigaction(SIGINT,&act,NULL)==0);
	assert(sigaction(SIGHUP,&act,NULL)==0);
	assert(sigaction(SIGQUIT,&act,NULL)==0);
	assert(sigaction(SIGTERM,&act,NULL)==0);

	act.sa_handler=SIG_IGN;

	while(1) {
		if(wait(NULL)!=-1) {
			children_dead++;
			for(j=i=0;i<am_clusters;i++) j+=am_pool[i].dead;
			if(j<children_dead) {
				fprintf(stderr,"One of the processes crashed, aborting now.\n");
				kill_all_children();
			}
		} else exit(0);
	}
}

static void start_debug(char *argv[])
{
	if(getenv("DEBUG_AM") || getenv("DEBUG_PSATHER")) {
		char com[200];
		if(getenv("DEBUG_AM_COMMAND"))
			sprintf(com,getenv("DEBUG_AM_COMMAND"),argv[0],getpid());
		else
			sprintf(com,"xterm -fn 7x13 -T \"%d - %s\" -e gdb %s %d&\n",am_my_cluster_id,argv[0],argv[0],(int)getpid());
		system(com);
		while(debug_am);
	}
}

void am_enable(int no_of_clusters,int argc,char *argv[])
{
	int i;
	struct sigaction act;

#ifdef AM_STAT
	stat_dir=argv[0];
#endif
	am_clusters=no_of_clusters;
	if(am_clusters==1) {
		am_my_cluster_id=0;
		start_debug(argv);
		T_INIT_MACHINE;
		T_INIT_CLUSTER;
		AM_INIT_THREAD;
		return;
	}
	am_my_cluster_id= -1; /* the process which called enable will
	                       * never return. It waits for the death
			       * of one of its children and stops the 
			       * whole cluster if this happens
			       */
	if(no_of_clusters>AM_MAX_NODES) {
		fprintf(stderr,"this implementation supports a maximum of %d clusters, you requested %d\n",AM_MAX_NODES,no_of_clusters);
		exit(1);
	}

/* Nobbi: was:
#ifdef SUNOS5
	sigemptyset(&act.sa_mask);
#else
	act.sa_mask=0;
#endif
*/
	sigemptyset(&act.sa_mask);

	act.sa_flags=0;
	act.sa_handler=(void (*)(int))kill_all_children;
	assert(sigaction(SIGINT,&act,NULL)==0);
	assert(sigaction(SIGHUP,&act,NULL)==0);
	assert(sigaction(SIGQUIT,&act,NULL)==0);
	assert(sigaction(SIGTERM,&act,NULL)==0);

	/* 
	 * initialize shared memory
	 * If it already exists, it will be first deleted and then created again
	 */
#ifndef __CYGWIN32__
	sid=shmget(IPC_PRIVATE,sizeof(AM_NODE_MSG_POOL)*AM_MAX_NODES,IPC_CREAT|IPC_EXCL|0700);
	if(sid<0) { 
		perror("shmget");
		exit(1);
	}
	am_pool=(AM_MSG_POOL)shmat(sid,NULL,0);
	/*
	 * we remove the shared memory immediatly from the system.
	 * as long as the processes created below are not killed,
	 * it will survive.
	 */
	 if(shmctl(sid,IPC_RMID,NULL)!=0) {
		 perror("shmctl");
		 exit(1);
	 }

#else
       { caddr_t addr = NULL ;
         caddr_t map = MAP_FAILED ;
         int file = open("/dev/zero", O_RDWR) ;
         if (file == -1) {
           perror("Unable to open /dev/zero for virtual map1");
           exit(1);
         }
         addr = malloc(sizeof(AM_NODE_MSG_POOL)*AM_MAX_NODES) ;
         map = mmap(addr,
                    sizeof(AM_NODE_MSG_POOL)*AM_MAX_NODES,
                    PROT_READ|PROT_WRITE,
                    MAP_SHARED,
                    file,
                    (off_t)0) ;
         if (map == MAP_FAILED) {
           perror("mmap of /dev/zero for page descriptor");
           exit(1);
         }
         sid = (int)map ;
         close(file) ;
       }
#endif

	/*
	 * initialize shared memory
	 */
	memset(am_pool,0,sizeof(AM_NODE_MSG_POOL)*AM_MAX_NODES);
	T_INIT_MACHINE;

	for(i=0;i<no_of_clusters;i++) {
		switch((chpids[i]=fork())) {
		case -1:
			perror("fork");
			kill_all_children();
		case 0: 
			am_my_cluster_id=i;
			i=no_of_clusters+1;
			break;
		default: 
			break;
		}
	}

	/*
	 * All clusters are running
	 */
	if(am_my_cluster_id!= -1) {
		act.sa_handler=SIG_DFL;
		assert(sigaction(SIGINT,&act,NULL)==0);
		assert(sigaction(SIGHUP,&act,NULL)==0);
		assert(sigaction(SIGQUIT,&act,NULL)==0);
		assert(sigaction(SIGTERM,&act,NULL)==0);
	} else {
		wait_for_children();
		return;
	}
	start_debug(argv);

	am_lock=AM_LOCK_CREATE;
	for(i=0;i<no_of_clusters;i++) am_write_lock[i]=AM_WRITE_CREATE;
	T_INIT_CLUSTER;
	AM_INIT_THREAD;
	return;
}

#ifdef USE_AM_TO_START_THREAD
static AM_START_THREAD_DCL(AM_MSG *msg)
{
	AM_INIT_THREAD;
	switch(msg->f.flags&AM_ARG_MASK) {
	case AM_ARG0: (*msg->i.handler)((vnn_t)msg->i.next);break;
	case AM_ARG1: (*msg->i.handler)((vnn_t)msg->i.next,msg->i.a0);break;
	case AM_ARG2: (*msg->i.handler)((vnn_t)msg->i.next,msg->i.a0,msg->i.a1);break;
	case AM_ARG3: (*msg->i.handler)((vnn_t)msg->i.next,msg->i.a0,msg->i.a1,msg->i.a2);break;
	case AM_ARG4: (*msg->i.handler)((vnn_t)msg->i.next,msg->i.a0,msg->i.a1,msg->i.a2,msg->i.a3);break;
	case AM_ARG_DBL: (*msg->d.handler)((vnn_t)msg->i.next,msg->d.a0,msg->d.a1,msg->d.a2);break;
	default: printf("msg->f.flags=%d\n",msg->f.flags);assert(0);
	}
	free(msg);
	AM_END_THREAD;
}
#endif

static void am_process(vnn_t from,AM_WRITE_BUF *other,int bit)
{
	AM_MSG *msg= &other->msg[bit];

	msg->f.flags^=AM_FIRST_PIECE; /* mark this messages as "work in progress" */
#ifdef USE_AM_TO_START_THREAD
	if(msg->f.flags&AM_NEW_THREAD) {
		AM_MSG *n;
		AM_UNLOCK;
		n=(AM_MSG *)malloc(sizeof(AM_MSG));
		memcpy(n,msg,sizeof(AM_MSG));
		n->i.next=from; 	    /* as most thread packages support
		                             * only one arg, we pass from
		 			     * through a free field in msg */
		AM_START_THREAD(n);
		AM_LOCK;
		other->r ^= 1<<bit;
	} else
#endif
	    if(AM_IS_ARGS(msg->f.flags)) {
		AM_UNLOCK;
		switch(msg->f.flags&AM_ARG_MASK) {
		case AM_ARG0: (*msg->i.handler)(from);break;
		case AM_ARG1: (*msg->i.handler)(from,msg->i.a0);break;
		case AM_ARG2: (*msg->i.handler)(from,msg->i.a0,msg->i.a1);break;
		case AM_ARG3: (*msg->i.handler)(from,msg->i.a0,msg->i.a1,msg->i.a2);break;
		case AM_ARG4: (*msg->i.handler)(from,msg->i.a0,msg->i.a1,msg->i.a2,msg->i.a3);break;
		case AM_ARG_DBL: (*msg->d.handler)(from,msg->d.a0,msg->d.a1,msg->d.a2);break;
		default: assert(0);
		}
		AM_LOCK;
		other->r ^= 1<<bit;
	} else if(msg->f.flags&AM_GET) {
		AM_UNLOCK;
		am_do_store(AM_REPLY_MASK,from,msg->g.rva,msg->g.lva,msg->g.bytes,msg->g.handler,msg->g.arg,NULL,NULL);
		AM_LOCK;
		other->r ^= 1<<bit;
	} else { /* Store Mesg */
		AM_MSG *next;
		char *buf;
		unsigned long bits=1<<(long)bit;
		int i,c;
		buf=(char *)msg->m.rva;
		memcpy(buf,msg->m.c,MMIN(msg->m.bytes,16));
		if(!(msg->m.flags&AM_LAST_PIECE)) {
			c=msg->m.bytes-16;
			i=16;
			next= &other->msg[(int)(msg->m.next)];
			bits|=1<<(long)msg->m.next;
			while(1) {
				memcpy(buf+i,next->c.c,MMIN(c,30));
				c-=30;i+=30;
				if(!(next->c.flags&AM_LAST_PIECE)) {
					bits|=1<<(long)next->m.next;
					next= &other->msg[(int)(next->c.next)];
				} else break;
			}
			assert(c<=0);
		}
		AM_UNLOCK;
		(*msg->m.handler)(from,msg->m.rva,msg->m.bytes,msg->m.arg);
		AM_LOCK;
		other->r ^= bits;
	}
}

void am_work_on_messages(unsigned long mask)
{
	int cl;
	unsigned bits;
	AM_WRITE_BUF *other;
	short bit;
	AM_MSG *msg;
#ifdef AM_THREADS
	int sigs;
#endif

	other= am_pool[am_my_cluster_id].node;
#define N(i) (other[i].r^other[i].w)
	if(!(mask&(N(0)|N(1)|N(2)|N(3)|N(4)|N(5)|N(6)|N(7)|N(8)|N(9)|N(10)|N(11)|N(12)|N(13)|N(14)|N(15)|N(16)|N(17)|N(18)|N(19)|N(20)|N(21)|N(22)|N(23)|N(24)|N(25)|N(26)|N(27)|N(28)|N(29)|N(30)|N(31)))) return;

#ifdef AM_THREADS
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
	AM_LOCK;

	for(cl=0;cl<am_clusters;cl++,other++) {
		bits=(other->r ^ other->w) & mask;
		while(bits) {
			bit=LOWEST_BIT(bits);
			assert(bit<AM_MSG_PER_NODE);
			msg= &other->msg[bit];
			if(msg->f.flags&AM_FIRST_PIECE) {
				am_process(cl,other,bit);
				bits=(other->r ^ other->w) & mask;
			} else bits^=1<<bit;
		}
	}
	AM_UNLOCK;
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
}


void am_poll()
{
	if(am_clusters>1) am_work_on_messages(AM_REQUEST_MASK|AM_REPLY_MASK);
}


int am_procs()
{
	return am_clusters;
}

int am_my_cluster_size()
{
  /* number of processors for a calling cluster */
  return am_cluster_size;
}
vnn_t am_my_proc()
{
	return am_my_cluster_id;
}

int am_max_size()
{
	return AM_MAX_MSG_SIZE;
}

#define AT_LEAST_BITS(bits,n) (((n)==1)?(bits):at_least_bits(bits,n))
static int at_least_bits(unsigned long bits,short n)
{
	while(n--) {
		if(!bits) return 0;
		bits&=bits-1;
	}
	return 1;
}

static void am_find_msgslot(unsigned long mask,vnn_t dest,int size,long *slots)
{
	int i;
	unsigned long bits;
	AM_WRITE_BUF *db;

	db= &am_pool[dest].node[am_my_cluster_id];

	while(1) { /* wait for enough message slots to send message */
		bits= mask & ~(db->r ^ db->w);
		if(!AT_LEAST_BITS(bits,size)) {
			AM_WRITE_UNLOCK(dest);
			am_work_on_messages(AM_REPLY_MASK|AM_REQUEST_MASK);
			AM_WRITE_LOCK(dest);
		} else break;
	}

	for(i=0;i<size;i++) {
		slots[i]=LOWEST_BIT(bits);
		bits^= 1<<slots[i];
	}
}

int am_get(vnn_t source,void *rva, void *lva,int nbytes,handler_mem_t handler,void *arg)
{
	AM_MSG *msg;
	long slot;
	STAT(get_stat[source][nbytes]);
	if(nbytes>AM_MAX_MSG_SIZE) {
		fprintf(stderr,"max msg size: %ld, requested msg size: %d\n",(long)AM_MAX_MSG_SIZE,nbytes);
		return -1;
	}
	if(source==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		memcpy(lva,rva,nbytes);
		(*handler)(source,lva,nbytes,arg);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	}

	AM_WRITE_LOCK(source);
	am_find_msgslot(AM_REQUEST_MASK,source,1,&slot);
	msg= am_pool[source].node[am_my_cluster_id].msg;
	msg[slot].g.flags=AM_FIRST_PIECE|AM_LAST_PIECE|AM_GET;
	msg[slot].g.bytes=nbytes;
	msg[slot].g.rva=rva;
	msg[slot].g.handler=handler;
	msg[slot].g.arg=arg;
	msg[slot].g.lva=lva;
	am_pool[source].node[am_my_cluster_id].w ^= 1<<slot;
	AM_WRITE_UNLOCK(source);
	SIGNAL_PROC(source);
	return 0;
}

static int am_do_store(unsigned long type,vnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *arg,handler_mem_t endfunc,void *e_arg)
{
	AM_MSG *msg;
	long slot[AM_MSG_PER_NODE/2];
	short slots;
	unsigned long bits=0;
	int i;
#ifdef AM_THREADS
	int sigs;
#endif
	if(nbytes>AM_MAX_MSG_SIZE) {
		fprintf(stderr,"max msg size: %ld, requested msg size: %d\n",(long)AM_MAX_MSG_SIZE,nbytes);
		return -1;
	}
	slots=1+((nbytes-16)+29)/30;

#ifdef AM_THREADS
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
	AM_WRITE_LOCK(dest);
	am_find_msgslot(type,dest,slots,slot);
	msg= am_pool[dest].node[am_my_cluster_id].msg;
	msg[slot[0]].m.flags=AM_FIRST_PIECE;
	msg[slot[0]].m.bytes=nbytes;
	msg[slot[0]].m.rva=rva;
	msg[slot[0]].m.handler=handler;
	msg[slot[0]].m.arg=arg;
	memcpy(msg[slot[0]].m.c,lva,MMIN(16,nbytes));
	bits=1<<(long)slot[0];
	if(slots==1) {
		msg[slot[0]].m.flags|=AM_LAST_PIECE;
	} else {
		nbytes-=16;
		for(i=1;i<slots;i++) {
			bits|=1<<(long)slot[i];
			msg[slot[i-1]].m.next=slot[i];
			msg[slot[i]].m.flags=(i==slots-1)?AM_LAST_PIECE:0;
			memcpy(msg[slot[i]].c.c,((char *)lva)+i*30-14,MMIN(30,nbytes));
			nbytes-=30;
		}
		assert(nbytes<=0);
	}
	am_pool[dest].node[am_my_cluster_id].w ^= bits;
	AM_WRITE_UNLOCK(dest);
	if(endfunc!=NULL) (*endfunc)(dest,lva,nbytes,e_arg);
	SIGNAL_PROC(dest);
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
	return 0;
}

int am_store(vnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *arg)
{
	STAT(sto_stat[dest][nbytes]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		memcpy(rva,lva,nbytes);
		(*handler)(am_my_cluster_id,rva,nbytes,arg);
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_do_store(AM_REQUEST_MASK,dest,lva,rva,nbytes,handler,arg,NULL,NULL);
}

int am_store_async(vnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *arg,handler_mem_t endfunc,void *e_arg)
{
	STAT(sto_async_stat[dest][nbytes]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		memcpy(rva,lva,nbytes);
		(*handler)(am_my_cluster_id,rva,nbytes,arg);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_do_store(AM_REQUEST_MASK,dest,lva,rva,nbytes,handler,arg,endfunc,e_arg);
}

int am_send_M(unsigned long type,vnn_t dest,handler_0_t handler,int args,long a0,long a1,long a2,long a3)
{
	AM_MSG *msg;
	long bit;
#ifdef AM_THREADS
	int sigs;
#endif

#ifdef AM_THREADS
	if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
	AM_WRITE_LOCK(dest);

	am_find_msgslot(type,dest,1,&bit);
	msg= &am_pool[dest].node[am_my_cluster_id].msg[bit];
	msg->f.flags=args|AM_COMPLETE;
	msg->f.next=0;
	msg->i.handler=(void (*)())handler;
	if(AM_ARGS(args)>=1) { 
		msg->i.a0=a0;
		if(AM_ARGS(args)>=2) { 
			msg->i.a1=a1;
			if(AM_ARGS(args)>=3) { 
				msg->i.a2=a2;
				if(AM_ARGS(args)>=4) msg->i.a3=a3;
			}
		}
	}
	am_pool[dest].node[am_my_cluster_id].w ^= 1<<bit;

	AM_WRITE_UNLOCK(dest);
	SIGNAL_PROC(dest);
#ifdef AM_THREADS
	if(sigs) THR_ENABLE_SIGNAL;
#endif
	return 0;
}

int am_request_0(vnn_t dest,handler_0_t handler) 
{ 
	STAT(req_stat[dest][0]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REQUEST_MASK,dest,handler,AM_ARG0,0,0,0,0); 
}
int am_request_1(vnn_t dest,handler_1_t handler,long arg0)
{ 
	STAT(req_stat[dest][1]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG1,arg0,0,0,0);
}
int am_request_2(vnn_t dest,handler_2_t handler,long arg0,long arg1)
{ 
	STAT(req_stat[dest][2]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG2,arg0,arg1,0,0);
}
int am_request_3(vnn_t dest,handler_3_t handler,long arg0,long arg1,long arg2)
{ 
	STAT(req_stat[dest][3]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1,arg2);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG3,arg0,arg1,arg2,0);
}
int am_request_4(vnn_t dest,handler_4_t handler,long arg0,long arg1,long arg2,long arg3)
{ 
	STAT(req_stat[dest][4]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1,arg2,arg3);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG4,arg0,arg1,arg2,arg3);
}
int am_request_df(vnn_t dest,handler_df_t handler,long arg0,long arg1,double arg2)
{
	union x {
		struct { long a0,a1; } l;
		double d;
	} d;
	STAT(req_stat[dest][5]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1,arg2);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else {
		d.d=arg2;
		return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG_DBL,arg0,arg1,d.l.a0,d.l.a1); 
	}
}

int am_reply_0(vnn_t dest,handler_0_t handler) 
{ 
	STAT(rep_stat[dest][0]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REPLY_MASK,dest,handler,AM_ARG0,0,0,0,0); 
}
int am_reply_1(vnn_t dest,handler_1_t handler,long arg0)
{ 
	STAT(rep_stat[dest][1]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REPLY_MASK,dest,(handler_0_t)handler,AM_ARG1,arg0,0,0,0);
}
int am_reply_2(vnn_t dest,handler_2_t handler,long arg0,long arg1)
{ 
	STAT(rep_stat[dest][2]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REPLY_MASK,dest,(handler_0_t)handler,AM_ARG2,arg0,arg1,0,0);
}
int am_reply_3(vnn_t dest,handler_3_t handler,long arg0,long arg1,long arg2)
{ 
	STAT(rep_stat[dest][3]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1,arg2);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REPLY_MASK,dest,(handler_0_t)handler,AM_ARG3,arg0,arg1,arg2,0);
}
int am_reply_4(vnn_t dest,handler_4_t handler,long arg0,long arg1,long arg2,long arg3)
{ 
	STAT(rep_stat[dest][4]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1,arg2,arg3);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else return am_send_M(AM_REPLY_MASK,dest,(handler_0_t)handler,AM_ARG4,arg0,arg1,arg2,arg3);
}
int am_reply_df(vnn_t dest,handler_df_t handler,long arg0,long arg1,double arg2)
{
	union x {
		struct { long a0,a1; } l;
		double d;
	} d;
	STAT(rep_stat[dest][5]);
	if(dest==am_my_cluster_id) {
#ifdef AM_THREADS
		int sigs;
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
#endif
		(*handler)(dest,arg0,arg1,arg2);
#ifdef AM_THREADS
		if(sigs) THR_ENABLE_SIGNAL;
#endif
		return 0;
	} else {
		d.d=arg2;
		return am_send_M(AM_REPLY_MASK,dest,(handler_0_t)handler,AM_ARG_DBL,arg0,arg1,d.l.a0,d.l.a1); 
	}
}

#ifdef USE_AM_TO_START_THREAD
int thr_create_0(vnn_t dest,handler_0_t handler) 
{ 
	STAT(thr_stat[dest][0]);
	if(dest==am_my_cluster_id) {
		AM_MSG *n;
		int sigs;
		n=malloc(sizeof(AM_MSG));
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
		n->i.handler=handler;
		n->f.flags=AM_ARG0;
		n->i.next=dest;
		AM_START_THREAD(n);
		if(sigs) THR_ENABLE_SIGNAL;
		return 0;
	}
	return am_send_M(AM_REQUEST_MASK,dest,handler,AM_ARG0|AM_NEW_THREAD,0,0,0,0); 
}
int thr_create_1(vnn_t dest,handler_1_t handler,long arg0)
{ 
	STAT(thr_stat[dest][1]);
	if(dest==am_my_cluster_id) {
		AM_MSG *n;
		int sigs;
		n=malloc(sizeof(AM_MSG));
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
		n->i.handler=handler;
		n->f.flags=AM_ARG1;
		n->i.a0=arg0;
		n->i.next=dest;
		AM_START_THREAD(n);
		if(sigs) THR_ENABLE_SIGNAL;
		return 0;
	}
	return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG1|AM_NEW_THREAD,arg0,0,0,0); 
}
int thr_create_2(vnn_t dest,handler_2_t handler,long arg0,long arg1)
{ 
	STAT(thr_stat[dest][2]);
	if(dest==am_my_cluster_id) {
		AM_MSG *n;
		int sigs;
		n=malloc(sizeof(AM_MSG));
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
		n->i.handler=handler;
		n->f.flags=AM_ARG2;
		n->i.a0=arg0;
		n->i.a1=arg1;
		n->i.next=dest;
		AM_START_THREAD(n);
		if(sigs) THR_ENABLE_SIGNAL;
		return 0;
	}
	return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG2|AM_NEW_THREAD,arg0,arg1,0,0); 
}
int thr_create_3(vnn_t dest,handler_3_t handler,long arg0,long arg1,long arg2)
{ 
	STAT(thr_stat[dest][3]);
	if(dest==am_my_cluster_id) {
		AM_MSG *n;
		int sigs;
		n=malloc(sizeof(AM_MSG));
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
		n->i.handler=handler;
		n->f.flags=AM_ARG3;
		n->i.a0=arg0;
		n->i.a1=arg1;
		n->i.a2=arg2;
		n->i.next=dest;
		AM_START_THREAD(n);
		if(sigs) THR_ENABLE_SIGNAL;
		return 0;
	}
	return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG3|AM_NEW_THREAD,arg0,arg1,arg2,0); 
}
int thr_create_4(vnn_t dest,handler_4_t handler,long arg0,long arg1,long arg2,long arg3)
{ 
	STAT(thr_stat[dest][4]);
	if(dest==am_my_cluster_id) {
		AM_MSG *n;
		int sigs;
		n=malloc(sizeof(AM_MSG));
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
		n->i.handler=handler;
		n->f.flags=AM_ARG4;
		n->i.a0=arg0;
		n->i.a1=arg1;
		n->i.a2=arg2;
		n->i.a3=arg3;
		n->i.next=dest;
		AM_START_THREAD(n);
		if(sigs) THR_ENABLE_SIGNAL;
		return 0;
	}
	return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG4|AM_NEW_THREAD,arg0,arg1,arg2,arg3); 
}
int thr_create_df(vnn_t dest,handler_df_t handler,long arg0,long arg1,double arg2)
{
	union x {
		struct { long a0,a1; } l;
		double d;
	} d;
	STAT(thr_stat[dest][5]);
	if(dest==am_my_cluster_id) {
		AM_MSG *n;
		int sigs;
		n=malloc(sizeof(AM_MSG));
		if((sigs=THR_SIGNAL_ENABLED)) THR_BLOCK_SIGNAL;
		n->d.handler=handler;
		n->f.flags=AM_ARG_DBL;
		n->d.a0=arg0;
		n->d.a1=arg1;
		n->d.a2=arg2;
		n->d.next=dest;
		AM_START_THREAD(n);
		if(sigs) THR_ENABLE_SIGNAL;
		return 0;
	}
	d.d=arg2;
	return am_send_M(AM_REQUEST_MASK,dest,(handler_0_t)handler,AM_ARG_DBL|AM_NEW_THREAD,arg0,arg1,d.l.a0,d.l.a1); 
}
#endif

#endif /* TCP */

#ifdef AM_STAT
long req_stat[AM_MAX_NODES][6];
long rep_stat[AM_MAX_NODES][6];
long thr_stat[AM_MAX_NODES][6];
long sto_stat[AM_MAX_NODES][AM_MAX_S+1];
long sto_async_stat[AM_MAX_NODES][AM_MAX_S+1];
long get_stat[AM_MAX_NODES][AM_MAX_S+1];
char *stat_dir;

void print_am_stat()
{
	FILE *f;
	char buf[256];
	extern int mkdir(char *,int);
	int i,j;
	if(strlen(stat_dir)>240) {
		fprintf(stderr,"cannot open statistic file %s.st/CL%03d.am (filename too long)\n",stat_dir,am_my_cluster_id);
		return;
	}
	strcpy(buf,stat_dir);strcat(buf,".st");mkdir(buf,0755);
	sprintf(buf,"%s.st/CL%03d.am",stat_dir,am_my_cluster_id);
	f=fopen(buf,"w");
	if(f==NULL) {
		fprintf(stderr,"cannot open file %s for writing\n",buf);
		return;
	}
	for(j=0;j<am_clusters;j++) {
		for(i=0;i<5;i++) {
			if(req_stat[j][i]) fprintf(f,"am_request_%d(%d):	%ld\n",i,j,req_stat[j][i]);
		}
		if(req_stat[j][5]) fprintf(f,"am_request_df(%d):	%ld\n",j,req_stat[j][5]);
	}
	for(j=0;j<am_clusters;j++) {
		for(i=0;i<5;i++) {
			if(rep_stat[j][i]) fprintf(f,"am_reply_%d(%d):	%ld\n",i,j,rep_stat[j][i]);
		}
		if(rep_stat[j][5]) fprintf(f,"am_reply_df(%d):	%ld\n",j,rep_stat[j][5]);
	}
	for(j=0;j<am_clusters;j++) {
		for(i=0;i<5;i++) {
			if(thr_stat[j][i]) fprintf(f,"thr_create_%d(%d):	%ld\n",i,j,thr_stat[j][i]);
		}
		if(thr_stat[j][5]) fprintf(f,"thr_create_df(%d):	%ld\n",j,thr_stat[j][5]);
	}
	for(j=0;j<am_clusters;j++) {
		for(i=0;i<=AM_MAX_S;i++) {
			if(sto_stat[j][i]) fprintf(f,"am_store(%d,*,%d):	%ld\n",j,i,sto_stat[j][i]);
		}
	}
	for(j=0;j<am_clusters;j++) {
		for(i=0;i<=AM_MAX_S;i++) {
			if(sto_async_stat[j][i]) fprintf(f,"am_store_async(%d,*,%d):	%ld\n",j,i,sto_async_stat[j][i]);
		}
	}
	for(j=0;j<am_clusters;j++) {
		for(i=0;i<=AM_MAX_S;i++) {
			if(get_stat[j][i]) fprintf(f,"am_get(%d,*,%d):	%ld\n",j,i,get_stat[j][i]);
		}
	}
	fclose(f);
}

#endif

