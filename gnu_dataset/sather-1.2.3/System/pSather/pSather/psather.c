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
 * General pSather runtime stuff
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include <stddef.h>
#include <unistd.h>
#include <signal.h>

#include "memory.h"
#include "debug.h"
#include "locks.h"
#include "trace.h"
#include "pSather.h"
#include "stat.h"
#include "attach.h"

extern void p_init_locks(void);
extern void init_thread(void);
char *bin_prog_name=NULL;
int  start_sec;  /* int is more portable */
extern void deamon_thread();

#if defined(_REENTRANT) && defined(__GNUC__) && defined(__sparc__)
/* use spinlocks */
spinlock_t vlocks[VLOCKS];
#else
BR_lock_t vlocks[VLOCKS];
#endif

#ifdef DEFAULT_CLUSTERS
int clusters=DEFAULT_CLUSTERS;
#else
int clusters=4;
#endif

/* used if backtracing in pSather in on */
CREATE_FF

/*
 * When the following function is called from within the debugger,
 * it will start a new thread that calls "debug_psather()" once
 * every second. This way one is sure to get a running thread 
 * while debugging pSather
 */
void debug_psather_request(vnn_t from)
{
	extern void get_thread();
	while(1) {
		sleep(1);
		get_thread();
	}
}

void get_thread() {}
void stop_thread() { BR_lock_t l=BR_LOCK_CREATE();while(1) BR_LOCK(l); }
void debug_psather_thread()
{
	/*
	int i;
	for(i=0;i<CLUSTERS;i++)
		BR_FORK_0(i,debug_psather_request);
	*/
}

/* The following seems not to be needed 
static void end_psather_done(vnn_t from,BR_word_t arg,BR_word_t lck)
{
	BR_LOCK((BR_lock_t)lck);
	(*(long *)arg)--;
	BR_UNLOCK((BR_lock_t)lck);
}

static void end_psather(vnn_t from,BR_word_t arg,BR_word_t lck)
{
	BR_REQUEST_2(from,end_psather_done,arg,lck);
	stat_print(bin_prog_name);
	psather_stop();
	TRACE("cluster died.");
	exit(0);
}
*/

void set_startsec(vnn_t from,BR_word_t s)
{
	start_sec=s;
}

void correct_startsec()
{
	int i;

	for(i=1;i<CLUSTERS;i++)
		BR_REQUEST_1(i,set_startsec,(BR_word_t)start_sec);
}

/* makes sure that the lock manager has the correct local 
   thread memory */
static void init_poll_manager(vnn_t from)
{
	extern void init_thread_memory();
	init_thread_memory();
	if(HERE==1) BR_REPLY_0(0,init_poll_manager);
}

void init_clusters(vnn_t from,long sema)
{
	extern void init_thread_count();
	extern void init_import_export();

	init_thread_count();
	init_import_export(); /* initialize import/export stuff */
	init_parloop_locks();

	clusters=BR_CLUSTERS();
	init_psather_end_detection();
	init_thread();

	TRACE("cluster started.");
	p_init_locks();
	/* initilaize registartion database */
	REG(NULL,NULL);UNREG;
	init_parloops();
	printf("SEMA_SIGNAL %p\n",(void *)sema);
	deamon_thread();
	BR_REQUEST_1(0,BR_signal_handler,sema);
}

void psather_start(int argc,char *argv[])
{
	volatile int j=1;
	BR_sema_t sem;
		/*extern int gettimeofday(struct timeval *,void *);*/
	struct timeval now;
	extern void gdb_signal_handler(int);
	extern void remote_gdb(int);
	extern void init_thread_count();
	extern void init_import_export();
        extern void init_thread_memory();

	extern int sscanf(const char *,const char *,...);
	/*
	do {
		gettimeofday(&now,NULL);
	} while(now.tv_usec>10000);
	*/
	gettimeofday(&now,NULL);
	start_sec=now.tv_sec;

        signal(SIGQUIT,remote_gdb);
        signal(SIGILL,gdb_signal_handler);
        signal(SIGEMT,gdb_signal_handler);
        signal(SIGFPE,gdb_signal_handler);
        signal(SIGBUS,gdb_signal_handler);
        signal(SIGSEGV,gdb_signal_handler);
        signal(SIGSYS,gdb_signal_handler);

	init_psather_prio();

	bin_prog_name=argv[0];

#if defined(PSATHER1)
	/*
        ** This happens for the solaris/smp platform, which should
	** be able to emulate multiple clusters on a single address
	** space but because of current runtime cannot.  It is important
	** to NOT ask Brahma how many clusters it wants, because the
	** runtime only supports one.  The do_test script sets the
	** CLUSTERS env var even if the distributed flag is not set
	** in the platform's CONFIG file.
	*/
	clusters=1;
	BR_init(1,argc,argv);
#else
	/* Clusters is set by the BR_init call - DPS */

	BR_init(0,argc,argv);
	clusters=BR_CLUSTERS();
#endif

	/* init_clusters(BR_HERE(),sem); */
        /* This is what used to be in init_clusters */
	
	/* Create synchronization objects, stc. */
        init_thread_count();
        init_import_export(); 
        init_parloop_locks();


	sem=BR_SEMA_CREATE(0);

#if defined(_REENTRANT) && defined(__GNUC__) && defined(__sparc__)
	/* use spinlocks */
	for(j=0;j<VLOCKS;j++) BR_SPINLOCK_INIT(vlocks[j]); /* initialize vlocks */
#else
	for(j=0;j<VLOCKS;j++) vlocks[j]=BR_LOCK_CREATE(); /* initialize vlocks */
#endif

#ifdef PSATHER_STAT
        /* This has to follow BR_init because it uses messages. */
	init_stat();
#endif

	/* Ready to start Brahma */
	BR_start();


        BR_BARRIER();
	
        p_init_locks();

        clusters=BR_CLUSTERS();
        init_psather_end_detection();
        init_thread();

        TRACE2("cluster started at %d.%06d",now.tv_sec,now.tv_usec);
        /* initilaize registartion database */
        REG(NULL,NULL);UNREG;
        init_parloops();

	/* This seems questionable! DPS */
        deamon_thread();

	BR_BARRIER();
	correct_startsec();

/*
** init_poll_manager calls init_thread_memory, and then bogusly
** waits for cluster 1 to respond.  Here, we just execute it once
** everywhere. DPS
*/
        init_thread_memory();
	BR_BARRIER();
	if (BR_HERE()>0) { /* All threads except what's on cluster zero */
		BR_lock_t t;
		debug_psather_thread(); /* only needed for debugging */
		t=BR_LOCK_CREATE();
		deamon_thread();
		while(1) BR_LOCK(t);
	}

}

int psather_stop(void)
{
	/* Brahma performs termination */
	if(BR_HERE()==0){
	  BR_exit();
	}
	return 0;
}

int read_tag(FOB obj)
{
	short i;
	READTAG(i,obj);
	return i;
}
#undef abort
int psather_abort(void) { abort(); return 0; }

void r_lck_unlock_mem(BR_cluster_t cl, caddr_t c, size_t s, caddr_t sem)
{ BR_UNLOCK((BR_lock_t)sem); }
void r_ta_sema_signal_mem(BR_cluster_t cl, caddr_t c, size_t s, BR_word_t sem)
{ TA_SEMA_SIGNAL((ta_sema_t)sem); }
void r_ta_sema_signal(BR_cluster_t cl, caddr_t sem)
{ TA_SEMA_SIGNAL((ta_sema_t)sem); }

int p_fwrite(FOB p,int size,int elems,FILE *file)
{
	void *x;
	int i;
	if(!NEAR(p)) {
		TA_SEMAPHORE(a);
		MALLOC(x,size*elems);
		BR_GET(WHERE(p),SENDFOBHOME(p),x,size*elems,r_ta_sema_signal_mem,(BR_word_t)a);
		TA_SEMA_WAIT(a);
	} else x=p;
	i=fwrite(x,size,elems,file);
	if(!NEAR(p)) FREE(x);
	return i;
}
long p_sysid(FOB r)
{
	long x,y;

	SAFE_POS
#ifdef PSATHER_CHK
		if(F_TAG(r)<0) RFATAL("Called SYSID::id on value type");
#endif
		x=WHERE(r);
#ifdef DETERMINISTIC
		F_VA_RATTR_NA(y,OB,r,header.id);
#else
		y=((long)POINTER(r))>>3;
#endif
	RESTORE_POS
	return (x<<CLUSTER_SHIFT)+y;
}

	
static void request_sysdestroy(vnn_t from,FOB x)
{
#ifdef SYSDESTROY /* this cannot be tested without the Sather Runtime files */
	extern rt_fatal2();
	SYSDESTROY(x);
#endif
}
void p_sysdestroy(FOB x)
{
	BR_REQUEST_1(WHERE(x),(BR_handler_1_t)request_sysdestroy,(long)SENDFOBHOME(x));
}
int pmin(int a, int b)
{
	return a<b?a:b;
}

int p_rfatalv(char *s)
{
	if((BR_GET_THREAD_LOCAL())==NULL) fprintf(stderr,"%s",s);
        else  {
		fprintf(stderr,"%s:%ld:%s\n",((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->file,(long)((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->line,s);
#ifdef PRINT_PO
		{
			extern void PT(),PF();
			extern int print_gdb;
			int pg;
			pg=print_gdb;
			if(getenv("START_GDB")==NULL) print_gdb=0;
			printf("------------------------------------\ncurrent function frame:\n");
			PF(((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->pFF);
			printf("------------------------------------\nbacktrace:\n");
			PT(((LOCAL_MEM)(BR_GET_THREAD_LOCAL()))->pFF);
			print_gdb=pg;
			printf("------------------------------------\n");
			fflush(stdout);
		}
#endif
	}
	start_gdb();
	PSATHER_ABORT;
	return 0;
}

int p_rfatalvfl(char *s,char *f,int l)
{
	DEBUG0(s);
	fprintf(stderr,"%s:%d:%s\n",f,l,s);
	fflush(stderr);
	start_gdb();
	PSATHER_ABORT;
	return 0;
}

void break_psather() { SYS_DEFER; } 




/* Simple PAR decerement handler */
void par_decrement_handler_1(BR_cluster_t ignore, BR_word_t arg){
  SIMPLE_PAR_ATTACH par = (SIMPLE_PAR_ATTACH)arg;
  
  BR_REAL_SPINLOCK_LOCK(par->slck); 
  par->counter--; 
  if(par->counter==0) {
    BR_REAL_SPINLOCK_UNLOCK(par->slck); 
    BR_UNLOCK(par->end_mutex); 
  } else { 
    BR_REAL_SPINLOCK_UNLOCK(par->slck); 
  }   
}

/* Simple AT expression finished handler */
void at_expr_finished_handler_2(BR_cluster_t ignore, BR_word_t at_attach_p,
				BR_word_t res) {
  AT_ATTACH at_attach = (AT_ATTACH) at_attach_p;
  at_attach->result = res;
  BR_SPINLOCK_UNLOCK(at_attach->end_slck);
}



