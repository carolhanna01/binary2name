/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
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
** This is the TCP win32 implementation of Brahma 
*/

#include <windows.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>

/* Stuff to freeze things */
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <fcntl.h>

#define BR_TCP_WIN32_IMPL
#include "brahma.h"

#define BR_DEBUG

static BR_thread_t BR_poll_mgr;   /* Poll manager thread */

static BR_lock_t BR_exit_lck;         /*
				       * "Main" threads on clusters
				       * block on these until BR_exit
				       * takes the necessary action to 
				       * unlock them
				       */

static HANDLE BR_poll_lwpid;         /* A thread ID of a poll manager */

/* 
 * A signal sent to the poll manager (and possibly other threads) to
 * inform about termination 
 */
#define BR_SIGTERM SIGUSR1
#define EXIT(n) _exit(n)

void BR_exit_func(void);
void BR_exit_thread(void);
int get_host_names(char **host_names);
int create_host_names(char **host_names, int reorder);

void BR_error(char *s) {
   fprintf(stderr,"[%d] Brahma fatal error: %s\n",BR_HERE(), s); 
   EXIT(1);
}

void BR_msg(char *s) {
  printf("[%d]    %s\n", BR_HERE(), s);
  fflush(stdout);
}

int BR_thread_may_poll() {
  /* Only a single "poll manager" thread per cluster is allowed to poll */
  return BR_THREAD_SAME(BR_poll_mgr, BR_THREAD_ID());
}

void BR_set_poll_mgr() {
  /* Sets the current thread to be a polling thread */
  BR_poll_mgr = BR_THREAD_ID();
}



/* This gets executed at high(!) priority */
static void BR_background_thread() {
  int i;
  
  /* run at higher priority */
  SetThreadPriority(GetCurrentThread(), 1);  
  
  /* Remember the LWP ID */
  BR_poll_lwpid = GetCurrentThreadId() ;
  
  /* Use blocking poll. Most of the time, this thread is blocked on select
     not wasting cycles */
  while(1){
    BR_BLOCK_POLL();
  }
}

#define MAX_HOSTS 64
#define STR_LEN 256

int create_host_names(char *(host_names[]), int reorder){
  int i, j, num_hosts;
  char *nm;
  char buf[STR_LEN];
  char *old_names[MAX_HOSTS];

  num_hosts = get_host_names(host_names);

  if (reorder){
    /* Create a host list to be used everywhere */
    if (gethostname(buf, STR_LEN) != 0){
      BR_error("Cannot get hostname");
    }
    if(strcmp(host_names[0], buf)!=0){
      for(i=0; i<num_hosts; i++){
	old_names[i] = host_names[i];
      }
      
      host_names[0] = strdup(buf);
      j = 1;
      for(i=0; i < num_hosts; i++){
	if(strcmp(buf, old_names[i])){
	  host_names[j] = old_names[i];
	  j++;
	}
	else{
	  /* local host, has been entered already */
	  free(old_names[i]);
	}
      }
      free(old_names);
      num_hosts = j;
    }
  }
  return num_hosts;
}

int get_host_names(char *(host_names[])){
  int num_hosts, i,j;
  char buf[STR_LEN];
  char *hosts;

  hosts = getenv("SATHER_NODES");
  if (!hosts) {
    BR_error("SATHER_NODES is not defined");
  }
  
  i = 0; 
  num_hosts = 0;
  while(i<strlen(hosts)){
    j=0;
    while (isalnum(hosts[i]) || (hosts[i] == '.')) {
      buf[j++] = hosts[i++];
    }
    if (j>0){
      /* New host name */
      host_names[num_hosts] = (char *)malloc(j+1);
      strncpy(host_names[num_hosts], buf, j);
      host_names[num_hosts][j]='\0';
      num_hosts++;
    }
    else {
      /* skip the space */
      if (isspace(hosts[i])){
	i++;
      }
    } 
  }
  return num_hosts;
}

/* These need to be static to make putenv happy */
static char SATHER_NODES_str[STR_LEN];
static char SATHER_CLUSTER_NUMBER_str[STR_LEN];
static char *SATHER_HOME_str;
static char CLUSTERS_str[STR_LEN];
static char* CWD_str;

void BR_init(int clusters, int argc, char *argv[]) {
   int i,j, iID;
   char *cluster_number_str;
   int cluster_number;
   int num_hosts;
   char *(host_names[MAX_HOSTS]);
   char *hosts;
   char buf[STR_LEN], buf1[STR_LEN];
   

   if (clusters==0) {
     if (getenv("CLUSTERS")!=NULL) {
       sscanf(getenv("CLUSTERS"), "%d", &clusters);
     } else {
       clusters = 1;
     }
   }


   /* 
    * start things up on all nodes, but only if necessary
    * Clusters that have an env variable SATHER_CLUSTER_NUMBER set
    * do no need to start processes remotely and have a cluster
    * number assigned already 
    */

      
   if (!(cluster_number_str = getenv("SATHER_CLUSTER_NUMBER"))) {
     char exec[1024];
     char sather_home[512];
     char *config_path;
     
     /* Cluster 0 - needs to start up other clusters */
     cluster_number = 0;

     num_hosts = create_host_names(host_names, 1);

     /* Manufacture new SATHER_NODES in which the position of a node
	correposnd to its number */
     buf[0] ='\0';
     for(i=0; i<num_hosts; i++){
       strcat(buf, host_names[i]);
       if(i!=(num_hosts-1)){
	 strcat(buf, " ");
       }
     }
     sprintf(SATHER_NODES_str, "SATHER_NODES=%s", buf);
#ifdef DEBUG
     printf("Generated %s\n", SATHER_NODES_str);
     fflush(stdout);
#endif
     
     /* Change the current environment */
     putenv(SATHER_NODES_str);
     SATHER_HOME_str = getenv("SATHER_HOME");

     /* if user does not specify full path of configuration file */
     if ((config_path = getenv("TCPAM_CONFIG")) == NULL) {
       /* use the current directory by default */
       if(SATHER_HOME_str==NULL) { 
	 config_path="./";
	 fprintf(stderr,"Neither TCPAM_CONFIG or SATHER_HOME set - will try to use current directory\nto locate tcp/ip startup script.\n");
       } else {
	 config_path = malloc(STR_LEN);
	 strcpy(config_path, SATHER_HOME_str);
	 strcat(config_path,"/System/Common/Brahma");
       }
     }
     
     /* start things up remotely */
     /*     
	    sprintf(exec,"%s/start_nodes %d ",config_path, clusters);
	    for(i=0;i<argc;i++) {
	    strcat(exec," \"");
	    strcat(exec,argv[i]);
	    strcat(exec,"\"");
	    }
	    system(exec);
     */

     /* Get cwd */
     if (!(CWD_str = getenv("PWD"))){
       BR_error("Can't get pwd");
     }

     /* assemble command arguments */
     buf[0]='\0';
     for(j=0;j<argc;j++) {
       strcat(buf," \"");
       strcat(buf,argv[j]);
       strcat(buf,"\"");
     }

     /* Assemble SATHER_NODES */
     strcpy(buf1, "SATHER_NODES=");
     for(j=0; j<num_hosts; j++){
       strcat(buf1, host_names[j]);
       if (j!=(num_hosts-1)){
	 strcat(buf1, "\\ ");
       }
     }

     /* Set CLUSTERS */
     sprintf(CLUSTERS_str, "CLUSTERS=%d", clusters);

     j = 1;
     for(i=1; i<clusters; i++){
       sprintf(SATHER_CLUSTER_NUMBER_str, "SATHER_CLUSTER_NUMBER=%d", i);
       putenv(SATHER_CLUSTER_NUMBER_str);

       if (getenv("BR_DEBUG")){
	 sprintf(exec,
		 "rsh %s \'xterm -fn 7x13 -T \"[%s: %d]\" -display %s:0 -exec sh -c \"export SATHER_NODES; export SATHER_CLUSTER_NUMBER; export CLUSTERS; export SATHER_HOME; %s; SATHER_CLUSTER_NUMBER=%d; CLUSTERS=%d; SATHER_HOME=%s; cd %s; gdb %s\"\' &",
		 host_names[j], host_names[j], i, host_names[0], buf1, i, clusters, SATHER_HOME_str,
		 CWD_str, buf);        	 
       }
       else {
	 if(getenv("BR_USE_CUSTOMS")){
	   /* Use customs export to start remote jobs */
	   sprintf(exec,
		   "/usr/local/bin/export -attr %s -force sh -c %s &",
		   host_names[j], buf); 
	 }
	 else {
	   /* Use rsh to start remote jobs */
	   sprintf(exec,
		   "rsh %s \'sh -c \"export SATHER_NODES; export SATHER_CLUSTER_NUMBER; export CLUSTERS; export SATHER_HOME; %s; SATHER_CLUSTER_NUMBER=%d; CLUSTERS=%d; SATHER_HOME=%s; cd %s; %s\"\' &",
		   host_names[j], buf1, i, clusters, SATHER_HOME_str,
		   CWD_str, buf);        
	 }
       } 
#ifdef BR_DEBUG
       /*       BR_msg(exec); */
#endif

       system(exec); 
       j = (j+1)%num_hosts;
     }
   }
   else {
     /* Get the assigned cluster number for future use */
     cluster_number = atoi(cluster_number_str);
   }

   
   /* Prevent clusters from early termination. "Main" threads will
    * block on these */
   BR_exit_lck = BR_LOCK_CREATE();
   BR_LOCK(BR_exit_lck);  

   /* Register a function to be called on termination by exit() */
   if(atexit(BR_exit_func)){
     BR_error("atexit failed!");
   }

   num_hosts = create_host_names(host_names, 0);
   
   /* Initialize active messages */
   qq_enable(clusters, cluster_number, num_hosts, host_names);
   

   /*
    * We need an extra LWP for the polling thread. It is blocked
    * almost all the time, and we do not want the concurrency level
    * go below the number of CPUs 
    */
   /* (JN) ??? !!! */
   thr_setconcurrency(BR_PROCESSORS()+1);
   thr_keycreate(&BR_key, NULL);
   
   /*Start the poll manager thread */
   CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)BR_background_thread,
		(LPVOID)(BR_poll_mgr.t), 0, &iID) ;
   
   /* Initialize the cluster local array */
   BR_cluster_local = (caddr_t *)calloc(BR_CLUSTER_LOCAL_SIZE(), 1);

   /* Make sure everything has been initialized everywhere */
   BR_barrier_init();
   BR_BARRIER();
#ifdef BR_DEBUG
   BR_msg("BR_init finished");
#endif
};


/* Already started ... */
void BR_start(){
}
/* Does absolutely nothing; convenient no-op handler. */
void BR_dummy(BR_cluster_t ignored) {
}


void BR_exit() {
  int i;

  
  /* Fork off termination threads on all clusters */
  /* Threads are used for simplicity, to avoid interaction with
   * the blocking poll manager */
  for(i=0; i<BR_CLUSTERS(); i++){
    BR_FORK_0(i, BR_exit_thread);
  }

  /* 
   * At this point, cluster 0 will enter exit() - others have already
   * done that. The rest of the termination protocol is handled by 
   * BR_exit_func() which is registered to be called by exit()
   */
  exit(0);
}


void BR_exit_func(void){
  /* 
   * Block on this until BR_exit() is called on cluster 0 and 
   * eventually unlocks this locks 
   */
  BR_LOCK(BR_exit_lck);
}

static int BR_ok_to_disable;

void network_disable_h(BR_cluster_t cl){
  BR_ok_to_disable = 1;
}

void BR_exit_thread(void){
  int i;

  BR_ok_to_disable=0;
  BR_BARRIER();

  /* Allow polls in the current thread (The "official" Poll Manager
   * has terminated */
  BR_set_poll_mgr();  
  
  /* 
   * This stage is necessary to disable the network only after
   * the poll manager has been disabled. Otherwise we can get
   * a bunch of horrible errors from select that will not give up
   * easily of destroyed file descriptors 
   */
  if (BR_HERE()==0){
    for(i=1;i<BR_CLUSTERS();i++){
      BR_REQUEST_0(i, network_disable_h);
    }
  }
  else{
    while(!BR_ok_to_disable){
      BR_ASYNC_POLL();
    }
  }
  
  /* Safe to disable */
  qq_disable();
  
  /* Just terminate now */
  BR_UNLOCK(BR_exit_lck); 

  _exit(0);
}
     

static prstatus_t BR_status, *BR_lwpstats;
/* (JN) ??? !!! the structure above is found in /usr/include/sys/elf.h on borg */

static HANDLE BR_my_lwpid;
static sigset_t BR_set, BR_old_mask;

/* BR_freeze and BR_thaw  are not quite right (Boris 10/3/96) */
/*
** Forcefully halt all threads on all clusters (other than those
** used by Brahma itself) and wait for any user active messages to
** drain.  On return there is no user activity other than the
** executing thread.  This is useful for debugging and garbage
** collection.
*/
void BR_freeze() {
   char buf[80];
   int fd, nummaps, retval, i;
   void mark_from_region(caddr_t start, size_t len);
   void GC_push_regs();

   int other_lwps_still_running;

   sprintf(buf, "/proc/%lu", getpid());
   fd = open(buf, O_RDONLY);
   if (fd < 0) { perror("Couldn't open proc file"); exit(1); }

   /* Turn off preemption. */
   sigfillset(&BR_set);
   sigprocmask(SIG_SETMASK, &BR_set, &BR_old_mask);

   BR_my_lwpid = GetCurrentThreadId() ;

   /*
   ** Stop all other lwps.  Because lwps may be forked or die out
   ** while doing this, we need to loop until we're sure that we
   ** are the only lwp running.
   */
   other_lwps_still_running = 1;
   while (other_lwps_still_running) {
      retval = ioctl(fd, PIOCSTATUS, &BR_status);
      if (fd < 0) { perror("Couldn't PIOCSTATUS"); exit(1); }

      /*
      ** The number of lwps might not be right here, so we fake
      ** it by making room for some extra lwps (100).  This is
      ** a hack, but there doesn't seem to be any way around it.
      */
      BR_lwpstats = malloc(sizeof(prstatus_t)*(BR_status.pr_nlwp+100));
      retval = ioctl(fd, PIOCLSTATUS, BR_lwpstats);
      if (fd < 0) { perror("Couldn't PIOCLSTATUS"); exit(1); }

      other_lwps_still_running = 0;
      /* Try to suspend the other lwps. */
      for (i=1; i<BR_status.pr_nlwp+1; i++) {
         if ((BR_lwpstats[i].pr_who == BR_my_lwpid) ||
	     (BR_lwpstats[i].pr_who == BR_poll_lwpid)) continue;
         if (!(BR_lwpstats[i].pr_flags & PR_STOPPED)) {
            if (SuspendThread(BR_lwpstats[i].pr_who) < 0) {
	      /* (JN) ??? !!! pr_who = HANDLE BR_lwpstats[i].pr_who */ 
               /* Might fail if lwp exited already */
            }
            other_lwps_still_running = 1;
         }
      }

      if (other_lwps_still_running) free(BR_lwpstats);
   }
   close(fd);
}

/*
** Restarts threads halted by "BR_freeze".  This should be executed
** exactly once.
*/
void BR_thaw() {
   int i, retval;

   /* Turn other lwps back on. */
   for (i=1; i<BR_status.pr_nlwp+1; i++) {
      if (BR_lwpstats[i].pr_who == BR_my_lwpid) continue;
      retval = ResumeThread(BR_lwpstats[i].pr_who);
      if (retval < 0) { perror("_lwp_continue failed"); exit(1); }
   }

   free(BR_lwpstats);

   /* Turn preemption back on. */
   sigprocmask(SIG_SETMASK, &BR_old_mask, NULL);
}




