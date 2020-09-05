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
** This is the Myrinet+Solaris implementation of Brahma 
** It supports 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>
#include <ctype.h>

/* Stuff to freeze things */
#include <sys/lwp.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <fcntl.h>

#ifdef BR_MYRINET_SOLARIS
#define BR_MYRINET_SOLARIS_IMPL
#endif

#ifdef BR_MYRINET_SOLARIS_AT
#define BR_MYRINET_SOLARIS_AT_IMPL
#endif



#include "brahma.h"
#include "migration.h"

extern int lam_myproc;
extern void BR_background();
extern void init_locks();
extern int gam_enable(int);
extern int gam_disable();
extern int gethostname(char *, int);


#define LAM_CONFIG_DEFAULT "/u/sather/network/myrinet_routes/lam_conf" 
#define LAM_MAP_DEFAULT    "/u/sather/network/myrinet_routes/master.map"


#define DEBUG

BR_lock_t BR_exit_lck;    /*
			   * "Main" threads on clusters
			   * block on these until BR_exit
			   * takes the necessary action to 
			   * unlock them
			   */
/* The following may be used for termination detection to find out */
/* The state of the single remaning thread.                        */
static volatile int BR_in_exit=0;/* is set to 1 after exit() is called*/

BR_lock_t BR_background_lck;

#define EXIT(n) _exit(n)

void BR_exit_func(void);
void BR_exit_thread(BR_cluster_t src);




void BR_error(char *s) {
   fprintf(stderr,"[%d] Brahma fatal error: %s\n",BR_HERE(), s); 
   EXIT(1);
}

void BR_msg(char *s) {
  printf("[%d]    %s\n", BR_HERE(), s);
  fflush(stdout);
}



#define MAX_HOSTS 64
#define STR_LEN 256

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

int parse_lam_config(char *(host_names[]), FILE *fp){
  int num_hosts;
  int i, j;
  char buf[STR_LEN];
  char *hosts;

  fscanf(fp, "%s", buf);  /* skip l4lcp location string */
  fscanf(fp, "%d", &num_hosts);

  for(i=0; i < num_hosts; i++){
	 host_names[i] = (char *)malloc(STR_LEN);
    fscanf(fp, "%d%s", &j, host_names[i]);
  }

  return num_hosts;
}

/* These need to be static to make putenv happy */
static char SATHER_NODES_str[STR_LEN];
static char *SATHER_HOME_str;
static char CLUSTERS_str[STR_LEN];
static char* CWD_str;

char LAM_config_filename[256];
char LAM_map_filename[256];

volatile int BR_started = 0;

extern void BR_safe_poll();
extern void BR_mig_poll();

static int _BR_argc;
static char **_BR_argv;
static int _BR_clusters;

/* Need to initialize some synchronization data structures 
   such as pools, etc. before can continue */
void BR_init(int clusters, int argc, char *argv[]) {
  char *s;
  int p;
  _BR_argc = argc;
  _BR_argv = argv;
  _BR_clusters = clusters;
#if defined(BR_MYRINET_SOLARIS_AT)
  /* If using Active Threads, need to initialize the thread package
     first */
  /* Use "small" stack size for now: 0x2000 */
  /* Preallocate enough for LOCAL_MEM structure in pSather.h */
  
#ifdef AT_MIGRATION
  p = 0;
  if(s=getenv("BR_PROCESSORS")){
    p = atoi(s);
  }
  at_init(p, MIG_STACK_SIZE, 0);
#else
  at_init(0, 0x20000, 200);   /* Use default concurrency */
#endif
#endif
}

void BR_start() {
   int i,j;
   int num_hosts;
   char *(host_names[MAX_HOSTS]);
   char buf[STR_LEN], buf1[STR_LEN], buf2[STR_LEN];
   char host_name[STR_LEN];
   char exec_str[1024];
   int cluster_number;
   char *LAM_config_path;
   char *LAM_map_path;
   FILE *map_file;
   FILE *config_file;
   int clusters;

   clusters = _BR_clusters;
   if (clusters==0) {
     if (getenv("CLUSTERS")!=NULL) {
       sscanf(getenv("CLUSTERS"), "%d", &clusters);
     } else {
       clusters = 3;  /* Use all three machines: icsib78 samosa icsib18 */
     }
   }


   /* set up default filename path */
	strcpy(LAM_config_filename, LAM_CONFIG_DEFAULT);
	strcpy(LAM_map_filename, LAM_MAP_DEFAULT);  
	
	/* if the environment var is set up, use that */
	if ( ((LAM_config_path = getenv("LAM_CONFIG")) != (char *)NULL) ) {
	  strcpy(LAM_config_filename, LAM_config_path);
	}
	
	/* if the environment var is set up, use that for the map file */
	if ( ((LAM_map_path = getenv("LAM_MAP")) != (char *)NULL) ) {
	  strcpy(LAM_map_filename, LAM_map_path);
	}
	
	map_file = fopen(LAM_map_filename, "r");
	if (map_file == (FILE *)0) {			
	  fprintf(stderr,"Cannot open network map file %s\n", LAM_map_filename);
	  exit(-1); 
	}
	fclose(map_file);
	
	config_file = fopen(LAM_config_filename, "r");
	if (config_file == (FILE *)0) {			
	  fprintf(stderr,"Cannot open configuration file %s\n", LAM_config_filename);
	  exit(-1);
	}
	
	/* 
	 * Parse the LAM configuration file to extract host names and the number
	 * of hosts 
	 */
	
	/*num_hosts = get_host_names(host_names); */
	num_hosts = parse_lam_config(host_names, config_file);
	
   if (gethostname(host_name, STR_LEN)==-1){
     BR_error("Cannot get hostname");
   }


   /* 
    * start things up on all nodes, but only if necessary
    * Clusters that have an env variable SECONDARY_CLUSTER set
    * do no need to start processes remotely and have a cluster
    * number assigned already 
    */

  
   if (!getenv("SECONDARY_CLUSTER")) {
     /*need to fork off processes everywhere */
     
     /* Manufacture new SATHER_NODES in which the position of a node
		  correposnd to its number */

     /* Well, for now SATHER_NODES should be in this order anyway... */
     buf[0] ='\0';
     for(i=0; i<num_hosts; i++){
       strcat(buf, host_names[i]);
       if(i!=(num_hosts-1)){
			strcat(buf, " ");
       }
     }
     sprintf(SATHER_NODES_str, "SATHER_NODES=%s", buf);
     
     SATHER_HOME_str = getenv("SATHER_HOME");

     /* Get cwd */
     if (!(CWD_str = getenv("PWD"))){
       BR_error("Can't get pwd");
     }

     /* assemble command arguments */
     buf[0]='\0'; buf2[0]='\0';
     for(j=0;j<_BR_argc;j++) {
       strcat(buf," \"");
       strcat(buf,_BR_argv[j]);
       strcat(buf,"\"");

       strcat(buf2, _BR_argv[j]);
       strcat(buf2, " ");
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

     for(i=0; i<clusters; i++){
		 /* Do not start it again on the same cluster! */
		 if (!strstr(host_names[i], host_name)) {
			if (getenv("BR_DEBUG")){
			  sprintf(exec_str,
						 "rsh %s \'xterm -fn 7x13 -T \"[%s]\" -display %s -exec sh -c \"export SATHER_NODES; export CLUSTERS; export SECONDARY_CLUSTER; export SATHER_HOME; %s; CLUSTERS=%d; SECONDARY_CLUSTER=YES; SATHER_HOME=%s; cd %s; gdb %s\"\' &",
						 host_names[i], host_names[i], getenv("DISPLAY"), buf1, 
						 clusters, SATHER_HOME_str, CWD_str, buf);  
			}
			else {
			  if(getenv("BR_CUSTOMS")){
				 /* Use customs export to start remote jobs */
				 sprintf(exec_str,
							"/usr/local/bin/export -attr %s -force sh -c \"export SATHER_NODES; export CLUSTERS; export SECONDARY_CLUSTER; export SATHER_HOME; %s; CLUSTERS=%d; SECONDARY_CLUSTER=YES; SATHER_HOME=%s; cd %s; %s\" &",
							host_names[i],buf1,clusters,SATHER_HOME_str,CWD_str, buf);
			  }
			  else {
				 /* Use rsh to start remote jobs */
				 sprintf(exec_str,
							"rsh %s \'sh -c \"export SATHER_NODES; export CLUSTERS; export SECONDARY_CLUSTER; export SATHER_HOME; %s; CLUSTERS=%d; SECONDARY_CLUSTER=YES; SATHER_HOME=%s; cd %s; %s\"\' &",
							host_names[i],buf1,clusters,SATHER_HOME_str,CWD_str, buf);
			  }
			} 
#ifdef BR_DEBUG
			BR_msg(exec_str); 
#endif
			
			system(exec_str); 
		 }
     }
   }

   /* It is important that Myrinet AM are initialize in the exact
      order of cluster numbers on different clusters */
   
   /*  
       for(i=0; i<num_hosts; i++){
       if(strstr(host_names[i], host_name)){
       cluster_number = i;
       break;
       }
       }
       */

   /* Now, wait until it is ok to start: cluster (cluster_number-1)
      already started */
   /*
     if(cluster_number != 0){
     while(!fopen("/tmp/br_go", "r")){
     }
     system("rm /tmp/br_go");
     }
     */


#if defined(BR_MYRINET_SOLARIS_AT)
     /* If using Active Threads, need to initialize the thread package
	first */
   /* Use "small" stack size for now: 0x2000 */
   /* Preallocate enough for LOCAL_MEM structure in pSather.h */
      
   /*     at_init(0, 0x2000, 64); */  /* Use default concurrency */
#endif

   /* Initialize different locks */
   init_locks();
   /* Initialize the cluster local array */
   BR_cluster_local = (caddr_t *)calloc(BR_CLUSTER_LOCAL_SIZE(), 1);

   gam_enable(0);  
      
   /* ok, allow the next one to proceed */
/*   if (cluster_number < (num_hosts-1)){
     if (getenv("BR_CUSTOMS")){
       sprintf(exec_str,"/usr/local/bin/export -attr %s -force sh -c \"echo start > /tmp/br_go\" 2>/dev/null ", host_names[cluster_number+1]);
     }
     else {
       sprintf(exec_str,"rsh %s \"echo start > /tmp/br_go\" ", host_names[cluster_number+1]);
     }
     system(exec_str);
   }
   */
   

    /* Prevent clusters from early termination. "Main" threads will
    * block on this */
   BR_exit_lck = BR_LOCK_CREATE();
   BR_LOCK(BR_exit_lck);  

   /* Register a function to be called on termination by exit() */
   if(atexit(BR_exit_func)){
     BR_error("atexit failed!");
   }
  
   
#if defined(BR_MYRINET_SOLARIS)
   thr_setconcurrency(BR_PROCESSORS());
   thr_keycreate(&BR_key, NULL);
#endif


   BR_background_lck = BR_LOCK_CREATE();
   BR_LOCK(BR_background_lck);

   
#if defined(BR_MYRINET_SOLARIS)
   /* Start the background thread that handles deferred forks */
   /* but only if using Solaris threads */
   thr_create(NULL, 0, (void *(*)(void*)) BR_background,
		    NULL, THR_DETACHED, NULL);

   /* Wait until the background thread has been forked off */
   BR_LOCK(BR_background_lck);

   /* Run self and all forked threads at higher priority */
   thr_setprio(thr_self(), 1); 
#endif
   /* Active Threads do need any background threads ... */


   BR_BARRIER(); 
   BR_started = 1;
/* Poll the network when idle */
#if defined(BR_MYRINET_SOLARIS_AT)
   /*#ifdef AT_MIGRATION
   at_do_when_idle(BR_mig_poll);
#else
   at_do_when_idle(BR_POLL);  
#endif*/
   at_do_when_idle(BR_POLL);  
#endif


}

extern int BR_migrated_to();
extern int BR_migrated_from();

void BR_exit_func(void){
  /* 
   * Block on this until BR_exit() is called on cluster 0 and 
   * eventually unlocks BR_exit_lck
   * In the absence of other runnable threads, the messages are
   * processed by the low priority background thread (BR_background)
   * that also takes care of thread creation/recycling/pooling
   *
   */
  BR_in_exit = 1;
  BR_LOCK(BR_exit_lck);

  at_printf("CLUSTER [%d]:  to: %d    from: %d   THREADS: %d\n", BR_HERE(),
	    BR_migrated_to(), BR_migrated_from(), at_thread_count());
  at_printf("AVERAGE STACK IN: %e  OUT: %e\n", 
	    ((double)BR_stack_received)/BR_migrated_to(),
	    ((double)BR_stack_pushed)/BR_migrated_from()); 
	    
  /* Now, call *real* exit */
  EXIT(0);
}

int BR_exit_reached(){
  return BR_in_exit;
}

void BR_exit_thread(BR_cluster_t src){
  BR_BARRIER(); /*Make sure all reached this point */

  BR_started = 0;  /* Brahma goes out of business */
  gam_disable();
  
  /* Ok to terminate the main thread now */
  BR_UNLOCK(BR_exit_lck); 
  /* 
   * This is necessary because the main thread might never
   * execute BR_exit_func when it never reaches the end of main
   * (stuck in an infinite loop, etc).
   */
  BR_exit_func();
}
     
void BR_exit() {
  int i;
  
  if(BR_HERE() != 0){
    BR_exit_remote();
  }
  else {
    for(i=1; i<BR_CLUSTERS(); i++){
      BR_FORK_0(i, BR_exit_thread);
    }
    
    /* Now do self */
    BR_FORK_0(0, BR_exit_thread);
    /* 
     * At this point, cluster 0 will enter BR_exit_func - others have already
     * done that. The rest of the termination protocol is handled by 
     * BR_exit_func() 
     */
    BR_exit_func();
  }
}

/* 
 * This FORK handler can be used on cluster 0 (from others) to 
 * signal termination. Note - it must be forked!
 */

void BR_exit_h_0(BR_cluster_t src){
  BR_exit();
}

void BR_exit_remote(){
  BR_FORK_0(0, BR_exit_h_0);
}










