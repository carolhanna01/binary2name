/*									tab:2
 *
 * lam_common.c - lam functions common the both LAM amd AM1.5
 *
 * "Copyright (c) 1996 by The Regents of the University of California.  
 *  All rights reserved."
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
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 * Author: 			Rich Martin
 * Version:			$Id: lam_common.c,v 1.6 1997/01/21 05:45:24 borisv Exp $
 * Creation Date:	Thu Apr 25 11:33:50 1996
 * Filename:		lam_common.c
 * History:
 *		
 */

#include  <stdio.h>
#include  <stdlib.h>
#include  <unistd.h>
#include <string.h>
#include  "lanai_device.h"
#include <signal.h>
#include <sys/types.h>

#include <sys/ipc.h>
#include <sys/shm.h>

#include <sys/time.h>
#include <sys/stat.h>
#include <pwd.h>

#include "ep.h"

#include "lam_sync.h"

#ifdef SOLARIS
#include <sys/systeminfo.h> 
#define gethostname(n,l)  sysinfo(SI_HOSTNAME,n,l)
#else
#define memmove(d,s,l) bcopy(s,d,l)
#endif

#ifdef USE_GLUNIX
/* GLUnix interface */
#include <cam.h>
#include <glib.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#endif 


unsigned int lam_glunix_debug=0 ;

#define LAM_SHMEM_KEY     0x700328
#define LAM_SHMEM_SIZE    0x1000
#define PERMS             0666

#define CLOCK_VALUE       0x50E450E4 

unsigned int *lam_shmem_addr; 

unsigned short sts;
int copy_block_len,units;
int unit;

volatile int *lanai_done, *host_done; 
volatile private_ep_t *p_endpt; 
volatile ep_t *endpt;
volatile route_t *routes;
volatile desc_t *in_d,*out_d;
int in_num,out_num;

lanai_symbol_table *lanai_symtab;

volatile int *dma_base;
volatile int *user_base;

int *lam_dma_base;

int lam_myproc;
int lam_procs =2;
int lam_reply =-1;  /* which node to reply to */

int flow_cntl[MAX_REMOTE];
int req_flow_in[MAX_REMOTE];
int req_flow_out [MAX_REMOTE];

/* sequence numbers for more robust debugging */ 

unsigned int seq_req_out[MAX_REMOTE];  /* outbound request sequence numbers*/
unsigned int seq_rep_in[MAX_REMOTE];   /* incomming reply sequence numbers */
unsigned int seq_req_in[MAX_REMOTE];   /* incomming request sequence numbers */

/* note there is no seq_rep_out (outbound reply sequence number ) because 
 * the am_poll code will just reflect back whatever the incomming seq. no
 * the incomming message had 
 */
unsigned int seq_glob;      /* used instead of seq_rep_out */

int am_poll_count =0;
int good_polls =0;
int null_polls =0;
int no_credits =0; 

int rep_flow_in[MAX_REMOTE];
int rep_flow_out[MAX_REMOTE];
int lam_frag_size; 


/* Synchronization stuff for thread safety */
BR_POLL_DEC;  /* The polling lock is declared in lam.c */
BR_OUT_DEC;   /* The outgoing buffer lock is declared in lam.c */
BR_CREDITS_DEC(MAX_REMOTE);  /* Locks protecting destination specific
                             stuff (such as credits)  */ 


/* nice little routing provided for by Yannis Schoinas <schoinas@cs.wisc.edu>
 */

void 
fmemcpy(void *to, void *from, int size) {

    if (((((int) (to)) & 0x7) == 0) && ((((int) (from)) & 0x7) == 0) &&
       (size % sizeof(double) == 0))
    {
      register int i;

      register double *tmp1 = (double *)from, *tmp2 = (double *)to;
      for (i = 0; i < size/sizeof(double); ++i)
      {
        tmp2[i] = tmp1[i];
      }
    }
    else
      memcpy(to, from, size);
}

/* reset the head of the config file pointer to the hostname-number part 
 *  of the file 
 */
int 
lam_reset_conf_fp_to_names(FILE *conf_file) {
  int num_procs; 
  char temp_path[256]; 

  num_procs = -1; 

  if(fseek(conf_file,0,SEEK_SET) == -1) {
    fprintf(stderr,"Cannot reset config file.\n");
    fflush(stderr);
    exit(-1);
  }
  
  if (fscanf(conf_file, "%s ", temp_path) == EOF){ 
    fprintf(stderr,"Cannot read lcp file in configuration file\n");
    fflush(stderr);
    exit(-1);
  }

  if (fscanf(conf_file, "%d ", &num_procs) == EOF) {
    fprintf(stderr,"Cannot read number of nodes in configuration file\n");
    fflush(stderr);
    exit(-1);
  }
  return num_procs; 
}


/* take a string in the form FUBARX and file pointer */
/* assume file is of the form <number> <name>        */ 
/* return number associated with name                */ 

int 
lam_name_to_number(char *name,FILE *conf_file) {
  int len;
  int i;
  int value;
  char *first_char; 
  char *last_char,*first_dot;   
  int num_procs, which_number; 
  char remote_host[256]; 
  char temp_path[256]; 
  long pos_orig; 

  value = -1;
  len= strlen(name);

  pos_orig = ftell(conf_file); 

  num_procs = lam_reset_conf_fp_to_names(conf_file); 

  which_number = -1;
  for(i=0;i<num_procs;i++) { 

    if(fscanf(conf_file,"%s %s",temp_path, remote_host) == EOF) {
      fprintf(stderr,"Cannot find this node in config file.\n");
      fflush(stderr);
      exit(-1);
    }
    
    if ( (first_dot = (char *) strchr(remote_host,'.')) != (char *) NULL) {
      first_dot[0] = '\0';
    }      

    if(strcmp(name,remote_host) == 0) {
      which_number = atoi(temp_path);
      break; 
    }
  }

  if ( fseek(conf_file, pos_orig, SEEK_SET)  == -1 ) {
    fprintf(stderr,"number_to_route can't return file pointer\n");
    fflush(stderr); fflush(stdout);
    exit(-1); 
  }

  return which_number;
}



/* for the route from i to j, fill in the route as a string */
int 
lam_number_to_route(FILE *map,int source,int dest, char *path) {
  int checked_src,checked_dest; 
  int line_len; 
  char line[512];
  char *token;
  char hops[MAX_DEPTH];

  if ( fseek(map, 0, SEEK_SET) == -1) {
    fprintf(stderr,"number_to_route can't reset file pointer\n");
    fflush(stderr); fflush(stdout);
    exit(-1); 
  }

  checked_src = -1;

  /* find the line with the source node */
  while (checked_src != source) {
    fscanf(map,"%[\n]",line); /* you have to consume the newline else
				 infinite loop! */

    if (fscanf(map,"%[^\n]",line) == EOF ) {
      fprintf(stderr,"could not find source %d in map file\n",source);
      exit(-1);
    }

    /* find line matching the 'source' number */

    if (sscanf(line,"Source %d",&checked_src) == EOF) {
      fprintf(stderr,"invalid map file format\n",source);
      exit(-1);      
    }
  }

  token = strtok(line," \t");
  token = strtok(NULL," \t"); /* skip _Source_ X */
  token = strtok(NULL," \t"); /* skip Source _X_ */
  while(token != NULL) {
    if(sscanf(token,"%d|%s",&checked_dest,hops) == EOF) {
      fprintf(stderr,"invalid map file format\n");
      exit(-1);
    }
    if(checked_dest == dest) {
/*      printf("Source %d, dest %d, hops %s\n",source,dest,hops); */
      strcpy(path,hops);
      return 1;
    }
    token = strtok(NULL," \t");
  }
  return 0;
}


#ifdef USE_GLUNIX
/* get configuration from GLUNIX */
void 
LAM_get_glu_config() {
  pid_t pid;
  Npid glunix_pid;
		
  if (lam_glunix_debug) {
    fprintf(stderr,"about to get parallel degree \n");
    fflush(stderr);
  }

  /* get total number of nodes */
  lam_procs = Glib_GetParallelDegree();

    if (lam_procs <= 0) {
      printf("GLUNIX reports that this proces is not part of a parallel program: AM_procs = %d\n", lam_procs);
      exit(-1);
    }
    /* get my node number */
    lam_myproc = Glib_GetMyVnn();

  pid = getpid();
  if (pid < 0) {
    perror("Cannot get local PID");
    exit(-1);
  }

  if (lam_glunix_debug) {
    fprintf(stderr,"about to get my npid\n");
    fflush(stderr);
  }

  /* get the GLUNIX pid to agree on a server port number */
  glunix_pid = Glib_GetMyNpid();

  if (glunix_pid == NPID_NOBODY) {
    printf("GLUNIX reports that this proces is not part of a parallel program: glunix_pid = %d\n", glunix_pid);
    exit(-1);
  }

  if (lam_glunix_debug) {
    fprintf(stderr,"got glunix info\n");
    fflush(stderr);
  }
}
#endif 


int 
am_grab_lock_or_die(char *my_name) {
  register int tmp_reg; 
  volatile int *mem_addr; 
  pid_t my_pid,other_pid; 
  int shmem_id;
  int tries,i; 
  struct stat stat_buf;
  char pid_str[64];
  char pid_str_padded[64];
  char stat_path[64];
  uid_t  uid; 
  struct passwd *st_pw;

  if (( shmem_id = shmget(LAM_SHMEM_KEY, LAM_SHMEM_SIZE,
			       (PERMS|IPC_CREAT)))<0) {
    perror("can't open shared segment to get lock");
    exit(1);
  }

  my_pid = getpid();
  tries =0;
  /* check if someone is using the card */

  lam_shmem_addr = (void *) shmat(shmem_id,(char *) 0 , 0);

 try_lock:

  tmp_reg = 0; 
  AM_SWAP( (*lam_shmem_addr) ,tmp_reg);
  tries++;
  if (tmp_reg == 0) {
    /* see if this person is around */
    other_pid = (pid_t) lam_shmem_addr[1];

    if ( (other_pid == 0) || (other_pid == my_pid))  {
       lam_shmem_addr[1]= my_pid;
       AM_INIT_LOCK( lam_shmem_addr[0]);
       goto try_lock;
    }

    /* i = kill(other_pid,0); */
    strcpy(pid_str_padded,"00000");
    sprintf(pid_str,"%d",other_pid);
    
    strcpy(&(pid_str_padded[5-strlen(pid_str)]),pid_str);
    strcpy(stat_path,"/proc/");
    strcat(stat_path,pid_str_padded);

    i = stat(stat_path, &stat_buf);
    if (i == 0) {  /* eek, process exists */
      uid= stat_buf.st_uid;
      st_pw = getpwuid(uid);
      fprintf(stderr,"Process %d (user %s) is holding the lock for the LANai card on %s\n",other_pid,st_pw->pw_name,my_name);
      exit(1);
    }
    else { /* doesn't exist? reset and try again */
      if (tries > 100) {
	fprintf(stderr,"Process %d failed to aquire lock after %d tries on %s\n",my_pid,my_name,tries);
	exit(1);
      }
      AM_INIT_LOCK( lam_shmem_addr[0]);
      goto try_lock;
    }
  }
  /*ok, got the lock  */
  lam_shmem_addr[1] = my_pid; 
}


extern  char LAM_config_filename[];
extern  char LAM_map_filename[];

int
gam_enable(int use_glunix) {	/* arg == list of up to 4 files, */

  int i,j,port,hop,my_physical_node;
  int port_num[MAX_REMOTE];
  int node_num;
  char *p, *s,*first_dot;
  char *home;
  volatile int *dma_sts; 
  static FILE *config_file;
  static FILE *map_file;
  char lcp_file[256];
  char node_name[256];
  char my_hostname[256];
  char remote_host[256];
  unsigned int clock_value;
  pid_t my_pid,other_pid; 
  int shmem_id;
  int *remote_ip_num;
  unsigned int l_addr; 
  char temp_name[256];
  char paths[MAX_REMOTE][MAX_HOPS]; /* eight hops for now */
  char *dest_route,temp_path[8];
  

  gethostname(my_hostname,80);

  /* truncate hostname to first dot */
  if ( (first_dot = (char *) strchr(my_hostname,'.')) != (char *) NULL) {
    first_dot[0] = '\0';
  }
  
#ifdef GLUNIX
  if (getenv("LAM_GLUNIX_DEBUG") != NULL)
    lam_glunix_debug =1; 

  if (lam_glunix_debug) {
    fprintf(stderr,"new LAM code \n");
    fprintf(stderr,"inside am_enable\n");
  }
  
#endif 

  
  config_file = fopen(LAM_config_filename, "r");
  if (config_file == (FILE *)0) {			
    fprintf(stderr,"Cannot open configuration file %s\n", LAM_config_filename);
    exit(-1);
  }

  map_file = fopen(LAM_map_filename, "r");
  if (map_file == (FILE *)0) {			
    fprintf(stderr,"Cannot open network map file %s\n", LAM_map_filename);
    exit(-1); 
  }
  
  
  (void) fscanf(config_file,"%s ",lcp_file);
  
  unit = 0;
  units = open_lanai_copy_block(&copy_block_len,&sts);	
  if (copy_block_len<=0) {
    perror("ops: open_copy_block()");
    fprintf(stderr,"error! Length = %d  <= 0\n",copy_block_len);
  }
  else {

#ifdef DEBUG
#if 0
  printf("ops: User opened copy_block \n"); 
  printf("ops: user_ptr = %x    dma_ptr = %x    copy_block_len = 0x%x\n",
	 (unsigned)UBLOCK[unit],(unsigned)DBLOCK[unit],copy_block_len);
#endif 
#endif 

  }
  
  
  if ( copy_block_len < 
      ((IN_Q_SIZE+OUT_Q_SIZE)*LAM_PKT_WORDS*sizeof(int))) {
    
    fprintf(stderr, "warning! copy_block_len to short, len %d bytes wanted %d\n",
	    copy_block_len,
	    (IN_Q_SIZE+OUT_Q_SIZE)*LAM_PKT_WORDS*sizeof(int));
  }


  am_grab_lock_or_die(my_hostname);
  memset((void *)UBLOCK[0],0,copy_block_len);

  lanai_interrupt_unit(0,0);
  lanai_reset_unit(0,1);
  lanai_clear_memory(0);
  clock_value = lanai_get_useful_clock_value(lanai_board_type(0)); 

  /* hack for 4.1 based board because myricom can't get this right */
  if (clock_value  != CLOCK_VALUE) { 
    clock_value = CLOCK_VALUE;
  }

  lanai_load_and_reset(0, lcp_file,0,1,clock_value,NULL); 

  if ((lanai_symtab=lanai_read_symbol_table(lcp_file)) == 0) {
    printf(" %s has no symbol information!\n", lcp_file);
    exit(-1);
  }

  if ((l_addr = lanai_symbol_value(lanai_symtab,"_endpt")) == 0xffffffff){
    printf("could not find symbol endpt in file %s\n",lcp_file);
    exit(-1);
  }
  endpt =  (ep_t *)  ( ((char*)LANAI[0]) + l_addr ); 

  if ((l_addr = lanai_symbol_value(lanai_symtab,"_routes")) == 0xffffffff){
    printf("could not find symbol routes in file %s\n",lcp_file);
    exit(-1);
  }
  routes =  (route_t *)  ( ((char*)LANAI[0]) + l_addr ); 

  if ((l_addr = lanai_symbol_value(lanai_symtab,"_p_endpt")) == 0xffffffff){
    printf("could not find symbol routes in file %s\n",lcp_file);
    exit(-1);
  }
  p_endpt = (private_ep_t *)  ( ((char*)LANAI[0]) + l_addr ); 


  if ((l_addr = lanai_symbol_value(lanai_symtab,"_host_done")) == 0xffffffff){
    printf("could not find symbol host_done in file %s\n",lcp_file);
    exit(-1);
  }
  host_done = (volatile int *)  ( ((char*)LANAI[0]) + l_addr ); 


  if ((l_addr = lanai_symbol_value(lanai_symtab,"_lanai_done")) == 0xffffffff){
    printf("could not find symbol lanai_done in file %s\n",lcp_file);
    exit(-1);
  }
  lanai_done = (volatile int *)  ( ((char*)LANAI[0]) + l_addr ); 

  if ((l_addr = lanai_symbol_value(lanai_symtab,"_dma_sts")) == 0xffffffff){
    printf("could not find symbol dma_sts in file %s\n",lcp_file);
    exit(-1);
  }
  dma_sts = (volatile int *)  ( ((char*)LANAI[0]) + l_addr ); 

  lanai_free_symbol_table(lanai_symtab);

  while (!(*lanai_done)) ;
  
#ifdef GLUNIX
  if (lam_glunix_debug) {
    fprintf(stderr,"lanai all done\n");  
  }
#endif 

  *dma_sts = (int) sts;
  /*printf("sts is 0x%x 0x%x \n",(*dma_sts),sts); */
  
  if (fscanf(config_file, "%d ", &lam_procs) == EOF) {
    fprintf(stderr,"Cannot read number of nodes in configuration file\n");
    fflush(stderr);
    exit(-1);
  }
  
  lam_myproc = -1;
  for(i=0;i<lam_procs;i++) {  /* this loop searches for the current lam_myproc */
    if(fscanf(config_file,"%s %s",temp_path, remote_host) == EOF) {
      fprintf(stderr,"Cannot find this node in config file.\n");
      fflush(stderr);
      exit(-1);
    }
    
    if ( (first_dot = (char *) strchr(remote_host,'.')) != (char *) NULL) {
      first_dot[0] = '\0';
    }      

    if(strcmp(my_hostname,remote_host) == 0) {
      lam_myproc = i;
    }
  }

  if(fseek(config_file,0,SEEK_SET) == -1) {
    fprintf(stderr,"Cannot reset config file.\n");
    fflush(stderr);
    exit(-1);
  }
  if(fscanf(config_file,"%s %d ", temp_path, &lam_procs) == EOF) {
    fprintf(stderr,"Cannot read number of nodes in configuration file\n");
    fflush(stderr);
    exit(-1);
  } /* this sets the file pointer back to its original location.  */
  
#ifdef USE_GLUNIX
  /* has side effect of resetting lam_procs and lam_myproc */
  if (lam_glunix_debug) {
    fprintf(stderr,"about to get glunix config info \n" );  
  }

  if (use_glunix) {
    LAM_get_glu_config();
  }
#endif 


  if (lam_procs > MAX_REMOTE) {
    fprintf(stderr,"too many nodes in config file, max is %d\n",MAX_REMOTE);
    exit(-1);
  }


  if (use_glunix) {
#ifdef USE_GLUNIX
    remote_ip_num = (int *) Am_GetFastAddrByVnn( (VNN) lam_myproc);
    strcpy(remote_host,(char *)Am_GetFastAddrStr(remote_ip_num));

    /* hack to get hostname to just one dot */
    if ( (first_dot = (char *) strchr(remote_host,'.')) != (char *) NULL) {
      first_dot[0] = '\0';
    }      
    my_physical_node = lam_name_to_number(remote_host,config_file);
#endif
  }
  else {
    my_physical_node = lam_name_to_number(my_hostname,config_file);
    /*    fprintf(stderr,"my physical node number %d\n",my_physical_node); */
  }

  fflush(stdout);

  lam_reset_conf_fp_to_names(config_file); 

  for (i=0; i< lam_procs; i++) {

#ifdef USE_GLUNIX
    if (use_glunix) {
      remote_ip_num = (int *) Am_GetFastAddrByVnn( (VNN) i);
      strcpy(remote_host,(char *)Am_GetFastAddrStr(remote_ip_num));
    }

#endif

    if (!use_glunix) {
      if (fscanf(config_file, "%s %s",temp_path,remote_host) == EOF) {
	fprintf(stderr,"not enough nodes in configuration file\n");
	fflush(stderr);
	exit(-1);
      }
    }

    if ( (first_dot = (char *) strchr(remote_host,'.')) != (char *) NULL) {
      first_dot[0] = '\0';
    }      

    node_num = lam_name_to_number(remote_host,config_file); 
    if (node_num == -1) {
      fprintf(stderr,"node names must have a number, sorry \n");
      exit(-1);
    }

    
    j = lam_number_to_route(map_file,my_physical_node,node_num,paths[i]);
      
    if (j ==0 ) {
      fprintf(stderr,"Node %s my_phys %d remote %d missing from map file\n",
	      remote_host,my_physical_node,node_num);
    }

    
  } /* end for i = 1 to procs */


  fclose(config_file); 
  fclose(map_file);

  if (lam_myproc == -1) {
    fprintf(stderr,"could not find my node \n"); fflush(stderr);
    exit(1);
  }      
  

  for (i = 0; i< lam_procs; i++) {
    routes[i].len = strlen(paths[i]);      
    for (j =0; j<  strlen(paths[i]); j++) {

      /* really dumb encoding */
      /* 01234567 is +0 ..  +7 */
      /* ABCDEFGH is -O ..  -7 */

      hop = (int) (paths[i][j] - '0') ;
      /* try a negative route */
      if ( (hop <0) || (hop > 7) ) {
	hop = paths[i][j] - 'A' ;
	hop = -hop;
	if ((hop > 0) || (hop < -7)) {
	  printf("invalid hop entry %c to node %d hop %d\n",(char) hop,i,j);
	  exit(1);
	}
      }
      temp_path[j] = ((hop&0x3f) |0x80);
    }
      
    dest_route = (char *) routes[i].path;

    switch (routes[i].len) {
    case 0:
      break;
    case 1:
      memcpy(&dest_route[1],temp_path,1);
      break;
    case 2:
      memcpy(dest_route+0,temp_path,2);
      break; 
    case 3:
      memcpy(dest_route+1,temp_path,3);	
      break;
    case 4:
      memcpy(dest_route+0,temp_path,4);	
      break;
    case 5:
      memcpy(dest_route+3,temp_path,5);	
      break;
    case 6:
      memcpy(dest_route+2,temp_path,6);	
      break;
    case 7:
      memcpy(dest_route+1,temp_path,7);	
      break;
    case 8:
      memcpy(dest_route+0,temp_path,8);	
      break;
    default:
      fprintf(stderr,"node %d: route to node %d too long \n",lam_myproc,i);
      exit(1);
      break; 
    } /* end switch */

  } /* end for lam_procs */
  
  /* set return path */
  p_endpt->src = lam_myproc; 
  
  in_num =out_num=0 ;
  in_d = &(endpt->in[in_num]);
  out_d = &(endpt->out[out_num]);

  /* check for double word alignment */
/* if ( (((int) (in_d)&0x7) !=0) ) {
 *    fprintf(stderr,"wonk! input queue not double word aligned. Fix lcp\n");
 *    exit(1); 
 *  }  
 */

  /* set up flow control info */
  for (i=0; i<lam_procs; i++) {
    flow_cntl[i] = (DEPTH); 
    req_flow_out[i] = rep_flow_in[i] = 0;
    req_flow_in[i] = rep_flow_out[i] = 0;
    seq_req_out[i] = seq_rep_in[i] = seq_req_in[i] = 0x1;  
  }

  dma_base = lam_dma_base = (unsigned *)DBLOCK[0];
  user_base = (volatile int *) UBLOCK[0];
  
  /* set up receive bulk packets  */
  for (i = 0; i < IN_Q_SIZE ; i++) {
    endpt->bulk_d_p[i].hi = (uint *) dma_base + (LAM_PKT_WORDS*i); 
  }
  
  /* set up send */
  for (i=0; i< OUT_Q_SIZE ; i ++) {
    endpt->out[i].hdr.b_hdr.la = (int *) dma_base +
      (LAM_PKT_WORDS*2*DEPTH*MAX_REMOTE)+(i*LAM_PKT_WORDS); 

  }; 
  
  lam_frag_size = (LAM_PKT_WORDS*sizeof(int));
  if (lam_frag_size > (LAM_PKT_WORDS*sizeof(int))) {
    printf("frag size > packet size! \n");
  }

  gam_init_tables();  
  BR_BARRIER_init(0);
  
  *host_done = 1;  

  if (lam_glunix_debug) {
    fprintf(stderr,"node  %d about to call glib_barrier\n", lam_myproc);
    fflush(stderr);
  }

#ifdef USE_GLUNIX /*<coy>*/
  if (use_glunix) {
    Glib_Barrier();
  }
  if (lam_glunix_debug) {
    fprintf(stderr,"node  %d finished glib_barrier\n", lam_myproc);
    fflush(stderr);
  }
#endif
}

int 
gam_disable() {
  shmdt((void *) lam_shmem_addr);
}


#ifdef USE_GLUNIX
int 
lam_glunix_spawn(int *argc_p, char **argv,char *envp[] ) {

  int i,retval;
  int num_nodes;
  char *node_names,*next_node;
  struct hostent *hp;
  int nodes_to_spawn[MAX_REMOTE];  /* IP addresses */


  if (!Glib_Initialize()) {
    fprintf(stderr,"Error - GLUnix couldn't initialize!\n");
    exit(-1);
  }


  if (Glib_AmIStartup()) {

    if ( *(argc_p) > 1) {
      /* argv[1] is number of nodes; argv[0] is program name; argv is
       * list of arguments including program name and number of nodes.
       * We need to remove argv[1] when the argument array is passed
       * to the splitc_main().
       */
      num_nodes = atoi(argv[1]);
      if (num_nodes < 0) {
	fprintf (stderr,"lam: Error - must have positive nodes.\n");
	exit(1);
      }

      i = Glib_GetNumAvailableNodes ();

      if (i < num_nodes) {
        fprintf (stderr,
		 "lam: Error - Only %d nodes available (requested %d).\n",
                 i, num_nodes);
        exit (1);
      }

      for (i = 2; i <= *argc_p; ++i) {
        argv[i - 1] = argv[i];
      }
      *argc_p --;

      /* NB GLUNIX_NODES works now, this is redundant */
      node_names = getenv("LAM_GLUNIX_NODES") ;

      if (node_names != (char *) NULL) {

	next_node = (char *) strtok(node_names,",");
	
	for (i=0; i< num_nodes; i++) {

	  if (next_node == NULL) {
	    fprintf(stderr,"not enough nodes in LAM_GLUNIX_NODES\n");
	    exit(1);
	  }

	 /* get the IP address */
	  hp = gethostbyname(next_node);
	  if ((hp == 0) || (hp->h_addr == 0)) {			
	    fprintf(stderr,"lam_glunix_spawn cant get IP address of host %s\n",
		    next_node);
	    return -1;
	  }		
  
	  memcpy(&nodes_to_spawn[i], ((unsigned int *)hp->h_addr),
		 sizeof(int));
	  
	  next_node = (char *) strtok( NULL," ");
	}

	/* now we have all the IP addresses. Time to spawn off jobs */
	if (Glib_Spawnefp(num_nodes, nodes_to_spawn, GLIB_SPAWN_ONE_EXIT,
			  argv[0], argv,envp) == -1 ) {
	  fprintf(stderr," Glib_Spawnefp failed \n");
	}
	exit(0);
      } /* end if spawn on specific nodes */
      else {
	fprintf(stderr,"about to spawn nodes\n");		
	if ( (retval = Glib_Spawn(num_nodes, argv[0], argv)) == -1 ) {
	  fprintf(stderr," Glib_Spawn failed \n");
	}
	printf("spawned nodes retval == %d\n", retval);	
      } /* else spawn on any old nodes */
      exit(0);
    } /* if more than one arg */
    else {
      fprintf(stderr,"about to spawn nodes\n");		
      if (   Glib_Spawn(1, argv[0], argv) == -1 ) {
	fprintf(stderr," Glib_Spawn failed \n");
      }
      exit(0);
    }
  }
  else { /* startup returns failure */

  }
}
#endif 
