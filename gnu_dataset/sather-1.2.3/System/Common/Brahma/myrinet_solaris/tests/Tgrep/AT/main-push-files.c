/* Copyright (c) 1993, 1994  Ron Winacott                               */
/* This program may be used, copied, modified, and redistributed freely */
/* for ANY purpose, so long as this notice remains intact.              */

#define _REENTRANT

#include <stdio.h>
#include <string.h>
#include <stdlib.h> 
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <ctype.h> 
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/uio.h>
#include "at.h"
#include <synch.h>

#include "version.h"
#include "pmatch.h"
#include "debug.h" 
#include "alloca.h"

#include "at.h"
#include "brahma.h"
#include "migration.h"

#define MIG_DEBUG(format, args...)

#define PATH_MAX		1024 /* max # of characters in a path name */
#define HOLD_FDS                6  /* stdin,out,err and a buffer */
#define UNLIMITED               99999 /* The default tglimit */
#define MAXREGEXP               10  /* max number of -e options */

#define FB_BLOCK                0x00001
#define FC_COUNT                0x00002
#define FH_HOLDNAME             0x00004
#define FI_IGNCASE              0x00008
#define FL_NAMEONLY             0x00010
#define FN_NUMBER               0x00020
#define FS_NOERROR              0x00040
#define FV_REVERSE              0x00080
#define FW_WORD                 0x00100
#define FR_RECUR                0x00200
#define FU_UNSORT               0x00400
#define FX_STDIN                0x00800
/*
  #define TG_BATCH                0x01000
  #define TG_FILEPAT              0x02000 
*/
#define FE_REGEXP               0x04000
#define FC_LINE                 0x10000
/* #define TG_PROGRESS             0x20000 */

#define FILET                   1
#define DIRT                    2
#define ALPHASIZ        	128

/*
 * New data types 
 */

typedef struct work_st {
  char                *path;
  int                 tp;
  long                size;
  struct work_st      *next;
} work_t;

typedef struct out_st {
  char                *line;
  int                 line_count;
  long                byte_count;
  struct out_st       *next;
} out_t;

typedef struct bm_pattern {     /* Boyer - Moore pattern                */
  short            p_m;           /* length of pattern string     */
  short            p_r[ALPHASIZ]; /* "r" vector                   */
  short           *p_R;           /* "R" vector                   */
  char            *p_pat;         /* pattern string               */
} BM_PATTERN;


/*
 * Prototypes
 */
work_t* make_work(char *path,int tp);
void *cascade(void *arg);
void *search_thr(void * arg);


/* bmpmatch.c */
extern BM_PATTERN *bm_makepat(char *);
extern char *bm_pmatch(BM_PATTERN *, register char *);
extern void bm_freepat(BM_PATTERN *);
/* pmatch.c */
extern char *pmatch(register PATTERN *, register char *, int *);
extern PATTERN *makepat(char *string, char *);
extern void freepat(register PATTERN *);
extern void printpat(PATTERN *);

#include "proto.h"  /* function prototypes of main.c */

void *SigThread(void *arg);

/*
 * Global data
 */

BM_PATTERN      *bm_pat;  /* the global target read only after main */

PATTERN         *pm_pat[MAXREGEXP];  /* global targets read only for pmatch */


int     all_done = 0;
int     work_cnt = 0;
int     current_open_files = 0;
int     tglimit = UNLIMITED;    /* if -B limit the number of threads */


sigset_t set, oldset;


at_mutex_t output_print_lk;
at_sema_t end_sema;
/* output_print_lk used to print multi-line output only */
at_mutex_t load_balance_mx;
/* Used for critical section of load-balance computation (files pushed)*/

unsigned int    flags = 0;
int     regexp_cnt = 0;
char    *string[MAXREGEXP];
int     debug = 0;
int     use_pmatch = 0;





/*
 * Main: This is where the fun starts
 */

static int         use_now=1;

#define POLL()     if (use_now) BR_POLL();
#define END()      if (use_now) BR_exit();  else exit(1);
int HERE(){
  if(use_now)
    return BR_HERE();
  else
    return 0;
}


static int cluster_loads[4];

int
main(int argc, char **argv)
{
  int         c,out_thr_flags;
  long        max_open_files = 0l, ncpus = 0l;
  extern int  optind;
  extern char *optarg;
  int         prio = 0;
  void        *discard_return;
  void        *status;
  char        *e = NULL, *d = NULL; /* for debug flags */
  int         debug_file = 0;
  int         err = 0, i = 0, pm_file_len = 0;
  int         restart_cnt = 10;
  char        *nprocs_str;
  int         nprocs=0;

  flags = FR_RECUR;  /* the default */

  /* Initialize threads */
  nprocs_str = getenv("TGREP_PROCS");
  if(nprocs_str){
    nprocs = atoi(nprocs_str);
    use_now=0;
  }


  if(use_now){
    BR_init(3, argc, argv);
    BR_mig_stacks_init();
    BR_start();
    /* Initialize cluster loads */
    for(i=1; i<BR_CLUSTERS(); i++){
      cluster_loads[i] = 0;
    }
    /* Initially, cluster 0 has the smallest load */
    cluster_loads[0] = -1;
    BR_BARRIER();
  }
  else {
    at_init(nprocs, MIG_STACK_SIZE, 0);
    BR_mig_stacks_init();
  }
  /*thr_setprio(thr_self(),127);*/  /* set me up HIGH */
  while ((c = getopt(argc, argv, "d:e:bchilnsvwruf:p:BCSZzHP:")) != EOF) {
    switch (c) {
      /*   case 'B':
	   flags |= TG_BATCH;
	   if ((e = getenv("TGLIMIT"))) {
	   tglimit = atoi(e);
	   }
	   else {
	   at_fprintf(stderr,"env TGLIMIT not set, overriding -B\n");
	   flags &= ~TG_BATCH;
	   }
	   break;
	   */
    case 'b': flags |= FB_BLOCK;    break;
      /*    case 'c': flags |= FC_COUNT;    break; */
    case 'h': flags |= FH_HOLDNAME; break;
    case 'i': flags |= FI_IGNCASE;  break;
    case 'l': flags |= FL_NAMEONLY; break;
    case 'n': flags |= FN_NUMBER;   break;
      /*    case 's': flags |= FS_NOERROR;  break; */
    case 'v': flags |= FV_REVERSE;  break;
    case 'w': flags |= FW_WORD;     break;
    case 'r': flags &= ~FR_RECUR;   break;
    case 'C': flags |= FC_LINE;     break;
    case 'e':
      if (regexp_cnt == MAXREGEXP) {
	at_fprintf(stderr,"Max number of regexp's (%d) exceeded!\n",
		MAXREGEXP);
	END();
      }
      flags |= FE_REGEXP;
      if ((string[regexp_cnt] =(char *)at_malloc(strlen(optarg)+1))==NULL){
	at_fprintf(stderr,"tgrep: No space for search string(s)\n");
	END();
      }
      memset(string[regexp_cnt],0,strlen(optarg)+1);
      strcpy(string[regexp_cnt],optarg); 
      regexp_cnt++;
      break;
    case 'z':
    case 'Z': regexp_usage();
      break;
    case 'H':
    case '?':
    default : usage();
    }
  }

  if (!(flags & FE_REGEXP)) {
    if (argc - optind < 1) {
      at_fprintf(stderr,"tgrep: Must supply a search string(s) "
	      "and file list or directory\n");
      usage();
    }
    if ((string[0]=(char *)at_malloc(strlen(argv[optind])+1))==NULL){
      at_fprintf(stderr,"tgrep: No space for search string(s)\n");
      END();
    }
    memset(string[0],0,strlen(argv[optind])+1);
    strcpy(string[0],argv[optind]);
    regexp_cnt=1;
    optind++;
  }

  if (flags & FI_IGNCASE)
    for (i=0; i<regexp_cnt; i++)
      uncase(string[i]);

  if (flags & FE_REGEXP) {
    for (i=0; i<regexp_cnt; i++)
      pm_pat[i] = makepat(string[i],NULL);
   use_pmatch = 1;
  }
  else {
    bm_pat = bm_makepat(string[0]); /* only one allowed */
  }
    
  flags |= FX_STDIN;

  max_open_files = sysconf(_SC_OPEN_MAX);
  ncpus = sysconf(_SC_NPROCESSORS_ONLN);
  if ((max_open_files - HOLD_FDS - debug_file) < 1) {
    at_fprintf(stderr,"tgrep: You MUST have at lest ONE fd "
	    "that can be used, check limit (>10)\n");
    END();
  }
  /*  search_thr_limit = 
  cascade_thr_limit = search_thr_limit / 2; */
  /* the number of files that can by open */
  current_open_files = max_open_files - HOLD_FDS - debug_file;

  at_mutex_init(&output_print_lk);
  at_mutex_init(&load_balance_mx);
  at_sema_init(&end_sema, 0);
  /* At this point, the shared structures have been created. Only 
     Cluster 0 should continue to execute */
  BR_mig_start_time();
  if(use_now){
    if(HERE()==0){
      int i;

      real_main(optind,argc,argv);
      while(at_thread_count()>1){
	POLL();
	at_yield();
      }

      /* Wait for other clusters */
      for(i=1; i<BR_CLUSTERS(); i++){
	at_sema_wait(&end_sema);
      }
      BR_mig_finish_time();
      END();      
    }
    else {
      /* First, wait until get a new directory */
      while(at_thread_count()==1){
	BR_POLL();
      }
      /* Now wait until done locally */
      while(at_thread_count()>1){
	POLL();
	at_yield();
      } 
      /* Send an ack to cluster 0 */
      BR_REQUEST_1(0, BR_signal_handler, (BR_word_t)&end_sema);
    }
  }
  else {
    real_main(optind,argc,argv);
    at_join_all();
    while(1){
      if (at_thread_count() == 1) {
	BR_mig_finish_time();
	END();
      } 
    }
  }    
}

/* Handle the startup of the main thread - fork off threads for the files
 and directories mentioned at the top level */
int
real_main(int optind, int argc, char **argv)
{
  work_t     *new_work;
  void       *discard_return;
  int         restart_cnt = 10;
  at_thread_t   *tid;
  struct stat sbuf;


  if ((argc == optind) && (flags & FR_RECUR)) {
    /* If there is no file specfied and assuming a recusive structure, use . */
    new_work = make_work(".",DIRT);
    tid = at_create_1(at_get_focus(),AT_UNBOUND,(at_userf_1_t *)cascade,(at_word_t)new_work);
    /* discard_return = cascade(new_work); */
    flags = (flags & ~FX_STDIN);
  }

    /* Handle the remaining stuff on the command line (filenames) */
  for ( ; optind < argc; optind++) {
    restart_cnt = 10;
    flags = (flags & ~FX_STDIN);
  STAT_AGAIN:
    AT_KERNEL_LOCK;
    if (stat(argv[optind], &sbuf)) {
      if (errno == EINTR) { /* try again !, restart */
	if (--restart_cnt){
	  AT_KERNEL_UNLOCK;
	  goto STAT_AGAIN;
	}
      }
      AT_KERNEL_UNLOCK;
      at_fprintf(stderr,"tgrep: Can't stat file/dir %s, %s\n", 
	      argv[optind], strerror(errno));         
      continue;
    }
    AT_KERNEL_UNLOCK;
    switch (sbuf.st_mode & S_IFMT) {
    case S_IFREG :
      new_work = make_work(argv[optind],FILET);
      new_work->size = sbuf.st_size;
      tid = at_create_1(at_get_focus(),AT_UNBOUND,(at_userf_1_t *)search_thr,(at_word_t)new_work);
    /*       discard_return = search_thr(new_work); */
      break;
    case S_IFDIR :
      if (flags & FR_RECUR) {
	new_work = make_work(argv[optind],DIRT);
	tid = at_create_1(at_get_focus(),AT_UNBOUND,(at_userf_1_t *)cascade,(at_word_t)new_work);
      /*	discard_return = cascade(new_work); */
      }
      else {
	at_fprintf(stderr,"tgrep: Can't search directory %s, "
		"-r option is on. Directory ignored.\n",
		argv[optind]);
      }
      break;
    }
  } 

  /*    thr_setconcurrency(3);*/

  if (flags & FX_STDIN) {
    /* Set by default, then unset if there is any file specified (or recursive) */
    at_fprintf(stderr,"tgrep: stdin option is not coded at this time\n");
    END();                        /* XXX Need to fix this SOON */
  }

OUT:
  /* we are done, print the stuff. All other threads ar parked */
  /*   return(0); */

/* should have a return from main */
}


work_t*
make_work(char *path,int tp)
{
    work_t      *wt;

    if ((wt = (work_t *)at_malloc(sizeof(work_t))) == NULL){
      at_fprintf(stderr,"tgrep: Could not at_malloc work\n");
    }
    if ((wt->path = (char *)at_malloc(strlen(path)+1)) == NULL){
      at_fprintf(stderr,"tgrep: Could not malloc path\n");
    }
    AT_KERNEL_LOCK;
    strcpy(wt->path,path);
    AT_KERNEL_UNLOCK;
    wt->tp = tp;
    wt->next = NULL;
    return wt;
}
  

void at_allow_migrate(){
  BR_cluster_t origin;

  origin = HERE();
  at_mig_enable();
  at_yield();
  POLL();
  /*  if (HERE() != origin){
    at_printf("THREAD MIGRATED FROM %d to %d\n", origin, HERE());
  }*/
  /* Well, now disallow migration */
  at_mig_disable();
}

static int index=-1;


void push_and_balance(work_t* wt){
  BR_cluster_t origin;
  int i, minload, dest;
  float coeff = 0.8;

  if(use_now){
    origin = HERE();
    POLL();
    
    /* Find a cluster with the smallest load and push this thread
       there ... */
    at_mutex_lock(&load_balance_mx);
    minload = cluster_loads[0]*coeff;
    dest = 0;
    
    for(i=1; i<BR_CLUSTERS()-1; i++){
      if(cluster_loads[i] < minload){
	minload = cluster_loads[i];
	dest = i;
      }
    }
    
    /* Update the loads */
    cluster_loads[dest] += wt->size;
    at_mutex_unlock(&load_balance_mx);
    
    /* Now push to the destination */
    if(dest!=HERE()){
      at_mig_enable();
      BR_push_self_to(dest);
    }
    
    /*  if (HERE() != origin){
	at_printf("THREAD MIGRATED FROM %d to %d\n", origin, HERE());
	}*/
    /* Well, now disallow migration */
    at_mig_disable();
    POLL();
  }
}


void smart_push(work_t* wt){
  BR_cluster_t origin;
  int dest;
  char *new_path;

  if(use_now){
    origin = HERE();
    POLL();
    
    dest = 0;
    /* Find a cluster with the smallest load and push this thread
       there ... */
    /* Parse the path and push to the cluster on which the data 
       lives */
    if(strstr(wt->path, "samosa")){
      dest = 1;  /* samosa is Cluster 1*/
    }
    else {
      if(strstr(wt->path, "icsib18")){
	dest = 2; /* icsib18 is Cluster 2 */
      }
    }

    if(dest!=0){
      at_mig_enable();
      BR_push_self_to(dest);
    }
    
    if (HERE() != origin){
      at_printf("THREAD MIGRATED FROM %d to %d: %s\n", origin, HERE(), wt->path);
      /* Convert the path to what it should be */
      /* Hack, sigh */
      wt->path = "/xa/borisv/Sather/System/Common/Brahma";
    }
    /* Well, now disallow migration */
    at_mig_disable();
    
    POLL();
  }
}



/*
 * Search thread: Started by the main thread when a file name is found
 * on the work Q to be serached. If all the needed resources are ready
 * a new search thread will be created.
 */
void *
search_thr(void *arg) /* work_t *arg */
{    
  FILE        *fin;
  char        *fin_buf;
  work_t      *wt,std;
  int         line_count;
  char        *rline;
  char        *cline;
  char        *line;
  register char *p,*pp;
  int            pm_len;
  int         len = 0;
  long        byte_count;
  long        next_line;
  int         show_line;  /* for the -v option */
  register int slen,plen,i;
  out_t       *out = NULL;    /* this threads output list */
  work_t      *arg_work;
  int         r;

  arg_work = (work_t *)arg;
  
  /* First copy the argument data into local space */
  if ((wt = (work_t *)alloca(sizeof(work_t))) == NULL)
    at_fprintf(stderr,"Could not alloca for cascade work");
  if ((wt->path = (char *)alloca(strlen(arg_work->path)+1)) == NULL)
    at_fprintf(stderr,"Could not alloca for cascade path");
  strcpy(wt->path,arg_work->path);
  wt->tp = arg_work->tp;
  wt->next = NULL;
  wt->size = arg_work->size;

  /* May not be local after the migrate. Must free now */
  at_free(arg_work->path);
  at_free(arg_work);

  /*-----------------------------------------------------------------*/
  /* Then the thread may be migrated */
  /*  smart_push(wt);*/

  /* We may be on a different cluster now! */
  fin_buf = (char *)at_malloc(BUFSIZ*4);  /* 4 Kbytes */
  rline = (char *)at_malloc(128);
  cline = (char *)at_malloc(128);

  /* len = strlen(string);*/  /* only set on first pass */
    
  /* init all back to zero */
  line_count = 0;
  byte_count = 0l;
  next_line = 0l;
  show_line = 0;

  /*  at_mutex_lock(&work_q_lk); tglimit--;  at_mutex_unlock(&work_q_lk); */
  DP(DLEVEL5,("searching file (STDIO) %s\n",wt->path));

  /* Need to lock the kernel at some point */
  /*--------------------------------------------------------*/
  AT_KERNEL_LOCK;
  fin = fopen(wt->path,"r");
  AT_KERNEL_UNLOCK;

  if (fin == NULL) {
    at_fprintf(stderr,"tgrep: %s. File \"%s\" not searched.\n",
	    strerror(errno),wt->path);
    goto ERROR;
  }
  AT_KERNEL_LOCK;
  setvbuf(fin,fin_buf,_IOFBF,(BUFSIZ*4));  /* XXX */
  AT_KERNEL_UNLOCK;

  DP(DLEVEL5,("Search thread has opened file %s\n",wt->path));
  while ((fgets(rline,127,fin)) != NULL) {
    slen = strlen(rline);
    next_line += slen;  
    line_count++;
    if (rline[slen-1] == '\n')
      rline[slen-1] = '\0';
    /*
    ** If the uncase flag is set, copy the read in line (rline)
    ** To the uncase line (cline) Set the line pointer to point at
    ** cline.
    ** If the case flag is NOT set, then point line at rline.
    ** line is what is compared, rline is waht is printed on a 
    ** match.
    */
    if (flags & FI_IGNCASE) {
      strcpy(cline,rline);
      uncase(cline);
      line = cline;
    }
    else {
      line = rline;
    }
    show_line = 1;  /* assume no match, if -v set */
    /* The old code removed */
    if (use_pmatch) {
      for (i=0; i<regexp_cnt; i++) {
	if (pmatch(pm_pat[i], line, &pm_len)) {
	  if (!(flags & FV_REVERSE)) {
	    add_output_local(&out,wt,line_count,
			     byte_count,rline);
	    continue_line(rline,fin,out,wt,
			  &line_count,&byte_count);
	  }
	  else {
	    show_line = 0; 
	  } /* end of if -v flag if / else block */
	  /* 
	  ** if we get here on ANY of the regexp targets
	  ** jump out of the loop, we found a single
	  ** match so, do not keep looking!
	  ** If name only, do not keep searcthing the same 
	  ** file, we found a single match, so close the file, 
	  ** print the file name and move on to the next file.
	  */
	  if (flags & FL_NAMEONLY) 
	    goto OUT_OF_LOOP;
	  else
	    goto OUT_AND_DONE;
	} /* end found a match if block */
      } /* end of the for pat[s] loop */
    }
    else {
      if (bm_pmatch( bm_pat, line)) {
	if (!(flags & FV_REVERSE)) {
	  add_output_local(&out,wt,line_count,byte_count,rline);
	  continue_line(rline,fin,out,wt,
			&line_count,&byte_count);
	}
	else {
	  show_line = 0; 
	}
	if (flags & FL_NAMEONLY) 
	  goto OUT_OF_LOOP;
      }
    }
  OUT_AND_DONE:
    if ((flags & FV_REVERSE) && show_line) { 
      add_output_local(&out,wt,line_count,byte_count,rline);
      show_line = 0;
    }
    byte_count = next_line;
  }
OUT_OF_LOOP:
  fclose(fin);
  /*
    ** The search part is done, but before we give back the FD,
    ** and park this thread in the search thread pool, print the
    ** local output we have gathered.
    */
  print_local_output(out,wt);  /* this also frees out nodes */
  out = NULL; /* for the next time around, if there is one */
ERROR:
  DP(DLEVEL5,("Search done for %s\n",wt->path));


}

/*
 * Continue line: Speacial case search with the -C flag set. If you are 
 * searching files like Makefiles, some lines may have escape char's to
 * contine the line on the next line. So the target string can be found, but 
 * no data is displayed. This function continues to print the escaped line
 * until there are no more "\" chars found.
 */
int
continue_line(char *rline, FILE *fin, out_t *out, work_t *wt, 
	      int *lc, long *bc)
{
  int len;
  int cnt = 0;
  char *line;
  char nline[128];

  if (!(flags & FC_LINE))
    return(0);

  line = rline;
AGAIN:
  len = strlen(line);
  if (line[len-1] == '\\') {
    if ((fgets(nline,127,fin)) == NULL) {
      return(cnt);
    }
    line = nline;
    len = strlen(line);
    if (line[len-1] == '\n')
      line[len-1] = '\0';
    *bc = *bc + len;
    *lc++;
    add_output_local(&out,wt,*lc,*bc,line);
    cnt++;
    goto AGAIN;
  }
  return(cnt);
}

/*
 * cascade: This thread is started by the main thread when directory names
 * are found on the work Q. The thread reads all the new file, and directory
 * names from the directory it was started when and adds the names to the 
 * work Q. (it finds more work!)
 */
void *
cascade(void *arg)  /* work_t *arg */
{
  char        *fullpath;
  int         restart_cnt = 10;
  DIR         *dp;
  work_t      *new_work;
  char        *dir_buf;
  struct dirent *dent;
  struct stat   sbuf;
  char        *fpath;
  work_t      *wt;
  int         fl = 0, dl = 0;
  int         pm_file_len = 0;
  void *      discard_return;
  struct dirent *tde;
  at_thread_t   *tid;
  work_t      *arg_work;
  int origin;

  arg_work = (work_t *)arg;

  /* First copy the argument data into local space */
  if ((wt = (work_t *)alloca(sizeof(work_t))) == NULL)
    at_fprintf(stderr,"Could not alloca for cascade work");
  if ((wt->path = (char *)alloca(strlen(arg_work->path)+1)) == NULL)
    at_fprintf(stderr,"Could not alloca for cascade path");
  strcpy(wt->path,arg_work->path);
  wt->tp = arg_work->tp;
  wt->next = NULL;

  /* May not be local after the migrate. Must free now */
  at_free(arg_work->path);
  at_free(arg_work);

  origin = HERE();
  /*--------------------------------------------------------------------*/
  /* Then the thread may be migrated */
  smart_push(wt);
  POLL();

  /* Safe to allocate stuff now */
  fullpath = (char *)at_malloc(1025);
  dir_buf = (char *)at_malloc(sizeof(struct dirent) + PATH_MAX);
  dent = (struct dirent *)dir_buf;

  /*thr_setprio(thr_self(),64);  set search to middle */
  /* at_yield();   try toi give control back to main thread */


  fl = 0;
  dl = 0;
  restart_cnt = 10;
  pm_file_len = 0;

  /* at_mutex_lock(&work_q_lk); tglimit--;  at_mutex_unlock(&work_q_lk); */

  if (!wt) {
    at_fprintf(stderr,"tgrep: Bad work node passed to cascade\n");
    goto DONE;
  }
  fpath = (char *)wt->path;
  if (!fpath) {
    at_fprintf(stderr,"tgrep: Bad path name passed to cascade\n");
    goto DONE;
  }
  DP(DLEVEL3,("Cascading on %s\n",fpath));
  AT_KERNEL_LOCK;
  dp = opendir(fpath);
  AT_KERNEL_UNLOCK;
    
  if (dp == NULL) {
    at_printf("tgrep: Can't open dir %s, %s. Ignored.\n",
	      fpath,strerror(errno));
    goto DONE;
  }
  while(1){
    AT_KERNEL_LOCK;
    tde = readdir_r(dp,dent);
    AT_KERNEL_UNLOCK;
    if(tde==NULL) break;
	  
    restart_cnt = 10;  /* only try to restart the interupted 10 X */
	    
    if (dent->d_name[0] == '.') {
      if (dent->d_name[1] == '.' && dent->d_name[2] == '\0') 
	continue;
      if (dent->d_name[1] == '\0')
	continue;
    }

    fl = strlen(fpath);
    dl = strlen(dent->d_name);
    if ((fl + 1 + dl) > 1024) {
      at_fprintf(stderr,"tgrep: Path %s/%s is too long. "
	      "MaxPath = 1024\n",
	      fpath, dent->d_name);
      continue;  /* try the next name in this directory */
    }
    AT_KERNEL_LOCK;
    strcpy(fullpath,fpath);
    strcat(fullpath,"/");
    strcat(fullpath,dent->d_name);
    assert(fullpath[0]!=0);
    AT_KERNEL_UNLOCK;

  RESTART_STAT:
    AT_KERNEL_LOCK;
    if (stat(fullpath,&sbuf)) {
      if (errno == EINTR) {
	if (--restart_cnt){
	  AT_KERNEL_UNLOCK;
	  goto RESTART_STAT;
	}
      }
      AT_KERNEL_UNLOCK;

      at_fprintf(stderr,"tgrep: Can't stat file/dir %s, %s. "
	      "Ignored.\n",
	      fullpath,strerror(errno));
      goto ERROR;
    }
    AT_KERNEL_UNLOCK;
    
    switch (sbuf.st_mode & S_IFMT) {
    case S_IFREG :
      new_work = make_work(fullpath,FILET);
      new_work->size = sbuf.st_size;      
      tid = at_create_1(at_get_focus(),AT_UNBOUND,(at_userf_1_t *)search_thr,(at_word_t)new_work);
      /* discard_return = search_thr(new_work); */
      DP(DLEVEL3,("cascade added file (MATCH) %s to Work Q\n",
		  fullpath));
      break;
    case S_IFDIR :
      MIG_DEBUG("cascade dir: %s\n",fullpath);
      new_work = make_work(fullpath,DIRT);
      tid = at_create_1(at_mig_bundle,AT_UNBOUND,(at_userf_1_t *)cascade,(at_word_t)new_work);
      /*      discard_return = cascade(new_work); */
      break;
    }
  }

ERROR:
  AT_KERNEL_LOCK;
  closedir(dp);
  AT_KERNEL_UNLOCK;
DONE:

}

/*
 * Print Local Output: Called by the search thread after it is done searching
 * a single file. If any oputput was saved (matching lines), the lines are 
 * displayed as a group on stdout. 
 */
int
print_local_output(out_t *out, work_t *wt)
{
  out_t       *pp, *op;
  int         out_count = 0;
  int         printed = 0;
  int 	print_name = 1;

  pp = out;
  at_mutex_lock(&output_print_lk);
  while (pp) {
    out_count++;
    if (!(flags & FC_COUNT)) {
      if (flags & FL_NAMEONLY) {  /* Pint name ONLY ! */
	if (!printed) {
	  printed = 1;
	  at_printf("%s\n",wt->path);
	}
      }
      else {  /* We are printing more then just the name */
	if (!(flags & FH_HOLDNAME))  /* do not print name ? */
	  at_printf("%s :",wt->path);
	if (flags & FB_BLOCK)
	  at_printf("%ld:",pp->byte_count/512+1);
	if (flags & FN_NUMBER)
	  at_printf("%d:",pp->line_count);
	at_printf("%s\n",pp->line);
      }
    }
    op = pp;
    pp = pp->next;
    /* free the nodes as we go down the list */
    at_free(op->line);
    at_free(op);
  }
  at_mutex_unlock(&output_print_lk);
  return(0);
}

/*
 * add output local: is called by a search thread as it finds matching lines. 
 * the matching line, it's byte offset, line count, etc are stored until the
 * search thread is done searching the file, then the lines are printed as 
 * a group. This way the lines from more then a single file are not mixed
 * together.
 */
int
add_output_local(out_t **out, work_t *wt,int lc, long bc, char *line)
{
  out_t       *ot,*oo, *op;

  if (( ot = (out_t *)at_malloc(sizeof(out_t))) == NULL)
    goto ERROR;
  if (( ot->line = (char *)at_malloc(strlen(line)+1)) == NULL)
    goto ERROR;

  strcpy(ot->line,line);
  ot->line_count = lc;
  ot->byte_count = bc;
    
  if (!*out) {
    *out = ot;
    ot->next = NULL;
    return(0);
  }
  /* append to the END of the list, keep things sorted! */
  op = oo = *out;    
  while(oo) {
    op = oo;
    oo = oo->next;
  }
  op->next = ot;
  ot->next = NULL;
  return(0);
ERROR:
  at_fprintf(stderr,"tgrep: Output lost. No space. "
	  "[%s: line %d byte %d match : %s\n",
	  wt->path,lc,bc,line);
  return(1);
}

/*
 * uncase: A glue function. If the -i (case insensitive) flag is set, the
 * target strng and the read in line is converted to lower case before
 * comparing them.
 */
void
uncase(char *s)
{
  char        *p;

  for (p = s; *p != 0; p++)
    *p = (char)tolower(*p);
}


/*
 * SigThread: if the -S option is set, the first ^C set to tgrep will
 * print the stats on the fly, the second will kill the process.
 */

void *
SigThread(void *arg)
{
  int sig;
  int stats_printed = 0;

  while (1) {
    sig = sigwait(&set);
    DP(DLEVEL7,("Signal %d caught\n",sig));
    switch (sig) {
    case -1:
      at_fprintf(stderr,"Signal error\n");
      break;
    case SIGINT:
      if (stats_printed)
	END();
      stats_printed = 1;
      break;
    case SIGHUP:
      break;
    default:
      DP(DLEVEL7,("Default action taken (exit) for signal %d\n",sig));
      END();  /* default action */
    }
  }
}


/*
 * usage: Have to have one of these.
 */
void 
usage(void)
{
  at_fprintf(stderr,"usage: tgrep <options> pattern <{file,dir}>...\n");
  at_fprintf(stderr,"\n");
  at_fprintf(stderr,"Where:\n");
#ifdef DEBUG    
  at_fprintf(stderr,"Debug     -d = debug level -d <levels> (-d0 for usage)\n");
  at_fprintf(stderr,"Debug     -f = block fd's from use (-f #)\n");
#endif    
  at_fprintf(stderr,"          -b = show block count (512 byte block)\n");
  at_fprintf(stderr,"          -c = print only a line count\n");
  at_fprintf(stderr,"          -h = do not print file names\n");
  at_fprintf(stderr,"          -i = case insensitive\n");
  at_fprintf(stderr,"          -l = print file name only\n");
  at_fprintf(stderr,"          -n = print the line number with the line\n");
  at_fprintf(stderr,"          -s = Suppress error messages\n");
  at_fprintf(stderr,"          -v = print all but matching lines\n");
#ifdef NOT_IMP    
  at_fprintf(stderr,"          -w = search for a \"word\"\n");
#endif    
  at_fprintf(stderr,"          -r = Do not search for files in all "
	  "sub-directories\n");
  at_fprintf(stderr,"          -C = show continued lines (\"\\\")\n");
  at_fprintf(stderr,"          -p = File name regexp pattern. (Quote it)\n");
  at_fprintf(stderr,"          -e = expression search.(regexp) More then one\n");
  at_fprintf(stderr,"          -B = limit the number of threads to TGLIMIT\n");
  at_fprintf(stderr,"          -S = Print thread stats when done.\n");
  at_fprintf(stderr,"          -Z = Print help on the regexp used.\n");
  at_fprintf(stderr,"\n");
  at_fprintf(stderr,"Notes:\n");
  at_fprintf(stderr,"      If you start tgrep with only a directory name\n");
  at_fprintf(stderr,"      and no file names, you must not have the -r option\n");
  at_fprintf(stderr,"      set or you will get no output.\n");
  at_fprintf(stderr,"      To search stdin (piped input), you must set -r\n");
  at_fprintf(stderr,"      Tgrep will search ALL files in ALL \n");
  at_fprintf(stderr,"      sub-directories. (like */* */*/* */*/*/* etc..)\n");
  at_fprintf(stderr,"      if you supply a directory name.\n");
  at_fprintf(stderr,"      If you do not supply a file, or directory name,\n");
  at_fprintf(stderr,"      and the -r option is not set, the current \n");
  at_fprintf(stderr,"      directory \".\" will be used.\n");
  at_fprintf(stderr,"      All the other options should work \"like\" grep\n");
  at_fprintf(stderr,"      The -p patten is regexp, tgrep will search only\n");
  at_fprintf(stderr,"      the file names that match the patten\n");
  at_fprintf(stderr,"\n");
  at_fprintf(stderr,"      Tgrep Version %s\n",Tgrep_Version);
  at_fprintf(stderr,"\n");
  at_fprintf(stderr,"      Copy Right By Ron Winacott, 1993-1995.\n");
  at_fprintf(stderr,"\n");
  exit(0);
}

/*
 * regexp usage: Tell the world about tgrep custom (THREAD SAFE) regexp!
 */
int 
regexp_usage (void)
{
  at_fprintf(stderr,"usage: tgrep <options> -e \"pattern\" <-e ...> "
	  "<{file,dir}>...\n");
  at_fprintf(stderr,"\n");
  at_fprintf(stderr,"metachars:\n");
  at_fprintf(stderr,"    . - match any character\n");
  at_fprintf(stderr,"    * - match 0 or more occurrences of pervious char\n");
  at_fprintf(stderr,"    + - match 1 or more occurrences of pervious char.\n");
  at_fprintf(stderr,"    ^ - match at begining of string\n");
  at_fprintf(stderr,"    $ - match end of string\n");
  at_fprintf(stderr,"    [ - start of character class\n");
  at_fprintf(stderr,"    ] - end of character class\n");
  at_fprintf(stderr,"    ( - start of a new pattern\n");
  at_fprintf(stderr,"    ) - end of a new pattern\n");
  at_fprintf(stderr,"    @(n)c - match <c> at column <n>\n");
  at_fprintf(stderr,"    | - match either pattern\n");
  at_fprintf(stderr,"    \\ - escape any special characters\n");
  at_fprintf(stderr,"    \\c - escape any special characters\n");
  at_fprintf(stderr,"    \\o - turn on any special characters\n");
  at_fprintf(stderr,"\n");
  at_fprintf(stderr,"To match two diffrerent patterns in the same command\n");
  at_fprintf(stderr,"Use the or function. \n"
	  "ie: tgrep -e \"(pat1)|(pat2)\" file\n"
	  "This will match any line with \"pat1\" or \"pat2\" in it.\n");
  at_fprintf(stderr,"You can also use up to %d -e expresions\n",MAXREGEXP);
  at_fprintf(stderr,"RegExp Pattern matching brought to you by Marc Staveley\n");
  END();
}


