/*
*  Multithreaded Demo Source
* 
*  Copyright (C) 1995 by Sun Microsystems, Inc.
*  All rights reserved.
* 
*  This file is a product of SunSoft, Inc. and is provided for
*  unrestricted use provided that this legend is included on all
*  media and as a part of the software program in whole or part.
*  Users may copy, modify or distribute this file at will.
* 
*  THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING
*  THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
*  PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
* 
*  This file is provided with no support and without any obligation on the
*  part of SunSoft, Inc. to assist in its use, correction, modification or
*  enhancement.
* 
*  SUNSOFT AND SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT
*  TO THE INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
*  FILE OR ANY PART THEREOF.
* 
*  IN NO EVENT WILL SUNSOFT OR SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY
*  LOST REVENUE OR PROFITS OR OTHER SPECIAL, INDIRECT AND CONSEQUENTIAL
*  DAMAGES, EVEN IF THEY HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH
*  DAMAGES.
* 
*  SunSoft, Inc.
*  2550 Garcia Avenue
*  Mountain View, California  94043
*/

#define TRY(aaa,bbb) { bbb = aaa; \
         if (bbb == -1) {    \
	 printf("whoops at %d : %d\n",__LINE__,errno);  \
         exit(1); }       \
	}

/* 
 * TEST PROBLEM: simple-minded parallel and recursive quicksort using pthreads
 *						-- Resii & cdq
 *						-- April 9, 1991
 */

#include <math.h>
#include <at.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdlib.h>
#include <assert.h>

#include "brahma.h"
#include "migration.h"

#define FAIL	1			/* Verification failed */
#define MINSIZE 100 			/* Cut-over size for bubble-sort */
#define PASS	0			/* Verification passed */
#define SIZE	200000 	                /* Size of array to sort */
#define SIZEBY4	SIZE/4			/* Quarter size of array */

/*
#define REC_LT(a,b) (((a)->id) < ((b)->id))
#define REC_LEQ(a,b) (((a)->id) <= ((b)->id))
#define REC_GT(a,b) (((a)->id) > ((b)->id))
*/
#define REC_LT(a,b) (((a).id) < ((b).id))
#define REC_LEQ(a,b) (((a).id) <= ((b).id))
#define REC_GT(a,b) (((a).id) > ((b).id))

typedef struct record {
  /*  struct record *next;
  char stuff[64];*/
  float id;
} record_t;

void qsortr(record_t recs[],int first,int last,at_mutex_t *done, 
	    int level, int src);

int max_level;

at_bundle_t *bundle;

void report_usage();

/***********
 *  Quicksort driver:  initialize, sort, and verify.
 */


record_t  records[SIZE];		/* Array to be sorted */
int size;
int minsize;



#define at_lff_share(x,y,z)
#define at_lff_flush(x)



void at_flush(at_bundle_t *b){
  at_lff_flush(b);
}

void records_create()
{
  int i;
  for (i=size-1; i>=0; i--) {
    /*records[i] = (record_t)at_malloc(sizeof(struct record));*/
    records[i].id= rand()*size;
  }   

}

void records_rand(){
  int i, j;
  record_t trec;
  
  /* Randomize the things */
  for(i=0; i<size; i++){
    j = (int) (drand48()*size);
    trec = records[i];
    records[i] = records[j];
    records[j] = trec;
  }
}
   
void main(int argc, char **argv)
{
   int i, j;	
   int check=PASS;			/* Verify failuer flag */
   at_mutex_t *mutex;
   int concurrency = 0;
   hrtime_t start, finish;
   /*clock_t start, finish;   */
   char *filenm=NULL;
   FILE *fp;
   record_t trec;
   at_thread_t *t;
   int misses; 

   size = SIZE;
   minsize = MINSIZE;

   if(argc>1){
     concurrency = atoi(argv[1]);
   }

   if(argc>2){
     size = atoi(argv[2]);
   }

   if(argc>3){
     minsize = atoi(argv[3]);
   }

   if(argc>4){
     filenm = argv[4];
     fp = fopen(filenm, "a+");
   }
   else{
     fp = stderr;
   }


  BR_init(3, argc, argv);
  BR_start();
  /* Initialize stacks for use by migration bundles */
  BR_BARRIER();

  BR_mig_stacks_init();

  /*at_init(concurrency, 0x2000, 0);*/

  if(BR_HERE()==0){
    records_create();
    
    records_rand();
    
    
    at_printf("at_init() finished\n");
    at_printf("DATA SIZE: %dK\n", sizeof(struct record)*size/1024);
  }
  
  /* Make sure we do not start before the data is initialized */
  BR_BARRIER();

  if(BR_HERE()==0){

    /*----------------------------------------------------------------------*/
    start=gethrtime();
    /*   start = clock(); */
    mutex = at_mutex_create();
    at_mutex_lock(mutex);
    

     t = at_create_6(at_mig_bundle, AT_MIG_DISABLE, 
		     (at_userf_6_t *)qsortr, (at_word_t)records,
		     0,size-1,(at_word_t)mutex,0, (at_word_t)BR_HERE());

    at_mutex_lock(mutex);
    finish = gethrtime();
    printf("time in us %e\n", (double)(finish-start)/1e9);   
    /*-----------------------------------------------------------------------*/

    /* Now, do it for real */
     /*----------------------------------------------------------------------*/
    records_rand();

     start=gethrtime();
     mutex = at_mutex_create();
     at_mutex_lock(mutex);
     
     t = at_create_6(at_mig_bundle, AT_MIG_DISABLE, 
		     (at_userf_6_t *)qsortr, (at_word_t)records,
		     0,size-1,(at_word_t)mutex,0, (at_word_t)BR_HERE());
     at_mutex_lock(mutex);
     finish=gethrtime();
     /*-----------------------------------------------------------------------*/
     

     
     /* Verify the results */
     for (i=1; i<size; i++) {
       if (REC_LT(records[i],records[i-1])) {
	 printf("INVALID SORT: records[%d]=%e, records[%d]=%e\n",i-1,records[i-1].id,i,records[i].id);
	 check=FAIL;
       }
     }
     /*   printf("\nSorted array is:\n");
	  for (i=0; i<SIZEBY4; i++) {
	  printf("%13d %13d %13d %13d\n",
	  a[i],a[i+SIZEBY4],a[i+(2*SIZEBY4)],a[i+(3*SIZEBY4)]);
	  }*/
     
     if (check == PASS) {
       printf("\nqsort worked.\n");
     }
     else {
       printf("\nqsort test failed!\n");
       /*exit(1);*/
     }
     printf("time in us %e\n", (double)(finish-start)/1e9);
     printf("max level: %d\n", max_level);
     fprintf(fp, "%d   %d   %d   %e    %d    %dK\n", concurrency, size, minsize,  
	     (double)(finish-start)/(1e9), at_thread_count(),max_mem_usage/1024);
     fclose(fp);
   }
   else {
     /* Steal some stuff */
     /*qsort_steal_term_loop();*/
     BR_steal_term_loop();
   }

   

   if(BR_HERE()==0){
     BR_exit();
   }
}

int qsort_steal_term_loop(){
  while(1){
    /* Entering a service section */
    BR_inc_service_forks();
    while(1){
      while((at_thread_count()-BR_service_forks())>=BR_PROCESSORS()){
	BR_POLL();
	at_yield();
      }
      
      BR_steal_from(0);
      BR_POLL();
      at_yield();
    }    
  }
}

/*
int list_size(record_t r){
  int i;

  i = 0;
  while(r){
    i++;
    r=r->next;
  }
  return i;
}

*/
void insertion(record_t *recs, int first, int last){
  record_t res;
  int i, j, s;
  record_t c, p;
  record_t v, n;

  /* Get one element */
  /*  res = recs[first];
      res->next=0;  
      
      for(i=first+1; i<=last; i++){
      n = recs[i];
      p = c = res;
      
      while(c && REC_GT(n,c)){
      p = c;
      c = c->next;
      }
      if(p==c){
      n->next = res;
      res = n;
      }
      else{
      n->next = c;
      p->next = n;
      }
      }
      s = list_size(res);
      
      if(s!=(last-first+1)){
      abort();
      }
      c = res;
      i = first;
      while(c){
      recs[i] = c;
      c=c->next;
      i++;
      }
      */

  for(i=first+1; i<=last; i++){
    v = recs[i];
    j = i;
    while( (j>first) && REC_GT(recs[j-1], v)){
      recs[j] = recs[j-1];
      j--;
    }
    recs[j]=v;
  }
}


static void bulk_get_reply_h(BR_cluster_t src,caddr_t buf, size_t len, 
			     caddr_t a)
{
  at_mutex_t *m;

  m = (at_mutex_t *)a;
  at_mutex_unlock(m);
} 

static void bulk_null_h(BR_cluster_t src,caddr_t buf, size_t len, 
			    caddr_t a){
}

static void bulk_get(BR_cluster_t node,caddr_t l_buf,caddr_t r_buf,size_t len)
{
  int i; 
  at_mutex_t m;
  int max_pkt;

  max_pkt = BR_MAX_XFER();

  at_mutex_init(&m);
  at_mutex_lock(&m);

  if (len <= max_pkt) { /* fast check, most often < max_pkt */
    BR_GET(node,r_buf,l_buf,len,bulk_get_reply_h,(caddr_t)&m);
  }
  else {   /* chunk the data into maximum packet sizes */
    for (i=max_pkt;i<len;i+=max_pkt) {
      BR_GET(node,r_buf,l_buf,max_pkt,bulk_null_h,NULL);
      (char *) l_buf+= max_pkt; (char *) r_buf+= max_pkt;
    }
    i-=max_pkt;
    /*get the last packet */
    BR_GET(node,r_buf,l_buf,len-i,bulk_get_reply_h,(caddr_t)&m);
  }
  
  /* Wait, until done */
  at_mutex_lock(&m);
} 


void BR_mem_unlock_handler(BR_cluster_t c, caddr_t to, size_t size, caddr_t a){
  at_mutex_t *m;

  m = (at_mutex_t *)a;
  at_mutex_unlock(m);
  return;
}
void copy_in(record_t *recs, int first, int last, int src){
  caddr_t addr;
  size_t s;
  
  if(BR_HERE()==src) return;

  /*at_printf("copy_in [%d] -> [%d]  %x  %d  %d\n", src, BR_HERE(),
	    recs, first, last);*/
  /* Ok, need to copy data from source node */

  addr = (caddr_t) &recs[first];
  s = (last-first+1)*sizeof(record_t);
  bulk_get(src, addr, addr, s);
}

void copy_out(record_t *recs, int first, int last, int src){
  caddr_t addr;
  size_t s;
  
  if(BR_HERE()==src) return;


  /*at_printf("copy_out [%d] <- [%d]  %x  %d  %d\n", src, BR_HERE(),
	    recs, first, last); */
  /* Ok, need to copy data to the node where it belongs */

  addr = (caddr_t) &recs[first];
  s = (last-first+1)*sizeof(record_t);
  BR_STORE(src, addr, addr, s, bulk_null_h, NULL);
}



/***********
 *  Quick sort routine; parallel and recursive.
 */
/* int * d;				        array to be sorted */
/* int first,last;				element range to sort */

void qsortr(record_t *recs,int first,int last,at_mutex_t *done, 
	    int level, int src)
{
   int lastpos;
   record_t temp;
   int swap;
   record_t pivot;
   int lesspos;
   int morepos;
   int i;
   at_mutex_t *mutex_left, *mutex_right;
   int proc=0;
   at_thread_t *t, *lc, *rc;
   record_t res, r;
   int s;
   int flags;
   int work_threads;
   int thr_load;

   /* Make sure the thread is no longer stolen ... */
   at_mig_disable();

   /* Poll and let "urgent" remote forks be processed */
   BR_POLL();
   at_yield();

   /* Copy the array in, of necessary */
   copy_in(recs, first, last, src);
   
   t = at_self();

   if(level>max_level) max_level = level;
   /*
    *  Once we get to a subrange of MIZSIZE elements, use a bubble sort.
    */
   if ((last-first+1) < minsize) {
     swap = 1;
     while (swap) {
       swap    = 0;
       lastpos = last;
       for (i=first; i<lastpos; i++) {
	 if (REC_GT(recs[i],recs[i+1])) {
	   temp   = recs[i];
	   recs[i]   = recs[i+1];
	   recs[i+1] = temp;
	   swap   = 1;
	 }
       }
       lastpos = lastpos-1;
     }
     
	     
     /*insertion(recs, first, last);*/
   }

   /*
    * Otherwise, quick sort the element range.
    */
   else {
     
     /* A hack to enable cache simulation starting at this point */
     /*at_setdata(-1);*/
      /*
       * Position pivot element such that elements before are less than
       * and elements after are greater than.
       */
      pivot   = recs[(first+last)/2];
      lesspos = last;
      morepos = first;
      do {
         while (REC_LT(pivot,recs[lesspos])) lesspos--;
         while (REC_LT(recs[morepos],pivot)) morepos++;
         if (morepos <= lesspos) {
	    temp = recs[morepos];
	    recs[morepos] = recs[lesspos];
	    recs[lesspos] = temp;
	    lesspos--;
	    morepos++;
         }
      } while (morepos < lesspos);

      mutex_left = at_mutex_create();
      at_mutex_lock(mutex_left);
      mutex_right = at_mutex_create();
      at_mutex_lock(mutex_right);

      /* For cache sim */
      /*exit(0);*/
      /*
       * if the less-than list is larger than two 
       * elements, hand it off to be sorted by a new
       * thread.  Then recursively sort the greater-than
       * list (if larger than two elements).
       */

      proc = AT_UNBOUND;

      /* 
       * Schedule the child threads to run on the same virtual processor
       * as the parent to try to reuse cache (if the bundle supprts 
       * memory conscious scheduling - otheriwise this stuff is ignored)
       */
      thr_load = 10;
      work_threads = at_thread_count() - BR_service_forks();
      if (first < lesspos) {
	if(((lesspos-first)>1000) && (work_threads > thr_load)){
	  flags = AT_MIG_ENABLE;
	}
	else {
	  flags = AT_MIG_DISABLE;
	}	  
	lc = at_create_6(at_mig_bundle, flags, 
			 (at_userf_6_t *)qsortr, (at_word_t)records, 
			 first, lesspos, (at_word_t)mutex_left, 
			 level+1, (at_word_t)BR_HERE());
	
      }

      work_threads = at_thread_count() - BR_service_forks();
      if (morepos < last) {
	if(((last-morepos)>1000) && (work_threads > thr_load)){
	  flags = AT_MIG_ENABLE;
	}
	else {
	  flags = AT_MIG_DISABLE;
	}	  
	rc = at_create_6(at_mig_bundle, flags, 
			 (at_userf_6_t *)qsortr, (at_word_t)records, 
			 morepos,last, (at_word_t)mutex_right, 
			 level+1, (at_word_t)BR_HERE());
      }


      /* wait for the thread sorting the less-than list */
      

      if (first < lesspos) {
	at_mutex_lock(mutex_left);
      }
      if(morepos < last){
	at_mutex_lock(mutex_right);
      }
   }

   /* Copy the array out, of necessary */
   copy_out(recs, first, last, src);
   /* Unlock the mutex, possibly on the remote node... */
   BR_REQUEST_1(src, BR_unlock_handler, (BR_word_t)done);
     
     /*at_mutex_unlock(done);*/
}

extern int getrusage(int who, struct rusage *rusage);

void report_usage()
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage);
  printf("Max res mem: %ld\n", rusage.ru_maxrss/8);
  printf("Int res mem: %ld\n", rusage.ru_idrss/8);
  printf("Page faults: %ld\n", rusage.ru_minflt+rusage.ru_majflt);
}





