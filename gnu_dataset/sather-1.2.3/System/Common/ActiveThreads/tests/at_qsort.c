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

#define FAIL	1			/* Verification failed */
#define MINSIZE 50 			/* Cut-over size for bubble-sort */
#define PASS	0			/* Verification passed */
#define SIZE	100000 	                /* Size of array to sort */
#define SIZEBY4	SIZE/4			/* Quarter size of array */

#define REC_LT(a,b) (((a)->id) < ((b)->id))
#define REC_LEQ(a,b) (((a)->id) <= ((b)->id))
#define REC_GT(a,b) (((a)->id) > ((b)->id))

typedef struct record {
  struct record *next;
  char stuff[64];
  double id;
} *record_t;

void qsortr(record_t recs[],int first,int last,at_mutex_t *done, int level);

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
    records[i] = (record_t)at_malloc(sizeof(struct record));
    records[i]->id= drand48()*size;
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
   
int main(int argc, char **argv)
{
   int i, j;	
   int check=PASS;			/* Verify failuer flag */
   at_mutex_t *mutex;
   int concurrency = 0;
   /*hrtime_t start, finish;*/
   clock_t start, finish;   
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


   at_init(concurrency, 0x2000, 0);

   records_create();
   records_rand();
   

   at_printf("at_init() finished\n");
   at_printf("DATA SIZE: %dK\n", sizeof(struct record)*size/1024);

   /*----------------------------------------------------------------------*/
   /*start=gethrtime();*/
   start = clock();
   mutex = at_mutex_create();
   at_mutex_lock(mutex);

   t = at_create_5(at_get_focus(), AT_UNBOUND, 
		   (at_userf_5_t *)qsortr, (at_word_t)records,
		   0,size-1,(at_word_t)mutex,0);
   at_mutex_lock(mutex);
   /*finish=gethrtime();*/
   finish = clock();
   /*
     at_set_focus(old_bundle);
     at_bundle_destroy(bundle);
     */
   printf("time in us %e\n", (double)(finish-start)/1e6);   
   /*-----------------------------------------------------------------------*/

   /* Now, do it for real */
   records_rand();
   
   /*----------------------------------------------------------------------*/
   /*start=gethrtime();*/
   start = clock();
   mutex = at_mutex_create();
   at_mutex_lock(mutex);

   t = at_create_5(at_get_focus(), AT_UNBOUND, 
		   (at_userf_5_t *)qsortr, (at_word_t)records,
		   0,size-1,(at_word_t)mutex,0);
   at_mutex_lock(mutex);
   /*finish=gethrtime();*/
   finish = clock();
   /*-----------------------------------------------------------------------*/
   


   /* Verify the results */
   for (i=1; i<size; i++) {
     if (REC_LT(records[i],records[i-1])) {
       printf("INVALID SORT: records[%d]=%e, records[%d]=%e\n",i-1,records[i-1]->id,i,records[i]->id);
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

   exit(0);
}

int list_size(record_t r){
  int i;

  i = 0;
  while(r){
    i++;
    r=r->next;
  }
  return i;
}

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


/***********
 *  Quick sort routine; parallel and recursive.
 */
/* int * d;				        array to be sorted */
/* int first,last;				element range to sort */

void qsortr(record_t *recs,int first,int last,at_mutex_t *done, int level)
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
      if (first < lesspos) {
	lc = at_create_5(at_get_focus(), at_get_affinity(), (at_userf_5_t *)qsortr, 
		    (at_word_t)records, first, lesspos, (at_word_t)mutex_left, level+1);
	/* Set the sharing coefficient */
	at_lff_share(at_self(), lc, (lesspos-first+1.0)/(last-first+1.0));
	
      }
      if (morepos < last) {
	rc = at_create_5(at_get_focus(), at_get_affinity(), (at_userf_5_t *)qsortr, 
		    (at_word_t)records, morepos,last, (at_word_t)mutex_right, level+1);
	at_lff_share(at_self(), rc, (last-morepos+1.0)/(last-first+1.0));
      }
      /* wait for the thread sorting the less-than list */

      if (first < lesspos) {
	at_mutex_lock(mutex_left);
      }
      if(morepos < last){
	at_mutex_lock(mutex_right);
      }
   }
   at_mutex_unlock(done);
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





