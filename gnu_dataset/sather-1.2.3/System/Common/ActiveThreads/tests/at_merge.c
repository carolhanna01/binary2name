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
#include <assert.h>
#include <time.h>

#define FAIL	1			/* Verification failed */
#define MINSIZE 50 			/* Cut-over size for merge-sort */
#define PASS	0			/* Verification passed */
#define SIZE	100000                  /* Size of array to sort */

#define REC_LT(a,b) ((a)->id < (b)->id)
#define REC_LEQ(a,b) ((a)->id <= (b)->id)
#define REC_GT(a,b) ((a)->id > (b)->id)


#define at_share(x,y,z)
#define at_crt_flush(b)

typedef struct record{
  struct record *next;
  char stuff[64];
  int  id;
} record_t;

void merge(record_t *recs, record_t **sorted,int size,at_sema_t *done, int level);
record_t *insertion(record_t *recs);
record_t *merge_lists(record_t *left, record_t *right);
  
volatile int pnum=0;
AT_SPINLOCK_DEC(pnum_lck);
int max_level;

void at_flush(at_bundle_t *b){
  at_crt_flush(b);
}


int next_proc(){
  int i;
  AT_SPINLOCK_LOCK(pnum_lck);
  i = pnum;
  pnum++;
  AT_SPINLOCK_UNLOCK(pnum_lck);
  return i;
}


/***********
 *  Quicksort driver:  initialize, sort, and verify.
 */

record_t *recs=NULL, *recs1=NULL;		        /* to be sorted */
at_bundle_t *bundle;

int size;
int minsize;

record_t *records_create(int size){
  int i;
  record_t *s, *r;

  s = NULL;
  for(i=0; i<size; i++){
    r = (record_t *)malloc(sizeof (record_t));
    r->id = (int)rand();
    r->next = s;
    s = r;
  }
  at_printf("DATA SIZE: %d\n", size*sizeof(record_t)/1024);
  return s;
}

void list_print(char *str, record_t *r){
  printf("%s\n", str);
  while(r){
    printf("%d  ", r->id);
    r = r->next;
  }
  printf("\n");
}


int main(int argc, char **argv)
{
   int check=PASS;			/* Verify failuer flag */
   at_sema_t sema;
   int concurrency = 0;
   /*hrtime_t start, finish;*/
   clock_t start, finish;
   record_t *r, *res;
   FILE *fp;
   at_bundle_t *old_bundle;

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
     fp = fopen(argv[4], "a+");
   }
   else {
     fp = stderr;
   }

   at_init(concurrency, 0x1000, 0);
   recs = records_create(size);
   
   AT_SPINLOCK_INIT(pnum_lck);


   /*-----------------------------------------------------------------*/
   old_bundle = at_get_focus();
   bundle = at_fifo_bundle_create(at_get_focus());
   at_set_focus(bundle);  

   recs = records_create(size);   
   at_sema_init(&sema, 0);
   /*start=gethrtime();*/
   start=clock();
   at_create_5(bundle, AT_UNBOUND, (at_userf_5_t *)merge, (at_word_t)recs, 
	       (at_word_t)&res, (at_word_t)size, (at_word_t)&sema, 0);
   at_sema_wait(&sema);

   /*finish=gethrtime();*/
   finish = clock();

   at_set_focus(old_bundle);
   at_bundle_destroy(bundle);
   /*-----------------------------------------------------------------*/   

   printf("time in us %e\n", (double)(finish-start)/1.0e9);
   fprintf(fp, "%d   %d   %d   %e   %d    %dK\n", concurrency, size, minsize,  
	   (double)(finish-start)/(1.0e9), at_thread_count(), max_mem_usage/1024);
   max_mem_usage = 0;
   /*-----------------------------------------------------------------*/
   old_bundle = at_get_focus();
   bundle = at_fifo_bundle_create(at_get_focus());
   at_set_focus(bundle);  

   recs = records_create(size);   
   at_sema_init(&sema, 0);
   /* start=gethrtime(); */
   start = clock();
   at_create_5(bundle, AT_UNBOUND, (at_userf_5_t *)merge, (at_word_t)recs, 
	       (at_word_t)&res, (at_word_t)size, (at_word_t)&sema, 0);
   at_sema_wait(&sema);
   /* finish=gethrtime(); */
   finish = clock();

   at_set_focus(old_bundle);
   at_bundle_destroy(bundle);
   /*-----------------------------------------------------------------*/   
   

   /* Verify the results */
   r = res;
   while(r->next){
     if (REC_LT(r->next,r)) {
       printf("INVALID SORT: %d, %d\n",r->id,r->next->id);
       check=FAIL;
     }
     r = r->next;
   }

   /*list_print("AFTER: ", res);*/

   if (check == PASS) {
      printf("\nmergesort worked.\n");
   }
   else {
      printf("\nmergesort test failed!\n");
      exit(1);
   }
   printf("time in us %e\n", (double)(finish-start)/1.0e9);
   printf("max level: %d\n", max_level);
   fprintf(fp, "%d   %d   %d   %e   %d    %dK\n", concurrency, size, minsize,  
	   (double)(finish-start)/(1.0e9), at_thread_count(), max_mem_usage/1024); 
   fclose(fp);
   return 0;
}


record_t *insertion(record_t *recs){
  record_t *res;

  /* Get one element */
  res = recs;
  recs = recs->next;
  res->next=0;
  
  while(recs){
    /* insert in proper place in "res" list */
    record_t *c, *p;
    record_t *n;

    n = recs;
    recs = recs->next;
    p = c = res;
    
    while(c && REC_GT(n,c)){
      p = c;
      c = c->next;
    }
    if(p==c){
      /* Insert in the beginning */
      n->next = res;
      res = n;
    }
    else{
      /* Insert in the middle/end */
      n->next = c;
      p->next = n;
    }
  }
  return res;
}

record_t *merge_lists(record_t *left, record_t *right){
  record_t *res, *last;
  
  if(REC_LT(left, right)){
    res = left;
    left = left->next;
  } else {
    res = right;
    right = right->next;
  }
  res->next = NULL;
  last = res;

  while(1){
    if(!((unsigned long)left & (unsigned long)right)){
      if(left==NULL){
	last->next = right;
	break;
      }
      if(right==NULL){
	last->next = left;
	break;
      }
    }
    if(REC_LT(left, right)){
      last->next = left;
      left = left->next;
    } else {
      last->next = right;
      right = right->next;
    }
    last = last->next;
    last->next = NULL;
  }

  return res;
}



void merge(record_t *recs, record_t **sorted_list, int size, at_sema_t *done, int level)
{
   record_t *mid, *left, *right, *p=NULL;
   record_t *res=NULL;
   at_sema_t sema;
   at_thread_t *lc, *rc;
   int i;

   if(level>max_level) max_level = level;
   /*
    *  Once we get to a subrange of MIZSIZE elements, use INSERTION  sort.
    */
   if (size < minsize) {
     res = insertion(recs);
   }
   else {
     /* Otherwise, call merge on two halfs and merge them together */
     at_sema_init(&sema, 0);

     /* Find the middle element */
     mid = recs;
     for(i=0; i<size/2; i++){
       p = mid;
       mid = mid->next;
     }
     /* Finish the left part */
     p->next = NULL;

     /* 
      * Execute on the same virtual processor as parent in order to reuse
      * some chache. Bundles that do not support memory-conscious
      * scheduling will ignore the affinity annotations
      */
     lc = at_create_5(bundle, at_get_affinity(), (at_userf_5_t *)merge,(at_word_t)recs, (at_word_t)&left, size/2,  (at_word_t)&sema, level+1);
     rc = at_create_5(bundle, at_get_affinity(), (at_userf_5_t *)merge, (at_word_t)mid, (at_word_t)&right, (size-size/2), (at_word_t)&sema, level+1);     
     at_share(lc, at_self(), 1.0);
     at_share(rc, at_self(), 1.0);
     
     /* wait until children are done */
     at_sema_wait(&sema);
     at_sema_wait(&sema);


     /* Now, merge the two lists */
     res = merge_lists(left, right);
   }
   *sorted_list = res;
   at_sema_signal(done);
}





