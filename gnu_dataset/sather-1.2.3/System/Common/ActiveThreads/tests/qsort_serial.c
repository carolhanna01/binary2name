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
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>

#define FAIL	1			/* Verification failed */
#define MINSIZE 100 			/* Cut-over size for bubble-sort */
#define PASS	0			/* Verification passed */
#define SIZE	2000000 	        /* Size of array to sort */
#define SIZEBY4	SIZE/4			/* Quarter size of array */

#define REC_LT(a,b) ((a)->id < (b)->id)
#define REC_LEQ(a,b) ((a)->id <= (b)->id)
#define REC_GT(a,b) ((a)->id > (b)->id)

typedef struct record{
  int  id;
  char stuff[50];
} *record_t;

void qsortr(record_t recs[],int first,int last,int level);

int max_level;


void report_usage();

/***********
 *  Quicksort driver:  initialize, sort, and verify.
 */


record_t  records[SIZE];		/* Array to be sorted */
int size;
int minsize;

int main(int argc, char **argv)
{
   int i;				/* Loop counter */
   int check=PASS;			/* Verify failuer flag */
   int concurrency = 0;
   clock_t start, finish;
   char *filenm=NULL;
   FILE *fp;


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
     
   for (i=size-1; i>=0; i--) {
     records[i] = (record_t)malloc(sizeof(struct record));
     records[i]->id=(int)rand();;
   }   

   printf("DATA SIZE: %dK\n", sizeof(struct record)*size/1024);
   start=clock();

   /* Quicksort the array */
   qsortr(records,0,size-1,0);

   finish=clock();
   
   /* Verify the results */
   for (i=1; i<size; i++) {
     if (REC_LT(records[i],records[i-1])) {
       printf("INVALID SORT: records[%d]=%d, records[%d]=%d\n",i-1,records[i-1]->id,i,records[i]->id);
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
      exit(1);
   }
   printf("time in us %e\n", (double)(finish-start)/(1000.0));
   printf("max level: %d\n", max_level);
   fclose(fp);

   exit(0);
}


/***********
 *  Quick sort routine; parallel and recursive.
 */
/* int * d;				        array to be sorted */
/* int first,last;				element range to sort */

void qsortr(record_t *recs,int first,int last, int level)
{
   int lastpos;
   record_t temp;
   int swap;
   record_t pivot;
   int lesspos;
   int morepos;
   int i;
   int proc=0;

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
   }

   /*
    * Otherwise, quick sort the element range.
    */
   else {

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

      /*
       * if the less-than list is larger than two 
       * elements, hand it off to be sorted by a new
       * thread.  Then recursively sort the greater-than
       * list (if larger than two elements).
       */

      if (first < lesspos) {
	qsortr(records, first, lesspos, level+1);
      }
      if (morepos < last) {
	qsortr(records, morepos,last, level+1);
      }
   }
}


