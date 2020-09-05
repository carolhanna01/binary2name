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

#include <thread.h>
#include <errno.h>

#define FAIL	1			/* Verification failed */
#define MINSIZE 20 			/* Cut-over size for bubble-sort */
#define PASS	0			/* Verification passed */
#define SIZE	2560			/* Size of array to sort */
#define SIZEBY4	640			/* Quarter size of array */

/***********
 *  Quicksort driver:  initialize, sort, and verify.
 */
main()
{
   int a[SIZE];				/* Array to be sorted */
   int i;				/* Loop counter */
   int check=PASS;			/* Verify failuer flag */

  /* Initialize sort array to random numbers */
   for (i=0; i<SIZE; i++) {
      a[i]=(int)rand();
   }

   /* Quicksort the array */
   qsortr(a,0,SIZE-1);

   /* Verify the results */
   for (i=1; i<SIZE; i++) {
      if (a[i] < a[i-1]) {
	 printf("INVALID SORT: a[%d]=%d, a[%d]=%d\n",i-1,a[i-1],i,a[i]);
         check=FAIL;
      }
   }
   printf("\nSorted array is:\n");
   for (i=0; i<SIZEBY4; i++) {
      printf("%13d %13d %13d %13d\n",
	  a[i],a[i+SIZEBY4],a[i+(2*SIZEBY4)],a[i+(3*SIZEBY4)]);
   }
   if (check == PASS) {
      printf("\nqsort worked.\n");
   }
   else {
      printf("\nqsort test failed!\n");
      exit(1);
   }
}


/***********
 *  Quick sort routine; parallel and recursive.
 */
qsortr(d,first,last)
int * d;				/* array to be sorted */
int first,last;				/* element range to sort */
{
   int lastpos;
   int temp;
   int swap;
   int pivot;
   int lesspos;
   int morepos;
   int i;
   thread_t pt;
   void * stat;
   
   /*
    *  Once we get to a subrange of MIZSIZE elements, use a bubble sort.
    */
   if ((last-first+1) < MINSIZE) {
      swap = 1;
      while (swap) {
         swap    = 0;
         lastpos = last;
         for (i=first; i<lastpos; i++) {
	    if (d[i] > d[i+1]) {
	       temp   = d[i];
	       d[i]   = d[i+1];
	       d[i+1] = temp;
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
      pivot   = d[(first+last)/2];
      lesspos = last;
      morepos = first;
      do {
         while (pivot < d[lesspos]) lesspos--;
         while (d[morepos] < pivot) morepos++;
         if (morepos <= lesspos) {
	    temp = d[morepos];
	    d[morepos] = d[lesspos];
	    d[lesspos] = temp;
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
	 thread_qsort(&pt,d,first,lesspos);
      }
      if (morepos < last) {
	 qsortr(d,morepos,last);
      }

      /* wait for the thread sorting the less-than list */
      TRY(thr_join(pt,0,&stat),i);
   }
}


/***********
 *  Start procedure for a thread running quick sort.
 *  Get the arguments passed to the thread and pass
 *  them on to the quick sort routine.
 */
void * start(ar)
void * ar;				/* array of arguments to qsortr */
{
   int a,b,c;				/* arguments to qsortr */
   int *args;

   /*
    * Get the arguments passed to the thread and
    * free up the space used to pass arguments.
    */
   args = (int *)ar;
   a = args[0];
   b = args[1];
   c = args[2];
   free(args);

   /* quick sort using the arguments */
   qsortr(a,b,c);

   /* Return successful completion status */
   return((void*)0);
}


/***********
 * Spawn a thread to quick sort an array and return a handle
 * to the spawned thread.
 */
thread_qsort(p,a,b,c)
thread_t * p;					/* Handle to spawned thread */
int *a,b,c;					/* Arguments for qsortr */
{
   int * args;					/* Ptr to space to pass args */
   int rc1;

   /* Allocate area to pass qsortr arguments in */
   args=(int*)malloc(8*3);
   args[0]=(int)a;
   args[1]=b;
   args[2]=c;

   /* Spawn the new thread */
   TRY(thr_create(0,0,start,(void*)args,0,p),rc1);
}
