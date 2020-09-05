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

/* Benchmarking conjunctive locks */

#include "at.h"
#include "at_c_locks.h"
#include <sys/time.h>

void single_benchmarks(int loops);
double hrtime_to_seconds(hrtime_t time);
void locker(at_word_t c_lock_, at_word_t mutex_start_,at_word_t mutex_stop_,
                        at_word_t finish_mutex_);
void single_benchmark(int loops, at_generic_c_lock_t* c_lock, 
                        at_generic_c_lock_t* blocker, char* c_lock_str);
void multiple_benchmarks(int loops, int max_conj_locks);
void multiple_benchmark(int loops, int max_conj_locks,
                        at_generic_c_lock_t** c_locks, 
                        at_generic_c_lock_t** blockers, char* c_lock_str);
void dynamic_benchmarks(int loops, int max_conj_locks);
void dynamic_benchmark(int loops, int max_conj_locks,
                        at_generic_c_lock_t** c_locks,
                        at_generic_c_lock_t** blockers, char* c_lock_str);


int main( int argc, char** argv)
{
   int loops = 1000;
   int max_conj_locks = 5;

   if (argc > 1) {
      loops = atoi( argv[ 1]);
      if (loops<1) loops = 1000;
   }

   if (argc > 2) {
      max_conj_locks = atoi( argv[ 2]);
      if (max_conj_locks<1) max_conj_locks = 5;
   }

   /* Initialize Active threads */
   at_init(0,0x2000,0);
   /* Single c_lock benchmarks */
   single_benchmarks(loops);
   /* Multiple c_locks benchmarks */
   multiple_benchmarks(loops,max_conj_locks);
   /* Dynamic benchmarks: Create and destroy managers */
   dynamic_benchmarks(loops,max_conj_locks);

   printf("\n");
   /* Test OK */
   return (0);
}


/* Single c_lock benchmarks */
void single_benchmarks(int loops)
{
   at_generic_c_lock_t* mutex = (at_generic_c_lock_t*)at_mutex_c_lock_create();
   at_rw_c_lock_t* rw_lock = at_rw_c_lock_create();
   at_generic_c_lock_t* reader = rw_lock->reader;
   at_generic_c_lock_t* writer = rw_lock->writer;

   single_benchmark( loops, mutex,  mutex,  " mutex");
   single_benchmark( loops, reader, writer, "reader");
   single_benchmark( loops, writer, writer, "writer");
}


void single_benchmark(int loops, at_generic_c_lock_t* c_lock, 
                      at_generic_c_lock_t* blocker, char* c_lock_str)
{
   int i; /* loop counter */
   hrtime_t start, stop; /* timer values */
   /* Mutexes for signalling */
   at_mutex_t *start_mutex, *stop_mutex, *finish_mutex;

   /* Locking and releasing an uncontested mutex */
   printf("\n=== Single %s ===\n", c_lock_str);

   /* Acquire a single c_lock */
   start = gethrtime();
   for ( i=0; i<loops; i++) {
      at_acquire_single_c_lock(c_lock);
   }
   stop = gethrtime();
   printf("Acquire a single %s:               %8.3f ns.\n", c_lock_str,
          hrtime_to_seconds(stop-start)/(double)loops);

   /* Release a single c_lock */
   start = gethrtime();
   for ( i=0; i<loops; i++) {
      at_release_single_c_lock(c_lock);
   }
   stop = gethrtime();
   printf("Release a single %s:               %8.3f ns.\n", c_lock_str,
          hrtime_to_seconds(stop-start)/(double)loops);

   /* Acquire and release a single c_lock */
   start = gethrtime();
   for ( i=0; i<loops; i++) {
      at_acquire_single_c_lock(c_lock);
      at_release_single_c_lock(c_lock);
   }
   stop = gethrtime();
   printf("Acquire and release a single %s:   %8.3f ns.\n", c_lock_str,
          hrtime_to_seconds(stop-start)/(double)loops);

   /* Create a thread that blocks the c_lock */
   /* Use two initially blocked basic mutexes for synchronization */
   start_mutex  = at_mutex_create(); at_mutex_lock(start_mutex);
   stop_mutex   = at_mutex_create(); at_mutex_lock(stop_mutex);
   finish_mutex = at_mutex_create(); at_mutex_lock(finish_mutex);
   at_create_4(at_get_focus(), AT_UNBOUND, locker, (at_word_t)blocker,
               (at_word_t)start_mutex, (at_word_t)stop_mutex,
               (at_word_t)finish_mutex);

   /* Wait until the c_lock is blocked */
   at_mutex_lock(start_mutex);
   at_mutex_unlock(start_mutex);

   /* Try a single c_lock */
   start = gethrtime();
   for ( i=0; i<loops; i++) {
      at_tryacquire_single_c_lock(c_lock);
   }
   stop = gethrtime();
   printf("Try and fail a single %s:          %8.3f ns.\n", c_lock_str,
          hrtime_to_seconds(stop-start)/(double)loops);

   /* Signal the other thread to release the mutex */
   at_mutex_unlock(stop_mutex);
   /* Wait for the other thread's termination */
   at_mutex_lock(finish_mutex);
   at_mutex_unlock(finish_mutex);

   /* Try a single c_lock */
   start = gethrtime();
   for ( i=0; i<loops; i++) {
      at_tryacquire_single_c_lock(c_lock);
   }
   stop = gethrtime();
   printf("Try and get a single %s:           %8.3f ns.\n", c_lock_str,
          hrtime_to_seconds(stop-start)/(double)loops);

   /* Release a single c_lock */
   start = gethrtime();
   for ( i=0; i<loops; i++) {
      at_release_single_c_lock(c_lock);
   }
   stop = gethrtime();

   /* Try and release a single c_lock */
   start = gethrtime();
   for ( i=0; i<loops; i++) {
      at_tryacquire_single_c_lock(c_lock);
      at_release_single_c_lock(c_lock);
   }
   stop = gethrtime();
   printf("Try, get and release a single %s:  %8.3f ns.\n", c_lock_str,
          hrtime_to_seconds(stop-start)/(double)loops);

}

/* Multiple c_lock benchmarks */
void multiple_benchmarks(int loops, int max_conj_locks)
{
   int i; /* loop counter */

  /* Arrays of c_locks */
   at_generic_c_lock_t** mutexes;
   at_generic_c_lock_t** readers;
   at_generic_c_lock_t** writers;

   /* Reserve memory for the arrays of c_locks */
   mutexes = malloc( max_conj_locks * sizeof(at_generic_c_lock_t*));
   readers = malloc( max_conj_locks * sizeof(at_generic_c_lock_t*));
   writers = malloc( max_conj_locks * sizeof(at_generic_c_lock_t*));

   /* Initialize the arrays */
   for ( i=0; i<max_conj_locks; i++) {
      at_rw_c_lock_t* rw_lock = at_rw_c_lock_create();
      readers[i] = rw_lock->reader;
      writers[i] = rw_lock->writer;
      mutexes[i] = (at_generic_c_lock_t*)at_mutex_c_lock_create();
   }

   multiple_benchmark( loops, max_conj_locks, mutexes, mutexes, "mutexes");
   multiple_benchmark( loops, max_conj_locks, readers, writers, "readers");
   multiple_benchmark( loops, max_conj_locks, writers, writers, "writers");
}


void multiple_benchmark(int loops, int max_conj_locks,
                       at_generic_c_lock_t** c_locks, 
                       at_generic_c_lock_t** blockers, char* c_lock_str)
{
   int i,j; /* loop counter */
   hrtime_t start, stop; /* timer values */
   /* Mutexes for signalling */
   at_mutex_t* start_mutex  = at_mutex_create();
   at_mutex_t* stop_mutex   = at_mutex_create();
   at_mutex_t* finish_mutex = at_mutex_create();

   /* Locking and releasing an uncontested mutex */
   printf("\n=== 1 to %2d %s ===                  ",
          max_conj_locks, c_lock_str);
   for ( j=1; j<=max_conj_locks; j++) printf(" %2d    ", j);
   printf("\n");

   /* Acquire c_locks */
   printf("Acquire conjunctive %s:             ", c_lock_str);fflush(stdout);
   for ( j=1; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++)
         at_acquire_c_locks(c_locks,j);
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");

   /* Release c_locks */
   printf("Release conjunctive %s:             ", c_lock_str);fflush(stdout);
   for ( j=1; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++)
         at_release_c_locks(c_locks,j);
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");

   /* Acquire and release c_locks */
   printf("Acquire and release conjunctive %s: ", c_lock_str);fflush(stdout);
   for ( j=1; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++) {
         at_acquire_c_locks(c_locks,j);
         at_release_c_locks(c_locks,j);
      }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");

   /* Try and fail c_locks */
   printf("Try and fail %s (first one blocks): ", c_lock_str);fflush(stdout);
   for ( j=1; j<=max_conj_locks; j++) {
      /* Create a thread that blocks the c_lock */
      /* Use initially blocked basic mutexes for synchronization */
      at_mutex_lock(start_mutex);
      at_mutex_lock(stop_mutex);
      at_mutex_lock(finish_mutex);
      at_create_4(at_get_focus(), AT_UNBOUND, locker,
                  (at_word_t)blockers[0], (at_word_t)start_mutex,
                  (at_word_t)stop_mutex, (at_word_t)finish_mutex);

      /* Wait until the c_lock is blocked */
      at_mutex_lock(start_mutex);
      at_mutex_unlock(start_mutex);

      /* Try c_locks */
      start = gethrtime();
      for ( i=0; i<loops; i++)
	 if (at_tryacquire_c_locks(c_locks,j)) { printf("Error1!\n");exit(1); }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);

      /* Signal the other thread to release the mutex */
      at_mutex_unlock(stop_mutex);
      /* Wait for the other thread's termination */
      at_mutex_lock(finish_mutex);
      at_mutex_unlock(finish_mutex);
   }
   printf("ns.\n");

   /* Try and fail c_locks */
   printf("Try and fail %s (last one blocks):  ", c_lock_str);fflush(stdout);
   for ( j=1; j<=max_conj_locks; j++) {
      /* Create a thread that blocks the c_lock */
      /* Use two initially blocked basic mutexes for synchronization */
      at_mutex_lock(start_mutex);
      at_mutex_lock(stop_mutex);
      at_mutex_lock(finish_mutex);
      at_create_4(at_get_focus(), AT_UNBOUND, locker,
                  (at_word_t)blockers[j-1], (at_word_t)start_mutex,
                  (at_word_t)stop_mutex, (at_word_t)finish_mutex);

      /* Wait until the c_lock is blocked */
      at_mutex_lock(start_mutex);
      at_mutex_unlock(start_mutex);

      /* Try c_locks */
      start = gethrtime();
      for ( i=0; i<loops; i++)
	 if (at_tryacquire_c_locks(c_locks,j)) { printf("Error2!\n");exit(1); }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);

      /* Signal the other thread to release the mutex */
      at_mutex_unlock(stop_mutex);
      /* Wait for the other thread's termination */
      at_mutex_lock(finish_mutex);
      at_mutex_unlock(finish_mutex);
   }
   printf("ns.\n");

   /* Try c_locks */
   printf("Try and get conjunctive %s:         ", c_lock_str);fflush(stdout);
   for ( j=1; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++)
	 if (!at_tryacquire_c_locks(c_locks,j)) { printf("Error3!\n");exit(1); }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");

   /* Release c_locks */
   for ( j=1; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++)
         at_release_c_locks(c_locks,j);
      stop = gethrtime();
   }

   /* Try and release c_locks */
   printf("Try, get and release conjunctive %s:", c_lock_str);fflush(stdout);
   for ( j=1; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++) {
	 if (!at_tryacquire_c_locks(c_locks,j)) { printf("Error3!\n");exit(1); }
         at_release_c_locks(c_locks,j);
      }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");
}


/* Dynamic benchmarks: Create and destroy managers */
void dynamic_benchmarks(int loops, int max_conj_locks)
{
   int i; /* loop counter */
 
  /* Arrays of c_locks */
   at_generic_c_lock_t** mutexes;
   at_generic_c_lock_t** readers;
   at_generic_c_lock_t** writers;
 
   /* Reserve memory for the arrays of c_locks */
   mutexes = malloc( max_conj_locks * sizeof(at_generic_c_lock_t*));
   readers = malloc( max_conj_locks * sizeof(at_generic_c_lock_t*));
   writers = malloc( max_conj_locks * sizeof(at_generic_c_lock_t*));
 
   /* Initialize the arrays */
   for ( i=0; i<max_conj_locks; i++) {
      at_rw_c_lock_t* rw_lock = at_rw_c_lock_create();
      readers[i] = rw_lock->reader;
      writers[i] = rw_lock->writer;
      mutexes[i] = (at_generic_c_lock_t*)at_mutex_c_lock_create();
   }
 
   dynamic_benchmark( loops, max_conj_locks, mutexes, mutexes, "mutexes");
   dynamic_benchmark( loops, max_conj_locks, readers, writers, "readers");
   dynamic_benchmark( loops, max_conj_locks, writers, writers, "writers");
}
 
 
void dynamic_benchmark(int loops, int max_conj_locks,
                       at_generic_c_lock_t** c_locks,
                       at_generic_c_lock_t** blockers, char* c_lock_str)
{
   int i,j; /* loop counter */
   hrtime_t start, stop; /* timer values */
   at_c_lock_manager_t* manager; /* temporary buffer */
   /* Mutexes for signalling */
   at_mutex_t* start_mutex  = at_mutex_create();
   at_mutex_t* stop_mutex   = at_mutex_create();
   at_mutex_t* finish_mutex = at_mutex_create();
 
   /* Benchmark without pools */
   printf("\n=== Acquire and release 2 to %2d %s ===     ",
          max_conj_locks, c_lock_str);
   for ( j=2; j<=max_conj_locks; j++) printf(" %2d    ", j);
   printf("\n");
 
   /* Acquire and release c_locks */
   printf("Create and split manager each time (no pools)  :");fflush(stdout);
   for ( j=2; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++) {
         at_acquire_c_locks(c_locks,j);
         manager = at_release_c_locks(c_locks,j);
         at_c_lock_manager_split(manager,c_locks,j);
      }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");

   /* Acquire and release c_locks */
   printf("Create and split manager each time (no pools)  :");fflush(stdout);
   for ( j=2; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++) {
         at_acquire_c_locks(c_locks,j);
         at_release_c_locks_and_split_manager(c_locks,j);
      }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");
 
   /* Initialize lock entry pool and lock manager pool */
   at_c_locks_init_pools();

   /* Acquire and release c_locks */
   printf("Create and split manager each time (with pools):");fflush(stdout);
   for ( j=2; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++) {
         at_acquire_c_locks(c_locks,j);
         manager = at_release_c_locks(c_locks,j);
         at_c_lock_manager_split(manager,c_locks,j);
      }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");

   /* Acquire and release c_locks */
   printf("Create and split manager each time (with pools):");fflush(stdout);
   for ( j=2; j<=max_conj_locks; j++) {
      start = gethrtime();
      for ( i=0; i<loops; i++) {
         at_acquire_c_locks(c_locks,j);
         at_release_c_locks_and_split_manager(c_locks,j);
      }
      stop = gethrtime();
      printf("%6.3f ", hrtime_to_seconds(stop-start)/(double)loops);
      fflush(stdout);
   }
   printf("ns.\n");

   /* Destroy lock entry pool and lock manager pool */
   at_c_locks_destroy_pools();
}


void locker(at_word_t c_lock_, at_word_t mutex_start_,at_word_t mutex_stop_,
            at_word_t finish_mutex_)
{
   at_generic_c_lock_t* c_lock = (at_generic_c_lock_t*)c_lock_;
   at_mutex_t* mutex_start  =(at_mutex_t*)mutex_start_;
   at_mutex_t* mutex_stop   =(at_mutex_t*)mutex_stop_;
   at_mutex_t* finish_mutex =(at_mutex_t*)finish_mutex_;

   /* acquire the c_lock */
   at_acquire_single_c_lock(c_lock);
   /* Signal the calling thread */
   at_mutex_unlock(mutex_start);
   /* Wait for signal to unlock */
   at_mutex_lock(mutex_stop);
   at_mutex_unlock(mutex_stop);
   /* release th c_lock */
   at_release_single_c_lock(c_lock);
   /* Signal termination */
   at_mutex_unlock(finish_mutex);
}
   
/* Convert hrtime value to double in seconds */
double hrtime_to_seconds(hrtime_t time)
{
   return ( (double)time / (double)1e3);
}
