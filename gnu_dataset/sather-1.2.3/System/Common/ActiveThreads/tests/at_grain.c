#include "at.h"
#include <time.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <stdio.h>

#define MAX_THREADS 1000

#define SEQUENTIAL

int thr_numbers[MAX_THREADS];

int concurrency = 1;
/*int size = 65536;*/
int size = 1024;
int leaf_delay = 0;

AT_SPINLOCK_DEC(slck);
int counter;
struct tms buf;
at_bundle_t *bundle, *old_bundle;
void grain(at_word_t s, at_word_t sem);

hrtime_t start, finish;
/*clock_t start, finish;*/

int main(int argc, char **argv){
  int i;
  at_sema_t sema;

  if(argc>1){
    concurrency = atoi(argv[1]);
  }

  if(argc>2){
    leaf_delay = atoi(argv[2]);
  }
  
  if(argc>3){
    size = atoi(argv[3]);
  }

  /*Initialize the thread package */
  at_init(concurrency, 0x1000, 0);

  at_printf("Initialization finished \n");

  old_bundle= at_get_focus();
  bundle = at_mcs_lazy_bundle_create(at_get_focus());
  /*bundle=at_get_focus();*/
  at_set_focus(bundle);

  start=gethrtime();
  at_sema_init(&sema, 0);

  grain((at_word_t)size, (at_word_t)&sema);

  finish=gethrtime(); 
  at_set_focus(old_bundle);
  /* destroy the work bundle now */
  at_bundle_destroy(bundle);
  at_printf("TIME: %e\n", (finish-start)/1.0e+9);
  exit(0);
}

#define DELAY       asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");\
asm volatile ("add %o0, 1, %o0");

  
void grain(at_word_t s, at_word_t sem){
  int size = (int)s;
  at_sema_t *parent_sema = (at_sema_t *)sem;
  at_sema_t sema;
  int left_size, right_size;
  int j;
  if(size > 1){
    /* Split the buffer */
    left_size = size/2;
    right_size = size - left_size;
    
    #ifndef SEQUENTIAL
    at_sema_init(&sema, 0);
    at_create_2(bundle, AT_UNBOUND, grain, 
		(at_word_t)left_size, (at_word_t)&sema);   
    at_create_2(bundle, AT_UNBOUND, grain, 
		(at_word_t)right_size, (at_word_t)&sema);   
    at_sema_wait(&sema);
    at_sema_wait(&sema);
    #else
    /* sequential execution */
    grain((at_word_t)left_size, (at_word_t)&sema);
    grain((at_word_t)right_size, (at_word_t)&sema);
    #endif
  }
  else {
    int i;
    for(i=0; i<(leaf_delay)/100; i++){
      DELAY;
      DELAY;
      DELAY;
      DELAY;
      DELAY;
      DELAY;
      DELAY;
      DELAY;
      DELAY;
      DELAY;
    }
  }
  /* Signal the parent */
  #ifndef SEQUENTIAL
  at_sema_signal(parent_sema);
  #endif
}
  










