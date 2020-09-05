#include <alloca.h>
#include "at.h"
#include "brahma.h"
#include "migration.h"

/* To make all of these work, at_mig_enable() needs to be called
   for intended threads */
at_mutex_t done;
at_mutex_t mx;
int threads = 110;
int threads_to_migrate = 100; 
int size=0;
FILE *fp=stdout;
int which=0;  /* 0 - steal benchmark, 1 - push benchmark */
hrtime_t start, finish;

extern BR_mig_stat_t overhead;
extern int overhead_times;


void leaving_thread(int n);


void benchmark_push_throughput(iterations){
  int i;
  at_thread_t *t;
  double time_per_thread;

  if(BR_HERE()==0){
    for(i=0; i<iterations+4*2; i++){
      t = at_create_1(at_mig_bundle, AT_UNBOUND,  
		      (at_userf_1_t *)leaving_thread, (at_word_t)size);
    }
  
  /* Let each thread start and run for a while to accumulate stack */
  }  

  BR_BARRIER();

  /* Let each thread start and run for a while to accumulate stack */
  for(i=0; i<2*iterations; i++){
    at_yield();
  }
  
  BR_BARRIER();

  if(BR_HERE()==0){
    /* Push things to 1 */
    /*    for(i=0; i<iterations+4; i++){
      if(BR_push_to(1)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }  

    for(i=0; i<iterations; i++){
      while (BR_steal_from(1)){}
    }      

    BR_stack_pushed = 0; */
    /* Now ready to do things */
    start = gethrtime();
    for(i=0; i<iterations; i++){
      if(BR_push_to(1)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }
    finish = gethrtime();
    time_per_thread = (double)(finish-start)/(iterations*1.0e+3);
    fprintf(fp, "THROUGHPUT %e     %e us     %e\n", 
	    ((double)(BR_stack_pushed))/iterations,
	    time_per_thread,
	    1.0e+6/time_per_thread); 
    fclose(fp);
    BR_exit();
  }
}

void benchmark_push_throughput2(iterations){
  int i;
  at_thread_t *t;
  double time_per_thread;
  
  if(BR_HERE()==0){
    for(i=0; i<iterations; i++){
      t = at_create_1(at_mig_bundle, AT_UNBOUND,  
		      (at_userf_1_t *)leaving_thread, (at_word_t)size);
    }
  }
  
  /* Let each thread start and run for a while to accumulate stack */
  for(i=0; i<2*iterations; i++){
    at_yield();
  }
  
  BR_BARRIER();

  if(BR_HERE()==0){
    /* Push things to 1 */
    for(i=0; i<iterations/2; i++){
      if(BR_push_to(1)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }  

    /* Steal them back - all caches are warm */
    for(i=0; i<iterations/2; i++){
      while (BR_steal_from(1)){}
    }      

    at_printf("CLuster 1 warmed up\n");

    /* Push things to 2 */
    for(i=0; i<iterations/2; i++){
      if(BR_push_to(2)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }  

    /* Steal them back - all caches are warm */
    for(i=0; i<iterations/2; i++){
      while (BR_steal_from(2)){}
    }      

    at_printf("CLuster 2 warmed up\n");

    BR_stack_pushed = 0;
    /* Now ready to do things */
    start = gethrtime();
    for(i=0; i<iterations/2; i++){
      if(BR_push_to(1)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
      if(BR_push_to(2)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }
    finish = gethrtime();
    time_per_thread = (double)(finish-start)/(iterations*1.0e+3);
    fprintf(fp, "THROUGHPUT %e     %e us     %e\n", 
	    ((double)(BR_stack_pushed))/iterations,
	    time_per_thread,
	    1.0e+6/time_per_thread); 
    fclose(fp);
    BR_exit();
  }
}

void null_thread(){
}

void benchmark_steal_throughput(iterations){
  int i;
  at_thread_t *t;
  double time_per_thread;
  
  if(BR_HERE()==0){
    for(i=0; i<iterations+2*4; i++){
      t = at_create_1(at_mig_bundle, AT_UNBOUND,  
		      (at_userf_1_t *)leaving_thread, (at_word_t)size);
      /*t = at_create_0(at_mig_bundle, AT_UNBOUND,  
		      (at_userf_0_t *)null_thread);*/
    }
  }
  
    /* Let each thread start and run for a while to accumulate stack */
  for(i=0; i<2*iterations; i++){
    at_yield();
  }
  
  BR_BARRIER();

  if(BR_HERE()==0){
    /* Push things to 1 */
    for(i=0; i<iterations+4; i++){
      if(BR_push_to(1)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }  
  }
  /* Wait on cluster 1 until all arrive */
  if(BR_HERE()==1){
    while(BR_mig_to!=(iterations+4)){
      BR_POLL();
    }
  at_printf("[%d] obtained all\n", BR_HERE());
  }


  BR_BARRIER();

  if(BR_HERE()==0){
    /* Steal them back - all caches are warm */

    start = gethrtime();
    for(i=0; i<iterations; i++){
      BR_steal_from(1);
      at_printf("stolen: %d\n", i);
    }

    /* Wait until done */
    while(BR_migrated_to()<iterations){
      BR_POLL();
    }

    finish = gethrtime();
    
    time_per_thread = (double)(finish-start)/(iterations*1.0e+3);
    fprintf(fp, "THROUGHPUT %e     %e us     %e\n", 
	    ((double)(BR_stack_stolen))/iterations,
	    time_per_thread,
	    1.0e+6/time_per_thread); 
    fclose(fp);
  }

  if(BR_HERE()==0){
    BR_exit();
  }

  if(BR_HERE()==1){
    while(1){
      BR_POLL();
      at_yield();
    }
  }
}


void benchmark_steal_throughput2(iterations){
  int i;
  at_thread_t *t;
  double time_per_thread;
  
  if(BR_HERE()==0){
    for(i=0; i<iterations; i++){
      t = at_create_1(at_mig_bundle, AT_UNBOUND,  
		      (at_userf_1_t *)leaving_thread, (at_word_t)size);
    }
  }
  
  /* Let each thread start and run for a while to accumulate stack */
  for(i=0; i<2*iterations; i++){
    at_yield();
  }
  
  BR_BARRIER();

  if(BR_HERE()==0){
    /* Push things to 1 */
    for(i=0; i<iterations/2; i++){
      if(BR_push_to(1)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }  
  }
  /* Wait on cluster 1 until all arrive */
  if(BR_HERE()==1){
    while(BR_mig_to!=iterations/2){
      BR_POLL();
    }
  }

  if(BR_HERE()==0){
    /* Push things to 2 */
    for(i=0; i<iterations/2; i++){
      if(BR_push_to(2)){
	at_printf("NOT ENOUGH THREADS TO PUSH\n");
	BR_exit();
      }
    }  
  }
  /* Wait on cluster 1 until all arrive */
  if(BR_HERE()==2){
    while(BR_mig_to!=iterations/2){
      BR_POLL();
    }
  }

  BR_BARRIER();

  if(BR_HERE()==0){
    /* Steal them back - all caches are warm */

    start = gethrtime();
    for(i=0; i<iterations/2; i++){
      /*      BR_steal_from_async(1);
      BR_steal_from_async(2);*/
    }

    /* Wait until done */
    while(BR_migrated_to()<iterations){
      BR_POLL();
    }

    finish = gethrtime();
    
    time_per_thread = (double)(finish-start)/(iterations*1.0e+3);
    fprintf(fp, "THROUGHPUT %e     %e us     %e\n", 
	    ((double)(BR_stack_stolen))/iterations,
	    time_per_thread,
	    1.0e+6/time_per_thread); 
    fclose(fp);
    BR_exit();
  }

  if(BR_HERE()==1){
    while(1){
      BR_POLL();
    }
  }
}



void leaving_thread(int n){
  char buff[n];
  int original_cluster;

  at_mig_enable();
  while(1){
    at_yield();
    BR_POLL();
  }
  
  /*
    at_printf("Sucessful migration from %d to %d\n", original_cluster, BR_HERE()); 
    */
}


void benchmark_steal(int iterations){
  int i=0;
  at_thread_t *t;
  BR_cluster_t from;

    /* Steal benchmark */
  
  if(BR_HERE()==0)
    from = 1;
  else
    from = 0;

  if(BR_HERE()==1){
    /*    for(i=0; i<iterations*5; i++){*/
      t = at_create_1(at_mig_bundle, AT_UNBOUND,  
		      (at_userf_1_t *)leaving_thread, (at_word_t)size);
  }

  BR_BARRIER();
  /* Now spin and till  ping-pongs enough times... */

  if((BR_HERE()==0)||(BR_HERE()==1)){
    start = gethrtime();
    while(1){
      BR_POLL();
      while(BR_steal_from(from)!=BR_MIG_SUCCESS){
	BR_POLL();
      }
      i++;
      if((BR_HERE()==1) && (i==iterations/2)){
	break;
      }
    }
    finish = gethrtime();  
   
    if(BR_HERE()==1){
      fprintf(stdout, "%e     %e\n", (double)(BR_stack_stolen/(iterations/2)),
	      (double)(finish-start)/(iterations*1.0e+3)); 
      fprintf(fp, "%e     %e\n", (double)2*(BR_stack_stolen/iterations),
	      (double)(finish-start)/(iterations*1.0e+3)); 
      fprintf(fp, "Steal overhead %e\n", 
	      (double)(overhead.total/(overhead_times*1.0e+3)));
      fclose(fp);
      BR_exit();
    }
  }
}


void pushed_thread(int n, at_mutex_t *mx){
  char buff[n];
  int original_cluster;

  at_mutex_unlock(mx);
  original_cluster = 0;  /* Threads go from cluster 0*/
  at_mig_enable();

  /*at_printf("STARTED\n");*/
  while(BR_HERE()==original_cluster){
    BR_POLL();
    at_yield();
  }
  
   at_printf("Sucessful migration from %d to %d\n", original_cluster, BR_HERE()); 
}

void benchmark_push(int iterations){
  int i;
  at_thread_t *t;
  at_mutex_t *mx;
  hrtime_t total_time=0;

  mx = at_mutex_create();

  
  while(1){
    /*    at_mutex_lock(mx);*/
    t = at_create_2(at_mig_bundle, AT_UNBOUND,  
	     (at_userf_2_t *)pushed_thread, (at_word_t)size, (at_word_t)mx);
    at_yield();
    /*at_mutex_lock(mx);*/
    start = gethrtime();
    BR_push_to(2);
    total_time += (gethrtime()-start);

    if(BR_migrated_from()==iterations){
      break;
    }    
  }

  fprintf(stderr, "Migrated threads: %d\n", BR_migrated_from());
  fprintf(stdout, "%e     %e\n", (double)(BR_stack_pushed/iterations),
	    (double)total_time/(iterations*1.0e+3)); 
  fprintf(fp, "%e     %e\n", (double)(BR_stack_pushed/iterations),
	    (double)total_time/(iterations*1.0e+3)); 
  fclose(fp);
  BR_exit();
}


void ping_pong_thread(at_word_t iterations, int n){
  int i;
  int started_on;
  volatile char buffer[n];
  hrtime_t start, finish;
  

  
  start = gethrtime();
  started_on = BR_HERE();

  for(i=0; i<iterations; i++){
    if(BR_HERE()==started_on){
      /* Visit another cluster */
      /*at_printf("Hello from home\n");*/
      BR_push_self_to(BR_HERE()+1);
    }
    else {
      /* Go back home */
      /*at_printf("Hello from away\n");*/
      BR_push_self_to(BR_HERE()-1);
    }
  }
  finish = gethrtime();

  if(fp!=stdout){
    fprintf(stdout, "ONEWAY  %e     %e\n", 
	    (double)(2*BR_stack_pushed/iterations),
	    (double)(finish-start)/(iterations*1.0e+3)); 

    fprintf(stdout, "OVERHEAD  %e     %e\n", 
	    (double)(2*BR_stack_pushed/iterations),
	    (double)BR_push_self_stat.total/(iterations*1.0e+3)); 

  }
  fprintf(fp, "ONEWAY  %e     %e\n", 
	  (double)(2*BR_stack_pushed/iterations),
	  (double)(finish-start)/(iterations*1.0e+3)); 
  fprintf(fp, "OVERHEAD  %e     %e\n", 
	  (double)(2*BR_stack_pushed/iterations),
	  (double)BR_push_self_stat.total/(iterations*1.0e+3)); 
  fprintf(fp, "------------------------------------------------------\n");
  fclose(fp);
  BR_exit();
}

void benchmark_push_ping_pong(int iterations){
  at_thread_t *t;

  if(BR_HERE()==0){
    t = at_create_2(at_mig_bundle, AT_UNBOUND,
	   (at_userf_2_t *)ping_pong_thread, (at_word_t)iterations, size);
  }
}



void simple_thread(BR_cluster_t src, at_word_t num){
  int i=0;
  at_printf("STARTED on %d\n", BR_HERE());
  while((BR_HERE()==src)&&(i<10000)){
    i++;
    at_yield();
    at_mig_enable();
    BR_POLL();
  }
  fprintf(stderr, "Hi from cluster %d\n", BR_HERE());
}


void test_termination(int iterations){
  int i;
  
  if(BR_HERE()==0){
    for(i=0; i<iterations; i++){
      at_create_2(at_mig_bundle, AT_MIG_DISABLE, simple_thread, BR_HERE(),i);
    }
  }

  BR_BARRIER();

  BR_steal_term_loop();
}



main(int argc,char *argv[])
{
  int i;
  at_thread_t *t;
  char *fname;

  double total_time,rtt,thruput;

  if(argc>1){
    if((argv[1][0]=='-')&&(argv[1][1]=='h')){
      at_printf("Arguments: <test_number> <stack_size> <iterations>\n");
      at_printf("test_number: 0 - steal\n");
      at_printf("             1 - push\n");
      at_printf("             2 - push ping_pong\n");
      at_printf("             3 - push throughput\n");
      at_printf("             4 - steal throughput\n");
      at_printf("             5 - push throughput 2\n");
      at_printf("             6 - steal throughput 2\n");
      at_printf("             7 - steal/termination test\n");
      exit(0);
    }
    else {
      which = atoi(argv[1]);
    }
  }

  if(argc>2){
    size = atoi(argv[2]);
  }

  if(argc>3){
    threads_to_migrate = atoi(argv[3]);
    threads = threads_to_migrate + 10;
  }

  if(argc>4){
    fname = argv[4];
    fp = fopen(fname, "a+");
  }


  BR_init(3, argc, argv);
  BR_start();
  /* Initialize stacks for use by migration bundles */
  BR_BARRIER();

  BR_mig_stacks_init();

  at_mutex_init(&mx);

  at_mutex_init(&done);
  at_mutex_lock(&done);
  
  BR_BARRIER();

  if(which == 0){
    benchmark_steal(threads_to_migrate);
  }
  
  if(which == 1){
    /* Push benchmark */
    if(BR_HERE()==0){
      benchmark_push(threads_to_migrate);
    }
  }

  if(which == 2){
    benchmark_push_ping_pong(threads_to_migrate);
  }

  if(which == 3){
    benchmark_push_throughput(threads_to_migrate);
  }

  if(which == 4){
    benchmark_steal_throughput(threads_to_migrate);
  }

  if(which == 5){
    benchmark_push_throughput2(threads_to_migrate);
  }

  if(which == 6){
    benchmark_steal_throughput2(threads_to_migrate);
  }

  if(which == 7){
    test_termination(threads_to_migrate);
  }
}






