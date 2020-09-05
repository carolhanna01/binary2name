#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <time.h>
#include <sys/systeminfo.h> 
#include "brahma.h"


extern int errno;
BR_SPINLOCK_DEC(go_lck);

void ping_thread(int src, int* arg);
void bulk_get_thread(int src, int* arg);
void bulk_put_thread(int src, int* arg);
static void incr(int src,volatile int *flag);
static void clear(int src,char *buf,int len,void *flag);
static void bulk_get_reply_h(BR_cluster_t src, caddr_t buf, size_t len, 
			     caddr_t cnt);
static void bulk_get(int node,void *l_buf,void *r_buf,int len, int *get_cnt_p);
static void bulk_put_reply_h(int src,volatile int *cnt,int len);
static void bulk_put_handler(BR_cluster_t src, caddr_t buf, size_t len,
			     caddr_t cnt);
static void bulk_put(int node,void *l_buf,void *r_buf,int len, int* put_cnt);
void check_buf(char *data, int a1, int a2, int s);
void remote_check_buf(int src,char *data, int a1, int a2, int s, int* clear_flag);



static void poof(int src, volatile int *flag)
{
  BR_SPINLOCK_LOCK(go_lck);
  (*flag)++; 
  BR_SPINLOCK_UNLOCK(go_lck);
}
  
static int num_pongs = 0;
static void pong(int src, volatile int *return_flag)
{	
  num_pongs++;
  BR_REPLY_4(src, (BR_handler_4_t)poof, (BR_word_t)return_flag,0,0,0); 
}


BR_sema_t thread_sema;
int me, to;
int iterations;
int num_threads = 4;
int procs;

/*Some global stuff for bulk transfer tests */

#define MAX_THREADS 20
#define SIZES 10
#define ALIGNMENTS 4
#define CLEAR_TAIL 32

#define TEST_PATTERN(x) ((x)|(x<<1)|(x<<2)|0x77) 

#define ALL_ALL_SIZE 512

int get_cnts[MAX_THREADS],put_cnts[MAX_THREADS];
int clear_flags[MAX_THREADS];  

char buffer1[MAX_THREADS][20*16384];
char buffer2[MAX_THREADS][20*16384];

int sizes[SIZES];
int am_self_address;
int am_max_pkt;

int thr_numbers[MAX_THREADS];
char host_name[256];

main(int argc,char *argv[],char *envp[])
{
  int i;
  char my_name[64];
  int thr_number;

  gethostname(host_name, 256);
  BR_init(3, argc, argv);
  BR_start();

  thread_sema = BR_SEMA_CREATE(0);
  BR_SPINLOCK_INIT(go_lck);

#ifdef DEBUG
  sysinfo(SI_HOSTNAME,my_name,64);
  printf("node %d running on %s\n",me,my_name);
  printf("main: calling am_enable()\n" );
#endif 

  me = BR_HERE();
  procs = BR_CLUSTERS();
  printf("node %d passed barrier \n", me);


  to=1;
  iterations = 1000;

  if (argc > 1) {
    to = atoi(argv[1]);
  }

  if (argc > 2 ) 
    iterations = atoi(argv[2]);

  if (argc > 3)
      num_threads = atoi(argv[3]);
  
  am_max_pkt= BR_MAX_XFER();
  
  for(i=0; i<num_threads; i++){
    thr_numbers[i] = i;
  }

  /* Set message sizes for bulk transfer tests */
  sizes[0]=1; 
  sizes[1]=4; sizes[2]=16; sizes[3]=37; sizes[4]=am_max_pkt/4; 
  sizes[5]=am_max_pkt; sizes[6]=2*am_max_pkt; sizes[7] = (4*am_max_pkt/3); 
  sizes[8]=(10*am_max_pkt); sizes[9]=1;

  BR_BARRIER();

  printf("node %d passed 2nd barrier \n",me,my_name);

  /* for fun, fork off remote threads in a round robin fashion */
  for(thr_number=0; thr_number<num_threads; thr_number++){
    BR_FORK_1((me+1)%procs, ping_thread, (int)&thr_numbers[thr_number]); 
    /*    thr_create(NULL, 0, ping_thread, NULL, 0L, NULL);  */
  }

  /* wait until all request/reply threads terminate */
  for(i=0; i<num_threads; i++){
    while (!BR_TRY_WAIT(thread_sema)) {
      BR_POLL();
      BR_THREAD_YIELD();
    }
  }

  printf("[%d] test get from 0 to %d \n",me, to);


  BR_BARRIER();
  
  for(thr_number=0; thr_number<num_threads; thr_number++){
    /*thr_create(NULL, 0, bulk_get_thread, (void *)&thr_numbers[thr_number], 0L, NULL); */
    BR_FORK_1(me, bulk_get_thread, (int)&thr_numbers[thr_number]); 
  }
  
  BR_BARRIER();

  /* wait until all bulk threads terminate */
  for(i=0; i<num_threads; i++){
    while (!BR_TRY_WAIT(thread_sema)) {
      BR_POLL();
      BR_THREAD_YIELD();
    }
  }

  BR_BARRIER();

  /* Test concurrent puts */
  
  for(thr_number=0; thr_number<num_threads; thr_number++){
    /*       thr_create(NULL, 0, bulk_put_thread, (void *)&thr_numbers[thr_number], 0L, NULL); */
    BR_FORK_1(me, bulk_put_thread, (int)&thr_numbers[thr_number]);   
  }

  /* wait until all bulk threads terminate */
  for(i=0; i<num_threads; i++){
    while (!BR_TRY_WAIT(thread_sema)) {
      BR_POLL();
      BR_THREAD_YIELD();
    }
  }

  BR_BARRIER();

  printf("\n%s done!\n", host_name);
  if (BR_HERE()==0){
    BR_exit();
  }
}


void ping_thread(int src, int* thr_number_p) {
  int i,j,k;
  static volatile int go;
  volatile int done1;
  struct timespec begin,end,lapsed;
  double total_time,rtt;

  /*  printf("%s ping_thread #%d started\n", host_name, *thr_number_p); 
  fflush(stdout); */

  if (me != to ) {

    printf("Round trip test from %d to %d\n\n",me,to);
    printf(" Timing full round trip time \n");

    clock_gettime(CLOCK_REALTIME,&begin);   
    for (i=0;i<iterations;i++){
      done1 =0;
      BR_REQUEST_4(to, (BR_handler_4_t) pong, (BR_word_t)&done1, 0, 0, 0);   
      while (done1 ==0) 
	BR_POLL();   
    }    
    clock_gettime(CLOCK_REALTIME,&end);    

    if (begin.tv_nsec > end.tv_nsec) {
      end.tv_nsec += 1000000000;
      end.tv_sec--;
    }


    lapsed.tv_nsec = end.tv_nsec - begin.tv_nsec;
    lapsed.tv_sec =  end.tv_sec - begin.tv_sec;
    
    total_time = (double)lapsed.tv_sec*1000000000.00+(double)lapsed.tv_nsec;
    total_time = total_time/1000.00;

    rtt = total_time/iterations;
     
    printf("total %.3lf, average full rtt = %.3lf usec \n",total_time,rtt);

    BR_REQUEST_1(to,(BR_handler_1_t)poof, (BR_word_t)&go);  
  }
    else {
      while(go < num_threads*(BR_CLUSTERS()-1)) {
      BR_POLL();
    }
  }

  BR_SIGNAL(thread_sema);
  printf("%s ping_thread #%d terminated\n", host_name, *thr_number_p);
  fflush(stdout);
  
}


void bulk_get_thread(int src, int* thr_number_p) {
  int i,j,k,s;
  static volatile int go;
  volatile int done1;
  int thr_number;
  char *buf1, *buf2;

  thr_number = *((int *)thr_number_p);
  /*  printf("Bulk_thread #%d\n", thr_number); */

/* do a series of gets/puts between 0 and other */
  buf1=&buffer1[thr_number][4];
  buf2=buffer2[thr_number];

  for (i=0;i<20*am_max_pkt;i++) {
    buf2[i] = (char) (TEST_PATTERN(i));
  }

  if (me == 0 ) {
    if (thr_number == 0){
      printf("\nNode %d testing get from %d \n",me,to);
    }
    printf(".");

    for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
      for (k=0;k<ALIGNMENTS;k++) { 
	for (s=0;s<SIZES;s++) {  
	  memset(buffer1[thr_number],0,sizes[s]+CLEAR_TAIL);
	  get_cnts[thr_number]=0;
	  bulk_get(to,(void *)&buf1[i],(void *) &buf2[k],sizes[s], 
		   &get_cnts[thr_number]);
	  gam_wait(&get_cnts[thr_number],sizes[s]);
	  check_buf(buf1,i,k,sizes[s]);
	}
      }
    }   
  }

  BR_SIGNAL(thread_sema);
  printf("bulk_get_thread #%d terminated\n", *thr_number_p);
  fflush(stdout); 
}

void bulk_put_thread(int src, int* thr_number_p) {
  int i,j,k,s;
  static volatile int go;
  volatile int done1;
  int thr_number;
  char *buf1, *buf2;

  thr_number = *((int *)thr_number_p);
  /*  printf("Bulk_put_thread #%d\n", thr_number); */

/* do a series of gets/puts between 0 and other */
  buf1=&buffer1[thr_number][4];
  buf2=buffer2[thr_number];

  for (i=0;i<20*am_max_pkt;i++) {
    buf2[i] = (char) (TEST_PATTERN(i));
  }

  
  if (me == 0){
    if (thr_number == 0){
      printf("\nNode %d testing put to %d \n",me,to);
    }
    printf(".");
    
    for (i=0;i<ALIGNMENTS;i++) {      /* for all alignments and sizes */
      for (k=0;k<ALIGNMENTS;k++) { 
	for (s=0;s<SIZES;s++) {  
	  clear_flags[thr_number] = 0;
	  put_cnts[thr_number] =0;
	  BR_REQUEST_4(to,
		       (BR_handler_4_t)clear, 
		       (BR_word_t)(buffer1[thr_number]),
		       (BR_word_t)sizes[s]+CLEAR_TAIL,
		       (BR_word_t)&clear_flags[thr_number],
		       0);
	  gam_wait(&clear_flags[thr_number],1);
	  
	  bulk_put(to,(void *)&buf2[k],(void *) &buf1[i],sizes[s], 
		   &put_cnts[thr_number]);
	  gam_wait(&put_cnts[thr_number], sizes[s]);
	  BR_REQUEST_5(to,
		       (BR_handler_5_t)remote_check_buf,
		       (BR_word_t)buf1,
		       (BR_word_t)i,
		       (BR_word_t)k,
		       (BR_word_t)sizes[s],
		       (BR_word_t)&clear_flags[thr_number]);
	  gam_wait(&clear_flags[thr_number],1);
	}
      }
    }
  }
    
  BR_SIGNAL(thread_sema);
  printf("bulk_put_thread #%d terminated\n", *thr_number_p);
  fflush(stdout); 
}

static void incr(int src,volatile int *flag) 
{
  (*flag)++;
}

static void clear(int src,char *buf,int len,void *flag)
{
  memset(buf,0,len);
  BR_REPLY_1(src,(BR_handler_4_t)incr,(BR_word_t)flag);
}

/* get functions
 * on the receiving side, the sink function. Just increment the 
 * get counter 
 */

static void bulk_get_reply_h(BR_cluster_t src, caddr_t buf, size_t len, 
			     caddr_t cnt)
{
  /* This is serialized by am_poll */
  (*((int *)cnt))+=len;
}

static void bulk_get(int node,void *l_buf,void *r_buf,int len,int* get_cnt)
{
  int i; 
  
  if (len <= am_max_pkt) { /* fast check, most often < am_max_pkt */
    BR_GET(node,r_buf,l_buf,len,bulk_get_reply_h, (caddr_t)get_cnt);
  }
  else {   /* chunk the data into maximum packet sizes */
    for (i=am_max_pkt;i<len;i+=am_max_pkt) {
      BR_GET(node,r_buf,l_buf,am_max_pkt,bulk_get_reply_h, (caddr_t)get_cnt);
      (char *) l_buf+= am_max_pkt; (char *) r_buf+= am_max_pkt;
    }
    i-=am_max_pkt;
    /*get the last packet */
    BR_GET(node,r_buf,l_buf,len-i,bulk_get_reply_h, (caddr_t)get_cnt);
  }
} 


/* put functions 
 * The protocol is for the sender to continually store into the 
 * node. Each ack returns the amount put. 
 */

static void bulk_put_reply_h(int src,volatile int *cnt,int len)
{
  (*cnt)+=len;
}

static void bulk_put_handler(BR_cluster_t src, caddr_t buf, size_t len,
			     caddr_t cnt)
{
  BR_REPLY_2(src,(BR_handler_4_t)bulk_put_reply_h,(BR_word_t)cnt,
	     (BR_word_t)len);
}

static void bulk_put(int node,void *l_buf,void *r_buf,int len, int* put_cnt)
{
  int i;

  if( len <= am_max_pkt) {
    BR_STORE(node,l_buf,r_buf,len,bulk_put_handler,(caddr_t)put_cnt);
  }
  else {
    /* chunk the data into maximum packet sizes */
    for (i=am_max_pkt;i<len;i+=am_max_pkt) {
      BR_STORE(node,l_buf,r_buf,am_max_pkt,bulk_put_handler,(caddr_t)put_cnt);
      (char *) l_buf+= am_max_pkt; (char *)r_buf+= am_max_pkt;
    }
    i-=am_max_pkt;
    /*put the last packet */
    BR_STORE(node,l_buf,r_buf,len-i,bulk_put_handler,(caddr_t)put_cnt);
  }

}

void check_buf(char *data, int a1, int a2, int s)
{
  int i,j;
  char c;
  int start_region, end_region;

  /* check to make sure didn't store something too soon*/

  start_region =-1;

  if (data[-1] !=0 )
    printf("node %d pre condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);  

  for (i=a1,j=a2;i<(s+a1);i++,j++) {

    c=(char) (TEST_PATTERN(j));

    if (data[i] !=c ) {
      if (start_region <0) {
	start_region = i;
	printf("node %d local check failed for size %d alignments %d %d at %d\n",
	       am_self_address, s,a1,a2,i);
      }
    }
    else {
      if (start_region >0) {
	end_region =i-1;
	printf("node %d failed region [%d:%d]\n",am_self_address,
	       start_region,end_region);
	start_region = -1;
      }
    }

  }

  /* check the last char, should be zero */
  if (data[i] !=0 )
    printf("node %d end condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);


} /* check_buf */

void remote_check_buf(int src,char *data, int a1, int a2, int s, int* clear_flag)
{
  int i,j;
  char c;

  /* check to make sure didn't store something too soon*/

  if (data[-1] !=0 )
    printf("node %d pre condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);  

  for (i=a1,j=a2;i<(s+a1);i++,j++) {

    c=(char) (TEST_PATTERN(j));

    if (data[i] !=c ) {
      printf("node %d remote check failed for size %d alignments %d %d at %d\n",
	     am_self_address, s,a1,a2,i);
      goto end;
    }

  }

  /* check the last char, should be zero */
  if (data[i] !=0 )
    printf("node %d end condition failed for size %d alignments %d %d\n",
	   am_self_address, s,a1,a2);

 end: BR_REPLY_1(src, (BR_handler_4_t)incr, (BR_word_t)clear_flag);
}









