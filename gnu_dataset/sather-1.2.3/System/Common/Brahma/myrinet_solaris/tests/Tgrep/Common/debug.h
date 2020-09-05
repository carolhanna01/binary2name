#include <stdio.h>
#ifdef PTHREADS_ACTIVE
#include <pthread.h>
#include <sched.h>
#else
#include <thread.h>
#include <synch.h>
#endif
#ifdef __sparc
#include <note.h> /* warlock/locklint */
#else
#define NOTE(s) 
#endif


#define DLEVEL1	0x0001	/* 1 */
#define DLEVEL2	0x0002	/* 2 */
#define DLEVEL3	0x0004	/* 4 */
#define DLEVEL4	0x0008	/* 8 */
#define DLEVEL5	0x0010	/* 16 */
#define DLEVEL6	0x0020  /* 32 */
#define DLEVEL7	0x0040	/* 64 */
#define DLEVEL8	0x0080  /* 128 */
#define DLEVEL9 0xFFFF  /* ALL */
#define DLEVELA 0xFFFF  /* ALL */

#ifdef DEBUG
typedef struct debug_set_st {
    char level;
    int  flag;
    char *name;
} debug_set_t;

debug_set_t debug_set[9] = { '1', DLEVEL1, "Main Thread", 
			     '2', DLEVEL2, "File Limit",
			     '3', DLEVEL3, "Cascade Thread",
			     '4', DLEVEL4, "N/A",
			     '5', DLEVEL5, "Search Thread",
			     '6', DLEVEL6, "Glue functions", 
			     '7', DLEVEL7, "Signals",
			     '8', DLEVEL8, "N/A",
			     '9', DLEVEL9, "All Levels"};

int debug_levels = 0x0000;

#define MAXDEBUG 10
#ifdef PTHREADS_ACTIVE
pthread_mutex_t debug_lock;

#define DP(level, string)  pthread_mutex_lock(&debug_lock); \
                           if (debug_levels & level) { \
                              printf("Thr %d: ", pthread_self()); \
			      printf string ; \
			   } \
                           pthread_mutex_unlock(&debug_lock);
#else
mutex_t debug_lock;

#define DP(level, string)  mutex_lock(&debug_lock); \
                           if (debug_levels & level) { \
                              printf("Thr %d: ", thr_self()); \
			      printf string ; \
			   } \
                           mutex_unlock(&debug_lock);
#endif /* PTHREADS_ACTIVE */
#else
#define DP(level, string)
#endif
NOTE(MUTEX_PROTECTS_DATA(debug_lock, debug_set debug_levels))
