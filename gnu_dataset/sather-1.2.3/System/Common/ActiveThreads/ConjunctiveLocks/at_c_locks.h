/*------------------------->  ANSI C - headerfile  <-------------------------*/
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

/* This is the common header of all conjunctive locks and their management */
/*
#define AT_CL_THREAD_COUNT
#define AT_CL_PRINT
#define AT_CL_TRACE_LOCKS
#define AT_CL_TRACE_MANAGER
#define AT_CL_TRACE_QUEUE_CHECK
#define AT_CL_TRACE_TAKE_OVER
*/

typedef int at_thread_id;

typedef struct at_generic_c_lock_struct {
   /* used to build lists of locks */
   struct at_generic_c_lock_struct *next_lock;
   /* used to build lists of locks managed by the same manager */
   struct at_generic_c_lock_struct *next_managed;
   /* The queue of requests for this lock */
   struct at_c_lock_queue_entry_struct *queue;
   int length_of_queue;
   /* The current manager */
   struct at_c_lock_manager_struct* manager;
   /* Function pointers corresponding to the methods acquirable, acquire,
    * release, reservation and cancel_reservation of Sather class $LOCK *
    * The additional function acquirable at all returns true, if there might
    * be another thread than the one possibly holding the lock, that can
    * acquire it. E.g. it returns true for a reader/writer lock held by a
    * reader or by no thread at all and it returns false for a reader/writer
    * lock held by a reader */
   int (*acquirable_at_all)(struct at_generic_c_lock_struct*);
   int (*acquirable)(struct at_generic_c_lock_struct*, at_thread_id);
   void (*acquire)(struct at_generic_c_lock_struct*, at_thread_id);
   void (*release)(struct at_generic_c_lock_struct*, at_thread_id);
   void (*reservation)(struct at_generic_c_lock_struct*, at_thread_id);
   void (*cancel_reservation)(struct at_generic_c_lock_struct*, at_thread_id);
} at_generic_c_lock_t;

/* The mutex lock */
typedef struct at_mutex_c_lock_struct {
   struct at_generic_c_lock_struct common;
   /* Thread holding the lock */
   at_thread_id locked_by;
   /* Number of times the thread acquired this lock */
   int locked;
} at_mutex_c_lock_t;

/* The writer lock */
typedef struct at_writer_c_lock_struct {
   struct at_generic_c_lock_struct common;
   /* Writing thread holding the lock */
   at_thread_id locked_by_writer;
   /* Number of times the writing thread acquired this lock */
   int locked;
   /* Number of readers holding the lock */
   int readers;
} at_writer_c_lock_t;

/* The reader lock */
typedef struct at_reader_c_lock_struct {
   struct at_generic_c_lock_struct common;
   /* Reference to the writer managing both locks */
   at_writer_c_lock_t* writer;
} at_reader_c_lock_t;

/* The reader/writer lock. This is not a conjunctive lock itself. */
typedef struct at_rw_c_lock_struct {
   at_generic_c_lock_t* reader;
   at_generic_c_lock_t* writer;
} at_rw_c_lock_t;

typedef struct at_c_lock_queue_entry_struct {
   /* Required for pooling of queue entries */
   at_pool_elt_t* next;
   /* The queue is represented as a linked list */
   struct at_c_lock_queue_entry_struct *next_entry;
   /* A conjunctive request consists of a set of entries in diferent queues.
    * These are linked as a ring */
   struct at_c_lock_queue_entry_struct *linked_entry;
   /* The ID of the blocked thread */
   at_thread_id tid;
   /* The low level lock the thread is blocked by */
   at_mutex_t* mutex;
   /* The lock this entry is waiting for. This attribute seems to be not
    * necessary, because this is an entry in the locks queue, but it is used
    * to handle conjunctive requests */
   at_generic_c_lock_t* c_lock;
} at_c_lock_queue_entry_t;

/* The lock manager */
typedef struct at_c_lock_manager_struct {
   /* Required for pooling of managers */
   at_pool_elt_t* next;
   /* Mutex protecting the manager */
   AT_SPINLOCK_DEC(mutex);
   /* Used to build lists of managers */
   struct at_c_lock_manager_struct *next_manager;
   /* The unidirectional linked list of managed locks */
   at_generic_c_lock_t* list_of_locks;
   int number_of_managed_locks;
   /* This is the minimal number of managed locks. If the number of managed
    * locks is not greater than this number, it may not be splitted. E.g. the
    * number is 2 for a reader/writer lock, where reader and writer have to
    * be managed by the same manager. */
   int number_of_linked_locks;
   /* List of managers that have been taken over */
   struct at_c_lock_manager_struct *replaced_managers;
#ifdef AT_CL_THREAD_COUNT
   int holding_threads_counter;         /* Number of threads holding locks */
   int waiting_threads_counter;         /* Number of blocked threads */
#endif
} at_c_lock_manager_t;

/* Combined locks function prototypes and inlined functions */

/* Pools */
void at_c_locks_init_pools();
void at_c_locks_destroy_pools();

/* Mutex */
at_mutex_c_lock_t* at_mutex_c_lock_create();
void at_mutex_c_lock_init(at_mutex_c_lock_t*);

/* RW */
at_rw_c_lock_t* at_rw_c_lock_create();
void at_rw_c_lock_init(at_rw_c_lock_t*);

/* Manager */
at_c_lock_manager_t* at_c_lock_manager_create(at_generic_c_lock_t**, int);
void at_acquire_single_c_lock(at_generic_c_lock_t*);
int  at_tryacquire_single_c_lock(at_generic_c_lock_t*);
void at_release_single_c_lock(at_generic_c_lock_t*);
void at_acquire_c_locks(at_generic_c_lock_t**, int);
int  at_tryacquire_c_locks(at_generic_c_lock_t**, int);
at_c_lock_manager_t* at_release_c_locks(at_generic_c_lock_t**, int);
int  at_c_lock_manager_split(at_c_lock_manager_t*, at_generic_c_lock_t**, int);

AT_INLINE int at_release_c_locks_and_split_manager(at_generic_c_lock_t** locks,
                                               int size) {
   return at_c_lock_manager_split(at_release_c_locks(locks,size),locks,size);
}

/* The following macros create the arrays needed for conjunctive calls */
#define AT_ACQUIRE_C_LOCKS_1( lock1)                                       \
        at_acquire_single_c_lock(lock1);

#define AT_ACQUIRE_C_LOCKS_2( lock1, lock2)                                \
        do { at_generic_c_lock_t* at_generic_c_lock_array[2] =             \
                 { lock1, lock2 };                                         \
             at_acquire_c_locks( at_generic_c_lock_array, 2);              \
        } while(0);

#define AT_ACQUIRE_C_LOCKS_3( lock1, lock2, lock3)                         \
        do { at_generic_c_lock_t* at_generic_c_lock_array[3] =             \
                 { lock1, lock2, lock3 };                                  \
             at_acquire_c_locks( at_generic_c_lock_array, 3);              \
        } while(0);

#define AT_ACQUIRE_C_LOCKS_4( lock1, lock2, lock3, lock4)                  \
        do { at_generic_c_lock_t* at_generic_c_lock_array[4] =             \
                 { lock1, lock2, lock3, lock4 };                           \
             at_acquire_c_locks( at_generic_c_lock_array, 4);              \
        } while(0);

#define AT_ACQUIRE_C_LOCKS_5( lock1, lock2, lock3, lock4, lock5)           \
        do { at_generic_c_lock_t* at_generic_c_lock_array[5] =             \
                 { lock1, lock2, lock3, lock4, lock5 };                    \
             at_acquire_c_locks( at_generic_c_lock_array, 5);              \
        } while(0);

#define AT_ACQUIRE_C_LOCKS_6( lock1, lock2, lock3, lock4, lock5, lock6)    \
        do { at_generic_c_lock_t* at_generic_c_lock_array[6] =             \
                 { lock1, lock2, lock3, lock4, lock5, lock6 };             \
             at_acquire_c_locks( at_generic_c_lock_array, 6);              \
        } while(0);

#define AT_ACQUIRE_C_LOCKS_7( lock1, lock2, lock3, lock4, lock5, lock6, lock7) \
        do { at_generic_c_lock_t* at_generic_c_lock_array[7] =             \
                 { lock1, lock2, lock3, lock4, lock5, lock6, lock7 };      \
             at_acquire_c_locks( at_generic_c_lock_array, 7);              \
        } while(0);

#define AT_ACQUIRE_C_LOCKS_8( lock1, lock2, lock3, lock4, lock5, lock6, lock7, lock8) \
        do { at_generic_c_lock_t* at_generic_c_lock_array[8] =             \
               { lock1, lock2, lock3, lock4, lock5, lock6, lock7, lock8 }; \
             at_acquire_c_locks( at_generic_c_lock_array, 8);              \
        } while(0);

 
#define AT_RELEASE_C_LOCKS_1( lock1)                                       \
        at_release_single_c_lock(lock1);

#define AT_RELEASE_C_LOCKS_2( lock1, lock2)                                \
        do { at_generic_c_lock_t* at_generic_c_lock_array[2] =             \
                 { lock1, lock2 };                                         \
             at_release_c_locks( at_generic_c_lock_array, 2);              \
        } while(0);

#define AT_RELEASE_C_LOCKS_3( lock1, lock2, lock3)                         \
        do { at_generic_c_lock_t* at_generic_c_lock_array[3] =             \
                 { lock1, lock2, lock3 };                                  \
             at_release_c_locks( at_generic_c_lock_array, 3);              \
        } while(0);

#define AT_RELEASE_C_LOCKS_4( lock1, lock2, lock3, lock4)                  \
        do { at_generic_c_lock_t* at_generic_c_lock_array[4] =             \
                 { lock1, lock2, lock3, lock4 };                           \
             at_release_c_locks( at_generic_c_lock_array, 4);              \
        } while(0);

#define AT_RELEASE_C_LOCKS_5( lock1, lock2, lock3, lock4, lock5)           \
        do { at_generic_c_lock_t* at_generic_c_lock_array[5] =             \
                 { lock1, lock2, lock3, lock4, lock5 };                    \
             at_release_c_locks( at_generic_c_lock_array, 5);              \
        } while(0);

#define AT_RELEASE_C_LOCKS_6( lock1, lock2, lock3, lock4, lock5, lock6)    \
        do { at_generic_c_lock_t* at_generic_c_lock_array[6] =             \
                 { lock1, lock2, lock3, lock4, lock5, lock6 };             \
             at_release_c_locks( at_generic_c_lock_array, 6);              \
        } while(0);

#define AT_RELEASE_C_LOCKS_7( lock1, lock2, lock3, lock4, lock5, lock6, lock7) \
        do { at_generic_c_lock_t* at_generic_c_lock_array[7] =             \
                 { lock1, lock2, lock3, lock4, lock5, lock6, lock7 };      \
             at_release_c_locks( at_generic_c_lock_array, 7);              \
        } while(0);

#define AT_RELEASE_C_LOCKS_8( lock1, lock2, lock3, lock4, lock5, lock6, lock7, lock8) \
        do { at_generic_c_lock_t* at_generic_c_lock_array[8] =             \
               { lock1, lock2, lock3, lock4, lock5, lock6, lock7, lock8 }; \
             at_release_c_locks( at_generic_c_lock_array, 8);              \
        } while(0);

