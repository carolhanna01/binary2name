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

#include "at.h"
#include "at_c_locks.h"

/* Static pointers to the queue entry pool and the lock manager pool */
static at_pool_t* at_c_locks_queue_entry_pool = NULL;
static at_pool_t* at_c_locks_manager_pool = NULL;

/* Define initial pool sizes */
#define AT_CL_INITIAL_QUEUE_ENTRY_POOL_SIZE    400
#define AT_CL_INITIAL_MANAGER_POOL_SIZE        100

/* Define size of temporary manager array on stack */
#define AT_CL_MANAGERS_ARRAY_SIZE_ON_STACK     8

/* Prototypes of static functions */

/* Queue Entry */
static at_c_lock_queue_entry_t* create_queue_entry();
static void init_queue_entry(at_c_lock_queue_entry_t*);

/* Manager */
static void init_manager(at_c_lock_manager_t*,at_generic_c_lock_t**,int);
static void check_queues(at_c_lock_manager_t*);
static int check_linked_queue_entries(at_c_lock_queue_entry_t*,
                                      at_c_lock_queue_entry_t*);
static void enqueue_conjunctive_request(at_generic_c_lock_t**, int,
                                        at_thread_id, at_mutex_t*);
static at_c_lock_manager_t* get_manager(at_generic_c_lock_t**, int);
static void take_over(at_c_lock_manager_t*, at_generic_c_lock_t**, int);
static int try_straight_take_over(at_c_lock_manager_t*,
                                  at_generic_c_lock_t**, int);
static void ordered_take_over(at_c_lock_manager_t*,at_generic_c_lock_t**, int);
static void split_manager(at_c_lock_manager_t* shared_manager,
                          at_generic_c_lock_t** locks, int number_of_locks);
static void print_queues(at_c_lock_manager_t*);


/* Pools: init */
void at_c_locks_init_pools() {
   /* Create queue entry pool */
   at_c_locks_queue_entry_pool = at_pool_create(
      AT_CL_INITIAL_QUEUE_ENTRY_POOL_SIZE,sizeof(at_c_lock_queue_entry_t));
   /* Create manager pool */
   at_c_locks_manager_pool = at_pool_create(
      AT_CL_INITIAL_MANAGER_POOL_SIZE,sizeof(at_c_lock_manager_t));
}

/* Pools: destroy */
void at_c_locks_destroy_pools() {
   /* Destroy the pools and set the pool pointers to NULL */
   at_pool_destroy(at_c_locks_queue_entry_pool);
   at_c_locks_queue_entry_pool = NULL;
   at_pool_destroy(at_c_locks_manager_pool);
   at_c_locks_manager_pool = NULL;
}


/* Queue entry: create */
static at_c_lock_queue_entry_t* create_queue_entry() {
   at_c_lock_queue_entry_t* new_entry;
   /* Allocate memory. Use pool, if it has been initialized. */
   if (at_c_locks_queue_entry_pool != NULL)
      new_entry = (at_c_lock_queue_entry_t*)
                          at_pool_get(at_c_locks_queue_entry_pool);
   else
      new_entry = (at_c_lock_queue_entry_t*)
                          malloc(sizeof(struct at_c_lock_queue_entry_struct));
#ifdef AT_CL_TRACE_MANAGER
   printf("Queue entry %p: created by thread %d.\n", new_entry, at_self()->id);
#endif
   /* initialize attributes */
   init_queue_entry(new_entry);
   return (new_entry);
}

/* Queue entry: init */
static void init_queue_entry(at_c_lock_queue_entry_t* self) {
#ifdef AT_CL_TRACE_MANAGER
   printf("Queue entry %p: init called by thread %d.\n", self, at_self()->id);
#endif
   self->next_entry = NULL;
}

/* Lock Manager: create */
at_c_lock_manager_t* at_c_lock_manager_create(
                        at_generic_c_lock_t** locks, int number_of_locks) {
   at_c_lock_manager_t* new_manager;
   /* Allocate memory Use pool, if it has been initialized. */
   if (at_c_locks_manager_pool != NULL)
      new_manager = (at_c_lock_manager_t*) at_pool_get(at_c_locks_manager_pool);
   else
      new_manager = (at_c_lock_manager_t*) malloc(
                         sizeof(struct at_c_lock_manager_struct));
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: created by thread %d.\n", new_manager, at_self()->id);
#endif
   /* initialize attributes */
   init_manager(new_manager,locks,number_of_locks);
   return (new_manager);
} /* end: at_c_lock_manager_create */

/* Lock Manager: init */
static void init_manager(at_c_lock_manager_t* self,
                        at_generic_c_lock_t** locks, int number_of_locks) {
   int i; /* loop counter */
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: init called by thread %d.\n", self, at_self()->id);
#endif
   AT_SPINLOCK_INIT(self->mutex);
   self->next_manager = NULL;
   /* Is there a list of locks */
   if (locks == NULL  ||  number_of_locks < 1)
      self->list_of_locks = NULL;
   else {
      /* Set the head of the list of locks */
      self->list_of_locks = locks[0];
      /* Append all other locks to the list */
      for ( i=1; i<number_of_locks; i++) {
         locks[i-1]->next_managed = locks[i];
         /* Set the lock's pointer to its manager */
         locks[i-1]->manager = self;
      }
      /* Terminate the list */
      locks[i-1]->next_managed = NULL;
      /* Set the lock's pointer to its manager */
      locks[i-1]->manager = self;
   }
   /* Set the number of managed locks */
   self->number_of_managed_locks = number_of_locks;
   /* This manager has not replaced another one yet */
   self->replaced_managers = NULL;
#ifdef AT_CL_DEBUG
   self->holding_threads_counter = 0;
   self->waiting_threads_counter = 0;
#endif
#ifdef AT_CL_PRINT
   print_queues(self);
#endif
} /* end: init_manager */

/* Lock Manager: acquire a single conjunctive lock */
void at_acquire_single_c_lock(at_generic_c_lock_t* lock) {
      at_mutex_t *new_mutex;
      at_c_lock_queue_entry_t* queue_entry;
   /* Keep the thread's ID */
   at_thread_id tid = at_self()->id;
   /* Lock the first lock's manager: Since the locks's manager might be
    * changed while we are waiting for the spinlock, we have to check after
    * acquiring the spinlock, if the manager remained unchanged */
   at_c_lock_manager_t* manager;
   while (1) {
      manager = lock->manager;
      AT_SPINLOCK_LOCK(manager->mutex);
      /* Got the right manager? */
      if (manager == lock->manager) break;
      /* No, it is the wrong one. Unlock it and try again */
      AT_SPINLOCK_UNLOCK(manager->mutex);
   }
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: acquire single called by thread %d.\n", manager, tid);
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Is the lock acqirable? */
   if ((*(lock->acquirable))(lock,tid)) {
      /* Yes, acquire it! */
      (*(lock->acquire))(lock,tid);
#ifdef AT_CL_THREAD_COUNT
      manager->holding_threads_counter++;
#endif
#ifdef AT_CL_PRINT
      print_queues(manager);
#endif
      /* Unlock the manager */
      AT_SPINLOCK_UNLOCK(manager->mutex);
      return;
   } else {
/*       at_mutex_t *new_mutex; */
/*       at_c_lock_queue_entry_t* queue_entry; */
      /* No, notify the lock and then create a request entry in the
       * locks queue and block the thread! */
      /* Is there a method reservation? If yes, call it now */
      if (lock->reservation != NULL)
         (*(lock->reservation))(lock,tid);
      queue_entry = create_queue_entry();
      /* This single request has no linked entries */
      queue_entry->linked_entry = NULL;
      queue_entry->tid = tid;
      /* Attribute c_lock need not to be set,
       * since it is used only for conjunctive requests */
      /* If the queue has been empty, this becomes the new head of it */
      if (lock->queue == NULL) lock->queue = queue_entry;
      else {
         /* The queue is not empty. Go to its end */
         at_c_lock_queue_entry_t* end_of_queue = lock->queue;
         while (end_of_queue->next_entry != NULL)
            end_of_queue = end_of_queue->next_entry;
         /* append new entry */
         end_of_queue->next_entry = queue_entry;
      }
      /* Create a locked low level mutex */
      new_mutex = at_mutex_create();
      queue_entry->mutex = new_mutex;
      at_mutex_lock(new_mutex);
#ifdef AT_CL_THREAD_COUNT
      manager->waiting_threads_counter++;
#endif
#ifdef AT_CL_PRINT
      print_queues(manager);
#endif
      /* Unlock the lock manager */
      AT_SPINLOCK_UNLOCK(manager->mutex);
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: thread %d blocked.\n", manager, tid);
#endif
      /* Block the thread */
      at_mutex_lock(new_mutex);
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: thread %d resumed.\n", manager, tid);
#endif
   }
} /* end: at_acquire_single_c_lock */

/* Lock Manager: try to acquire a single conjunctive lock */
int at_tryacquire_single_c_lock(at_generic_c_lock_t* lock) {
   /* Keep the thread's ID */
   at_thread_id tid = at_self()->id;
   /* Lock the first lock's manager: Since the locks's manager might be
    * changed while we are waiting for the spinlock, we have to check after
    * acquiring the spinlock, if the manager remained unchanged */
   at_c_lock_manager_t* manager;
   while (1) {
      manager = lock->manager;
      AT_SPINLOCK_LOCK(manager->mutex);
      /* Got the right manager? */
      if (manager == lock->manager) break;
      /* No, it is the wrong one. Unlock it and try again */
      AT_SPINLOCK_UNLOCK(manager->mutex);
   }
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: try single called by thread %d.\n", manager, tid);
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Is the lock acqirable? */
   if ((*(lock->acquirable))(lock,tid)) {
      /* Yes, acquire it! */
      (*(lock->acquire))(lock,tid);
#ifdef AT_CL_THREAD_COUNT
      manager->holding_threads_counter++;
#endif
#ifdef AT_CL_PRINT
      print_queues(manager);
#endif
      /* Unlock the manager */
      AT_SPINLOCK_UNLOCK(manager->mutex);
      return (1);
   }
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Unlock the manager */
   AT_SPINLOCK_UNLOCK(manager->mutex);
   return (0);
} /* end: at_tryacquire_single_c_lock */

/* Lock Manager: release a single conjunctive lock */
void at_release_single_c_lock(at_generic_c_lock_t* lock) {
   /* Lock the first lock's manager: Since the locks's manager might be
    * changed while we are waiting for the spinlock, we have to check after
    * acquiring the spinlock, if the manager remained unchanged */
   at_c_lock_manager_t* manager;
   while (1) {
      manager = lock->manager;
      AT_SPINLOCK_LOCK(manager->mutex);
      /* Got the right manager? */
      if (manager == lock->manager) break;
      /* No, it is the wrong one. Unlock it and try again */
      AT_SPINLOCK_UNLOCK(manager->mutex);
   }
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: release single called by thread %d.\n",
          manager, at_self()->id);
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Release the lock */
   (*(lock->release))(lock,at_self()->id);
#ifdef AT_CL_THREAD_COUNT
   manager->holding_threads_counter--;
#endif
   /* check the lock's queue */
   check_queues(manager);
   /* Unlock the manager */
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   AT_SPINLOCK_UNLOCK(manager->mutex);
} /* end: at_release_single_c_lock */

/* Lock Manager: Check all queues for threads that can acquire locks */
static void check_queues(at_c_lock_manager_t* manager) {
         at_c_lock_queue_entry_t* queue_entry;
   /* Current lock having its queue scanned */
   at_generic_c_lock_t* lock;
   /* Loop over all managed locks */
   for (lock = manager->list_of_locks; lock!=NULL; lock = lock->next_managed) {
      /* Pointer to the current queue entry */
      at_c_lock_queue_entry_t* *queue_entry_p = &(lock->queue);
      while (*queue_entry_p != NULL) {
         /* For better readability */
/*          at_c_lock_queue_entry_t* queue_entry = *queue_entry_p; */
         queue_entry = *queue_entry_p;
         /* Can the waiting thread acquire the lock? */
         if (!((*(lock->acquirable))(lock,queue_entry->tid)))
         {
            /* Update entry pointer and contimue */
            queue_entry_p = &(queue_entry->next_entry);
            continue;
         }
         /* It can be acquired. Are there any linked entries? */
         if (queue_entry->linked_entry != NULL) {
            /* Try all linked entries, if they can acquire their locks */
            if (!check_linked_queue_entries(queue_entry->linked_entry,
                                            queue_entry)) {
               /* At least one of them cannot be acquired.
                * Update the entry pointer and continue */
               queue_entry_p = &(queue_entry->next_entry);
               continue;
            }
         }
         /* Acquire the lock */
         (*(lock->acquire))(lock,queue_entry->tid);
         /* Unlock the thread */
         at_mutex_unlock(queue_entry->mutex);
#ifdef AT_CL_THREAD_COUNT
         manager->waiting_threads_counter--;
         manager->holding_threads_counter++;
#endif
         /* Remove the entry from the queue */
         *queue_entry_p = queue_entry->next_entry;
         /* The entry is not needed any more. Return to pool or free */
         if (at_c_locks_queue_entry_pool != NULL)
            at_pool_put(at_c_locks_queue_entry_pool,queue_entry);
         else
            free(queue_entry);
         /* Do not update the entry pointer, since entry removed */
      } while (*queue_entry_p != NULL);
   }
} /* end: check_queues */

/* Lock Manager: Check a set of linked queue entries. Acquire all or none */
static int check_linked_queue_entries( at_c_lock_queue_entry_t* entry,
                                    at_c_lock_queue_entry_t* first_entry) {
      at_c_lock_queue_entry_t* cursor;
   /* Can the waiting thread acquire the lock? */
   if (!((*(entry->c_lock->acquirable))(entry->c_lock,entry->tid)))
       /* It cannot. Return 'false' */
       return (0);
   /* Have all entries been checked? All entries are linked as a ring */
   if (entry->linked_entry != first_entry)
      /* Check all entries recursively */
      if (!check_linked_queue_entries(entry->linked_entry,first_entry))
         /* Some of the linked entries cannot acquire its lock */
         return (0);
   /* All entries can acquire their corresponding lock. Acquire this one */
   /* Only the first entry is not acquired here. It is acquired by the calling
    * function which also unlocks the thread */
   if (entry == first_entry) return (1);
   (*(entry->c_lock->acquire))(entry->c_lock,entry->tid);
   /* Is there a method cancel_reservation? If yes, call it now */
   if (entry->c_lock->cancel_reservation != NULL)
      (*(entry->c_lock->cancel_reservation))(entry->c_lock,entry->tid);
   /* Remove the entry from the list. This is a little bit circumstancial,
    * because the queue is not double linked */
   if (entry == entry->c_lock->queue)
      entry->c_lock->queue = entry->next_entry;
   else {
      /* at_c_lock_queue_entry_t* cursor = entry->c_lock->queue; */
      cursor = entry->c_lock->queue;
      while (cursor->next_entry != entry)
         cursor = cursor->next_entry;
      cursor->next_entry = entry->next_entry;
   }
   /* The entry is not used any more. Return to pool or free */
   if (at_c_locks_queue_entry_pool != NULL)
      at_pool_put(at_c_locks_queue_entry_pool,entry);
   else
      free(entry);
   return (1);
} /* end: check_linked_queue_entries */

/* Lock Manager: acquire conjunctive locks */
void at_acquire_c_locks(at_generic_c_lock_t** locks, int number_of_locks)
{
   int i; /* loop counter */
   /* Keep the thread's ID */
   at_thread_id tid = at_self()->id;
   /* Lock the locks' manager. If necessary, create a new one */
   at_c_lock_manager_t* manager = get_manager(locks, number_of_locks);
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: acquire called by thread %d.\n", manager, tid);
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Check if all locks are acqirable */
   for ( i=0; i<number_of_locks; i++)
      /* Is this lock acquirable? */
      if (!((*(locks[i]->acquirable))(locks[i],tid))) {
         /* It's not. Enqueue the thread at all locks' queues and block it */
         /* Create a locked low level mutex */
         at_mutex_t* new_mutex = at_mutex_create();
         at_mutex_lock(new_mutex);
         enqueue_conjunctive_request(locks,number_of_locks,tid,new_mutex);
#ifdef AT_CL_THREAD_COUNT
         manager->waiting_threads_counter++;
#endif
#ifdef AT_CL_PRINT
         print_queues(manager);
#endif
         /* Unlock the lock manager */
         AT_SPINLOCK_UNLOCK(manager->mutex);
         /* Block the thread */
         at_mutex_lock(new_mutex);
         return;
      }
   /* All locks are acquirable. Acquire them! */
   for ( i=0; i<number_of_locks; i++)
      (*(locks[i]->acquire))(locks[i],tid);
#ifdef AT_CL_THREAD_COUNT
   manager->holding_threads_counter++;
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Unlock the manager */
   AT_SPINLOCK_UNLOCK(manager->mutex);
} /* end: at_acquire_c_locks */

/* Lock Manager: enqueue thread tid at the locks' queues */
static void enqueue_conjunctive_request(at_generic_c_lock_t** locks,
                   int number_of_locks, at_thread_id tid, at_mutex_t* mutex) {
   int i; /* loop counter */
   at_c_lock_queue_entry_t* first_entry;
   at_c_lock_queue_entry_t* previous_entry = NULL;
   /* Loop over all locks */
   for ( i=0; i<number_of_locks; i++) {
      at_generic_c_lock_t* cursor = locks[i];
      at_c_lock_queue_entry_t* new_entry;
      /* Is there a method called reservation? If yes, call it */
      if (cursor->reservation != NULL)
         (*(cursor->reservation))(cursor,tid);
      /* Create an entry for this lock's queue */
      new_entry = create_queue_entry();
      /* Create the ring of all linked entries */
      if (previous_entry == NULL)
         first_entry = new_entry;
      else
         new_entry->linked_entry = previous_entry;
      previous_entry = new_entry;
      new_entry->tid = tid;
      new_entry->mutex = mutex;
      new_entry->c_lock = cursor;
      /* If the queue has been empty, this becomes the new head of it */
      if (cursor->queue == NULL) cursor->queue = new_entry;
      else {
         /* The queue is not empty. Go to its end */
         at_c_lock_queue_entry_t* end_of_queue = cursor->queue;
         while (end_of_queue->next_entry != NULL)
            end_of_queue = end_of_queue->next_entry;
         /* Append new entry */
         end_of_queue->next_entry = new_entry;
      }
   }
   /* Complete the ring */
   first_entry->linked_entry = previous_entry;
} /* end: enqueue_requests */

/* Lock Manager: try to acquire conjunctive locks */
int at_tryacquire_c_locks(at_generic_c_lock_t** locks,
                             int number_of_locks) {
   int i; /* loop counter */
   /* Keep the thread's ID */
   at_thread_id tid = at_self()->id;
   /* Lock the locks' manager. If necessary, create a new one */
   at_c_lock_manager_t* manager = get_manager(locks, number_of_locks);
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: try called by thread %d.\n", manager, tid);
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Check if all locks are acqirable? */
   for ( i=0; i<number_of_locks; i++)
      /* Is this lock acquireable? */
      if (!((*(locks[i]->acquirable))(locks[i],tid))) {
         /* It's not. The try failed */
#ifdef AT_CL_PRINT
         print_queues(manager);
#endif
         /* Unlock the lock manager */
         AT_SPINLOCK_UNLOCK(manager->mutex);
         return (0);
      }
   /* All locks are acquirable. Acquire them! */
   for ( i=0; i<number_of_locks; i++)
      (*(locks[i]->acquire))(locks[i],tid);
#ifdef AT_CL_THREAD_COUNT
   manager->holding_threads_counter++;
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Unlock the manager */
   AT_SPINLOCK_UNLOCK(manager->mutex);
   return (1);
} /* end: at_tryacquire_c_lock */

/* Lock Manager: release conjunctive locks */
at_c_lock_manager_t* at_release_c_locks(at_generic_c_lock_t** locks,
                                        int number_of_locks) {
   int i; /* loop counter */
   /* Keep the thread's ID */
   at_thread_id tid = at_self()->id;
   /* Lock the locks' manager. If necessary, create a new one */
   at_c_lock_manager_t* manager = get_manager(locks,number_of_locks);
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: release called by thread %d.\n", manager, tid);
#endif
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Loop over all locks */
   for ( i=0; i<number_of_locks; i++)
      /* Release the lock */
      (*(locks[i]->release))(locks[i],tid);
#ifdef AT_CL_THREAD_COUNT
   manager->holding_threads_counter--;
#endif
   /* check the queues for waiting threads */
   check_queues(manager);
#ifdef AT_CL_PRINT
   print_queues(manager);
#endif
   /* Unlock the manager */
   AT_SPINLOCK_UNLOCK(manager->mutex);
   return (manager);
} /* end: at_release_c_lock */

/* Lock Manager: Get the manager of a lock or a set of locks. If not all
 * locks of a set are managed by the same manager, create a new one that
 * takes over the others */
static at_c_lock_manager_t* get_manager(at_generic_c_lock_t** locks,
                                           int number_of_locks) {
   int i; /* loop counter */
   at_c_lock_manager_t* manager;
#ifdef AT_CL_TRACE_MANAGER
   printf("Lock %p: get called by thread %d.\n", locks, at_self()->id);
#endif
   /* Lock the first lock's manager: Since the locks's manager might be
    * changed while we are waiting for the spinlock, we have to check after
    * acquiring the spinlock, if the manager remained unchanged */
   while (1) {
      manager = locks[0]->manager;
      AT_SPINLOCK_LOCK(manager->mutex);
      /* Got the right manager? */
      if (manager == locks[0]->manager) break;
      /* No, it is the wrong one. Unlock it and try again */
      AT_SPINLOCK_UNLOCK(manager->mutex);
   }
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: found by thread %d.\n", manager, at_self()->id);
#endif
   /* Does the manager manage all locks? */
   for ( i=0; i<number_of_locks; i++)
      if (locks[i]->manager != manager) {
         /* The manager does not control all locks. */
#ifdef AT_CL_TRACE_MANAGER
         printf("Manager %p: get failed for thread %d.\n",
                                                     manager, at_self()->id);
#endif
         /* Release the old manager */
         AT_SPINLOCK_UNLOCK(manager->mutex);
         /* Create a new one that takes over all the others. The list of locks
          * to be controlled by this manager has not been determined yet */
         manager = at_c_lock_manager_create(NULL,0);
         /* Lock the new manager */
         AT_SPINLOCK_LOCK(manager->mutex);
         /* Take over the others */
         take_over(manager,locks, number_of_locks);
         break;
      }
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: thread %d got manager.\n", manager, at_self()->id);
#endif
   return (manager);
} /* end: get_manager */

/* Lock Manager: Take over other managers. The function returns when all
 * the passed manager controls all locks of the passed list of locks. */
static void take_over( at_c_lock_manager_t* self,
                       at_generic_c_lock_t** locks, int number_of_locks) {
   at_c_lock_manager_t* manager_cursor;
   at_generic_c_lock_t* lock_cursor;
   /* Append the next managed lock here: */
   at_generic_c_lock_t* end_of_lock_list;
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: take over called by thread %d.\n", self, at_self()->id);
#endif
   /* First try it straight in the order of the locks without blocking */
/*      if (!try_straight_take_over(self,locks,number_of_locks)) */
      /* Start the blocking take over with reordering the locks first */
      ordered_take_over(self,locks,number_of_locks);
   /* Now all managers are taken over */
   /* Set all lock's new manager. Since a manager might have controlled
    * some locks that are not in the list, we have to scan each manager's
    * entire list of locks */
   /* The number of managed locks can now be determined */
   self->number_of_managed_locks = 0;
   /* Loop over all managers taken over */
   for (manager_cursor=self->replaced_managers; manager_cursor!=NULL;
                                manager_cursor=manager_cursor->next_manager) {
#ifdef AT_CL_TRACE_TAKE_OVER
      printf("Manager %p: took over manager %p.\n", self, manager_cursor);
#endif
      /* Loop over all locks managed by this manager */
      for (lock_cursor=manager_cursor->list_of_locks; lock_cursor!=NULL;
                                      lock_cursor=lock_cursor->next_managed) {
         /* Set the lock's pointer to its manager */
         lock_cursor->manager = self;
         /* Append lock to the list of managed locks */
         /* Is the list empty? */
         if (self->list_of_locks == NULL)
            /* Initialize the list */
            self->list_of_locks = end_of_lock_list = lock_cursor;
         else
            /* Append to the end of the list */
            end_of_lock_list = end_of_lock_list->next_managed = lock_cursor;
         /* Increment counter of managed locks */
         self->number_of_managed_locks++;
#ifdef AT_CL_TRACE_TAKE_OVER
         printf("Manager %p: took over lock %p.\n", self, lock_cursor);
#endif
      }
#ifdef AT_CL_THREAD_COUNT
      /* Also the counters are taken over */
      self->waiting_threads_counter += manager_cursor->waiting_threads_counter;
      self->holding_threads_counter += manager_cursor->holding_threads_counter;
#endif
      /* Mark manager to be taken over by setting its list of locks to NULL */
      manager_cursor->list_of_locks = NULL;
      /* Unlock the replaced managers */
      AT_SPINLOCK_UNLOCK(manager_cursor->mutex);
   }
#ifdef AT_CL_TRACE_MANAGER
   printf("Manager %p: take over succeeded by thread %d.\n",self,at_self()->id);
#endif
} /* End: take_over */

/* Lock Manager: Try locking all locks' managers in the order of the locks
 * without blocking. Return 1 in case of success, return 0 otherwise */
static int try_straight_take_over(at_c_lock_manager_t* self,
                        at_generic_c_lock_t** locks, int number_of_locks) {
   int i; /* loop counter */
   at_c_lock_manager_t* end_of_manager_list;
   /* For all locks */
   for ( i=0; i<number_of_locks; i++) {
      at_c_lock_manager_t* manager_cursor;
      at_c_lock_manager_t* manager = locks[i]->manager;
      int already_taken_over = 0; /* boolean */
#ifdef AT_CL_TRACE_TAKE_OVER
      printf("Manager %p: scanning lock %p.\n", self, locks[i]);
#endif
      /* Has the lock's manager already been taken over by this thread? */
      for (manager_cursor=self->replaced_managers; manager_cursor!=NULL;
                                   manager_cursor=manager_cursor->next_manager)
        if (manager_cursor == manager) {
           /* Yes, it has. Set 'already_taken_over' to continue outer loop */
           already_taken_over = 1;
#ifdef AT_CL_TRACE_TAKE_OVER
           printf("Manager %p: lock %p already taken over.\n",self,locks[i]);
#endif
           break;
        }
      /* Already taken over? */
      if (already_taken_over) continue;
      /* Try the locks manager. If its list of locks is NULL, it already
       * has been taken over by another manager */
      while (1) {
         if (!AT_SPINLOCK_TRY(manager->mutex)
             || manager->list_of_locks==NULL) {
            /* Failed! Unlock all managers locked yet and return */
            for (manager_cursor=self->replaced_managers; manager_cursor!=NULL;
                                    manager_cursor=manager_cursor->next_manager)
               AT_SPINLOCK_UNLOCK(manager_cursor->mutex);
            /* Reset the list of managed managers */
            self->replaced_managers = NULL;
#ifdef AT_CL_TRACE_TAKE_OVER
            printf("Manager %p: try locking lock's %p manager %p failed.\n",
                   self, locks[i], manager);
#endif
            return (0);
         }
         /* Got the right manager? */
         if (manager == locks[i]->manager) break;
         AT_SPINLOCK_UNLOCK(manager->mutex);
      }
      /* Got the manager */
#ifdef AT_CL_TRACE_TAKE_OVER
      printf("Manager %p: got lock's %p manager %p.\n",
             self, locks[i], manager);
#endif
      /* Add it to the list of replaced managers */
      if (self->replaced_managers == NULL)
         self->replaced_managers = end_of_manager_list = manager;
      else
         end_of_manager_list = end_of_manager_list->next_manager = manager;
   }
   /* Got all managers */
   return (1);
} /* end: try_straight_take_over */

/* Lock Manager: Lock all locks' managers. Order them first.
 * Check continuously, if any of the locks' managers changes */
static void ordered_take_over(at_c_lock_manager_t* self,
                         at_generic_c_lock_t** locks, int number_of_locks) {
   at_c_lock_manager_t* managers_on_stack[AT_CL_MANAGERS_ARRAY_SIZE_ON_STACK];
   at_c_lock_manager_t** managers;
   /* The maximum number of managers is the number of locks */
   /* Use the stack array, if it fits. Otherwise allocate sufficient memory */
   if (number_of_locks > AT_CL_MANAGERS_ARRAY_SIZE_ON_STACK)
      managers = (at_c_lock_manager_t**) malloc(
                           number_of_locks * sizeof(at_c_lock_manager_t*));
   else
      managers = managers_on_stack;
   /* Loop over determining managers, sorting them, locking them,
    * and then checking, if they are the right ones until success.
    * This is necessary, since the list of required managers
    * might change during the execution of this loop. In this case,
    * all locked managers are released and the next loop tries again */
   while (1) {
      int i,j; /* loop counters */
      int number_of_managers = 0;
      int got_them_all = 1; /* boolean, reset if failure occurs */
      /* Determine the required managers */
      for (i=0; i<number_of_locks; i++) {
         at_c_lock_manager_t* manager = locks[i]->manager;
         /* Is this manager already in the array? */
         int already_in = 0; /* boolean */
         for(j=0; j<number_of_managers; j++)
            if (manager==managers[j]) { already_in = 1; break; }
         if (already_in) continue;
         /* It's a new manager. Append it */
         managers[number_of_managers++] = manager;
      }
      /* Bubble sort the array of managers */
      for (i=number_of_managers; i>0; i--) {
         int max_index = 0;
         at_c_lock_manager_t* max_manager = managers[0];
         for (j=1; j<i; j++)
            if (managers[j] > max_manager) {
               max_manager = managers[j];
               max_index = j;
            }
         /* Swap the max and the last */
         managers[max_index] = managers[i-1];
         managers[i-1] = max_manager;
      }
      /* Lock the ordered managers */
      for (i=0; i<number_of_managers; i++)
         AT_SPINLOCK_LOCK(managers[i]->mutex);
      /* Check, if they still are the right managers */
      for (i=0; i<number_of_locks; i++) {
         int found_it = 0; /* boolean */
         for (j=0; j<number_of_managers; j++)
            if (managers[j] == locks[i]->manager) { found_it = 1; break; }
         if (!found_it) { got_them_all = 0; break; }
      }
      /* Complete success? */
      if (got_them_all) {
         /* Yes, store the managers taken over and return */
         /* Create the list of replaced managers */
         /* Head of the list */
         self->replaced_managers = managers[0];
         for (i=1; i<number_of_managers; i++)
            /* Append this one to the list */
            managers[i-1]->next_manager = managers[i];
         managers[i-1] = NULL;
         /* Done. Free memory, if necessary,  and return */
         if (number_of_locks > AT_CL_MANAGERS_ARRAY_SIZE_ON_STACK)
            free(managers);
         return;
      } else {
         /* No, Unlock all managers and try again */
         for (i=0; i<number_of_managers; i++)
            AT_SPINLOCK_UNLOCK(managers[i]->mutex);
      }
   }
} /* end: ordered_take_over */

/* Lock Manager: Split a common manager. Management of the locks
 * is passed to the managers which were replaced by the one being
 * split now. Managers having reached their minimal number of managed
 * threads are not split. 
 * The return value is 0, if the manager has been split and 1 otherwise */
int at_c_lock_manager_split(at_c_lock_manager_t* shared_manager,
                            at_generic_c_lock_t** locks, int number_of_locks) {
   int i; /* loop counter */
   /* Lock the shared manager */
   AT_SPINLOCK_LOCK(shared_manager->mutex);
   /* Check, if this manager can be split. Otherwise the split fails */
   if (shared_manager->number_of_managed_locks ==
                                 shared_manager->number_of_linked_locks) {
      AT_SPINLOCK_UNLOCK(shared_manager->mutex);
      return (1);
   }
   /* Check if all locks share this manager */
   for (i=1; i<number_of_locks; i++)
      /* If one is different, the split fails */
      if (shared_manager != locks[i]->manager) {
         AT_SPINLOCK_UNLOCK(shared_manager->mutex);
         return (1);
      }

   /* Now start split the shared manager */
   split_manager(shared_manager, locks, number_of_locks);
   /* Destroy the old manager. Return to pool or free */
   AT_SPINLOCK_UNLOCK(shared_manager->mutex);
   if (at_c_locks_manager_pool != NULL)
      at_pool_put(at_c_locks_manager_pool,shared_manager);
   else
      free(shared_manager);
   return (0);
}

/* Lock Manager: Split a common manager. All managers that manage all of the
 * locks in the list 'locks' are terminated. The management of the locks
 * is passed to the managers which were replaced by the ones being
 * split now. Managers having reached their minimal number of managed
 * threads are not split.
 * This function is NOT SAFE (yet). If locks are split, that are part of
 * a blocked conjunctive request, the program may fail or deadlock */
void at_c_lock_manager_split_all(at_generic_c_lock_t** locks,
                                 int number_of_locks) {
   int i; /* loop counter */
   at_c_lock_manager_t* shared_manager;
   /* Loop endlessly while there is a manager shared by all
    * and while the shared manager may  be split */
   while (1) {
      /* Lock the first lock's manager */
      shared_manager = locks[0]->manager;
      AT_SPINLOCK_LOCK(shared_manager->mutex);
      /* Check, if this manager can be split. 
       * Otherwise there is nothing left to do */
      if (shared_manager->number_of_managed_locks ==
                                 shared_manager->number_of_linked_locks) {
         AT_SPINLOCK_UNLOCK(shared_manager->mutex);
         return;
      }
      /* Check if all locks share this manager */
      for (i=1; i<number_of_locks; i++)
	 /* If one is different, the work is done */
         if (shared_manager != locks[i]->manager) {
            AT_SPINLOCK_UNLOCK(shared_manager->mutex);
            return;
	 }

      /* Now start split the shared manager */
      split_manager(shared_manager, locks, number_of_locks);
      /* Destroy the old manager. Return to pool or free */
      AT_SPINLOCK_UNLOCK(shared_manager->mutex);
      if (at_c_locks_manager_pool != NULL)
         at_pool_put(at_c_locks_manager_pool,shared_manager);
      else
         free(shared_manager);
   }
}

static void split_manager(at_c_lock_manager_t* shared_manager,
                          at_generic_c_lock_t** locks, int number_of_locks) {
   int i; /* loop counter */
   at_generic_c_lock_t* lock_cursor;
   at_c_lock_manager_t* manager_cursor;

   /* Loop over all replaced manager */
   manager_cursor = shared_manager->replaced_managers;
   for (manager_cursor = shared_manager->replaced_managers;
        manager_cursor != NULL;
        manager_cursor = shared_manager->replaced_managers) {
      /* Lock the manager. 
       * The list of replaced manager is assumed to be sorted */
      AT_SPINLOCK_LOCK(manager_cursor->mutex);
      /* Set the head of the list of managed locks */
      manager_cursor->list_of_locks = shared_manager->list_of_locks;
      /* Loop over all locks managed by this manager */
      lock_cursor = manager_cursor->list_of_locks;
      for ( i=1; i <= manager_cursor->number_of_managed_locks; i++) {
         lock_cursor = shared_manager->list_of_locks;
	 /* Set the lock's new manager */
         lock_cursor->manager = manager_cursor;
         /* Move to the next lock */
         shared_manager->list_of_locks = lock_cursor->next_managed;
      }
      /* Terminate the list of locks */
      lock_cursor->next_managed = NULL;
      /* Remove the manager from the list of replaced managers */
      shared_manager->replaced_managers = manager_cursor->next_manager;
      manager_cursor->next_manager = NULL;
#ifdef AT_CL_THREAD_COUNT
      /* After a split the counters cannot be restored.
       * Set them to negative values indicating they are invalid */
      manager_cursor->holding_threads_counter = -1000;
      manager_cursor->waiting_threads_counter = -1000;
#endif
      /* Unlock the reactivated manager */
      AT_SPINLOCK_UNLOCK(manager_cursor->mutex);
   }
}


static void print_queues(at_c_lock_manager_t* self) {
   at_generic_c_lock_t* lock;
   at_c_lock_queue_entry_t* entry;
   printf("Manager %p:", self);
#ifdef AT_CL_THREAD_COUNT
   printf(" %d waiting threads, %d holding threads.",
          self->waiting_threads_counter, self->holding_threads_counter);
#endif
   printf("\n");
   printf("------------------------------------------------------------\n");
   for (lock=self->list_of_locks; lock!=NULL; lock=lock->next_managed) {
      printf(" Lock %p:", lock);
      for (entry=lock->queue; entry!=NULL; entry=entry->next_entry)
         printf(" %d", entry->tid);
      printf("\n");
      printf("------------------------------------------------------------\n");
   }
} /* end: print_queues */
