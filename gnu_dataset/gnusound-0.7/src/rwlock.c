/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

/*
 * An rwlock is a recursive, shared read/exclusive write lock
 * between MAX_THREADS number of threads.
 */

#include <stdlib.h>
#include <pthread.h>
#include <errno.h>
#include <config.h>
#include "rwlock.h"

static int
rwlock_readers_on_other_threads(rwlock *rwl) {
    int r = 0, i;
    for(i = 0; i < MAX_THREADS; i++) {
        if(!rwl->readers[i].is_valid)
            continue;
        if(pthread_equal(rwl->readers[i].tid, pthread_self()))
            continue;
        r += rwl->readers[i].rcount;
    }
    return r;
}

/*
 * Acquires the write lock. This function blocks if another thread 
 * already holds the write or read lock. If the write lock is held
 * by the calling thread then a counter is incremented and the function
 * returns.
 */
void
rwlock_wlock(rwlock *rwl) {
    pthread_mutex_lock(&rwl->lock);

    /* Wait for writers/readers on other threads. */

    while((rwl->wcount && !pthread_equal(rwl->writer_tid, pthread_self())) ||
          rwlock_readers_on_other_threads(rwl))
        pthread_cond_wait(&rwl->cond, &rwl->lock);

    rwl->wcount++;
    rwl->writer_tid = pthread_self();
    pthread_mutex_unlock(&rwl->lock);
}

/**
 * Releases the write lock. This wakes up the threads waiting in
 * rwlock_rlock() or rwlock_wlock().
 */
void
rwlock_wunlock(rwlock *rwl) {
    pthread_mutex_lock(&rwl->lock);
    rwl->wcount--;
    pthread_cond_broadcast(&rwl->cond);
    pthread_mutex_unlock(&rwl->lock);
}

static void
rwlock_reader_add(rwlock *rwl) {
    int i;

    /* Find whether this thread already holds read locks. */

    for(i = 0; i < MAX_THREADS; i++) {
        if(!rwl->readers[i].is_valid) 
            continue;

        if(pthread_equal(rwl->readers[i].tid, pthread_self())) {
            rwl->readers[i].rcount++;
            return;
        }
    }

    /* New thread. */

    for(i = 0; i < MAX_THREADS; i++) {
        if(rwl->readers[i].is_valid) 
            continue;

        rwl->readers[i].tid = pthread_self();
        rwl->readers[i].rcount = 1;
        rwl->readers[i].is_valid = 1;
        return;
    }
    
    FAIL("thread not found and pool is full, cannot acquire read lock %p for thread %lx\n",
         rwl, pthread_self());
}

/**
 * Acquires the read lock. This function blocks if another thread
 * holds the write lock, otherwise returns immediately.
 */

void
rwlock_rlock(rwlock *rwl) {
    pthread_mutex_lock(&rwl->lock);
    while(rwl->wcount && !pthread_equal(rwl->writer_tid, pthread_self()))
        pthread_cond_wait(&rwl->cond, &rwl->lock);

    rwlock_reader_add(rwl);
    pthread_mutex_unlock(&rwl->lock);
}

static void
rwlock_reader_remove(rwlock *rwl) {
    int i;

    for(i = 0; i < MAX_THREADS; i++) {
        if(!rwl->readers[i].is_valid)
            continue;

        if(!pthread_equal(rwl->readers[i].tid, pthread_self()))
            continue;

        /* Found our thread. */

        rwl->readers[i].rcount--;

        /* Thread has released last read lock. */

        if(!rwl->readers[i].rcount)
            rwl->readers[i].is_valid = 0;
        pthread_cond_broadcast(&rwl->cond);
        return;
    }
    
    FAIL("thread not found, cannot release read lock %p for thread %lx\n",
         rwl, pthread_self());
}

/**
 * Releases the read lock. This wakes up any threads waiting in
 * rwlock_wlock().
 */
void
rwlock_runlock(rwlock *rwl) {
    pthread_mutex_lock(&rwl->lock);
    rwlock_reader_remove(rwl);
    pthread_mutex_unlock(&rwl->lock);
}

void
rwlock_init(rwlock *rwl) {
    int i;
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutex_init(&rwl->lock, NULL);
    pthread_cond_init(&rwl->cond, NULL);
    rwl->wcount = 0;
    for(i = 0; i < MAX_THREADS; i++)
        rwl->readers[i].is_valid = 0;
}

void
rwlock_destroy(rwlock *rwl) {
    if(pthread_mutex_destroy(&rwl->lock) == EBUSY) 
        FAIL("unable to destroy mutex, busy?\n");
    pthread_cond_destroy(&rwl->cond);
}
