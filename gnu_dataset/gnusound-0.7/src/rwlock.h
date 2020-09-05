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

#ifndef RWLOCK_H
#define RWLOCK_H

#include <pthread.h>

/* GUI thread, playback thread, and JACK thread. */

#define MAX_THREADS 3

struct readers {
    int is_valid;
    pthread_t tid;
    int rcount;
};

typedef struct {
    pthread_mutex_t lock;
    pthread_cond_t cond;
    pthread_t writer_tid;
    int wcount;
    struct readers readers[MAX_THREADS];
} rwlock;

void
rwlock_wlock(rwlock *rwl);

void
rwlock_rlock(rwlock *rwl);

void
rwlock_wunlock(rwlock *rwl);

void
rwlock_runlock(rwlock *rwl);

void
rwlock_init(rwlock *rwl);

void
rwlock_destroy(rwlock *rwl);

#endif /* ! RWLOCK_H */
