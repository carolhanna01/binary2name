/*
 * EDMA: Entorno de Desarrollo Modular y Abierto
 * Object Oriented and Componetware Framework
 * Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
 *    David Martínez Oliveira
 *
 * This file is part of EDMA.
 *
 * EDMA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * EDMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with EDMA.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * June, 12th, 2004
 * Added implementation for thread_registry function
 */
#include "portable.h"
#include "vglobal.h"

#include "shmem.h"
#include "misc.h"
#include <pthread.h>

/* FIXME: Contitional include bellow for different OSes*/
#include "linuxth.h"

/* Thread Specific Functions */
/*****************************/

/* edma_thread_create
 *  Linux specific thread creation function
 */

ESint32 EDMAPROC
edma_thread_create (ETHREAD *et, EPROC func, ETHREAD_PARAMS par)
{
  /* FIXME: For GNU/EDMA thread registration follows*/
  return pthread_create (et, NULL, func, par);
}

/* edma_thread_self
 *   Linux specific thread self function
 */

ETHREAD EDMAPROC
edma_thread_self ()
{
  return pthread_self();
}

/* Mutex Specific Functions */
/****************************/

/* edma_mutex_create
 *   Linux specific MUTEX create function
 */

ESint32 EDMAPROC
edma_mutex_create (EMUTEX emux)
{
  return pthread_mutex_init (emux, NULL);
}

/* edma_mutex_destroy
 *  Linux specific MUTEX destroy function
 */

ESint32 EDMAPROC
edma_mutex_destroy (EMUTEX emux)
{
  return pthread_mutex_destroy (emux);
}

/* edma_mutex_lock
 *   Linux specific MUTEX lock function
 */

ESint32 EDMAPROC
edma_mutex_lock (EMUTEX emux)
{
  return pthread_mutex_lock (emux);
}


/* edma_mutex_unlock
 *   Linux specific MUTEX unlock function
 */

ESint32 EDMAPROC
edma_mutex_unlock (EMUTEX emux)
{
  return pthread_mutex_unlock (emux);
}

/* Conditional Variable Functions */

/* edma_cond_create
 *   Linux specific Condition Variable creation
 */

ESint32 EDMAPROC
edma_cond_create (ECOND econd)
{
  return pthread_cond_init (econd, NULL);
}

/* edma_cond_destroy
 *  Linux specific Condition Variable destroy
 */

ESint32 EDMAPROC
edma_cond_destroy (ECOND econd)
{
  return pthread_cond_destroy (econd);
}

/* edma_cond_signal
 *   Linux specific Condition Variable signal
 */

ESint32 EDMAPROC
edma_cond_signal (ECOND econd)
{
  return pthread_cond_signal (econd);
}

/* edma_cond_broadcast
 *  Linux specific Condition Variable broadcast
 */

ESint32 EDMAPROC
edma_cond_broadcast (ECOND econd)
{
  return pthread_cond_broadcast (econd);
}

/* edma_cond_wait
 *    Linux Specific Condition Variable wait
 */

ESint32 EDMAPROC
edma_cond_wait (ECOND econd, EMUTEX emux)
{
  return pthread_cond_wait (econd, emux);
}

/* Thread Local Storage Functions*/

/* edma_thread_key_create
 *  Linux specific thread key creation for TLS function
 */

ESint32 EDMAPROC
edma_thread_key_create (ETKEY *key)
{
  return pthread_key_create (key, NULL);
}

/* edma_thread_key_destroy
 *   Linux Specific thread key destruction for TLS function
 */

ESint32 EDMAPROC
edma_thread_key_destroy (ETKEY key)
{
  return pthread_key_delete (key);
}


/* edma_tsd_set_data
 *   Linux Specific TLS set data function 
 */

ESint32 EDMAPROC
edma_tsd_set_data (ETKEY key, EPVoid dat)
{
  return pthread_setspecific (key, dat);
}


/* edma_tsd_get_data
 *    Linux Specific TLS get data function
 */
EPVoid EDMAPROC
edma_tsd_get_data (ETKEY key)
{
  return pthread_getspecific (key);
}

