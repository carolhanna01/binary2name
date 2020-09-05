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
 * August, 19th, 2004
 * Thread related data types moved to portable. h
 */

#ifndef LINUXTH_H
#define LINUXTH_H

#include <portable.h>

/* Thread Functions*/
ESint32 EDMAPROC edma_thread_create (ETHREAD *et, EPROC func, ETHREAD_PARAMS par);
ETHREAD EDMAPROC edma_thread_self ();

/* Mutex Functions */
ESint32 EDMAPROC edma_mutex_create (EMUTEX emux);
ESint32 EDMAPROC edma_mutex_destroy (EMUTEX emux);
ESint32 EDMAPROC edma_mutex_lock (EMUTEX emux);
ESint32 EDMAPROC edma_mutex_unlock (EMUTEX emux);

/* Conditional Variables FUnctions*/
ESint32 EDMAPROC edma_cond_create (ECOND econd);
ESint32 EDMAPROC edma_cond_destroy (ECOND econd);
ESint32 EDMAPROC edma_cond_signal (ECOND econd);
ESint32 EDMAPROC edma_cond_broadcast (ECOND econd);
ESint32 EDMAPROC edma_cond_wait (ECOND econd, EMUTEX emux);

/* Thread Local Storage Functions */
ESint32 EDMAPROC edma_thread_key_create (ETKEY *etls);
ESint32 EDMAPROC edma_thread_key_destroy (ETKEY etls);
ESint32 EDMAPROC edma_tsd_set_data (ETKEY etls, EPVoid dat);
EPVoid EDMAPROC edma_tsd_get_data (ETKEY etls);
#endif
