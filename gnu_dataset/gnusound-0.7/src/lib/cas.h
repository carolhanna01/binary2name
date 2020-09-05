/*
 * Atomic compare-and-swap operators.
 * Copyright (C) 2002-2004  Pascal Haakmat
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef CAS_H
#define CAS_H

/* Type with same size as memory location. */

typedef int cas_word_t;

/* Function types for convenience. */

typedef int (*cas_function_t)(void *addr, 
                              cas_word_t old, 
                              cas_word_t new);
typedef int (*cas2_function_t)(void *long_addr, 
                               cas_word_t old1, 
                               cas_word_t new1,
                               cas_word_t old2,
                               cas_word_t new2);

/*
 * Atomically compare and swap a value.
 *
 * Compares the value at address addr to old. If equal, new is stored
 * at the location of addr and 1 is returned. Otherwise 0 is returned.
 * The operation is guaranteed to be atomic.
 *
 * @param addr The memory location to compare.
 * @param old The expected value at the memory location.
 * @param new The new value to store into the memory location.
 *
 * int          
 * cas(void *addr,            
 *     cas_word_t old, 
 *     cas_word_t new);
 *
 */

/*
 * Atomically compare and swap two values.
 *
 * Compares the two values stored at address long_addr to old1 and
 * old2. If equal, new1 and new2 are stored at the location of
 * long_addr and 1 is returned. Otherwise 0 is returned.
 *
 * @param long_addr The memory location, storing 2 values, to use for
 * the comparison.
 * @param old1 The expected value at the first memory location.
 * @param new1 The new value to store into the first memory location.
 * @param old2 The expected value at the second memory location.
 * @param new2 The new value to store into the second memory location.
 *
 * int            
 * cas2(void *long_addr, 
 *      cas_word_t old1, 
 *      cas_word_t new1, 
 *      cas_word_t old2, 
 *      cas_word_t new2);
 */

/* Implementation prototypes. */

int
cas_c_pthread(void *addr, cas_word_t old, cas_word_t new);
int
cas2_c_pthread(void *long_addr, cas_word_t old1, cas_word_t new1, cas_word_t old2, cas_word_t new2);

int
cas_x86_32(void *addr, cas_word_t old, cas_word_t new);
int
cas2_x86_32(void *long_addr, cas_word_t old1, cas_word_t new1, cas_word_t old2, cas_word_t new2);
int
cas_x86_32_smp(void *addr, cas_word_t old, cas_word_t new);
int
cas2_x86_32_smp(void *long_addr, cas_word_t old1, cas_word_t new1, cas_word_t old2, cas_word_t new2);
int
cas_x86_64(void *addr, cas_word_t old, cas_word_t new);
int
cas2_x86_64(void *long_addr, cas_word_t old1, cas_word_t new1, cas_word_t old2, cas_word_t new2);
int
cas_x86_64_smp(void *addr, cas_word_t old, cas_word_t new);
int
cas2_x86_64_smp(void *long_addr, cas_word_t old1, cas_word_t new1, cas_word_t old2, cas_word_t new2);

#endif /* CAS_H */
