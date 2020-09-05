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

#include <config.h>
#include <stdio.h>
#include <pthread.h>
#include <assert.h>
#include "cas.h"

pthread_mutex_t cas_lock = PTHREAD_MUTEX_INITIALIZER;

int
cas2_c_pthread(void *long_addr,
               cas_word_t old1,
               cas_word_t new1,
               cas_word_t old2,
               cas_word_t new2) {
    cas_word_t *a1 = long_addr, *a2 = (cas_word_t *)long_addr + 1;
    int r = 0;
    assert(!pthread_mutex_lock(&cas_lock));
    if(*a1 == old1 && *a2 == old2) {
        *a1 = new1;
        *a2 = new2;
        r = 1;
    }
    assert(!pthread_mutex_unlock(&cas_lock));
    return r;
}

int
cas_c_pthread(void *addr, 
              cas_word_t old,
              cas_word_t new) {
    cas_word_t *a = addr;
    int r = 0;
    assert(!pthread_mutex_lock(&cas_lock));
    if(*a == old) {
        *a = new;
        r = 1;
    }
    assert(!pthread_mutex_unlock(&cas_lock));
    return r;
}

#define HAVE_X86
#ifdef HAVE_X86
#define CAS_X86(funcname, insn, lock)           \
  int                                           \
  funcname(void *addr,                          \
           cas_word_t old,                      \
           cas_word_t new) {                    \
      char r;                                   \
      __asm__ __volatile__ (lock insn " %3,(%1)\n" \
                            "setz %0\n"         \
                            : "=r" (r)          \
                            : "r" (addr), "a" (old), "r" (new) \
                            : "memory");        \
      return r;                                 \
  }

#define CAS2_X86(funcname, insn, lock)          \
  int                                           \
  funcname(void *long_addr,                     \
           cas_word_t old1,                     \
           cas_word_t new1,                     \
           cas_word_t old2,                     \
           cas_word_t new2) {                   \
      char r;                                   \
      __asm__ __volatile__ (lock insn " (%1)\n" \
                            "setz %0\n"         \
                            : "=r" (r)          \
                            : "r" (long_addr), "a" (old1), "d" (old2), "b" (new1), "c" (new2) \
                            : "memory");        \
      return r;                                 \
  }
CAS_X86(cas_x86_32, "cmpxchg", "")
CAS2_X86(cas2_x86_32, "cmpxchg8b", "")
CAS_X86(cas_x86_32_smp, "cmpxchg", "lock ")
CAS2_X86(cas2_x86_32_smp, "cmpxchg8b", "lock ")
# ifdef __x86_64__ /* untested! */
CAS_X86(cas_x86_64, "cmpxchgq", "")
CAS2_X86(cas2_x86_64, "cmpxchg8b", "")
CAS_X86(cas_x86_64_smp, "cmpxchgq", "lock ")
CAS2_X86(cas2_x86_64_smp, "cmpxchg8b", "lock ")
# endif /* __x86_64__ */
#endif /* HAVE_X86 */

