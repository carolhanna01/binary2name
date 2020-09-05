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

#include <config.h>
#include <stdlib.h>

#define SAFETY_BUFFER_SIZE 512 * 1024
#define ABORT_ON_FAIL 1

static void *mem_safety_buffer = NULL;
int mem_fail_allocation_on_zero = 0;

int
mem_init() {
    mem_safety_buffer = malloc(SAFETY_BUFFER_SIZE);
    if(!mem_safety_buffer)
        return 1;
    return 0;
}

void
mem_exit() {
    if(mem_safety_buffer)
        free(mem_safety_buffer);
}

void *
mem_alloc(size_t sz) {
    void *ptr;

    if(sz < 0) 
        abort();

    if(mem_fail_allocation_on_zero > 0)
        if(!(--mem_fail_allocation_on_zero))
            return NULL;

    ptr = malloc(sz);

    if(!ptr) {
        FAIL("alloc of %lu bytes failed\n", (unsigned long)sz);
        if(mem_safety_buffer) {
            free(mem_safety_buffer);
            FAIL("freed safety buffer\n");
            mem_safety_buffer = NULL;
        } else {
            FAIL("safety buffer already freed!\n");
        }
#if ABORT_ON_FAIL == 1
        abort();
#endif
        return NULL;
    }
    return ptr;
}

void *
mem_realloc(void *old_ptr, 
            size_t sz) {
    void *ptr;
    
    if(mem_fail_allocation_on_zero > 0)
        if(!(--mem_fail_allocation_on_zero))
            return NULL;

    ptr = realloc(old_ptr, sz);

    if(!ptr) {
        FAIL("realloc of %"CONVSPEC_SIZE_T" bytes failed\n", sz);
        if(mem_safety_buffer) {
            free(mem_safety_buffer);
            FAIL("freed safety buffer\n");
            mem_safety_buffer = NULL;
        } else {
            FAIL("safety buffer already freed!\n");
        }
#if ABORT_ON_FAIL == 1
        abort();
#endif
        return NULL;
    }
    return ptr;
}


void *
mem_calloc(size_t nmemb,
           size_t sz) {
    void *ptr;
 
    if(mem_fail_allocation_on_zero > 0)
        if(!(--mem_fail_allocation_on_zero))
            return NULL;

    ptr = calloc(nmemb, sz);

    if(!ptr) {
        FAIL("calloc of %"CONVSPEC_SIZE_T" bytes failed\n", nmemb * sz);
        if(mem_safety_buffer) {
            free(mem_safety_buffer);
            FAIL("freed safety buffer\n");
            mem_safety_buffer = NULL;
        } else {
            FAIL("safety buffer already freed!\n");
        }
#if ABORT_ON_FAIL == 1
        abort();
#endif
        return NULL;
    }
    return ptr;
}


void
mem_free(void *ptr) {
    free(ptr);
}
