/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1995/96 by International Computer Science Institute         */
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

/*
 * The Lock manager (including dead lock detection)
 *
 * Version 1.0 (released for 1.0.9) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 * Version 1.1 (released for 1.1  ) by Claudio Fleiner (fleiner@icsi.berkeley.edu)
 */
#include "pSather.h"

#ifdef LOCK_TRACE
static char *print_locks(void *vex,int res)
{
        EXCEPT_LOCK_ELEMENT(1) *ex=vex;
        int i,j;
        char *p;
        CALLOC(p,ex->cols*ex->branches*10+20,1);
        p[0]=0;
        if(ex->flags&LOCK_HIDDEN) strcat(p,"R:");
        if(ex->flags&LOCK_WITH_ELSE) strcat(p,"ELSE:");
        strcat(p,"[");
        for(j=0;j<ex->branches;j++) {
                if(j!=0) strcat(p,"| ");
                if(j==res) strcat(p,"=> ");
                for(i=0;i<ex->cols;i++)
                        sprintf(p+strlen(p),"%p ",ex->locks[i+j*ex->cols]);
                if(j==res) strcat(p,"<= ");
        }
        p[strlen(p)-1]=']';
        return p;
}
#endif

/*
 * call stubs for member function of locks
 */

extern void LM_EXTERNAL_INTERFACE_p_init_locks( );
extern void LM_EXTERNAL_INTERFACE_p_init_lock_header(LOCK l);
/* Initialize different lock structures.
   This function should be executed on ALL clusters.  */
void p_init_locks() { LM_EXTERNAL_INTERFACE_p_init_locks( ); }
void p_init_lock_header(LOCK l) { LM_EXTERNAL_INTERFACE_p_init_lock_header(l); }

void my_lck_unlock(BR_lock_t l)
{ BR_UNLOCK(l); }

static void store_value(vnn_t from,BR_word_t a,BR_word_t b)
{
        *(void **)a=(void *)b;
}

static void rll_lock_lck_remote(vnn_t from,LL_LOCK l,BR_word_t r)
{
        BR_LOCK(l->lck);
        BR_REQUEST_2(from,store_value,r,-1);
}
void ll_lock_lck_remote(LL_LOCK l)
{
        int w;
        if((w=WHERE(l))==HERE) { BR_LOCK(l->lck); return; }
        BR_FORK_2(w,(BR_handler_2_t)rll_lock_lck_remote,(long)SENDFOBHOME(l),(long)&w);
        am_wait_for(w<0);
}
static void rll_lock_unlck_remote(vnn_t from,LL_LOCK l,long r)
{
        BR_UNLOCK(l->lck);
        BR_REQUEST_2(from,store_value,r,-1);
}
void ll_lock_unlck_remote(LL_LOCK l)
{
        int w;
        if((w=WHERE(l))==HERE) { BR_UNLOCK(l->lck); return; }
        BR_FORK_2(w,(BR_handler_2_t)rll_lock_unlck_remote,(long)SENDFOBHOME(l),(long)&w);
        am_wait_for(w<0);
}
static void rll_lock_try_remote(vnn_t from,LL_LOCK l,long r)
{
        int rr;
        rr=BR_TRY_LOCK(l->lck);
        BR_REQUEST_2(from,store_value,r,rr);
}
int ll_lock_try_remote(LL_LOCK l)
{
        int w,r=-99;
        if((w=WHERE(l))==HERE) return BR_TRY_LOCK(l->lck);
        BR_FORK_2(w,(BR_handler_2_t)rll_lock_try_remote,(long)SENDFOBHOME(l),(long)&r);
        am_wait_for(r!=-99);
        return r;
}


#ifdef SPINLOCK_LOCK
static void rspinlock_lck_remote(vnn_t from,SPINLOCK l,long r)
{
        SPINLOCK_LCK(l);
        BR_REQUEST_2(from,store_value,r,-1);
}
void spinlock_lck_remote(SPINLOCK l)
{
        int w;
        if((w=WHERE(l))==HERE) { SPINLOCK_LCK(l); return; }
        BR_FORK_2(w,(BR_handler_2_t)rspinlock_lck_remote,(long)SENDFOBHOME(l),(long)&w);
        am_wait_for(w<0);
}
static void rspinlock_unlck_remote(vnn_t from,SPINLOCK l,long r)
{
        SPINLOCK_UNLCK(l);
        BR_REQUEST_2(from,store_value,r,-1);
}
void spinlock_unlck_remote(SPINLOCK l)
{
        int w;
        if((w=WHERE(l))==HERE) { SPINLOCK_UNLCK(l); return; }
        BR_FORK_2(w,(BR_handler_2_t)rspinlock_unlck_remote,(long)SENDFOBHOME(l),(long)&w);
        am_wait_for(w<0);
}
static void rspinlock_try_remote(vnn_t from,SPINLOCK l,long r)
{
        int rr;
        rr=SPINLOCK_TRY(l);
        BR_REQUEST_2(from,store_value,r,rr);
}
int spinlock_try_remote(SPINLOCK l)
{
        int w,r=-99;
        if((w=WHERE(l))==HERE) return SPINLOCK_TRY(l);
        BR_FORK_2(w,(BR_handler_2_t)rspinlock_try_remote,(long)SENDFOBHOME(l),(long)&r);
        am_wait_for(r!=-99);
        return r;
}
#endif


/* Functions for copying a C array between clusters */

void lm_get_remote_ptr_reply(BR_cluster_t source, BR_word_t to,
                                BR_word_t lock, BR_word_t spinlock)
{
   printf("Writing %p(%p) at cluster %d to %p.\n",(void *)lock,POINTER((FOB)lock),HERE,(void *)to);
   *((BR_word_t*)to) = lock;
   BR_SPINLOCK_UNLOCK(*((BR_spinlock_t*)spinlock));
}

void lm_get_remote_ptr_request(BR_cluster_t target, BR_word_t from,
                                 BR_word_t to, BR_word_t spinlock)
{
printf("Reading %p(%p) at cluster %d at %p.\n",*((FOB*)from),POINTER(*((FOB*)from)),HERE,(void *)from);
   BR_REPLY_3( target, (BR_handler_3_t)lm_get_remote_ptr_reply, to,
               (BR_word_t)(SENDFOB( *((FOB*)from),target)), spinlock);
}
