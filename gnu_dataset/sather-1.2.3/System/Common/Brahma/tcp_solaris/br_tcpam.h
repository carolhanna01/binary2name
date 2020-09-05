/*------------------------->  ANSI C - headerfile  <-------------------------*/
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
 * The GAM interface, including the thread module
 * This has been slighly modified to make it work with Brahma and
 * to make it a bit more type safe.
 */
#ifndef _BR_TCPAM_H_
#define _BR_TCPAM_H_

#include <assert.h>
#include <stdio.h>

typedef unsigned long qqvnn_t;
typedef void (*handler_0_t)(qqvnn_t);
typedef void (*handler_1_t)(qqvnn_t,long);
typedef void (*handler_2_t)(qqvnn_t,long,long);
typedef void (*handler_3_t)(qqvnn_t,long,long,long);
typedef void (*handler_4_t)(qqvnn_t,long,long,long,long);
typedef void (*handler_df_t)(qqvnn_t,long,long,double);
typedef void (*handler_mem_t)(qqvnn_t,void *,size_t,void *);
typedef void (*signal_handler_t)(void);
typedef void (*handler_t)(qqvnn_t,...);

void qq_enable(int nodes,int my_nide_num, int argc,char *argv[]);
void qq_disable(void);
void qq_poll(int);
int  qq_procs(void);
int  qq_my_cluster_size(void);
int  qq_set_my_cluster_size(void);
int  qq_max_size(void);
qqvnn_t qq_my_proc(void);

extern int qq_clusters;
extern int qq_my_cluster_id;
extern int qq_cluster_size;
#define QQ_PROCS	qq_clusters
#define QQ_MY_PROC	qq_my_cluster_id
#define QQ_MY_CLUSTER_SIZE qq_cluster_size

int qq_request_0(qqvnn_t dest,handler_0_t handler);
int qq_request_1(qqvnn_t dest,handler_1_t handler,long arg0);
int qq_request_2(qqvnn_t dest,handler_2_t handler,long arg0,long arg1);
int qq_request_3(qqvnn_t dest,handler_3_t handler,long arg0,long arg1,long arg2);
int qq_request_4(qqvnn_t dest,handler_4_t handler,long arg0,long arg1,long arg2,long arg3);
int qq_request_df(qqvnn_t dest,handler_df_t handler,long arg0,long arg1,double arg2);

int qq_reply_0(qqvnn_t dest,handler_0_t handler);
int qq_reply_1(qqvnn_t dest,handler_1_t handler,long arg0);
int qq_reply_2(qqvnn_t dest,handler_2_t handler,long arg0,long arg1);
int qq_reply_3(qqvnn_t dest,handler_3_t handler,long arg0,long arg1,long arg2);
int qq_reply_4(qqvnn_t dest,handler_4_t handler,long arg0,long arg1,long arg2,long arg3);
int qq_reply_df(qqvnn_t dest,handler_df_t handler,long arg0,long arg1,double arg2);

int qq_store(qqvnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *h_arg);
int qq_get(qqvnn_t dest,void *rva,void *lva,int nbytes,handler_mem_t handler,void *h_arg);
int qq_store_async(qqvnn_t dest,void *lva,void *rva,int nbytes,handler_mem_t handler,void *h_arg,handler_mem_t endfunc,void *e_arg);

void qq_dummy(qqvnn_t,void *,int,void*);

/* #define sleep(a) qq_sleep(a)  DPS: WHY?? */
void qq_sleep(unsigned long);

#define qq_wait_for(cond) do { if(cond) break;qq_thread_poll(); } while(1)
void qq_thread_poll(void);

int thr_create_0(qqvnn_t where,handler_0_t handler);
int thr_create_1(qqvnn_t where,handler_1_t handler,long arg0);
int thr_create_2(qqvnn_t where,handler_2_t handler,long arg0,long arg1);
int thr_create_3(qqvnn_t where,handler_3_t handler,long arg0,long arg1,long arg2);
int thr_create_4(qqvnn_t where,handler_4_t handler,long arg0,long arg1,long arg2,long arg3);
int thr_create_df(qqvnn_t where,handler_df_t handler,long arg0,long arg1,double arg2);

#endif
