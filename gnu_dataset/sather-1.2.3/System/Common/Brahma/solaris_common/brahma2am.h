/*------------------------->  ANSI C - headerfile  <-------------------------*/
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

/*
 * This expresses all brahma things in terms of AM types. 
 * qq_* calls need to be thread safe. This file could be used by all 
 * Brahma implementations based on thread safe GAM (well, provided
 * it was slighly hacked to allow remote thread creation)
 */

#ifndef _BRAHMA2AM_H_
#define _BRAHMA2AM_H_

/* Standard Active Message handler types */
typedef void (*BR_handler_0_t)(BR_cluster_t);
typedef void (*BR_handler_1_t)(BR_cluster_t,BR_word_t);
typedef void (*BR_handler_2_t)(BR_cluster_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_3_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t);
typedef void (*BR_handler_4_t)(BR_cluster_t,BR_word_t,BR_word_t,BR_word_t,BR_word_t);

/* Bulk xfer */
typedef void (*BR_handler_mem_t)(BR_cluster_t,caddr_t,size_t,caddr_t);
typedef void (*BR_signal_handler_t)(void);
typedef void (*BR_handler_t)(BR_cluster_t,...);

#define BR_REQUEST_0(c,h) qq_request_0((qqvnn_t)c,(handler_0_t)h) 
#define BR_REQUEST_1(c,h,a0) qq_request_1((qqvnn_t)c,(handler_1_t)h,(long)a0) 

#define BR_REQUEST_2(c,h,a0,a1) qq_request_2((qqvnn_t)c,(handler_2_t)h,(long)a0,(long)a1)
#define BR_REQUEST_3(c,h,a0,a1,a2) qq_request_3((qqvnn_t)c,(handler_3_t)h,(long)a0,(long)a1,(long)a2)
#define BR_REQUEST_4(c,h,a0,a1,a2,a3) qq_request_4((qqvnn_t)c,(handler_4_t)h,(long)a0,(long)a1,(long)a2,(long)a3)


#define BR_REPLY_0(c,h) qq_reply_0((qqvnn_t)c,(handler_0_t)h) 
#define BR_REPLY_1(c,h,a0) qq_reply_1((qqvnn_t)c,(handler_1_t)h,(long)a0) 

#define BR_REPLY_2(c,h,a0,a1) qq_reply_2((qqvnn_t)c,(handler_2_t)h,(long)a0,(long)a1)
#define BR_REPLY_3(c,h,a0,a1,a2) qq_reply_3((qqvnn_t)c,(handler_3_t)h,(long)a0,(long)a1,(long)a2)
#define BR_REPLY_4(c,h,a0,a1,a2,a3) qq_reply_4((qqvnn_t)c,(handler_4_t)h,(long)a0,(long)a1,(long)a2,(long)a3)

#define BR_STORE(c,from,to,size,h,a0) qq_store((qqvnn_t)c,(void *)from,(void *)to,(int)size,(handler_mem_t)h,(void *)a0)
#define BR_ASYNC_STORE(c,from,to,size,h1,a1,h2,a2) qq_store_async((qqvnn_t)c,(void *)from,(void *)to,(int)size,(handler_mem_t)h1,(void *)a1,(handler_mem_t)h2,(void *)a2)
#define BR_GET(c,from,to,size,h,a0) qq_get((qqvnn_t)c,(void *)from,(void *)to,(int)size,(handler_mem_t)h,(void *)a0)

/* These are not a part of standrard GAM, however they were adaded to
 * TCP and LAM versions by Claudio Fleiner.
 * If not defined by AM, these may need to implemented elsewhere
 */
#define BR_FORK_0(cl,h) thr_create_0((qqvnn_t)cl,(handler_0_t)h)
#define BR_FORK_1(cl,h,a0) thr_create_1((qqvnn_t)cl,(handler_1_t)h,(long)a0)
#define BR_FORK_2(cl,h,a0,a1) thr_create_2((qqvnn_t)cl,(handler_2_t)h,(long)a0,(long)a1)
#define BR_FORK_3(cl,h,a0,a1,a2) thr_create_3((qqvnn_t)cl,(handler_3_t)h,(long)a0,(long)a1,(long)a2)
#define BR_FORK_4(cl,h,a0,a1,a2,a3) thr_create_4((qqvnn_t)cl,(handler_4_t)h,(long)a0,(long)a1,(long)a2,(long)a3)
#define BR_FORK_5(cl,h,a0,a1,a2,a3) thr_create_5((qqvnn_t)cl,(handler_4_t)h,(long)a0,(long)a1,(long)a2,(long)a3)

#define BR_CLUSTERS() qq_procs()
#define BR_HERE() qq_my_proc()
#define BR_MAX_XFER() qq_max_size()

#endif





