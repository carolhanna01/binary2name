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

#ifndef __AM_H_
#define __AM_H_

typedef unsigned int vnn_t;
typedef	void (*handler_t)();

#ifdef __GNUG__ 
    #define CFUNC "C"
#else
    #define CFUNC  
#endif


#define barrier() gam_barrier()
#define splitc_poll() gam_poll()
		
/* Must be called before any other am calls */
extern CFUNC int gam_enable();

/* Returns virtual node number */
extern CFUNC int gam_my_proc();

/* Returns the number of nodes */
extern CFUNC int gam_procs();

/* Must be called for a graceful shutdown */
extern CFUNC void gam_disable();

/* Not in spec. Must be called to clean up exit */
extern CFUNC void gam_exit(int code);

/* Receive messages by polling */
extern CFUNC void gam_poll();

extern CFUNC void gam_barrier();

/* Send request message with up to 4 integer arguments */
/*extern CFUNC int gam_request_4(vnn_t dest, void *fun, ...); */

/* Send request message with up to 2 integer arguments and a buffer of 
   of up to gam_max_size(). Unlike store, the remote buffer is not 
   specified, the system provides the storage (up to gam_max_size()) */
/* the remote handler is invoked with the first 2 args as the system
   buffer with the data, the length and the two user args */
extern CFUNC int gam_request(vnn_t dest,  void *fun, void *buf, int len, ... ); 

/* Send request message with 2 integer arguments and one double argument */
extern CFUNC  int gam_request_fd(vnn_t dest, void *fun, double arg1, int arg2, int arg3);


/* Send reply message with up to 4 integer arguments */
extern CFUNC int gam_reply_4(vnn_t dest, void *fun, ...);

/* Send reply message with up to 2 integer arguments and buffer up to 
   gam_max_size() */
extern CFUNC int gam_reply(int dest, handler_t fun , void *lva, int nbytes,
		    int arg0, int arg1);
/* Send reply message with 2 integer arguments and one double argument */
extern CFUNC int gam_reply_fd(vnn_t dest, void *fun, double arg1, int arg2, int arg3);

/* gam_get in spec is renamed as am_store to be consistent with Split-C.
 * Copy nbytes bytes of data from address lva on calling node to address
 * rva on dest node. Invoke handler with handler_arg on dest node when
 * all data are received on dest node. 
 */
extern CFUNC int gam_store(vnn_t dest, void *lva, void *rva, int nbytes,
						 handler_t request_handler, void *handler_arg);

/* non-blocking store. endfunc is called when send is completed on sender */
extern CFUNC int gam_store_async(vnn_t dest, void *src_buf, void *dest_buf,
							int len, handler_t request_handler, void *request_handler_arg,
							handler_t endfunc, void *endfunc_arg);
		
/* Retrive nbytes bytes of data from address rva on dest node to address
 * lva on calling node. Invoke handler with handler_arg on calling node when
 * all data are received on calling node.
 */
extern CFUNC int gam_get(vnn_t dest, void *sva, void *dva, int nbytes,
					 handler_t reply_handler, void *handler_arg);

/* Return maximum buffer size in bytes for put and get */
extern CFUNC int gam_max_size();

/* Macros for variable number of arguments */
#define gam_reply_0(dest, fun) \
	gam_reply_4(dest, fun) 
#define gam_reply_1(dest, fun, one) \
	gam_reply_4(dest, fun, one)
#define gam_reply_2(dest, fun, one, two) \
	gam_reply_4(dest, fun, one, two)
#define gam_reply_3(dest, fun, one, two, three) \
	gam_reply_4(dest, fun, one, two, three)

#define gam_request_0(node, fun) \
	gam_request_4(node, fun)
#define gam_request_1(node, fun, one) \
	gam_request_4(node, fun, one)
#define gam_request_2(node, fun, one, two) \
	gam_request_4(node, fun, one, two)
#define gam_request_3(node, fun, one, two, three) \
	gam_request_4(node, fun, one, two, three)

/* Macros for variable number of arguments -- for backward compatibility
	 with Split-C library
 */


#define gam_request0(node, fun) \
	gam_request_4(node, fun)
#define gam_request1(node, fun, one) \
	gam_request_4(node, fun, one)
#define gam_request2(node, fun, one, two) \
	gam_request_4(node, fun, one, two)
#define gam_request3(node, fun, one, two, three) \
	gam_request_4(node, fun, one, two, three)
#define gam_request4(node, fun, one, two, three, four) \
	gam_request_4(node, fun, one, two, three, four)

#define gam_reply0(dest, fun) \
	gam_reply_4(dest, fun) 
#define gam_reply1(dest, fun, one) \
	gam_reply_4(dest, fun, one)
#define gam_reply2(dest, fun, one, two) \
	gam_reply_4(dest, fun, one, two)
#define gam_reply3(dest, fun, one, two, three) \
	gam_reply_4(dest, fun, one, two, three)
#define gam_reply4(dest, fun, one, two, three, four) \
	gam_reply_4(dest, fun, one, two, three, four)


#endif





