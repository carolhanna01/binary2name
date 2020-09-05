/* symdump.h: -*- C -*-  DESCRIPTIVE TEXT. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Sun Jul 21 00:54:24 1996.

   This file is part of <Meta-HTML>(tm), a system for the rapid deployment
   of Internet and Intranet applications via the use of the Meta-HTML
   language.

   Copyright (c) 1995, 1996, Brian J. Fox (bfox@ai.mit.edu).
   Copyright (c) 1996, Universal Access Inc. (http://www.ua.com).

   Meta-HTML is free software; you can redistribute it and/or modify
   it under the terms of the UAI Free Software License as published
   by Universal Access Inc.; either version 1, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   UAI Free Software License for more details.

   You should have received a copy of the UAI Free Software License
   along with this program; if you have not, you may obtain one by
   writing to:

   Universal Access Inc.
   129 El Paseo Court
   Santa Barbara, CA
   93101  */

#if defined (__cplusplus)
extern "C"
{
#endif

/* Write out PACKAGE to FD such that SYMBOL_LOAD_PACKAGE can read
   it in again. */
extern void symbol_dump_package (int fd, Package *package);

/* Read a package from FD, installing the symbols and values found within. */
extern Package *symbol_load_package (int fd);

/* Return an SD (like an FD, but for the string buffer pool.
   Newly allocates a buffer.
   The SD returned is suitable for passing to symbol_load_package ()
   and symbol_dump_package (). */
extern int symdump_open_string_data (void);

/* Return the BPRINTF_BUFFER corresponding to SD, and return the allocated
   slot back to the pool.  Does NOT free the BPRINTF_BUFFER. */
extern BPRINTF_BUFFER *symdump_close_string_data (int sd);

/* Read LEN bytes from SD into DATA. */
extern int symdump_read_string_data (int sd, int len, unsigned char *data);

/* Write LEN bytes to SD from DATA. */
extern int symdump_write_string_data (int sd, int len, unsigned char *data);

/* Set the current offstring in SD to OFFSET. */
extern void symdump_seek_string_data (int sd, int offset);

/* Set the current offstring in SD to OFFSET. */
extern void symdump_set_string_data_buffer_size (int sd, int end);

#if defined (__cplusplus)
}
#endif
