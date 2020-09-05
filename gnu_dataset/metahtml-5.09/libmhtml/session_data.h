/* session_data.h: Header for persistant association of data and session. */

/* Author: Brian J. Fox (bfox@ua.com) Thu Jul  6 10:07:54 1995.

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

#if !defined (_SESSION_DATA_H_)
#define _SESSION_DATA_H_

#include <forms.h>
#include <session.h>

#if defined (__cplusplus)
extern "C"
{
#endif

/* The functions declared in this file manipulate the translation of
   an array of POSTED_ITEM * with the contents of a SESSION_INFO->data
   pointer. */

/* Given the ASCII representation of an alist in INFO->data, return an
   array of POSTED_ITEM * representing that data. */
extern void sd_info_to_package (SESSION_INFO *info, Package *package);

/* Given an array of POSTED_ITEM *, replace the data in INFO with an
   ASCII representation of an alist. */
extern void sd_package_to_info (SESSION_INFO *info, Package *package);

#if defined (__cplusplus)
}
#endif

#endif /* !_SESSION_DATA_H_ */

