/* language.h: -*- C -*-  Typical header for language definition files.. */

/*  Copyright (c) 1996 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Wed Dec 25 01:11:10 1996.

    This file is part of <Meta-HTML>(tm), a system for the rapid
    deployment of Internet and Intranet applications via the use of
    the Meta-HTML language.

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

#define LANGUAGE_DEFINITIONS_FILE 1
#define MHTML_ARITHMETIC 1

#if defined (_GNUC_) && !defined (VOLATILE)
#  define VOLATILE volatile
#endif

#if !defined (VOLATILE)
#  define VOLATILE
#endif

#include "mhtmlstd.h"
#include "forms.h"
#include "session.h"
#include "pages.h"
#include "parser.h"
#include "streamfuncs.h"
#include "bootstrapper.h"
