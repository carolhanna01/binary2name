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

#ifndef _HEADER_H_
#define _HEADER_H_
#include "../../../Common/c_header.h"
#include "../../../Common/runtime.h"
#define BRAHMA
#define BR_OSF_AT_AXP_SMP
#include "../../../Common/Brahma/brahma.h"
#ifndef _PSATHER_H_
#include "../../../pSather/pSather/pSather.h"
#endif

#undef GC_ENABLE_INCREMENTAL

#endif
