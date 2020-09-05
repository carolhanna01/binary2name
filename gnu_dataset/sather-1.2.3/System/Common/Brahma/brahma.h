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

#ifndef BRAHMA_H
#define BRAHMA_H

#if defined(BR_SERIAL)
# include "serial/serial.h"
#elif defined(BR_LWP_LINUX)
# include "lwp_linux/lwp_linux.h"
#elif defined(BR_SMP_SOLARIS)
# include "smp_solaris/smp_solaris.h"
# include "solaris_common/solaris_sync.h"
#elif defined(BR_AT_SOLARIS_SMP)
# include "at_solaris_smp/at_solaris_smp.h"
# include "at_common/at_sync.h"
#elif defined(BR_AT_LINUX_SMP)
# include "at_linux_smp/at_linux_smp.h"
# include "at_common/at_sync.h"
#elif defined(BR_AT_WIN32_SMP)
# include "at_win32_smp/at_win32_smp.h"
# include "at_common/at_sync.h"
#elif defined(BR_TCP_SOLARIS) 
# include "tcp_solaris/tcp_solaris.h"
# include "solaris_common/solaris_sync.h"
#elif defined(BR_MYRINET_SOLARIS)
# include "myrinet_solaris/src/myrinet_solaris.h"  
# include "solaris_common/solaris_sync.h"
#elif defined(BR_MYRINET_SOLARIS_AT)
# include "myrinet_solaris/src/myrinet_solaris.h"  
# include "at_common/at_sync.h"
#elif defined(BR_OSF_AT_AXP_SMP)
#  include "osf_at_axp_smp/osf_at_axp_smp.h"
# include "at_common/at_sync.h"
#elif defined(BR_HPUX_AT_HPPA_SMP)
#  include "hpux_at_hppa_smp/hpux_at_hppa_smp.h"
# include "at_common/at_sync.h"
#else
# error Undefined platform in brahma.h
#endif

#endif
