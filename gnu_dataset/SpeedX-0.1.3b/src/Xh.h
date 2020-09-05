/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef __XH_H

#define __XH_H



#include "config.h"


#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>



#include <X11/Xlib.h>

#include <X11/Xutil.h>

#include <X11/keysym.h>

#include <X11/cursorfont.h>



/* MIT-SHM */

#if MITSHM



#ifdef HPUX



#include <X11R6/X11/extensions/XShm.h>



#else



#include <X11/extensions/XShm.h>



#endif /* HPUX */



#endif /* MITSHM */



#endif /* __XH_H */



