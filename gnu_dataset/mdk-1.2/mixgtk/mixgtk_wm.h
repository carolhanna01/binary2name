/* -*-c-*- ---------------- mixgtk_wm.h :
 * Functions for window management
 * ------------------------------------------------------------------
 * $Id: mixgtk_wm.h,v 1.4 2004/07/02 00:13:16 jao Exp $
 * ------------------------------------------------------------------
 * Copyright (C) 2001, 2004 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */


#ifndef MIXGTK_WM_H
#define MIXGTK_WM_H

#include "mixgtk.h"

typedef enum {
  MIXGTK_MIXVM_WINDOW,
  MIXGTK_MIXAL_WINDOW,
  MIXGTK_DEVICES_WINDOW
} mixgtk_window_id_t;

extern gboolean
mixgtk_wm_init (void);

extern void
mixgtk_wm_attach_window (mixgtk_window_id_t w);

extern void
mixgtk_wm_detach_window (mixgtk_window_id_t w);

#endif /* MIXGTK_WM_H */

