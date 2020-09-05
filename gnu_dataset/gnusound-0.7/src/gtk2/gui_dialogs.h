/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2004  Pascal Haakmat <a.haakmat@chello.nl>
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
 * A copy of the GNU General Public License can be found in the file
 * LICENSE in the top directory of the source distribution. If not,
 * write to the Free Software * Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 */

#ifndef GUI_DIALOGS_H
#define GUI_DIALOGS_H

#include <config.h>
#include <glib.h>

#define GUI_CANCEL -1
#define GUI_YES 0
#define GUI_NO 1

void 
gui_alert(const gchar *format, 
          ...);

int 
gui_yes_no(const char *title, 
           const char *format,
           ...);

#endif /* ! GUI_DIALOGS_H */
