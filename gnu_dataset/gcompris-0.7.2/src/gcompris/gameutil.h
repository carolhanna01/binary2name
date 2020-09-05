/* gcompris - gameutil.h
 *
 * Time-stamp: <2000/07/16 00:32:26 bruno>
 *
 * Copyright (C) 2000 Bruno Coudoin
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#ifndef GAMEUTIL_H
#define GAMEUTIL_H

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gnome-canvas-pixbuf.h>

#include "gcompris.h"

GdkPixbuf	*gcompris_load_operation_pixmap(char operation);
GdkPixbuf	*gcompris_load_number_pixmap(char number);
GdkPixbuf	*gcompris_load_pixmap(char *pixmapfile);
void		 gcompris_set_image_focus(GdkPixbuf *pixmap, gboolean focus);
gint		 gcompris_item_event_focus(GnomeCanvasItem *item, GdkEvent *event, GdkPixbuf *pixmap);
void		 gcompris_play_sound (const char *soundlistfile, const char *which);
GcomprisBoard	*gcompris_read_xml_file(char *fname);

#endif
