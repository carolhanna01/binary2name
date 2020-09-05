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

#ifndef GUI_LETTERBOX_H
#define GUI_LETTERBOX_H

#include <config.h>
#include <gtk/gtk.h>

struct gui_letterbox {
    GtkWidget *widget;
    const char *string;
};

struct gui_letterbox_extents {
    int lbearing;
    int rbearing;
    int ascent;
    int descent;
    int width;
    int height;
};

struct gui_letterbox *
gui_letterbox_new(GtkWidget *w,
                  const char *s);

void
gui_letterbox_get_extents(struct gui_letterbox *lb,
                          struct gui_letterbox_extents *ink_lbe,
                          struct gui_letterbox_extents *logical_lbe);

void
gui_letterbox_draw(struct gui_letterbox *lb,
                   GdkDrawable *drawable,
                   int x,
                   int y);

void
gui_letterbox_destroy(struct gui_letterbox *lb);

#endif /* ! GUI_LETTERBOX_H */
