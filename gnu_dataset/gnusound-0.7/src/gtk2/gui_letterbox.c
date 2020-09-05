/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2005  Pascal Haakmat <a.haakmat@chello.nl>
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

#include <config.h>
#include <stdlib.h>
#include <gui_letterbox.h>

struct gui_letterbox *
gui_letterbox_new(GtkWidget *w,
                  const char *s) {
    struct gui_letterbox *lb = malloc(sizeof(*lb));

    if(!lb)
        return NULL;

    lb->widget = w;
    lb->layout = gtk_widget_create_pango_layout(w, s);

    return lb;
}

void
gui_letterbox_get_extents(struct gui_letterbox *lb,
                          struct gui_letterbox_extents *ink_lbe,
                          struct gui_letterbox_extents *logical_lbe) {
    PangoRectangle ink_rect, logical_rect;

    PangoLayoutLine *pll = pango_layout_get_line(lb->layout, 0);

    pango_layout_line_get_pixel_extents (pll,
                                         &ink_rect, 
                                         &logical_rect);

    if(ink_lbe) {
        ink_lbe->lbearing = PANGO_LBEARING(ink_rect);
        ink_lbe->rbearing = PANGO_RBEARING(ink_rect);
        ink_lbe->ascent = PANGO_ASCENT(ink_rect);
        ink_lbe->descent = PANGO_DESCENT(ink_rect);
        ink_lbe->width = ink_rect.width;
        ink_lbe->height = ink_rect.height;
    }

    if(logical_lbe) {
        logical_lbe->lbearing = PANGO_LBEARING(logical_rect);
        logical_lbe->rbearing = PANGO_RBEARING(logical_rect);
        logical_lbe->ascent = PANGO_ASCENT(logical_rect);
        logical_lbe->descent = PANGO_DESCENT(logical_rect);
        logical_lbe->width = logical_rect.width;
        logical_lbe->height = logical_rect.height;
    }
}

void
gui_letterbox_draw(struct gui_letterbox *lb,
                   GdkDrawable *drawable,
                   int x,
                   int y) {
    PangoLayoutLine *pll = pango_layout_get_line(lb->layout, 0);
                                                 
    gdk_draw_layout_line(drawable,
                         lb->widget->style->fg_gc[lb->widget->state],
                         x,
                         y,
                         pll);
    
}

void
gui_letterbox_destroy(struct gui_letterbox *lb) {
    g_object_unref(G_OBJECT(lb->layout));
    free(lb);
}
