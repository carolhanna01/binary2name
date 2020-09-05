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

#include <config.h>
#include <stdlib.h>
#include <gui_letterbox.h>

struct gui_letterbox *
gui_letterbox_new(GtkWidget *w,
                  const char *s) {
    struct gui_letterbox *lb = malloc(sizeof(*lb));
    lb->widget = w;
    lb->string = s;
    return lb;
}

void
gui_letterbox_get_extents(struct gui_letterbox *lb,
                          struct gui_letterbox_extents *ink_lbe,
                          struct gui_letterbox_extents *logical_lbe) {
    GdkGCValues gc_vals;
    int width, lbearing, rbearing, ascent, descent;

    gdk_gc_get_values(lb->widget->style->fg_gc[lb->widget->state], &gc_vals);
    gdk_string_extents(gc_vals.font,
                       lb->string,
                       &lbearing,
                       &rbearing,
                       &width,
                       &ascent,
                       &descent);

    if(ink_lbe) {
        ink_lbe->lbearing = lbearing;
        ink_lbe->rbearing = rbearing;
        ink_lbe->ascent = ascent;
        ink_lbe->descent = descent;
        ink_lbe->width = width;
        ink_lbe->height = ascent + descent + 2;
    }
    
    if(logical_lbe) {
        logical_lbe->lbearing = lbearing;
        logical_lbe->rbearing = rbearing;
        logical_lbe->ascent = ascent;
        logical_lbe->descent = descent;
        logical_lbe->width = width;
        logical_lbe->height = ascent + descent;
    }

}

void
gui_letterbox_draw(struct gui_letterbox *lb,
                   GdkDrawable *drawable,
                   int x,
                   int y) {
    GdkGCValues gc_vals;
    int width, lbearing, rbearing, ascent, descent;

    gdk_gc_get_values(lb->widget->style->fg_gc[lb->widget->state], &gc_vals);

    gdk_string_extents(gc_vals.font,
                       lb->string,
                       &lbearing,
                       &rbearing,
                       &width,
                       &ascent,
                       &descent);

    gdk_draw_string(drawable,
                    gc_vals.font,
                    lb->widget->style->fg_gc[lb->widget->state],
                    x,
                    y,
                    lb->string);
}

void
gui_letterbox_destroy(struct gui_letterbox *lb) {
    free(lb);
}
