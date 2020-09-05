/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
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

/**
 * @file
 * This file contains a few macros which thunk GTK2 functions to
 * their corresponding GTK1 functions. 
 */

#include <config.h>

#ifndef GUI_GTK_H
#define GUI_GTK_H

#include <gnome.h>
#include <glade/glade.h>

#define G_SIGNAL_MATCH_DATA (1 << 4)

#define g_signal_handlers_block_matched(object, mask, signal_id, detail, closure, func, data) \
  if(mask != G_SIGNAL_MATCH_DATA) { \
    FAIL("can only emulate G_SIGNAL_MATCH_DATA\n"); \
    return; \
  } \
  gtk_signal_handler_block_by_data(object, data)

#define g_signal_handlers_unblock_matched(object, mask, signal_id, detail, closure, func, data) \
  if(mask != G_SIGNAL_MATCH_DATA) { \
    FAIL("can only emulate G_SIGNAL_MATCH_DATA\n"); \
    return; \
  } \
  gtk_signal_handler_unblock_by_data(object, data)

#define g_main_context_iteration(context, may_block) gtk_main_iteration(may_block)
#define gdk_draw_drawable        gdk_draw_pixmap
#define g_path_get_dirname       g_dirname
#define g_timeout_add            gtk_timeout_add
#define g_source_remove          gtk_timeout_remove
#define gtk_container_get_children  gtk_container_children
#define glade_xml_new(file, root, dummy) glade_xml_new(file, root)
#define g_signal_connect(a, args...)        gtk_signal_connect(a, args)
#define g_signal_handlers_disconnect_by_func gtk_signal_disconnect_by_func
#define g_signal_connect_swapped gtk_signal_connect_object
#define gtk_spin_button_get_value gtk_spin_button_get_value_as_float
#define g_object_ref(a)          gtk_object_ref(GTK_OBJECT(a))
#define g_object_unref(a)          gtk_object_unref(GTK_OBJECT(a))
#define G_OBJECT                GTK_OBJECT
#define G_CALLBACK              GTK_SIGNAL_FUNC
#define GCallback               GtkSignalFunc
#define g_object_set_data       gtk_object_set_data
#define g_object_get_data       gtk_object_get_data
#define gtk_progress_bar_set_fraction gtk_progress_set_percentage

#define GTK_COLOR_BUTTON        GNOME_COLOR_PICKER
#define gtk_editable_select_region(w, s, e) gtk_entry_select_region(GTK_ENTRY(w), s, e);

void 
gtk_color_button_set_color(GnomeColorPicker *color_button,
                           const GdkColor *color);

void 
gtk_color_button_get_color(GnomeColorPicker *color_button,
                           GdkColor *color);

GtkWidget *
gtk_image_new_from_pixmap(GdkPixmap *pixmap,
                          GdkBitmap *alpha);

void
gdk_window_process_updates(GdkWindow *window,
                           gboolean update_children);

const char *
gtk_label_get_text(GtkLabel *w);

gchar*
g_path_get_basename (const gchar   *file_name);

void 
g_static_mutex_init (GStaticMutex *mutex);

#endif /* GUI_GTK_H */
