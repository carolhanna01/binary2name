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
#include <gui_gtk.h>

/**
 * @file
 * This file provides some GLib2/GTK2 functions not available in GTK1.
 */

void
gdk_window_process_updates(GdkWindow *window,
                           gboolean update_children) {
}

void 
g_static_mutex_init (GStaticMutex *mutex)
{
    static GStaticMutex init_mutex = G_STATIC_MUTEX_INIT;

    g_return_if_fail (mutex);

    memcpy (mutex, &init_mutex, sizeof (GStaticMutex));
}

/**
 * Note:  Copied verbatim from gutils.c, CVS version 1.157
 * Note:  See http://cvs.gnome.org/viewcvs/glib/glib/gutils.c?rev=1.157&view=auto
 * Note:  Copyright remains with original authors
 *
 * g_path_get_basename:
 * @file_name: the name of the file.
 *
 * Gets the last component of the filename. If @file_name ends with a 
 * directory separator it gets the component before the last slash. If 
 * @file_name consists only of directory separators (and on Windows, 
 * possibly a drive letter), a single separator is returned. If
 * @file_name is empty, it gets ".".
 *
 * Return value: a newly allocated string containing the last component of 
 *   the filename.
 */

#define G_IS_DIR_SEPARATOR(c) (c == '/')

gchar*
g_path_get_basename (const gchar   *file_name)
{
  register gssize base;             
  register gssize last_nonslash;    
  gsize len;    
  gchar *retval;
 
  g_return_val_if_fail (file_name != NULL, NULL);

  if (file_name[0] == '\0')
    /* empty string */
    return g_strdup (".");
  
  last_nonslash = strlen (file_name) - 1;

  while (last_nonslash >= 0 && G_IS_DIR_SEPARATOR (file_name [last_nonslash]))
    last_nonslash--;

  if (last_nonslash == -1)
    /* string only containing slashes */
    return g_strdup (G_DIR_SEPARATOR_S);

  base = last_nonslash;

  while (base >=0 && !G_IS_DIR_SEPARATOR (file_name [base]))
    base--;

  len = last_nonslash - base;
  retval = g_malloc (len + 1);
  memcpy (retval, file_name + base + 1, len);
  retval [len] = '\0';
  return retval;
}

const char *
gtk_label_get_text(GtkLabel *w) {
    const char *s;

    gtk_label_get(w, (char **)&s);

    return s;
}

GtkWidget *
gtk_image_new_from_pixmap(GdkPixmap *pixmap,
                          GdkBitmap *alpha) {
    int w, h;
    GdkImage *image;
    GtkWidget *widget;

    gdk_window_get_size(pixmap, &w, &h);
    image = gdk_image_get(pixmap, 0, 0, w, h);

    if(!image) {
        FAIL("cannot get image from pixmap\n");
        return NULL;
    }

    widget = gtk_image_new(image, alpha);
    gtk_widget_ref(widget);

    return widget;
}

void 
gtk_color_button_set_color(GnomeColorPicker *color_picker,
                           const GdkColor *color) {
    gnome_color_picker_set_i16(color_picker, 
                               color->red,
                               color->green,
                               color->blue,
                               0);
    
}

void 
gtk_color_button_get_color(GnomeColorPicker *color_picker,
                           GdkColor *color) {
    gushort dummy;
    gnome_color_picker_get_i16(color_picker, 
                               &color->red,
                               &color->green,
                               &color->blue,
                               &dummy);

}
