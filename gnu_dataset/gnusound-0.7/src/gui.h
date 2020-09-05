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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef GUI_H
#define GUI_H

#include <config.h>

#ifdef HAVE_GNOME2
# define GTK_DISABLE_DEPRECATED   1
# define GNOME_DISABLE_DEPRECATED 1
# define GUI_GTK_VERSION_TAG      "-2"
#else
# define GUI_GTK_VERSION_TAG      ""
#endif

#define GLADE_FILE                ("gnusound" GUI_GTK_VERSION_TAG ".glade")
#define GTKRC_FILE                ("gnusound" GUI_GTK_VERSION_TAG ".rc")
#define LOGO_FILE                 ("logo.xpm")

#define LOGO_SEARCH_PATH          ("gui:" GUIDIR)
#define GLADE_SEARCH_PATH         (".:gui:" GUIDIR)
#define GTKRC_SEARCH_PATH         ("gui:" GUIDIR)

#define GUI_CURSOR_CACHE          4

#include <gui_gtk.h>
#include <gui_letterbox.h>
#include <gui_file_selector.h>
#include <gui_dialogs.h>
#include <glade/glade.h>

extern const guint gtk_major_version;
extern const guint gtk_minor_version;

int 
gui_init();

void
gui_exit();

GdkWindow *
gui_get_window(GtkWidget *w);

void
gui_window_set_cursor(GdkWindow *w,
                      GdkCursorType type);

void
gui_get_widget_position_absolute(GtkWidget *w,
                                 int *x,
                                 int *y);

void
gui_get_pixmap(const char *name,
               GdkPixmap **pixmap,
               GdkBitmap **mask);

GladeXML *
gui_get_xml(const char *path,
            const char *root);

GdkColor *
gui_get_color(const char *name);

void
gui_color_to_hex_string(char *hex,
                        GdkColor *color);

void
gui_alloc_colors();

struct dialog *
gui_get_dialog(const char *name);

#endif /* ! GUI_H */
