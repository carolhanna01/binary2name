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

#ifndef GUI_FILE_SELECTOR_H
#define GUI_FILE_SELECTOR_H

#include <config.h>
#include <gtk/gtk.h>

enum gui_file_selector_type {
    GUI_FILE_SELECTOR_OPEN,
    GUI_FILE_SELECTOR_SAVE
};

void
gui_file_selector_run(GtkWidget *fs);

GtkWidget *
gui_file_selector_new(const char *title,
                      const char *path,
                      enum gui_file_selector_type type,
                      void (*select_handler)(const char *fn, 
                                             void *user_data),
                      void (*destroy_handler)(void *user_data),
                      void *user_data);

#endif /* ! GUI_FILE_SELECTOR_H */
