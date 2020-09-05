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

#ifndef DIALOG_SAVE_H
#define DIALOG_SAVE_H

#include <config.h>
#include "shell.h"
#include "dialog.h"

struct _shell;

struct dialog_save {
    struct dialog dialog;
    int keep_file;
    char *filename;
    struct file *file;
    struct file_driver *fd;
    void (*ok_func)(struct _shell *shl,
                    struct file *file,
                    void *user_data);
    void *user_data;
    GtkWidget *options_widget;
};

struct save_gui_format {
    char *id;
    struct file_format *format;
};

struct dialog *
dialog_save_new(shell *shl,
                struct file *file,
                void (*on_ok)(struct _shell *shl,
                              struct file *file,
                              void *user_data),
                void *user_data);

#endif /* ! DIALOG_SAVE_H */
