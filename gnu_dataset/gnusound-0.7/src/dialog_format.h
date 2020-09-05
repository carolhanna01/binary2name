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

#ifndef DIALOG_FORMAT_H
#define DIALOG_FORMAT_H

#include <config.h>
#include "shell.h"
#include "dialog.h"

struct _shell;

struct dialog_format {
    struct dialog dialog;
    char *filename;
    struct file *file;
    struct file_driver *fd;
    GtkWidget *options_widget;
};

struct save_gui_format {
    char *id;
    struct file_format *format;
};

struct dialog *
dialog_format_new(shell *shl,
                  struct file *file);

int
dialog_format_init(struct dialog_format *df,
                   shell *shl,
                   struct file *file);

void
dialog_format_apply(struct dialog *dialog,
                    void *user_data);

#endif /* ! DIALOG_FORMAT_H */
