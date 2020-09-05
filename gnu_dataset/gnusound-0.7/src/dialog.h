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

#ifndef DIALOG_H
#define DIALOG_H

#include <config.h>
#include "pane.h"
#include "shell.h"

#define DIALOG_IS_OPEN (1 << 0)

struct dialog {
    char *root;
    struct _shell *shl;
    struct pane *pane;
    int flags;
    void *user_data;
    void (*open)(struct dialog *dialog,
                 void *user_data);
    void (*apply)(struct dialog *dialog,
                  void *user_data);
    void (*close)(struct dialog *dialog,
                  void *user_data);
    void (*dtor)(struct dialog *dialog,
                 void *user_data);
};

struct dialog *
dialog_new(size_t size,
           const char *glade_xml_file,
           const char *name,
           struct _shell *shl,
           int n_bindings,
           const struct pane_signal_bindings bindings[],
           void *user_data);

int
dialog_init(struct dialog *dialog,
            const char *glade_xml_file,
            const char *name,
            struct _shell *shl,
            int n_bindings,
            const struct pane_signal_bindings bindings[],
            void *user_data);

void
dialog_attach_child(struct dialog *dialog,
                    GtkWidget *widget);

void
dialog_detach_child(struct dialog *dialog,
                    GtkWidget *widget);

void
dialog_destroy(struct dialog *dialog);

void
dialog_open(struct dialog *dialog);

void
dialog_close(struct dialog *dialog);

#endif /* DIALOG_H */
