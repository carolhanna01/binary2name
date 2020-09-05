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

#ifndef DIALOG_MIXDOWN_H
#define DIALOG_MIXDOWN_H

#include <config.h>
#include "shell.h"
#include "pane.h"
#include "file.h"
#include "dialog.h"

struct _shell;

struct dialog_mixdown {
    struct dialog dialog;
    struct file *file;
};

struct dialog *
dialog_mixdown_new(struct _shell *shl,
                   struct file *file);

#endif /* ! DIALOG_MIXDOWN_H */

