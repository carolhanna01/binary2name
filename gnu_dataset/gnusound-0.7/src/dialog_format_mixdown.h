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

#ifndef DIALOG_FORMAT_MIXDOWN_H
#define DIALOG_FORMAT_MIXDOWN_H

#include <config.h>
#include "shell.h"
#include "file.h"
#include "dialog.h"
#include "dialog_format.h"

struct dialog_format_mixdown {
    struct dialog_format parent;
    track_map_t map;
};

struct dialog *
dialog_format_mixdown_new(shell *shl,
                          struct file *file,
                          track_map_t map);


#endif /* ! DIALOG_FORMAT_MIXDOWN_H */
