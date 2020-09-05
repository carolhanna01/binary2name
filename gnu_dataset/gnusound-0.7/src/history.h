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

#ifndef HISTORY_H
#define HISTORY_H

#include <config.h>
#include <glib.h>
#include "cmd.h"

enum history_state {
    HISTORY_NORMAL,
    HISTORY_GO_BACK,
    HISTORY_GO_FORWARD,
    HISTORY_ROLLBACK
};

struct history_transition {
    char *what;
    GList *how;
    int committed;
};

struct history {
    /** The position in the history, -1 meaning the beginning. */
    int position;
    /** The nested command depth. */
    int depth;
    /** What state the history is in, i.e. whether it's going 
        forward or backward. */
    enum history_state state;
    /** The stack of transitions. */
    GPtrArray *transitions;
    /** The currently running transitions. */
    GPtrArray *pending;
    /** The saved redo information. */
    GPtrArray *stored_future;
};

struct cmd;

struct cmd_value *
history_go_back(struct history *h);

struct cmd_value *
history_go_forward(struct history *h);

int
history_begin(struct history *h,
              const char *what);

void
history_remember(struct history *h,
                 struct cmd *how);

void
history_commit(struct history *h);

void
history_rollback(struct history *h);

const struct history_transition *
history_get_previous(struct history *h);

const struct history_transition *
history_get_next(struct history *h);

int
history_get_depth(struct history *h);

void
history_clear(struct history *h);

struct history *
history_new();

void
history_destroy(struct history *h);

#endif /* ! HISTORY_H */
