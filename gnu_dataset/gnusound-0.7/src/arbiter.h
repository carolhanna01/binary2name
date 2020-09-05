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

#ifndef ARBITER_H
#define ARBITER_H

#include <config.h>
#include <glib.h>
#include <audiofile.h>
#include "lib/cpudetect.h"
#include "snd.h"
#include "shell.h"
#include "marker.h"
#include "cmd.h"
#include "dialog.h"

struct arbiter {
    GList *shells;
    GList *cmd_queue;
    GStaticMutex cmd_queue_lock;
    int quit_requested;
    int busy;
    struct marker_list_array *cuepoints_clipboard;
    struct marker_list_array *envelope_clipboard;
};

int
arbiter_init();

void
arbiter_exit();

CpuCaps *
arbiter_get_cpu_caps();

int
arbiter_yield();

int
arbiter_is_quit_requested();

void
arbiter_request_quit();

shell *
arbiter_find_shell(int (*select)(shell *shl,
                                 void *user_data),
                   void *user_data);

void
arbiter_foreach_shell(void (*callback)(shell *shl, 
                                       void *user_data),
                      void *user_data);

void
arbiter_add_shell(shell *shl);

void
arbiter_remove_shell(shell *shl);

GList *
arbiter_get_shells();

struct marker_list_array *
arbiter_get_cuepoints_clipboard();

struct marker_list_array *
arbiter_get_envelopes_clipboard();

void
arbiter_queue_cmd(struct cmd *cmd);

int 
arbiter_run(int argc,
            char *argv[]);

#endif /* ARBITER_H */
