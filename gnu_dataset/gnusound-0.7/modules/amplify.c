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

#include <gnusound.h>

static struct cmd_value *
amplify_execute(int id,
                shell *shl,
                void *data) {
    struct cmd *cmd;
    struct cmd_value *r;

    cmd = CMD_NEW("process-amplitude",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(shl->select_channel_map),
                  cmd_new_long_val(shl->select_start),
                  cmd_new_long_val(shl->select_end - shl->select_start),
                  cmd_new_double_val(1),
                  cmd_new_double_val(0));
    cmd_do_or_fail(cmd, "Cannot process amplitude (%s)", &r);

    return r;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Amplify",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2004",
    "GPL",
    NULL,
    0,

    NULL,
    NULL,
    NULL,
    amplify_execute,
    NULL,
    NULL,
    NULL
};

