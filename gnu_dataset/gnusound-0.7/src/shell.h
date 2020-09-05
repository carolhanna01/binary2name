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

#ifndef SHELL_H
#define SHELL_H

#include <config.h>
#include <glade/glade.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>
#include <gnome.h>
#include <pthread.h>
#include <audiofile.h>
#include "snd.h"
#include "mixer.h"
#include "module.h"
#include "marker.h"
#include "grid.h"
#include "player.h"
#include "file.h"
#include "view.h"
#include "tool.h"
#include "clip.h"
#include "cmd.h"
#include "history.h"
#include "constraints.h"

#define LOOP_IS_ACTIVE(shl) ((shl)->loop && (shl)->loop_start != (shl)->loop_end)

#define DEBUG_FLAG_STEP_MODE   0x01
#define DEBUG_FLAG_DRAW_BLOCKS 0x02

#define WHEEL_FORWARD 0
#define WHEEL_BACKWARD 1
#define SCRUB_LEFT -1
#define SCRUB_RIGHT 1

struct cmd;
struct module_state;
struct tool;
struct file;
struct file_params;

typedef struct _shell {

    /* Contains 'Ok'. */

    char magic[2];

    /* The clip (sound info, mixer, markers). */

    struct clip *clip;

    /* The command history. */

    struct history *history;

    /* The playback/record engine. */

    struct player *player;

    /* The view. */

    struct view *view;

    /* The grid. */

    struct grid grid;

    /* This flag is set when the current operation should be cancelled. */

    int cancel_requested;

    /* Shell is about to close. */

    int close_requested;

    /* Loop start and end. */

    AFframecount loop_start;
    AFframecount loop_end;

    /* Selection; select_channel_map is a bitfield specifying which
       channels are selected. */

    int select_channel_map;
    AFframecount select_start;
    AFframecount select_end;

    /* Fields above are part of the binary API, don't change. */

    /* Increasing number for labels. */
    
    int label;
    char label_str[16];

    /* Currently active tool. Use shell_get_active_tool() to access. */

    const char *active_tool;

    /* Current action recursion depth, when user closes this shell
       we wait for this to become zero before closing. */

    int use;

    /* State for the modules in this shell. Every loaded module gets a
       slot in this array to store their state. Use
       shell_get_module_state() to access a slot. */

    struct module_state module_state[MAX_MODULES];

    /* Misc flags. */

    unsigned int loop: 1;
    unsigned int has_changed: 1;
    unsigned int has_name: 1;
    unsigned int snap_to_grid: 1;
    unsigned int snap_to_cuepoints: 1;
    unsigned int record_mode: 1;
    unsigned int record_replace: 1;
    unsigned int debug_flags: 1;

    /* Tools. */

    GHashTable *tools;

    /* Information about the file associated with this document. */

    struct file *file;
    
    /* Active constraints. */

    struct constraints *constraints;

    /* Miscellaneous user data. */

    //    GHashTable *user_data;

} shell;

const char *
shell_get_next_label(shell *shl);

void
shell_attach_file(shell *shl,
                  struct file *file);

int
shell_configure(shell *shl,
                const char *filename,
                struct file_params *params);

void
shell_start_operation(shell *shl,
                      const char *operation);

void
shell_end_operation(shell *shl);

void
shell_bind_tool(shell *shl,
                struct tool *tool);

void
shell_activate_tool(shell *shl,
                    const char *name);

struct tool *
shell_get_active_tool(shell *shl);

struct tool *
shell_get_tool(shell *shl, 
               const char *name);

struct module_state *
shell_get_module_state(shell *shl, 
                       int id);

struct grid *
shell_get_grid(shell *shl);


int
shell_is_clipboard(shell *shl);

void
shell_dispatch(shell *shl,
               struct cmd *cmd);

void
shell_dispatch_as(shell *shl,
                  struct cmd *cmd,
                  const char *name);

void
shell_destroy_internal(shell *shl);

void
shell_destroy(shell *shl);

shell *
shell_new(struct clip *clip);

#endif /* ! SHELL_H */
