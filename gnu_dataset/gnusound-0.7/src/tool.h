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

#ifndef TOOL_H
#define TOOL_H

#include <config.h>
#include <gdk/gdk.h>
#include "draw.h"
#include "shell.h"
#include "cmd.h"
#include "draw_hooks.h"

struct tool {
    /** The name of this tool. */
    const char *name;
    /** The displayname. */
    const char *displayname;
    /** A tooltip. */
    const char *tooltip;
    /** The position of this tool relative to the others. */
    int ordinal;
    /** The mouse cursor to use when this tool is activated. */
    GdkCursorType cursor;
    /** The accelerator key which activates this tool. */
    guint accel_key;
    /** The accelerator modifiers. */
    GdkModifierType accel_mods;
    /** The shell associated with this tool. */
    struct _shell *shl;
    /** Optional config panel. */
    struct pane *panel;
    /** Functions for this tool. */
    struct tool_funcs *funcs;
};

struct tool_funcs {
    /** Called when the tool's shell disappears. */
    void (*destroy)(struct tool *tool);
    
    /** Called when the user activates the tool. */
    void (*activate)(struct tool *tool);
    /** Called when the user deactivates the tool. */
    void (*deactivate)(struct tool *tool);
    
    /** Called when a button press event is received for the tool. */
    struct cmd_value *(*button_press)(struct tool *tool,
                                      GdkEventButton *event);
    /** Called when a button release event is received for the tool. */
    struct cmd_value *(*button_release)(struct tool *tool,
                                        GdkEventButton *event);
    /** Called when a mouse motion event is received for the tool. */
    struct cmd_value *(*motion)(struct tool *tool,
                                GdkEventMotion *event);
    
    /** Called when a key press event is received for the tool. */
    struct cmd_value *(*key_press)(struct tool *tool,
                                   GdkEventKey *event);
    /** Called when a key release event is received for the tool. */
    struct cmd_value *(*key_release)(struct tool *tool,
                                     GdkEventKey *event);

    /** Called when a key release event is received for the tool. */
    struct cmd_value *(*enter)(struct tool *tool,
                               GdkEventCrossing *event);
    /** Called when a key release event is received for the tool. */
    struct cmd_value *(*leave)(struct tool *tool,
                               GdkEventCrossing *event);
    
    /** Can be used to install a draw hook onto the wavecanvas. */
    struct draw_hook draw_hook;
};

int
tool_init();

void
tool_exit();

void 
tool_destroy(struct tool *tool);

int
tool_register(char *name,
              struct tool *(*ctor)(struct _shell *shl));

void
tool_get_icon(struct tool *tool,
              GdkPixmap **pixmap,
              GdkBitmap **mask);

GtkWidget *
tool_get_panel(struct tool *tool);

void
tool_bind_all(struct _shell *shl);

#endif /* TOOL_H */
