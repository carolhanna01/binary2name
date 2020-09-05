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

#ifndef DRAW_HOOKS_H
#define DRAW_HOOKS_H

#include <config.h>
#include <gtk/gtk.h>

#define DRAW_HOOK_DISABLED (1 << 0)

struct draw_hook {
    /** The name of this draw hook. */
    const char *name;
    /** Flags for this draw hook. */
    int flags;
    /** The z-index of this draw hook respective to other draw hooks. */
    int z;
    /** The function which does the drawing. */
    void (*draw)(GtkWidget *widget,
                 GdkDrawable *drawable, 
                 GdkGC *gc,
                 GdkRectangle *area,
                 void *user_data);
};

/**
 * A collection of draw hooks.
 */

struct draw_hooks {
    GList *l;
};

void
draw_hooks_dispatch(struct draw_hooks *hooks,
                    GtkWidget *widget,
                    GdkDrawable *drawable,
                    GdkGC *gc,
                    GdkRectangle *area,
                    void *user_data);

void
draw_hooks_remove_all(struct draw_hooks *hooks);

void
draw_hooks_remove_hook(struct draw_hooks *hooks,
                       const char *name);

void
draw_hooks_disable_hook(struct draw_hooks *hooks,
                        const char *name);

void
draw_hooks_enable_hook(struct draw_hooks *hooks,
                       const char *name);

void
draw_hooks_disable_all(struct draw_hooks *hooks);

void
draw_hooks_enable_all(struct draw_hooks *hooks);

int
draw_hooks_add_hook(struct draw_hooks *hooks,
                    const struct draw_hook *hook);

void
draw_hooks_destroy(struct draw_hooks *hooks);

struct draw_hooks *
draw_hooks_new();

#endif /* ! DRAW_HOOKS_H */
