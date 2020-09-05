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

#include <config.h>
#include <glib.h>
#include "mem.h"
#include "draw_hooks.h"

struct draw_hook_data {
    GtkWidget *widget;
    GdkDrawable *drawable;
    GdkRectangle *area;
    GdkGC *gc;
    void *user_data;
};

void
draw_hooks_dispatch_one(gpointer data,
                        gpointer user_data) {
    struct draw_hook *hook = data;
    struct draw_hook_data *dhd = user_data;
    if(hook->draw && !(hook->flags & DRAW_HOOK_DISABLED))
        hook->draw(dhd->widget, dhd->drawable, dhd->gc,
                   dhd->area, dhd->user_data);
    
}

/**
 * Invokes all eligible draw hooks.
 * @param hooks The hook collection.
 * @param widget The widget where the drawing will appear.
 * @param drawable The surface to draw onto.
 * @param gc The graphics context to use.
 * @param area The area of the drawable surface to draw.
 * @param user_data Data to pass on to the draw hooks.
 */

void
draw_hooks_dispatch(struct draw_hooks *hooks,
                    GtkWidget *widget,
                    GdkDrawable *drawable,
                    GdkGC *gc,
                    GdkRectangle *area,
                    void *user_data) {
    struct draw_hook_data dhd;
    dhd.user_data = user_data;
    dhd.widget = widget;
    dhd.drawable = drawable;
    dhd.area = area;
    dhd.gc = gc;
    g_list_foreach(hooks->l, draw_hooks_dispatch_one, &dhd);
}

/**
 * Removes all draw hooks.
 */

void
draw_hooks_remove_all(struct draw_hooks *hooks) {
    GList *l;

    for(l = hooks->l; l; l = l->next) 
        mem_free(l->data);
    
    g_list_free(hooks->l);

    hooks->l = NULL;
}

/**
 * Removes a draw hook.
 */

void
draw_hooks_remove_hook(struct draw_hooks *hooks,
                       const char *name) {
    GList *l;
    
    for(l = hooks->l; l; l = l->next) {
        if(!strcmp(((struct draw_hook *)l->data)->name, name)) {
            hooks->l = g_list_remove_link(hooks->l, l);
            mem_free(l->data);
            g_list_free(l);
            return;
        }
    }
}

void
draw_hooks_set_hook_flags(struct draw_hooks *hooks,
                          const char *name,
                          int flags) {
    GList *l;
    for(l = hooks->l; l; l = l->next) {
        if(!strcmp(((struct draw_hook *)l->data)->name, name)) {
            ((struct draw_hook *)l->data)->flags = flags;
            return;
        }
    }
}

int
draw_hooks_get_hook_flags(struct draw_hooks *hooks,
                          const char *name) {
    GList *l;
    for(l = hooks->l; l; l = l->next) 
        if(!strcmp(((struct draw_hook *)l->data)->name, name)) 
            return ((struct draw_hook *)l->data)->flags;
    return 0;
}

/**
 * Disables a draw hook.
 */

void
draw_hooks_disable_hook(struct draw_hooks *hooks,
                        const char *name) {
    draw_hooks_set_hook_flags(hooks,
                              name, 
                              (draw_hooks_get_hook_flags(hooks, name) | 
                               DRAW_HOOK_DISABLED));
}

/**
 * Enables a draw hook.
 */

void
draw_hooks_enable_hook(struct draw_hooks *hooks,
                       const char *name) {
    draw_hooks_set_hook_flags(hooks,
                              name, 
                              (draw_hooks_get_hook_flags(hooks, name) &
                               ~DRAW_HOOK_DISABLED));
}

void
draw_hooks_disable_all(struct draw_hooks *hooks) {
    GList *l;
    for(l = hooks->l; l; l = l->next) 
        ((struct draw_hook *)l->data)->flags |= DRAW_HOOK_DISABLED;
}

void
draw_hooks_enable_all(struct draw_hooks *hooks) {
    GList *l;
    for(l = hooks->l; l; l = l->next) 
        ((struct draw_hook *)l->data)->flags &= ~DRAW_HOOK_DISABLED;
}

gint
draw_hooks_compare(gconstpointer a,
                   gconstpointer b) {
    return(((struct draw_hook *)a)->z -
           ((struct draw_hook *)b)->z);
}

/**
 * Adds a draw hook. 
 * @param hooks The hooks collection.
 * @param hook The hook to add.
 * @return 0 for success, non-zero otherwise.
 */

int
draw_hooks_add_hook(struct draw_hooks *hooks,
                    const struct draw_hook *new_hook) {
    struct draw_hook *hook = mem_alloc(sizeof *hook);
    if(!hook)
        return 1;
    memcpy(hook, new_hook, sizeof *hook);
    hooks->l = g_list_append(hooks->l, hook);
    hooks->l = g_list_sort(hooks->l, draw_hooks_compare);
    return 0;
}

void
draw_hooks_destroy_one(void *data,
                       void *user_data) {
    mem_free(data);
}

void
draw_hooks_destroy(struct draw_hooks *hooks) {
    g_list_foreach(hooks->l, draw_hooks_destroy_one, NULL);
    g_list_free(hooks->l);
    mem_free(hooks);
}

struct draw_hooks *
draw_hooks_new() {
    struct draw_hooks *hooks = mem_alloc(sizeof *hooks);
    if(!hooks)
        return NULL;
    hooks->l = NULL;
    return hooks;
}

