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


/**
 * @file
 * A pane is a widget collection.
 */

#include <config.h>
#include <gnome.h>
#include <glade/glade.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <string.h>
#include "gui.h"
#include "mem.h"
#include "pane.h"

void
pane_unregister_widget(struct pane *pane,
                       const char *name) {
    gpointer key, value;

    if(!g_hash_table_lookup_extended(pane->widgets, name, &key, &value)) {
        FAIL("cannot unregister widget %s because it does not exist\n", name);
        abort();
    }
    g_hash_table_remove(pane->widgets, name);
    free(key);
}

void
pane_register_widget(struct pane *pane,
                     const char *name,
                     GtkWidget *w) {
    char *copy = strdup(name);
    GtkWidget *w2;
    if(!copy) {
        FAIL("cannot strdup %s\n", name);
        abort();
    }
    w2 = g_hash_table_lookup(pane->widgets, name);
    if(w2) {
        FAIL("already registered %s\n", name);
        abort();
    }
    g_hash_table_insert(pane->widgets, copy, w);
}


/**
 * Like pane_get_widget() but doesn't abort() if the widget cannot be
 * found.
 */

GtkWidget *
pane_find_widget(struct pane *pane,
                 const char *name) {
    GtkWidget *w = g_hash_table_lookup(pane->widgets, name);

    if(!w) {
        w = glade_xml_get_widget(pane->glade_xml, name);
        
        if(!w) 
            return NULL;

        // FIXME: unchecked strdup
        g_hash_table_insert(pane->widgets, strdup(name), w);
    }

    return w;
}

GtkWidget *
pane_get_widget(struct pane *pane,
                const char *name) {
    GtkWidget *w = g_hash_table_lookup(pane->widgets, name);
    if(!w) {
        w = glade_xml_get_widget(pane->glade_xml, name);
        if(!w) {
            FAIL("could not find widget %s\n", name);
            abort();
        }
        // FIXME: unchecked strdup
        g_hash_table_insert(pane->widgets, strdup(name), w);
    }

    return w;
}

void
pane_connect_bindings(struct pane *pane,
                      int n_bindings,
                      const struct pane_signal_bindings bindings[],
                      void *user_data) {
    int i;
    
    for(i = 0; i < n_bindings; i++) {
        //        DEBUG("widget name: %s, signal name: %s\n",
        //              bindings[i].widget_name, bindings[i].signal_name);
        g_signal_connect(G_OBJECT(pane_get_widget(pane,
                                                  bindings[i].widget_name)), 
                         bindings[i].signal_name,
                         bindings[i].signal_func, 
                         user_data);
    }
}

static void
pane_unref_widgets(void *key,
                   void *value,
                   void *user_data) {
    free(key);
    //    gtk_widget_unref(GTK_WIDGET(value));
}

void
pane_destroy(struct pane *pane) {
    g_hash_table_foreach(pane->widgets, pane_unref_widgets, NULL);
    g_hash_table_destroy(pane->widgets);
    g_object_unref(G_OBJECT(pane->glade_xml));
    mem_free(pane);
}

struct pane *
pane_new(GladeXML *xml) {
    struct pane *pane = mem_alloc(sizeof *pane);
    if(!pane)
        return NULL;

    pane->glade_xml = xml;
    pane->widgets = g_hash_table_new(g_str_hash, g_str_equal);
    return pane;
}
