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

#ifndef PANE_H
#define PANE_H

#include <config.h>
#include <glade/glade.h>
#include <glib.h>

struct pane_signal_bindings {
    char *widget_name;
    char *signal_name;
    void *signal_func;
};

struct pane {
    GladeXML *glade_xml;
    GHashTable *widgets;
};

void
pane_unregister_widget(struct pane *pane,
                       const char *name);

void
pane_register_widget(struct pane *pane,
                     const char *name,
                     GtkWidget *w);

GtkWidget *
pane_find_widget(struct pane *pane,
                 const char *name);

GtkWidget *
pane_get_widget(struct pane *pane,
                const char *name);

void
pane_connect_bindings(struct pane *pane,
                      int n_bindings,
                      const struct pane_signal_bindings bindings[],
                      void *user_data);

struct pane *
pane_new(GladeXML *xml);

void
pane_destroy(struct pane *pane);

#endif /* ! PANE_H */
