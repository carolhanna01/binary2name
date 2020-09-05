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

#include <gtk/gtk.h>
#include "combo_box.h"

enum {
  CHANGED,
  LAST_SIGNAL
};

static void combo_box_class_init          (ComboBoxClass *klass);
static void combo_box_init                (ComboBox *combo_box);

static gint combo_box_signals[LAST_SIGNAL] = { 0 };

guint
combo_box_get_type ()
{
  static guint cb_type = 0;

  if (!cb_type)
    {
      GtkTypeInfo cb_info =
      {
	"ComboBox",
	sizeof (ComboBox),
	sizeof (ComboBoxClass),
	(GtkClassInitFunc) combo_box_class_init,
	(GtkObjectInitFunc) combo_box_init,
	(GtkArgSetFunc) NULL,
	(GtkArgGetFunc) NULL,
      };

      cb_type = gtk_type_unique (gtk_combo_get_type (), &cb_info);
    }

  return cb_type;
}

static void
combo_box_class_init (ComboBoxClass *class)
{
  GtkObjectClass *object_class;

  object_class = (GtkObjectClass*) class;
  
  combo_box_signals[CHANGED] = 
      gtk_signal_new ("changed",
                      GTK_RUN_FIRST,
                      object_class->type,
                      GTK_SIGNAL_OFFSET (ComboBoxClass, changed),
                      gtk_signal_default_marshaller, GTK_TYPE_NONE, 0);

  gtk_object_class_add_signals (object_class, combo_box_signals, LAST_SIGNAL);

  class->changed = NULL;
}

static void
combo_box_entry_changed(GtkWidget *w,
                        void *user_data) {
    
    GtkObject *source = user_data;

    gtk_signal_emit_by_name(source, "changed");
}

static void
combo_box_init (ComboBox *combo_box)
{
    gtk_signal_connect(GTK_OBJECT(GTK_COMBO(combo_box)->entry),
                       "changed",
                       GTK_SIGNAL_FUNC(combo_box_entry_changed),
                       combo_box);
}

const char *
combo_box_get_value(ComboBox *combo_box) {
    return gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(combo_box)->entry));
}

void
combo_box_set_strings(ComboBox *combo_box,
                      GList *strings) {
    GList *children;

    if(strings == NULL) {

        /* gtk_combo_set_popdown_strings() complains for empty lists, 
           so just remove all elements by hand. */
    
        children = gtk_container_children(GTK_CONTAINER(GTK_COMBO(combo_box)->list));
        for(; children; children = children->next) 
            gtk_container_remove(GTK_CONTAINER(GTK_COMBO(combo_box)->list),
                                 children->data);
        return;
    }
    
    gtk_combo_set_popdown_strings(GTK_COMBO(combo_box), strings);
}

GList *
combo_box_get_strings(ComboBox *combo_box) {
    char *s;
    GtkLabel *label;
    GList *children = gtk_container_children(GTK_CONTAINER(GTK_COMBO(combo_box)->list));
    GList *l = NULL;

    for(; children; children = children->next) {

        label = GTK_LABEL(GTK_BIN(children->data)->child);
        gtk_label_get(label, (char **)&s);

        l = g_list_append(l, g_strdup(s));
    }

    g_list_free(children);

    return l;
}

void
combo_box_set_active(ComboBox *combo_box,
                     int active) {
    gtk_list_select_item(GTK_LIST(GTK_COMBO(combo_box)->list), active);
}

void
combo_box_set_editable(ComboBox *combo_box,
                       gboolean editable) {
    gtk_entry_set_editable(GTK_ENTRY(GTK_COMBO(combo_box)->entry), FALSE);
}

GtkWidget *
combo_box_get_entry(ComboBox *combo_box) {
    return GTK_COMBO(combo_box)->entry;
}

GtkWidget *
combo_box_new () {
    GtkWidget *w = gtk_type_new (combo_box_get_type ());

    gtk_combo_disable_activate (GTK_COMBO (w));

    return w;
}
