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
#include <gtk/gtkcomboboxentry.h>
#include "combo_box.h"

static void combo_box_class_init    (gpointer               g_class,
                                     gpointer               class_data);
static void combo_box_init          (GTypeInstance         *instance,
                                     gpointer               g_class);
static GObject *combo_box_construct (GType                  type,
                                     guint                  n_constr_props,
                                     GObjectConstructParam *constr_props);

static void combo_box_finalize      (GObject *object);

GType
combo_box_get_type (void)
{
  static GType cb_type = 0;
  
  if (!cb_type)
    {
      static const GTypeInfo cb_info =
      {
	sizeof (ComboBoxClass),
	NULL, /* base_init */
        NULL, /* base_finalize */
        combo_box_class_init,
        NULL, /* class_finalize */
	NULL, /* class_data */
        sizeof (ComboBox),
	0,    /* n_preallocs */
	combo_box_init,
      };

      cb_type = g_type_register_static (GTK_TYPE_COMBO_BOX_ENTRY,
                                        "ComboBox", &cb_info, 0);
    }

  return cb_type;
}

static void
combo_box_class_init (gpointer g_class,
                      gpointer class_data)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS(g_class);

    gobject_class->constructor = combo_box_construct;
    gobject_class->finalize = combo_box_finalize;
}

static void
combo_box_init (GTypeInstance *instance,
                gpointer g_class)
{
    ComboBox *combo_box = COMBO_BOX (instance);

    combo_box->private.value = NULL;
}

static GObject *
combo_box_construct (GType                  type,
                     guint                  n_constr_props,
                     GObjectConstructParam *constr_props)
{
    GObject *self;
    ComboBoxClass *klass;
    GObjectClass *parent_class;  

    /* Invoke parent constructor. */

    klass = COMBO_BOX_CLASS (g_type_class_peek (COMBO_BOX_TYPE));
    parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));
    self = parent_class->constructor (type,
                                      n_constr_props,
                                      constr_props);

    /* Finishing touches. */
    /*
    cell = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (self), cell, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (self), cell,
                                    "text", 0,
                                    NULL);
    */

    return self;
}

static void
combo_box_finalize (GObject *object) 
{
    ComboBoxClass *klass;
    GObjectClass *parent_class;  
    ComboBox *combo_box = COMBO_BOX (object);

    if(combo_box->private.value) 
        g_free(combo_box->private.value);

    klass = COMBO_BOX_CLASS (g_type_class_peek (COMBO_BOX_TYPE));
    parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));
    parent_class->finalize (object);

}

GtkWidget*
combo_box_new ()
{
    GtkListStore *model;
    GObject *self;

    /* Set up model. */

    model = gtk_list_store_new (1, G_TYPE_STRING);

    /* Construct object. */

    self = g_object_new (COMBO_BOX_TYPE, 
                         "model", model, 
                         "text_column", 0,
                         NULL);

    /* Release model. */

    g_object_unref (model);

    return GTK_WIDGET (self);
}


const char *
combo_box_get_value (ComboBox *combo_box) {
    int i = gtk_combo_box_get_active (GTK_COMBO_BOX (combo_box));
    char path[255];
    GtkTreeIter iter;
    GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX (combo_box));

    if (i == -1)
        return NULL;

    g_snprintf (path, sizeof (path), "%d", i);

    if (!gtk_tree_model_get_iter_from_string (model, &iter, path))
        return NULL;

    if (combo_box->private.value)
        g_free (combo_box->private.value);

    gtk_tree_model_get (model, &iter, 0, &combo_box->private.value, -1);

    return combo_box->private.value;
}

void
combo_box_set_strings (ComboBox *combo_box,
                       GList *strings) {
    GList *l;
    GtkTreeModel *model;

    model = gtk_combo_box_get_model (GTK_COMBO_BOX (combo_box));
    gtk_list_store_clear (GTK_LIST_STORE (model));
    
    for(l = strings; l; l = l->next)
        gtk_combo_box_append_text(GTK_COMBO_BOX (combo_box), 
                                  (const gchar *) l->data);

}

GList *
combo_box_get_strings (ComboBox *combo_box) {
    gchar *str_data;
    gboolean valid;
    GtkTreeModel *list_store;
    GtkTreeIter iter;
    GList *l = NULL;

    list_store = gtk_combo_box_get_model (GTK_COMBO_BOX (combo_box));
        
    valid = gtk_tree_model_get_iter_first (list_store, &iter);

    while (valid) {

        gtk_tree_model_get (list_store, &iter, 
                            0, &str_data,
                            -1);
        
        l = g_list_append (l, str_data);
        
        valid = gtk_tree_model_iter_next (list_store, &iter);

    }

    return l;
}
                      
void
combo_box_set_active (ComboBox *combo_box,
                      int active) {
    gtk_combo_box_set_active (GTK_COMBO_BOX (combo_box), active);
}

void
combo_box_set_editable (ComboBox *combo_box,
                        gboolean editable) {
    gtk_editable_set_editable (GTK_EDITABLE (GTK_BIN (combo_box)->child), editable);
}


GtkWidget *
combo_box_get_entry (ComboBox *combo_box) {
    return GTK_BIN (combo_box)->child;
}
