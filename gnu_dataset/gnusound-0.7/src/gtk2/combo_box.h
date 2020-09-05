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

#ifndef COMBO_BOX_H
#define COMBO_BOX_H

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtkcomboboxentry.h>

G_BEGIN_DECLS

#define COMBO_BOX_TYPE            (combo_box_get_type ())
#define COMBO_BOX(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), COMBO_BOX_TYPE, ComboBox))
#define COMBO_BOX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), COMBO_BOX_TYPE, ComboBoxClass))
#define IS_COMBO_BOX(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), COMBO_BOX_TYPE))
#define IS_COMBO_BOX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), COMBO_BOX_TYPE))


typedef struct _ComboBox        ComboBox;
typedef struct _ComboBoxClass   ComboBoxClass;
typedef struct _ComboBoxPrivate ComboBoxPrivate;

struct _ComboBoxPrivate 
{
    gchar *value;
};

struct _ComboBox
{
    GtkComboBoxEntry combo;

    ComboBoxPrivate private;
};

struct _ComboBoxClass
{
    GtkComboBoxEntryClass parent_class;

    void (* changed) (ComboBox *combo_box);
};

GType          combo_box_get_type        (void);
GtkWidget *    combo_box_new             (void);
const char *   combo_box_get_value       (ComboBox *combo_box);
GList *        combo_box_get_strings     (ComboBox *combo_box);
void           combo_box_set_strings     (ComboBox *combo_box, 
                                          GList *strings);
void           combo_box_set_active      (ComboBox *combo_box,
                                          int index);
void           combo_box_set_editable    (ComboBox *combo_box,
                                          gboolean editable);
GtkWidget *    combo_box_get_entry       (ComboBox *combo_box);

G_END_DECLS

#endif /* COMBO_BOX_H */
