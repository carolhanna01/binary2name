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

#ifndef COMBOBOX_H
#define COMBOBOX_H

#include <gdk/gdk.h>
#include <gtk/gtkcombo.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define COMBO_BOX(obj)          GTK_CHECK_CAST (obj, combo_box_get_type (), ComboBox)
#define COMBO_BOX_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, combo_box_get_type (), ComboBoxClass)
#define IS_COMBO_BOX(obj)       GTK_CHECK_TYPE (obj, combo_box_get_type ())


typedef struct _ComboBox       ComboBox;
typedef struct _ComboBoxClass  ComboBoxClass;

struct _ComboBox
{
    GtkCombo combo;
};

struct _ComboBoxClass
{
    GtkComboClass parent_class;

    void (* changed) (ComboBox *combo_box);
};

guint          combo_box_get_type        (void);
GtkWidget *    combo_box_new             ();

const char *   combo_box_get_value       (ComboBox *combo_box);
GList *        combo_box_get_strings     (ComboBox *combo_box);
void           combo_box_set_strings     (ComboBox *combo_box, 
                                          GList *strings);
void           combo_box_set_editable    (ComboBox *combo_box,
                                          gboolean editable);
void           combo_box_set_active      (ComboBox *combo_box,
                                          int index);
GtkWidget *    combo_box_get_entry       (ComboBox *combo_box);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* COMBO_BOX_H */
