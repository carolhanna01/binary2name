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

#include <config.h>
#include <gnome.h>
#include "gui.h"
#include "shell.h"
#include "pane.h"
#include "file.h"
#include "arbiter.h"
#include "dialog.h"
#include "dialog_mixdown.h"
#include "dialog_format_mixdown.h"

void
dialog_mixdown_close(struct dialog *dialog,
                     void *user_data) {
    dialog_destroy(dialog);
}

void
dialog_mixdown_apply(struct dialog *dialog,
                     void *user_data) {
    struct dialog_mixdown *dm = user_data;
    GList *l;
    track_map_t map = 0;
    const char *s;

    /* 
     * Construct a track map specifying which output channels to
     * mixdown.
     */
    
    for(l = gtk_container_get_children(GTK_CONTAINER(pane_get_widget(dialog->pane, "mixdown_output_channels_vbox"))); l; l = l->next) {

        s = gtk_label_get_text(GTK_LABEL(GTK_BIN(l->data)->child));
        
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(l->data))) 
            map |= (1 << (atoi(s) - 1));
    }
    
    dialog = dialog_format_mixdown_new(dialog->shl, dm->file, map);
    dialog_open(dialog);
}

void
dialog_mixdown_open(struct dialog *dialog,
                    void *user_data) {
    shell *shl = dialog->shl;
    struct file *file = ((struct dialog_mixdown *)dialog)->file;
    int i;
    GtkWidget *checkbutton;
    char label[50];
    char info[4096];
    GList *l;

    snprintf(info, sizeof info, "Mixdown %s\nto %s", 
             shl->file->name, file->name);
    gtk_label_set_text(GTK_LABEL(pane_get_widget(dialog->pane, "mixdown_info_label")), info);
    
    for(l = gtk_container_get_children(GTK_CONTAINER(pane_get_widget(dialog->pane, "mixdown_output_channels_vbox"))); l; l = l->next)
        gtk_container_remove(GTK_CONTAINER(pane_get_widget(dialog->pane, "mixdown_output_channels_vbox")), GTK_WIDGET(l->data));
    
    for(i = 0; i < shl->clip->mixer->output_channels; i++) {
        snprintf(label, sizeof label, "%d", i + 1);
        checkbutton = gtk_check_button_new_with_label(label);
        gtk_widget_show(checkbutton);
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton), TRUE);
        gtk_box_pack_start(GTK_BOX(pane_get_widget(dialog->pane, "mixdown_output_channels_vbox")), checkbutton, FALSE, FALSE, 0);
    }
}

void
dialog_mixdown_destroy(struct dialog *dialog,
                       void *user_data) {
    struct dialog_mixdown *dm = user_data;
    if(dm->file)
        file_destroy(dm->file);
}

struct dialog *
dialog_mixdown_new(shell *shl,
                   struct file *file) {
    struct dialog_mixdown *dm = mem_alloc(sizeof *dm);
    if(!dm)
        return NULL;
    
    dialog_init((struct dialog *)dm, 
                GLADE_FILE,
                "mixdowndialog", 
                shl,
                0, 
                NULL, 
                dm);

    ((struct dialog *)dm)->open = dialog_mixdown_open;
    ((struct dialog *)dm)->apply = dialog_mixdown_apply;
    ((struct dialog *)dm)->close = dialog_mixdown_close;
    ((struct dialog *)dm)->dtor = dialog_mixdown_destroy;
    
    dm->file = file;

    return (struct dialog *)dm;
}
