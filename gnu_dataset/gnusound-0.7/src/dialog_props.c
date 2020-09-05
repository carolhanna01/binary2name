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
#include <gnome.h>
#include "shell.h"
#include "pane.h"
#include "resample.h"
#include "gui.h"
#include "arbiter.h"
#include "dialog_props.h"

void
dialog_props_set_size_labels(struct dialog_props *dp) {
    char s[50];
    size_t current_size, new_size;
    struct dialog *dialog = (struct dialog *)dp;
    int resample_toggle = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "properties_resample")));
    shell *shl = dialog->shl;

    current_size = shl->clip->sr->channels * 
        snd_frame_count(shl->clip->sr, MAP_ALL) *
        sample_get_width(shl->clip->sr->sample_type);
    new_size = ceil(shl->clip->sr->channels * 
                    snd_frame_count(shl->clip->sr, MAP_ALL) *
                    sample_get_width(dp->sample_type) *
                    (resample_toggle ?
                     (dp->sample_rate / shl->clip->sr->rate) :
                     1));
    snprintf(s, 50, "%d", current_size);
    gtk_label_set_text(GTK_LABEL(pane_get_widget(dialog->pane, "properties_current_size")), s);
    snprintf(s, 50, "%d", new_size);
    gtk_label_set_text(GTK_LABEL(pane_get_widget(dialog->pane, "properties_new_size")), s);
}

void
dialog_props_sample_rate_changed(GtkSpinButton *w,
                                 struct dialog_props *dp) {
    DEBUG("sample rate\n");
    dp->sample_rate = gtk_spin_button_get_value(w);
    dialog_props_set_size_labels(dp);
}

void
dialog_props_sample_type_int_8_clicked(GtkToggleButton *w,
                                       struct dialog_props *dp) {
    DEBUG("sample width 8\n");
    if(gtk_toggle_button_get_active(w)) 
        dp->sample_type = SAMPLE_TYPE_INT_8;
    dialog_props_set_size_labels(dp);
}

void
dialog_props_sample_type_int_16_clicked(GtkToggleButton *w,
                                        struct dialog_props *dp) {
    DEBUG("sample width 16\n");
    if(gtk_toggle_button_get_active(w)) 
        dp->sample_type = SAMPLE_TYPE_INT_16;
    dialog_props_set_size_labels(dp);
}

void
dialog_props_sample_type_int_32_clicked(GtkToggleButton *w,
                                        struct dialog_props *dp) {
    DEBUG("sample width 32\n");
    if(gtk_toggle_button_get_active(w)) 
        dp->sample_type = SAMPLE_TYPE_INT_32;
    dialog_props_set_size_labels(dp);
}

void
dialog_props_sample_type_float_32_clicked(GtkToggleButton *w,
                                          struct dialog_props *dp) {
    DEBUG("sample width float 32\n");
    if(gtk_toggle_button_get_active(w)) 
        dp->sample_type = SAMPLE_TYPE_FLOAT_32;
    dialog_props_set_size_labels(dp);
}

void
dialog_props_resample_toggled(GtkWidget *w,
                              struct dialog_props *dp) {
    DEBUG("resample\n");
    dialog_props_set_size_labels(dp);
}

void
dialog_props_open(struct dialog *dialog,
                  void *user_data) {
    struct dialog_props *dp = user_data;
    shell *shl = dialog->shl;

    dialog_props_set_size_labels(dp);

    dp->sample_rate = shl->clip->sr->rate;
    dp->sample_type = shl->clip->sr->sample_type;
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "properties_sample_rate")), shl->clip->sr->rate);
    switch(dp->sample_type) {
    case SAMPLE_TYPE_INT_8:
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "properties_sample_type_int_8")), TRUE);
        break;
    case SAMPLE_TYPE_INT_16:
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "properties_sample_type_int_16")), TRUE);
        break;
    case SAMPLE_TYPE_INT_32:
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "properties_sample_type_int_32")), TRUE);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "properties_sample_type_float_32")), TRUE);
        break;
    }
}

void
dialog_props_apply(struct dialog *dialog,
                   void *user_data) {
    struct dialog_props *dp = user_data;
    arbiter_queue_cmd(CMD_NEW("convert-snd",
                              cmd_new_shellp_val(dialog->shl),
                              cmd_new_double_val(dp->sample_rate),
                              cmd_new_int_val(dp->sample_type),
                              cmd_new_int_val(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "properties_resample"))))));
}

struct dialog *
dialog_props_new(shell *shl) {
    struct dialog_props *dp = mem_alloc(sizeof *dp);
    static struct pane_signal_bindings bindings[] = {
        { "properties_resample", "toggled", 
          dialog_props_resample_toggled },
        { "properties_sample_rate", "changed",
          dialog_props_sample_rate_changed },
        { "properties_sample_type_int_8", "clicked", 
          dialog_props_sample_type_int_8_clicked },
        { "properties_sample_type_int_16", "clicked", 
          dialog_props_sample_type_int_16_clicked },
        { "properties_sample_type_int_32", "clicked", 
          dialog_props_sample_type_int_32_clicked },
        { "properties_sample_type_float_32", "clicked", 
          dialog_props_sample_type_float_32_clicked },
    };

    if(!dp)
        return NULL;

    dialog_init((struct dialog *)dp, 
                GLADE_FILE, 
                "propertiesdialog", 
                shl, 
                sizeof(bindings) / sizeof(bindings[0]), 
                bindings, 
                dp);
    
    ((struct dialog *)dp)->open = dialog_props_open;
    ((struct dialog *)dp)->apply = dialog_props_apply;
    
    dp->sample_rate = shl->clip->sr->rate;
    dp->sample_type = shl->clip->sr->sample_type;
    
    return (struct dialog *)dp;
}

