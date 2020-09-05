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
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <math.h>
#include "lib/misc.h"
#include <combo_box.h>
#include "pref.h"
#include "gui.h"
#include "player.h"
#include "dialog_pref.h"
#include "pane.h"
#include "arbiter.h"

static const char *current_audio_driver;
static GtkWidget *current_audio_driver_widget;

/*
 * Preferences dialog.
 */

static void
dialog_pref_commit_audio_driver_config(struct dialog *dialog) {
    const struct player_driver_container *pdc;
    DEBUG("current audio driver: %s\n", current_audio_driver);
    if(current_audio_driver) {
        pdc = player_get_driver_by_name(current_audio_driver);
        if(pdc) 
            pdc->pd->commit_config();
    }
}

static void
dialog_pref_close_audio_driver_config(struct dialog *dialog) {
    const struct player_driver_container *pdc;
    
    if(current_audio_driver_widget) {
        gtk_container_remove(GTK_CONTAINER(pane_get_widget(dialog->pane, "preferences_audio_drivers_container")), current_audio_driver_widget);
        current_audio_driver_widget = NULL;
    }
    
    if(current_audio_driver) {
        pdc = player_get_driver_by_name(current_audio_driver);
        if(pdc) 
            pdc->pd->close_config();
        current_audio_driver = NULL;
    }
}

static void
dialog_pref_open_audio_driver_config(struct dialog *dialog,
                                     const char *driver) {
    const struct player_driver_container *pdc;
    GtkWidget *w;
    
    dialog_pref_close_audio_driver_config(dialog);
    
    pdc = player_get_driver_by_name(driver);
    if(!pdc) {
        gui_alert("Cannot show configuration for unknown audio driver %s, "
                  "showing configuration for currently active driver %s "
                  "instead",
                  driver, player_get_current_driver()->pd->name);
        pdc = player_get_current_driver();
    }

    combo_box_set_active(COMBO_BOX(pane_get_widget(dialog->pane, "preferences_audio_drivers_menu")), pdc->num);

    w = pdc->pd->open_config();
    if(!w) 
        w = gtk_label_new("No options.");
    
    gtk_table_attach(GTK_TABLE(pane_get_widget(dialog->pane, "preferences_audio_drivers_container")),
                     w,
                     0, 2, 1, 2,
                     GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
                     0, 0);

    /* FIXME: need to somehow reflow the dialog */

    gtk_widget_hide(pane_get_widget(dialog->pane, "preferences_audio_drivers_vbox"));
    gtk_widget_show(pane_get_widget(dialog->pane, "preferences_audio_drivers_vbox"));
    current_audio_driver_widget = w;
    current_audio_driver = pdc->pd->name;
}

void
dialog_pref_close(struct dialog *dialog,
                  void *user_data) {
    dialog_pref_close_audio_driver_config(dialog);
}

#define STORE_FLOAT(what) \
    pref_set_float(what, gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "preferences_" what))))
#define STORE_INT(what) \
    pref_set_int(what, gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "preferences_" what))))
#define STORE_TOGGLE(what) \
    pref_set_int(what, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "preferences_" what))))

#define STORE_COLOR(what) \
    gtk_color_button_get_color(GTK_COLOR_BUTTON(pane_get_widget(dialog->pane, "preferences_colors_" what)), &color); \
    gui_color_to_hex_string(color_hex_spec, &color); \
    pref_set_string("colors." what, color_hex_spec); 


void 
pref_redraw_shell(shell *shl,
                  void *user_data) {
    view_redraw(shl->view);
}

void
dialog_pref_apply(struct dialog *dialog,
                  void *user_data) {
    GdkColor color;
    char color_hex_spec[8];

    /* Edit */

    STORE_INT("max_tracks");
    STORE_INT("max_undo_depth");

    /* View */

    STORE_TOGGLE("restore_window_positions");
    STORE_TOGGLE("restore_scrollbar_positions");

    STORE_TOGGLE("view.wave.draw.rms.enable");

    STORE_COLOR("background");
    STORE_COLOR("wave.peaks");
    STORE_COLOR("wave.rms");
    STORE_COLOR("selection");
    STORE_COLOR("wave.peaks.selected");
    STORE_COLOR("wave.rms.selected");

    /* Playback/record */

    STORE_INT("playback_display_interval");
    STORE_FLOAT("audio_chunk_duration");

    /* File */

    STORE_FLOAT("default_sample_rate");

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "preferences_default_sample_type_int_8"))))
        pref_set_int("default_sample_type", SAMPLE_TYPE_INT_8);
    else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "preferences_default_sample_type_int_16"))))
        pref_set_int("default_sample_type", SAMPLE_TYPE_INT_16);
    else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "preferences_default_sample_type_int_32"))))
        pref_set_int("default_sample_type", SAMPLE_TYPE_INT_32);
    else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "preferences_default_sample_type_float_32"))))
        pref_set_int("default_sample_type", SAMPLE_TYPE_FLOAT_32);
    
    pref_set_string("audio_driver", combo_box_get_value(COMBO_BOX(pane_get_widget(dialog->pane, "preferences_audio_drivers_menu"))));

    gui_alloc_colors();

    dialog_pref_commit_audio_driver_config(dialog);
    dialog_pref_close_audio_driver_config(dialog);

    pref_sync();

    arbiter_foreach_shell(pref_redraw_shell, NULL);
}

void
dialog_pref_audio_driver_activate(GtkWidget *w,
                                  void *user_data) {
    struct dialog *dialog = user_data;
    
    DEBUG("selected driver %s\n", combo_box_get_value(COMBO_BOX(w)));
    dialog_pref_open_audio_driver_config(dialog, 
                                         combo_box_get_value(COMBO_BOX(w)));
    //    dialog_pref_select_audio_driver(driver);
}

void
dialog_pref_setup_audio_drivers_menu(struct dialog *dialog) {
    int i;
    GtkWidget *combo_box;
    GList *l = NULL;

    combo_box = combo_box_new();

    for(i = 0; i < player_get_driver_count(); i++) 
        l = g_list_append(l, (void *)player_get_driver(i)->pd->name);

    combo_box_set_strings(COMBO_BOX(combo_box), l);
    combo_box_set_editable(COMBO_BOX(combo_box), FALSE);
    gtk_widget_show(GTK_WIDGET(combo_box));

    pane_register_widget(dialog->pane,
                         "preferences_audio_drivers_menu",
                         combo_box);

    g_signal_connect(G_OBJECT(combo_box), 
                     "changed",
                     G_CALLBACK(dialog_pref_audio_driver_activate), 
                     dialog);
    
    gtk_table_attach(GTK_TABLE(pane_get_widget(dialog->pane, "preferences_audio_drivers_container")),
                     combo_box,
                     1, 2, 0, 1,
                     GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
                     0, 0);

    g_list_free(l);
}

#define LOAD_INT(what) \
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "preferences_" what)), pref_get_as_int(what))
#define LOAD_FLOAT(what) \
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "preferences_" what)), pref_get_as_float(what))
#define LOAD_TOGGLE(what) \
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "preferences_" what)), pref_get_as_int(what));

#define LOAD_COLOR(what) \
    gtk_color_button_set_color(GTK_COLOR_BUTTON(pane_get_widget(dialog->pane, "preferences_colors_" what)), gui_get_color(what))

void
dialog_pref_populate(struct dialog *dialog) {
    char *sample_type_toggle = NULL;

    /* Edit */

    LOAD_INT("max_undo_depth");
    LOAD_INT("max_tracks");

    /* View */

    LOAD_TOGGLE("restore_window_positions");
    LOAD_TOGGLE("restore_scrollbar_positions");

    LOAD_TOGGLE("view.wave.draw.rms.enable");

    LOAD_COLOR("background");
    LOAD_COLOR("wave.peaks");
    LOAD_COLOR("wave.rms");
    LOAD_COLOR("selection");
    LOAD_COLOR("wave.peaks.selected");
    LOAD_COLOR("wave.rms.selected");

    /* Playback */

    LOAD_INT("playback_display_interval");
    LOAD_FLOAT("audio_chunk_duration");

    /* File */

    LOAD_FLOAT("default_sample_rate");
    if(pref_get_as_int("default_sample_type") == SAMPLE_TYPE_INT_8)
        sample_type_toggle = "preferences_default_sample_type_int_8";
    else if(pref_get_as_int("default_sample_type") == SAMPLE_TYPE_INT_16)
        sample_type_toggle = "preferences_default_sample_type_int_16";
    else if(pref_get_as_int("default_sample_type") == SAMPLE_TYPE_INT_32)
        sample_type_toggle = "preferences_default_sample_type_int_32";
    else if(pref_get_as_int("default_sample_type") == SAMPLE_TYPE_FLOAT_32)
        sample_type_toggle = "preferences_default_sample_type_float";
    
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, sample_type_toggle)), TRUE);
                                 
}

void
dialog_pref_open(struct dialog *dialog,
                 void *user_data) {
    dialog_pref_populate(dialog);
    dialog_pref_open_audio_driver_config(dialog,
                                         pref_get_as_string("audio_driver"));
}

struct dialog *
dialog_pref_new() {
    struct dialog_pref *dp = 
        (struct dialog_pref *)dialog_new(sizeof(*dp),
                                         GLADE_FILE, 
                                         "preferencesdialog", 
                                         NULL, 
                                         0,
                                         NULL, 
                                         NULL);

    g_return_val_if_fail(dp != NULL, NULL);

    ((struct dialog *)dp)->open = dialog_pref_open;
    ((struct dialog *)dp)->apply = dialog_pref_apply;
    ((struct dialog *)dp)->close = dialog_pref_close;
    ((struct dialog *)dp)->user_data = dp;

    dialog_pref_setup_audio_drivers_menu((struct dialog *)dp);
    
    return (struct dialog *)dp;
}
