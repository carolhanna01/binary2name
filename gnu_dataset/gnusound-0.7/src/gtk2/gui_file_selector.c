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
#include <gui_file_selector.h>

static void
gui_file_selector_destroy(GtkFileSelection *fs) {
    void (*destroy_handler)(void *user_data) = 
        g_object_get_data(G_OBJECT(fs), "destroy_handler");
    void *user_data = g_object_get_data(G_OBJECT(fs), "user_data");
    DEBUG("in destroy handler\n");
    if(destroy_handler)
        destroy_handler(user_data);
}

/**
 * Creates a file selector widget. Call gui_file_selector_run()
 * to actually show it and get a filename.
 * This provides a common interface for both the 
 * GTK+ >= 2.4 GtkFileChooser widget and the GTK+ 1.2 GtkFileSelection 
 * widget, so that higher level code automatically uses the best 
 * available file selector.
 * 
 * @param title The title for the file selector.
 * @param path The path/filename to open the file selector on.
 * @param type On of GUI_FILE_SELECTOR_OPEN or GUI_FILE_SELECTOR_SAVE.
 * @param select_handler Function to call when the user has selected
 * a file.
 * @param destroy_handler Function to call when the dialog is closed.
 * @param user_data Pointer to user data.
 * @return A file selector widget.
 */

GtkWidget *
gui_file_selector_new(const char *title,
                      const char *path,
                      enum gui_file_selector_type type,
                      void (*select_handler)(const char *fn, 
                                             void *user_data),
                      void (*destroy_handler)(void *user_data),
                      void *user_data) {
    GtkWidget *fc;
    char *dirname, *basename;

    fc = gtk_file_chooser_dialog_new(title,
                                     NULL,
                                     (type == GUI_FILE_SELECTOR_OPEN ? 
                                      GTK_FILE_CHOOSER_ACTION_OPEN :
                                      GTK_FILE_CHOOSER_ACTION_SAVE),
                                     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                     (type == GUI_FILE_SELECTOR_OPEN ? 
                                      GTK_STOCK_OPEN : 
                                      GTK_STOCK_SAVE), GTK_RESPONSE_ACCEPT,
                                     NULL);

    if(path) {
        dirname = g_path_get_dirname(path);
        gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(fc), dirname);
        free(dirname);
    }

    if(type == GUI_FILE_SELECTOR_SAVE) {
        if(path) {
            basename = g_path_get_basename(path);
            gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(fc), basename);
            free(basename);
        }
    }

    g_object_set_data(G_OBJECT(fc), "user_data", user_data);
    g_object_set_data(G_OBJECT(fc), "select_handler", select_handler);
    g_object_set_data(G_OBJECT(fc), "destroy_handler", destroy_handler);
    
    g_signal_connect_object(G_OBJECT (fc),
                            "destroy",
                            G_CALLBACK(gui_file_selector_destroy),
                            (gpointer) fc, 0);

    return fc;
}

/**
 * Shows a file selector previously created using gui_file_selector_new().
 * @param fs The file selector.
 */

void
gui_file_selector_run(GtkWidget *fs) {
    char *filename;
    void *user_data = g_object_get_data(G_OBJECT(fs), "user_data");
    void (*select_handler)(const char *filename, void *user_data) = 
        g_object_get_data(G_OBJECT(fs), "select_handler");
    
    if(gtk_dialog_run(GTK_DIALOG(fs)) == GTK_RESPONSE_ACCEPT) {
        filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fs));
        if(select_handler)
            select_handler(filename, user_data);
        g_free(filename);
    }
    
    gtk_widget_destroy(GTK_WIDGET(fs));
}
