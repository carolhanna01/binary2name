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
#include <combo_box.h>
#include "lib/misc.h"
#include "gui.h"
#include "shell.h"
#include "pane.h"
#include "arbiter.h"
#include "dialog_format.h"

static int dialog_format_inited = 0;

/**
 * List of format id strings. A format id is simply the driver name
 * appended by ": " and the format name.  
 */
static GList *dialog_format_format_ids = NULL;

/** 
 * Table which maps format id to format struct.
 */
static GHashTable *dialog_format_formats = NULL;

static void
dialog_format_browse_selected(const char *filename,
                              void *user_data) {
    struct dialog *dialog = user_data;
    GtkWidget *w = pane_get_widget(dialog->pane, "save_filename");

    gtk_entry_set_text(GTK_ENTRY(w), filename);
}

static void
dialog_format_selector_destroyed(GtkWidget *selector,
                                 void *user_data) {
    GtkWidget *root = user_data;
    g_signal_handlers_disconnect_by_func(G_OBJECT(root), 
                                         gtk_widget_destroy,
                                         selector);
}

static void
dialog_format_browse_clicked(GtkWidget *widget,
                             struct dialog *dialog) {
    GtkWidget *selector, *root;
    
    root = pane_get_widget(dialog->pane, dialog->root);
    
    selector = 
        gui_file_selector_new("Select file to save as",
                              ((struct dialog_format *)dialog)->file->name,
                              GUI_FILE_SELECTOR_SAVE,
                              dialog_format_browse_selected,
                              NULL,
                              dialog);

    g_signal_connect_swapped(G_OBJECT(root),
                             "destroy",
                             G_CALLBACK(gtk_widget_destroy),
                             G_OBJECT(selector));

    g_signal_connect(G_OBJECT(selector),
                     "destroy",
                     G_CALLBACK(dialog_format_selector_destroyed),
                     root);

    gui_file_selector_run(selector);
}

void
dialog_format_select_format(struct dialog_format *df,
                            const char *driver,
                            const char *format) {
    struct dialog *dialog = (struct dialog *)df;
    struct file_driver *fd = file_find_driver(driver);
    struct cmd_value *r;
    GtkWidget *options = NULL;
    GtkWidget *container;

    if(!fd) {
        FAIL("could not find driver for %s\n", driver);
        return;
    }

    container = pane_get_widget(dialog->pane, "save_options");
    
    if(GTK_BIN(container)->child) {
        if(df->fd && df->options_widget) 
            df->fd->close_file_config(df->file);
        if(df->file)
            fd->detach(df->file);

        gtk_container_remove(GTK_CONTAINER(container), 
                             GTK_BIN(container)->child);
    }
    
    r = fd->attach(df->file, format);
    if(cmd_is_error(r)) {
        df->fd = NULL;
        FAIL("could not attach %s\n", df->filename);
        options = gtk_label_new("Error selecting driver");
    } else {
        df->fd = fd;
        if(fd->open_file_config) 
            options = fd->open_file_config(df->file, format);
        
        if(!options) {
            options = gtk_label_new("No options.");
            df->options_widget = NULL;
        } else {
            df->options_widget = options;
        }
    }
    cmd_destroy_value(r);

    gtk_widget_show(options);
        
    gtk_container_add(GTK_CONTAINER(container), options);
        
}

void 
dialog_format_select_format_by_id(struct dialog_format *df,
                                  const char *format_id) {
    char *format, *driver, *s;

    s = strdup(format_id);
    if(!s)
        return;

    format = strchr(s, ':');
    if(!format) {
        free(s);
        return;
    }

    *format = '\0';
    format += 2;
    driver = s;

    dialog_format_select_format(df, driver, format);

    free(s);
}

int
dialog_format_select_format_by_filename(struct dialog_format *df,
                                        const char *filename) {
    int i = 0;
    char *fid;
    const char *dot;
    struct file_format *ff;
    GList *l;

    dot = find_extension(filename);
    
    if(!dot)
        return 1;

    dot++;

    /*
     * Find the format associated with this extension and select
     * it. 
     */

    for(l = dialog_format_format_ids; l; l = l->next) {
        fid = l->data;
        ff = g_hash_table_lookup(dialog_format_formats, fid);
        if(!strcasecmp(dot, ff->extension)) {
            dialog_format_select_format_by_id(df, fid);
            combo_box_set_active(COMBO_BOX(pane_get_widget(((struct dialog *)df)->pane, "save_formats")), i);
            return 0;
        }
        i++;
    }

    return 1;
}

void 
dialog_format_replace_filename_extension(struct dialog_format *df,
                                         const char *extension) {
    const char *fn = gtk_entry_get_text(GTK_ENTRY(pane_get_widget(((struct dialog *)df)->pane, "save_filename")));
    char tmp[4096], new_fn[4096];
    char *dot;

    strcpy(tmp, fn);
    dot = strrchr(tmp, '.');
    if(dot) 
        *dot = '\0';
    snprintf(new_fn, sizeof new_fn, "%s.%s", tmp, extension);
    gtk_entry_set_text(GTK_ENTRY(pane_get_widget(((struct dialog *)df)->pane, "save_filename")),
                       new_fn);
}

void
dialog_format_close(struct dialog *dialog,
                    void *user_data) {
    struct dialog_format *df = user_data;
    if(df->fd && df->options_widget) 
        df->fd->close_file_config(df->file);
    dialog_destroy(dialog);
}

void
dialog_format_apply(struct dialog *dialog,
                    void *user_data) {
    struct dialog_format *df = user_data;
    const char *filename = gtk_entry_get_text(GTK_ENTRY(pane_get_widget(dialog->pane, "save_filename")));

    DEBUG("filename: %s\n", filename);

    file_set_name(df->file, filename);

    if(df->fd && df->options_widget) 
        df->fd->commit_file_config(df->file);

    DEBUG("filename: %s, file: %p, driver: %p\n",
          df->file->name, df->file, df->file->driver);
}

void 
dialog_format_format_changed(GtkWidget *w,
                             void *user_data) {
    struct dialog_format *df = user_data;
    const char *id = combo_box_get_value(COMBO_BOX(w));
    struct file_format *ff = g_hash_table_lookup(dialog_format_formats, id);
    
    if(ff) 
        dialog_format_replace_filename_extension(df, ff->extension);

    dialog_format_select_format_by_id(df, id);
}

/*
 * Construction/destruction.
 */

void
dialog_format_destroy(struct dialog *dialog,
                      void *user_data) {
    file_destroy(((struct dialog_format *)dialog)->file);
}

GtkWidget *
dialog_format_new_formats_list(struct dialog_format *df) {
    GtkWidget *formats_list = combo_box_new();
    combo_box_set_strings(COMBO_BOX(formats_list), dialog_format_format_ids);
    g_signal_connect(G_OBJECT(formats_list), 
                     "changed",
                     G_CALLBACK(dialog_format_format_changed), 
                     df);
    combo_box_set_editable(COMBO_BOX(formats_list), FALSE);
    gtk_widget_show(formats_list);
    return formats_list;
}

static void
dialog_format_get_formats_for_driver(struct file_driver *fd,
                                     void *user_data) {
    int i;
    char tmp[128], *s;
    const struct file_format *formats;
    
    DEBUG("driver: %s\n", fd->name);

    if(!fd->get_write_formats)
        return;

    formats = fd->get_write_formats();
    if(!formats)
        return;

    for(i = 0; formats[i].name; i++) {
        snprintf(tmp, sizeof tmp, "%s: %s", fd->name, formats[i].name);
        s = strdup(tmp);
        if(!s) 
            return;

        dialog_format_format_ids = g_list_append(dialog_format_format_ids, s);
        g_hash_table_insert(dialog_format_formats, s, 
                            (struct file_format *)&formats[i]);
    }
}

void
dialog_format_get_formats() {
    dialog_format_formats = g_hash_table_new(g_str_hash, g_str_equal);

    file_foreach_driver(dialog_format_get_formats_for_driver, NULL);
}

int
dialog_format_init(struct dialog_format *df,
                   shell *shl,
                   struct file *file) {
    GtkWidget *table, *formats_list;
    struct dialog *dialog = (struct dialog *)df;
    static struct pane_signal_bindings bindings[] = {
        { "save_browse", "clicked", dialog_format_browse_clicked },
    };

    if(!dialog_format_inited) {
        dialog_format_get_formats();
        dialog_format_inited = 1;
        if(!dialog_format_format_ids) {
            mem_free(df);
            return 1;
        }
    }

    if(dialog_init((struct dialog *)df, 
                   GLADE_FILE,
                   "formatsdialog", 
                   shl,
                   sizeof(bindings) / sizeof(bindings[0]),
                   bindings, df))
        return 1;

    ((struct dialog *)df)->dtor = dialog_format_destroy;
    ((struct dialog *)df)->close = dialog_format_close;
    
    file_addref(file);
    df->file = file;
    df->fd = NULL;
    df->options_widget = NULL;

    formats_list = dialog_format_new_formats_list(df);
    table = pane_get_widget(dialog->pane, "save_format_table");
    gtk_table_attach(GTK_TABLE(table),
                     formats_list,
                     1, 2,
                     1, 2,
                     GTK_EXPAND | GTK_FILL, 0,
                     0, 0);
    pane_register_widget(dialog->pane, "save_formats", formats_list);
    
    gtk_entry_set_text(GTK_ENTRY(pane_get_widget(dialog->pane, "save_filename")), file->name);
    
    if(dialog_format_select_format_by_filename(df, file->name))
        dialog_format_select_format_by_id(df, dialog_format_format_ids->data);
    
    return 0;
}

struct dialog *
dialog_format_new(shell *shl,
                  struct file *file) {
    struct dialog_format *df = mem_alloc(sizeof *df);

    if(!df)
        return NULL;

    if(dialog_format_init(df, shl, file)) {
        mem_free(df);
        return NULL;
    }

    return (struct dialog *)df;
}
