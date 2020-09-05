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
#include "view.h"
#include "shell.h"
#include "gui.h"
#include "dialog.h"

/**
 * @file 
 * A dialog provides a thin abstraction for a GUI dialog.  It's not
 * been thought out very well to be honest but it does cut down a
 * little on the amount of code that has to be written for each
 * dialog.
 *
 * What a dialog does is take a GTK top level widget and connect to
 * the "clicked" signals of the dialog_ok, dialog_close and
 * (optionally) the dialog_apply buttons. Then it dispatches those
 * events through the ok, apply and close callbacks.
 *
 * So essentially, a dialog encapsulates the powerful generalized GLib
 * signal architecture within a less powerful but in some ways
 * cleaner/easier interface. 
 */

void
dialog_close(struct dialog *dialog) {
    gtk_widget_hide(pane_get_widget(dialog->pane, dialog->root));

    dialog->flags &= ~DIALOG_IS_OPEN;

    if(dialog->close)
        dialog->close(dialog, dialog->user_data);
}

void
dialog_open(struct dialog *dialog) {
    if(dialog->flags & DIALOG_IS_OPEN) {
        gdk_window_raise(GTK_WIDGET(pane_get_widget(dialog->pane, dialog->root))->window);
        return;
    }

    if(dialog->open)
        dialog->open(dialog, dialog->user_data);

    gtk_widget_show(pane_get_widget(dialog->pane, dialog->root));

    dialog->flags |= DIALOG_IS_OPEN;
}

static void
dialog_on_destroy(GtkWidget *w,
                  struct dialog *dialog) {

    DEBUG("destroying dialog %s\n", dialog->root);

    if(dialog->shl)
        view_detach_dialog(dialog->shl->view, w);

    if(dialog->flags & DIALOG_IS_OPEN)
        dialog_close(dialog);

    if(dialog->dtor)
        dialog->dtor(dialog, dialog->user_data);

    if(dialog->pane)
        pane_destroy(dialog->pane);

    if(dialog->root)
        mem_free(dialog->root);

    mem_free(dialog);
}

static void
dialog_close_clicked(GtkWidget *w,
                     struct dialog *dialog) {
    dialog_close(dialog);
}

static void
dialog_ok_clicked(GtkWidget *w,
                  struct dialog *dialog) {
    if(dialog->apply)
        dialog->apply(dialog, dialog->user_data);

    dialog_close(dialog);
}

static void
dialog_apply_clicked(GtkWidget *w,
                     struct dialog *dialog) {
    if(dialog->apply)
        dialog->apply(dialog, dialog->user_data);
}

int
dialog_init(struct dialog *dialog,
            const char *glade_xml_file,
            const char *name,
            shell *shl,
            int n_bindings,
            const struct pane_signal_bindings bindings[],
            void *user_data) {
    GladeXML *xml;
    char *widget_name;
    static struct pane_signal_bindings default_bindings[] = {
        { "dialog_ok", "clicked", dialog_ok_clicked },
        { "dialog_close", "clicked", dialog_close_clicked },
    };
    static struct pane_signal_bindings apply_bindings[] = {
        { "dialog_apply", "clicked", dialog_apply_clicked },
    };
    
    widget_name = strdup(name);

    g_return_val_if_fail(widget_name != NULL, 1);

    DEBUG("creating dialog %s\n", name);
    xml = gui_get_xml(glade_xml_file, name);

    g_return_val_if_fail(xml != NULL, 1);

    dialog->pane = pane_new(xml);
    if(!dialog->pane) {
        g_object_unref(G_OBJECT(xml));
        return 1;
    }

    dialog->shl = shl;
    dialog->user_data = user_data;
    dialog->root = widget_name;
    dialog->flags = 0;

    dialog->open = NULL;
    dialog->apply = NULL;
    dialog->close = NULL;
    dialog->dtor = NULL;

    if(shl)
        view_attach_dialog(shl->view, pane_get_widget(dialog->pane, name));

    g_signal_connect(G_OBJECT(pane_get_widget(dialog->pane, name)),
                     "destroy",
                     G_CALLBACK(dialog_on_destroy),
                     dialog);
    pane_connect_bindings(dialog->pane, 
                          sizeof(default_bindings) / sizeof(default_bindings[0]), 
                          default_bindings, 
                          dialog);

    if(pane_find_widget(dialog->pane, "dialog_apply")) 
        pane_connect_bindings(dialog->pane,
                              (sizeof(apply_bindings) / 
                               sizeof(apply_bindings[0])), 
                              apply_bindings, 
                              dialog);
                              
    pane_connect_bindings(dialog->pane, n_bindings, bindings, user_data);
    return 0;
}

void
dialog_destroy(struct dialog *dialog) {
    gtk_widget_destroy(pane_get_widget(dialog->pane, dialog->root));
}

/**
 * Creates a new dialog.
 * @param size The size of the struct, must be >= sizeof(struct dialog).
 * @param shl The parent shell of this dialog, can be NULL.
 * @param name The name of this dialog (the Glade dialog widget name).
 * @param n_bindings Number of bindings in the bindings array.
 * @param bindings Widget bindings.
 * @param user_data Data to pass on to ok, apply and close handlers.
 */

struct dialog *
dialog_new(size_t size,
           const char *glade_xml_file,
           const char *name,
           shell *shl,
           int n_bindings,
           const struct pane_signal_bindings bindings[],
           void *user_data) {
    struct dialog *dialog;

    g_return_val_if_fail(size >= sizeof(struct dialog), NULL);
    g_return_val_if_fail(name != NULL, NULL);

    dialog = mem_alloc(size);

    g_return_val_if_fail(dialog != NULL, NULL);

    if(dialog_init(dialog,
                   glade_xml_file,
                   name,
                   shl,
                   n_bindings,
                   bindings,
                   user_data == NULL ? dialog : user_data)) {
        mem_free(dialog);
        return NULL;
    }

    return dialog;
}
