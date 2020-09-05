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
#include <gtk/gtk.h>
#include <gui_dialogs.h>
#include "../lib/misc.h"
#include "../mem.h"

/**
 * Prompts the user for a yes/no decision.
 * @param title The window title.
 * @param format The format string for the message to display to the user.
 * @return One of GUI_CANCEL, GUI_YES, or GUI_NO.
 */

int 
gui_yes_no(const char *title,
           const char *format,
           ...) {
    va_list ap;
    char *message;
    int button;
    GtkWidget *dialog;

    message = (char *) mem_alloc(strlen(format) + 4096);

    if(!message) {
        FAIL("could not get memory to display error string. start of error: %s\n", format);
        return GUI_CANCEL;
    }
    
    va_start(ap, format);
    vsnprintf(message, strlen(format) + 4096, format, ap);
    va_end(ap);

    dialog = gtk_message_dialog_new(NULL,
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_QUESTION,
                                    GTK_BUTTONS_YES_NO,
                                    message);
    button = gtk_dialog_run(GTK_DIALOG(dialog));
    switch(button) {
    case GTK_RESPONSE_YES:
        button = GUI_YES;
        break;
    case GTK_RESPONSE_NO:
        button = GUI_NO;
        break;
    default:
        button = GUI_CANCEL;
    }
    gtk_widget_destroy(dialog);

    free(message);
    DEBUG("returning button: %d\n", button);
    return button;
}

/**
 * Displays a message to the user.
 * @param format The format string for the message to display to the user.
 */

void
gui_alert(const char *format,
          ...) {
    va_list ap;
    char message[4096];
    GtkWidget *dialog;

    va_start(ap, format);
    vsnprintf(message, 4096, format, ap);
    va_end(ap);

    dialog = gtk_message_dialog_new(NULL,
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_CLOSE,
                                    wordwrap(message, 60));
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
}

