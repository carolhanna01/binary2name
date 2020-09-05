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

    dialog = gnome_message_box_new(message,
                                   GNOME_MESSAGE_BOX_QUESTION,
                                   GNOME_STOCK_BUTTON_YES,
                                   GNOME_STOCK_BUTTON_NO,
                                   NULL);
    button = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

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

    dialog = gnome_message_box_new(wordwrap(message, 60),
                                   GNOME_MESSAGE_BOX_WARNING,
                                   GNOME_STOCK_BUTTON_OK,
                                   NULL);
    gnome_dialog_run(GNOME_DIALOG(dialog));

}

