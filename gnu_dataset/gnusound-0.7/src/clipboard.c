/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2004,2005  Pascal Haakmat <a.haakmat@chello.nl>
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
#include "pref.h"
#include "snd.h"
#include "shell.h"
#include "clip.h"
#include "clipboard.h"

shell *clipboard_shell;
struct clip *clipboard = NULL;

void
clipboard_show() {
    GtkWidget *widget;
    GdkWindow *window;

    static const char *
        disabled_bindings_for_clipboard[] = {
            "save",
            "saveas",
            "mixdown",
            "copy",
            "cut",
            "paste",
            "paste_over",
            "paste_fit",
            "paste_mix",
            "show_clipboard",
            NULL
        };
    
    if(clipboard_shell) {

        widget = view_get_widget(clipboard_shell->view, "shell");
        window = gtk_widget_get_parent_window(widget);
        gdk_window_raise(window);
        return;

    }

    clipboard_shell = shell_new(clipboard);
    view_disable_widgets(clipboard_shell->view, 
                         disabled_bindings_for_clipboard);
    view_show(clipboard_shell->view);
}

void
clipboard_replace_snd(snd *new_snd) {

    DEBUG("putting %ld frames onto clipboard\n", snd_frame_count(new_snd, MAP_ALL));
    snd_set_name(new_snd, "clipboard");
    clip_set_snd(clipboard, new_snd);
}

snd *
clipboard_copy_snd(enum sample_type type,
                   int rate) {
    snd *copy = snd_clone(clipboard->sr, 
                          CLONE_STRUCTURE |
                          CLONE_TRACK_STRUCTURE | 
                          CLONE_TRACK_DATA);
    if(error_thrown(ERROR(clipboard))) 
        goto recover_clone;
    
    snd_set_name(copy, "clipboard converted copy");
    snd_convert(copy, type, rate);
    if(error_thrown(ERROR(copy)))
        goto recover_convert;
    
    return copy;

 recover_clone:
 recover_convert:
    snd_destroy(copy);
    return NULL;
}

void
clipboard_exit() {
    clip_destroy(clipboard);
}

int
clipboard_init() {
    clipboard_shell = NULL;
    clipboard = clip_new("clipboard", 
                         0, 
                         pref_get_as_float("default_sample_rate"),
                         pref_get_as_int("default_sample_type"));
    
    g_return_val_if_fail(clipboard != NULL, 1);

    return 0;
}
