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
#include "cmd.h"
#include "arbiter.h"
#include "dialog_format.h"
#include "dialog_format_save.h"

static void
dialog_format_save_apply(struct dialog *dialog,
                         void *user_data) {
    struct dialog_format *df = user_data;

    dialog_format_apply(dialog, user_data);

    file_addref(df->file);
    shell_dispatch(dialog->shl,
                   CMD_NEW("save-document-as",
                           cmd_new_shellp_val(dialog->shl),
                           cmd_new_filep_val_with_dtor(df->file,
                                                       cmd_filep_dtor)));
}
struct dialog *
dialog_format_save_new(shell *shl,
                       struct file *file) {
    struct dialog *dialog = dialog_format_new(shl, file);
    if(!dialog)
        return NULL;

    dialog->apply = dialog_format_save_apply;

    return dialog;
}

