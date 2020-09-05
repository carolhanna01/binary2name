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
#include "dialog_format_mixdown.h"

static void
dialog_format_mixdown_apply(struct dialog *dialog,
                            void *user_data) {
    struct dialog_format *df = user_data;
    struct dialog_format_mixdown *dfm = user_data;

    dialog_format_apply(dialog, user_data);

    file_addref(df->file);
    shell_dispatch(dialog->shl,
                   CMD_NEW("mixdown-document-with-map-as",
                           cmd_new_shellp_val(dialog->shl),
                           cmd_new_filep_val_with_dtor(df->file,
                                                       cmd_filep_dtor),
                           cmd_new_int_val(dfm->map)));
}

struct dialog *
dialog_format_mixdown_new(shell *shl,
                          struct file *file,
                          track_map_t map) {
    struct dialog_format_mixdown *dfm = mem_alloc(sizeof *dfm);
    if(!dfm)
        return NULL;

    if(dialog_format_init((struct dialog_format *)dfm, shl, file)) {
        mem_free(dfm);
        return NULL;
    }
    
    ((struct dialog *)dfm)->apply = dialog_format_mixdown_apply;

    dfm->map = map;
    
    return (struct dialog *)dfm;
}
