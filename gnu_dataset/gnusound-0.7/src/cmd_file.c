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
#include "pref.h"
#include "file.h"
#include "gui.h"
#include "view.h"
#include "cmd.h"
#include "arbiter.h"
#include "constraints.h"
#include "dialog_mixdown.h"
#include "dialog_format_save.h"
#include "dialog_format_mixdown.h"

static int untitled_counter = 1;
static char untitled_name[20];
static char *path_open = NULL;
static char *path_save = NULL;

static struct cmd_value *
cmd_file_new_document(const char *name,
                      struct cmd_argv *args) {
    struct clip *clip;
    shell *shl;

    snprintf(untitled_name, 12, "Untitled%d", untitled_counter++);

    clip = clip_new(untitled_name, 1, 
                    pref_get_as_float("default_sample_rate"),
                    pref_get_as_int("default_sample_type"));
    if(!clip)
        return cmd_new_error_val("Could not create clip. Maybe memory "
                                 "is low.");

    shl = shell_new(clip);
    if(!shl) {
        clip_destroy(clip);
        return cmd_new_error_val("Could not create shell. Maybe the GUI "
                                 "definition file cannot be found or memory "
                                 "is low.");
    }

    view_show(shl->view);
    return cmd_new_shellp_val(shl);
}

static struct cmd_value *
cmd_file_open(const char *name,
              struct cmd_argv *args) {
    struct file *file = cmd_filep(args->argv[0]);
    shell *shl;
    struct cmd_value *r;
    struct cmd *cmd;

    cmd = CMD_NEW0("new-document");
    if(cmd_do_or_fail(cmd, "Cannot create new document (%s)", &r)) 
        return r;
    shl = cmd_shellp(r);
    cmd_destroy_value(r);
    
    /* Need to add a reference to make sure this object isn't
       destroyed when this command gets destroyed as part of the
       command destruct sequence. */

    file_addref(file);

    shell_dispatch(shl,
                   CMD_NEW("load-file", 
                           cmd_new_shellp_val(shl),
                           cmd_new_filep_val(file)));
    
    return cmd_new_shellp_val(shl);
}

static struct cmd_value *
cmd_file_close_document(const char *name,
                        struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    player_stop(shl->player);
    shl->close_requested = 1;
    shl->cancel_requested = 1;
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_file_request_close_document(const char *name,
                                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int button, allow = 0;
    struct cmd *cmd;
    struct cmd_value *r;

    if(!shl->has_changed) {
        allow = 1;
    } else {
        button = gui_yes_no("Close document?",
                            "Close document %s without saving changes?", 
                            shl->file->name);
        if(button == GUI_NO || button == GUI_CANCEL) 
            allow = 0;
        else
            allow = 1;
    }

    if(allow) {
        shell_dispatch(shl,
                       CMD_NEW("close-document", cmd_new_shellp_val(shl)));
    }

    return cmd_new_int_val(allow);
}

static void
cmd_file_open_selected(const char *filename,
                       void *user_data) {
    struct file *file = file_new(filename);
    if(!file) 
        return;

    if(path_open)
        free(path_open);

    if(filename)
        path_open = g_path_get_dirname(filename);

    arbiter_queue_cmd(CMD_NEW("open-file", 
                              cmd_new_filep_val_with_dtor(file, 
                                                          cmd_filep_dtor)));
}

static struct cmd_value *
cmd_file_select_and_open(const char *name,
                         struct cmd_argv *args) {
    GtkWidget *w = gui_file_selector_new("Select an audio file",
                                         path_open,
                                         GUI_FILE_SELECTOR_OPEN,
                                         cmd_file_open_selected,
                                         NULL,
                                         NULL);

    gui_file_selector_run(w);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_file_load(const char *name,
              struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct file *file = cmd_filep(args->argv[1]);
    struct cmd_value *r;

    shell_start_operation(shl, "Loading");
    constraints_push(shl->constraints,
                     "Loading file",
                     region_new(REGION_MATCH_ANYTHING, 
                                REGION_MATCH_ANYTHING, 
                                REGION_MATCH_ANYTHING),
                     (CONSTRAIN_POSITION |
                      CONSTRAIN_LENGTH |
                      CONSTRAIN_CONTENTS));
    
    r = file_load(shl, file);

    constraints_pop(shl->constraints);
    shell_end_operation(shl);

    return r;
}

/*
 * There are three commands which save a file:
 *
 * - save-document
 * - select-file-and-save
 * - save-document-as
 * 
 * The confusing thing is that they don't correspond to the
 * menu items. 
 *
 * save-document corresponds to the standard "Save"
 * menu item.
 * select-file-and-save corresponds to the "Save As" menu item.
 * save-document-as has no corresponding menu item, it is the
 * command underlying both save-document and select-file-and-save:
 *
 *  save-document(shell)
 *           |
 *           v
 *  [document has name?] ----> select-file-and-save(shell) 
 *           |            no            |
 *           | yes                      | 
 *           |                          v
 *           |                  [user selected a file?] ----> [no action]
 *           |                          |                no
 *           |                          | yes
 *           |                          |
 *           v                          v
 *         save-document-as(shell, filename)
 */

static struct cmd_value *
cmd_file_select_format_and_save(const char *name,
                                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct file *file = cmd_filep(args->argv[1]);
    struct dialog *dialog = dialog_format_save_new(shl, file);

    dialog_open(dialog);

    return cmd_new_void_val();
}

static void
cmd_file_save_file_selected(const char *filename,
                            void *user_data) {
    shell *shl = user_data;
    struct file *file;
    int button;
    
    /* If file exists, then ask whether to overwrite. */

    DEBUG("filename: %s\n", filename);
    if(!access(filename, W_OK)) {
        button = gui_yes_no("Overwrite file?",
                            "Overwrite file %s?", 
                            filename);
        if(button == GUI_NO || button == GUI_CANCEL) 
            return;
    }
    file = file_new(filename);
    if(!file) {
        FAIL("could not create file object!\n");
        return;
    }

    path_save = g_strdup(filename);

    arbiter_queue_cmd(CMD_NEW("select-format-and-save", 
                              cmd_new_shellp_val(shl),
                              cmd_new_filep_val_with_dtor(file,
                                                          cmd_filep_dtor)));
}

static void
widget_detach_from_view(GtkWidget *w,
                        struct view *view) {
    view_detach_dialog(view, w);
}

static struct cmd_value *
cmd_file_select_file_and_save(const char *name,
                              struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    GtkWidget *w;
    
    w = gui_file_selector_new("Select file to save as",
                              path_save,
                              GUI_FILE_SELECTOR_SAVE,
                              cmd_file_save_file_selected,
                              NULL,
                              shl);

    g_signal_connect(G_OBJECT(w),
                     "destroy",
                     G_CALLBACK(widget_detach_from_view),
                     shl->view);

    view_attach_dialog(shl->view, w);

    gui_file_selector_run(w);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_file_save_document(const char *name,
                       struct cmd_argv *args) {
    int need_info, i;
    const struct file_format *output_formats;
    char file_format[256];
    shell *shl = cmd_shellp(args->argv[0]);

    /* 
     * Determine whether this document can be saved: it must have a
     * name, and it must be associated with a driver that can write
     * the format. 
     */

    need_info = 0;

    if(shl->has_name)
        need_info++;
    
    if(shl->file->driver) {
        shl->file->driver->snprint(shl->file, FILE_FORMAT, 
                                   file_format, sizeof file_format);
        output_formats = shl->file->driver->get_write_formats(shl->file);
        if(output_formats) {
            for(i = 0; output_formats[i].name; i++) {
                if(!strcmp(file_format, output_formats[i].name))
                    need_info++;
            }
        }
    }
    
    if(need_info == 2)
        shell_dispatch(shl,
                       CMD_NEW("save-document-as", 
                               cmd_new_shellp_val(shl),
                               cmd_new_filep_val(shl->file)));
    else
        arbiter_queue_cmd(CMD_NEW("select-file-and-save", 
                                  cmd_new_shellp_val(shl)));

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_file_save_document_as(const char *name,
                          struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct file *file = cmd_filep(args->argv[1]);
    struct file_params params;
    struct cmd_value *r;
    const char *s;
    
    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot save because region is locked "
                                 "(%s)", s);
    
    constraints_push(shl->constraints,
                     "Saving document",
                     region_new(REGION_MATCH_ANYTHING, 
                                REGION_MATCH_ANYTHING, 
                                REGION_MATCH_ANYTHING),
                     (CONSTRAIN_POSITION |
                      CONSTRAIN_LENGTH |
                      CONSTRAIN_CONTENTS));

    params.channels = shl->clip->sr->channels;
    params.sample_type = shl->clip->sr->sample_type;
    params.sample_rate = shl->clip->sr->rate;
    params.frame_count = snd_frame_count(shl->clip->sr, MAP_ALL);

    r = file_save(shl, file, TRUE);

    if(!cmd_is_error(r)) 
        shell_attach_file(shl, file);

    view_reset_status(shl->view);

    constraints_pop(shl->constraints);

    return r; 
}

/* Mixdown. */

static void
cmd_file_mixdown_selected(const char *filename,
                          void *user_data) {
    shell *shl = user_data;
    struct file *file;
    int button;
    
    /* If file exists, then ask whether to overwrite. */
    
    DEBUG("filename: %s\n", filename);
    if(!access(filename, W_OK)) {
        button = gui_yes_no("Overwrite file?",
                            "Overwrite file %s?", 
                            filename);
        if(button == GUI_NO || button == GUI_CANCEL) 
            return;
    }
    file = file_new(filename);
    if(!file) {
        FAIL("could not create file object!\n");
        return;
    }
    arbiter_queue_cmd(CMD_NEW("mixdown-document-as", 
                              cmd_new_shellp_val(shl),
                              cmd_new_filep_val_with_dtor(file, 
                                                          cmd_filep_dtor)));
}

static struct cmd_value *
cmd_file_select_and_mixdown(const char *name,
                            struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    GtkWidget *w;

    w = gui_file_selector_new("Select file to mixdown as",
                              path_save,
                              GUI_FILE_SELECTOR_SAVE,
                              cmd_file_mixdown_selected,
                              NULL,
                              shl);
    g_signal_connect(G_OBJECT(w),
                     "destroy",
                     G_CALLBACK(widget_detach_from_view),
                     shl->view);

    view_attach_dialog(shl->view, w);

    gui_file_selector_run(w);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_file_mixdown_document_as(const char *name,
                             struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct file *file = cmd_filep(args->argv[1]);
    struct dialog *dialog;

    file_addref(file);
    dialog = dialog_mixdown_new(shl, file);
    dialog_open(dialog);

    return cmd_new_void_val();
}

static struct cmd_value *
cmd_file_mixdown_document_with_map_as(const char *name,
                                      struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    struct file *file = cmd_filep(args->argv[1]);
    int map = cmd_int(args->argv[2]);
    const char *s;
    mixer *mixer;
    struct cmd_value *r;

    if((s = constraints_test(shl->constraints,
                             region_new(shl->select_channel_map,
                                        shl->select_start,
                                        shl->select_end - shl->select_start),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot %s because region is locked "
                                 "(%s)", name, s);
    
    mixer = mixer_clone(shl->clip->mixer);
    if(!mixer) {
        FAIL("could not clone mixer\n");
        return cmd_new_error_val("Could not clone mixer");
    }

    if(mixer->output_channels == 0) 
        return cmd_new_error_val("Cannot mixdown zero channels");
        
    /*
     * The map specifies which channels to mix down. We need the
     * opposite, i.e. which channels to remove from the mixer. We
     * can't just use the bitwise inverse operator, since that would
     * set bits for tracks which don't exist. So after inverting we
     * apply channels^2-1 as a mask. 
     */

    if(mixer->output_channels == 1)
        map = ~map & 1;
    else
        map = ~map & ((mixer->output_channels * mixer->output_channels) - 1);

    mixer_destroy(mixer_delete_output_channels(mixer, map));

    constraints_push(shl->constraints,
                     "Mixdown document",
                     region_new(REGION_MATCH_ANYTHING, 
                                REGION_MATCH_ANYTHING, 
                                REGION_MATCH_ANYTHING),
                     (CONSTRAIN_POSITION |
                      CONSTRAIN_LENGTH |
                      CONSTRAIN_CONTENTS));

    r = file_mixdown(shl, mixer, file, TRUE);
    
    constraints_pop(shl->constraints);

    mixer_destroy(mixer);

    return r;
}

/* Closing and quitting. */

void
try_to_close_shell(shell *shl,
                   void *user_data) {
    int *failed = (int *)user_data;
    struct cmd *cmd;
    struct cmd_value *r;

    if(*failed)
        return;
    
    cmd = CMD_NEW("request-close-document", cmd_new_shellp_val(shl));
    if(cmd_do_or_fail(cmd, "Cannot request close (%s)", &r)) {
        *failed = 1;
        cmd_destroy_value(r);
        return;
    }
    *failed = !cmd_int(r);

    cmd_destroy_value(r);
}

static struct cmd_value *
cmd_file_request_exit(const char *name,
                      struct cmd_argv *args) {
    int failed = 0;
    struct cmd *cmd;
    struct cmd_value *r;

    arbiter_foreach_shell(try_to_close_shell, &failed);
    if(failed) 
        return cmd_new_void_val();
    
    cmd = CMD_NEW0("exit");
    if(cmd_do_or_fail(cmd, "Cannot exit (%s)", &r)) 
        return r;
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_file_exit(const char *name,
              struct cmd_argv *args) {
    arbiter_request_quit();
    return cmd_new_void_val();
}

int
cmd_file_init() {
    int i; 
    struct cmd_signature f[] = {

        { "new-document", "Opens a new document.",
          cmd_file_new_document, CMD_shellp_T, 
          cmd_new_paramdecl(0) },

        { "select-file-and-open", 
          "Queries the user for a file then opens it into a new document.",
          cmd_file_select_and_open, CMD_VOID_T,
          cmd_new_paramdecl(0) },

        { "load-file", 
          "Loads a file into an existing document, "
          "erasing the existing document.",
          cmd_file_load, CMD_VOID_T, 
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_filep_T) },

        { "open-file", "Opens a new document and loads a file into it.",
          cmd_file_open, CMD_shellp_T, 
          cmd_new_paramdecl(1, CMD_filep_T) },

        { "save-document", 
          "Saves a document to a file. Asks the user for a filename "
          "if the document hasn't been saved or loaded earlier.",
          cmd_file_save_document, CMD_VOID_T,
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "save-document-as", "Saves a document to a file.",
          cmd_file_save_document_as, CMD_VOID_T,
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_filep_T) },

        { "select-file-and-save", 
          "Queries the user for a filename and saves the document "
          "to that file.",
          cmd_file_select_file_and_save, CMD_VOID_T,
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "select-format-and-save", 
          "Queries the user for a file format and saves the document "
          "to the specified file.",
          cmd_file_select_format_and_save, CMD_VOID_T,
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_filep_T) },

        { "select-file-and-mixdown", 
          "Queries the user for a filename and saves a mixdown of "
          "the document to that file.",
          cmd_file_select_and_mixdown, CMD_VOID_T,
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "mixdown-document-as", 
          "Saves a mixdown of the document to a file.",
          cmd_file_mixdown_document_as, CMD_VOID_T,
          cmd_new_paramdecl(2, CMD_shellp_T, CMD_filep_T) },

        { "mixdown-document-with-map-as", 
          "Saves a mixdown of the specified tracks of the document to a file.",
          cmd_file_mixdown_document_with_map_as, CMD_VOID_T,
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_filep_T, CMD_int_T) },

        { "request-close-document", "Requests closing a document.",
          cmd_file_request_close_document, CMD_int_T,
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "close-document", "Unconditionally closes a document.",
          cmd_file_close_document, CMD_VOID_T,
          cmd_new_paramdecl(1, CMD_shellp_T) },

        { "request-exit", "Closes all documents and quits.",
          cmd_file_request_exit, CMD_VOID_T,
          cmd_new_paramdecl(0) },

        { "exit", "Closes all documents and quits.",
          cmd_file_exit, CMD_VOID_T,
          cmd_new_paramdecl(0) }

    };

    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
