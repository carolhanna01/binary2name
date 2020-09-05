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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

/**
 * @file
 * The document shell, corresponds to an open document window.
 */

#include <config.h>
#include "pref.h"
#include "gui.h"
#include "arbiter.h"
#include "tool.h"
#include "view.h"
#include "shell.h"

extern shell *clipboard_shell;
extern struct clip *clipboard;

const char *
shell_get_next_label(shell *shl) {
    g_return_val_if_fail(shl != NULL, NULL);

    snprintf(shl->label_str, sizeof shl->label_str, "label%d", shl->label++);

    return shl->label_str;
}

void
shell_attach_file(shell *shl,
                  struct file *file) {
    if(file == shl->file)
        return;

    if(shl->file)
        file_destroy(shl->file);
    
    shl->file = file;
    file_addref(file);
    view_sync_display(shl->view);

    return;
}

int
shell_configure(shell *shl,
                const char *filename,
                struct file_params *params) {
    struct clip *new_clip;

    new_clip = clip_new(filename, 
                        params->channels,
                        params->sample_rate,
                        params->sample_type);

    if(!new_clip)
        return 1;

    if(shl->clip)
        clip_destroy(shl->clip);

    shl->clip = new_clip;

    view_clip_changed(shl->view);

    return 0;
}

/**
 * Starts an interruptable operation. Interruptable operations are
 * those which invoke arbiter_yield() at some point during their
 * execution.
 * @param shl The shell.
 * @param operation The operation.
 */

void
shell_start_operation(shell *shl,
                      const char *operation) {
    shl->cancel_requested = 0;
    shl->use++;
    view_start_operation(shl->view, operation);
}

/**
 * Ends an operation previously started by shell_start_operation().
 * @param shl The shell.
 */

void
shell_end_operation(shell *shl) {
    view_end_operation(shl->view);
    shl->use--;
    shl->cancel_requested = 0;
}

/**
 * Determines whether the given shell is the clipboard shell.
 * @param shl The shell.
 * @return 0 if the given shell is not the clipboard shell, non-zero 
 * otherwise.
 */

int
shell_is_clipboard(shell *shl) {
    return(shl == clipboard_shell);
}

/**
 * Executes the given command in the context of the
 * given shell. @see shell_dispatch_as().
 * @param shl The shell.
 * @param cmd The command.
 */

void
shell_dispatch(shell *shl,
               struct cmd *cmd) {
    arbiter_queue_cmd(CMD_NEW("dispatch-cmd", 
                              cmd_new_shellp_val(shl),
                              cmd_new_cmdp_val_with_dtor(cmd, cmd_cmd_dtor),
                              cmd_new_charp_val(cmd->name)));
}

/**
 * Executes the given command under the specified name in the
 * context of the given shell. This invokes the dispatch-cmd
 * command with the specified command. dispatch-cmd sets up the
 * history and checks to see whether the shell should be closed
 * after executing the command. This is a convenience function
 * that may disappear.
 * @param shl The shell.
 * @param cmd The command.
 * @param name The name of the command as it should appear in
 * the undo history.
 */

void
shell_dispatch_as(shell *shl,
                  struct cmd *cmd,
                  const char *name) {
    arbiter_queue_cmd(CMD_NEW("dispatch-cmd", 
                              cmd_new_shellp_val(shl),
                              cmd_new_cmdp_val_with_dtor(cmd, cmd_cmd_dtor),
                              cmd_new_charp_val((char *)name)));
}

struct grid *
shell_get_grid(shell *shl) {
    return &shl->grid;
}

/**
 * Returns the state for the specified module.
 * @param shl The shell.
 * @param id The module to get state for.
 * @return The module state or NULL if id is out of range.
 */

struct module_state *
shell_get_module_state(shell *shl, 
                       int id) {
    if(id < 0 || id >= module_get_count())
        return NULL;

    return &(shl->module_state[id]);
}

/****************************************************************
 * Tools support.
 ****************************************************************/

struct tool *
shell_get_tool(shell *shl,
               const char *name) {
    return g_hash_table_lookup(shl->tools, (char *)name);
}

struct tool *
shell_get_active_tool(shell *shl) {
    if(!shl->active_tool)
        return NULL;

    return g_hash_table_lookup(shl->tools, shl->active_tool);
}

/**
 * Switches to a different tool.
 * @param shl The shell.
 * @param name The tool name.
 */

void
shell_activate_tool(shell *shl,
                    const char *name) {
    struct tool *t = NULL;
    
    if(shl->active_tool) 
        t = g_hash_table_lookup(shl->tools, shl->active_tool);
    
    if(t && t->funcs->deactivate) 
        t->funcs->deactivate(t);
    
    t = g_hash_table_lookup(shl->tools, name);

    if(!t)
        return;

    if(t && t->funcs->activate) 
        t->funcs->activate(t);
    
    shl->active_tool = name;
    
    view_activate_tool(shl->view, name);
    view_set_default_cursor(shl->view, t->cursor);
}

/**
 * Called by tool_bind() to register a tool.
 */

void
shell_bind_tool(shell *shl,
                struct tool *tool) {
    g_hash_table_insert(shl->tools, (char *)tool->name, tool);
    view_bind_tool(shl->view, tool);
}

/****************************************************************
 * Destruction.
 ****************************************************************/
 
static void
shell_destroy_tool_one(void *key,
                       void *value,
                       void *user_data) {
    struct tool *tool = value;
    DEBUG("destroying tool %s\n", tool->name);
    tool->funcs->destroy(tool);
}

static void
shell_destroy_tools(shell *shl) {
    g_hash_table_foreach(shl->tools, shell_destroy_tool_one, shl);
    g_hash_table_destroy(shl->tools);
}

static void
shell_detach_modules(shell *shl) {
    int i;
    struct gnusound_module *module;

    for(i = 0; i < module_get_count(); i++) {
        
        module = module_get(i);

        if(module->detach)
            module->detach(i, shl);
    }

}

void
shell_close_modules(shell *shl) {
    int i;
    struct module_state *module_state;
    struct gnusound_module *module;
    
    for(i = 0; i < module_get_count(); i++) {
        
        module = module_get(i);
        module_state = shell_get_module_state(shl, i);
        
        if(module_state->is_open)
            if(module->close) 
                module->close(i, shl);
            
    }
}

void
shell_destroy_internal(shell *shl) {
    DEBUG("destroying %s\n", shl->file->name);

    player_stop(shl->player);
    player_destroy(shl->player);
    shell_destroy_tools(shl);
    shell_close_modules(shl);
    shell_detach_modules(shl);
    history_destroy(shl->history);
    view_destroy(shl->view);
    constraints_destroy(shl->constraints);

    if(shl->clip != clipboard)
        clip_destroy(shl->clip);

    if(shl == clipboard_shell)
        clipboard_shell = NULL;

    file_destroy(shl->file);

    arbiter_remove_shell(shl);
    mem_free(shl);
}

void
shell_destroy(shell *shl) {
    gtk_object_destroy(GTK_OBJECT(view_get_widget(shl->view, "shell")));
}

/****************************************************************
 * Initialization.
 ****************************************************************/

static void
shell_attach_modules(shell *shl) {
    int i;
    struct gnusound_module *module;

    for(i = 0; i < module_get_count(); i++) {

        module = module_get(i);

        /* FIXME: attach failure is ignored */

        if(module->attach)
            module->attach(i, shl);
    }

}

shell *
shell_new(struct clip *clip) { 
    shell *shl;

    shl = mem_calloc(sizeof(shell), 1);
    if(!shl) {
        FAIL("could not allocate memory for new shell object\n");
        return NULL;
    }
    shl->clip = clip;

    shl->constraints = constraints_new();
    if(!shl->constraints) {
        mem_free(shl);
        return NULL;
    }
    shl->player = player_new(shl);
    if(!shl->player) {
        constraints_destroy(shl->constraints);
        mem_free(shl);
        return NULL;
    }
    shl->history = history_new();
    if(!shl->history) {
        constraints_destroy(shl->constraints);
        player_destroy(shl->player);
        mem_free(shl);
        return NULL;
    }
    shl->file = file_new(clip->sr->namep);
    if(!shl->file) {
        constraints_destroy(shl->constraints);
        history_destroy(shl->history);
        player_destroy(shl->player);
        mem_free(shl);
        return NULL;
    }
    shl->view = view_new(shl);
    if(!shl->view) {
        constraints_destroy(shl->constraints);
        file_destroy(shl->file);
        history_destroy(shl->history);
        player_destroy(shl->player);
        mem_free(shl);
        return NULL;
    }

    shl->magic[0] = 'O';
    shl->magic[1] = 'k';

    shl->select_start = shl->select_end = 0; 
    shl->select_channel_map = 1;

    shl->label = 1;
    shl->record_mode = 0;
    shl->loop = 1;
    shl->record_replace = pref_get_as_int("record_replace");
    shl->snap_to_grid = pref_get_as_int("snap_to_grid");
    shl->snap_to_cuepoints = pref_get_as_int("snap_to_cuepoints");

    shl->cancel_requested = 0;
    shl->close_requested = 0;
    shl->use = 0;
    shl->active_tool = NULL;

    grid_bpm_set(&shl->grid, pref_get_as_float("default_bpm"));
    grid_units_set(&shl->grid, 1);
    grid_rate_set(&shl->grid, pref_get_as_float("default_sample_rate"));
    grid_measurement_set(&shl->grid, GRID_SECONDS);

    arbiter_add_shell(shl);

    shell_attach_modules(shl);

    /* Create our tools. */

    shl->tools = g_hash_table_new(g_str_hash, g_str_equal);
    tool_bind_all(shl);
    shell_activate_tool(shl, "select");
    
    return shl;
}
