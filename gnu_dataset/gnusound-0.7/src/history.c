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

/**
 * @file
 *
 * The history is a stack of state transitions. The index variable
 * "position" describes the current state. Each transition describes
 * what happened and contains a list of commands that describes how to
 * undo or redo it, depending on the transition's stack position
 * relative to the index. Undo and redo are implemented as movement
 * back and forward through the stack.
 *
 * Transitions are created using history_begin(), history_remember()
 * and history_commit() or history_rollback().
 */

#include <config.h>
#include <assert.h>
#include "history.h"
#include "pref.h"
#include "cmd.h"

const struct history_transition *
history_get_previous(struct history *h) {
    if(h->position < 0)
        return NULL;
    return g_ptr_array_index(h->transitions, h->position);
}

const struct history_transition *
history_get_next(struct history *h) {
    if(h->position + 1 >= h->transitions->len)
        return NULL;
    return g_ptr_array_index(h->transitions, h->position + 1);
}

int
history_get_depth(struct history *h) {
    return h->transitions->len;
}

static void
history_free_transition(struct history_transition *ht) {
    GList *l;
    for(l = ht->how; l; l = l->next)
        cmd_destroy((struct cmd *)l->data);
    g_list_free(ht->how);
    mem_free(ht);
}

static void
history_destroy_future(struct history *h) {
    while(h->stored_future->len) {
        history_free_transition(g_ptr_array_index(h->stored_future, 0));
        g_ptr_array_remove_index(h->stored_future, 0);
    }
}

static void
history_save_future(struct history *h) {
    if(h->position + 1 < h->transitions->len)
        DEBUG("saving %d transitions from history\n",
              h->transitions->len - (h->position+1));
    while(h->position + 1 < h->transitions->len) {
        g_ptr_array_add(h->stored_future, 
                        g_ptr_array_index(h->transitions, h->position + 1));
        g_ptr_array_remove_index(h->transitions, h->position + 1);
    }
}

static void
history_restore_future(struct history *h) {
    int i;
    for(i = 0; i < h->stored_future->len; i++)
        g_ptr_array_add(h->transitions, 
                        g_ptr_array_index(h->stored_future, i));
    g_ptr_array_set_size(h->stored_future, 0);
}

void
history_clear(struct history *h) {
    assert(h->depth == 0 && h->state == HISTORY_NORMAL);

    while(h->transitions->len) {
        history_free_transition(g_ptr_array_index(h->transitions, 0));
        g_ptr_array_remove_index(h->transitions, 0);
    }
    while(h->pending->len) {
        history_free_transition(g_ptr_array_index(h->pending, 0));
        g_ptr_array_remove_index(h->pending, 0);
    }
    while(h->stored_future->len) {
        history_free_transition(g_ptr_array_index(h->stored_future, 0));
        g_ptr_array_remove_index(h->stored_future, 0);
    }
    h->position = -1;
}

static void
history_print(struct history *h) {
    int i;
    GList *l;
    struct history_transition *ht;

    return;

    DEBUG("");
    for(i = 0; i < h->transitions->len; i++) {
        ht = g_ptr_array_index(h->transitions, i);
        printf("[ %s, { ", ht->what);
        for(l = ht->how; l; l = l->next) {
            cmd_print(l->data);
            if(l->next)
                printf(", ");
        }
        printf(" } ]");
        if(i == h->position)
            printf(" <--- UNDO | REDO ---> ");
        else if(i + 1 < h->transitions->len)
            printf(",");
        printf(" ");
    }
    printf("\n");
}

static struct cmd_value *
history_replay(struct history *h,
               struct history_transition *ht) {
    struct cmd *cmd;
    struct cmd_value *r = NULL;
    int success = 1;
    GList *l;

    history_begin(h, ht->what);
    for(l = ht->how; l && success; l = l->next) {
        cmd = l->data;
        DEBUG("replaying %s\n", cmd->name);
        r = cmd_do(cmd);
        if(cmd_is_error(r)) {
            FAIL("failure: %s\n", cmd_get_error_message(r));
            success = 0;
        }
        //        cmd_destroy(cmd);

        if(!success)
            break;

        cmd_destroy_value(r);
    }
    if(!success) {
        history_rollback(h);
    } else {
        for(l = ht->how; l; l = l->next)
            cmd_destroy((struct cmd *)l->data);
        ht->how = NULL;
        r = cmd_new_void_val();
        history_commit(h);
    }

    return r;
}

struct cmd_value *
history_go_back(struct history *h) {
    struct history_transition *ht;
    struct cmd_value *r;

    assert(h->state == HISTORY_NORMAL);
    
    if(h->position < 0) 
        return cmd_new_error_val("Nothing to go back to.");

    ht = g_ptr_array_index(h->transitions, h->position);

    if(!ht->committed)
        return cmd_new_error_val("Cannot undo '%s' because it is still "
                                 "being processed.", ht->what);
    
    h->state = HISTORY_GO_BACK;
    r = history_replay(h, ht);
    if(!cmd_is_error(r)) 
        h->position--;
    h->state = HISTORY_NORMAL;
    history_print(h);
    return r;
}

struct cmd_value *
history_go_forward(struct history *h) {
    struct history_transition *ht;
    struct cmd_value *r;

    assert(h->state == HISTORY_NORMAL);

    if(h->position + 1 >= h->transitions->len) 
        return cmd_new_error_val("Nothing to go forward to.");
    
    ht = g_ptr_array_index(h->transitions, h->position + 1);
    h->state = HISTORY_GO_FORWARD;
    h->position++;
    r = history_replay(h, ht);
    if(cmd_is_error(r))
        h->position--;
    h->state = HISTORY_NORMAL;
    history_print(h);
    return r;
}

/**
 * Begins a history transition. The transition is not added to the
 * history until a subsequent call to history_commit(). This function
 * can be called recursively, as long as calls to history_begin() and
 * history_commit() or history_rollback() are properly paired.
 * @param h The history to add the transition to.
 * @param what What this transition describes.
 * @return 0 on success, non-zero error code otherwise.
 */

int
history_begin(struct history *h,
              const char *what) {
    struct history_transition *ht;

    assert(h->state != HISTORY_ROLLBACK);

    ht = mem_alloc(sizeof(*ht) + strlen(what) + 1);
    if(!ht) {
        FAIL("not enough memory to begin %s\n", what);
        return 1;
    }
    ht->what = (char *)&ht[1];
    strcpy(ht->what, what);
    ht->how = NULL;
    ht->committed = 0;

    if(h->state == HISTORY_NORMAL) {
        history_save_future(h);
        g_ptr_array_add(h->transitions, ht);
        h->position = h->transitions->len - 1;
    }
    g_ptr_array_add(h->pending, ht);
    //    DEBUG("beginning %s\n", what);

    h->depth++;

    return 0;
}

/**
 * Adds a command to the pending transition previously created by
 * history_begin(). This command should reverse the effects of an
 * action.
 * @param h The history.
 * @param cmd The command to add.
 */

void
history_remember(struct history *h,
                 struct cmd *cmd) {
    struct history_transition *ht;

    /* Indicates history_begin() was not called */

    g_return_if_fail(h->pending->len > 0);
        
    if(h->state == HISTORY_ROLLBACK) {
        cmd_destroy(cmd);
        return;
    }
    ht = g_ptr_array_index(h->pending, h->pending->len - 1);
    
    ht->how = g_list_prepend(ht->how, cmd);
}

/**
 * @internal
 * Appends the pending transition to the stack.
 */

static void
history_append(struct history *h) {
    struct history_transition *ht;

    assert(h->pending->len > 0);

    ht = g_ptr_array_index(h->pending, h->pending->len - 1);
    //    DEBUG("committing (append) %s\n", ht->what);

    ht->committed = 1;
    //    ht->how = g_list_reverse(ht->how);

    /* No undo provided. */

    if(ht->how == NULL) {
        //        DEBUG("no undo for %s\n", ht->what);
        g_ptr_array_remove(h->transitions, ht);
        h->position--;
        history_free_transition(ht);

        if(h->depth == 0) 
            history_restore_future(h);

        return;
    }

    /* Destroy bottom undo if it falls below treshold. */

    if(h->transitions->len > pref_get_as_int("max_undo_depth")) {
        DEBUG("exceeded undo depth (%d), removing bottom element\n", 
              pref_get_as_int("max_undo_depth"));
        history_free_transition(g_ptr_array_index(h->transitions, 0));
        g_ptr_array_remove_index(h->transitions, 0);
        h->position--;
    }

    history_destroy_future(h);
}

/**
 * @internal
 * Replaces the current transition by the pending transition.
 */

static void
history_replace(struct history *h) {
    struct history_transition *ht, *htp;

    assert(h->pending->len > 0);

    ht = g_ptr_array_index(h->transitions, h->position);
    htp = g_ptr_array_index(h->pending, h->pending->len - 1);

    //    DEBUG("committing (replace) %s\n", htp->what);
    
    ht->how = htp->how;
    mem_free(htp);
}

/**
 * Commits the pending transition previously created by history_begin().
 * If no intervening calls to history_remember() have been made, then
 * the transition is discarded. After this call, the transition appears
 * in the history.
 * @param h The history.
 */

void
history_commit(struct history *h) {
    assert(h->pending->len > 0);

    h->depth--;

    switch(h->state) {
    case HISTORY_NORMAL:
        history_append(h);
        break;
    case HISTORY_GO_BACK:
    case HISTORY_GO_FORWARD:
        history_replace(h);
        break;
    case HISTORY_ROLLBACK:
        assert("rollback");
        break;
    }

    g_ptr_array_remove_index(h->pending, h->pending->len - 1);

    h->state = HISTORY_NORMAL;
    history_print(h);
}

/**
 * Cancels the pending transition. Executes all the commands added to the
 * transition so far and destroys the transition.
 * @param h The history.
 */

void
history_rollback(struct history *h) {
    GList *l;
    int success = 1;
    struct cmd *cmd;
    struct cmd_value *r;
    struct history_transition *ht;

    assert(h->pending->len > 0);

    h->depth--;

    ht = g_ptr_array_index(h->pending, h->pending->len - 1);
    
    DEBUG("rolling back %s\n", ht->what);
    h->state = HISTORY_ROLLBACK;

    for(l = ht->how; l && success; l = l->next) {
        cmd = l->data;
        DEBUG("rollback of component %s\n", cmd->name);
        r = cmd_do(cmd);
        if(cmd_is_error(r)) {
            FAIL("rollback failed: %s\n", cmd_get_error_message(r));
            success = 0;
        }
        cmd_destroy_value(r);
    }

    g_ptr_array_remove_index(h->pending, h->pending->len - 1);
    g_ptr_array_remove(h->transitions, ht);
    h->position--;
    history_free_transition(ht);
    history_print(h);

    if(h->depth == 0)
        history_restore_future(h);

    h->state = HISTORY_NORMAL;
}

struct history *
history_new() {
    struct history *h = mem_alloc(sizeof *h);
    if(!h)
        return NULL;
    h->transitions = g_ptr_array_new();
    h->pending = g_ptr_array_new();
    h->stored_future = g_ptr_array_new();
    h->position = -1;
    h->state = HISTORY_NORMAL;
    h->depth = 0;
    return h;
}

void
history_destroy(struct history *h) {
    assert(h->pending->len == 0);
    history_clear(h);
    mem_free(h);
}

