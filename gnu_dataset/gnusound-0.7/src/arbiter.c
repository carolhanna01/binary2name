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
 * The arbiter is the backbone of the program. It maintains
 * global state. The most important piece of global state is the task
 * queue. Commands can be pushed onto the task queue via 
 * arbiter_queue_cmd(). The task queue is polled from the GTK+ event
 * loop. If the task queue contains commands to run, then the arbiter
 * takes the first command off the queue and executes it. 
 */

#include <config.h>
#include <gtk/gtk.h>
#include "tool.h"
#include "shell.h"
#include "marker.h"
#include "gui.h"
#include "dialog.h"
#include "dialog_pref.h"
#include "arbiter.h"

struct arbiter ad = { 0, };

/*
 * Initialized by GetCpuCaps, this contains a set of CPU capabilities
 * (e.g. it's type and whether it supports MMX and such). Needs to be a 
 * global to avoid major surgery on the MPlayer cpudetect code.
 */

extern CpuCaps gCpuCaps;

/**
 * Yields control to the GTK event handler to process outstanding GUI
 * events such as redraws and button clicks. This function returns 1
 * if this processing resulted in the user selecting "quit".
 *
 * @return 0 for success, 1 if the program should quit.
 */

int
arbiter_yield() {
    
    while(gtk_events_pending())
        gtk_main_iteration_do(FALSE);
    
    return arbiter_is_quit_requested();
}

/**
 * Places a command onto the task queue. This function is thread-safe.
 * @param cmd The command to put into the task queue.
 */

void
arbiter_queue_cmd(struct cmd *cmd) {
    g_static_mutex_lock(&ad.cmd_queue_lock);
    ad.cmd_queue = g_list_append(ad.cmd_queue, cmd);
    g_static_mutex_unlock(&ad.cmd_queue_lock);
}


struct marker_list_array *
arbiter_get_cuepoints_clipboard() {
    return ad.cuepoints_clipboard;
}

struct marker_list_array *
arbiter_get_envelopes_clipboard() {
    return ad.envelope_clipboard;
}

int
arbiter_is_quit_requested() {
    return ad.quit_requested;
}

/**
 * Requests that the application be terminated at the earliest
 * possible opportunity.
 */

void
arbiter_request_quit() {
    ad.quit_requested = 1;
}

/**
 * Returns a shell according to some criterium.
 * @param select Function pointer of the selector function. This function
 * is called for every shell, and should return 1 if the shell satisfies
 * the criterium or 0 if it doesn't.
 * @param user_data Pointer passed to the select function.
 * @return The shell that was found or NULL.
 */

shell *
arbiter_find_shell(int (*select)(shell *shl,
                                 void *user_data),
                   void *user_data) {
    GList *l;

    for(l = ad.shells; l; l = l->next) 
        if(select((shell *)l->data, user_data))
            return (shell *)l->data;

    return NULL;
}

/**
 * Calls a function for each shell.
 * @param callback The function to call.
 * @param user_data Data to pass to the callback.
 */

void
arbiter_foreach_shell(void (*callback)(shell *shl, 
                                       void *user_data),
                      void *user_data) {
    GList *l;
    shell *shl;

    for(l = ad.shells; l; ) {
        /* This way we can walk the list of shells even if it gets
           changed by the callback . */
        shl = l->data;
        l = l->next;
        callback(shl, user_data);
    }
}

GList *
arbiter_get_shells() {
    return ad.shells;
}

/**
 * @internal
 */

void
arbiter_add_shell(shell *shl) {
    ad.shells = g_list_append(ad.shells, shl);
}

/** 
 * @internal
 */

void
arbiter_remove_shell(shell *shl) {
    ad.shells = g_list_remove(ad.shells, shl);
    DEBUG("shells: %d\n", g_list_length(ad.shells));
    if(!g_list_length(ad.shells))
        arbiter_request_quit();
}

CpuCaps *
arbiter_get_cpu_caps() {
    return &gCpuCaps;
}


gboolean
#ifdef HAVE_GNOME2
arbiter_event_source_prepare(GSource *source,
                             gint *timeout)
#else
arbiter_event_source_prepare(gpointer source_data,
                             GTimeVal *current_time,
                             gint *timeout,
                             gpointer user_data)
#endif
{
    gboolean ready;

    g_static_mutex_lock(&ad.cmd_queue_lock);
    ready = g_list_first(ad.cmd_queue) ? TRUE : FALSE;
    g_static_mutex_unlock(&ad.cmd_queue_lock);
    *timeout = 5000;

    return ready;
}

gboolean
#ifdef HAVE_GNOME2
arbiter_event_source_check(GSource *source)
#else
arbiter_event_source_check(gpointer source_data,
                           GTimeVal *current_time,
                           gpointer user_data)
#endif
{
    gboolean ready;

    g_static_mutex_lock(&ad.cmd_queue_lock);
    ready = g_list_first(ad.cmd_queue) ? TRUE : FALSE;
    g_static_mutex_unlock(&ad.cmd_queue_lock);

    return ready;
}

/**
 * Main program loop.
 */

gboolean
#ifdef HAVE_GNOME2
arbiter_event_source_dispatch(GSource *source,
                              GSourceFunc callback,
                              gpointer user_data)
#else
arbiter_event_source_dispatch(gpointer source_data,
                              GTimeVal *current_time,
                              gpointer user_data)
#endif
{
    struct cmd *cmd;
    struct cmd_value *r;

    gdk_threads_enter();

    while((cmd = g_list_nth_data(ad.cmd_queue, 0))) {
        ad.busy++;

        g_static_mutex_lock(&ad.cmd_queue_lock);
        ad.cmd_queue = g_list_remove(ad.cmd_queue, cmd);
        g_static_mutex_unlock(&ad.cmd_queue_lock);

        //        DEBUG("executing '%s'\n", cmd->name);
        r = cmd_do(cmd);

        if(!r) {

            FAIL("execution failure\n");

        } else if(cmd_is_error(r)) {

            gdk_pointer_ungrab(GDK_CURRENT_TIME);
            gui_alert("Error: %s\n\n[%s]",
                      cmd_get_error_message(r), cmd->name);
            FAIL("runtime error during %s: %s\n", 
                 cmd->name, cmd_get_error_message(r));

        }

        cmd_destroy_value(r);
        cmd_destroy(cmd);
        //        DEBUG("finished '%s'\n", cmd->name);

        ad.busy--;
    }

    if(g_list_length(ad.shells) == 0)
        arbiter_request_quit();

    if(ad.busy == 0 && 
       arbiter_is_quit_requested() && 
       g_list_length(ad.shells) == 0) 
        gtk_main_quit();
    
    gdk_threads_leave();

    return TRUE;
}

void
#ifdef HAVE_GNOME2
arbiter_event_source_destroy(GSource *source) 
#else
arbiter_event_source_destroy(gpointer data) 
#endif
{
    DEBUG("destroyed\n");
}

void
arbiter_exit() {
}

int
arbiter_init() {
    static GSourceFuncs arbiter_event_source_funcs = {
        arbiter_event_source_prepare,
        arbiter_event_source_check,
        arbiter_event_source_dispatch,
        arbiter_event_source_destroy
#ifdef HAVE_GNOME2
        , NULL, NULL
#endif
    };
#ifdef HAVE_GNOME2
    GSource *src;
#endif

    memset(&gCpuCaps, '\0', sizeof(gCpuCaps));
#ifdef RUNTIME_CPUDETECT
    GetCpuCaps(&gCpuCaps);
#endif
    memset(&ad, '\0', sizeof(ad));

    g_static_mutex_init(&ad.cmd_queue_lock);
    ad.cmd_queue = NULL;
    ad.shells = NULL;

    ad.cuepoints_clipboard = marker_list_array_new(0);
    ad.envelope_clipboard = marker_list_array_new(0);

#ifdef HAVE_GNOME2
    src = g_source_new(&arbiter_event_source_funcs,
                       sizeof(GSource));
    g_source_set_priority(src, G_PRIORITY_DEFAULT);
    g_source_set_can_recurse(src, TRUE);
    g_source_attach(src, NULL);
#else
    g_source_add(G_PRIORITY_DEFAULT,
                 TRUE,
                 &arbiter_event_source_funcs,
                 NULL,
                 NULL,
                 NULL);
#endif

    return 0;
}

/**
 * The program entry point.
 * @return Program exit code.
 */

int 
arbiter_run(int argc,
            char *argv[]) {
    int i;
    struct file *file;
    
    if(argc >= 2) {
        for(i = 1; i < argc; i++) {
            file = file_new(argv[i]);
            if(file) {
                arbiter_queue_cmd(CMD_NEW("open-file", 
                                          cmd_new_filep_val(file)));
            } else {
                FAIL("couldn't create file object! quitting for safety\n");
                return 1;
            }
        }
    } else {
        arbiter_queue_cmd(CMD_NEW0("new-document"));
    }

    gdk_threads_enter();
    gtk_main();
    gdk_threads_leave();
    return 0;
}

