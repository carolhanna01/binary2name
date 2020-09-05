
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
 * The view contains all things related to the sample editing window.
 */

#include <config.h>
#include <unistd.h>
/* Has to come before gui.h */
#include <glade/glade.h>
#include "lib/misc.h"
#include <timeline.h>
#include <combo_box.h>
#include "mem.h"
#include "gui.h"
#include "shell.h"
#include "view_callbacks.h"
#include "arbiter.h"
#include "module.h"
#include "pane.h"
#include "pref.h"
#include "history.h"
#include "tool.h"
#include "dialog_props.h"


void
view_disconnect_from_clip(struct view *view,
                          struct clip *clip) {
    int i;
    
    for(i = 0; i < view->msg_count; i++) 
        msg_unsubscribe(clip->msg,
                        view->msg_ids[i]);
}

void
view_connect_to_clip(struct view *view,
                     struct clip *clip) {

    /* The message handlers ("view-track-inserted" etc) are defined in
       cmd_view.c. */

    view->msg_count = 0;
    view->msg_ids[view->msg_count++] = 
        msg_subscribe(clip->msg, 
                      "clip::track-inserted",
                      "view-track-inserted", 
                      view);
    view->msg_ids[view->msg_count++] = 
        msg_subscribe(clip->msg,
                      "clip::track-deleted",
                      "view-track-deleted",
                      view);
    view->msg_ids[view->msg_count++] = 
        msg_subscribe(clip->msg,
                      "clip::track-moved",
                      "view-track-moved",
                      view);
    view->msg_ids[view->msg_count++] = 
        msg_subscribe(clip->msg,
                      "clip::snd-changed",
                      "view-clip-snd-changed",
                      view);

}

void
view_set_default_track_draw_hooks(struct view *v,
                                  int track) {
    static struct draw_hook track_peaks = 
        { "peaks", 0, 10, draw_peaks };
    static struct draw_hook track_rms = 
        { "rms", 0, 20, draw_rms };

    draw_hooks_add_hook(v->track_hooks[track], &track_peaks);
    draw_hooks_add_hook(v->track_hooks[track], &track_rms);
}

void
view_reset_track_draw_hooks(struct view *view) {
    int i;

    for(i = 0; i < view->shl->clip->sr->channels; i++) {
        draw_hooks_remove_all(view->track_hooks[i]);
        view_set_default_track_draw_hooks(view, i);
    }

}

void
view_clip_changed(struct view *view) {
    view_connect_to_clip(view, view->shl->clip);
    view_reset_track_draw_hooks(view);
}

void
view_set_wavecanvas_auto_scroll(struct view *view,
                                gboolean enable) {
    view->wavecanvas_auto_scroll_enabled = enable ? 1 : 0;
}

void
view_attach_dialog(struct view *view,
                   GtkWidget *dialog) {
    DEBUG("attaching %p\n", dialog);
    view->dialogs = g_list_append(view->dialogs, dialog);
}

void
view_detach_dialog(struct view *view,
                   GtkWidget *dialog) {
    DEBUG("detaching %p\n", dialog);
    view->dialogs = g_list_remove(view->dialogs, dialog);
}

void
view_start_operation(struct view *view,
                     const char *operation) {
    char cancellabel[100];
    char *op = strdup(operation);
    if(!op) {
        FAIL("strdup failed\n");
        return;
    }

    g_ptr_array_add(view->opstack, op);

    view_push_status(view, "%s ...", op);
    view_set_progress(view, 0);

    snprintf(cancellabel, sizeof cancellabel, "Cancel '%s'", op);

    gtk_label_set_text(GTK_LABEL(GTK_BIN(view_get_widget(view, "cancel"))->child), cancellabel);
    gtk_widget_set_sensitive(view_get_widget(view, "cancel"), TRUE);
}

void
view_end_operation(struct view *view) {
    char cancellabel[100];

    if(!view->opstack->len) {
        FAIL("nothing to end\n");
        return;
    }

    view_set_progress(view, 0);

    view_pop_status(view);
    free(g_ptr_array_index(view->opstack, view->opstack->len-1));
    g_ptr_array_remove_index(view->opstack, view->opstack->len-1);
    
    if(view->opstack->len) {

        /* If there is still an operation on the stack, display it. */

        view_push_status(view, "%s ...", 
                         g_ptr_array_index(view->opstack, view->opstack->len-1));
        snprintf(cancellabel, sizeof cancellabel, "Cancel '%s'", 
                 (char *)g_ptr_array_index(view->opstack, view->opstack->len-1));
        gtk_label_set_text(GTK_LABEL(GTK_BIN(view_get_widget(view, "cancel"))->child), cancellabel);
    } else {

        /* Otherwise revert to inactive state. */

        view_reset_status(view);
        gtk_label_set_text(GTK_LABEL(GTK_BIN(view_get_widget(view, "cancel"))->child), "Cancel");
        gtk_widget_set_sensitive(GTK_WIDGET(view_get_widget(view, "cancel")), FALSE);
    }
    
    /* Force cancel button resize by hiding then showing again. */

    gtk_widget_hide(view_get_widget(view, "cancel"));
    gtk_widget_show(view_get_widget(view, "cancel"));
    view_redraw(view);
}

void
view_setup_history_menu_items(struct view *view,
                              const char *which,
                              const char *template,
                              const struct history_transition *ht) {
    GtkWidget *w = view_get_widget(view, which);
    char label[4096];
    gboolean sensitive;

    if(!ht) {
        sensitive = FALSE;
        snprintf(label, sizeof label, template, "");
    } else {
        sensitive = TRUE;
        snprintf(label, sizeof label, template, ht->what);
        sym2words(label);
    }
    gtk_widget_set_sensitive(w, sensitive);
    gtk_label_set_text(GTK_LABEL(GTK_BIN(w)->child), label);
}

void
view_sync_menus(struct view *view) {
    const struct history_transition *ht;

    ht = history_get_previous(view->shl->history);
    view_setup_history_menu_items(view, "undo", "Undo %s", ht);
    ht = history_get_next(view->shl->history);
    //    DEBUG("history_get_next: %p\n", (void *)ht);
    view_setup_history_menu_items(view, "redo", "Redo %s", ht);
}

struct view_wavecanvas_motion_notify {
    struct view *view;
    GdkEventMotion *event;
};

/**
 * Timeout callback which emits a synthetic motion-notify event
 * if required.
 */

static gboolean
view_wavecanvas_emit_motion_notify(gpointer data) {
    struct view_wavecanvas_motion_notify *motion_notify = data;
    struct view *view = motion_notify->view;
    GdkEvent *event = (GdkEvent *)motion_notify->event;

    if(view->wavecanvas_emit_motion_notify) {
        
        arbiter_queue_cmd(CMD_NEW("on-wavecanvas-motion-notify",
                                  cmd_new_shellp_val(view->shl),
                                  cmd_new_GtkObjectp_val(GTK_OBJECT(view_get_widget(view, "wavecanvas"))),
                                  cmd_new_GdkEventp_val_with_dtor(event, cmd_GdkEventp_dtor)));

    } else {

        gdk_threads_enter();
        gdk_event_free((GdkEvent *)motion_notify->event);
        gdk_threads_leave();

    }

    mem_free(motion_notify);

    view->wavecanvas_motion_notify_pending = 0;

    return FALSE;
}

/**
 * Determines whether a motion-notify event has occurred outside of
 * the wavecanvas. If it has, the canvas is scrolled as appropriate,
 * and a timeout callback is installed to emit further motion-notify
 * events. This way the canvas keeps scrolling automatically.
 *
 * Since motion-notify events outside of the wavecanvas can only occur
 * when the pointer has been grabbed, and the pointer is grabbed by a
 * button-press event, the net result is that dragging motions by the
 * user cause the wavecanvas to automatically scroll the invisible
 * area into view.
 *
 * Setting view->wavecanvas_auto_scroll to 0 disables auto scroll
 * completely.
 *
 * @param view The view.
 * @param event The motion-notify event.
 */

void
view_wavecanvas_process_motion_notify(struct view *view,
                                      GdkEventMotion *event) {
    int width, height, abs_x, abs_y, delta, x_root;
    AFframecount hpos;
    GtkWidget *canvas;
    struct view_wavecanvas_motion_notify *motion_notify;

    if(!view->wavecanvas_auto_scroll_enabled)
        return;

    canvas = view_get_widget(view, "wavecanvas");
    x_root = event->x_root;

    /* See if the motion-notify event has occurred outside the
       canvas. */
    
    gdk_window_get_size(canvas->window, &width, &height);
    gdk_window_get_origin(canvas->window, &abs_x, &abs_y);
    delta = x_root - abs_x;

    if(delta > 0 && delta - width < 0) {

        /* No need to scroll. */

        view->wavecanvas_emit_motion_notify = 0;
        return;
    }

    if(delta > 0)
        delta -= width;
    
    view->last_mouse_x_root = x_root;
    
    /* Establish the horizontal scrollbar position which exposes
       the invisible area. */
    
    hpos = CLAMP(view->hadjust->value + ((delta / 4) * view->hres),
                 0,
                 (snd_frame_count(view->shl->clip->sr, MAP_ALL) - 
                  view->hres * width));
    
    /* No need to waste time if there's nothing new to expose. */
    
    if(view->hadjust->value == hpos)
        return;
    
    view_set_hpos(view, hpos);
    gtk_widget_queue_draw(canvas);
    //    gdk_window_process_updates(canvas->window, TRUE);
    
    /* Start emitting motion events. */
    
    view->wavecanvas_emit_motion_notify = 1;
    
    /* Don't try to emit a new motion-notify event if one is
       pending. */
    
    if(!view->wavecanvas_motion_notify_pending) {
        
        motion_notify = mem_alloc(sizeof(*motion_notify));
        if(!motion_notify) {
            FAIL("cannot allocate motion event\n");
            return;
        }
        
        motion_notify->event = (GdkEventMotion *)gdk_event_copy((GdkEvent *)event);
        motion_notify->view = view;
        
        view->wavecanvas_motion_notify_pending = 1;
        g_timeout_add(25, view_wavecanvas_emit_motion_notify, 
                      motion_notify);
        
    }
}

void
view_show(struct view *view) {
    gtk_widget_show(view_get_widget(view, "shell"));
    view_sync_display(view);
}


/**
 * Displays a message to the user in an unobtrusive way.
 * This function is thread-safe, but the message is not shown until the
 * next call to view_redraw() (which is not thread-safe).
 *
 * @param view The view.
 * @param level The importance of the message (see MSG_INFO, MSG_WARN, 
 * MSG_ERROR)
 * @param fmt The format string.
 */

void
view_set_transient(struct view *view,
                   int level,
                   const char *fmt,
                   ...) {
    va_list ap;
    va_start(ap, fmt);
    view->transient_level = level;
    if(view->transient_info)
        free(view->transient_info);
    view->transient_info = mem_alloc(256);
    if(view->transient_info) 
        vsnprintf(view->transient_info, 256, fmt, ap);
    va_end(ap);
    //    gdk_threads_enter();
    //    gtk_widget_queue_draw(view_get_widget(view, "infocanvas"));
    //    gdk_threads_leave();
}

/**
 * Clears a message previously set by view_set_transient().
 *
 * @param view The view.
 */

void
view_clear_transient(struct view *view) {
    if(view->transient_info)
        free(view->transient_info);
    view->transient_info = NULL;
    //    gdk_threads_enter();
    //    gtk_widget_queue_draw(view_get_widget(view, "infocanvas"));
    //    gdk_threads_enter();
}


void
view_set_progress(struct view *view,
                  float fract) {
    static float pulse __attribute__ ((unused)) = 0;
    if(fract > 1) {
#ifdef HAVE_GNOME2
        gtk_progress_bar_set_pulse_step(GTK_PROGRESS_BAR(view_get_widget(view, "progress")), .03);
        gtk_progress_bar_pulse(GTK_PROGRESS_BAR(view_get_widget(view, "progress")));
#else
        gtk_progress_set_activity_mode(GTK_PROGRESS(view_get_widget(view, "progress")), TRUE);
        gtk_progress_bar_set_activity_step(GTK_PROGRESS_BAR(view_get_widget(view, "progress")), 2);
        gtk_progress_set_value(GTK_PROGRESS(view_get_widget(view, "progress")), pulse);
        pulse += .5;
        if(pulse > 1)
            pulse = 0;
#endif
    } else {

#ifdef HAVE_GNOME2
        gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(view_get_widget(view, "progress")), CLAMP(fract, 0, 1));        
#else
        gtk_progress_set_activity_mode(GTK_PROGRESS(view_get_widget(view, "progress")), FALSE);
        gtk_progress_bar_set_fraction(GTK_PROGRESS(view_get_widget(view, "progress")), CLAMP(fract, 0, 1));
#endif
    }
}

void
view_disable_all_tool_buttons(void *key,
                              void *value,
                              void *user_data) {
    struct view *view = user_data;
    char wname[256];
    GtkWidget *w;
    snprintf(wname, sizeof wname, "tool_button_%s", (char *)key);
    w = view_get_widget(view, wname);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), FALSE);
}

void
view_activate_tool(struct view *view,
                   const char *name) {
    char wname[256];
    struct tool *tool = shell_get_tool(view->shl, name);
    GtkWidget *w;
    GtkWidget *tool_panel = tool_get_panel(tool);

    snprintf(wname, sizeof wname, "tool_button_%s", name);

    g_hash_table_foreach(view->shl->tools, 
                         view_disable_all_tool_buttons,
                         view);
    
    w = view_get_widget(view, wname);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), TRUE);
    gtk_widget_queue_draw(view_get_widget(view, "wavecanvas"));
    
    if(!tool_panel)
        tool_panel = gtk_label_new("No options");

    if(view->last_tool_page != -1)
        gtk_notebook_remove_page(GTK_NOTEBOOK(view_get_widget(view, "panels")), -1);

    snprintf(wname, sizeof wname, "%s Tool", tool->displayname);

    gtk_notebook_append_page(GTK_NOTEBOOK(view_get_widget(view, "panels")),
                             tool_panel,
                             gtk_label_new(wname));
    view->last_tool_page = 1;
}

void
view_sync_grid_display(struct view *view) {
    struct grid *grid = shell_get_grid(view->shl);

    g_return_if_fail(view->shl->clip->sr);

    /* Reset invalid unit entry. */
    
    if(grid->units != 
       gtk_spin_button_get_value(GTK_SPIN_BUTTON(view_get_widget(view, "grid_units"))))
        gtk_spin_button_set_value(GTK_SPIN_BUTTON(view_get_widget(view, "grid_units")),
                                  grid->units);

    //gtk_spin_button_set_value(GTK_SPIN_BUTTON(view_get_widget(view, "grid_bpm")), grid->bpm);

    if(grid->measurement == GRID_BEATS) {
        gtk_widget_show(view_get_widget(view, "grid_bpm"));
        gtk_widget_show(view_get_widget(view, "grid_bpm_label"));
    } else {
        gtk_widget_hide(view_get_widget(view, "grid_bpm"));
        gtk_widget_hide(view_get_widget(view, "grid_bpm_label"));
    }
    combo_box_set_active(COMBO_BOX(view_get_widget(view, "grid_measurement")), grid->measurement);
}

void 
view_set_vpos(struct view *view,
              double value) {

    g_return_if_fail(view->shl->clip->sr != NULL);

    view->vadjust->lower = 0;
    view->vadjust->upper = view->shl->clip->sr->channels;
    view->vadjust->page_size = 1;
    view->vadjust->step_increment = view->vadjust->page_increment = 1;
    view->vadjust->value = CLAMP(value, 0, view->shl->clip->sr->channels);
    gtk_adjustment_changed(view->vadjust);
    gtk_adjustment_value_changed(view->vadjust);
}

void 
view_set_hpos_internal(struct view *view,
                       AFframecount value) {

    g_return_if_fail(view->shl->clip->sr != NULL);

    view->hadjust->upper = snd_frame_count(view->shl->clip->sr, MAP_ALL);
    view->hadjust->lower = 0;
    view->hadjust->step_increment = view->hres < 1 ? 1 : view->hres * 10;
    view->hadjust->page_size = 
        MIN(view->hadjust->upper,
            floor(view_get_widget(view, "wavecanvas")->allocation.width *
                  view->hres));
    view->hadjust->page_increment = 
        MIN(view->hadjust->upper, 
            MAX(0, view->hadjust->page_size - view->hres));

    /*
     * Always make sure that we start at a view->hres boundary,
     * otherwise you get very annoying aliasing effects when drawing
     * the waveform.
     */

    if(view->hres > 1) 
        value -= ((AFframecount)value % (int)view->hres);

    view->hadjust->value = 
        /*
        CLAMP(value,
              view->hadjust->lower,
              view->hadjust->upper);
        */
        CLAMP((rint(value) + view->hadjust->page_size > view->hadjust->upper ? 
               view->hadjust->upper - view->hadjust->page_increment : 
               rint(value)),
              view->hadjust->lower,
              view->hadjust->upper);
    view_update_timeline(view);
}

void
view_set_hpos(struct view *view,
              AFframecount value) {
    view_set_hpos_internal(view, value);
    gtk_adjustment_changed(view->hadjust);
    gtk_adjustment_value_changed(view->hadjust);
}

void
view_center_hrange(struct view *view,
                   AFframecount start,
                   AFframecount end) {
    AFframecount delta = end - start;
    view->hadjust->value = 
        CLAMP(start, view->hadjust->lower, view->hadjust->upper);
    
    if(delta < view_get_widget(view, "wavecanvas")->allocation.width * 
       view->hres)
        view->hadjust->value = 
            MAX(0, view->hadjust->value -
                (view_get_widget(view, "wavecanvas")->allocation.width * 
                 view->hres - delta) / 2);


    view_set_hpos(view, view->hadjust->value);
}

void 
view_update_timeline(struct view *view) {
    GtkWidget *timeline;
    gdouble units_per_pixel;
    gdouble mark_interval, mark_factor, label_interval, label_factor;
    
    g_return_if_fail(view->shl->clip->sr != NULL);

    timeline = view_get_widget(view, "timeline");

    switch(shell_get_grid(view->shl)->measurement) {
    case GRID_SECONDS:
        label_interval = view->shl->clip->sr->rate;
        label_factor = 2;
        mark_interval = view->shl->clip->sr->rate / 10;
        mark_factor = 10;

        if(view->hres < 256) {
            label_interval = view->shl->clip->sr->rate / 4;
            label_factor = 2;
            mark_interval = view->shl->clip->sr->rate / 40;
            mark_factor = 4;
        }
        /*

        if(view->hres < 64) {
            label_interval = view->shl->clip->sr->rate / 100;
            label_factor = 1;
            mark_interval = view->shl->clip->sr->rate / 400;
            mark_factor = 10;
        }
        */
        if(view->hres < 4) {
            label_interval = view->shl->clip->sr->rate / 1000;
            label_factor = 1;
            mark_interval = view->shl->clip->sr->rate / 10000;
            mark_factor = 100;
        }

        break;
    case GRID_BEATS:
        label_interval = grid_get_frames_per_beat(shell_get_grid(view->shl));
        label_factor = 2;
        mark_interval = grid_get_frames_per_beat(shell_get_grid(view->shl)) / 32;
        mark_factor = 4;
        break;
    case GRID_FRAMES:
        label_interval = 1;
        label_factor = 100;
        mark_interval = 1;
        mark_factor = 10;

        if(view->hres < 4) {
            label_interval = 1;
            label_factor = 4;
            mark_interval = 1;
            mark_factor = 1;
        }
        break;
    }
        
    units_per_pixel = view->hres;

    timeline_configure(TIMELINE(timeline),
                       units_per_pixel,
                       view->hadjust->value,
                       view->hadjust->value + (timeline->allocation.width *
                                               view->hres),
                       mark_interval,
                       mark_factor,
                       label_interval,
                       label_factor);
    //    gtk_widget_queue_draw(timeline);
    //    gdk_window_process_updates(timeline->window, TRUE);

}

void
view_set_hres(struct view *view,
              float zoom) {
    AFframecount oldpos = PIXEL_TO_FRAME(view, view_get_widget(view, "wavecanvas")->allocation.width / 2);
    if(zoom < HRES_MIN)
        zoom = HRES_MIN;
    if(zoom > HRES_MAX)
        zoom = HRES_MAX;
    view->hres = zoom;
    oldpos = MAX(oldpos, 0);
    view_center_hrange(view, oldpos, oldpos);
    //    shell_status_default_set(shl);
    view_reset_status(view);
    view_redraw(view);
}

void
view_set_vres(struct view *view,
              int zoom) {
    if(zoom < 16)
        zoom = 16;
    if(zoom > GRAPH_BITS_VRES)
        zoom = GRAPH_BITS_VRES;
    DEBUG("setting zoom %d\n", zoom);
    view->vres = zoom;
    gtk_widget_queue_draw(view_get_widget(view, "mixercanvas"));
    view_sync_display(view);
    view_redraw(view);
}

void 
view_set_cursor(struct view *view,
                GdkCursorType cursor) {
    GtkWidget *w = view_get_widget(view, "wavecanvas");
    if(w && w->window)
        gui_window_set_cursor(w->window, cursor);
}

GdkCursorType 
view_get_default_cursor(struct view *view) {
    return view->default_cursor;
}

void
view_set_default_cursor(struct view *view,
                        GdkCursorType cursor) {
    view->default_cursor = cursor;
    view_set_cursor(view, cursor);
}

void
view_pop_status(struct view *view) {
    gnome_appbar_pop(GNOME_APPBAR(view_get_widget(view, "appbar")));
}

void
view_push_status(struct view *view,
                 const char *format, 
                 ...) {
    char status[4096];
    va_list ap;
    
    va_start(ap, format);
    vsnprintf(status, 4096, format, ap);
    va_end(ap);
    
    gnome_appbar_push(GNOME_APPBAR(view_get_widget(view, "appbar")), status);
}

void
view_reset_status(struct view *view) {
    char freq[20];
    char fileformat[100];
    char driverformat[80];
    char *filename;

    g_return_if_fail(view->shl->clip->sr != NULL);
    
    gtk_label_set_text(GTK_LABEL(view_get_widget(view, "filesize")),
                       printable_byte_count(snd_frame_count(view->shl->clip->sr, MAP_ALL) *
                                            view->shl->clip->sr->channels *
                                            sample_get_width(view->shl->clip->sr->sample_type)));
    gtk_label_set_text(GTK_LABEL(view_get_widget(view, "sampleformat")),
                       sample_get_description(view->shl->clip->sr->sample_type));

    if(view->shl->file->driver) { 
        view->shl->file->driver->snprint(view->shl->file,
                                         FILE_DETAILED_FORMAT,
                                         driverformat, 
                                         sizeof driverformat);
        snprintf(fileformat, sizeof fileformat, "%s (via %s)", 
                 driverformat, view->shl->file->driver->name);
    } else {
        snprintf(fileformat, sizeof fileformat, "(not saved)");
    }
    
    gtk_label_set_text(GTK_LABEL(view_get_widget(view, "fileformat")),
                       fileformat);

    snprintf(freq, sizeof freq, "%d HZ", (int)view->shl->clip->sr->rate);
    gtk_label_set_text(GTK_LABEL(view_get_widget(view, "samplefrequency")),
                       freq);
    
    view_pop_status(view);
    filename = g_path_get_basename(view->shl->file->name);
    view_push_status(view,
                     "%s  %.2f sec.  %ld frames  zoom %d:%d", 
                     filename,
                     snd_frames_to_time(view->shl->clip->sr,
                                        snd_frame_count(view->shl->clip->sr, MAP_ALL)), 
                     snd_frame_count(view->shl->clip->sr, MAP_ALL),
                     (int)(view->hres < 1 ? ((float)1 / view->hres) : 1),
                     (int)(view->hres < 1 ? 1 : view->hres));
    if(filename)
        free(filename);

    //    shell_properties_size_labels_set(shl);
}

void
view_redraw(struct view *view) {

    g_return_if_fail(view->shl->clip->sr != NULL);
    
    view_update_timeline(view);
    gtk_widget_queue_draw(view_get_widget(view, "wavecanvas"));
    //    gdk_window_process_updates(view_get_widget(view, "wavecanvas")->window,
    //                               TRUE);
}

/*
 * Zoom step function, determines the (normalized) zoom step size 
 * from the (normalized) current zoom setting.
 */

double
view_hres_step_size_func(double x) {

    /* Quadratic curve from [0..1] to [0..1]. */

    double k = (x*2)-1;
    double l = k * k;
    double y = -(l)+1;
    return y;
}

float
view_get_zoom_step(float current_zoom) {
    double a = view_hres_step_size_func(current_zoom / HRES_MAX);
    float r = MAX(PEAK_HRES, (int)(a * ZOOM_STEP_MAX));

    /*
     * Make sure step is divisible by PEAK_HRES.
     */

    r = (int)r & ~((int)PEAK_HRES-1);
    return r;
}

void
view_zoom_in(struct view *view) {
    float zoom = view->hres;
    if(zoom > PEAK_HRES) 
        zoom = zoom - view_get_zoom_step(zoom);
    else
        zoom = view->hres - (view->hres / 2);

    view_set_hres(view, zoom);
}

void
view_zoom_out(struct view *view) {
    float zoom = view->hres;
    if(zoom < PEAK_HRES)
        zoom = view->hres + view->hres;
    else 
        zoom = zoom + view_get_zoom_step(zoom);
            
    view_set_hres(view, zoom);
}

void
view_scroll_hcenter(struct view *view,
                    AFframecount from,
                    AFframecount to) {
    double ratios[] = { .05, .15, .4, .25, .125 };
    int i, sign = (from - to) > 0 ? 1 : -1;
    struct timespec ts = { 0, 50000 };
    AFframecount delta = MAX(from, to) - MIN(from, to);
    AFframecount pos = from;
    AFframecount amount;
    if(view->scrolling)
        return;
    view->scrolling = 1;
    for(i = 0; i < sizeof ratios / sizeof ratios[0]; i++) {
        amount = ratios[i] * delta;
        pos -= sign * amount;
        view_center_hrange(view, pos, pos);
        arbiter_yield();
        nanosleep(&ts, NULL);
    }
    view->scrolling = 0;
    view_center_hrange(view, to, to);
}

#define HANDLE(widget, signal) \
 { #widget, #signal, view ## _ ## widget ## _ ## signal }

#define HANDLE_WITH(widget, signal, handler) \
 { #widget, #signal, view ## _ ## handler ## _ ## signal }

static struct pane_signal_bindings bindings[] = {

    /* Window events. */

    HANDLE(shell, size_allocate),
    HANDLE(shell, delete_event),
    HANDLE(shell, focus_in_event),
    HANDLE(shell, focus_out_event),
    HANDLE(shell, destroy),

    /* Waveform DrawingArea events. */

    HANDLE(wavecanvas, expose_event),
    HANDLE(wavecanvas, configure_event),
    HANDLE(wavecanvas, key_press_event),
    HANDLE(wavecanvas, key_release_event),
    HANDLE(wavecanvas, button_press_event),
    HANDLE(wavecanvas, button_release_event),
    HANDLE(wavecanvas, motion_notify_event),
    HANDLE(wavecanvas, enter_notify_event),
    HANDLE(wavecanvas, leave_notify_event),
    HANDLE(wavecanvas, visibility_notify_event),

#ifdef HAVE_GNOME2
    HANDLE(wavecanvas, scroll_event),
#endif

    /* Info DrawingArea events. */

    HANDLE(infocanvas, expose_event),
    HANDLE(infocanvas, button_press_event),
    HANDLE(infocanvas, size_request),

    /* Mixer DrawingArea events. */

    HANDLE(mixercanvas, expose_event),
    HANDLE(mixercanvas, configure_event),
    HANDLE(mixercanvas, button_press_event),
    HANDLE(mixercanvas, button_release_event),
    HANDLE(mixercanvas, motion_notify_event),

    /* Grid. */

#ifndef HAVE_GNOME2
    HANDLE(grid_units, changed),
    HANDLE(grid_bpm, changed),
#else
    HANDLE(grid_units, value_changed),
    HANDLE(grid_bpm, value_changed),
#endif

    /* File menu. */

    HANDLE(new, activate),
    HANDLE(open, activate),
    HANDLE(save, activate),
    HANDLE(saveas, activate),
    HANDLE(mixdown, activate),
    HANDLE(close, activate),
    HANDLE(exit, activate),

    /* Edit menu. */

    HANDLE(undo, activate),
    HANDLE(redo, activate),
    HANDLE(cut, activate),
    HANDLE(paste, activate),
    HANDLE(paste_fit, activate),
    HANDLE(paste_over, activate),
    HANDLE(paste_mix, activate),
    HANDLE(copy, activate),
    HANDLE(clear, activate),
    HANDLE(erase, activate),
    HANDLE(crop, activate),
    HANDLE(insert_tracks, activate),
    HANDLE(delete_tracks, activate),
    HANDLE(push_left, activate),
    HANDLE(push_right, activate),
    HANDLE(shove_left, activate),
    HANDLE(shove_right, activate),
    HANDLE(properties, activate),
    HANDLE(show_clipboard, activate),

    /* Markers menu. */
    
    HANDLE(insert_cuepoint, activate),
    HANDLE(delete_cuepoints, activate),
    HANDLE(copy_cuepoints, activate),
    HANDLE(paste_cuepoints, activate),
    HANDLE(insert_envelope, activate),
    HANDLE(delete_envelopes, activate),
    HANDLE(copy_envelopes, activate),
    HANDLE(paste_envelopes, activate),
    HANDLE(invert_envelopes, activate),
    HANDLE(delete_time, activate),
    HANDLE(insert_time, activate),
    HANDLE(disable_envelopes_toggle, activate),

    /* View menu. */

    HANDLE_WITH(16_1, activate, zoom),
    HANDLE_WITH(8_1, activate, zoom),
    HANDLE_WITH(4_1, activate, zoom),
    HANDLE_WITH(2_1, activate, zoom),
    HANDLE_WITH(1_1, activate, zoom),
    HANDLE_WITH(1_2, activate, zoom),
    HANDLE_WITH(1_3, activate, zoom),
    HANDLE_WITH(1_4, activate, zoom),
    HANDLE_WITH(1_5, activate, zoom),
    HANDLE_WITH(1_6, activate, zoom),
    HANDLE_WITH(1_7, activate, zoom),
    HANDLE_WITH(1_8, activate, zoom),
    HANDLE_WITH(1_9, activate, zoom),
    HANDLE_WITH(1_10, activate, zoom),
    HANDLE(zoom_in, activate),
    HANDLE(zoom_out, activate),
    HANDLE(smaller, activate),
    HANDLE(bigger, activate),
    HANDLE(show_zero_toggle, activate),
    HANDLE(show_grid_toggle, activate),

    /* Select menu. */
    
    HANDLE(left, activate),
    HANDLE(right, activate),
    HANDLE(track_up, activate),
    HANDLE(track_down, activate),
    HANDLE(left_select, activate),
    HANDLE(right_select, activate),
    HANDLE(track_up_select, activate),
    HANDLE(track_down_select, activate),
    HANDLE(left_nudge, activate),
    HANDLE(right_nudge, activate),
    HANDLE(left_jump, activate),
    HANDLE(right_jump, activate),
    HANDLE(select_all, activate),
    HANDLE(selection_to_loop, activate),
    HANDLE(loop_to_selection, activate),
    HANDLE(fit_selection_to_window, activate),
    HANDLE(center_on_selection_start, activate),
    HANDLE(center_on_selection_end, activate),
    HANDLE(selection_to_4_beats, activate),
    HANDLE(snap_to_cuepoints_toggle, activate),
    HANDLE(snap_to_grid_toggle, activate),

    /* Playback menu. */

    HANDLE(play, activate),
    HANDLE(cue_play, activate),
    HANDLE(record, activate),
    HANDLE(scrub_left, activate),
    HANDLE(scrub_right, activate),
    HANDLE(scrub_left_fast, activate),
    HANDLE(scrub_right_fast, activate),
    HANDLE(loop_toggle, activate),
    HANDLE(follow_playback_toggle, activate),
    HANDLE(record_replace_toggle, activate),

    /* Player toolbar buttons. */

    HANDLE(button_rwd, button_press_event),
    HANDLE(button_rwd, button_release_event),
    HANDLE(button_ffwd, button_press_event),
    HANDLE(button_ffwd, button_release_event),
    HANDLE(button_stop, clicked),
    HANDLE(button_record, clicked),
    HANDLE(button_play, clicked),
    HANDLE(button_cueplay, clicked),

    /* Settings menu. */

    HANDLE(preferences, activate),

    /* Debug menu. */

    HANDLE(dump_blocks, activate),
    HANDLE(dump_sound_info, activate),
    HANDLE(dump_mixer, activate),
    HANDLE(destroy_undo, activate),
    HANDLE(draw_blocks_toggle, activate),
    HANDLE(connect_samples_toggle, activate),
    HANDLE(draw_regionlock_toggle, activate),
    HANDLE(step_mode_toggle, activate),
    HANDLE(join_blocks, activate),
    HANDLE(fail_next_allocation_toggle, activate),
    HANDLE(force_sigsegv, activate),

};

void
view_disable_widgets(struct view *view,
                     const char *names[]) {
    int i;
    
    for(i = 0; names[i]; i++) 
        gtk_widget_set_sensitive(view_get_widget(view, names[i]), FALSE);
}

void
view_connect_others(struct view *view) {

    /* Adjustment signals. */

    view->hadjust = 
        gtk_range_get_adjustment(GTK_RANGE(view_get_widget(view, 
                                                           "hscrollbar")));
    view->vadjust =
        gtk_range_get_adjustment(GTK_RANGE(view_get_widget(view, 
                                                           "vscrollbar")));
    
    g_signal_connect(GTK_OBJECT(view->hadjust), "value_changed",
                     G_CALLBACK(view_hadjust_value_changed), view);
    g_signal_connect(GTK_OBJECT(view->vadjust), "value_changed",
                     G_CALLBACK(view_vadjust_value_changed), view);

    /* Connect grid combobox signal. */

    g_signal_connect(G_OBJECT(view_get_widget(view, "grid_measurement")), 
                     "changed",
                     G_CALLBACK(view_grid_measurement_changed), 
                     view);

    /* Connect cancel button. */

    g_signal_connect(GTK_OBJECT(view_get_widget(view, "cancel")), "clicked",
                     G_CALLBACK(view_cancel_activate), view);
    

}

GtkWidget *
view_get_widget(struct view *view,
                const char *name) {
    return pane_get_widget(view->pane, name);
}

/**
 * Creates a tool button for the specified tool and connects it's
 * signal handler and the draw hooks.
 */

void
view_bind_tool(struct view *view,
               struct tool *tool) {
    static GtkTooltips *tooltips = NULL;
    char button_name[256];
    char toolbar_name[40];
    char tooltip[4096], *accel_label;
    GtkWidget *tool_buttons;
    GtkWidget *button;
    GdkPixmap *icon_pixmap; 
    GdkBitmap *icon_mask;
    GtkWidget *icon;
    static GtkAccelGroup *accel_group;

    if(accel_group == NULL) {
        accel_group = gtk_accel_group_new ();
        gtk_window_add_accel_group(GTK_WINDOW(view_get_widget(view, "shell")),
                                   accel_group);
    }
    
    button = gtk_toggle_button_new();

    gtk_widget_add_accelerator(button, "clicked", accel_group, tool->accel_key,
                               tool->accel_mods, GTK_ACCEL_VISIBLE);

    tool_get_icon(tool, &icon_pixmap, &icon_mask);
    icon = gtk_image_new_from_pixmap(icon_pixmap, icon_mask);

    snprintf(button_name, sizeof button_name, "tool_button_%s", tool->name);
    snprintf(toolbar_name, sizeof toolbar_name, "tool_buttons_%d",
             (view->tool_count++ / 2) + 1);
    tool_buttons = view_get_widget(view, toolbar_name);
    pane_register_widget(view->pane, button_name, button);
    gtk_button_set_relief(GTK_BUTTON(button), GTK_RELIEF_NORMAL);

    gtk_container_add(GTK_CONTAINER(button), icon);
    gtk_widget_show(button);
    gtk_widget_show(icon);
    gtk_widget_set_name(button, button_name);
    g_signal_connect(GTK_OBJECT(button), "toggled",
                     G_CALLBACK(view_tool_toggled), view);    
    gtk_container_add(GTK_CONTAINER(tool_buttons), button);
    //    gtk_widget_unref(icon);

    if(!tooltips)
        tooltips = gtk_tooltips_new();

    accel_label = gtk_accelerator_name(tool->accel_key, tool->accel_mods);

    snprintf(tooltip, sizeof tooltip, "%s [%s]", tool->tooltip, accel_label);

    gtk_tooltips_set_tip(tooltips, button, tooltip, NULL);

    draw_hooks_add_hook(view->wavecanvas_hooks, 
                        &(tool->funcs->draw_hook));

}

void
view_destroy(struct view *view) {
    GList *dialogs = NULL, *l;

    view_disconnect_from_clip(view, view->shl->clip);

    if(view->wavepixmap)
        gdk_pixmap_unref(view->wavepixmap);
    if(view->peaks_low)
        mem_free(view->peaks_low);
    if(view->rms)
        mem_free(view->rms);

    for(l = view->dialogs; l; l = l->next)
        dialogs = g_list_append(dialogs, l->data);

    for(l = dialogs; l; l = l->next)
        gtk_widget_destroy(GTK_WIDGET(l->data));

    draw_hooks_destroy(view->wavecanvas_hooks);
    pane_destroy(view->pane);
    mem_free(view);
}

void 
view_setup_help_menu(struct view *view) {
    static GnomeUIInfo help_menu[] = {
        GNOMEUIINFO_HELP("gnusound"),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_MENU_ABOUT_ITEM(view_about_activate, "About GNUsound"),
        GNOMEUIINFO_END
    };

    GtkMenu *help = GTK_MENU(gtk_menu_new());
    gnome_app_fill_menu(GTK_MENU_SHELL(help), help_menu, 
                        GNOME_APP(view_get_widget(view, 
                                                  "shell"))->accel_group, 
                        1, 0);
    gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_get_widget(view, "help")),
                              GTK_WIDGET(help));
    
}

void
view_timeline_generate_label(gdouble value,
                             char *s,
                             size_t n,
                             void *user_data) {
    struct view *view = user_data;
    grid_format(shell_get_grid(view->shl),
                (double)value, s, n, 
                (view->hres <= 128 ? 
                 GRID_FORMAT_LONG : GRID_FORMAT_SHORT));
}


void
view_setup_timeline(struct view *view) {
    GtkTable *canvas_table = GTK_TABLE(view_get_widget(view, "canvas_table"));
    GtkWidget *w = timeline_new();
    gtk_widget_show(w);
    gtk_table_attach(canvas_table,
                     w, 
                     1, 2, /* left, right */
                     0, 1, /* top, bottom */
                     GTK_EXPAND | GTK_FILL, 0, /* xoptions, yoptions */
                     0, 0); /* xpad, ypad */
    timeline_set_label_generator(TIMELINE(w),
                                 view_timeline_generate_label,
                                 view);
    pane_register_widget(view->pane, "timeline", GTK_WIDGET(w));
}

void
view_setup_grid_measurement(struct view *view) {
    GtkTable *grid_table = GTK_TABLE(view_get_widget(view, "grid_table"));
    GtkWidget *w = combo_box_new();
    GList *l = NULL;

    l = g_list_append(l, "Frames");
    l = g_list_append(l, "Seconds");
    l = g_list_append(l, "Beats");
    combo_box_set_strings(COMBO_BOX(w), l);
    combo_box_set_editable(COMBO_BOX(w), FALSE);

    gtk_widget_show(w);
    gtk_table_attach(grid_table,
                     w, 
                     1, 2, /* left, right */
                     0, 1, /* top, bottom */
                     GTK_EXPAND | GTK_FILL, 0, /* xoptions, yoptions */
                     0, 0); /* xpad, ypad */

    pane_register_widget(view->pane, "grid_measurement", GTK_WIDGET(w));
}

void
view_setup_cancel_button(struct view *view) {
    GtkWidget *w = gtk_button_new_with_label("Cancel");
    gtk_widget_show(w);
    gtk_widget_set_sensitive(w, FALSE);
    gtk_box_pack_end(GTK_BOX(view_get_widget(view, "appbar")), 
                     w,
                     FALSE,
                     FALSE,
                     0);
    pane_register_widget(view->pane, "cancel", w);
}

void
view_setup_modules(struct view *view) {
    int i;
    GtkMenu *tools;
    GtkMenuItem *item;
    struct gnusound_module *module;

    /* Setup the tools menu. */

    tools = GTK_MENU(gtk_menu_new());
    gtk_menu_set_title(tools, "Tools");
    
    item = GTK_MENU_ITEM(gtk_tearoff_menu_item_new());
    gtk_widget_show(GTK_WIDGET(item));
    gtk_menu_shell_append(GTK_MENU_SHELL(tools), GTK_WIDGET(item));

#if 0
    /* Cancel item. */

    item = GTK_MENU_ITEM(gtk_menu_item_new_with_label("Cancel"));
    gtk_menu_shell_append(GTK_MENU_SHELL(tools), GTK_WIDGET(item));
    gtk_widget_show(GTK_WIDGET(item));
    gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
#ifndef HAVE_GNOME2
    gtk_widget_add_accelerator(GTK_WIDGET(item),
                               "activate",
                               gtk_accel_group_get_default(),
                               GDK_period,
                               GDK_CONTROL_MASK,
                               GTK_ACCEL_VISIBLE);
#endif
    g_signal_connect(GTK_OBJECT(item),
                     "activate",
                     G_CALLBACK(view_module_cancel_activate),
                     view);
    pane_register_widget(view->pane, "cancelmodule", GTK_WIDGET(item));

    /* Separator. */

    item = GTK_MENU_ITEM(gtk_menu_item_new());
    gtk_menu_shell_append(GTK_MENU_SHELL(tools), GTK_WIDGET(item));
    gtk_widget_show(GTK_WIDGET(item));
#endif
    
    /* Modules. */

    for(i = 0; i < module_get_count(); i++) {

        module = module_get(i);

        /* Don't put faceless modules in the menu. */

        if(module->flags & MODULE_FLAG_FACELESS)
            continue;
        
        item = GTK_MENU_ITEM(gtk_menu_item_new_with_label(module->name));
        
        g_object_set_data(G_OBJECT(item), "user_data", GINT_TO_POINTER(i));
        gtk_menu_shell_append(GTK_MENU_SHELL(tools), GTK_WIDGET(item));
        gtk_widget_show(GTK_WIDGET(item));
        g_signal_connect(GTK_OBJECT(item), 
                         "activate",
                         G_CALLBACK(view_module_activate), 
                         view);
    }
    gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_get_widget(view, "tools")),
                              GTK_WIDGET(tools));
    
}

#define SYNC_TOGGLE(name) gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(view_get_widget(view, name "_toggle")), pref_get_as_int(name));

void
view_sync_display(struct view *view) {
    char title[400];
    char *filename;
    
    filename = g_path_get_basename(view->shl->file->name);
    if(filename) {
        snprintf(title, sizeof title, "%s%s%s", 
                 view->shl->has_changed ? "*" : "",
                 filename,
                 (strlen(filename) >= sizeof title - 5) ? "..." : "");
        gtk_window_set_title(GTK_WINDOW(view_get_widget(view, "shell")),
                             title);
        free(filename);
    }

    SYNC_TOGGLE("show_zero");
    SYNC_TOGGLE("record_replace");
    SYNC_TOGGLE("follow_playback");
    SYNC_TOGGLE("show_grid");
    SYNC_TOGGLE("snap_to_grid");
    SYNC_TOGGLE("snap_to_cuepoints");
    
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(view_get_widget(view, "grid_bpm")), 
                              shell_get_grid(view->shl)->bpm);

    view_set_hpos(view, view->hadjust->value);
    view_set_vpos(view, view->vadjust->value);
    view_sync_menus(view);
    view_sync_grid_display(view);

    /* Show/hide the vertical scrollbar. */
    
    if(view->vadjust->value != 0 ||
       (view->shl->clip->sr->channels * view->vres) +
       (view->shl->clip->sr->channels * TRACK_Y_SPACING) > 
       view_get_widget(view, "wavecanvas")->allocation.height)
        gtk_widget_show(view_get_widget(view, "vscrollbar"));
    else 
        gtk_widget_hide(view_get_widget(view, "vscrollbar"));

}

void
view_fixup(struct view *view) {
#ifndef HAVE_GNOME2
    GdkEventConfigure cfg; 
#endif

#ifdef HAVE_GNOME2
    /* Some massaging needed for GNOME2. */
    gtk_widget_set_redraw_on_allocate(view_get_widget(view, "wavecanvas"), 
                                      FALSE);

    /* We perform our own buffering for the sample canvas. */
    gtk_widget_set_double_buffered(view_get_widget(view, "wavecanvas"),
                                   FALSE);

    /* Annoyingly libglade-2 doesn't seem to pick these up from the
       config file, or maybe it's a bug elsewhere. */
    gtk_widget_add_events(view_get_widget(view, "wavecanvas"), 
                          GDK_BUTTON_PRESS_MASK | 
                          GDK_BUTTON_RELEASE_MASK | 
                          GDK_POINTER_MOTION_MASK |
                          GDK_SCROLL_MASK);
    gtk_widget_add_events(view_get_widget(view, "mixercanvas"),
                          GDK_BUTTON_PRESS_MASK | 
                          GDK_BUTTON_RELEASE_MASK | 
                          GDK_POINTER_MOTION_MASK);
                          
#endif

#ifndef HAVE_GNOME2
    
    /* Not picked up by libglade. */

    gtk_toolbar_set_style(GTK_TOOLBAR(view_get_widget(view, "toolbar_player")), 
                          GTK_TOOLBAR_ICONS);
    
    /* Explicitly send another configure event because we might miss
       the first, I think. */

    cfg.type = GDK_CONFIGURE;
    cfg.window = view_get_widget(view, "shell")->window;
    cfg.send_event = 0;
    cfg.x = 0;
    cfg.y = 0;
    cfg.width = 500;
    cfg.height = 500;
    gtk_main_do_event((GdkEvent *)&cfg);
#endif
}

void
view_connect_draw_hooks(struct view *v) {
    int i;
    static struct draw_hook background = 
        { "background", 0, 0, draw_background };
    static struct draw_hook zero = 
        { "zeroline", 0, 10, draw_zero };
    static struct draw_hook grid = 
        { "grid", 0, 20, draw_grid };
    static struct draw_hook tracks = 
        { "tracks", 0, 30, draw_tracks };
    static struct draw_hook pointer = 
        { "pointer", 0, 40, draw_pointer };
    static struct draw_hook loop = 
        { "loop", 0, 50, draw_loop };
    static struct draw_hook cursor = 
        { "cursor", 0, 60, draw_cursor };
    static struct draw_hook blocks = 
        { "blocks", 0, 60, draw_blocks };
    static struct draw_hook constraints = 
        { "constraints", 0, 60, draw_constraints };
    static struct draw_hook trackdividers = 
        { "trackdividers", 0, 25, draw_track_dividers };
    
    draw_hooks_add_hook(v->wavecanvas_hooks, &background);
    draw_hooks_add_hook(v->wavecanvas_hooks, &trackdividers);
    draw_hooks_add_hook(v->wavecanvas_hooks, &zero);
    draw_hooks_add_hook(v->wavecanvas_hooks, &grid);
    draw_hooks_add_hook(v->wavecanvas_hooks, &tracks);
    draw_hooks_add_hook(v->wavecanvas_hooks, &pointer);
    draw_hooks_add_hook(v->wavecanvas_hooks, &cursor);
    draw_hooks_add_hook(v->wavecanvas_hooks, &loop);
    draw_hooks_add_hook(v->wavecanvas_hooks, &blocks);
    draw_hooks_add_hook(v->wavecanvas_hooks, &constraints);

    view_reset_track_draw_hooks(v);
}

void
view_register_clip_(struct view *view) {
    int i;
    struct cmd_signature f[] = {
        
        
    };


    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

}

struct view *
view_new(shell *shl) {
    int i;
    struct view *v = mem_calloc(sizeof(struct view), 1);
    GladeXML *glade_xml;

    if(!v)
        return NULL;

    glade_xml = gui_get_xml(GLADE_FILE, "shell");
    if(!glade_xml) {
        mem_free(v);
        return NULL;
    }
    
    v->pane = pane_new(glade_xml);
    if(!glade_xml) {
        mem_free(v);
        return NULL;
    }

    v->wavecanvas_hooks = draw_hooks_new();
    if(!v->wavecanvas_hooks) {
        pane_destroy(v->pane);
        mem_free(v);
        return NULL;
    }

    for(i = 0; i < MAX_TRACKS; i++) {
        v->track_hooks[i] = draw_hooks_new();
        if(!v->track_hooks[i]) {
            for(--i; i >= 0; i--)
                draw_hooks_destroy(v->track_hooks[i]);
            draw_hooks_destroy(v->wavecanvas_hooks);
            pane_destroy(v->pane);
            mem_free(v);
            return NULL;
        }
    }

    v->shl = shl;
    v->hres = PEAK_HRES;
    v->vres = 128;
    v->transient_info = NULL;
    v->follow_playback = pref_get_as_int("follow_playback");
    v->show_grid = pref_get_as_int("show_grid");
    v->show_zero = pref_get_as_int("show_zero");
    v->draw_blocks = 0;
    v->draw_regionlock = 0;
    v->last_tool_page = -1;
    v->opstack = g_ptr_array_new();
    v->tool_count = 0;
    v->dialogs = NULL;
    v->dialog_props = NULL;
    v->target_channel_being_dragged = -1;
    v->source_channel_being_dragged = -1;
    v->wavecanvas_emit_motion_notify = 0;
    v->wavecanvas_motion_notify_pending = 0;
    v->wavecanvas_auto_scroll_enabled = 1;
    v->mixerpixmap = NULL;
    v->wavepixmap = NULL;
    v->peaks_low = NULL;
    v->peaks_high = NULL;
    v->rms = NULL;

    /* Insert our custom timeline widget. */

    view_setup_timeline(v);

    /* Insert grid measurement. */

    view_setup_grid_measurement(v);

    /* Setup Help menu. */

    view_setup_help_menu(v);

    /* Setup modules. */

    view_setup_modules(v);

    view_setup_cancel_button(v);

    /* Connect the signals for the widgets we can access through
       libglade. */

    pane_connect_bindings(v->pane, 
                          sizeof(bindings) / sizeof(bindings[0]),
                          bindings, 
                          v);

    /* Connect other signals. */

    view_connect_others(v);

    pane_register_widget(v->pane, "progress", GTK_WIDGET(gnome_appbar_get_progress(GNOME_APPBAR(view_get_widget(v, "appbar")))));
    
    /* Show/hide debug menu. */

#ifndef DEBUG_FLAG
    gtk_widget_hide(GTK_WIDGET(debug_trigger));
#endif

    /* Connect draw hooks. */

    view_connect_draw_hooks(v);

    /* Connect messages. */

    view_connect_to_clip(v, v->shl->clip);

    /* Perform final fixups. */
    
    view_fixup(v);

    view_reset_status(v);


    return v;
}
