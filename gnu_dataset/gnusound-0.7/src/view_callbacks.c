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
#include <gnome.h>
#include <combo_box.h>
#include "lib/misc.h"
#include "gui.h"
#include "mem.h"
#include "tool.h"
#include "draw.h"
#include "shell.h"
#include "arbiter.h"
#include "grid.h"
#include "pref.h"
#include "module.h"
#include "dialog_props.h"

extern snd *clipboard;
extern shell *clipboard_shell;

/* Modules. */

void
view_cancel_activate(GtkButton *button,
                     struct view *view) {
    view->shl->cancel_requested = 1;
}

void 
view_module_activate(GtkMenuItem *menuitem,
                     struct view *view) {
    int id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menuitem), 
                                               "user_data"));
    
    shell_dispatch_as(view->shl,
                      CMD_NEW("activate-module",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_int_val(id)),
                      module_get(id)->name);
}


/* Tool buttons. */

void
view_tool_toggled(GtkWidget *w,
                  struct view *view) {
    static int ignore = 0;
    const char *name = gtk_widget_get_name(w);

    if(ignore)
        return;

    ignore = 1;    
    shell_activate_tool(view->shl, name + strlen("tool_button_"));
    ignore = 0;
}

/* About dialog. */

void
view_about_activate(GtkWidget *w,
                    struct view *view) {
    GtkWidget *about;
    const gchar *authors[] = {
        "Pascal Haakmat",
        NULL
    };
#ifdef HAVE_GNOME2
    const gchar *documenters[] = {
        NULL
    };
    const gchar *translator_credits = NULL;
#endif
    char *logo_filename = findfile(LOGO_SEARCH_PATH, LOGO_FILE, R_OK);
    const gchar *comments = 
        _("GNOME U sound editor.\n" 
          "GNUsound comes with ABSOLUTELY NO WARRANTY.\n"
          "This is free software, and you are welcome to redistribute it\n"
          "under certain conditions.\n \n"
          "http://awacs.dhs.org/software/gnusound");
          
#ifdef HAVE_GNOME2
    static GdkPixbuf *logo_pixbuf = NULL;
    GError *err = NULL;

    if(!logo_pixbuf && logo_filename) {
        logo_pixbuf = gdk_pixbuf_new_from_file(logo_filename, &err);
        free(logo_filename);
    }
    
#endif

    DEBUG("about_activate...\n");
    about = gnome_about_new("GNUsound",
                            VERSION,
                            "Copyright (C) 2002-2005 Pascal Haakmat.",
#ifdef HAVE_GNOME2
                            comments,
                            authors,
                            documenters,
                            translator_credits,
                            logo_pixbuf
#else
                            authors,
                            comments,
                            logo_filename
#endif
                            );
    gtk_widget_show(about);
}

/* Debug menu. */

void
view_destroy_undo_activate(GtkWidget *w,
                           struct view *view) {
    
    /*
    undo_destroy(view->shl->undo_stack);
    view->shl->undo_stack = NULL;
    */
}

void
view_dump_mixer_activate(GtkWidget *w,
                         struct view *view) {
    mixer_dump(view->shl->clip->mixer);
}

void
view_dump_blocks_activate(GtkWidget *w,
                          struct view *view) {
    shell *shl = view->shl;
    int i;
    if(!shl->clip->sr)
        return;

    for(i = 0; i < shl->clip->sr->channels; i++) {
        DEBUG("blocks for track %d\n", i);
        blocklist_dump(shl->clip->sr->tracks[i]->bl);
    }
}

void
view_dump_sound_info_activate(GtkWidget *w,
                              struct view *view) {
    shell *shl = view->shl;

    if(!shl->clip->sr)
        return;

    snd_info_dump(shl->clip->sr);
}

void
view_join_blocks_activate(GtkWidget *w,
                           struct view *view) {
    //    int i;
    //    if(!shl->clip->sr)
    //        return;
    DEBUG("not enabled\n");
    /*
    rwlock_rlock(&shl->clip->sr->rwl);
    for(i = 0; i < shl->clip->sr->channels; i++)
        track_compact(shl->clip->sr->tracks[i]);
    rwlock_runlock(&shl->clip->sr->rwl);
    */
}

static void 
do_sigsegv() {
    char *p = 0x0;
    *p = '0';
}

void
view_force_sigsegv_activate(GtkWidget *w,
                            struct view *view) {
    do_sigsegv();
}

void
view_fail_next_allocation_toggle_activate(GtkWidget *w,
                                           struct view *view) {
    //    mem_fail_allocation_on_zero = mem_fail_allocation_on_zero > 0 ? 0 : 1;
    //    DEBUG("mem_fail_allocation_on_zero: %d\n",
    //          mem_fail_allocation_on_zero);
}

void
view_step_mode_toggle_activate(GtkWidget *w,
                                  struct view *view) {
    /*    if(shl->debug_flags & DEBUG_FLAG_STEP_MODE)
        shl->debug_flags &= ~DEBUG_FLAG_STEP_MODE;
    else
        shl->debug_flags |= DEBUG_FLAG_STEP_MODE;
    */
    view_redraw(view);
}

void
view_draw_blocks_toggle_activate(GtkWidget *w,
                                  struct view *view) {
    view->draw_blocks = !view->draw_blocks;
    view_redraw(view);
}

void
view_connect_samples_toggle_activate(GtkWidget *w,
                                     struct view *view) {
    pref_set_int("view.wave.draw.connect_samples",
                 !pref_get_as_int("view.wave.draw.connect_samples"));
    view_redraw(view);
}

void
view_draw_regionlock_toggle_activate(GtkWidget *w,
                                     struct view *view) {
    view->draw_regionlock = !view->draw_regionlock;
    view_redraw(view);
}

/* Settings menu. */

void 
view_preferences_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    dialog_open(gui_get_dialog("preferences"));
}

/* Playback menu. */

void
view_play_activate(GtkMenuItem *menuitem,
                   struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("toggle-player", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_stop_activate(GtkMenuItem *menuitem,
                   struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("stop-player", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_cue_play_activate(GtkMenuItem *menuitem,
                        struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("start-player-from-cue", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_record_activate(GtkMenuItem *menuitem,
                     struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("record",
                                      cmd_new_shellp_val(view->shl)));
}

void
view_scrub_right_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    arbiter_queue_cmd(CMD_NEW("scrub-right",
                              cmd_new_shellp_val(view->shl)));
}

void
view_scrub_left_activate(GtkMenuItem *menuitem,
                         struct view *view) {
    arbiter_queue_cmd(CMD_NEW("scrub-left",
                              cmd_new_shellp_val(view->shl)));
}

void
view_scrub_right_fast_activate(GtkMenuItem *menuitem,
                               struct view *view) {
    arbiter_queue_cmd(CMD_NEW("scrub-right-fast",
                              cmd_new_shellp_val(view->shl)));
}

void
view_scrub_left_fast_activate(GtkMenuItem *menuitem,
                              struct view *view) {
    arbiter_queue_cmd(CMD_NEW("scrub-left-fast",
                              cmd_new_shellp_val(view->shl)));
}

void
view_loop_toggle_activate(GtkCheckMenuItem *menuitem,
                          struct view *view) {
    view->shl->loop = menuitem->active ? 1 : 0;
    view_redraw(view);
}

void
view_record_replace_toggle_activate(GtkCheckMenuItem *menuitem,
                                    struct view *view) {
    view->shl->record_replace = menuitem->active ? 1 : 0;
    pref_set_int("record_replace", view->shl->record_replace ? 1 : 0);
}

void
view_follow_playback_toggle_activate(GtkCheckMenuItem *menuitem,
                                     struct view *view) {
    view->follow_playback = menuitem->active ? 1 : 0;
    pref_set_int("follow_playback", view->follow_playback ? 1 : 0);
}

/* Player toolbar buttons */

gint
view_keep_scrubbing_left(gpointer userdata) {
    struct view *view = userdata;
    if(view->scrubbing) {
        arbiter_queue_cmd(CMD_NEW("scrub-left", 
                                  cmd_new_shellp_val(view->shl)));
        return TRUE;
    }
    return FALSE;
}

gint
view_keep_scrubbing_right(gpointer userdata) {
    struct view *view = userdata;
    if(view->scrubbing) {
        arbiter_queue_cmd(CMD_NEW("scrub-right",
                                  cmd_new_shellp_val(view->shl)));
        return TRUE;
    }
    return FALSE;
}

void
view_button_rwd_button_press_event(GtkWidget *widget,
                                   GdkEventButton *event,
                                   struct view *view) {
    view->scrubbing = 1;
    g_timeout_add(50, view_keep_scrubbing_left, view);
    arbiter_queue_cmd(CMD_NEW("scrub-left",
                              cmd_new_shellp_val(view->shl)));
}

void
view_button_rwd_button_release_event(GtkWidget *widget,
                                     GdkEventButton *event,
                                     struct view *view) {
    view->scrubbing = 0;
}

void
view_button_ffwd_button_press_event(GtkWidget *widget,
                                    GdkEventButton *event,
                                    struct view *view) {
    view->scrubbing = 1;
    g_timeout_add(50, view_keep_scrubbing_right, view);
    arbiter_queue_cmd(CMD_NEW("scrub-right",
                              cmd_new_shellp_val(view->shl)));
}

void
view_button_ffwd_button_release_event(GtkWidget *widget,
                                      GdkEventButton *event,
                                      struct view *view) {
    view->scrubbing = 0;
}

void
view_button_stop_clicked(GtkWidget *widget,
                         struct view *view) {
    DEBUG("stop\n");
    shell_dispatch(view->shl, CMD_NEW("stop-player", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_button_record_clicked(GtkWidget *widget,
                            struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("record",
                                      cmd_new_shellp_val(view->shl)));
}

void
view_button_play_clicked(GtkWidget *widget,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("start-player", 
                                      cmd_new_shellp_val(view->shl),
                                      cmd_new_long_val(view->shl->select_start)));
}

void
view_button_cueplay_clicked(GtkWidget *widget,
                             struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("start-player-from-cue", 
                                      cmd_new_shellp_val(view->shl)));
}

/* Select menu. */

void
view_left_activate(GtkMenuItem *menuitem,
                   struct view *view) {

    /* Using arbiter_queue_cmd() to circumvent history. */

    arbiter_queue_cmd(CMD_NEW("move-left", 
                              cmd_new_shellp_val(view->shl)));
}

void
view_right_activate(GtkMenuItem *menuitem,
                    struct view *view) {

    /* Using arbiter_queue_cmd() to circumvent history. */

    arbiter_queue_cmd(CMD_NEW("move-right", 
                              cmd_new_shellp_val(view->shl)));
}

void
view_track_down_activate(GtkMenuItem *menuitem,
                         struct view *view) {

    /* Using arbiter_queue_cmd() to circumvent history. */

    arbiter_queue_cmd(CMD_NEW("move-down", 
                              cmd_new_shellp_val(view->shl)));
}

void
view_track_up_activate(GtkMenuItem *menuitem,
                       struct view *view) {

    /* Using arbiter_queue_cmd() to circumvent history. */

    arbiter_queue_cmd(CMD_NEW("move-up", 
                              cmd_new_shellp_val(view->shl)));

}

void
view_left_select_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("grow-left", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_right_select_activate(GtkMenuItem *menuitem,
                           struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("grow-right", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_track_down_select_activate(GtkMenuItem *menuitem,
                                struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("grow-down", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_track_up_select_activate(GtkMenuItem *menuitem,
                              struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("grow-up", 
                                      cmd_new_shellp_val(view->shl)));
}


void
view_left_nudge_activate(GtkMenuItem *menuitem,
                         struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("nudge-left", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_right_nudge_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("nudge-right", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_left_jump_activate(GtkMenuItem *menuitem,
                        struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("jump-left", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_right_jump_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("jump-right", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_select_all_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("select-all", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_selection_to_loop_activate(GtkMenuItem *menuitem,
                                 struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("selection-to-loop", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_loop_to_selection_activate(GtkMenuItem *menuitem,
                                 struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("loop-to-selection", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_selection_to_4_beats_activate(GtkMenuItem *menuitem,
                                   struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("selection-to-4-beats",
                                      cmd_new_shellp_val(view->shl)));
}

void
view_snap_to_cuepoints_toggle_activate(GtkCheckMenuItem *menuitem,
                                       struct view *view) {
    view->shl->snap_to_cuepoints = menuitem->active ? 1 : 0;
    pref_set_int("snap_to_cuepoints", view->shl->snap_to_cuepoints ? 1 : 0);
}

void
view_snap_to_grid_toggle_activate(GtkCheckMenuItem *menuitem,
                                   struct view *view) {
    view->shl->snap_to_grid = menuitem->active ? 1 : 0;
    pref_set_int("snap_to_grid", view->shl->snap_to_grid ? 1 : 0);
}

/* View menu. */

static float
view_parse_hres_spec(const char *s) {
    int numer, denom;

    while(*s != ':') s++;
    while(*s != ' ') s--;
    denom = atoi(s);
    while(*s != ':') s++;
    numer = atoi(++s);
    return (float)numer / denom;
}

void
view_zoom_activate(GtkWidget *w,
                   struct view *view) {
    const char *s;

    s = gtk_label_get_text(GTK_LABEL(GTK_BIN(w)->child));
    arbiter_queue_cmd(CMD_NEW("set-zoom", 
                              cmd_new_shellp_val(view->shl),
                              cmd_new_double_val(view_parse_hres_spec(s))));
    
}

void
view_zoom_in_activate(GtkWidget *w,
                      struct view *view) {
    arbiter_queue_cmd(CMD_NEW("zoom-in", cmd_new_shellp_val(view->shl))); 
}

void
view_zoom_out_activate(GtkWidget *w,
                       struct view *view) {
    arbiter_queue_cmd(CMD_NEW("zoom-out", cmd_new_shellp_val(view->shl))); 
}

void
view_smaller_activate(GtkWidget *w,
                      struct view *view) {
    arbiter_queue_cmd(CMD_NEW("smaller", cmd_new_shellp_val(view->shl))); 
}

void
view_bigger_activate(GtkWidget *w,
                     struct view *view) {
    arbiter_queue_cmd(CMD_NEW("bigger", cmd_new_shellp_val(view->shl))); 
}

void
view_fit_selection_to_window_activate(GtkMenuItem *menuitem,
                                      struct view *view) {
    arbiter_queue_cmd(CMD_NEW("fit-selection", 
                              cmd_new_shellp_val(view->shl)));
}

void
view_center_on_selection_start_activate(GtkMenuItem *menuitem,
                                         struct view *view) {
    arbiter_queue_cmd(CMD_NEW("center-selection-start", 
                              cmd_new_shellp_val(view->shl)));
    
}

void
view_center_on_selection_end_activate(GtkMenuItem *menuitem,
                                       struct view *view) {
    arbiter_queue_cmd(CMD_NEW("center-selection-end", 
                              cmd_new_shellp_val(view->shl)));
}


void
view_show_zero_toggle_activate(GtkMenuItem *menuitem,
                               struct view *view) {
    pref_set_int("show_zero", view->show_zero ? 0 : 1);
    view->show_zero = pref_get_as_int("show_zero");
    view_redraw(view);
}

void
view_show_grid_toggle_activate(GtkCheckMenuItem *menuitem,
                               struct view *view) {
    pref_set_int("show_grid", view->show_grid ? 0 : 1);
    view->show_grid = pref_get_as_int("show_grid");
    view_redraw(view);
}

/* Marker menu. */

void 
view_insert_envelope_activate(GtkMenuItem *menuitem,
                              struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("insert-envelopes", 
                                      cmd_new_shellp_val(view->shl)));

}

void
view_copy_envelopes_activate(GtkMenuItem *menuitem,
                             struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("copy-envelopes", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_paste_envelopes_activate(GtkMenuItem *menuitem,
                              struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("paste-envelopes", 
                                      cmd_new_shellp_val(view->shl)));

}

void
view_invert_envelopes_activate(GtkMenuItem *menuitem,
                               struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("invert-envelopes", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_delete_envelopes_activate(GtkMenuItem *menuitem,
                               struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("delete-envelopes", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_insert_cuepoint_activate(GtkMenuItem *menuitem,
                              struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("insert-cuepoints", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_copy_cuepoints_activate(GtkMenuItem *menuitem,
                             struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("copy-cuepoints", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_paste_cuepoints_activate(GtkMenuItem *menuitem,
                              struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("paste-cuepoints", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_delete_cuepoints_activate(GtkMenuItem *menuitem,
                               struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("delete-cuepoints", 
                                      cmd_new_shellp_val(view->shl)));

}

void
view_delete_time_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("delete-time", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_insert_time_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("insert-time", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_disable_envelopes_toggle_activate(GtkCheckMenuItem *menuitem,
                                       struct view *view) {
    int i;
    int mask = menuitem->active ? 0 : MARKER_SLOPE | MARKER_SLOPE_AUX;
    
    for(i = 0; i < view->shl->clip->markers->len; i++) 
        view->shl->clip->markers->lists[i]->marker_types_enabled = mask;
        
    view_redraw(view);
}

/* Edit menu. */

void 
view_undo_activate(GtkMenuItem *menuitem,
                   struct view *view) {
    arbiter_queue_cmd(CMD_NEW("undo", cmd_new_shellp_val(view->shl)));
}

void 
view_redo_activate(GtkMenuItem *menuitem,
                   struct view *view) {
    arbiter_queue_cmd(CMD_NEW("redo", cmd_new_shellp_val(view->shl)));
}

void 
view_copy_activate(GtkMenuItem *menuitem,
                   struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("copy", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_paste_activate(GtkMenuItem *menuitem,
                    struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("paste", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_paste_fit_activate(GtkMenuItem *menuitem,
                        struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("paste-fit", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_paste_mix_activate(GtkMenuItem *menuitem,
                        struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("paste-mix", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_paste_over_activate(GtkMenuItem *menuitem,
                         struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("paste-over", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_insert_tracks_activate(GtkMenuItem *menuitem,
                            struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("insert-tracks",
                                      cmd_new_shellp_val(view->shl)));

}

void 
view_delete_tracks_activate(GtkMenuItem *menuitem,
                            struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("delete-tracks",
                                      cmd_new_shellp_val(view->shl)));
}

void
view_cut_activate(GtkMenuItem *menuitem,
                  struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("cut", cmd_new_shellp_val(view->shl)));
}

void 
view_clear_activate(GtkMenuItem *menuitem,
                    struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("clear", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_erase_activate(GtkMenuItem *menuitem,
                    struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("erase", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_crop_activate(GtkMenuItem *menuitem,
                   struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("crop", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_push_left_activate(GtkMenuItem *menuitem,
                        struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("push-left", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_push_right_activate(GtkMenuItem *menuitem,
                        struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("push-right", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_shove_left_activate(GtkMenuItem *menuitem,
                         struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("shove-left", 
                                      cmd_new_shellp_val(view->shl)));
}

void 
view_shove_right_activate(GtkMenuItem *menuitem,
                          struct view *view) {
    shell_dispatch(view->shl, CMD_NEW("shove-right", 
                                      cmd_new_shellp_val(view->shl)));
}

void
view_properties_activate(GtkMenuItem *menuitem,
                         struct view *view) {
    if(!view->dialog_props) 
        view->dialog_props = dialog_props_new(view->shl);

    if(view->dialog_props)
        dialog_open(view->dialog_props);
}

void 
view_show_clipboard_activate(GtkMenuItem *menuitem,
                             struct view *view) {
    DEBUG("show clipboard\n");
    arbiter_queue_cmd(CMD_NEW0("show-clipboard"));
}

/* File menu. */

void
view_new_activate(GtkWidget *w,
                  struct view *view) {
    arbiter_queue_cmd(CMD_NEW0("new-document"));
}

void
view_open_activate(GtkWidget *w,
                   struct view *view) {
    arbiter_queue_cmd(CMD_NEW0("select-file-and-open"));
}

void
view_mixdown_activate(GtkWidget *w,
                      struct view *view) {
    shell_dispatch(view->shl,
                   CMD_NEW("select-file-and-mixdown", 
                           cmd_new_shellp_val(view->shl)));
}

void
view_saveas_activate(GtkWidget *w,
                     struct view *view) {
    shell_dispatch(view->shl,
                   CMD_NEW("select-file-and-save", 
                           cmd_new_shellp_val(view->shl)));
}

void
view_save_activate(GtkWidget *w,
                   struct view *view) {
    shell_dispatch(view->shl,
                   CMD_NEW("save-document", 
                           cmd_new_shellp_val(view->shl)));
}

void
view_close_activate(GtkWidget *w,
                    struct view *view) {
    shell_dispatch(view->shl,
                   CMD_NEW("request-close-document", 
                           cmd_new_shellp_val(view->shl)));
}

void
view_exit_activate(GtkWidget *w,
                   struct view *view) {
    arbiter_queue_cmd(CMD_NEW0("request-exit"));
}

/* Grid. */

void
view_grid_bpm_changed(GtkSpinButton *spinbutton,
                      struct view *view) {
    struct grid *grid = shell_get_grid(view->shl);
    grid_bpm_set(grid, gtk_spin_button_get_value(spinbutton));
    view_sync_grid_display(view);
    view_redraw(view);
}

void
view_grid_units_changed(GtkSpinButton *spinbutton,
                        struct view *view) {
    struct grid *grid = shell_get_grid(view->shl);
    grid_units_set(grid, gtk_spin_button_get_value(spinbutton));
    view_sync_grid_display(view);
    view_redraw(view);
}

void
view_grid_bpm_value_changed(GtkSpinButton *spinbutton,
                            struct view *view) {
    struct grid *grid = shell_get_grid(view->shl);
    grid_bpm_set(grid, gtk_spin_button_get_value(spinbutton));
    view_sync_grid_display(view);
    view_redraw(view);
}

void
view_grid_units_value_changed(GtkSpinButton *spinbutton,
                              struct view *view) {
    struct grid *grid = shell_get_grid(view->shl);
    grid_units_set(grid, gtk_spin_button_get_value(spinbutton));
    view_sync_grid_display(view);
    view_redraw(view);
}

void
view_grid_measurement_changed(GtkWidget *w,
                              struct view *view) {
    enum grid_measurement m;
    struct grid *grid = shell_get_grid(view->shl);
    const char *units = combo_box_get_value(COMBO_BOX(w));

    if(!strcmp(units, "Frames")) {
        m = GRID_FRAMES;
    } else if(!strcmp(units, "Seconds")) {
        m = GRID_SECONDS;
    } else if(!strcmp(units, "Beats")) {
        m = GRID_BEATS;
    } else {
        FAIL("fell through, units = %s\n", units);
        m = GRID_SECONDS;
    }
    grid_measurement_set(grid, m);

    g_signal_handlers_block_matched(G_OBJECT(w), 
                                    G_SIGNAL_MATCH_DATA,
                                    0,
                                    0,
                                    NULL,
                                    NULL,
                                    view);
    view_sync_grid_display(view);
    g_signal_handlers_unblock_matched(G_OBJECT(w), 
                                      G_SIGNAL_MATCH_DATA,
                                      0,
                                      0,
                                      NULL,
                                      NULL,
                                      view);

    view_redraw(view);
}

/* Info canvas. */

void
view_infocanvas_size_request(GtkWidget *widget, 
                             GtkRequisition *requisition,
                             struct view *view) {
    struct gui_letterbox *lb;
    struct gui_letterbox_extents ink_lbe, logical_lbe;

#ifndef HAVE_GNOME2
    if(!GTK_WIDGET_REALIZED (widget)) 
        return;
#endif

    lb = gui_letterbox_new(widget, "0000000 ... 00000000");
    
    gui_letterbox_get_extents(lb, &ink_lbe, &logical_lbe);

    gui_letterbox_destroy(lb);

    requisition->height = 6 + DASH_FONT_BIG_HEIGHT + 
        (logical_lbe.ascent + logical_lbe.descent) * 3;
    
    requisition->width = logical_lbe.width * 1.5f;
}

gboolean 
view_infocanvas_expose_event(GtkWidget *widget, 
                             GdkEventExpose *event, 
                             struct view *view) {
    draw_infocanvas(view, widget, &event->area);
    return TRUE;
}

gboolean
view_infocanvas_button_press_event(GtkWidget *widget,
                                   GdkEventButton *event,
                                   struct view *view) {
    view->draw_lengths = view->draw_lengths ? 0 : 1;
    gtk_widget_queue_draw(widget);
    return TRUE;
}

/* Mixer canvas. */

gboolean 
view_mixercanvas_expose_event(GtkWidget *widget, 
                              GdkEventExpose *event, 
                              struct view *view) {
    draw_mixercanvas(view, widget, &event->area);
    return TRUE;
}

gint 
view_mixercanvas_configure_event(GtkWidget *widget, 
                                 GdkEventConfigure *event,
                                 struct view *view) {
    if(view->mixerpixmap)
        gdk_pixmap_unref(view->mixerpixmap);

    view->mixerpixmap = gdk_pixmap_new(widget->window,
                                       widget->allocation.width,
                                       widget->allocation.height,
                                       -1);
    return TRUE;

}

gboolean 
view_mixercanvas_button_press_event(GtkWidget *widget,
                                    GdkEventButton *event,
                                    struct view *view) {

    arbiter_queue_cmd(CMD_NEW("on-mixercanvas-button-press",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    return TRUE;
    
}

gboolean
view_mixercanvas_motion_notify_event(GtkWidget *widget,
                                     GdkEventMotion *event,
                                     struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-mixercanvas-motion-notify",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    return TRUE;
}

gboolean 
view_mixercanvas_button_release_event(GtkWidget *widget,
                                      GdkEventButton *event,
                                      struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-mixercanvas-button-release",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    return TRUE;
}

/* Adjustments */

void 
view_vadjust_value_changed(GtkAdjustment *adj, 
                           struct view *view) {
    adj->value = (int) adj->value;
    gtk_widget_queue_draw(view_get_widget(view, "mixercanvas"));
    gtk_widget_queue_draw(view_get_widget(view, "wavecanvas"));
}

void 
view_hadjust_value_changed(GtkAdjustment *adj, 
                           struct view *view) {

    view_set_hpos_internal(view, (AFframecount)floor(adj->value));
    gtk_widget_queue_draw(view_get_widget(view, "wavecanvas"));
}

/* Main window. */

gboolean
view_shell_delete_event(GtkWidget *w,
                        GdkEvent *ev,
                        struct view *view) {
    shell_dispatch(view->shl,
                   CMD_NEW("request-close-document", 
                           cmd_new_shellp_val(view->shl)));
    return TRUE;
}

void
view_shell_size_allocate(GtkWidget *widget,
                         GtkAllocation *allocation,
                         struct view *view) {
    
    /* This function attaches to the window's "size_allocate"
       signal. The assumption is that we get here after the drawing
       area's "configure" handler has run (see below). We simply force
       another resize of the drawing area here after processing any
       outstanding events. Otherwise it may happen in some cases that
       the drawing area gets the wrong size; in particular, when I use
       my window manager's maximize function, and if this causes the
       vertical scrollbar to be hidden (because there is now enough
       vertical screen space to show all the tracks), then without
       this code, the drawing area will be sized as if the scrollbar
       was not there. The strange thing is that this problem does not
       happen when resizing a window by its corner. 

       This problem does not occur under GNOME 2 and Sawfish 2. */

    if(arbiter_is_quit_requested())
        return;
#ifndef HAVE_GNOME2
    gtk_widget_queue_resize(view_get_widget(view, "wavecanvas"));
#endif
}

gboolean 
view_shell_focus_in_event(GtkWidget *widget,
                          GdkEventFocus *event,
                          struct view *view) {
    return FALSE;
}

gboolean 
view_shell_focus_out_event(GtkWidget *widget,
                           GdkEventFocus *event,
                           struct view *view) {
    return FALSE;
}

void
view_shell_destroy(GtkWidget *w,
                   struct view *view) {
    if(view->shl->use)
        FAIL("warning: destroy while still busy?!?\n");
    shell_destroy_internal(view->shl);
}

/* Waveform canvas. */

gboolean 
view_wavecanvas_expose_event(GtkWidget *widget, 
                             GdkEventExpose *event, 
                             struct view *view) {
    draw_wavecanvas(view, widget, &event->area);
    return TRUE;
}

gint 
view_wavecanvas_configure_event(GtkWidget *widget, 
                                GdkEventConfigure *event,
                                struct view *view) {
    size_t peaks_size;

    if(view->wavepixmap)
        gdk_pixmap_unref(view->wavepixmap);

    if(view->peaks_low)
        mem_free(view->peaks_low);

    if(view->rms)
        mem_free(view->rms);

    view->peaks_low = NULL;
    view->peaks_high = NULL;
    view->rms = NULL;

    view->wavepixmap = gdk_pixmap_new(widget->window,
                                      widget->allocation.width,
                                      widget->allocation.height,
                                      -1);
    
    peaks_size = widget->allocation.width * sizeof(peak_unit_t) * 2;
    view->peaks_low = mem_calloc(1, peaks_size);

    if(!view->peaks_low) {
        FAIL("not enough memory to allocate peak buffer (%d bytes)\n",
             peaks_size);
        return TRUE;
    }
    
    view->peaks_high = &view->peaks_low[(size_t)widget->allocation.width];

    view->rms = mem_calloc(1, widget->allocation.width * sizeof(rms_unit_t));
    if(!view->rms) {
        FAIL("not enough memory to allocate average buffer\n");
        return TRUE;
    }
    
    view_set_vpos(view, view->vadjust->value);
    view_set_hpos(view, view->hadjust->value);
    view_update_timeline(view);
    
    return TRUE;

}

gboolean
view_wavecanvas_key_press_event(GtkWidget *widget,
                                GdkEventKey *event,
                                struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-key-press",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    
    return TRUE;
}

gboolean
view_wavecanvas_key_release_event(GtkWidget *widget,
                                  GdkEventKey *event,
                                  struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-key-release",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    
    return TRUE;
}

#ifdef HAVE_GNOME2
gboolean
view_wavecanvas_scroll_event(GtkWidget *widget,
                             GdkEventScroll *event,
                             struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-scroll",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    
    return TRUE;

}
#endif


gboolean 
view_wavecanvas_button_press_event(GtkWidget *widget,
                                   GdkEventButton *event,
                                   struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-button-press",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    
    return TRUE;
}

gboolean
view_wavecanvas_motion_notify_event(GtkWidget *widget,
                                    GdkEventMotion *event,
                                    struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-motion-notify",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    
    return TRUE;

}

gboolean 
view_wavecanvas_button_release_event(GtkWidget *widget,
                                     GdkEventButton *event,
                                     struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-button-release",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    return TRUE;
}


gboolean 
view_wavecanvas_enter_notify_event(GtkWidget *widget,
                                   GdkEventCrossing *event,
                                   struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-enter-notify",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    return TRUE;
}

gboolean 
view_wavecanvas_leave_notify_event(GtkWidget *widget,
                                   GdkEventCrossing *event,
                                   struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-leave-notify",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    return TRUE;
}

gboolean 
view_wavecanvas_visibility_notify_event(GtkWidget *widget,
                                        GdkEventVisibility *event,
                                        struct view *view) {
    arbiter_queue_cmd(CMD_NEW("on-wavecanvas-visibility-notify",
                              cmd_new_shellp_val(view->shl),
                              cmd_new_GtkObjectp_val(GTK_OBJECT(widget)),
                              cmd_new_GdkEventp_val_with_dtor(gdk_event_copy((GdkEvent *)event), cmd_GdkEventp_dtor)));
    return TRUE;
}

