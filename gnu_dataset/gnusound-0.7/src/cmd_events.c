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
#include "arbiter.h"
#include "cmd.h"

/**
 * @file
 * These are commands in response to UI events: mouse presses,
 * key presses, etc.
 */

#define PREAMBLE(event_type) \
    shell *shl __attribute__ ((unused)) = cmd_shellp(args->argv[0]); \
    GtkWidget *widget __attribute__ ((unused)) = GTK_WIDGET(cmd_GtkObjectp(args->argv[1])); \
    event_type *event __attribute__ ((unused)) = (event_type *)cmd_GdkEventp(args->argv[2])
  
static struct cmd_value *
cmd_events_on_wavecanvas_button_press(const char *name,
                                      struct cmd_argv *args) {
    PREAMBLE(GdkEventButton);

    struct view *view = shl->view;
    struct tool *tool;
    struct cmd_value *r = cmd_new_void_val();

    if(!view->shl->clip->sr)
        return cmd_new_void_val();

    /* Vertical mouse wheel? */

    if(event->button == 4 || event->button == 5) {
        arbiter_queue_cmd(CMD_NEW(event->button == 4 ? "zoom-out" : "zoom-in", 
                                  cmd_new_shellp_val(view->shl))); 
        return cmd_new_void_val();
    }

    /* Horizontal mouse wheel? */

    if(event->button == 6 || event->button == 7) {
        view_set_hpos(view, view->hadjust->value + 
                      view->hadjust->step_increment * 
                      (event->button == 6 ? -1 : 1));
        return cmd_new_void_val();
    }

    tool = shell_get_active_tool(view->shl);
    if(tool && tool->funcs->button_press) {
        cmd_destroy_value(r);
        r = tool->funcs->button_press(tool, event);
        view_reset_status(shl->view);
    }

    /* Don't grab pointer on doubleclicks or tripleclicks. */

    if(!cmd_is_error(r) && event->type == GDK_BUTTON_PRESS) {
        gdk_pointer_grab(widget->window,
                         FALSE,
                         (GDK_BUTTON_MOTION_MASK | 
                          GDK_BUTTON_RELEASE_MASK),
                         NULL,
                         NULL,
                         GDK_CURRENT_TIME);
    }

    view_redraw(shl->view);
    return r;
}

static struct cmd_value *
cmd_events_on_wavecanvas_motion_notify(const char *name,
                                       struct cmd_argv *args) {
    PREAMBLE(GdkEventMotion);

    struct view *view = shl->view;
    struct tool *tool;
    struct cmd_value *r = cmd_new_void_val();

    if(view->last_mouse_x == -1)
        view->last_mouse_x = event->x;
    if(view->last_mouse_y == -1)
        view->last_mouse_y = event->y;
    if(view->last_mouse_offset == -1)
        view->last_mouse_offset = view->hadjust->value + 
            (event->x * view->hres);

    if(!view->shl->clip->sr) 
        return cmd_new_void_val();

    view_wavecanvas_process_motion_notify(view, event);

    tool = shell_get_active_tool(view->shl);
    if(tool && tool->funcs->motion) {
        cmd_destroy_value(r);
        r = tool->funcs->motion(tool, event);
        view_reset_status(shl->view);
    }

    view->last_mouse_x = event->x;
    view->last_mouse_y = event->y;
    view->last_mouse_offset = view->hadjust->value + (event->x * view->hres);
    gtk_widget_queue_draw(view_get_widget(view, "infocanvas"));

    return r;
}


static struct cmd_value *
cmd_events_on_wavecanvas_button_release(const char *name,
                                        struct cmd_argv *args) {
    PREAMBLE(GdkEventButton);

    struct view *view = shl->view;
    struct tool *tool;
    struct cmd_value *r = cmd_new_void_val();

    /* Stop emitting motion events. */

    view->wavecanvas_emit_motion_notify = 0;

    //    view_clear_transient(shl);

    if(!view->shl->clip->sr)
        return cmd_new_void_val();

    /* Mouse wheel release. */

    if(event->button == 4 || event->button == 5 ||
       event->button == 6 || event->button == 7) 
        return cmd_new_void_val();

    gdk_pointer_ungrab(GDK_CURRENT_TIME);

    tool = shell_get_active_tool(view->shl);
    if(tool && tool->funcs->button_release) {
        cmd_destroy_value(r);
        r = tool->funcs->button_release(tool, event);
        view_reset_status(shl->view);
    }

    view_redraw(shl->view);

    return r;
}

static struct cmd_value *
cmd_events_on_wavecanvas_key_press(const char *name,
                                   struct cmd_argv *args) {
    PREAMBLE(GdkEventKey);
    
    struct view *view = shl->view;
    const char *cmd = NULL;

    /*
     * Can't set these as accelerators for the corresponding menu items,
     * so hard-code them. FIXME: this doesn't really work. Only every
     * second keypress actually gets here.
     */

    switch(event->keyval) {
    case GDK_Left:
        cmd = (event->state & GDK_SHIFT_MASK) ? "grow-left" : "move-left";
        break;
    case GDK_Right:
        cmd = (event->state & GDK_SHIFT_MASK) ? "grow-right" : "move-right";
        break;
    case GDK_Down:
        cmd = (event->state & GDK_SHIFT_MASK) ? "grow-down" : "move-down";
        break;
    case GDK_Up:
        cmd = (event->state & GDK_SHIFT_MASK) ? "grow-up" : "move-up";
        break;
    default:
        return cmd_new_void_val();
    }
    
    arbiter_queue_cmd(CMD_NEW(cmd, cmd_new_shellp_val(view->shl)));
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_events_on_wavecanvas_key_release(const char *name,
                                     struct cmd_argv *args) {
    PREAMBLE(GdkEventKey);
    
    return cmd_new_void_val();
}

static struct cmd_value *
cmd_events_on_wavecanvas_enter_notify(const char *name,
                                      struct cmd_argv *args) {
    PREAMBLE(GdkEventCrossing);

    struct cmd_value *r = NULL;
    struct tool *tool;

    if(!shl->clip->sr)
        return cmd_new_void_val();

    tool = shell_get_active_tool(shl);
    if(tool && tool->funcs->enter) {
        r = tool->funcs->enter(tool, event);
        view_reset_status(shl->view);
    }

    return r ? r : cmd_new_void_val();
}

static struct cmd_value *
cmd_events_on_wavecanvas_leave_notify(const char *name,
                                      struct cmd_argv *args) {
    PREAMBLE(GdkEventCrossing);

    struct cmd_value *r = NULL;
    struct tool *tool;
    
    /* Stop emitting motion events. */

    shl->view->wavecanvas_emit_motion_notify = 0;

    if(!shl->clip->sr)
        return cmd_new_void_val();

    gdk_pointer_ungrab(GDK_CURRENT_TIME);

    tool = shell_get_active_tool(shl);
    if(tool && tool->funcs->leave) {
        r = tool->funcs->leave(tool, event);
        view_reset_status(shl->view);
    }

    return r ? r : cmd_new_void_val();
}

static struct cmd_value *
cmd_events_on_wavecanvas_visibility_notify(const char *name,
                                           struct cmd_argv *args) {
    PREAMBLE(GdkEventVisibility);
    return cmd_new_void_val();
}

#ifdef HAVE_GNOME2
static struct cmd_value *
cmd_events_on_wavecanvas_scroll(const char *name,
                                struct cmd_argv *args) {
    PREAMBLE(GdkEventScroll);
    struct view *view = shl->view;
    
    if(event->direction == GDK_SCROLL_UP ||
       event->direction == GDK_SCROLL_DOWN) {
        if(event->direction == GDK_SCROLL_UP) 
            arbiter_queue_cmd(CMD_NEW("zoom-out", 
                                      cmd_new_shellp_val(view->shl))); 
        else
            arbiter_queue_cmd(CMD_NEW("zoom-in", 
                                      cmd_new_shellp_val(view->shl))); 
    }
    if(event->direction == GDK_SCROLL_LEFT ||
       event->direction == GDK_SCROLL_RIGHT)
        view_set_hpos(view, view->hadjust->value + 
                      view->hadjust->step_increment * 
                      (event->direction == GDK_SCROLL_LEFT ? -1 : 1));
    return cmd_new_void_val();
}
#endif

/****************************************************************
 * Mixer canvas.
 ****************************************************************/

static struct cmd_value *
cmd_events_on_mixer_switches_button_press(GtkWidget *widget,
                                          GdkEventButton *event,
                                          struct view *view) {
    //    int index_rel = event->y / (shl->vres + TRACK_Y_SPACING);
    int index_abs = PIXEL_TO_TRACK(view, event->y);
    double rel_y = PIXEL_TO_RELATIVE_Y(view, index_abs, event->y);
    char *name = "toggle-solo";
    struct cmd *cmd;
    struct cmd_value *r;

    if(index_abs == -1)
        return cmd_new_void_val();
    
    if(rel_y > 
       MIN((view->vres - 1),
           MIXER_TOGGLES_MUTE_HEIGHT + MIXER_TOGGLES_SOLO_HEIGHT + 4))
        return cmd_new_void_val();
    
    if(rel_y > MIN((view->vres - 1) / 2, MIXER_TOGGLES_MUTE_HEIGHT + 2)) {
        name = "toggle-solo";
    } else {
        if(view->shl->clip->mixer->num_solos)
            name = "toggle-solo";
        else
            name = "toggle-mute";
    }
   
    cmd = CMD_NEW(name,
                  cmd_new_shellp_val(view->shl),
                  cmd_new_int_val(index_abs));
    cmd_do_or_fail(cmd, "Cannot toggle mixer switch (%s)", &r);
    return r;
}

static struct cmd_value *
cmd_events_on_mixer_level_adjust(struct view *view,
                                 double x) {
    double mixervalue, rel_x;
    struct cmd *cmd;
    struct cmd_value *r;

    if(x > (MIXER_LEVEL_OFFSET_X + 8) - 1 && 
       x < (MIXER_LEVEL_OFFSET_X + 8 + MIXER_LEVEL_WIDTH)) {
        rel_x = x - (MIXER_LEVEL_OFFSET_X + 8);
        mixervalue = (double)rel_x / MIXER_LEVEL_WIDTH;
    } else if(x < MIXER_LEVEL_OFFSET_X + 8) {
        mixervalue = 0;
    } else {
        mixervalue = 1;
    }

    cmd = CMD_NEW("set-mix-level",
                  cmd_new_shellp_val(view->shl),
                  cmd_new_int_val(view->source_channel_being_dragged),
                  cmd_new_int_val(view->target_channel_being_dragged),
                  cmd_new_double_val(mixervalue));
    cmd_do_or_fail(cmd, "Cannot set mix level (%s)", &r);
    return r;
}


static struct cmd_value *
cmd_events_on_mixercanvas_button_press(const char *name,
                                       struct cmd_argv *args) {
    PREAMBLE(GdkEventButton);

    struct view *view = shl->view;
    int index_abs = PIXEL_TO_TRACK(view, event->y);
    int index_rel = index_abs - view->vadjust->value;
    int dst_track;
    int y = event->y;
    int mix_lvl_height = MIXER_LEVEL_HEIGHT;

    if(event->x < MIXER_LEVEL_OFFSET_X) 
        return cmd_events_on_mixer_switches_button_press(widget, event, view);

    /* Check which slider was clicked. */
    
    y -= (index_rel * (view->vres + TRACK_Y_SPACING)) + 1;

    view->source_channel_being_dragged = -1;
    view->target_channel_being_dragged = -1;
   
    if(index_abs < 0 || index_abs >= view->shl->clip->sr->channels)
        return cmd_new_void_val();

    while(mix_lvl_height > 1 && 
          (shl->clip->mixer->output_channels) +
          (shl->clip->mixer->output_channels * mix_lvl_height) > view->vres) 
        mix_lvl_height--; // = mix_lvl_height > 1 ? (mix_lvl_height - 1) / 2 : 1;

    /*    if(view->shl->clip->mixer->output_channels * mix_lvl_height > view->vres) 
        mix_lvl_height /= 2;
    */

    dst_track = y / (mix_lvl_height + 1);
    
    if(dst_track < 0 || dst_track >= view->shl->clip->mixer->output_channels)
        return cmd_new_void_val();

    gdk_pointer_grab(widget->window,
                     FALSE,
                     gtk_widget_get_events(widget),
                     NULL,
                     NULL,
                     GDK_CURRENT_TIME);
                     
    view->source_channel_being_dragged = index_abs;
    view->target_channel_being_dragged = dst_track;
    
    return cmd_events_on_mixer_level_adjust(view, event->x);
}

static struct cmd_value *
cmd_events_on_mixercanvas_motion_notify(const char *name,
                                        struct cmd_argv *args) {
    PREAMBLE(GdkEventMotion);

    struct view *view = shl->view;

    if(view->source_channel_being_dragged == -1 ||
       view->target_channel_being_dragged == -1)
        return cmd_new_void_val();

    return cmd_events_on_mixer_level_adjust(view, event->x);
}

static struct cmd_value *
cmd_events_on_mixercanvas_button_release(const char *name,
                                         struct cmd_argv *args) {
    PREAMBLE(GdkEventButton);

    struct view *view = shl->view;

    view->source_channel_being_dragged = -1;
    view->target_channel_being_dragged = -1;
    view_clear_transient(shl->view);
    gdk_pointer_ungrab(GDK_CURRENT_TIME);

    return cmd_new_void_val();
}

int
cmd_events_init() {
    int i; 
    struct cmd_signature f[] = {
        
        { "on-wavecanvas-button-press", 
          "Called when the mouse button is pressed on the waveform canvas.",
          cmd_events_on_wavecanvas_button_press, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-wavecanvas-motion-notify", 
          "Called when motion notify events are received on the "
          "waveform canvas.",
          cmd_events_on_wavecanvas_motion_notify, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-wavecanvas-button-release", 
          "Called when button release events are received on the "
          "waveform canvas.",
          cmd_events_on_wavecanvas_button_release, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-wavecanvas-key-press", 
          "Called when key press events are received on the "
          "waveform canvas.",
          cmd_events_on_wavecanvas_key_press, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-wavecanvas-key-release", 
          "Called when key release events are received on the "
          "waveform canvas.",
          cmd_events_on_wavecanvas_key_release, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-wavecanvas-enter-notify", 
          "Called when the mouse pointer enters the waveform canvas.",
          cmd_events_on_wavecanvas_enter_notify, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-wavecanvas-leave-notify", 
          "Called when the mouse pointer leaves the waveform canvas.",
          cmd_events_on_wavecanvas_leave_notify, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-wavecanvas-visibility-notify", 
          "Called when the visibility of the waveform canvas changes.",
          cmd_events_on_wavecanvas_visibility_notify, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

#ifdef HAVE_GNOME2
        { "on-wavecanvas-scroll",
          "Called when the scroll wheel(s) are used on the waveform canvas.",
          cmd_events_on_wavecanvas_scroll, CMD_VOID_T,
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },
#endif
          
        { "on-mixercanvas-button-press", 
          "Called when the mouse button is pressed on the waveform canvas.",
          cmd_events_on_mixercanvas_button_press, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-mixercanvas-motion-notify", 
          "Called when motion notify events are received on the "
          "waveform canvas.",
          cmd_events_on_mixercanvas_motion_notify, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

        { "on-mixercanvas-button-release", 
          "Called when button release events are received on the "
          "waveform canvas.",
          cmd_events_on_mixercanvas_button_release, CMD_VOID_T, 
          cmd_new_paramdecl(3, CMD_shellp_T, CMD_GtkObjectp_T,
                            CMD_GdkEventp_T) },

    };
    
    for(i = 0; i < sizeof(f) / sizeof(f[0]); i++)
        cmd_register(f[i].name, f[i].description, f[i].func, f[i].returntype,
                     f[i].pdecl);

    return 0;
}
