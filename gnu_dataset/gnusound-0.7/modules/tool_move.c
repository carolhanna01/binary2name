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
 * With the move tool the user can reposition a track by dragging it
 * to a new position. To provide instant feedback the track is
 * repositioned while the user drags it. This happens in the
 * motion-notify handler.
 * 
 * Because the interface is interactive, it's not feasible to create
 * an undo for each movement the user makes: this could lead to a
 * large number of useless undos.
 *
 * Instead the undo is created when the user finishes the operation,
 * i.e. when he releases the mouse button. This generates a
 * button-release event, even when the user releases the mouse button
 * outside of the waveform canvas area, since the pointer is grabbed
 * (gdk_pointer_grab()) when the button is pressed.
 * 
 * However, there are a few cirumstances in which no button-release
 * event is generated. For example, when the user switches to a
 * different virtual desktop while dragging. In this case the X server
 * automatically relinquishes the pointer grab and does not generate
 * a button-release event.
 * 
 * This problem is solved by listening for a leave-notify event in
 * addition to a button-release event. This works because during the
 * pointer grab, no leave-notify events are generated, even if the
 * user moves the cursor outside of the waveform canvas. 
 * 
 * When the user switches to a different virtual desktop, however, the
 * pointer grab is relinquished and a leave-notify event is generated.
 * So by handling both button-release and leave-notify events, it is
 * possible to determine when the user has finished the operation.
 *
 * When the user releases the mouse button outside of the waveform
 * canvas, two events are generated: button-release followed by
 * leave-notify. To accomodate for this only the first event which
 * occurs creates an undo.
 */

#include <gnusound.h>

struct tool_move {
    struct tool tool;

    int what;
    int start_index;
    int end_index;
    int current_index;

    int pending;
};

struct cmd_value *
tool_move_begin(struct tool_move *tm,
                int from) {
    shell *shl = ((struct tool *)tm)->shl;

    if(tm->pending)
        FAIL("asked to begin move but already begun?!\n");

    if(from < 0 || from > shl->clip->sr->channels - 1)
        return cmd_new_void_val();

    tm->what = 0;

    if(((struct tool *)tm)->panel) {
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(((struct tool *)tm)->panel, "move_tracks"))))
            tm->what |= CLIP_SND;
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(((struct tool *)tm)->panel, "move_mixer"))))
            tm->what |= CLIP_MIXER;
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(((struct tool *)tm)->panel, "move_markers"))))
            tm->what |= CLIP_MARKERS;
    }

    tm->start_index = from;
    tm->end_index = from;
    tm->current_index = from;
    tm->pending = 1;

    return cmd_new_void_val();
}

struct cmd_value *
tool_move_update(struct tool_move *tm,
                 int to) {
    shell *shl = ((struct tool *)tm)->shl;
    const char *s;
    int i, modify_map = 0;

    if(to < 0 || to > shl->clip->sr->channels - 1)
        return cmd_new_void_val();
    
    if(to != tm->current_index) {

        /* 
         * move(from, to) reorders all tracks between [from, to]
         * inclusive, so check constraints for all of them. 
         */

        for(i = MIN(tm->current_index, to); 
            i < MAX(tm->current_index, to);
            i++)
            modify_map |= i;
        
        if((s = constraints_test(shl->constraints,
                                 region_new(modify_map,
                                            REGION_MATCH_ANYTHING,
                                            REGION_MATCH_ANYTHING),
                                 CONSTRAINTS_OPER_REPLACE)))
            return cmd_new_error_val("Cannot move because region is locked "
                                     "(%s)", s);

        clip_move_track(shl->clip, tm->current_index, to, tm->what);
        if(error_thrown(ERROR(shl->clip))) 
            return cmd_error_cascade(cmd_new_error_val("Cannot move"),
                                     ERROR(shl->clip));
    }
    
    tm->current_index = to;
    tm->end_index = to;
    gtk_widget_queue_draw(view_get_widget(shl->view, "wavecanvas"));
    gtk_widget_queue_draw(view_get_widget(shl->view, "mixercanvas"));
    return cmd_new_void_val();
}

struct cmd_value *
tool_move_commit(struct tool_move *tm) {
    shell *shl = ((struct tool *)tm)->shl;

    if(!tm->pending)
        return cmd_new_void_val();

    tm->pending = 0;

    if(tm->end_index == tm->start_index)
        return cmd_new_void_val();

    history_begin(shl->history, "Move Tracks");
    history_remember(shl->history,
                     CMD_NEW("move-track",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(tm->end_index),
                             cmd_new_int_val(tm->start_index),
                             cmd_new_int_val(tm->what)));
    history_commit(shl->history);
   
    return cmd_new_void_val();
}

struct cmd_value *
tool_move_button_press(struct tool *tool,
                       GdkEventButton *event) {
    int track = PIXEL_TO_TRACK(tool->shl->view, event->y);

    return tool_move_begin((struct tool_move *)tool, track);
}

struct cmd_value *
tool_move_motion(struct tool *tool,
                 GdkEventMotion *event) {
    int track = PIXEL_TO_TRACK(tool->shl->view, event->y);

    if(!(event->state & GDK_BUTTON1_MASK))
        return cmd_new_void_val();

    return tool_move_update((struct tool_move *)tool, track);
}

struct cmd_value *
tool_move_button_release(struct tool *tool,
                         GdkEventButton *event) {
    return tool_move_commit((struct tool_move *)tool);
}

struct cmd_value *
tool_move_leave(struct tool *tool,
                GdkEventCrossing *event) {
    return tool_move_commit((struct tool_move *)tool);
}

struct tool *
tool_move_new(shell *shl) {
    struct tool_move *tm = mem_calloc(sizeof(*tm), 1);
    static struct tool_funcs tool_move = {
        tool_destroy,
        
        NULL, // activate
        NULL, // deactivate
        
        tool_move_button_press,
        tool_move_button_release,
        tool_move_motion,
        
        NULL, // key_press
        NULL, // key_release

        NULL, // enter
        tool_move_leave,

        {
            NULL,
            0,
            10,
            NULL
        }
    };

    if(!tm)
        return NULL;

    ((struct tool *)tm)->name = "move";
    ((struct tool *)tm)->displayname = "Move";
    ((struct tool *)tm)->tooltip = "Moves tracks";    
    ((struct tool *)tm)->ordinal = 50;
    ((struct tool *)tm)->cursor = GDK_FLEUR;
    ((struct tool *)tm)->accel_key = GDK_z;
    ((struct tool *)tm)->accel_mods = GDK_MOD1_MASK;
    ((struct tool *)tm)->funcs = &tool_move;

    tm->pending = 0;

    return (struct tool *)tm;
}

int
tool_move_init(int id) {
    return tool_register("move", tool_move_new);
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Move Tool",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002,2003,2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    tool_move_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

