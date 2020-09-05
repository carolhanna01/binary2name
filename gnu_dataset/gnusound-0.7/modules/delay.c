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

#include <gnusound.h>

#define DELAY_GLADE_FILE "delay" GUI_GTK_VERSION_TAG ".glade"

struct delay_params {
    AFframecount duration;
    int times;
    double decay;
};

static int self_id = 0;

static void
delay(shell *shl,
      int track,
      AFframecount start_offset,
      AFframecount end_offset,
      int duration,
      int times,
      double decay) {
    int32_t *delay_buffer = mem_calloc(1, duration * 2 * sizeof(int32_t));
    int i, j = 0, loops = 1;
    double duration_adjust, decay_adjust;
    ITERATOR_INIT(start_offset, end_offset - start_offset);

    if(!delay_buffer) {
        FAIL("not enough memory for delay buffer (%d bytes)\n",
             duration * 2 * sizeof(int32_t));
        ITERATOR_EXIT();
        return;
    }

    ITERATOR(shl, shl->clip->sr->tracks[track], 
             for(i = 0; i < iter_read; i++) {
                 decay_adjust =
                     marker_list_slope_value(shl->clip->markers->lists[track],
                                             iter_frame_offset + i,
                                             MARKER_SLOPE_AUX); 
                 
                 int32_frame_bits[i] = 
                     int32_frame_bits[i] + delay_buffer[j] * 
                     (decay + (decay * decay_adjust));
                 delay_buffer[j] = int32_frame_bits[i];
                 j++;
                 duration_adjust = 
                     marker_list_slope_value(shl->clip->markers->lists[track],
                                             iter_frame_offset + i,
                                             MARKER_SLOPE); 
                 
                 if(j >= duration + (duration * duration_adjust)) {
                     loops++;
                     if(loops > times)
                         ITERATOR_ESCAPE();
                     j = 0;
                 }
             }
             track_replace_samples_from(shl->clip->sr->tracks[track],
                                        SAMPLE_TYPE_INT_32,
                                        int32_frame_bits,
                                        iter_frame_offset,
                                        iter_read));
    
    free(delay_buffer);
    ITERATOR_EXIT();
    return;
}

static void
delay_dialog_apply(struct dialog *dialog,
                   void *user_data) {
    struct delay_params *p = mem_alloc(sizeof(*p));

    g_return_if_fail(p != NULL);

    p->duration = gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "duration"))) * dialog->shl->grid.gap;
    p->times = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "times")));
    p->decay = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(dialog->pane, "decay")))->value / 100;
    
    DEBUG("grid.gap: %ld, spinbuttonvalue: %f, p->duration: %ld\n",
          (long)dialog->shl->grid.gap, gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "duration"))), p->duration);

    shell_dispatch_as(dialog->shl, 
                      CMD_NEW("execute-module", 
                              cmd_new_shellp_val(dialog->shl),
                              cmd_new_int_val(self_id),
                              cmd_new_voidp_val_with_dtor(p, cmd_voidp_dtor)),
                      module_get(self_id)->name);
}

static void
delay_dialog_close(struct dialog *dialog,
                   void *user_data) {
    struct module_state *module_state = 
        shell_get_module_state(dialog->shl, self_id);

    module_state->is_open = 0;
    dialog_destroy(dialog);
}

static struct cmd_value *
delay_open(int id,
           shell *shl) {
    char path[4096];
    struct module_state *module_state = shell_get_module_state(shl, id);
    struct dialog *dialog;

    snprintf(path, 4096, "%s/%s", module_get_path(id), DELAY_GLADE_FILE);

    dialog = dialog_new(sizeof(*dialog),
                        path,
                        "dialog",
                        shl,
                        0,
                        NULL,
                        NULL);
    
    if(!dialog) 
        return cmd_new_error_val("Could not load interface %s.", path);

    dialog->apply = delay_dialog_apply;
    dialog->close = delay_dialog_close;
    
    module_state = shell_get_module_state(shl, id);
    module_state->is_open = 1;
    module_state->data = dialog;

    return cmd_new_void_val();
}

static void
delay_close(int id, 
            shell *shl) {
    struct module_state *module_state = shell_get_module_state(shl, id);
    dialog_destroy((struct dialog *)module_state->data);
}

static struct cmd_value *
delay_execute(int id,
              shell *shl,
              void *data) {
    AFframecount start = shl->select_start,
        end = shl->select_end;
    int map = shl->select_channel_map;
    int t, duration, times;
    double decay;
    struct delay_params *p = data;
    struct cmd_value *r;
    struct cmd *cmd;
    struct module_state *module_state = shell_get_module_state(shl, id);
    
    if(!p) 
        return cmd_new_error_val("Delay does not have defaults.");

    duration = p->duration;
    times = p->times;
    decay = p->decay;

    /* Preserve the selection. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(start),
                  cmd_new_long_val(end - start));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) 
        return r;
    cmd_destroy_value(r);

    rwlock_rlock(&shl->clip->sr->rwl);

    DEBUG("duration: %d\n", duration);

    for(t = 0; t < snd_track_count(shl->clip->sr); t++) 
        if((1 << t) & map && t < shl->clip->sr->channels) 
            delay(shl,
                  t,
                  start,
                  end,
                  duration,
                  times,
                  decay);
    rwlock_runlock(&shl->clip->sr->rwl);

    return cmd_new_void_val();
}

static int
delay_init(int id) {
    self_id = id;
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Delay",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    0,

    delay_init,
    NULL,
    delay_open,
    delay_execute,
    delay_close,
    NULL,
    NULL
};

