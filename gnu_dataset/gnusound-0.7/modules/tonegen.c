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

/* Whenever I look at this file I feel sorry for it. */

#define TONEGEN_GLADE_FILE "tonegen" GUI_GTK_VERSION_TAG ".glade"
#define GENERATOR_COUNT 4

struct tonegen_params {
    AFframecount duration;
    float frequency;
    int generators;
};

struct generator {
    char *control_name;
};

struct generator generator_info[] = {
    { "sine" },
    { "triangle" },
    { "saw" },
    { "square" },
    { NULL }
};

enum generator_types {
    SINE_GENERATOR        = (1 << 0),
    TRIANGLE_GENERATOR    = (1 << 1),
    SAW_GENERATOR         = (1 << 2),
    SQUARE_GENERATOR      = (1 << 3)
};

static int self_id = 0;

inline int32_t 
sine_generator(AFframecount offset,
               double rate,
               double frequency) {
    double rps = 2 * M_PI / rate; 
    return (int32_t)(sin(offset * rps * frequency) * INT32_MAX);
}

inline int32_t
triangle_generator(AFframecount offset,
                   double rate,
                   double frequency) {
    double rps = 2 * M_PI / rate; 
    double y = fmod(offset * rps * frequency, 2 * M_PI);
    if(y < M_PI / 2)
        return (int32_t)((y / (M_PI / 2)) * INT32_MAX);
    else if(y >= M_PI / 2 && y < M_PI) {
        y -= M_PI;
        return (int32_t)(-(y / (M_PI / 2)) * INT32_MAX);
    } else if(y >= M_PI && y < M_PI + (M_PI / 2)) {
        y -= M_PI;
        return (int32_t)(-(y / (M_PI / 2)) * INT32_MAX);
    } else {
        y -= M_PI + (M_PI / 2);
        return (int32_t)((y / (M_PI / 2)) * INT32_MAX) + INT32_MAX + 1;
    }
}

inline int32_t
saw_generator(AFframecount offset,
              double rate,
              double frequency) {
    double rps = 2 * M_PI / rate; 
    double y = fmod(offset * rps * frequency, 2 * M_PI);
    if(y < M_PI)
        return (int32_t)(((((y / (2 * M_PI)) - .5) * 4) + 1) * INT32_MAX) + (INT32_MAX);
    else
        return (int32_t)(((((y / (2 * M_PI)) - .5) * 4) - 1) * INT32_MAX) + (INT32_MAX);
}

inline int32_t
square_generator(AFframecount offset,
                 double rate,
                 double frequency) {
    double rps = 2 * M_PI / rate; 
    double y = fmod(offset * rps * frequency, 2 * M_PI);
    return (y < M_PI ? INT32_MAX : INT32_MIN);
}

AFframecount
tonegen(shell *shl,
        int track,
        AFframecount start_offset,
        AFframecount end_offset,
        float frequency,
        int generators) {
    int i, gens_active = 0;
    double ampl_adjust, freq_adjust, atten = 0;
    ITERATOR_INIT(start_offset, end_offset - start_offset);
    for(i = 0; i < GENERATOR_COUNT; i++)
        if((1 << i) & generators)
            gens_active++;
    DEBUG("gens_active: %d\n", gens_active);
    if(gens_active)
        atten = (double)1 / (double)gens_active;
    ITERATOR_SKEL(shl, shl->clip->sr->tracks[track], ITERATOR_CHECKER(),
                  ;, 
                  for(i = 0; i < MIN(EFFECT_BUF_SIZE, iter_frame_count); i++) {
                      int32_frame_bits[i] = 0;
                      ampl_adjust = marker_list_slope_value(shl->clip->markers->lists[track],
                                                            iter_frame_offset + i,
                                                            MARKER_SLOPE) + 1;
                      freq_adjust = marker_list_slope_value(shl->clip->markers->lists[track],
                                                            iter_frame_offset + i,
                                                            MARKER_SLOPE_AUX);
                      freq_adjust = (frequency * freq_adjust) + frequency;
                      //                      DEBUG("freq_adjust: %f\n", freq_adjust);
                      if(generators & SINE_GENERATOR)
                          int32_frame_bits[i] += 
                              sine_generator(iter_frames_processed + i,
                                             shl->clip->sr->rate,
                                             freq_adjust) * ampl_adjust * atten;
                      if(generators & SAW_GENERATOR)
                          int32_frame_bits[i] += 
                              saw_generator(iter_frames_processed + i,
                                            shl->clip->sr->rate,
                                            freq_adjust) * ampl_adjust * atten;
                      if(generators & TRIANGLE_GENERATOR)
                          int32_frame_bits[i] += 
                              triangle_generator(iter_frames_processed + i,
                                                 shl->clip->sr->rate,
                                                 freq_adjust) * ampl_adjust * atten;
                      if(generators & SQUARE_GENERATOR)
                          int32_frame_bits[i] += 
                              square_generator(iter_frames_processed + i,
                                               shl->clip->sr->rate,
                                               freq_adjust) * ampl_adjust * atten;
                  }
                  track_insert_samples_from(shl->clip->sr->tracks[track],
                                            SAMPLE_TYPE_INT_32,
                                            int32_frame_bits,
                                            iter_frame_offset,
                                            MIN(EFFECT_BUF_SIZE, iter_frame_count));
                  iter_read = iter_written = MIN(EFFECT_BUF_SIZE, iter_frame_count));
    iter_is_last_iteration = 1;
    ITERATOR_EXIT();
    return iter_frames_processed;
}

static void
tonegen_dialog_apply(struct dialog *dialog,
                     void *user_data) {
    int i;
    struct tonegen_params *p = mem_alloc(sizeof(*p));

    g_return_if_fail(p != NULL);

    p->duration = gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "duration"))) * dialog->shl->grid.gap;
    p->frequency = gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "frequency")));

    p->generators = 0;
    for(i = 0; i < GENERATOR_COUNT; i++)
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, generator_info[i].control_name))))
            p->generators |= (1 << i);

    shell_dispatch_as(dialog->shl, 
                      CMD_NEW("execute-module", 
                              cmd_new_shellp_val(dialog->shl),
                              cmd_new_int_val(self_id),
                              cmd_new_voidp_val_with_dtor(p, cmd_voidp_dtor)),
                      module_get(self_id)->name);
}

static void
tonegen_dialog_close(struct dialog *dialog,
                     void *user_data) {
    struct module_state *module_state = 
        shell_get_module_state(dialog->shl, self_id);

    module_state->is_open = 0;
    dialog_destroy(dialog);
}

static struct cmd_value *
tonegen_open(int id,
             shell *shl) {
    char path[4096];
    struct module_state *module_state;
    struct dialog *dialog;

    snprintf(path, 4096, "%s/%s", module_get_path(id), TONEGEN_GLADE_FILE);

    dialog = dialog_new(sizeof(*dialog),
                        path,
                        "dialog",
                        shl,
                        0,
                        NULL,
                        NULL);
    
    if(!dialog) 
        return cmd_new_error_val("Could not load interface %s.", path);

    ((struct dialog *)dialog)->apply = tonegen_dialog_apply;
    ((struct dialog *)dialog)->close = tonegen_dialog_close;

    module_state = shell_get_module_state(shl, id);
    module_state->is_open = 1;
    module_state->data = dialog;

    return cmd_new_void_val();
}

static void
tonegen_close(int id,
              shell *shl) {
    struct module_state *module_state = shell_get_module_state(shl, id);
    dialog_destroy((struct dialog *)module_state->data);
}

static struct cmd_value *
tonegen_execute(int id,
                shell *shl,
                void *data) {
    AFframecount start = shl->select_start, end;
    int map = shl->select_channel_map;
    int t, generators;
    float frequency;
    AFframecount highest = 0, frames_generated;
    struct tonegen_params *p = data;
    struct cmd *cmd;
    struct cmd_value *r;
    const char *s;

    if(!p) 
        return cmd_new_error_val("Tone Generator does not have defaults.");

    if((s = constraints_test(shl->constraints,
                             region_new(map,
                                        start, 
                                        end - start),
                             CONSTRAINTS_OPER_INSERT)))
        return cmd_new_error_val("Cannot use Tone Generator because region is "
                                 "locked (%s)", s);

    constraints_push(shl->constraints,
                     "Tone Generator",
                     region_new(map, start, end - start),
                     (CONSTRAIN_POSITION | 
                      CONSTRAIN_LENGTH |
                      CONSTRAIN_CONTENTS));

    DEBUG("tonegen, duration: %ld, frequency: %f, generator mask: %d\n",
          p->duration, p->frequency, p->generators);

    end = shl->select_start + p->duration;
    frequency = p->frequency;
    generators = p->generators;

    rwlock_rlock(&shl->clip->sr->rwl);

    for(t = 0; t < snd_track_count(shl->clip->sr); t++) {
        if((1 << t) & map && t < shl->clip->sr->channels) { 

            /* In principle tonegen always generates the amount 
               of frames we specify but because it may be cancelled
               midway we must create a seperate delete action for
               every track we touch. */

            frames_generated = tonegen(shl,
                                       t,
                                       start,
                                       end,
                                       frequency,
                                       generators);
            history_remember(shl->history,
                             CMD_NEW("delete-snd",
                                     cmd_new_int_val(1 << t),
                                     cmd_new_long_val(start),
                                     cmd_new_long_val(frames_generated)));
            if(frames_generated > highest)
                highest = frames_generated;
        }
    }

    rwlock_runlock(&shl->clip->sr->rwl);
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(start),
                  cmd_new_long_val(highest));
    cmd_do_or_fail(cmd, "Cannot set selection (%s)", &r);

    constraints_pop(shl->constraints);

    return r;
}

static int
tonegen_init(int id) {
    self_id = id;
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Tone Generator",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    0,

    tonegen_init,
    NULL,
    tonegen_open,
    tonegen_execute,
    tonegen_close,
    NULL,
    NULL
};
