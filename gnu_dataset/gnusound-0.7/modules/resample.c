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
#include <resample.h>

#define RESAMPLE_GLADE_FILE "resample" GUI_GTK_VERSION_TAG ".glade"
#define RECALC_FROM_FACTOR  0
#define RECALC_FROM_FRAMES  1
#define RECALC_FROM_RATE    2

struct resample_dialog {
    struct dialog dialog;

    int new_frame_count_lock;
    AFframecount new_frame_count;
    double base_rate;
    AFframecount base_frame_count;
};
    
struct resample_params {
    int method;
    enum resample_flags flags;
    AFframecount new_frame_count;
};

static int self_id = 0;

static void
recalc(struct resample_dialog *rd, 
       int caller) {
    double a, b;
    double factor = (double)rd->new_frame_count / (double)rd->base_frame_count;
    GtkSpinButton *rate_w, *factor_w, *frames_w;
    
    rate_w = GTK_SPIN_BUTTON(pane_get_widget(((struct dialog *)rd)->pane, "rate"));
    factor_w = GTK_SPIN_BUTTON(pane_get_widget(((struct dialog *)rd)->pane, "factor"));
    frames_w = GTK_SPIN_BUTTON(pane_get_widget(((struct dialog *)rd)->pane, "frames"));
    
    rd->new_frame_count_lock = 1;

    a = gtk_spin_button_get_value(rate_w);
    b = factor * rd->base_rate;
    if(a != b && caller != RECALC_FROM_RATE)
        gtk_spin_button_set_value(rate_w, b);
    
    a = gtk_spin_button_get_value(frames_w);
    b = rd->new_frame_count;
    if(a != b && caller != RECALC_FROM_FRAMES) 
        gtk_spin_button_set_value(frames_w, b);
    
    a = gtk_spin_button_get_value(factor_w);
    b = factor;
    if(a != b && caller != RECALC_FROM_FACTOR) 
        gtk_spin_button_set_value(factor_w, b);

    rd->new_frame_count_lock = 0;

}

static void
on_rate_changed(GtkWidget *w,
                gpointer user_data) {
    struct resample_dialog *rd = user_data;

    if(rd->new_frame_count_lock)
        return;

    rd->new_frame_count = rd->base_frame_count * 
        (gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(((struct dialog *)rd)->pane, "rate"))) / rd->base_rate);
    recalc(rd, RECALC_FROM_RATE);
}

static void
on_frames_changed(GtkWidget *w,
                  gpointer user_data) {
    struct resample_dialog *rd = user_data;

    if(rd->new_frame_count_lock)
        return;

    rd->new_frame_count = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(((struct dialog *)rd)->pane, "frames")));
    recalc(rd, RECALC_FROM_FRAMES);
}

static void
on_factor_changed(GtkWidget *w,
                  gpointer user_data) {
    struct resample_dialog *rd = user_data;

    if(rd->new_frame_count_lock)
        return;

    rd->new_frame_count = rd->base_frame_count * 
        (1 / gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(((struct dialog *)rd)->pane, "factor"))));
    recalc(rd, RECALC_FROM_FACTOR);
}

/*
static void
on_dialog_focus_in_event(GtkWidget *w,
                         GdkEvent *event,
                         gpointer user_data) {
    struct module_state *module_state = 
        g_object_get_data(G_OBJECT(gtk_widget_get_toplevel(w)),
                          "user_data");
    resample_params *p = (resample_params *)module_state->data;    

    if(p->base_frame_count != p->shl->select_end - p->shl->select_start) {
        p->base_frame_count = p->shl->select_end - p->shl->select_start;
        gtk_spin_button_set_value(p->time_control,
                                  p->shl->select_end - p->shl->select_start);
    }
}
*/

static void
resample_dialog_apply(struct dialog *dialog,
                      void *user_data) {
    int i;
    struct resample_params *p = mem_alloc(sizeof(*p));
    const char *method_name;

    g_return_if_fail(p != NULL);

    p->flags = RESAMPLE_HONOR_ENVELOPES;
    p->new_frame_count = gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "frames")));
    method_name = combo_box_get_value(COMBO_BOX(pane_get_widget(dialog->pane, "method")));
    p->method = 0;
    for(i = 0; resample_algo_name(i); i++) 
        if(!strcmp(method_name, resample_algo_name(i))) 
            p->method = i;

    shell_dispatch_as(dialog->shl, 
                      CMD_NEW("execute-module", 
                              cmd_new_shellp_val(dialog->shl),
                              cmd_new_int_val(self_id),
                              cmd_new_voidp_val_with_dtor(p, cmd_voidp_dtor)),
                      module_get(self_id)->name);
}

static void
resample_dialog_close(struct dialog *dialog,
                      void *user_data) {
    struct module_state *module_state = 
        shell_get_module_state(dialog->shl, self_id);

    module_state->is_open = 0;
    dialog_destroy(dialog);
}

static struct cmd_value *
resample_open(int id,
              shell *shl) {
    GList *l = NULL;
    GtkWidget *method_combo_box;
    char path[4096];
    int i;
    struct module_state *module_state;
    struct dialog *dialog;
    static struct pane_signal_bindings bindings[] = {
        { "factor", "changed", on_factor_changed },
        { "rate", "changed", on_rate_changed },
        { "frames", "changed", on_frames_changed },
    };

    snprintf(path, 4096, "%s/%s", module_get_path(id), RESAMPLE_GLADE_FILE);

    dialog = dialog_new(sizeof(struct resample_dialog),
                        path,
                        "dialog",
                        shl,
                        sizeof(bindings) / sizeof(bindings[0]),
                        bindings,
                        NULL);
    
    if(!dialog) 
        return cmd_new_error_val("Could not load interface %s.", path);

    dialog->apply = resample_dialog_apply;
    dialog->close = resample_dialog_close;

    ((struct resample_dialog *)dialog)->new_frame_count_lock = 0;
    ((struct resample_dialog *)dialog)->base_rate = shl->clip->sr->rate;
    ((struct resample_dialog *)dialog)->base_frame_count = shl->select_end - shl->select_start;
    
    /*
    g_signal_connect(GTK_OBJECT(p->dialog), "focus_in_event", 
                     G_CALLBACK(on_dialog_focus_in_event), shl);
    */

    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "rate")), shl->clip->sr->rate);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "frames")), shl->select_end - shl->select_start);

    method_combo_box = combo_box_new();
    gtk_widget_show(method_combo_box);
    for(i = 0; resample_algo_name(i); i++) 
        l = g_list_append(l, (char *)resample_algo_name(i));
    combo_box_set_strings(COMBO_BOX(method_combo_box), l);
    combo_box_set_active(COMBO_BOX(method_combo_box), 0);
    combo_box_set_editable(COMBO_BOX(method_combo_box), FALSE);
    g_list_free(l);

    pane_register_widget(dialog->pane, "method", method_combo_box);

    gtk_table_attach(GTK_TABLE(pane_get_widget(dialog->pane, "resample_table")),
                     GTK_WIDGET(method_combo_box), 
                     1, 2, /* left, right */
                     3, 4, /* top, bottom */
                     GTK_EXPAND | GTK_FILL, 0, /* xoptions, yoptions */
                     0, 0); /* xpad, ypad */

    module_state = shell_get_module_state(shl, id);
    module_state->is_open = 1;
    module_state->data = dialog;

    return cmd_new_void_val();
}

static void
resample_close(int id, 
               shell *shl) {
    struct module_state *module_state = shell_get_module_state(shl, id);
    dialog_destroy((struct dialog *)module_state->data);
}

static struct cmd_value *
resample_execute(int id,
                 shell *shl,
                 void *data) {
    AFframecount offset = shl->select_start;
    AFframecount count = shl->select_end - shl->select_start;
    int map = shl->select_channel_map;
    int t, algo;
    struct filter_stats stats[MAX_TRACKS] = { 0 };
    AFframecount max_written = 0;
    double factor;
    struct cmd *cmd;
    struct cmd_value *r;
    struct resample_params *p = data;
    const char *s;
    snd *copy_sr;
    
    if(!p) 
        return cmd_new_error_val("Resample does not have defaults.");

    if(p->new_frame_count == 0) 
        return cmd_new_error_val("Factor cannot be 0.");

    algo = p->method;
    factor = (double)(count) / (double)p->new_frame_count;

#ifdef HAVE_SAMPLERATE
    if(src_get_name(algo)) {
        if(!src_is_valid_ratio(factor)) 
            return cmd_new_error_val("Factor is out of range for this method."
                                     "Try the Integer ZOH method.");
    }
#endif
    
    /* Preserve the selection. */

    copy_sr = snd_copy(shl->clip->sr, map, offset, count);
    
    if(error_thrown(ERROR(shl->clip->sr))) 
        return cmd_error_cascade(cmd_new_error_val("Unable to preserve region"),
                                 ERROR(shl->clip->sr));

    history_remember(shl->history,
                     CMD_NEW("insert-snd",
                             cmd_new_shellp_val(shl),
                             cmd_new_int_val(map),
                             cmd_new_sndp_val_with_dtor(copy_sr, 
                                                        cmd_sndp_dtor),
                             cmd_new_int_val(map),
                             cmd_new_long_val(offset)));
    
    if((s = constraints_test(shl->constraints,
                             region_new(map, offset, REGION_MATCH_ANYTHING),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot Resample because region is locked "
                                 "(%s)", s);

    constraints_push(shl->constraints,
                     "Resampling",
                     region_new(map, offset, REGION_MATCH_ANYTHING),
                     (CONSTRAIN_POSITION | 
                      CONSTRAIN_CONTENTS));

    rwlock_rlock(&shl->clip->sr->rwl);

    for(t = 0; t < snd_track_count(shl->clip->sr); t++) {
        if((1 << t) & map) {
            resample_track(shl,
                           t,
                           offset,
                           offset + count,
                           p->new_frame_count,
                           algo,
                           RESAMPLE_HONOR_ENVELOPES,
                           &stats[t]);
            if(stats[t].produced > max_written)
                max_written = stats[t].produced;
            DEBUG("track %d: consumed: %ld, produced: %ld\n",
                  t, stats[t].consumed, stats[t].produced);
        }
    }

    rwlock_runlock(&shl->clip->sr->rwl);

    /* To undo a resample operation we need to delete the segment
       which was resampled, plus any potential remainder which was
       left untouched (e.g. because the resample operation was
       cancelled). */
       
    for(t = 0; t < snd_track_count(shl->clip->sr); t++) 
        if((1 << t) & map && stats[t].consumed > 0) 
            history_remember(shl->history,
                             CMD_NEW("delete-snd",
                                     cmd_new_shellp_val(shl),
                                     cmd_new_int_val(1 << t),
                                     cmd_new_long_val(offset),
                                     cmd_new_long_val(stats[t].produced + 
                                                      (count - 
                                                       stats[t].consumed))));
    
    cmd = CMD_NEW("set-selection",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(max_written));
    cmd_do_or_fail(cmd, "Cannot set selection (%s)", &r);
    
    constraints_pop(shl->constraints);

    return r;
}

static int
resample_init(int id) {
    self_id = id;
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Resample",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    0,

    resample_init,
    NULL,
    resample_open,
    resample_execute,
    resample_close,
    NULL,
    NULL
};
 
