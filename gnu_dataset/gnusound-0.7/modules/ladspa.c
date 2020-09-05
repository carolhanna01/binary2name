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

/* still using GtkCList: */
#undef GTK_DISABLE_DEPRECATED

#include <gnusound.h>
#include <dlfcn.h>
#include <dirent.h>
#include <errno.h>
#include <ctype.h>
#include <wordexp.h>
#include "ladspa.h"

/* Redefine EFFECT_BUF_SIZE because at least 1 LADSPA plugin
   (Chebstortion) doesn't like it when the buffer size is a power of
   two. */

#undef EFFECT_BUF_SIZE
#define EFFECT_BUF_SIZE 32000

#define MAX_AUDIO_INPUTS 10
#define MAX_AUDIO_OUTPUTS 10
#define MAX_CONTROLS 60
#define MAX_PRESETS 20
#define MAX_PLUGINS 300

#define LADSPA_GLADE_FILE  "ladspa" GUI_GTK_VERSION_TAG ".glade"

struct ladspa_module {
    char *filename;
    void *handle;
    LADSPA_Descriptor *desc;
};

struct ladspa_dialog {
    struct dialog dialog;

    LADSPA_Descriptor *currently_selected;

    /** The number of controls for the currently selected plugin. */
    int controls_count;
    GtkWidget *controls[MAX_CONTROLS];
    GtkWidget *envelope1[MAX_CONTROLS];
    GtkWidget *envelope2[MAX_CONTROLS];

    /** The current, minimum and maximum values for each control. */
    LADSPA_Data control_values[MAX_CONTROLS];
    LADSPA_Data control_min[MAX_CONTROLS];
    LADSPA_Data control_max[MAX_CONTROLS];

    /** Tooltips. */
    GtkTooltips *tips;
};

struct ladspa_params {
    LADSPA_Descriptor *desc;
    LADSPA_Handle *handle;

    /** The number of controls for the currently selected plugin. */
    int controls_count;
    /** The current, minimum and maximum values for each control. */
    LADSPA_Data control_values[MAX_CONTROLS];
    LADSPA_Data control_min[MAX_CONTROLS];
    LADSPA_Data control_max[MAX_CONTROLS];
    /** Bitfield specifying, for each control, which envelopes are 
        connected to it. */
    int envelopes_enabled[MAX_CONTROLS];
    /** How many controls are connected to the primary envelope. */
    int envelope1_connect_count;
    /** Points at one of the controls in the control_values array. */
    LADSPA_Data *envelope1_connections[MAX_CONTROLS];
    /** What values correspond to a primary envelope value of 0, -1
        and 1 respectively. */
    LADSPA_Data envelope1_defaults[MAX_CONTROLS];
    LADSPA_Data envelope1_min[MAX_CONTROLS];
    LADSPA_Data envelope1_max[MAX_CONTROLS];
    /** How many controls are connected to the secondary envelope. */
    int envelope2_connect_count;
    /** Points at one of the controls in the control_values array. */
    LADSPA_Data *envelope2_connections[MAX_CONTROLS];
    /** What values correspond to a secondary envelope value of 0, -1
        and 1 respectively. */
    LADSPA_Data envelope2_defaults[MAX_CONTROLS];
    LADSPA_Data envelope2_min[MAX_CONTROLS];
    LADSPA_Data envelope2_max[MAX_CONTROLS];
    /** The number of input channels for the plugin. */
    int audio_input_count;
    /** Contains the port numbers for the inputs. */
    int audio_input_map[MAX_AUDIO_INPUTS];
    /** The number of output channels for the plugin. */
    int audio_output_count;
    /** Contains the port numbers for the outputs. */
    int audio_output_map[MAX_AUDIO_OUTPUTS];
    /** Data buffers. */
    LADSPA_Data *in_buf[MAX_AUDIO_INPUTS];
    LADSPA_Data *out_buf[MAX_AUDIO_OUTPUTS];
};

static struct ladspa_module ladspa_modules[MAX_PLUGINS];
static int ladspa_count = 0;
static const char *ladspa_path;
static int self_id = 0;

static void
ladspa_process_core(struct ladspa_params *p,
                    float sample_rate,
                    AFframecount frame_offset,
                    AFframecount frame_count,
                    struct marker_list *ml) {
    AFframecount seg_count, seg_remain = frame_count, seg_offset = 0;
    LADSPA_Descriptor *desc = p->desc;
    double adjust = 0, delta;
    int i, seg_size = (int)(pref_get_as_float("ladspa.segment_time") *
                            sample_rate);

    for(seg_count = MIN(seg_remain, seg_size); 
        seg_remain; 
        seg_count = MIN(seg_remain, seg_size)) {
        
        /* Point audio ports at proper offset in buffers. */
        
        for(i = 0; i < p->audio_input_count; i++) 
            desc->connect_port(p->handle, p->audio_input_map[i],
                               &p->in_buf[i][seg_offset]);
        
        for(i = 0; i < p->audio_output_count; i++) 
            desc->connect_port(p->handle, p->audio_output_map[i], 
                               &p->out_buf[i][seg_offset]);
        
        /* Twiddle control ports via envelopes. */
        
        if(p->envelope1_connect_count)
            adjust = marker_list_slope_value(ml, frame_offset + seg_offset,
                                             MARKER_SLOPE);
        for(i = 0; i < p->envelope1_connect_count; i++) {
            if(adjust > 0) 
                delta = p->envelope1_max[i] - p->envelope1_defaults[i];
            else 
                delta = p->envelope1_defaults[i] - p->envelope1_min[i];
            *(p->envelope1_connections[i]) =
                p->envelope1_defaults[i] + (adjust * delta);
            //            DEBUG("min: %f, max: %f, default: %f, value: %f\n", 
            //                  p->envelope1_min[i], p->envelope1_max[i], p->envelope1_defaults[i], *(p->envelope1_connections[i]));
            
        }

        if(p->envelope2_connect_count)
            adjust = marker_list_slope_value(ml, frame_offset + seg_offset,
                                             MARKER_SLOPE_AUX);
        
        for(i = 0; i < p->envelope2_connect_count; i++) {
            if(adjust > 0) 
                delta = p->envelope2_max[i] - p->envelope2_defaults[i];
            else 
                delta = p->envelope2_defaults[i] - p->envelope2_min[i];
            *(p->envelope2_connections[i]) =
                p->envelope2_defaults[i] + (adjust * delta);
            //            DEBUG("min: %f, max: %f, default: %f, value: %f\n", 
            //                  p->envelope2_min[i], p->envelope2_max[i], p->envelope2_defaults[i], *(p->envelope2_connections[i]));
            
        }

        //        for(i = 0; i < p->envelope2_connect_count; i++) 
        //            *(p->envelope2_connections[i]) =
        //                p->envelope2_defaults[i] + (adjust * p->envelope2_defaults[i]);

        /* Process the segment. */

        desc->run(p->handle, seg_count);
        
        seg_offset += seg_count;
        seg_remain -= seg_count;
    }
}

/*
 * Process a multichannel run, that is apply the plugin to the 
 * selected range on all selected channels simultaneously.
 */
static void 
ladspa_process_many(shell *shl,
                    struct ladspa_params *p,
                    track_map_t map,                    
                    AFframecount start, 
                    AFframecount end) {
    AFframecount frame_offset = start, frame_count = end - start, 
        frame_count_old = frame_count, frames_processed = 0, rmax, 
        r[MAX_AUDIO_INPUTS], count;
    int t, i, err, envelope_track = 0;

    for(t = 0; t < shl->clip->sr->channels; t++) {
        if((1 << t) & map) {
            envelope_track = t;
            break;
        }
    }
    
    do {
        count = MIN(frame_count, EFFECT_BUF_SIZE);
        rmax = 0;
        
        for(t = 0, i = 0; (i < p->audio_input_count && 
                           t < shl->clip->sr->channels); t++) {
            if((1 << t) & map) {                
                r[i] = track_get_samples_as(shl->clip->sr->tracks[t],
                                            SAMPLE_TYPE_FLOAT_32,
                                            (float *)(p->in_buf[i]),
                                            frame_offset,
                                            count);
              
                if(r[i] == -1)
                    break;

                if(r[i] != count) 
                    /* Got fewer frames than expected so clear buffer
                       until end. */
                    memset(&p->in_buf[i][r[i]], 
                           (count - r[i]) * sizeof(LADSPA_Data), '\0');
                if(r[i] > rmax)
                    rmax = r[i];
                i++;
            }
        }
        
        if(!rmax)
            break;

        ladspa_process_core(p, shl->clip->sr->rate, frame_offset, rmax, 
                            shl->clip->markers->lists[envelope_track]);
        arbiter_yield(); 
        
        for(t = 0, i = 0; (i < p->audio_output_count &&
                           t < shl->clip->sr->channels); t++) {
            if((1 << t) & map) {
                /* FIXME: error checking */
                err = track_replace_samples_from(shl->clip->sr->tracks[t],
                                                 SAMPLE_TYPE_FLOAT_32,
                                                 (float *)(p->out_buf[i]),
                                                 frame_offset,
                                                 rmax);
                //                DEBUG("replacing %ld frames at %ld on track %d from buf %d, err: %d\n",
                //                      rmax, frame_offset, t, i, err);
                i++;
            }
        }
        
        frame_offset += rmax;
        frame_count -= rmax;
        frames_processed += rmax;
        view_set_progress(shl->view, CLAMP((float)frames_processed / 
                                           (float)(frame_count_old), 0, 1));
    } while(!shl->cancel_requested && rmax > 0 && frame_count > 0);
    
}

/*
 * Process a mono run, that is apply the plugin to the selected range
 * on every selected track in succession.
 */
static void
ladspa_process_1on1(shell *shl,
                    struct ladspa_params *p,
                    track_map_t map,                    
                    AFframecount start, 
                    AFframecount end) {
    AFframecount frame_offset = start, frame_count = end - start, 
        frame_count_old = frame_count, frames_processed = 0, r, count;
    int t;

    for(t = 0; t < shl->clip->sr->channels; t++) {
        if((1 << t) & map) {
            
            frame_offset = start;
            frame_count = end - start;
            frame_count_old = frame_count;
            frames_processed = 0;
            
            do {
                count = frame_count > EFFECT_BUF_SIZE ? 
                    EFFECT_BUF_SIZE : frame_count;
                
                if(p->in_buf[0]) {
                    r = track_get_samples_as(shl->clip->sr->tracks[t],
                                             SAMPLE_TYPE_FLOAT_32,
                                             (float *)(p->in_buf[0]),
                                             frame_offset,
                                             count);
                } else {
                    r = count;
                }
                
                if(r == -1) 
                    break;

                ladspa_process_core(p, shl->clip->sr->rate, frame_offset, r,
                                    shl->clip->markers->lists[t]);

                track_replace_samples_from(shl->clip->sr->tracks[t],
                                           SAMPLE_TYPE_FLOAT_32,
                                           (float *)(p->out_buf[0]),
                                           frame_offset,
                                           r);
                frame_offset += r;
                frame_count -= r;
                frames_processed += r;
                view_set_progress(shl->view,
                                  CLAMP((float)frames_processed / 
                                        (float)(frame_count_old), 0, 1));
                arbiter_yield(); 
            } while(!shl->cancel_requested &&
                    r > 0 && frame_count > 0);
        }
    }

}



static int
ladspa_get_port_count_by_type(const LADSPA_Descriptor *psDescriptor,
                              const LADSPA_PortDescriptor iType) {
    unsigned long lCount;
    unsigned long lIndex;
  
    lCount = 0;
    for (lIndex = 0; lIndex < psDescriptor->PortCount; lIndex++)
        if ((psDescriptor->PortDescriptors[lIndex] & iType) == iType)
            lCount++;
  
    return lCount;
}

static void
ladspa_load(const char *path) {
    void *handle;
    char *error;
    int i, audio_output_count, audio_input_count;
    LADSPA_Descriptor *desc;
    LADSPA_Descriptor *(*ladspa_desc)(unsigned long i);

    handle = dlopen(path, RTLD_NOW);
    if (!handle) {
        //        FAIL("could not open %s: %s\n", path, dlerror());
        return;
    }
    if((error = dlerror()) != NULL) {
        //        FAIL("could not open %s: %s\n", path, error);
        dlclose(handle);
        return;
    }

    ladspa_desc = dlsym(handle, "ladspa_descriptor");
    if((error = dlerror()) != NULL) {
        //        FAIL("could not open %s: %s\n", path, error);
        dlclose(handle);
        return;
    }

    for(i = 0; (desc = ladspa_desc(i)); i++) {
        audio_output_count = 
            ladspa_get_port_count_by_type(desc, 
                                          LADSPA_PORT_AUDIO | 
                                          LADSPA_PORT_OUTPUT);
        audio_input_count = 
            ladspa_get_port_count_by_type(desc, 
                                          LADSPA_PORT_AUDIO | 
                                          LADSPA_PORT_INPUT);

        /* If plugin is unsupported, skip it. */

        if(audio_output_count > MAX_AUDIO_OUTPUTS ||
           audio_input_count > MAX_AUDIO_INPUTS) {
            FAIL("GLADSPA plugin %s (%s/%ld) not supported: too many inputs or outputs (%d inputs, %d outputs)\n", desc->Name, desc->Label, desc->UniqueID, audio_input_count, audio_output_count);
            continue;
        }

        /* Don't load too many plugins. */

        if(ladspa_count >= MAX_PLUGINS) {
            FAIL("GLADSPA plugin %s (%s/%ld) not loaded because maximum amount of GLADSPA plugins was reached, increase MAX_PLUGINS (loaded: %d, max: %d).\n", desc->Name, desc->Label, desc->UniqueID, ladspa_count, MAX_PLUGINS);
            continue;
        }

        /*
         * Since a single shared object can contain multiple LADSPA
         * plugins, associate the dlopen() handle with the first of
         * them only (otherwise we run the risk of closing a module
         * multiple times).
         */

        ladspa_modules[ladspa_count].handle = (i == 0 ? handle : NULL);
        ladspa_modules[ladspa_count].desc = desc;
        ladspa_count++;
        /*
        DEBUG("\t%s %s (%lu/%s) %ld\n", 
              path,
              desc->Name,
              desc->UniqueID,
              desc->Label,
              desc->PortCount);
        */
    }
}

static int
ladspa_compare(const void *a, 
               const void *b) {
    return strcmp(((struct ladspa_module *)a)->desc->Name, 
                  ((struct ladspa_module *)b)->desc->Name);
}

static void
ladspa_collect(const char *path) {
    char tmp[4096];
    struct dirent **namelist;
    int i, n;

    n = scandir(path, &namelist, 0, alphasort);
    if (n < 0) {
        FAIL("unable to load modules in %s: %s\n", path, strerror(errno));
        return;
    }

    for(i = 0; i < n; i++) {
        snprintf(tmp, 4096, "%s/%s", path, namelist[i]->d_name);
        free(namelist[i]);
        //	DEBUG("tmp: %s\n", tmp);
        ladspa_load(tmp);
    }

    free(namelist);

    qsort(ladspa_modules, ladspa_count, sizeof(struct ladspa_module), ladspa_compare);

    return;

}

/**
 * Retrieve port values from the stored preset for the plugin.
 */

static int
ladspa_get_values_from_preset(const char *plugin_name,
                              const char *preset_name,
                              int max_values,
                              LADSPA_Data *values) {
    int i;
    char path[256];
    char **values_loaded;
    int num_values_loaded, num_values_set;

    snprintf(path, sizeof path, "/gnusound/%s/%s", plugin_name, preset_name);
    gnome_config_get_vector(path, &num_values_loaded, &values_loaded);

    for(i = 0; i < max_values && i < num_values_loaded; i++) 
        values[i] = atof(values_loaded[i]);

    num_values_set = i;

    for(i = 0; i < num_values_loaded; i++)
        g_free(values_loaded[i]);
    
    if(values_loaded)
        g_free(values_loaded);

    return num_values_set;
}

/**
 * Retrieve port values from the widgets in the dialog.
 */

static int
ladspa_get_values_from_controls(struct ladspa_dialog *ld,
                                int max_values,
                                LADSPA_Data *values) {
    int i;

    for(i = 0; i < ld->controls_count && i < max_values; i++) {

        if(GTK_IS_TOGGLE_BUTTON(ld->controls[i])) {

            values[i] = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ld->controls[i])) ? 1 : 0;

        } else if(GTK_IS_RANGE(ld->controls[i])) {

            values[i] = gtk_range_get_adjustment(GTK_RANGE(ld->controls[i]))->value;

        } else {

            FAIL("unknown control widget\n");
        }

    }

    return i;
}

static int
ladspa_set_controls_from_values(struct ladspa_dialog *ld,
                                int num_values,
                                LADSPA_Data *values) {
    int i;
    GtkAdjustment *adjust;

    for(i = 0; i < num_values && i < ld->controls_count; i++) {

        if(GTK_IS_TOGGLE_BUTTON(ld->controls[i])) {

            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ld->controls[i]), 
                                         values[i] > 0);
            
        } else if(GTK_IS_RANGE(ld->controls[i])) {

            adjust = gtk_range_get_adjustment(GTK_RANGE(ld->controls[i]));
            gtk_adjustment_set_value(adjust, values[i]);

        } else {

            FAIL("unknown control widget\n");

        }

    }

    return i;
}

static int
ladspa_store_preset(const char *plugin_name,
                    const char *preset_name,
                    int num_values,
                    LADSPA_Data *values) {
    int i, num_values_stored;
    char *values_stored[num_values];
    char path[256];

    for(i = 0; i < num_values; i++) 
        values_stored[i] = g_strdup_printf("%f", values[i]);

    num_values_stored = i;

    /* Save the array of preset values. */
    
    snprintf(path, sizeof path, "/gnusound/%s/%s", plugin_name, preset_name);
    gnome_config_set_vector(path, 
                            num_values_stored, 
                            (const char * const *) values_stored);
    gnome_config_sync();

    for(i = 0; i < num_values_stored; i++)
        g_free(values_stored[i]);

    return num_values_stored;
}

static GList *
ladspa_get_preset_names(const char *plugin_name) {
    char path[256];
    int i, num_preset_names;
    char **preset_names;
    GList *loaded_preset_names = NULL;

    /* Recall presets. */
    
    snprintf(path, sizeof path, "/gnusound/%s/Presets", plugin_name);
    gnome_config_get_vector(path, &num_preset_names, &preset_names);
    
    for(i = 0; i < num_preset_names; i++) {

        /* Empty strings can appear as a result of having earlier
           written an empty vector. */

        if(!strcmp(preset_names[i], "")) {
            g_free(preset_names[i]);
            continue;
        }
        
        loaded_preset_names = g_list_append(loaded_preset_names, 
                                            preset_names[i]);
    }
    
    if(preset_names) 
        g_free(preset_names);
    
    return loaded_preset_names;
}

static GList *
ladspa_get_preset_names_from_menu(struct dialog *dialog) {
    ComboBox *presets = COMBO_BOX(pane_get_widget(dialog->pane, "presets"));

    return combo_box_get_strings(presets);
}

static void
ladspa_set_menu_from_preset_names(struct dialog *dialog,
                                  GList *preset_names) {
    ComboBox *presets = COMBO_BOX(pane_get_widget(dialog->pane, "presets"));

    combo_box_set_strings(presets, preset_names);
}

static void
ladspa_free_strings_in_list(gpointer data,
                            gpointer user_data) {
    free(data);
}

static int
ladspa_find_string_in_list(gconstpointer a,
                           gconstpointer b) {
    return strcmp((const char *)a, (const char *)b);
}

static void
ladspa_store_preset_names(const char *plugin_name,
                          GList *preset_names) {
    char path[256];
    int i = 0;
    int num_preset_names = g_list_length(preset_names);
    char *preset_names_stored[num_preset_names];
    GList *l;

    for(l = preset_names; l; l = l->next)
        preset_names_stored[i++] = l->data;

    /* Save the array of preset names. */

    snprintf(path, sizeof path, "/gnusound/%s/Presets", plugin_name);
    gnome_config_set_vector(path, 
                            num_preset_names,
                            (const char * const *) preset_names_stored);
    gnome_config_sync();
}

static void
on_delete_preset_clicked(GtkWidget *w,
                         gpointer user_data) {
    struct dialog *dialog = user_data;
    struct ladspa_dialog *ld = (struct ladspa_dialog *)dialog;
    const char *selected_preset;
    GList *preset_names, *l;
    ComboBox *presets = COMBO_BOX(pane_get_widget(dialog->pane, "presets"));
    
    selected_preset = combo_box_get_value(presets);

    g_return_if_fail(selected_preset != NULL);
    g_return_if_fail(ld->currently_selected != NULL);

    preset_names = ladspa_get_preset_names_from_menu(dialog);

    /* Find and remove the selected preset. */

    l = g_list_find_custom(preset_names, 
                           selected_preset, 
                           ladspa_find_string_in_list);

    if(l) {
        preset_names = g_list_remove_link(preset_names, l);
        free(l->data);
        g_list_free_1(l);
    }
        
    ladspa_store_preset_names(ld->currently_selected->Name, preset_names);

    ladspa_set_menu_from_preset_names(dialog, preset_names);
    
    gtk_editable_delete_text(GTK_EDITABLE(pane_get_widget(dialog->pane, "presets_entry")), 0, -1);

    g_list_foreach(preset_names, ladspa_free_strings_in_list, NULL);
    g_list_free(preset_names);
}

static void
on_presets_changed(ComboBox *combo_box,
                   gpointer user_data) {
    struct ladspa_dialog *ld = user_data;
    const char *selected_preset;
    LADSPA_Data values[ld->controls_count];
    int got;

    selected_preset = combo_box_get_value(combo_box);

    /* FIXME: how the hell can this be NULL if we just received a
       changed signal? */

    g_return_if_fail(selected_preset != NULL);
    g_return_if_fail(ld->currently_selected != NULL);

    got = ladspa_get_values_from_preset(ld->currently_selected->Name,
                                        selected_preset,
                                        sizeof(values) / sizeof(values[0]),
                                        values);

    ladspa_set_controls_from_values(ld, got, values);
}

static void
on_presets_entry_activate(GtkEditable *w,
                          gpointer user_data) {
    struct dialog *dialog = user_data;
    struct ladspa_dialog *ld = (struct ladspa_dialog *)dialog;
    int got;
    const char *new_preset_name;
    GList *preset_names;
    LADSPA_Data values[MAX_CONTROLS];
    ComboBox *presets = COMBO_BOX(pane_get_widget(dialog->pane, "presets"));
    GtkEntry *entry = GTK_ENTRY(combo_box_get_entry(presets));
    
    new_preset_name = gtk_entry_get_text(entry);

    g_return_if_fail(new_preset_name != NULL);
    g_return_if_fail(ld->currently_selected != NULL);

    /* Ignore empty input. */

    if(!strcmp("", new_preset_name))
        return;

    preset_names = ladspa_get_preset_names_from_menu(dialog);

    /* See if a new preset name was entered. */

    if(!g_list_find_custom(preset_names, 
                           new_preset_name,
                           ladspa_find_string_in_list)) {
    
        /* If the preset name is new, add it to the list of
           presets. */
        
        preset_names = g_list_prepend(preset_names, strdup(new_preset_name));
        ladspa_store_preset_names(ld->currently_selected->Name, preset_names);
        ladspa_set_menu_from_preset_names(dialog, preset_names);

    }

    /* Store the port values for the preset. */

    got = ladspa_get_values_from_controls(ld, 
                                          sizeof(values) / sizeof(values[0]),
                                          values);

    ladspa_store_preset(ld->currently_selected->Name,
                        new_preset_name,
                        got,
                        values);

    g_list_foreach(preset_names, ladspa_free_strings_in_list, NULL);
    g_list_free(preset_names);
}

static GtkWidget *
make_widget_from_port(LADSPA_Descriptor *desc,
                      int port,
                      float sample_rate,
                      LADSPA_Data *defalt,
                      LADSPA_Data *min,
                      LADSPA_Data *max) {
    GtkAdjustment *adjust;
    GtkWidget *w;
    LADSPA_PortRangeHintDescriptor hint = desc->PortRangeHints[port].HintDescriptor;
    LADSPA_Data upper, lower, value = 0;
    double step_increment, page_increment, page_size;

    *defalt = 0;
    *min = -10000;
    *max = 10000;

    if(LADSPA_IS_HINT_TOGGLED(hint)) {
        w = gtk_check_button_new_with_label(desc->PortNames[port]);
        if(LADSPA_IS_HINT_DEFAULT_0(hint)) {
            value = 0;
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w),
                                         FALSE);
        }
        if(LADSPA_IS_HINT_DEFAULT_1(hint)) {
            value = 1;
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w),
                                         TRUE);
        }
        *min = 0;
        *max = 1;
    } else {
        lower = 0;
        upper = 100000;
        value = 100;
        if(LADSPA_IS_HINT_BOUNDED_BELOW(hint)) 
            lower = desc->PortRangeHints[port].LowerBound;
        if(LADSPA_IS_HINT_BOUNDED_ABOVE(hint)) 
            upper = desc->PortRangeHints[port].UpperBound;
        
        if(LADSPA_IS_HINT_SAMPLE_RATE(hint)) {
            lower = lower * sample_rate;
            upper = upper * sample_rate;
        }
        
        step_increment = (upper - lower) / 1000;
        page_increment = (upper - lower) / 100;
        page_size = (upper - lower) / 100;
        
        if(LADSPA_IS_HINT_DEFAULT_MINIMUM(hint)) 
            value = lower;
        if(LADSPA_IS_HINT_DEFAULT_MAXIMUM(hint)) 
            value = upper;
        
        if(LADSPA_IS_HINT_DEFAULT_LOW(hint)) {
            if(LADSPA_IS_HINT_LOGARITHMIC(hint))
                value = lower * .75 + upper * .25;
            else 
                value = exp(log(lower) * .75 + log(upper) * .25);
        }
        if(LADSPA_IS_HINT_DEFAULT_MIDDLE(hint)) {
            if(LADSPA_IS_HINT_LOGARITHMIC(hint))
                value = lower * .5 + upper * .5;
            else
                value = exp(log(lower) * .5 + log(upper) * .5);
        }
        if(LADSPA_IS_HINT_DEFAULT_HIGH(hint)) {
            if(LADSPA_IS_HINT_LOGARITHMIC(hint))
                value = lower * .25 + upper * .75;
            else
                value = exp(log(lower) * .25 + log(upper) * .75);
        }
        if(LADSPA_IS_HINT_DEFAULT_0(hint)) 
            value = 0;
        if(LADSPA_IS_HINT_DEFAULT_1(hint)) 
            value = 1;
        if(LADSPA_IS_HINT_DEFAULT_100(hint)) 
            value = 100;
        if(LADSPA_IS_HINT_DEFAULT_440(hint)) 
            value = 440;
        if(LADSPA_IS_HINT_INTEGER(hint)) {
            step_increment = page_increment = page_size = 1;
        }
        upper += page_increment;
        adjust = GTK_ADJUSTMENT(gtk_adjustment_new(value, 
                                                   lower,
                                                   upper, 
                                                   step_increment,
                                                   page_increment, 
                                                   page_size));
        //                DEBUG("lower: %f, upper: %f, value: %f, step_increment: %f, page_increment: %f, page_size: %f\n", lower, upper, value, step_increment, page_increment, page_size);
        //                v = gtk_spin_button_new(adjust, 1, decimals);
        w = gtk_hscale_new(adjust);
        gtk_scale_set_digits(GTK_SCALE(w), 2);
        if(LADSPA_IS_HINT_INTEGER(hint)) {
            //            adjust->upper += .9;
            gtk_scale_set_digits(GTK_SCALE(w), 0);
        }
        *min = lower;
        *max = upper;
    }
    *defalt = value;

    return w;
}

static void
on_clist_key_press_event(GtkCList *clist,
                         GdkEventKey *event,
                         gpointer user_data) {
    int i, key = event->keyval;
    for(i = 0; i < ladspa_count; i++) {
        if(tolower(ladspa_modules[i].desc->Name[0]) == 
           tolower(key)) {
            gtk_clist_moveto(clist, i, 0, 0, 0); 
            gtk_clist_select_row(clist, i, 0);
            break;
        }
    }
}

static void    
on_clist_select_row(GtkCList *clist,
                    gint row,
                    gint column,
                    GdkEventButton *event,
                    gpointer user_data) {
    struct dialog *dialog = user_data;
    struct ladspa_dialog *ld = (struct ladspa_dialog *)dialog;
    int i, j;
    char buf[256];
    LADSPA_Descriptor *desc = ladspa_modules[row].desc;
    GtkLabel *l;
    GtkWidget *hbox, *controls_table;
    GList *preset_names;

    printf("selected: %s, ports: %ld\n",
           desc->Name,
           desc->PortCount);

    ld->currently_selected = desc;

    preset_names = ladspa_get_preset_names(ld->currently_selected->Name);

    ladspa_set_menu_from_preset_names(dialog, preset_names);

    g_list_foreach(preset_names, ladspa_free_strings_in_list, NULL);
    g_list_free(preset_names);

    gtk_editable_delete_text(GTK_EDITABLE(pane_get_widget(dialog->pane, "presets_entry")), 0, -1);

    /* Destroy port controls for previously selected plugin. */

    controls_table = pane_find_widget(dialog->pane, "controls_table");

    if(controls_table) {
        //        gtk_object_unref(GTK_OBJECT(controls_table));
        gtk_container_remove(GTK_CONTAINER(pane_get_widget(dialog->pane, "viewport")), controls_table);
        pane_unregister_widget(dialog->pane, "controls_table");
    }

    /* Setup port controls for currently selected plugin. */

    snprintf(buf, sizeof buf, "%d/%d", 
             ladspa_get_port_count_by_type(desc, 
                                           LADSPA_PORT_AUDIO | 
                                           LADSPA_PORT_INPUT),
             ladspa_get_port_count_by_type(desc, 
                                           LADSPA_PORT_AUDIO | 
                                           LADSPA_PORT_OUTPUT));
    ld->controls_count = 0;
    gtk_label_set_text(GTK_LABEL(pane_get_widget(dialog->pane, "author")), desc->Maker);
    gtk_label_set_text(GTK_LABEL(pane_get_widget(dialog->pane, "copyright")), desc->Copyright);
    gtk_label_set_text(GTK_LABEL(pane_get_widget(dialog->pane, "audio_io_ports")), buf);
    pane_register_widget(dialog->pane, 
                         "controls_table", 
                         gtk_table_new(0, 2, FALSE));
    //    gtk_object_ref(GTK_OBJECT(controls_table));
    gtk_widget_show(pane_get_widget(dialog->pane, "controls_table"));
    gtk_container_add(GTK_CONTAINER(pane_get_widget(dialog->pane, "viewport")),
                      pane_get_widget(dialog->pane, "controls_table"));

    for(i = 0, j = 0; i < desc->PortCount; i++) {
        //        DEBUG("portname: %s\n", desc->PortNames[i]);
        if(LADSPA_IS_PORT_CONTROL(desc->PortDescriptors[i]) &&
           LADSPA_IS_PORT_INPUT(desc->PortDescriptors[i])) {

            if(ld->controls_count >= MAX_CONTROLS) {
                FAIL("no room for any more controls, skipping control %s\n",
                     desc->PortNames[i]);

                /* Continue rather than break so a warning message is
                   printed for each control we ignore. */

                continue;
            }

            gtk_table_resize(GTK_TABLE(pane_get_widget(dialog->pane, "controls_table")), ld->controls_count + 1, 2);
            l = GTK_LABEL(gtk_label_new(desc->PortNames[i]));
            gtk_label_set_justify(l, GTK_JUSTIFY_RIGHT);
            gtk_misc_set_alignment(GTK_MISC(l), 1, 1);
            gtk_widget_show(GTK_WIDGET(l));
            gtk_table_attach(GTK_TABLE(pane_get_widget(dialog->pane, "controls_table")),
                             GTK_WIDGET(l),
                             0, 1, j, j + 1,
                             GTK_FILL, GTK_FILL,
                             6, 0);
            DEBUG("port: %s, hints: 0x%x\n",
                  desc->PortNames[i],
                  desc->PortRangeHints[i].HintDescriptor);
            
            ld->controls[j] = 
                make_widget_from_port(ld->currently_selected,
                                      i,
                                      dialog->shl->clip->sr->rate,
                                      &ld->control_values[j], 
                                      &ld->control_min[j], 
                                      &ld->control_max[j]);
            ld->envelope1[j] = gtk_toggle_button_new_with_label("Env1");
            ld->envelope2[j] = gtk_toggle_button_new_with_label("Env2");
            gtk_tooltips_set_tip(ld->tips, ld->envelope1[j],
                                 "Assigns this parameter to the primary "
                                 "envelope.", ""); 
            gtk_tooltips_set_tip(ld->tips, ld->envelope2[j],
                                 "Assigns this parameter to the auxiliary "
                                 "envelope.", "");
            hbox = gtk_hbox_new(FALSE, 0);
            gtk_box_pack_start(GTK_BOX(hbox), ld->controls[j], TRUE, TRUE, 0);
            gtk_box_pack_start(GTK_BOX(hbox), ld->envelope1[j], FALSE, FALSE, 0);
            gtk_box_pack_start(GTK_BOX(hbox), ld->envelope2[j], FALSE, FALSE, 0);
            gtk_widget_show(ld->controls[j]);
            gtk_widget_show(ld->envelope1[j]);
            gtk_widget_show(ld->envelope2[j]);
            gtk_widget_show(hbox);
            
            gtk_table_attach(GTK_TABLE(pane_get_widget(dialog->pane, "controls_table")),
                             hbox,
                             1, 2, j, j + 1,
                             GTK_EXPAND | GTK_FILL, GTK_FILL,
                             0, 0);

            //            DEBUG("ld->envelope1[%d]: %p\n", i, (void *)ld->envelope1[i]);

            ld->controls_count++;
            j++;
        }
    }

}

static void
ladspa_exit(int id) {
    int i;
    char *err;

    for(i = 0; i < ladspa_count; i++) {

        if(!ladspa_modules[i].handle)
            continue;

        dlclose(ladspa_modules[i].handle);        
        if((err = dlerror()) != NULL) {
            FAIL("error: %s\n", err);
            continue;
        }
        
        ladspa_modules[i].handle = NULL;

    }

}

static void
ladspa_dialog_apply(struct dialog *dialog,
                    void *user_data) {
    int i;
    struct ladspa_dialog *ld = (struct ladspa_dialog *)dialog;
    struct ladspa_params *p = mem_alloc(sizeof(*p));
    GtkAdjustment *adj;

    g_return_if_fail(p != NULL);

    p->desc = ld->currently_selected;
    p->controls_count = ld->controls_count;

    DEBUG("p->controls_count: %d\n", p->controls_count);

    for(i = 0; i < p->controls_count; i++) {
        p->envelopes_enabled[i] = 0;
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ld->envelope1[i])))
            p->envelopes_enabled[i] |= MARKER_SLOPE;
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ld->envelope2[i])))
            p->envelopes_enabled[i] |= MARKER_SLOPE_AUX;

        if(GTK_IS_TOGGLE_BUTTON(ld->controls[i])) {
            p->control_values[i] = 
                gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ld->controls[i])) ? 0 : 1;
        } else {
            adj = gtk_range_get_adjustment(GTK_RANGE(ld->controls[i]));
            p->control_values[i] = adj->value;
            p->control_min[i] = ld->control_min[i];
            p->control_max[i] = ld->control_max[i];
        }
    }
    
    shell_dispatch_as(dialog->shl, 
                      CMD_NEW("execute-module", 
                              cmd_new_shellp_val(dialog->shl),
                              cmd_new_int_val(self_id),
                              cmd_new_voidp_val_with_dtor(p, cmd_voidp_dtor)),
                      p->desc->Name);
}

static void
ladspa_dialog_close(struct dialog *dialog,
                    void *user_data) {
    struct module_state *module_state = 
        shell_get_module_state(dialog->shl, self_id);

    module_state->is_open = 0;
    dialog_destroy(dialog);
}

static struct cmd_value *
ladspa_open(int id,
            shell *shl) {
    GtkWidget *presets;
    int i;
    char path[4096];
    struct dialog *dialog;
    struct cmd_value *r;
    struct module_state *module_state;
    struct pane_signal_bindings bindings[] = {
        { "clist", "select_row", on_clist_select_row },
        { "clist", "key-press-event", on_clist_key_press_event },
        { "delete_preset", "clicked", on_delete_preset_clicked }
    };
    struct pane_signal_bindings presets_bindings[] = {
        { "presets", "changed", on_presets_changed },
        { "presets_entry", "activate", on_presets_entry_activate }
    };

    if(ladspa_count == 0) 
        return cmd_new_error_val("Did not find any GLADSPA plugins. "
                                 "Check your LADSPA_PATH environment "
                                 "variable. Download GLADSPA plugins "
                                 "at http://ladspa.org.");
    
    snprintf(path, 4096, "%s/%s", module_get_path(id), LADSPA_GLADE_FILE);

    dialog = dialog_new(sizeof(struct ladspa_dialog),
                        path,
                        "dialog",
                        shl,
                        sizeof(bindings) / sizeof(bindings[0]),
                        bindings,
                        NULL);
    
    if(!dialog) 
        return cmd_new_error_val("Could not load interface %s.", path);

    dialog->apply = ladspa_dialog_apply;
    dialog->close = ladspa_dialog_close;

    ((struct ladspa_dialog *)dialog)->tips = gtk_tooltips_new();
    ((struct ladspa_dialog *)dialog)->controls_count = 0;
    ((struct ladspa_dialog *)dialog)->currently_selected = NULL;

    presets = combo_box_new();
    gtk_tooltips_set_tip(((struct ladspa_dialog *)dialog)->tips, 
                         combo_box_get_entry(COMBO_BOX(presets)),
                         "To save the current settings, type a name "
                         "and press Enter.", ""); 
    
    pane_register_widget(dialog->pane, "presets", presets);
    pane_register_widget(dialog->pane, "presets_entry", 
                         combo_box_get_entry(COMBO_BOX(presets)));
    pane_connect_bindings(dialog->pane,
                          (sizeof(presets_bindings) / 
                           sizeof(presets_bindings[0])),
                          presets_bindings,
                          dialog);
    gtk_widget_show(presets);
    gtk_table_attach(GTK_TABLE(pane_get_widget(dialog->pane, "presets_table")),
                     presets, 
                     1, 2, /* left, right */
                     0, 1, /* top, bottom */
                     GTK_EXPAND | GTK_FILL, 0, /* xoptions, yoptions */
                     0, 0); /* xpad, ypad */
    
    gtk_clist_freeze(GTK_CLIST(pane_get_widget(dialog->pane, "clist")));
    for(i = 0; i < ladspa_count; i++) 
        gtk_clist_append(GTK_CLIST(pane_get_widget(dialog->pane, "clist")), 
                         (gchar **)&ladspa_modules[i].desc->Name);
    gtk_clist_thaw(GTK_CLIST(pane_get_widget(dialog->pane, "clist")));
    gtk_clist_select_row(GTK_CLIST(pane_get_widget(dialog->pane, "clist")), 
                         0, 0);

    module_state = shell_get_module_state(shl, id);
    module_state->is_open = 1;
    module_state->data = dialog;

    return cmd_new_void_val();
}

static void
ladspa_close(int id,
             shell *shl) {
    struct module_state *module_state = shell_get_module_state(shl, id);
    dialog_destroy((struct dialog *)module_state->data);
}

static struct cmd_value *
ladspa_execute(int id,
               shell *shl,
               void *data) {
    AFframecount start = shl->select_start,
        end = shl->select_end;
    track_map_t map = shl->select_channel_map;
    int i, j, t;
    LADSPA_Descriptor *desc;
    LADSPA_Data dummy;
    int selected_tracks = 0;
    struct cmd *cmd;
    struct cmd_value *r;
    struct ladspa_params *p = data;
    const char *s;

    if(!p) 
        return cmd_new_error_val("GLADSPA does not have defaults.");

    if(!p->desc) 
        return cmd_new_error_val("No plugin selected.");

    desc = p->desc;
    
    p->audio_output_count = 
        ladspa_get_port_count_by_type(desc, 
                                      LADSPA_PORT_AUDIO | LADSPA_PORT_OUTPUT);
    p->audio_input_count = 
        ladspa_get_port_count_by_type(desc, 
                                      LADSPA_PORT_AUDIO | LADSPA_PORT_INPUT);

    for(t = 0; t < shl->clip->sr->channels; t++) 
        if((1 << t) & map) 
            selected_tracks++;

    if((p->audio_input_count > 1 || p->audio_output_count > 1) &&
       selected_tracks != MAX(p->audio_input_count, p->audio_output_count)) {
        return cmd_new_error_val("This plugin needs %d inputs and "
                                 "%d outputs. You need to select %d tracks.",
                                 p->audio_input_count, p->audio_output_count,
                                 MAX(p->audio_input_count, 
                                     p->audio_output_count));
    }

    p->handle = desc->instantiate(desc, shl->clip->sr->rate);
    
    if(!p->handle) 
        return cmd_new_error_val("Cannot instantiate plugin \n%s\n",
                                 desc->Name);
    
    /* Construct envelope -> control port arrays. */

    p->envelope1_connect_count = p->envelope2_connect_count = 0;
    for(i = 0; i < p->controls_count; i++) {
        p->envelope1_connections[i] = NULL;
        p->envelope2_connections[i] = NULL;
        if(p->envelopes_enabled[i] & MARKER_SLOPE) {
            p->envelope1_connections[p->envelope1_connect_count] = 
                &p->control_values[i];
            p->envelope1_defaults[p->envelope1_connect_count] =
                p->control_values[i];
            p->envelope1_min[p->envelope1_connect_count] =
                p->control_min[i];
            p->envelope1_max[p->envelope1_connect_count] =
                p->control_max[i];
            p->envelope1_connect_count++;
        }
        if(p->envelopes_enabled[i] & MARKER_SLOPE_AUX) {
            p->envelope2_connections[p->envelope2_connect_count] = 
                &p->control_values[i];
            p->envelope2_defaults[p->envelope2_connect_count] =
                p->control_values[i];
            p->envelope2_min[p->envelope2_connect_count] =
                p->control_min[i];
            p->envelope2_max[p->envelope2_connect_count] =
                p->control_max[i];
            p->envelope2_connect_count++;
        }
    }

    /* Map port numbers for audio inputs and outputs. */

    p->audio_input_count = p->audio_output_count = 0;
    for(i = 0; i < desc->PortCount; i++) {
        if(LADSPA_IS_PORT_AUDIO(desc->PortDescriptors[i])) {
            if(LADSPA_IS_PORT_INPUT(desc->PortDescriptors[i]))
                p->audio_input_map[p->audio_input_count++] = i;
            else
                p->audio_output_map[p->audio_output_count++] = i;
        }
    }

    /* Allocate I/O buffers. */

    for(i = 0; i < MAX_AUDIO_INPUTS; i++) 
        p->in_buf[i] = NULL;
    for(i = 0; i < MAX_AUDIO_OUTPUTS; i++) 
        p->out_buf[i] = NULL;

    for(i = 0; i < p->audio_input_count; i++) {
        p->in_buf[i] = mem_calloc(sizeof(float), EFFECT_BUF_SIZE);
        DEBUG("input %d at %p\n", i, p->in_buf[i]);
        if(!p->in_buf[i]) {
            for(i = i-1; i; i--)
                free(p->in_buf[i]);
            FAIL("Not enough memory to allocate input buffers!");
            return cmd_new_error_val("Could not allocate input buffers!");
        }
    }
    for(i = 0; i < p->audio_output_count; i++) {
        p->out_buf[i] = mem_calloc(sizeof(float), EFFECT_BUF_SIZE);
        DEBUG("output %d at %p\n", i, p->out_buf[i]);
        if(!p->out_buf[i]) {
            for(i = i-1; i; i--) 
                free(p->out_buf[i]);
            for(i = 0; i < p->audio_input_count; i++) 
                free(p->in_buf[i]);
            FAIL("Not enough memory to allocate output buffer!");
            return cmd_new_error_val("Could not allocate output buffer!");
        }
    }

    /* Connect control ports. */

    for(i = 0, j = 0; i < desc->PortCount; i++) {
        if(LADSPA_IS_PORT_CONTROL(desc->PortDescriptors[i])) {
            printf("connecting control port: %s\n", desc->PortNames[i]);
            if(LADSPA_IS_PORT_INPUT(desc->PortDescriptors[i])) {
                desc->connect_port(p->handle, i, &p->control_values[j++]);
            } else {
                desc->connect_port(p->handle, i, &dummy);
            }
        }
    }

    if(desc->activate)
        desc->activate(p->handle);

    /* Preserve the selection. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(start),
                  cmd_new_long_val(end - start));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) 
        goto cleanup;
    cmd_destroy_value(r);

    if((s = constraints_test(shl->constraints,
                             region_new(map, start, end - start),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot apply %s because region is locked "
                                 "(%s)", p->desc->Name, s);

    constraints_push(shl->constraints,
                     p->desc->Name,
                     region_new(map, start, end - start),
                     (CONSTRAIN_POSITION | 
                      CONSTRAIN_LENGTH | 
                      CONSTRAIN_CONTENTS));

    /* Do it. */

    rwlock_rlock(&shl->clip->sr->rwl);

    if(p->audio_input_count < 2 && p->audio_output_count < 2)
        ladspa_process_1on1(shl,
                            p,
                            map, start, end);
    else
        ladspa_process_many(shl,
                            p,
                            map, start, end);
        
    rwlock_runlock(&shl->clip->sr->rwl);

    constraints_pop(shl->constraints);

    r = cmd_new_void_val();

 cleanup:
    view_set_progress(shl->view, 0);
    
    for(i = 0; i < p->audio_input_count; i++)
        if(p->in_buf[i])
            mem_free(p->in_buf[i]);

    for(i = 0; i < p->audio_output_count; i++)
        if(p->out_buf[i])
            mem_free(p->out_buf[i]);

    if(desc->deactivate)
        desc->deactivate(p->handle);

    if(desc->cleanup)
        desc->cleanup(p->handle);

    return r;

}
static struct pref default_prefs[] = { 
    PREF_STRING("ladspa.path", "/usr/local/lib/ladspa",
                "Path to GLADSPA plugins."), 
    PREF_FLOAT("ladspa.segment_time", 0.01, 0.00001, 10000,
               "Specifies the granularity with which envelopes are applied "
               "to GLADSPA plugin processing, in seconds."),
};

static int
ladspa_init(int id) {
    char *path, *p;
    wordexp_t exp;
    int r, i;

    self_id = id;

    pref_register(sizeof(default_prefs) / sizeof(default_prefs[0]),
                  default_prefs);
    pref_load("ladspa.*");
    ladspa_path = getenv("LADSPA_PATH");
    if(!ladspa_path) {
        ladspa_path = pref_get_as_string("ladspa.path");
        FAIL("Warning: You do not have a LADSPA_PATH "
             "environment variable set. Trying default %s.\n", ladspa_path);
    }
    path = strdup(ladspa_path);

    while((p = strsep(&path, ":"))) {
        r = wordexp(p, &exp, 0);
        if(r) {
            FAIL("unable to initalize GLADSPA plugins in path %s, "
                 "invalid path?\n",
                 p);
            continue;
        }
        DEBUG("collecting GLADSPA plugins in path %s\n", p);
        for(i = 0; i < exp.we_wordc; i++) 
            ladspa_collect(exp.we_wordv[i]);
        wordfree(&exp);
    }
    free(path);
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "GLADSPA Plugins",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2003-2004",
    "GPL",
    NULL,
    0,

    ladspa_init,
    NULL,
    ladspa_open,
    ladspa_execute,
    ladspa_close,
    NULL,
    ladspa_exit
};
