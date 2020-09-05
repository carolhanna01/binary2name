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

#ifndef HAVE_LAME
#warning "Not building LAME support."
#else

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <lame/lame.h>

#define ENCODER_FAST     1
#define ENCODER_STANDARD 2
#define ENCODER_HQ       3

#define TARGET_BITRATE   1
#define TARGET_QUALITY   2

#define VBR_MODE_OLD     1
#define VBR_MODE_NEW     2

static int lamemp3_bitrates[] = {
    32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320
};

struct lamemp3_file_info {
    lame_global_flags *gfp;
    mp3data_struct mp3data;

    int opened_for_reading;

    FILE *stream;

    int target;

    int encoder;
    int mono;

    int bitrate;
    int use_cbr;

    int quality;
    int vbr_mode;

    int first_bitrate;
    int total_bitrate;
    int num_headers;

    GtkWidget *file_config_widget;
    struct pane *file_config_pane;
};

static int self_id;

static struct file_driver lamemp3_driver;

static struct cmd_value *
lamemp3_attach(struct file *file,
               const char *format) {
    struct lamemp3_file_info *lfi = mem_calloc(1, sizeof(*lfi));

    if(!lfi)
        return cmd_new_error_val("Cannot allocate LAME parameters");

    lfi->target = pref_get_as_int("lame.target");
    lfi->encoder = pref_get_as_int("lame.encoder");
    lfi->mono = pref_get_as_int("lame.mono");
    lfi->bitrate = pref_get_as_int("lame.bitrate");
    lfi->use_cbr = pref_get_as_int("lame.use_cbr");
    lfi->quality = pref_get_as_int("lame.quality");
    lfi->vbr_mode = pref_get_as_int("lame.vbr_mode");
    lfi->gfp = NULL;

    file->driver = &lamemp3_driver;
    file->driver_data = lfi;

    return cmd_new_void_val();
}

static int
lamemp3_identify(struct file *file) {
    unsigned char mp3buf[1024];
    short pcm_l[1152], pcm_r[1152];
    int tries = 0;
    FILE *f;
    int r;
    mp3data_struct mp3data = { 0 };
    struct lamemp3_file_info *lfi = file->driver_data;

    f = fopen(file->name, "r");
    if(!f) 
        return 1;

    r = lame_decode_init();
    
    if(r) {
        fclose(f);
        return 1;
    }

    while(1) {
        r = fread(mp3buf, 1, sizeof(mp3buf), f);
        
        if(r <= 0) {
            lame_decode_exit();
            fclose(f);
            return 1;
        }
        
        r = lame_decode1_headers(mp3buf, r, pcm_l, pcm_r, &mp3data);

        DEBUG("lame_decode1_headers returns %d\n", r);

        if(r == -1) {
            lame_decode_exit();
            fclose(f);
            return 1;
        }

        if(r > 0)
            break;

        /* After a few tries we give up. */

        tries++;
        if(tries >= 2) {
            lame_decode_exit();
            fclose(f);
            return 1;
        }
    }

    if(!mp3data.header_parsed) {
        lame_decode_exit();
        fclose(f);
        return 1;
    }
        
    lfi->mp3data = mp3data;

    DEBUG("identify success\n");

    lame_decode_exit();
    fclose(f);
    
    return 0;
}

static struct cmd_value *
lamemp3_open_for_reading(struct file *file,
                         struct file_params *params) {
    struct lamemp3_file_info *lfi = file->driver_data;
    int r;

    r = lamemp3_identify(file);
    
    if(r)
        return cmd_new_error_val("Cannot open %s", file->name);

    r = lame_decode_init();

    if(r) 
        return cmd_new_error_val("Failed to initialize LAME decoder");

    lfi->stream = fopen(file->name, "r");
    lfi->num_headers = 0;
    lfi->total_bitrate = 0;
    lfi->first_bitrate = 0;
    lfi->opened_for_reading = 1;
    lfi->use_cbr = 1;

    params->channels = lfi->mp3data.stereo;
    params->sample_type = SAMPLE_TYPE_INT_16;
    params->frame_count = lfi->mp3data.nsamp ? lfi->mp3data.nsamp : -1;
    params->sample_rate = lfi->mp3data.samplerate;

    return cmd_new_void_val();
}

static struct cmd_value *
lamemp3_open_for_writing(struct file *file,
                         struct file_params *params) {
    struct lamemp3_file_info *lfi = file->driver_data;
    int r;
    FILE *f;

    /* FIXME: need to convert sample data if necessary. */

    if(params->sample_type != SAMPLE_TYPE_INT_16)
        return cmd_new_error_val("Can only encode 16 bit int sample data");

    f = fopen(file->name, "wb");
    if(!f) {
        FAIL("could not open %s\n", file->name);
        return cmd_new_error_val("Could not open %s: %s",
                                 file->name, strerror(errno));
    }

    
    lfi->opened_for_reading = 0;
    lfi->stream = f;

    lfi->gfp = lame_init();

    if(!lfi->gfp) {
        FAIL("could not initialize LAME\n");
        fclose(lfi->stream);
        return cmd_new_error_val("Could not initialize LAME");
    }
    /*
    lame_set_errorf(lfi->gfp,error_handler_function);
    lame_set_debugf(lfi->gfp,error_handler_function);
    lame_set_msgf(lfi->gfp,error_handler_function);
    */

    lame_set_num_channels(lfi->gfp, params->channels);
    lame_set_in_samplerate(lfi->gfp, params->sample_rate);
    if(lfi->mono)
        lame_set_mode(lfi->gfp, 3);
    else
        lame_set_mode(lfi->gfp, 0);

    if(lfi->encoder == ENCODER_FAST)
        lame_set_quality(lfi->gfp, 7);   /* 2=high  5 = medium  7=low */
    else if(lfi->encoder == ENCODER_STANDARD) 
        lame_set_quality(lfi->gfp, 5);   /* 2=high  5 = medium  7=low */
    else if(lfi->encoder == ENCODER_HQ) 
        lame_set_quality(lfi->gfp, 2);   /* 2=high  5 = medium  7=low */
    if(lfi->target == TARGET_BITRATE) {
        lame_set_brate(lfi->gfp, lfi->bitrate);
        if(!lfi->use_cbr) {
            lame_set_VBR(lfi->gfp, vbr_abr);
            lame_set_VBR_mean_bitrate_kbps(lfi->gfp, lfi->bitrate);
        }
    } else {
        if(lfi->vbr_mode == VBR_MODE_OLD) {
            lame_set_VBR(lfi->gfp, vbr_rh);
        } else {
            lame_set_VBR(lfi->gfp, vbr_mtrh);
        }

        lame_set_VBR(lfi->gfp, vbr_default);
        lame_set_VBR_q(lfi->gfp, 9 - lfi->quality);
    } 

    r = lame_init_params(lfi->gfp);

    if(r < 0) {
        FAIL("could not initialize LAME parameters\n");
        fclose(lfi->stream);
        lame_close(lfi->gfp);
        return cmd_new_error_val("Could not initialize LAME parameters.");
    }

    return cmd_new_void_val();
}

static struct cmd_value *
lamemp3_open(struct file *file,
             const char *mode,
             struct file_params *params) {
    if(mode[0] == 'r')
        return lamemp3_open_for_reading(file, params);
    else if(mode[0] == 'w')
        return lamemp3_open_for_writing(file, params);
    else
        return cmd_new_error_val("Unknown mode %s", mode);
}

static long
lamemp3_read(struct file *file,
             void *buf,
             long count) {
    struct lamemp3_file_info *lfi = file->driver_data;
    char mp3buf[1024];
    short pcm_l[1152], pcm_r[1152], *pcm[2] = { pcm_l, pcm_r };
    size_t r;
    mp3data_struct mp3data;

    while(1) {
        r = fread(mp3buf, 1, sizeof(mp3buf), lfi->stream);
        
        if(r <= 0) {
            /* Maybe some data is still buffered. */
            r = lame_decode1_headers(mp3buf, r, pcm[0], pcm[1], &mp3data);
            if(r > 0)
                goto flush;
            return r;
        }
        
        r = lame_decode1_headers(mp3buf, r, pcm[0], pcm[1], &mp3data);
        if(r == -1) {
            DEBUG("lame_decode1 returned -1\n");
            return -1;
        }

        if(r > 0)
            break;
    }

 flush:

    lfi->num_headers++;
    if(lfi->first_bitrate == 0)
        lfi->first_bitrate = mp3data.bitrate;
    else
        if(lfi->first_bitrate != mp3data.bitrate)
            lfi->use_cbr = 0;
    lfi->total_bitrate += mp3data.bitrate;

    if(lfi->mp3data.stereo > 1) 
        sample_interleave(SAMPLE_TYPE_INT_16,
                          buf,
                          pcm,
                          lfi->mp3data.stereo,
                          0,
                          r);
    else
        memcpy(buf, pcm_l, sizeof(short) * r);

    return r;
}

static long
lamemp3_write(struct file *file,
              void *buf,
              long count) {
    struct lamemp3_file_info *lfi = file->driver_data;
    unsigned char *mp3buf = mem_alloc(1.25 * (float)count + 7200);
    int r;

    if(!mp3buf)
        return -1;

    r = lame_encode_buffer_interleaved(lfi->gfp, 
                                       buf, 
                                       count, 
                                       mp3buf, 
                                       0);
    if(r < 1) {
        free(mp3buf);
        return -1;
    }
    
    r = fwrite(mp3buf, 1, r, lfi->stream);
    free(mp3buf);

    /* FIXME: hard to know how many frames were actually written. */

    return count;
}

static struct cmd_value *
lamemp3_close(struct file *file) {
    struct lamemp3_file_info *lfi = file->driver_data;
    unsigned char *mp3buf = mem_alloc(7200);
    struct cmd_value *err = NULL;
    int r;

    DEBUG("closing %s\n", file->name);

    if(lfi->opened_for_reading) {
        lame_decode_exit();
        if(lfi->stream)
            fclose(lfi->stream);
        if(lfi->num_headers > 0)
            lfi->bitrate = lfi->total_bitrate / lfi->num_headers;
        else
            lfi->bitrate = lfi->total_bitrate;
        lfi->bitrate -= lfi->bitrate % 8;
        lfi->bitrate = CLAMP(lfi->bitrate, 8, 320);
    } else {
        if(lfi->gfp) {
            if(!mp3buf) {
                err = cmd_new_error_val("Could not flush output");
            } else {
                r = lame_encode_flush(lfi->gfp, mp3buf, 0);
                fwrite(mp3buf, 1, r, lfi->stream);
            }
            
            fflush(lfi->stream);
            lame_mp3_tags_fid(lfi->gfp, lfi->stream);
            
            lame_close(lfi->gfp);
            lfi->gfp = NULL;
            
            r = fclose(lfi->stream);
            if(r == -1)
                err = cmd_new_error_val("Error closing output: %s",
                                    strerror(errno));
            
        }
    }
    
    lfi->stream = NULL;
        
    if(mp3buf)
        mem_free(mp3buf);

    return err ? err : cmd_new_void_val();
}

static void
lamemp3_detach(struct file *file) {
    mem_free(file->driver_data);
    file->driver = NULL;
}

static int
lamemp3_snprint(struct file *file,
                enum file_property what,
                char *s,
                int len) {    
    struct lamemp3_file_info *lfi = file->driver_data;

    assert(file->driver == &lamemp3_driver);
    assert(file->driver_data != NULL);

    switch(what) {
    case FILE_FORMAT:
        return snprintf(s, len, "MP3");
    case FILE_DETAILED_FORMAT:
        if(lfi->target == TARGET_BITRATE) {
            return snprintf(s, len, "MP3 %d kbps %s",
                            lfi->bitrate, lfi->use_cbr ? "CBR" : "ABR");
        } else {
            return snprintf(s, len, "MP3 VBR quality %d",
                            lfi->quality);
        }
    default:
        return snprintf(s, len, "unsupported");
    }
        
}

static const struct file_format formats[] = {
    { "MP3", "mp3" },
    { NULL, NULL },
};

static const struct file_format *
lamemp3_get_read_formats() {
    return formats;
}

static const struct file_format *
lamemp3_get_write_formats() {
    return formats;
}


# define LAME_GLADE_FILE "file_lame" GUI_GTK_VERSION_TAG ".glade"

static void
lamemp3_sync_display(struct file *file) {
    struct lamemp3_file_info *lfi = file->driver_data;
    gboolean bitrate_settings = FALSE;

    if(lfi->target == TARGET_BITRATE)
        bitrate_settings = TRUE;

    gtk_widget_set_sensitive(pane_get_widget(lfi->file_config_pane, "lame_bitrate_settings"), bitrate_settings);
    gtk_widget_set_sensitive(pane_get_widget(lfi->file_config_pane, "lame_quality_settings"), !bitrate_settings);
    
    if(bitrate_settings) 
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_target_bitrate")), TRUE); 
    else
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_target_quality")), TRUE); 

    combo_box_set_active(COMBO_BOX(pane_get_widget(lfi->file_config_pane, "lame_encoder_quality")), lfi->encoder - 1);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_encoder_mono")), lfi->mono); 
    
    gtk_adjustment_set_value(gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(lfi->file_config_pane, "lame_bitrate"))), lfi->bitrate);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_bitrate_cbr")), lfi->use_cbr); 

    gtk_adjustment_set_value(gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(lfi->file_config_pane, "lame_quality"))), lfi->quality);
    
    if(lfi->vbr_mode == VBR_MODE_NEW) 
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_vbr_mode_new")), TRUE); 
    else
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_vbr_mode_old")), TRUE); 
}

static void
lamemp3_on_target_toggled(GtkWidget *w,
                          void *user_data) {
    static int ignore = 0;
    struct file *file = user_data;
    struct lamemp3_file_info *lfi = file->driver_data;

    if(ignore)
        return;
    ignore = 1;

    lfi->target = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_target_bitrate"))) ? TARGET_BITRATE : TARGET_QUALITY;
    
    lamemp3_sync_display(file);
    ignore = 0;
}

static void
lamemp3_on_bitrate_changed(GtkWidget *w,
                           void *user_data) {
    GtkAdjustment *adjustment = GTK_ADJUSTMENT(w);
    
    pref_set_int("lame.bitrate", adjustment->value);
    if(pref_get_as_int("lame.bitrate") != adjustment->value) {
        adjustment->value = pref_get_as_int("lame.bitrate");
        gtk_adjustment_value_changed(adjustment);
    }
}

static void
lamemp3_close_file_config(struct file *file) {
    struct lamemp3_file_info *lfi = file->driver_data;
    pane_destroy(lfi->file_config_pane);
}

static void 
lamemp3_commit_file_config(struct file *file) {
    GtkAdjustment *adjustment;
    struct lamemp3_file_info *lfi = file->driver_data;

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_target_bitrate")))) 
        lfi->target = TARGET_BITRATE;
    else
        lfi->target = TARGET_QUALITY;

    if(!strcmp("Fast", combo_box_get_value(COMBO_BOX(pane_get_widget(lfi->file_config_pane, "lame_encoder_quality"))))) {
        lfi->encoder = ENCODER_FAST;
    } else if(!strcmp("Standard", combo_box_get_value(COMBO_BOX(pane_get_widget(lfi->file_config_pane, "lame_encoder_quality"))))) {
        lfi->encoder = ENCODER_STANDARD;
    } else {
        lfi->encoder = ENCODER_HQ;
    }

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_encoder_mono"))))
        lfi->mono = 1;
    else
        lfi->mono = 0;

    lfi->bitrate = 0;
    lfi->use_cbr = 0;
    lfi->quality = 0;
    lfi->vbr_mode = 0;
    
    if(lfi->target == TARGET_BITRATE) {
        adjustment = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(lfi->file_config_pane, "lame_bitrate")));
        lfi->bitrate = CLAMP(adjustment->value, 8, 320);
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_bitrate_cbr"))))
            lfi->use_cbr = 1;
    } else {
        adjustment = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(lfi->file_config_pane, "lame_quality")));
        lfi->quality = CLAMP(adjustment->value, 0, 9);
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(lfi->file_config_pane, "lame_vbr_mode_old"))))
            lfi->vbr_mode = VBR_MODE_OLD;
        else
            lfi->vbr_mode = VBR_MODE_NEW;
    }

    DEBUG("Encoder: %s\n", lfi->encoder == ENCODER_FAST ? "Fast" : 
          (lfi->encoder == ENCODER_STANDARD ? "Standard" : "High Quality"));
    DEBUG("Target: %s\n", lfi->target == TARGET_BITRATE ? 
          "Bitrate" : "Quality");
    if(lfi->target == TARGET_BITRATE) {
        DEBUG("Bitrate: %d\n", lfi->bitrate);
        DEBUG("Use CBR: %s\n", lfi->use_cbr ? "Yes" : "No");
    } else {
        DEBUG("Quality: %d\n", lfi->quality);
        DEBUG("VBR mode: %s\n", lfi->vbr_mode == VBR_MODE_OLD ? 
              "Old" : "New");
    }

    pref_set_int("lame.target", lfi->target);
    pref_set_int("lame.encoder", lfi->encoder);
    pref_set_int("lame.mono", lfi->mono);
    pref_set_int("lame.bitrate", lfi->bitrate);
    pref_set_int("lame.use_cbr", lfi->use_cbr);
    pref_set_int("lame.quality", lfi->quality);
    pref_set_int("lame.vbr_mode", lfi->vbr_mode);
    pref_sync();
}

static struct pane_signal_bindings bindings[] = {
    { "lame_target_bitrate", "toggled", lamemp3_on_target_toggled }
};

static GtkWidget *
lamemp3_open_file_config(struct file *file,
                         const char *format) {
    GladeXML *xml = NULL;
    GtkWidget *w = NULL, *encoder_quality;
    GtkAdjustment *adjustment;
    GList *l = NULL;
    char path[4096];
    struct pane *config_pane = NULL;
    struct lamemp3_file_info *lfi = file->driver_data;

    /* Load the XML file. */

    snprintf(path, sizeof(path), "%s/%s", module_get_path(self_id), 
             LAME_GLADE_FILE);
    DEBUG("loading interface %s\n", path);
    xml = glade_xml_new(path, NULL, NULL);
    
    if(!xml) {
        FAIL("could not find interface definition, " "looked at %s\n", 
             path);
        return NULL;
    }

    /* Associate a pane with the XML file. */

    config_pane = pane_new(xml);
    if(!config_pane) {
        FAIL("could not create pane for configuration\n");
        g_object_unref(G_OBJECT(xml));
        return NULL;
    }

    w = pane_get_widget(config_pane, "lame_config_panel");
    gtk_widget_ref(w);
    gtk_container_remove(GTK_CONTAINER(pane_get_widget(config_pane, "lame_config")), w);

    adjustment = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(config_pane, "lame_bitrate")));
    g_signal_connect(GTK_OBJECT(adjustment), "value-changed", 
                     G_CALLBACK(lamemp3_on_bitrate_changed), file);

    encoder_quality = combo_box_new();
    gtk_widget_show(encoder_quality);
    l = g_list_append(l, "Fast");
    l = g_list_append(l, "Standard");
    l = g_list_append(l, "High Quality");
    combo_box_set_strings(COMBO_BOX(encoder_quality), l);
    g_list_free(l);
    combo_box_set_editable(COMBO_BOX(encoder_quality), FALSE);
    gtk_box_pack_start(GTK_BOX(pane_get_widget(config_pane, "lame_encoder_options")), encoder_quality, TRUE, TRUE, 0);
    
    pane_register_widget(config_pane, "lame_encoder_quality", encoder_quality);

    pane_connect_bindings(config_pane, 
                          sizeof(bindings) / sizeof(bindings[0]),
                          bindings, 
                          file);

    lfi->file_config_pane = config_pane;
    lfi->file_config_widget = w;
    g_object_set_data(G_OBJECT(w), "user_data", file);
    DEBUG("returning widget %p\n", w);

    lamemp3_sync_display(file);

    return w;
}

static struct file_driver lamemp3_driver = {
    "LAME",
    lamemp3_attach,
    lamemp3_open,
    lamemp3_read,
    lamemp3_write,
    lamemp3_close,
    lamemp3_detach,

    lamemp3_snprint,

    lamemp3_get_read_formats,
    lamemp3_get_write_formats,

    NULL,
    NULL,
    NULL,

    lamemp3_open_file_config,
    lamemp3_commit_file_config,
    lamemp3_close_file_config
};

static int 
lamemp3_constrain_bitrate(const struct pref *pref,
                          union pref_value *value) {
    int i, min = 1000, best = 0;
    for(i = 0; i < sizeof(lamemp3_bitrates) / sizeof(lamemp3_bitrates[0]); i++) {
        if(ABS(value->i - lamemp3_bitrates[i]) < min) {
            min = value->i - lamemp3_bitrates[i];
            best = i;
        }
    }
    value->i = lamemp3_bitrates[best];
    return 0;
}

static struct pref default_prefs[] = { 

    PREF_INT("lame.target", 1, 1, 2,
             "Whether the encoder should target a certain quality (giving "
             "a variable bitrate), or whether to encoder should target a "
             "certain bitrate."),
    PREF_INT("lame.encoder", 2, 1, 3,
             "Undocumented."),
    PREF_BOOL("lame.mono", 0, "Whether to use mono encoding."),
    PREF_INT_FULL("lame.bitrate", 0, 192, 32, 320, lamemp3_constrain_bitrate,
                  "The bitrate to use."),
    PREF_BOOL("lame.use_cbr", 0, "Whether to keep the bitrate constant."),
    PREF_INT("lame.quality", 5, 0, 9, "The quality level to achieve when "
             "the encoder should target quality."),
    PREF_INT("lame.vbr_mode", 2, 1, 2, "Whether to use the old style VBR "
             "encoder or the new one."),
    
};

static int
lamemp3_module_init(int id) {
    self_id = id;
    pref_register(sizeof(default_prefs) / sizeof(default_prefs[0]),
                  default_prefs);
    pref_load("lame.*");
    file_register_driver(&lamemp3_driver);
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "LAME MP3 driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    lamemp3_module_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif /* HAVE_LAME */
