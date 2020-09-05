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

/*
 * This implements support for the Advanced Linux Sound Architecture.
 */

#include <config.h>

#ifndef HAVE_ALSA

#warning "Not building ALSA driver."

#else /* HAVE_ALSA */

#include <gnusound.h>
#include <alsa/asoundlib.h>

struct alsa_data {
    snd_pcm_t *play_handle;
    snd_pcm_t *record_handle;
    snd_pcm_sframes_t buffer_size;
    snd_pcm_sframes_t period_size;
    snd_pcm_sframes_t delay;
    snd_output_t *output;
    int err;
    int sample_rate;
    size_t playback_buffer_size;
    size_t record_buffer_size;
};

static int self_id;

size_t 
alsa_get_playback_buffer_size(struct player *p) {
    struct alsa_data *dd = p->driver_data;
    return dd->playback_buffer_size;
}

size_t 
alsa_get_record_buffer_size(struct player *p) {
    struct alsa_data *dd = p->driver_data;
    return dd->record_buffer_size;
}

int
alsa_get_sample_rate(struct player *p) {
    struct alsa_data *dd = p->driver_data;
    return dd->sample_rate;
}

AFframecount
alsa_get_audio_delay(struct player *p) {
    struct alsa_data *dd = p->driver_data;
    //    snd_pcm_state_t state = snd_pcm_state(dd->play_handle);
    //    if(state != SND_PCM_STATE_RUNNING)
    //        return 0;
    snd_pcm_delay(dd->play_handle, &dd->delay);
    return dd->delay;
}

unsigned int
alsa_get_input_channels() {
    return pref_get_as_int("alsa.input_channels");
}

unsigned int
alsa_get_output_channels() {
    return pref_get_as_int("alsa.output_channels");
}

int 
alsa_xrun_recovery(snd_pcm_t *handle, 
                   int err) {
    if (err == -EPIPE) {    /* under-run */
        err = snd_pcm_prepare(handle);
        if (err < 0)
            printf("Can't recover from underrun, prepare failed: %s\n", snd_strerror(err));
        return 0;
    } else if (err == -ESTRPIPE) {
        while ((err = snd_pcm_resume(handle)) == -EAGAIN)
            sleep(1);       /* wait until the suspend flag is released */
        if (err < 0) {
            err = snd_pcm_prepare(handle);
            if (err < 0)
                printf("Can't recover from suspend, prepare failed: %s\n", snd_strerror(err));
        }
        return 0;
    }
    return err;
}

int
alsa_handle_errors(struct player *p,
                   snd_pcm_t *handle,
                   AFframecount pos,
                   snd_pcm_sframes_t *err,
                   int recover_xruns,
                   int is_underrun) {
    char s[20];
    if(*err >= 0)
        return *err;
    switch(*err) {
    case -EPIPE:
    case -ESTRPIPE:
        FAIL("xrun\n");
        grid_format(&p->shl->grid, pos, s, sizeof s, GRID_FORMAT_LONG);
        view_set_transient(p->shl->view, MSG_WARN, "%srun at %s", 
                           is_underrun ? "Under" : "Over", s);
        if(!recover_xruns) 
            return *err;
        if(alsa_xrun_recovery(handle, *err)) {
            FAIL("unrecoverable %srun error on %s: %s\n", 
                 is_underrun ? "under" : "over",
                 snd_pcm_name(handle), snd_strerror(*err));
            return *err;
        }
        player_set_position(p, pos);
        *err = 0;
        return 0;
    case -EAGAIN:
        *err = 0;
        return 0;
    default:
        FAIL("error on %s: %s\n", snd_pcm_name(handle), snd_strerror(*err));
        return *err;
    }
    return *err;
}

int
alsa_play(struct player *p,
          AFframecount frame_count) {
    struct alsa_data *dd = p->driver_data;
    int bytes_per_frame = sample_get_width(p->state->sample_type) * 
        p->shl->clip->mixer->output_channels;
    snd_pcm_sframes_t err = 0, avail;
    void *buf;

    /*
     * Find out how many frames the audio device can accept and fetch
     * at most that amount to play back.
     */

    if((avail = snd_pcm_avail_update(dd->play_handle)) < 0) {
        FAIL("snd_pcm_avail_update: %s\n", snd_strerror(avail));
        if(alsa_handle_errors(p,
                              dd->play_handle, 
                              p->state->playback_pos,
                              &avail, 
                              pref_get_as_int("alsa.underrun_recovery"),
                              1) < 0) 
            return avail;

        /*
         * If the error was not serious or recoverable then return
         * success and hope for the best on the next iteration.
         */

        return 0;
    }

    frame_count = MIN(frame_count, avail);
    if((err = player_get_playback_bufi(p, &buf, &frame_count))) {
        FAIL("error getting playback buffer\n");
        return err;
    }

    /* 
     * Must keep playing as long as there is data to record because
     * the playback and record devices are linked. If we don't have
     * anything to play back, just play back silence. FIXME: The ALSA
     * API has some functions like set_silence_threshold() and
     * set_silence_size() which seem to do this automatically, but I
     * can't figure out how they work.
     */

    if(player_get_record_avail(p) > frame_count) {
        memset(buf + (frame_count * bytes_per_frame), 
               '\0', 
               (avail - frame_count) * bytes_per_frame);
        frame_count = avail;
    }

    //    DEBUG("writing %ld frames...\n", frame_count);
    err = snd_pcm_writei(dd->play_handle, buf, frame_count);
    //    DEBUG("...returned from write, result: %ld\n", err);
    if(alsa_handle_errors(p,
                          dd->play_handle, 
                          p->state->playback_pos,
                          &err, 
                          pref_get_as_int("alsa.underrun_recovery"),
                          1) < 0) 
        return err;
    
    snd_pcm_delay(dd->play_handle, &dd->delay);

    if((err = player_flush_playback_bufi(p, err))) {
        FAIL("error flushing playback buffer\n");
        return err;
    }

    return err;
}

AFframecount
alsa_record(struct player *p,
            AFframecount frame_count) {
    struct alsa_data *dd = p->driver_data;
    void *buf;
    snd_pcm_sframes_t err = 0;
    
    if((err = player_get_record_bufi(p, &buf, &frame_count))) {
        FAIL("error getting record buffer\n");
        return err;
    }
    
    //    DEBUG("entering snd_pcm_readi(%ld)...\n", (long)frame_count);
    err = snd_pcm_readi(dd->record_handle, buf, frame_count);
    //    DEBUG("...snd_pcm_readi returned %ld\n", err);
    if(alsa_handle_errors(p,
                          dd->record_handle, 
                          p->state->record_pos,
                          &err, 
                          pref_get_as_int("alsa.overrun_recovery"),
                          0) < 0) 
        return err;

    if(err == 0)
        return 0;

    if((err = player_flush_record_bufi(p, err))) {
        FAIL("error flushing record buffer\n");
        return err;
    }
    
    return 0;
}

int
alsa_transfer(struct player *p) {
    struct alsa_data *dd = p->driver_data;
    AFframecount err;
    int started = 0;

    while(player_has_work(p)) {
        if((err = alsa_play(p, player_get_playback_avail(p))) < 0) {
            FAIL("playback error, err: %ld\n", err);
            view_set_transient(p->shl->view, MSG_ERR, "Playback error %d", err);
            return err;
        }

        /*
        if(!started) {
            snd_pcm_start(dd->play_handle);
            started = 1;
        }
        */

        if(p->state->record_mode) {
            if((err = alsa_record(p, player_get_record_avail(p))) < 0) {
                FAIL("record error, err: %ld\n", err);
                view_set_transient(p->shl->view, MSG_ERR, "Record error %d", err);
                return err;
            }
        }
        //        snd_pcm_delay(dd->play_handle, &dd->delay);

        snd_pcm_wait(dd->play_handle, 1000);
    }

    /* Make sure final samples are played. */

    snd_pcm_nonblock(dd->play_handle, 0);
    return dd->err;
}

void
alsa_close(struct player *p) {
    struct alsa_data *dd = p->driver_data;

    if(dd->record_handle) {
        snd_pcm_drop(dd->record_handle);
        snd_pcm_close(dd->record_handle);
        //        snd_pcm_hw_free(dd->record_handle);
        dd->record_handle = NULL;
    }

    if(dd->play_handle) {
        snd_pcm_delay(dd->play_handle, &dd->delay);
        snd_pcm_drain(dd->play_handle);
        snd_pcm_close(dd->play_handle);
        //        snd_pcm_hw_free(dd->play_handle);
        dd->play_handle = NULL;
    }

    if(dd->output)
        snd_output_close(dd->output);
}

int 
set_swparams(struct player *p,
             snd_pcm_t *handle, 
             snd_pcm_sw_params_t *swparams) {
    int err;
    struct alsa_data *dd = p->driver_data;

    /* get the current swparams */
    err = snd_pcm_sw_params_current(handle, swparams);
    if (err < 0) {
        FAIL("Unable to determine current swparams for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    /* start the transfer when the buffer is full */
    
    err = snd_pcm_sw_params_set_start_threshold(handle,
                                                swparams,
                                                handle==dd->record_handle ? 0 : dd->buffer_size);
    if (err < 0) {
        FAIL("Unable to set start threshold mode for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }

    /* allow the transfer when at least period_size samples can be processed */

    err = snd_pcm_sw_params_set_avail_min(handle, swparams, dd->period_size);
    if (err < 0) {
        FAIL("Unable to set avail min for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }

    /* align all transfers to 1 sample */
    err = snd_pcm_sw_params_set_xfer_align(handle, swparams, 1);
    if (err < 0) {
        FAIL("Unable to set transfer align for %s: %s\n", 
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
#if 0
    /* set silence treshold. */
    err = snd_pcm_sw_params_set_silence_threshold(handle, swparams, 0);
    if(err < 0) {
        FAIL("Unable to set silence treshold for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    /* get boundary. */
    err = snd_pcm_sw_params_get_boundary(swparams, &boundary);
    if(err < 0) {
        FAIL("Unable to get boundary for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    /* set silence size. */
    err = snd_pcm_sw_params_set_silence_size(handle, swparams, boundary);
    if(err < 0) {
        FAIL("Unable to set silence size for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
#endif
    /* write the parameters to the playback device */
    err = snd_pcm_sw_params(handle, swparams);
    if (err < 0) {
        FAIL("Unable to set sw params for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }

    return 0;
}

int 
set_hwparams(struct player *p,
             snd_pcm_t *handle,
             snd_pcm_hw_params_t *params,
             snd_pcm_access_t access,
             int format,
             int channels,
             int rate) {
    int err, dir = -1;
    unsigned int rrate;    
    unsigned int buffer_time = p->state->audio_chunk_duration * 1000000;
    unsigned int period_time = 50000;
    struct alsa_data *dd = p->driver_data;

    DEBUG("format: %d, channels: %d, rate: %d, access: %d\n",
          format, channels, rate, access);

    /* choose all parameters */
    err = snd_pcm_hw_params_any(handle, params);
    if (err < 0) {
        FAIL("Broken config: no configurations available for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    /* set the interleaved read/write format */
    err = snd_pcm_hw_params_set_access(handle, params, access);
    if (err < 0) {
        FAIL("Access type not available for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    /* set the sample format */
    err = snd_pcm_hw_params_set_format(handle, params, format);
    if (err < 0) {
        FAIL("Sample format not available for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    /* set the count of channels */
    err = snd_pcm_hw_params_set_channels(handle, params, channels);
    if (err < 0) {
        FAIL("Channels count (%i) not available for %s: %s\n",
             channels,
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    /* set the stream rate */
    rrate = rate;
    DEBUG("rate: %d\n", rate);
    err = snd_pcm_hw_params_set_rate_near(handle, params, &rrate, 0);
    if (err < 0) {
        FAIL("Rate %iHz not available for %s: %s\n",
             rate,
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    DEBUG("rrate: %u\n", rrate);

    if (rrate != rate) {
        FAIL("Rate doesn't match (requested %iHz, got %iHz)\n", rate, rrate);
        return -EINVAL;
    }
    dd->sample_rate = rrate;

    DEBUG("buffer time: %d\n", buffer_time);

    /* set the buffer time */
    err = snd_pcm_hw_params_set_buffer_time_near(handle, 
                                                 params, 
                                                 &buffer_time,
                                                 &dir);
    if (err < 0) {
        FAIL("Unable to set buffer time %i for %s: %s\n", 
             buffer_time, 
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    err = snd_pcm_hw_params_get_buffer_size(params, &dd->buffer_size);
    if (err < 0) {
        FAIL("Unable to get buffer size for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    dd->playback_buffer_size = dd->buffer_size;
    dd->record_buffer_size = dd->buffer_size;
    DEBUG("buffer size: %ld\n", dd->buffer_size);

    /* set the period time */
    err = snd_pcm_hw_params_set_period_time_near(handle, 
                                                 params, 
                                                 &period_time, 
                                                 &dir);
    if (err < 0) {
        FAIL("Unable to set period time %i for %s: %s\n", 
             period_time,
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    err = snd_pcm_hw_params_get_period_size(params, &dd->period_size, &dir);
    if (err < 0) {
        FAIL("Unable to get period size for %s: %s\n", 
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }

    DEBUG("period_size: %ld\n", dd->period_size);

    /* write the parameters to device */
    err = snd_pcm_hw_params(handle, params);
    if (err < 0) {
        FAIL("Unable to set hw params for %s: %s\n",
             snd_pcm_name(handle),
             snd_strerror(err));
        return err;
    }
    return 0;
}

int
alsa_init_device(struct player *p,
                 snd_pcm_t **handle,
                 snd_pcm_hw_params_t *hwparams,
                 snd_pcm_sw_params_t *swparams,
                 const char *dev,
                 int format,
                 int channels,
                 int rate,
                 snd_pcm_stream_t stream) {
    struct alsa_data *dd = p->driver_data;
    int err;

    *handle = NULL;

    if((err = snd_pcm_open(handle, dev, stream, SND_PCM_NONBLOCK)) < 0) {
        FAIL("Device %s open error: %s\n", dev, snd_strerror(err));
        view_set_transient(p->shl->view, MSG_ERR, "Can't open %s", dev);
        return err;
    }
    if((err = set_hwparams(p,
                           *handle, 
                           hwparams, 
                           SND_PCM_ACCESS_RW_INTERLEAVED,
                           format,
                           channels,
                           rate)) < 0) {
        view_set_transient(p->shl->view, MSG_ERR, "Can't set hwparams");
        snd_pcm_close(*handle);
        *handle = NULL;
        FAIL("Setting of hwparams failed: %s\n", snd_strerror(err));
        return err;
    }
    if((err = set_swparams(p, *handle, swparams)) < 0) {
        view_set_transient(p->shl->view, MSG_ERR, "Can't set swparams");
        FAIL("Setting of swparams failed: %s\n", snd_strerror(err));
        snd_pcm_close(*handle);
        *handle = NULL;
        return err;
    }
    if((err = snd_pcm_prepare(*handle))) {
        view_set_transient(p->shl->view, MSG_ERR, "Can't prepare %s", dev);
        FAIL("snd_pcm_prepare() failed: %s\n", snd_strerror(err));
        snd_pcm_close(*handle);
        *handle = NULL;
        return err;
    }
    return 0;
}

int
alsa_get_format(struct player *p) {
    int format;
    switch(p->state->sample_type) {
    case SAMPLE_TYPE_INT_8:
        format = SND_PCM_FORMAT_S8;
        break;
    case SAMPLE_TYPE_INT_16:
        format = SND_PCM_FORMAT_S16;
        break;
    case SAMPLE_TYPE_INT_32:
        format = SND_PCM_FORMAT_S32;
        break;
    case SAMPLE_TYPE_FLOAT_32:
        format = SND_PCM_FORMAT_FLOAT;
        break;
    default:
        DEBUG("unknown sample type!\n");
        format = SND_PCM_FORMAT_UNKNOWN;
        break;
    }
    return format;
}

int
alsa_open(struct player *p) {
    struct alsa_data *dd = p->driver_data;
    int err, format;
    snd_pcm_hw_params_t *play_hwparams;
    snd_pcm_sw_params_t *play_swparams;
    snd_pcm_hw_params_t *record_hwparams;
    snd_pcm_sw_params_t *record_swparams;

    snd_pcm_hw_params_alloca(&play_hwparams);
    snd_pcm_sw_params_alloca(&play_swparams);
    snd_pcm_hw_params_alloca(&record_hwparams);
    snd_pcm_sw_params_alloca(&record_swparams);

    snd_output_stdio_attach(&dd->output, stdout, 0);

    format = alsa_get_format(p);
    
    DEBUG("initializing playback device\n");
    if((err = alsa_init_device(p,
                               &dd->play_handle,
                               play_hwparams,
                               play_swparams,
                               pref_get_as_string("alsa.playback_device"),
                               format,
                               p->shl->clip->mixer->output_channels,
                               p->state->source_rate,
                               SND_PCM_STREAM_PLAYBACK)) < 0) {
        FAIL("Initialization error on playback device %s: %s\n", 
             pref_get_as_string("alsa.playback_device"),
             snd_strerror(err));

        return 1;
    }

    if(p->state->record_mode) {
        DEBUG("initializing record device\n");
        if((err = alsa_init_device(p,
                                   &dd->record_handle,
                                   record_hwparams,
                                   record_swparams,
                                   pref_get_as_string("alsa.record_device"),
                                   format,
                                   p->state->target_tracks,
                                   p->state->source_rate,
                                   SND_PCM_STREAM_CAPTURE)) < 0) {
            FAIL("Initialization error on record device %s: %s\n", 
                 pref_get_as_string("alsa.record_device"),
                 snd_strerror(err));
            snd_pcm_close(dd->play_handle);
            return 1;
        }
        
        if((err = snd_pcm_link(dd->play_handle, dd->record_handle))) {
            view_set_transient(p->shl->view, MSG_ERR, "Device link error");
            FAIL("Cannot link playback device %s and record device %s: %s\n",
                 pref_get_as_string("alsa.playback_device"),
                 pref_get_as_string("alsa.record_device"),
                 snd_strerror(err));
            snd_pcm_close(dd->play_handle);
            snd_pcm_close(dd->record_handle);
            return 1;
        }
        
    }

    dd->err = 0;
    dd->output = NULL;
    return 0;
}

void
alsa_exit(struct player *p) {
    if(p->driver_data)
        mem_free(p->driver_data);
}

int
alsa_init(struct player *p) {
    p->driver_data = mem_calloc(sizeof(struct alsa_data), 1);
    if(!p->driver_data)
        return 1;
    return 0;
}

void
alsa_destroy() {
}

int
alsa_new() {
    return 0;
}


static struct pane *config_pane = NULL;

void
alsa_populate_dialog() {
    gtk_entry_set_text(GTK_ENTRY(pane_get_widget(config_pane, "playback_device")),
                       pref_get_as_string("alsa.playback_device"));
    gtk_entry_set_text(GTK_ENTRY(pane_get_widget(config_pane, "record_device")),
                       pref_get_as_string("alsa.record_device"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "output_channels")),
                              pref_get_as_int("alsa.output_channels"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "input_channels")),
                              pref_get_as_int("alsa.input_channels"));

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(config_pane, "underrun_recovery")),
                                 pref_get_as_int("alsa.underrun_recovery"));
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane_get_widget(config_pane, "overrun_recovery")),
                                 pref_get_as_int("alsa.overrun_recovery"));
}

#define ALSA_GLADE_FILE "player_alsa" GUI_GTK_VERSION_TAG ".glade"

GtkWidget *
alsa_open_config() {
    static GladeXML *xml = NULL;
    static GtkWidget *w = NULL;
    char path[4096];

    if(w) 
        return w;

    if(!xml) {
        snprintf(path, sizeof(path), "%s/%s", module_get_path(self_id), 
            ALSA_GLADE_FILE);
        DEBUG("loading interface %s\n", path);
        xml = glade_xml_new(path, NULL, NULL);
        
        if(!xml) {
            FAIL("could not find interface definition, " "looked at %s\n", 
                 path);
            return NULL;
        }
    }

    if(!config_pane) {
        config_pane = pane_new(xml);
        if(!config_pane) {
            FAIL("could not create pane for configuration\n");
            return NULL;
        }
    }

    alsa_populate_dialog();

    w = pane_get_widget(config_pane, "alsa_config_panel");
    gtk_widget_ref(w);
    gtk_container_remove(GTK_CONTAINER(pane_get_widget(config_pane, "alsa_config")), w);
    return w;
}

void
alsa_commit_config() {
    pref_set_string("alsa.playback_device",
                    gtk_entry_get_text(GTK_ENTRY(pane_get_widget(config_pane, "playback_device"))));
    pref_set_string("alsa.record_device",
                    gtk_entry_get_text(GTK_ENTRY(pane_get_widget(config_pane, "record_device"))));
    pref_set_int("alsa.output_channels",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "output_channels"))));
    pref_set_int("alsa.input_channels",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "input_channels"))));
    pref_set_int("alsa.underrun_recovery",
                 gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(config_pane, "underrun_recovery"))));
    pref_set_int("alsa.overrun_recovery",
                 gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(config_pane, "overrun_recovery"))));
}

void
alsa_close_config() {
}

static struct player_driver driver_info = {
    "ALSA", alsa_new, alsa_destroy, alsa_init, alsa_exit, 
    alsa_open, alsa_close, alsa_transfer,
    alsa_get_output_channels, alsa_get_input_channels,
    alsa_get_audio_delay, alsa_get_sample_rate,
    alsa_get_record_buffer_size, alsa_get_playback_buffer_size,
    alsa_open_config, alsa_commit_config, alsa_close_config
};

static struct pref default_prefs[] = { 

    /*
     * ALSA specific settings.
     */
    
    PREF_STRING("alsa.playback_device", "default", "The playback device."),
    PREF_STRING("alsa.record_device", "default", "The capture device."),
    PREF_INT_WO("alsa.output_channels", 2, 1, MAX_TRACKS, 
                "The number of output channels to allocate on the playback "
                "device."),
    PREF_INT_WO("alsa.input_channels", 2, 1, MAX_TRACKS,
                "The number of input channels to allocate on the capture "
                "device."),
    PREF_BOOL("alsa.underrun_recovery", 1,
              "Whether to stop playback/recording on underruns."),
    PREF_BOOL("alsa.overrun_recovery", 0,
              "Whether to stop playback/recording on overruns."),
};

static int
alsa_module_init(int id) {
    self_id = id;
    player_register_driver(&driver_info);
    pref_register(sizeof(default_prefs) / sizeof(default_prefs[0]),
                  default_prefs);
    pref_load("alsa.*");
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "ALSA driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2003-2005",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    alsa_module_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif /* HAVE_ALSA */
