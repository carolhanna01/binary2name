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

#include <config.h>

#ifndef HAVE_OSS

#warning "Not building OSS driver."

#else /* HAVE_OSS */

#include <gnusound.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

static int self_id = -1;

struct oss_data {
    int play_audio_fd;
    int rec_audio_fd;
    int format;
    int sample_rate;
    size_t playback_buffer_size;
    size_t record_buffer_size;
};

AFframecount
oss_get_audio_delay(struct player *p) {
    struct oss_data *dd = p->driver_data;
    return dd->playback_buffer_size;
}

size_t 
oss_get_playback_buffer_size(struct player *p) {
    struct oss_data *dd = p->driver_data;
    return dd->playback_buffer_size;
}

size_t 
oss_get_record_buffer_size(struct player *p) {
    struct oss_data *dd = p->driver_data;
    return dd->record_buffer_size;
}

int
oss_get_sample_rate(struct player *p) {
    struct oss_data *dd = p->driver_data;
    return dd->sample_rate;
}

unsigned int
oss_get_input_channels() {
    return pref_get_as_int("oss.input_channels");
}

unsigned int
oss_get_output_channels() {
    return pref_get_as_int("oss.output_channels");
}

int
oss_device_init(struct player *p,
                int audio_fd, 
                int frame_width,
                int channels, 
                int speed,
                int *format) { 
    int reqformat, reqchannels, reqspeed, frags;
    int32_t shft;
    audio_buf_info info;
    struct oss_data *dd = p->driver_data;
    
    /* "This call switches the device to full duplex mode and makes the
       driver prepared for full duplex access. This must be done
       before checking the DSP_CAP_DUPLEX bit, since otherwise the
       driver may report that the device doesn't support full
       duplex." (oss.pdf p. 106) */
    /*
    if(record_mode)
        if(ioctl(audio_fd, SNDCTL_DSP_SETDUPLEX, 1) == -1) 
            FAIL("Could not set device to full duplex: %s.\n", 
                 strerror(errno));
    
    if(ioctl(audio_fd, SNDCTL_DSP_GETCAPS, &caps) == -1) 
        FAIL("Could not get %s device capabilities: %s\n", 
             AUDIO_DEVICE, strerror(errno));

    if(record_mode && (!(caps & DSP_CAP_DUPLEX) ||
                       !(caps & DSP_CAP_TRIGGER))) {
        FAIL("Soundcard has insufficient capabilities for play and record, " \
             "require DSP_CAP_DUPLEX DSP_CAP_TRIGGER, got %s%s\n",
             (caps & DSP_CAP_DUPLEX) ? "DSP_CAP_DUPLEX " : "",
             (caps & DSP_CAP_TRIGGER) ? "DSP_CAP_TRIGGER " : "");
        return 0;
    }
    */
    /* "This ioctl call must be used as early as possible. The optimum
       location is immediately after opening the device." (oss.pdf,
       p. 98) */
    
    shft = floor(log(player_get_chunk_size(p) / 
                     pref_get_as_int("oss.fragments"))/log(2));
    frags = (shft & (int32_t)0xFFFF) | 
        (pref_get_as_int("oss.fragments") << 16); 
    DEBUG("frags: 0x%.8x\n", frags);
    if (ioctl(audio_fd, SNDCTL_DSP_SETFRAGMENT, &frags)) 
        perror("could not set fragments");
        
    /* FIXME: We ask audiofile to always return signed data, so we do
       not need to consider signed/unsigned issues. Endianness issues
       are not considered however, that needs work. */

    switch(frame_width) {
    case 1:
        if(pref_get_as_int("playback_signed_int8_to_unsigned_int8"))
            /* Arghhh, AFMT_S8 doesn't work for some cards and the
               values returned by SNDCTL_DSP_GETFMTS are totally
               useless (they won't tell you whether AFMT_U8 _is_
               supported). */
            *format = AFMT_U8;
        else
            *format = AFMT_S8;
        break;
    case 2:
        *format = AFMT_S16_NE;
        break;
    case 4:
        *format = AFMT_S32_NE;
        break;
    }

    /* Set format (sign, bits, byteorder). */

    reqformat = *format;    
    if(ioctl(audio_fd, SNDCTL_DSP_SETFMT, &reqformat) == -1) {
        perror("SNDCTL_DSP_SETFMT");
        return errno;
    }
    if(*format != reqformat) {
        FAIL("Device doesn't support format\n");
        return -1;
    }

    /* Set channels. */

    reqchannels = channels;
    if(ioctl(audio_fd, SNDCTL_DSP_CHANNELS, &reqchannels) == -1) {
        perror("SNDCTL_DSP_CHANNELS"); 
        return errno;
    }
    if(channels != reqchannels) {
        FAIL("device doesn't support %d channels\n", channels);
        return -1;
    }

    /* Set speed. */

    reqspeed = speed;    
    if(ioctl(audio_fd, SNDCTL_DSP_SPEED, &reqspeed) == -1) {
        perror("SNDCTL_DSP_SPEED"); 
        return errno;
    }

    if(reqspeed < (speed - 10) ||
       reqspeed > (speed + 10))
        FAIL("Warning, device requires %d samplerate (requested %d)\n",
             reqspeed, speed);
    
    dd->sample_rate = reqspeed;
    /*
    if (-1 == ioctl(audio_fd, SNDCTL_DSP_GETBLKSIZE,  &sound_blocksize)) {
        FAIL("could not get blocksize, SNDCTL_DSP_GETBLKSIZE failed: %s\n",
             strerror(errno));
        p->state->dev_playback_buffer_size = pref_get_as_int("oss.fragment_size");
    } else {
        p->state->dev_playback_buffer_size = sound_blocksize;
    }
    p->state->dev_record_buffer_size = p->state->dev_playback_buffer_size;
    
    DEBUG("blksize: %d\n", sound_blocksize);
    */
    if(ioctl(audio_fd, SNDCTL_DSP_GETISPACE, &info) == -1) {
        FAIL("SNDCTL_DSP_GETISPACE: %s\n", strerror(errno));
    } else {
        DEBUG("ispace.fragments: %d\n", info.fragments);
        DEBUG("ispace.fragstotal: %d\n", info.fragstotal);
        DEBUG("ispace.fragsize: %d\n", info.fragsize);
        DEBUG("ispace.bytes: %d\n", info.bytes);
        dd->record_buffer_size = 
            (info.fragstotal * info.fragsize) / (frame_width * channels);
    }
    if(ioctl(audio_fd, SNDCTL_DSP_GETOSPACE, &info) == -1) {
        FAIL("SNDCTL_DSP_GETOSPACE: %s\n", strerror(errno));
    } else {
        DEBUG("ospace.fragments: %d\n", info.fragments);
        DEBUG("ospace.fragstotal: %d\n", info.fragstotal);
        DEBUG("ospace.fragsize: %d\n", info.fragsize);
        DEBUG("ospace.bytes: %d\n", info.bytes);
        dd->playback_buffer_size = 
            (info.fragstotal * info.fragsize) / (frame_width * channels);
    }
    return 0;
}

/*
 * Open record and play device. We don't use full-duplex, because on
 * Linux record and play device can be the same device, and this is
 * easier. 
 */
int
oss_dsp_init(struct player *p,
             int *play_audio_fd, 
             int *rec_audio_fd,
             int *format, 
             int frame_width,
             int channels, 
             int input_channels,
             int speed,
             int record_mode) {
    int err;
    struct oss_data *dd = p->driver_data;
    if((*play_audio_fd = open(pref_get_as_string("oss.playback_device"),
                              O_WRONLY)) < 0) {
        view_set_transient(p->shl->view, MSG_ERR, "Can't open playback device");
        FAIL("Could not open %s for write.\n", 
             pref_get_as_string("oss.playback_device"));
        return errno;
    }
    if((err = oss_device_init(p,
                              *play_audio_fd,
                              frame_width,
                              channels,
                              speed,
                              format))) {
        view_set_transient(p->shl->view, MSG_ERR, "Can't setup playback device");
        FAIL("Could not initialize %s.\n",
             pref_get_as_string("oss.playback_device"));
        return err;
    }

    if(record_mode) {
        if((*rec_audio_fd = open(pref_get_as_string("oss.record_device"), 
                                 O_RDONLY)) < 0) {
            view_set_transient(p->shl->view, MSG_ERR, "Can't open record device");
            FAIL("Could not open %s for read.\n", 
                 pref_get_as_string("oss.record_device"));
            close(*play_audio_fd);
            return errno;
        }
        if((err = oss_device_init(p,
                                  *rec_audio_fd,
                                  frame_width,
                                  input_channels,
                                  speed,
                                  format))) {
            view_set_transient(p->shl->view, MSG_ERR, 
                                "Can't setup playback device");
            FAIL("Could not initialize %s.\n",
                 pref_get_as_string("oss.record_device"));
            return err;
        }
    }
    dd->playback_buffer_size = 
        CLAMP(dd->playback_buffer_size, 512, 
              dd->playback_buffer_size);
    dd->record_buffer_size = 
        CLAMP(dd->record_buffer_size, 512, 
              dd->record_buffer_size);
    DEBUG("dev_playback_buffer_size: %d\n",
          dd->playback_buffer_size); 
    DEBUG("dev_record_buffer_size: %d\n",
          dd->record_buffer_size); 
    return 0;
}

int
oss_play(struct player *p,
         AFframecount frame_count) {
    struct oss_data *dd = p->driver_data;
    void *buf;
    ssize_t err;

    frame_count = CLAMP(frame_count, 0, dd->playback_buffer_size);
    if((err = player_get_playback_bufi(p, &buf, &frame_count))) {
        FAIL("player_get_playback_buffer: %d\n", err);
        return err;
    }
    
    do {
        errno = 0;
        if((err = write(dd->play_audio_fd, buf, 
                        frame_count * 
                        (sample_get_width(p->state->sample_type) *
                         p->shl->clip->mixer->output_channels))) < 0) {
            if(errno == EINTR)
                continue;
            FAIL("write failed on %s: %s\n", 
                 pref_get_as_string("oss.playback_device"),
                 strerror(errno));
            view_set_transient(p->shl->view, MSG_ERR, "Playback error %d", 
                               errno);
            return -errno;
        }
    } while(errno == EINTR);
    
    err /= sample_get_width(p->state->sample_type) *
        p->shl->clip->mixer->output_channels;
    if((err = player_flush_playback_bufi(p, err))) {
        FAIL("player_flush_playback_buffer failed: %d\n", err);
        return err;
    }
    
    return err;
}

int
oss_record(struct player *p,
           AFframecount frame_count) {
    struct oss_data *dd = p->driver_data;
    void *buf;
    ssize_t err;

    /* 
     * We need to read the same amount of frames that we play back
     * since everything happens in lock-step. That is why we specify
     * the same buffer length as for playback.
     */

    frame_count = CLAMP(frame_count, 0, dd->playback_buffer_size);
    if((err = player_get_record_bufi(p, &buf, &frame_count))) {
        FAIL("player_get_record_buffer failed: %d\n", err);
        return err;
    }
    do {
        errno = 0;
        if((err = read(dd->rec_audio_fd, buf, 
                       frame_count * 
                       (sample_get_width(p->state->sample_type) * 
                        p->state->target_tracks))) < 0) {
            if(errno == EINTR)
                continue;
            FAIL("read failed on %s: %s (%d)\n", 
                 pref_get_as_string("oss.record_device"),
                 strerror(errno), errno);
            view_set_transient(p->shl->view, MSG_ERR, "Record error %d", 
                               errno);
            return -errno;
        }
    } while(errno == EINTR);

    err /= sample_get_width(p->state->sample_type) * p->state->target_tracks;
    if((err = player_flush_record_bufi(p, err))) {
        FAIL("player_flush_record_buffer failed: %d\n", err);
        return err;
    }
    
    return err;
}

int
oss_transfer(struct player *p) {
    int err;
    
    while(player_has_work(p)) {
        if((err = oss_play(p, player_get_playback_avail(p))) < 0) {
            FAIL("playback error: %d\n", err);
            return err;
        }
        if(p->state->record_mode) {
            if((err = oss_record(p,  player_get_record_avail(p))) < 0) {
                FAIL("record error: %d\n", err);
                return err;
            }
        }
    }
    
    return 0;
}

int
oss_setup(struct player *p) {
    return 0;
}

void
oss_close(struct player *p) {
    struct oss_data *dd = p->driver_data;

    if(dd->play_audio_fd != -1)
        close(dd->play_audio_fd);
    dd->play_audio_fd = -1;
    if(dd->rec_audio_fd != -1)
        close(dd->rec_audio_fd);
    dd->rec_audio_fd = -1;
}

int
oss_open(struct player *p) {
    struct oss_data *dd = p->driver_data;
    shell *shl = p->shl;
    int err;
    if((err = oss_dsp_init(p,
                           &dd->play_audio_fd,
                           &dd->rec_audio_fd,
                           &dd->format,
                           sample_get_width(shl->clip->sr->sample_type),
                           shl->clip->mixer->output_channels,
                           p->state->target_tracks,
                           shl->clip->sr->rate,
                           p->state->record_mode)))
        return err;
    
    return 0;
}

void
oss_exit(struct player *p) {
    if(p->driver_data)
        mem_free(p->driver_data);
}

int
oss_init(struct player *p) {
    p->driver_data = mem_calloc(sizeof(struct oss_data), 1);
    if(!p->driver_data)
        return 1;
    return 0;
}

void
oss_destroy() {
}

int
oss_new() {
    return 0;
}

static struct pane *config_pane = NULL;

void
oss_populate_dialog() {
    gtk_entry_set_text(GTK_ENTRY(pane_get_widget(config_pane, "playback_device")),
                       pref_get_as_string("oss.playback_device"));
    gtk_entry_set_text(GTK_ENTRY(pane_get_widget(config_pane, "record_device")),
                       pref_get_as_string("oss.record_device"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "output_channels")),
                              pref_get_as_int("oss.output_channels"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "input_channels")),
                              pref_get_as_int("oss.input_channels"));

    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "fragments")),
                              pref_get_as_int("oss.fragments"));
}

#define OSS_GLADE_FILE "player_oss" GUI_GTK_VERSION_TAG ".glade"

GtkWidget *
oss_open_config() {
    static GladeXML *xml = NULL;
    static GtkWidget *w = NULL;
    char path[4096];

    if(w) 
        return w;

    if(!xml) {
        snprintf(path, sizeof(path), 
                 "%s/%s", module_get_path(self_id), OSS_GLADE_FILE);
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

    oss_populate_dialog();

    w = pane_get_widget(config_pane, "oss_config_panel");
    gtk_widget_ref(w);
    gtk_container_remove(GTK_CONTAINER(pane_get_widget(config_pane, "oss_config")), w);
    return w;
}

void
oss_commit_config() {
    pref_set_string("oss.playback_device",
                    gtk_entry_get_text(GTK_ENTRY(pane_get_widget(config_pane, "playback_device"))));
    pref_set_string("oss.record_device",
                    gtk_entry_get_text(GTK_ENTRY(pane_get_widget(config_pane, "record_device"))));
    pref_set_int("oss.output_channels",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "output_channels"))));
    pref_set_int("oss.input_channels",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "input_channels"))));
    pref_set_int("oss.fragments",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "fragments"))));
}

void
oss_close_config() {
}

static struct player_driver driver_info = {
    "OSS", oss_new, oss_destroy, oss_init, oss_exit, 
    oss_open, oss_close, oss_transfer,
    oss_get_output_channels, oss_get_input_channels,
    oss_get_audio_delay, oss_get_sample_rate,
    oss_get_record_buffer_size, oss_get_playback_buffer_size,
    oss_open_config, oss_commit_config, oss_close_config
};

static struct pref default_prefs[] = { 

    /*
     * OSS specific settings.
     */
    
    PREF_STRING("oss.playback_device", "/dev/dsp", "The playback device."),
    PREF_STRING("oss.record_device", "/dev/dsp", "The capture device."),
    PREF_BOOL("oss.playback_signed_int8_to_unsigned_int8", 0,
              "Whether to convert signed 8 bit samples to unsigned 8 bit "
              "samples on playback. For soundcards where AFMT_S8 doesn't "
              "work."),
    PREF_INT_WO("oss.output_channels", 2, 1, MAX_TRACKS,
                "The number of output channels to allocate on the playback "
                "device."),
    PREF_INT_WO("oss.input_channels", 2, 1, MAX_TRACKS,
                "The number of input channels to allocate on the capture "
                "device."),
    PREF_INT("oss.fragments", 32, 1, 128,
             "Undocumented"),
};

static int
oss_module_init(int id) {
    self_id = id;
    player_register_driver(&driver_info);
    pref_register(sizeof(default_prefs) / sizeof(default_prefs[0]),
                  default_prefs);
    pref_load("oss.*");
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "OSS driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    oss_module_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif /* HAVE_OSS */

