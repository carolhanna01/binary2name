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

/**
 * @file
 * This implements support for the JACK Audio Connection Kit.
 * We register as a client once during program startup, and 
 * activate/deactivate ourselves whenever the user wants to 
 * record/playback. 
 *
 * Some flaws with this implementation:
 * - it doesn't support a buffer size change during playback
 *   or recording.
 * - it completely ignores the JACK sample rate.
 */

#include <config.h>

#ifndef HAVE_JACK

#warning "Not building JACK driver."

#else /* HAVE_JACK */

#include <gnusound.h>
#include <jack/jack.h>
#include <signal.h>

#define MAX_BUFFERS 32

/* FIXME: use of MAX_TRACKS is arbitrary limitation */
struct jackdrv_globals {
    jack_client_t *client;
    jack_port_t *inputs[MAX_TRACKS];
    jack_port_t *outputs[MAX_TRACKS];
    jack_nframes_t sample_rate;
    jack_nframes_t buffer_size;
    int error;
    sig_atomic_t connected;

    jack_nframes_t delay;
    struct player *active_player;
};
    
struct jackdrv_data {
    size_t recbuf_size;
    size_t recbuf_pos;
    void **recbuf;
    int data_ready;
    pthread_mutex_t data_ready_lock;
    pthread_cond_t data_ready_cond;
};

static int self_id;
static struct jackdrv_globals jackdrv_globals = { 0, };

const char **
jackdrv_get_ports(const char *port_name_pattern,
                  const char *type_name_pattern,
                  unsigned long flags) {
    jack_client_t *client;
    const char **ports;
    if((client = jack_client_new("jackdrv")) == 0) {
        FAIL("jack server not running?\n");
        return NULL;
    }

    ports = jack_get_ports(client,
                           port_name_pattern,
                           type_name_pattern,
                           flags);
    jack_client_close(client);
    return ports;
}

size_t 
jackdrv_get_playback_buffer_size(struct player *p) {
    return jackdrv_globals.buffer_size;
}

size_t 
jackdrv_get_record_buffer_size(struct player *p) {
    return jackdrv_globals.buffer_size;
}

int
jackdrv_get_sample_rate(struct player *p) {
    return jackdrv_globals.sample_rate;
}

AFframecount
jackdrv_get_audio_delay(struct player *p) {
    return jackdrv_globals.buffer_size;
}

unsigned int
jackdrv_get_input_channels() {
    /* FIXME: max_tracks clamp is arbitrary limitation. */
    return MIN(pref_get_as_int("jack.input_channels"),
               pref_get_as_int("max_tracks"));
}

unsigned int
jackdrv_get_output_channels() {
    /* FIXME: max_tracks clamp is arbitrary limitation. */
    return MIN(pref_get_as_int("jack.output_channels"),
               pref_get_as_int("max_tracks"));
}

void
jackdrv_error(const char *msg) {
    jackdrv_globals.error = 1;
    FAIL("jack error: %s\n", msg);
}

void
jackdrv_free_buffers(void **bufs,
                     unsigned int channels) {
    int i;
    if(!bufs)
        return;
    for(i = 0; i < channels; i++) 
        if(bufs[i])
            mem_free(bufs[i]);
    mem_free(bufs);
}

void **
jackdrv_alloc_buffers(unsigned int channels,
                      unsigned int sample_width,
                      size_t size) {
    void **bufs;
    int i;
    bufs = mem_calloc(sizeof(void *), channels);
    if(!bufs) 
        return NULL;
    for(i = 0; i < channels; i++) {
        bufs[i] = mem_alloc(sample_width * size);
        if(!bufs[i]) {
            jackdrv_free_buffers(bufs, channels);
            return NULL;
        }
    }
    return bufs;
}

int
jackdrv_buffer_size_changed(jack_nframes_t nframes,
                            void *arg) {
    /* FIXME: reallocate buffers */
    jackdrv_globals.error = 1;
    FAIL("buffer size changed to: %d, this is not supported!\n", nframes);
    return 1;
}

int
jackdrv_sample_rate_changed(jack_nframes_t nframes,
                         void *arg) {
    jackdrv_globals.sample_rate = nframes;
    DEBUG("sample rate: %d\n", nframes);
    return 0;
}

void
jackdrv_shutdown(void *arg) {
    DEBUG("jack shutting down\n");
    jackdrv_globals.connected = 0;
}

int
jackdrv_xrun(void *arg) {
    FAIL("xrun\n");
    return 0;
}

/**
 * This is the callback called by JACK when there's work to be done.
 * We use a slightly convoluted scheme to get data to and from JACK,
 * but it works and seems to be fast enough.
 *
 * For playback, we get the necessary data and copy-convert it into
 * JACK's output buffers. This is very straightforward and basically
 * identical to all the other audio drivers.
 *
 * For record, we can't use the straightforward approach, because
 * inserting the data into the snd object incurs too much overhead
 * here (mostly because JACK uses very tiny buffers). So what we do is
 * copy-convert the floating point audio to a storage buffer which can
 * hold several JACK buffer sizes. When that storage buffer is full,
 * we signal the transfer() thread that there is data to be
 * collected. The transfer thread then inserts the storage buffer into
 * the snd object.
 *
 * Why is this convoluted? Well, it's not orthogonal. It would be more
 * elegant to have the transfer() thread do the work of getting data
 * from/to the snd object, and have this thread do nothing but collect
 * and fill some temporary storage buffers. Also, to signal the
 * transfer() thread, we need to acquire a lock here, which is
 * contrary to the design of the JACK process loop. It might be better
 * to use a lock-free FIFO for communications. Finally, the current
 * setup means that the rwlock needs to support accesses from 3
 * threads rather than 2: the JACK thread, the transfer() thread, and
 * the GUI thread. So this means a tiny slowdown on all rwlock locking
 * operations.
 */

int
jackdrv_process(jack_nframes_t nframes,
                void *arg) {
    int i, err;
    struct player *p = jackdrv_globals.active_player;
    struct jackdrv_data *dd;
    void **playback_buf;
    void *pb;
    AFframecount playback_frames = nframes, record_frames = nframes;

    /* Zero-fill output buffers. */

    for(i = 0; i < jackdrv_get_output_channels(NULL); i++) {
        pb = jack_port_get_buffer(jackdrv_globals.outputs[i], playback_frames);
        memset(pb, '\0', playback_frames * sizeof(float));
    }

    if(!p)
        return 0;

    dd = p->driver_data;

    if(!player_has_work(p) || jackdrv_globals.error)
        return 1;

    /* Playback. */

    if((err = player_get_playback_bufn(p, &playback_buf, &playback_frames))) {
        FAIL("error getting playback buffer\n");
        return err;
    }

    for(i = 0; i < jackdrv_get_output_channels(NULL); i++) {
        pb = jack_port_get_buffer(jackdrv_globals.outputs[i], playback_frames);
        sample_convert(p->shl->clip->sr->sample_type,
                       SAMPLE_TYPE_FLOAT_32,
                       playback_buf[i],
                       pb,
                       playback_frames);
    }

    if((err = player_flush_playback_bufn(p, playback_frames))) {
        FAIL("error flushing playback buffer\n");
        return err;
    }

    if(!p->state->record_mode) 
        return 0;

    /* Recording. */

    if(dd->data_ready) {
        pthread_mutex_lock(&dd->data_ready_lock);
        DEBUG("waiting for data to be saved...\n");
        while(dd->data_ready)
            pthread_cond_wait(&dd->data_ready_cond, &dd->data_ready_lock);
        DEBUG("...data saved\n");
        pthread_mutex_unlock(&dd->data_ready_lock);
    }

    for(i = 0; i < p->state->target_tracks; i++) {
        pb = jack_port_get_buffer(jackdrv_globals.inputs[i], record_frames);
        sample_convert(SAMPLE_TYPE_FLOAT_32,
                       p->shl->clip->sr->sample_type,
                       pb,
                       dd->recbuf[i] + 
                       (dd->recbuf_pos * 
                        sample_get_width(p->shl->clip->sr->sample_type)),
                       record_frames);
    }
    dd->recbuf_pos += record_frames;

    if(dd->recbuf_pos == dd->recbuf_size) {
        pthread_mutex_lock(&dd->data_ready_lock);
        dd->data_ready = 1;
        pthread_cond_signal(&dd->data_ready_cond);
        pthread_mutex_unlock(&dd->data_ready_lock);
    }

    return err;
}

int
jackdrv_store_recorded(struct player *p) {
    int i, err, offset, count;
    void **dstbuf;
    struct jackdrv_data *dd = p->driver_data;
    AFframecount dstbuf_size = MIN(dd->recbuf_pos, player_get_record_avail(p)),
        left, done, total = 0;

    for(left = dstbuf_size; left; left -= done) {
        dstbuf_size = left;
        if((err = player_get_record_bufn(p, &dstbuf, &dstbuf_size))) {
            FAIL("error getting record buffer\n");
            return err;
        }
        
        done = MIN(dstbuf_size, left);
        for(i = 0; i < p->state->target_tracks; i++) {
            offset = total * sample_get_width(p->shl->clip->sr->sample_type);
            count = done * sample_get_width(p->shl->clip->sr->sample_type); 
            memcpy(dstbuf[i] + offset, dd->recbuf[i] + offset, count);
        }
        total += done;

        if((err = player_flush_record_bufn(p, done))) {
            FAIL("error flushing record buffer\n");
            return err;
        }
    }
    return 0;
}

int
jackdrv_transfer(struct player *p) {
    int err;
    struct jackdrv_data *dd = p->driver_data;
    struct timeval now;
    struct timespec tm;

    while(player_has_work(p) && 
          jackdrv_globals.connected &&
          !jackdrv_globals.error) {

        /*
         * Wait for the JACK process thread to signal that some
         * recorded data is available and store it in the snd
         * object. 
         */

        pthread_mutex_lock(&dd->data_ready_lock);
        gettimeofday(&now, NULL);
        tm.tv_sec = now.tv_sec + 1;
        tm.tv_nsec = now.tv_usec;
        pthread_cond_timedwait(&dd->data_ready_cond, &dd->data_ready_lock,
                               &tm);
        if(dd->data_ready) {
            if((err = jackdrv_store_recorded(p))) 
                jackdrv_globals.error = err;
            dd->recbuf_pos = 0;
            dd->data_ready = 0;
            pthread_cond_signal(&dd->data_ready_cond);
        }
        pthread_mutex_unlock(&dd->data_ready_lock);
    }
    return jackdrv_globals.error;
}

void
jackdrv_stop(struct player *p) {
    int i;
    struct jackdrv_data *dd = p->driver_data;

    if(p != jackdrv_globals.active_player)
        return;

    jackdrv_free_buffers(dd->recbuf,
                         p->state->target_tracks);
    pthread_mutex_destroy(&dd->data_ready_lock);
    pthread_cond_destroy(&dd->data_ready_cond);
    jackdrv_globals.active_player = NULL;

}

int
jackdrv_register_ports(int port_count,
                       const char *portname_template,
                       jack_port_t **ports,
                       unsigned long flags) {
    int i;
    char s[100];
    for(i = 0; i < port_count; i++) {
        snprintf(s, sizeof(s), portname_template, i + 1);
        ports[i] = jack_port_register(jackdrv_globals.client, s,
                                      JACK_DEFAULT_AUDIO_TYPE,
                                      flags, 0);
        if(!ports[i]) {
            FAIL("cannot create port %s\n", s);
            return 1;
        }
    }
    return 0;
}

int
jackdrv_connect_ports(int port_count,
                      const char *portname_template,
                      jack_port_t **ports) {
    int i;
    char port[100], path[255], *dst;
    const char *in, *out;
    for(i = 0; i < port_count; i++) {
        snprintf(port, sizeof(port), portname_template, i+1);
        snprintf(path, sizeof(path), "/gnusound/preferences/jack.%s", port);
        dst = gnome_config_get_string(path);
        if(!dst) {
            FAIL("don't know how to connect %s\n", port);
            continue;
        }

        in = jack_port_name(ports[i]);
        out = dst;
        if(jack_port_flags(ports[i]) & JackPortIsInput) {
            out = jack_port_name(ports[i]);
            in = dst;
        }

        if(jack_connect(jackdrv_globals.client, in, out))
            FAIL("error connecting %s to %s\n", in, out);

        g_free(dst);
    }
    return 0;
}

int
jackdrv_setup() {
    int err;

    jack_set_error_function(jackdrv_error);

    if(jackdrv_globals.connected) 
        return 0;

    if(jackdrv_globals.client) {
        jack_client_close(jackdrv_globals.client);
        jackdrv_globals.client = NULL;
    }

    if((jackdrv_globals.client = jack_client_new("GNUsound")) == 0) {
        FAIL("jack server not running?\n");
        return 1;
    }

    DEBUG("registering ports\n");
    if((err = jackdrv_register_ports(jackdrv_get_input_channels(NULL),
                                     "record_%d",
                                     jackdrv_globals.inputs,
                                     JackPortIsInput)))
        return err;
    if((err = jackdrv_register_ports(jackdrv_get_output_channels(NULL),
                                     "playback_%d",
                                     jackdrv_globals.outputs,
                                     JackPortIsOutput)))
        return err;

    jack_on_shutdown(jackdrv_globals.client, jackdrv_shutdown, 0);
    jackdrv_globals.sample_rate = jack_get_sample_rate(jackdrv_globals.client);
    
    if(jack_set_sample_rate_callback(jackdrv_globals.client, 
                                     jackdrv_sample_rate_changed, NULL)) {
        FAIL("cannot set sample rate callback\n");
        return 1;
    }

    if(jack_set_buffer_size_callback(jackdrv_globals.client,
                                     jackdrv_buffer_size_changed, NULL)) {
        FAIL("cannot set buffer size callback\n");
        return 1;
    }
    jackdrv_globals.buffer_size = jack_get_buffer_size(jackdrv_globals.client);

    if(jack_set_process_callback(jackdrv_globals.client, 
                                 jackdrv_process, NULL)) {
        FAIL("cannot set process callback\n");
        return 1;
    }

    DEBUG("activating jack\n");
    if(jack_activate(jackdrv_globals.client)) {
        FAIL("cannot activate client\n");
        return 1;
    }

    DEBUG("connecting ports\n");
    if((err = jackdrv_connect_ports(jackdrv_get_input_channels(NULL),
                                    "record_%d",
                                    jackdrv_globals.inputs)))
        return err;

    if((err = jackdrv_connect_ports(jackdrv_get_output_channels(NULL),
                                    "playback_%d",
                                    jackdrv_globals.outputs)))
        return err;

    jackdrv_globals.connected = 1;
    return 0;
}


int
jackdrv_start(struct player *p) {
    int i, err;
    struct jackdrv_data *dd = p->driver_data;

    if((err = jackdrv_setup())) 
        return err;

    /*
     * FIXME: tiny race here since this function may be
     * called multiple times from multiple players
     */
    if(jackdrv_globals.active_player) {
        FAIL("already a player active\n");
        return 1;
    }
    jackdrv_globals.active_player = p;

    dd->recbuf = 
        jackdrv_alloc_buffers(jackdrv_get_input_channels(NULL),
                              sample_get_width(p->shl->clip->sr->sample_type),
                              jackdrv_globals.buffer_size * 
                              MAX_BUFFERS);
    dd->recbuf_pos = 0;
    dd->recbuf_size = MAX_BUFFERS * jackdrv_globals.buffer_size;
    pthread_mutex_init(&dd->data_ready_lock, NULL);
    pthread_cond_init(&dd->data_ready_cond, NULL);

    return 0;
}

void
jackdrv_close(struct player *p) {
    struct jackdrv_data *dd = p->driver_data;

    if(p->driver_data)
        mem_free(p->driver_data);
}

int
jackdrv_open(struct player *p) {
    struct jackdrv_data *dd;
    p->driver_data = mem_calloc(sizeof(struct jackdrv_data), 1);
    if(!p->driver_data)
        return 1;
    dd = p->driver_data;
    return 0;
}

void
jackdrv_unregister_ports(int port_count,
                         jack_port_t **ports) {
    int i;
    for(i = 0; i < jackdrv_get_input_channels(NULL); i++) {
        if(ports[i]) {
            jack_port_unregister(jackdrv_globals.client, ports[i]);
            ports[i] = NULL;
        }
    }
}

void
jackdrv_destroy() {
    if(jackdrv_globals.client) {
        jackdrv_unregister_ports(jackdrv_get_input_channels(NULL),
                                 jackdrv_globals.inputs);
        jackdrv_unregister_ports(jackdrv_get_output_channels(NULL),
                                 jackdrv_globals.outputs);
        jack_deactivate(jackdrv_globals.client);
        jack_client_close(jackdrv_globals.client);
    }
}

int
jackdrv_new() {
    jackdrv_setup();

    return 0;
}


static struct pane *config_pane = NULL;
static GHashTable *jackdrv_input_channel_selections;
static GHashTable *jackdrv_output_channel_selections;
static const char **jackdrv_input_ports;
static const char **jackdrv_output_ports;


GtkWidget * 
jackdrv_make_ports_menu(const char *selected_port,
                        const char **ports) {
    int i, match = -1;
    GtkWidget *menu;
    GList *l = NULL;
    
    l = g_list_append(l, "unspecified");
    for(i = 0; ports && ports[i]; i++) {
        if(selected_port && !strcmp(ports[i], selected_port))
            match = i;
        l = g_list_append(l, (char *)ports[i]);
    }
    
    menu = combo_box_new();
    gtk_widget_show(menu);
    combo_box_set_strings(COMBO_BOX(menu), l);
    combo_box_set_editable(COMBO_BOX(menu), FALSE);
    
    if(match != -1) 
        combo_box_set_active(COMBO_BOX(menu), match+1);
    
    g_list_free(l);

    return menu;
}

void
jackdrv_map(int port_count,
            const char *port_template,
            GHashTable *ht,
            GtkWidget *map_table,
            const char **ports) {
    int i;
    char s[100];
    char path[255], *selected_port;
    GtkWidget *l, *menu;
    for(i = 0; i < port_count; i++) {
        snprintf(s, sizeof(s), port_template, i+1);
        snprintf(path, sizeof(path), "/gnusound/preferences/jack.%s", s);
        selected_port = gnome_config_get_string(path);
        l = gtk_label_new(s);
        gtk_widget_show(l);
        gtk_table_attach(GTK_TABLE(map_table),
                         l,
                         0, 1, i, i+1,
                         GTK_EXPAND, GTK_EXPAND,
                         0, 0);
        menu = jackdrv_make_ports_menu(selected_port, ports);
        if(selected_port)
            g_free(selected_port);
        gtk_table_attach(GTK_TABLE(map_table),
                         menu,
                         1, 2, i, i+1,
                         GTK_FILL | GTK_EXPAND, GTK_EXPAND,
                         0, 0);
        g_hash_table_insert(ht, strdup(s), menu);
    }
}

void
jackdrv_populate_gui() {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "output_channels")),
                              pref_get_as_int("jack.output_channels"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "input_channels")),
                              pref_get_as_int("jack.input_channels"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "buffers")),
                              pref_get_as_int("jack.buffers"));

    jackdrv_input_channel_selections = 
        g_hash_table_new(g_str_hash, g_str_equal);
    jackdrv_output_channel_selections = 
        g_hash_table_new(g_str_hash, g_str_equal);
    
    jackdrv_input_ports = jackdrv_get_ports(NULL, NULL, JackPortIsInput);
    jackdrv_output_ports = jackdrv_get_ports(NULL, NULL, JackPortIsOutput);

    jackdrv_map(pref_get_as_int("jack.output_channels"),
                "playback_%d",
                jackdrv_output_channel_selections,
                pane_get_widget(config_pane, "output_channel_map"),
                jackdrv_input_ports);
    jackdrv_map(pref_get_as_int("jack.input_channels"),
                "record_%d",
                jackdrv_input_channel_selections,
                pane_get_widget(config_pane, "input_channel_map"),
                jackdrv_output_ports);
    
}

#define JACKDRV_GLADE_FILE "player_jack" GUI_GTK_VERSION_TAG ".glade"

GtkWidget *
jackdrv_open_config() {
    static GladeXML *xml = NULL;
    static GtkWidget *w = NULL;
    char path[4096];

    if(w) 
        return w;

    if(!xml) {
        snprintf(path, sizeof(path), 
                 "%s/%s", module_get_path(self_id), JACKDRV_GLADE_FILE);
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

    jackdrv_populate_gui();

    w = pane_get_widget(config_pane, "jack_config_panel");
    gtk_widget_ref(w);
    gtk_container_remove(GTK_CONTAINER(pane_get_widget(config_pane, "jack_config")), w);
    return w;
}

void
jackdrv_commit_channels(gpointer key,
                        gpointer value,
                        gpointer user_data) {
    const char *port = combo_box_get_value(COMBO_BOX(value));;
    char path[256];

    if(strcmp(port, "unspecified")) {
        snprintf(path, sizeof(path), "/gnusound/preferences/jack.%s", 
                 (char *)key);
        gnome_config_set_string(path, port);
    }    
}

void
jackdrv_free_jack_hashtable_keys(gpointer key,
                                 gpointer value,
                                 gpointer user_data) {
    GList **freelist = user_data;
    *freelist = g_list_append(*freelist, key);
}

void 
jackdrv_free_config() {
    GList *freelist = NULL, *l;

    /* Free the strdup()ped hash key values. */

    g_hash_table_foreach(jackdrv_output_channel_selections,
                         jackdrv_free_jack_hashtable_keys,
                         &freelist);
    for(l = freelist; l; l = l->next)
        if(l->data)
            free(l->data);
    g_list_free(freelist);
    freelist = NULL;
    g_hash_table_foreach(jackdrv_input_channel_selections,
                         jackdrv_free_jack_hashtable_keys,
                         &freelist);
    for(l = freelist; l; l = l->next)
        if(l->data)
            free(l->data);
    g_list_free(freelist);
    
    g_hash_table_destroy(jackdrv_output_channel_selections);
    g_hash_table_destroy(jackdrv_input_channel_selections);

    freelist = gtk_container_get_children(GTK_CONTAINER(pane_get_widget(config_pane, "input_channel_map")));
    for(l = freelist; l; l = l->next)
        gtk_widget_destroy(GTK_WIDGET(l->data));
        
    freelist = gtk_container_get_children(GTK_CONTAINER(pane_get_widget(config_pane, "output_channel_map")));
    for(l = freelist; l; l = l->next)
        gtk_widget_destroy(GTK_WIDGET(l->data));
    
    if(jackdrv_input_ports)
        free(jackdrv_input_ports);
    if(jackdrv_output_ports)
        free(jackdrv_output_ports);
}

void
jackdrv_commit_config() {
    pref_set_int("jack.output_channels",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "output_channels"))));
    pref_set_int("jack.input_channels",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "input_channels"))));
    pref_set_int("jack.buffers",
                 gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(pane_get_widget(config_pane, "buffers"))));
    
    g_hash_table_foreach(jackdrv_output_channel_selections,
                         jackdrv_commit_channels,
                         jackdrv_input_ports);

    g_hash_table_foreach(jackdrv_input_channel_selections,
                         jackdrv_commit_channels,
                         jackdrv_output_ports);
}

void
jackdrv_close_config() {
    //    jackdrv_free_config();
}

static struct player_driver driver_info = {
    "JACK", jackdrv_new, jackdrv_destroy, jackdrv_open, jackdrv_close, 
    jackdrv_start, jackdrv_stop, jackdrv_transfer,
    jackdrv_get_output_channels, jackdrv_get_input_channels,
    jackdrv_get_audio_delay, jackdrv_get_sample_rate,
    jackdrv_get_record_buffer_size, jackdrv_get_playback_buffer_size,
    jackdrv_open_config, jackdrv_commit_config, jackdrv_close_config
};

static struct pref default_prefs[] = { 

    /* 
     * JACK specific settings.
     */

    PREF_INT_WO("jack.output_channels", 2, 1, MAX_TRACKS,
                "How many output ports to create."),
    PREF_INT_WO("jack.input_channels", 2, 1, MAX_TRACKS,
                "How many input ports to accept."),
    PREF_INT_WO("jack.buffers", 2, 1, 100,
                "How many buffers should be used."),
    
};


static int
jackdrv_module_init(int id) {
    self_id = id;
    player_register_driver(&driver_info);
    pref_register(sizeof(default_prefs) / sizeof(default_prefs[0]),
                  default_prefs);
    pref_load("jack.*");
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "JACK driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2003-2005",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    jackdrv_module_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif /* HAVE_JACK */
