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
 * FIXME: this code only works as expected when sizeof(char) ==
 * sizeof(int8_t), sizeof(short) == sizeof(int16_t), and sizeof(int)
 * == sizeof(int32_t).
 */

#include <config.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdio.h>
#include <gnome.h>
#include "lib/misc.h"
#include "pref.h"
#include "mem.h"
#include "shell.h"
#include "arbiter.h"
#include "file.h"
#include "marker.h"
#include "mixer.h"

extern int Emergency;

static GList *drivers = NULL;

struct file_driver *
file_find_driver(const char *name) {
    GList *l;

    for(l = drivers; l; l = l->next) 
        if(!strcmp(((struct file_driver *)l->data)->name, name)) 
            return (struct file_driver *)l->data;
    return NULL;
}

void
file_foreach_driver(void (*func)(struct file_driver *fd,
                                 void *user_data),
                    void *user_data) {
    g_list_foreach(drivers, (GFunc)func, user_data);
}

gint
file_compare_drivers(gconstpointer a,
                     gconstpointer b) {
    const struct file_driver *fd_a = a, *fd_b = b;
    const char *s = pref_get_as_string("file.drivers.probe_order");
    char **preferred;
    int i, prio_a = 100, prio_b = 100;

    if(!s)
        return 0;

    preferred = g_strsplit(s, ",", 255);
    for(i = 0; preferred[i]; i++) {
        if(!strcasecmp(preferred[i], fd_a->name)) 
            prio_a = i;
        if(!strcasecmp(preferred[i], fd_b->name)) 
            prio_b = i;
        g_free(preferred[i]);
    }
    g_free(preferred);
    
    return prio_a - prio_b;
}

int
file_register_driver(struct file_driver *fd) {
    GList *l;
    struct file_driver *tmp;

    if(!fd->attach || 
       !fd->open ||
       !fd->read ||
       !fd->write ||
       !fd->close ||
       !fd->detach ||
       !fd->snprint)
        return 1;

    if(fd->open_file_config) 
        if(!fd->close_file_config || !fd->commit_file_config)
            return 1;

    if(fd->open_global_config) 
        if(!fd->close_global_config || !fd->commit_global_config)
            return 1;

    for(l = drivers; l; l = l->next) {

        tmp = (struct file_driver *)l->data;

        if(!strcmp(tmp->name, fd->name)) {
            FAIL("already registered %s\n", fd->name);
            return 1;
        }

    }

    drivers = g_list_insert_sorted(drivers, fd, file_compare_drivers);
    DEBUG("registered %s\n", fd->name);

    return 0;
}

/**
 * Probe the file using all available drivers, pick the driver that
 * suits us best.
 */

int
file_identify(struct file *file,
              struct file_params *params) {
    GList *l;
    struct file_driver *fd;
    struct cmd_value *r;

    for(l = drivers; l; l = l->next) {
        fd = (struct file_driver *)l->data;
        DEBUG("probing %s using driver %s\n", file->name, fd->name);
        r = fd->attach(file, NULL);
        if(cmd_is_error(r)) {
            cmd_destroy_value(r);
            continue;
        }
        cmd_destroy_value(r);

        r = fd->open(file, "r", params);
        if(!cmd_is_error(r)) {
            DEBUG("successful probe %s\n", file->name);
            cmd_destroy_value(r);
            return 0;
        }
        cmd_destroy_value(r);
        fd->detach(file);
    }
    return 1;
}

void
file_destroy(struct file *file) {
    DEBUG("destroy %p, use: %d\n", file, file->use);
    if(!file)
        return;
    file->use--;
    if(file->use)
        return;
    if(file->driver)
        file->driver->detach(file);
    if(file->name)
        free(file->name);
    mem_free(file);
}

void
file_addref(struct file *file) {
    file->use++;
}

int
file_set_name(struct file *file,
              const char *name) {
    char *s = strdup(name);

    if(!s)
        return 1;

    if(file->name)
        mem_free(file->name);

    file->name = s;

    return 0;
}

struct file *
file_new(const char *name) {
    struct file *file = mem_alloc(sizeof(*file));
    
    if(!file)
        return NULL;

    file->use = 1;
    file->driver = NULL;
    file->driver_data = NULL;

    file->name = strdup(name);
    if(!file->name) {
        mem_free(file);
        return NULL;
    }

    return file;
}

void
file_setup_post(shell *shl, 
                const char *filename) {
    char usxpath[512], key[512];
    if(!pref_get_as_int("restore_scrollbar_positions"))
        return;
    snprintf(usxpath, 512, "%s.usx", filename);
    snprintf(key, 512, "=%s=/View/Horizontal Scrollbar=0", usxpath);
    view_set_hpos(shl->view, gnome_config_get_float(key));
    snprintf(key, 512, "=%s=/View/Vertical Scrollbar=0", usxpath);
    view_set_vpos(shl->view, gnome_config_get_float(key));
}

int
file_load_settings(shell *shl,
                   const char *filename) {
    char usxpath[512], key[512];
    int i, w_x, w_y, w_width, w_height;
    GdkWindow *window;
    struct stat stats;

    snprintf(usxpath, 512, "%s.usx", filename);
    if(!strcmp(usxpath, filename)) {
        FAIL("%s == %s, did not load settings\n", usxpath, filename);
        return -1;
    }

    if(stat(usxpath, &stats) == -1)
        return -1;

    mixer_load(shl->clip->mixer, usxpath);
    grid_load(&shl->grid, usxpath);
    for(i = 0; i < shl->clip->markers->len; i++)
        marker_list_load(shl->clip->markers->lists[i], usxpath, i);
    snprintf(key, 512, "=%s=/Selection/Selection Start=0", usxpath);
    shl->select_start = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/Selection/Selection End=0", usxpath);
    shl->select_end = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/Selection/Selection Channel Map", usxpath);
    shl->select_channel_map = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/Selection/Loop Start=0", usxpath);
    shl->loop_start = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/Selection/Loop End=0", usxpath);
    shl->loop_end = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/Selection/Loop Enabled=0", usxpath);
    shl->loop = gnome_config_get_int(key);

    snprintf(key, 512, "=%s=/View/Horizontal Zoom=128", usxpath);
    view_set_hres(shl->view, gnome_config_get_float(key));
    snprintf(key, 512, "=%s=/View/Vertical Zoom=128", usxpath);
    view_set_vres(shl->view, gnome_config_get_int(key));

    snprintf(key, 512, "=%s=/View/Window X Position=-1", usxpath);
    w_x = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/View/Window Y Position=-1", usxpath);
    w_y = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/View/Window Width=-1", usxpath);
    w_width = gnome_config_get_int(key);
    snprintf(key, 512, "=%s=/View/Window Height=-1", usxpath);
    w_height = gnome_config_get_int(key);
    
    window = gdk_window_get_toplevel(view_get_widget(shl->view, "wavecanvas")->window);
    if(pref_get_as_int("restore_window_positions") && 
       w_x >= 0 && w_y >= 0 && w_width >= 0 && w_height >= -1) 
        gdk_window_move_resize(window, w_x, w_y, w_width, w_height);

    return 0;
}

struct cmd_value *
file_load_stage_two(shell *shl,
                    struct file *file,
                    struct file_params *params,
                    void *bufi,
                    void **bufn,
                    AFframecount buflen) {
    int i;
    char path[512];
    struct cmd_value *r = NULL;
    struct timeval tv_start, tv_stop;
    track_map_t input_map;
    long frame_count, frame_offset, total, read, draw_remain = 0;
    float perc, seconds;

    input_map = 0;
    for(i = 0; i < params->channels; i++)
        input_map += (1 << i);

    shl->select_channel_map = input_map;

    /* Try to load settings. Try to load old-style .umix if .usx file
       does not exist. */

    if(file_load_settings(shl, file->name)) {
        snprintf(path, 512, "%s.umix", file->name);
        if(strcmp(path, file->name)) 
            mixer_load_compat(shl->clip->mixer, path);
        else 
            FAIL("%s == %s, did not load mixer settings\n", path, file->name);
    }
    
    grid_rate_set(shell_get_grid(shl), shl->clip->sr->rate);
    view_sync_grid_display(shl->view);

    /* Load sound file. */

    gettimeofday(&tv_start, NULL);
    rwlock_rlock(&shl->clip->sr->rwl);
    
    frame_count = params->frame_count;
    total = 0;

    for(frame_offset = 0; 
        frame_offset < frame_count || frame_count == -1; 
        frame_offset += read) {

        read = file->driver->read(file, bufi, buflen);
        
        if(read <= 0)
            break;

        snd_puti(shl->clip->sr,
                 bufi,
                 bufn,
                 params->channels,
                 input_map,
                 frame_offset,
                 read);

        if(error_thrown(ERROR(shl->clip->sr))) {
            r = cmd_new_error_val("Error while loading %s: %s", 
                                  shl->file->name,
                                  error_get_message(ERROR(shl->clip->sr)));
            break;
        }
        
        if(frame_count == -1)
            perc = 100;
        else
            perc = (float) ((float) total / (float) (frame_count));
        view_set_progress(shl->view, perc);
        
        draw_remain -= read;
        if(draw_remain < 0) {
            view_redraw(shl->view);
            draw_remain = buflen;
        }
        total += read;

        /* Process GUI events, exit if that kills us. */

        if(arbiter_yield() || shl->cancel_requested) {
            DEBUG("exiting per cancel request\n");
            break;
        }
    }

    rwlock_runlock(&shl->clip->sr->rwl);

    DEBUG("loaded %ld frames, expected %ld frames, calculated %ld frames\n",
          total, frame_count, snd_frame_count(shl->clip->sr, MAP_ALL));
    
    if(total != frame_count && !shl->cancel_requested && 
       frame_count != -1 && !error_thrown(ERROR(shl->clip->sr)))
        r = cmd_new_error_val("File might be damaged, expected %ld frames "
                              "but read only %ld", frame_count, total);
    
    gettimeofday(&tv_stop, NULL);
    seconds = tv_diff_secs(&tv_start, &tv_stop);
    
    INFO("read %ld frames (%ld bytes) in %f secs (%.3f KB/sec)\n", 
         snd_frame_count(shl->clip->sr, MAP_ALL), 
         (snd_frame_count(shl->clip->sr, MAP_ALL) * 
          sample_get_width(params->sample_type) * params->channels), seconds,
         (snd_frame_count(shl->clip->sr, MAP_ALL) * 
          sample_get_width(params->sample_type) * params->channels / seconds) /
         1024);

    /* Enforce the settings read from the settings file. */

    file_setup_post(shl, file->name);

    return r ? r : cmd_new_void_val();
}

struct cmd_value *
file_load_internal(shell *shl,
                   struct file *file) {
    int i;
    struct cmd_value *r = NULL;
    void *bufi = NULL, *bufn[MAX_TRACKS] = { NULL };
    int buflen;
    struct file_params params;

    if(access(file->name, R_OK)) 
        return cmd_new_error_val("The file %s doesn't seem to exist",
                                 file->name);

    if(file_identify(file, &params))
        return cmd_new_error_val("Unknown file format for %s", file->name);
    
    if(params.channels < 1 ||
       params.channels > pref_get_as_int("max_tracks")) {
        r = cmd_new_error_val("File contains an unsupported number of "
                              "tracks (%d), try increasing maximum number "
                              "of tracks in Preferences.", params.channels);
        goto failed;
    }

    /* Setup buffers for loading the file. */
    
    buflen = LOAD_BUF_SIZE * sample_get_width(params.sample_type);
    bufi = mem_alloc(buflen * params.channels);
    if(!bufi) {
        r = cmd_new_error_val("Cannot allocate load buffer");
        goto failed;
    }
    for(i = 0; i < params.channels; i++) {
        bufn[i] = mem_alloc(buflen);
        if(!bufn[i]) {
            r = cmd_new_error_val("Cannot allocate load buffers");
            goto failed;
        }
    }

    if(shell_configure(shl, file->name, &params)) {
        r = cmd_new_error_val("Could not reconfigure shell");
        goto failed;
    }

    /*
     * Point of no return: destroy the original content of the shell
     * and replace it with the file we're about to load.
     */

    shell_attach_file(shl, file);

    r = file_load_stage_two(shl, file, &params, bufi, bufn, LOAD_BUF_SIZE);

    /* FIXME: need to do something with return value. */

    cmd_destroy_value(file->driver->close(file));

    mem_free(bufi);
    for(i = 0; i < params.channels; i++)
        if(bufn[i])
            mem_free(bufn[i]);    

    return r;
    
 failed:
    if(file) {
        file->driver->close(file);
        file->driver->detach(file);
    }
    if(file)
        file_destroy(file);
    if(bufi)
        mem_free(bufi);
    for(i = 0; i < MAX_TRACKS; i++)
        if(bufn[i])
            mem_free(bufn[i]);
    return r ? r : cmd_new_error_val("Unspecified error");
}


struct cmd_value *
file_load(shell *shl,
          struct file *file) {
    struct cmd_value *r;

    view_set_cursor(shl->view, GDK_WATCH);
    
    r = file_load_internal(shl, file);
    
    shl->has_name = (cmd_is_error(r) ? 0 : 1);

    /* If the load was cancelled the file is missing some data,
       so make sure that when the user hits "Save" he is asked
       to specify a new name for the file. */

    if(shl->cancel_requested)
        shl->has_name = 0;
    
    view_set_cursor(shl->view, view_get_default_cursor(shl->view));

    return r;
}


void
file_save_settings(shell *shl,
                   mixer *mixer,
                   const char *filename) {
    char usxpath[512], gcpath[512], key[512];
    int i;
    int w_width, w_height, w_x, w_y;
    GdkWindow *window;

    snprintf(usxpath, 512, "%s.usx", filename);
    if(!strcmp(usxpath, filename)) {
        FAIL("%s == %s, did not save settings\n", usxpath, filename);
        return;
    }
    snprintf(gcpath, 512, "=%s=", usxpath);

    DEBUG("gcpath: %s, usxpath: %s\n", gcpath, usxpath);

    gnome_config_clean_file(gcpath);
    
    mixer_save(mixer, usxpath);
    grid_save(&shl->grid, usxpath);
    for(i = 0; i < shl->clip->markers->len; i++)
        marker_list_save(shl->clip->markers->lists[i], usxpath, i);
    snprintf(key, 512, "=%s=/Selection/Selection Start", usxpath);
    gnome_config_set_int(key, shl->select_start);
    snprintf(key, 512, "=%s=/Selection/Selection End", usxpath);
    gnome_config_set_int(key, shl->select_end);
    snprintf(key, 512, "=%s=/Selection/Selection Channel Map", usxpath);
    gnome_config_set_int(key, shl->select_channel_map);
    snprintf(key, 512, "=%s=/Selection/Loop Start", usxpath);
    gnome_config_set_int(key, shl->loop_start);
    snprintf(key, 512, "=%s=/Selection/Loop End", usxpath);
    gnome_config_set_int(key, shl->loop_end);
    snprintf(key, 512, "=%s=/Selection/Loop Enabled", usxpath);
    gnome_config_set_int(key, shl->loop);

    snprintf(key, 512, "=%s=/View/Horizontal Zoom", usxpath);
    gnome_config_set_float(key, shl->view->hres);
    snprintf(key, 512, "=%s=/View/Vertical Zoom", usxpath);
    gnome_config_set_int(key, shl->view->vres);

    snprintf(key, 512, "=%s=/View/Horizontal Scrollbar", usxpath);
    gnome_config_set_float(key, shl->view->hadjust->value);
    snprintf(key, 512, "=%s=/View/Vertical Scrollbar", usxpath);
    gnome_config_set_float(key, shl->view->vadjust->value);

    window = gdk_window_get_toplevel(view_get_widget(shl->view, "wavecanvas")->window);
    gdk_window_get_geometry(window,
                            NULL, NULL, &w_width, &w_height, NULL);
    gdk_window_get_root_origin(window, &w_x, &w_y);

    snprintf(key, 512, "=%s=/View/Window X Position", usxpath);
    gnome_config_set_int(key, w_x);
    snprintf(key, 512, "=%s=/View/Window Y Position", usxpath);
    gnome_config_set_int(key, w_y);
    snprintf(key, 512, "=%s=/View/Window Width", usxpath);
    gnome_config_set_int(key, w_width);
    snprintf(key, 512, "=%s=/View/Window Height", usxpath);
    gnome_config_set_int(key, w_height);

    gnome_config_sync_file(gcpath);
}

               
struct cmd_value *
file_save_internal(shell *shl,
                   mixer *mixer,
                   struct file *file) {
    struct cmd_value *r = NULL, *r2 = NULL;
    AFframecount offset, count, got, total_written = 0;
    int written = 0;
    void *bufi, *bufn[pref_get_as_int("max_tracks")];
    //    frame_bits_t fb_muxbuf, fb_srcbufs[pref_get_as_int("max_tracks")];
    snd *sr = shl->clip->sr;
    struct timeval tv_start, tv_stop;
    struct file_params params;
    float perc, seconds;

    DEBUG("in save internal\n");

    /* Setup local stuff. */
    
    bufi = mixer_buffers_alloc(sample_get_width(sr->sample_type),
                               MAX(mixer->source_tracks,
                                   mixer->output_channels), 
                               &bufi,
                               bufn,
                               SAVE_BUF_SIZE);
    
    if(!bufi) 
        return cmd_new_error_val("Cannot allocate output buffers.");

    params.channels = mixer->output_channels;
    params.sample_type = sr->sample_type;
    params.frame_count = snd_frame_count(sr, MAP_ALL);
    params.sample_rate = sr->rate;

    DEBUG("file: %p, driver: %p\n", file, file->driver);

    r = file->driver->open(file, "w", &params);
    if(cmd_is_error(r)) 
        goto recover;
    cmd_destroy_value(r);
    r = NULL;

    /* Try to save settings. */
    
    file_save_settings(shl, shl->clip->mixer, file->name);

    gettimeofday(&tv_start, NULL);

    DEBUG("starting mux-to-disk: name: %s, frame_count: %ld\n",
          file->name, snd_frame_count(sr, MAP_ALL));

    view_set_progress(shl->view, 0);
    count = snd_frame_count(shl->clip->sr, MAP_ALL);
    offset = 0;
    got = 1;
    while(count && got) {
        arbiter_yield();

        memset(bufi, 
               '\0', 
               (SAVE_BUF_SIZE * sample_get_width(sr->sample_type) * 
                mixer->output_channels));
        
        got = snd_getn(shl->clip->sr,
                       bufn,
                       MAP_ALL,
                       offset,
                       SAVE_BUF_SIZE);
        
        mixer_mixi(mixer,
                   shl->clip->sr->sample_type,
                   bufi,
                   bufn,
                   got);

        written = file->driver->write(file, bufi, got);
        if(written == -1) 
            break;
        
        total_written += written;
        count -= got;
        offset += got;

        /* Progress meter behaves strange for values around
           0.01. */
        
        perc = (float) ((float) offset / 
                        (float) (snd_frame_count(shl->clip->sr, MAP_ALL)));
        view_set_progress(shl->view, perc);
    }
    view_set_progress(shl->view, 0);
    
    if(count || total_written != snd_frame_count(shl->clip->sr, MAP_ALL)) 
        r = cmd_new_error_val("Save %s: not all frames were written "
                              "(%ld of %ld). Some data is not on disk. "
                              "Check disk space",
                              file->name, 
                              total_written,
                              snd_frame_count(shl->clip->sr, MAP_ALL));

    r2 = file->driver->close(file);
    if(cmd_is_error(r2)) {
        if(r) {
            r = cmd_new_error_val("An error occurred trying to close %s. "
                                  "Some data may not have been written to "
                                  "disk. Check disk space (%s). In addition "
                                  "the following error occurred: %s",
                                  file->name, cmd_get_error_message(r2),
                                  cmd_get_error_message(r));
        } else {
            r = cmd_new_error_val("An error occurred trying to close %s. "
                                  "Some data may not have been written to "
                                  "disk. Check disk space (%s)",
                                  file->name, cmd_get_error_message(r2));
        }

    }
    cmd_destroy_value(r2);

    gettimeofday(&tv_stop, NULL);
    seconds = tv_diff_secs(&tv_start, &tv_stop);

    INFO("wrote %ld frames (%ld bytes) in %f secs (%.3f KB/sec)\n",
         total_written,
         (total_written * sample_get_width(sr->sample_type) * 
          mixer->output_channels),
         seconds,
         (total_written * sample_get_width(sr->sample_type) * 
          mixer->output_channels / seconds) / 1024);

    mixer_buffers_free(MAX(mixer->source_tracks, mixer->output_channels), 
                       bufi, bufn);
    return r ? r : cmd_new_void_val();

 recover:
    mixer_buffers_free(MAX(mixer->source_tracks, mixer->output_channels),
                       bufi, bufn);
    return r;
}



struct cmd_value *
file_save_or_mixdown(shell *shl, 
                     mixer *output_mixer,
                     struct file *file,
                     gboolean keep_backup,
                     gboolean is_mixdown) {
    char *newname, *name = file->name;
    struct cmd_value *r = NULL;

    /* Rename old file. */

    if(access(name, R_OK) == 0 && keep_backup) {
        newname = mem_alloc(strlen(name) + 5);
        if(!newname) 
            return cmd_new_error_val("Cannot %s, very low memory",
                                     is_mixdown ? "mixdown" : "save");
        
        strcpy(newname, name);
        strcat(newname, ".bak");

        if(rename(name, newname)) {
            r = cmd_new_error_val("Cannot %s, unable to rename %s to "
                                  "%s.bak (%s)",
                                  is_mixdown ? "mixdown" : "save", 
                                  name, newname,
                                  strerror(errno));
            mem_free(newname);
            return r;
        }

        DEBUG("renamed '%s' to '%s'\n", name, newname);
        mem_free(newname);
    }

    /* Setup output mixer and channels. */

    /* Do output mix. */

    view_push_status(shl->view, "%s %s ...", 
                     is_mixdown ? "Mixdown" : "Saving", name);
    view_set_cursor(shl->view, GDK_WATCH);

    r = file_save_internal(shl, output_mixer, file);
    if(cmd_is_error(r) && !is_mixdown) 
        shl->has_changed = 1;
    else 
        shl->has_changed = 0;

    view_set_cursor(shl->view, view_get_default_cursor(shl->view));
    view_reset_status(shl->view);
    view_sync_grid_display(shl->view);
    view_redraw(shl->view);

    return r ? r : cmd_new_void_val();
}

struct cmd_value *
file_mixdown(shell *shl,
             mixer *mixer,
             struct file *file,
             gboolean keep_backup) {
    return file_save_or_mixdown(shl,
                                mixer,
                                file,
                                keep_backup,
                                TRUE);
}

struct cmd_value *
file_save(shell *shl, 
          struct file *file,
          gboolean keep_backup) {
    struct cmd_value *r;

    mixer *output_mixer = mixer_new(shl->clip->sr->channels, 
                                    shl->clip->sr->channels);        
    if(!output_mixer) 
        return cmd_new_error_val("Cannot create output mixer for %d "
                                 "tracks. Very low memory.", 
                                 shl->clip->sr->channels);
    output_mixer->is_unity = 1;
    shl->has_name = 1;
    r = file_save_or_mixdown(shl,
                             output_mixer,
                             file,
                             keep_backup,
                             FALSE);
    mixer_destroy(output_mixer);
    return r;
}
