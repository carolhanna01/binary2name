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
#include <gnome.h>
#include "tests.h"
#include "lib/misc.h"
#include "mem.h"
#include "snd.h" /* SAMPLE_TYPE defs */
#include "block.h" /* block_set_calculate_rms() prototype */
#include "pref.h"

static int pref_count = 0;
static struct pref *prefs = NULL;

/* Validation functions for preferences. */

int
pref_verify_enable_rms(const struct pref *pref,
                       union pref_value *value) {
    block_set_calculate_rms(value->i);
    return 0;
}

int
pref_verify_sample_width(const struct pref *pref,
                         union pref_value *value) {
    return !(value->i != 1 ||
             value->i != 2 ||
             value->i != 4);
    
}

int
pref_verify_sample_type(const struct pref *pref,
                        union pref_value *value) {
    switch(value->i) {
    case SAMPLE_TYPE_INT_8:
    case SAMPLE_TYPE_INT_16:
    case SAMPLE_TYPE_INT_32:
    case SAMPLE_TYPE_FLOAT_32:
        return 0;
    }
    return 1;    
}

/* 
 * The table with default settings. You're probably better off editing
 * this stuff in the ~/.gnome/gnusound or ~/.gnome2/gnusound
 * configuration file.
 *
 * Only very few prefs actually get used regularly and need fast
 * access. These are simply put at the top. In addition, this table
 * gets sorted by access count at runtime.
 */

static struct pref default_prefs[] = { 
    
    /* Put LAME last since it gets autodetection wrong often. */

    PREF_STRING("file.drivers.probe_order", 
                "flac,audiofile,sndfile,ffmpeg,lame",
                "Comma-separated list describing the probing order for "
                "file format drivers."),

    PREF_BOOL_FULL("view.wave.draw.rms.enable", 0, 0,
                   pref_verify_enable_rms,
                   "Whether to draw the waveform RMS. Disabling gives "
                   "faster display."),
    PREF_INT("view.wave.draw.rms.minimum_zoom", 64, 1, HRES_MAX,
             "From what zoom factor the waveform RMS should be drawn."),
    PREF_BOOL("view.wave.draw.connect_samples", 1,
              "Whether to draw samples as if they were contiguous or not."),
    
    PREF_FLOAT("audio_chunk_duration", 0.1, 0.001, 100,
               "This value determines the maximum audio buffer size for "
               "recording and playback, in seconds."),
    PREF_INT_FULL("max_tracks", PREF_WRITEONCE, 16, 1, MAX_TRACKS, NULL,
                  "The maximum number of tracks that can be edited."),

    /*
     * The number of pixels between two mouse wheel event locations
     * that make them separate events.
     */
    
    PREF_INT("wheel_x_treshold", 15, 0, 1000,
             "Undocumented"),
    
    PREF_INT("scrub_silent_amount", 128, 1, 16384,
             "By how many samples the scrub command moves the viewport "
             "left or right when not playing back."),
    PREF_INT("scrub_playback_amount", 32768, 1, 131072,
             "By how many samples the scrub command moves the playback "
             "cursor left or right during playback."),
    PREF_INT("scrub_fast_multiplier", 4, 1, 256,
             "By what amount to multiply scrub movement when Fast scrubbing."),

    /* 
     * Number of undo's. 
     */

    PREF_INT("max_undo_depth", 40, 0, 1000,
             "The maximum number of Undo's."),

    /* 
     * If your recording is always "too late" then you can use this
     * parameter to throw away the first few frames of your recording
     * and get better sync. Not very elegant but it works. FIXME:
     * feature removed due to player engine rewrite, maybe add again
     * later.
     */

    //    PREF_INT("record_discard_frames", 0, 0, 100000), 
    PREF_FLOAT("default_bpm", 120, 1, 10000,
               "The default beats per minute when none has been specified."),
    
    PREF_FLOAT("default_sample_rate", 44100, 1, 1000000,
               "The default sample rate when creating a new document"),
    PREF_INT_FULL("default_sample_type", 0, 1, 1, 10, 
                  pref_verify_sample_type,
                  "The default sample type when creating a new document."),
    PREF_BOOL("snap_to_grid", 0, 
              "Determines whether to automatically snap the selection "
              "at grid positions."),
    PREF_BOOL("snap_to_cuepoints", 0,
              "Determines whether to automatically snap the selection "
              "at cuepoint positions."),
    PREF_BOOL("show_grid", 1,
              "Whether to show the grid."),
    PREF_BOOL("show_zero", 1,
              "Whether to show the amplitude 0 line."),
    PREF_BOOL("show_envelope", 1,
              "Whether to show envelopes."),
    PREF_BOOL("record_replace", 0,
              "Whether record should overwrite data underneath the recording "
              "cursor or insert the recorded data before the recording "
              "cursor."),
    PREF_BOOL("follow_playback", 1,
              "Whether the view should track the playback position."),
    PREF_BOOL("restore_window_positions", 1,
              "Whether to restore the last saved window positions when "
              "opening a document."),
    PREF_BOOL("restore_scrollbar_positions", 1,
              "Whether to restore the last saved scrollbar positions when "
              "opening a document."),

    /*
     * How often (in millisecs) to update the display during
     * playback.
     */

    PREF_INT("playback_display_interval", 50, 10, 5000,
             "How often to update the display during playback (in "
             "milliseconds)."),
    PREF_STRING("audio_driver", "OSS",
                "Which audio driver to use."),

    /*
     * Colors. 
     */

    PREF_STRING("colors.background", "#000000", "Undocumented"),
    PREF_STRING("colors.wave.peaks", "#00CC44", "Undocumented"),
    PREF_STRING("colors.wave.rms", "#009922", "Undocumented"),
    PREF_STRING("colors.selection", "#005522", "Undocumented"),
    PREF_STRING("colors.selection.background", "#000000", "Undocumented"),
    PREF_STRING("colors.wave.peaks.selected", "#008833", "Undocumented"),
    PREF_STRING("colors.wave.rms.selected", "#E5E5E5", "Undocumented"),

    PREF_STRING("colors.point_record", "#FF0000", "Undocumented"),
    PREF_STRING("colors.point_play", "#00FF00", "Undocumented"),
    PREF_STRING("colors.zero", "#00CC44", "Undocumented"),
    PREF_STRING("colors.block", "#ff0000", "Undocumented"),
    PREF_STRING("colors.loop", "#FFFF00", "Undocumented"),
    PREF_STRING("colors.grid", "#4a4a4a", "Undocumented"),
    PREF_STRING("colors.grid_font", "#E5E5E5", "Undocumented"),
    PREF_STRING("colors.info_font", "#E5E5E5", "Undocumented"),
    PREF_STRING("colors.info_font_record", "#FF0000", "Undocumented"),
    PREF_STRING("colors.info_font_play", "#00FF00", "Undocumented"),
    PREF_STRING("colors.marker_slope_main", "#6BF0F9", "Undocumented"),
    PREF_STRING("colors.marker_slope_aux", "#489FA5", "Undocumented"),
    PREF_STRING("colors.marker_slope_main_disabled", "#858585", "Undocumented"),
    PREF_STRING("colors.marker_slope_aux_disabled", "#555555", "Undocumented"),
    PREF_STRING("colors.marker_text_background", "#FF0000", "Undocumented"),
    PREF_STRING("colors.marker_text", "#FFFFFF", "Undocumented"),
    PREF_STRING("colors.marker_text_nonactive", "#aaaaaa", "Undocumented"),
    PREF_STRING("colors.toggles_mute", "#0000FF", "Undocumented"),
    PREF_STRING("colors.toggles_solo", "#FFFF00", "Undocumented"),
    PREF_STRING("colors.track_divider", "#333333", "Undocumented"),

    /* 
     * Pixmaps.
     */

    PREF_STRING("pixmaps.mixer.level", "lvl.xpm", "Undocumented"),
    PREF_STRING("pixmaps.mixer.mute_on", "mute_on.xpm", "Undocumented"),
    PREF_STRING("pixmaps.mixer.mute_off", "mute_off.xpm", "Undocumented"),
    PREF_STRING("pixmaps.mixer.solo_on", "solo_on.xpm", "Undocumented"),
    PREF_STRING("pixmaps.mixer.solo_off", "solo_off.xpm", "Undocumented"),
    PREF_STRING("pixmaps.wave.background", "wave_bg.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.font_big", "dash_font_big.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.record_on", "dash_record_on.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.record_off", "dash_record_off.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.play_on", "dash_play_on.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.play_off", "dash_play_off.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.loop_on", "dash_loop_on.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.loop_off", "dash_loop_off.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.xrun_on", "dash_xrun_on.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.xrun_off", "dash_xrun_off.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.replace_on", "dash_replace_on.xpm", "Undocumented"),
    PREF_STRING("pixmaps.dash.replace_off", "dash_replace_off.xpm", "Undocumented"),
    PREF_STRING("pixmaps.icons.small.warning", "warning.xpm", "Undocumented"),
    PREF_STRING("pixmaps.icons.small.error", "error.xpm", "Undocumented"),
    PREF_STRING("pixmaps.mix_tool.source", "mix_tool_source.xpm", "Undocumented"),
    PREF_STRING("pixmaps.mix_tool.current_pos", "mix_tool_curpos.xpm", "Undocumented"),
    
};

#define DUMP_VALUE(name, thing, type, value) \
  DEBUG("%s%s", name, thing); \
  switch(type) { \
    case PREF_INT_T: \
      printf(".i: %d\n", (value)->i); \
      break; \
    case PREF_FLOAT_T: \
      printf(".f: %f\n", (value)->f); \
      break; \
    case PREF_STRING_T: \
      printf(".s: %s\n", (value)->s); \
      break; \
  } 

void
pref_dump(struct pref *p) {
    DEBUG("type: %d\n", p->type);
    DEBUG("flags: %x\n", p->flags);
    DUMP_VALUE(p->name, "[val]:", p->type, &p->value);
    DUMP_VALUE(p->name, "[def]:", p->type, &p->def);
    DUMP_VALUE(p->name, "[sync]:", p->type, &p->sync);
}

/**
 * Returns a list of prefs matching the given pattern.  The pattern
 * can contain ? and * wildcards. Wildcard escaping is not supported.
 *
 * @param pattern The pattern to match.
 * @return NULL terminated array of prefs matching given pattern, or NULL
 * on error. If non-NULL, the return value must be free()'d by the caller.
 */

char **
pref_match(const char *pattern) {
    int i, j = 0;
    int indexes[pref_count];
    int sizes[pref_count];
    int total = 0;
    char **buf;

    for(i = 0; i < pref_count; i++) {
        indexes[i] = -1;
        sizes[i] = 0;
        if(!match_wildcard(pattern, prefs[i].name)) {
            indexes[j] = i;
            sizes[j] = strlen(prefs[i].name) + 1;
            total += sizes[j];
            j++;
        }
    }

    buf = mem_calloc(1, ((j + 1) * sizeof(char *)) + total);
    if(!buf)
        return NULL;
    buf[j] = NULL;

    for(i = 0, total = 0; i < j; i++) {
        buf[i] = (char *)buf + ((j + 1) * sizeof(char *)) + total;
        strcpy(buf[i], prefs[indexes[i]].name);
        total += sizes[i];
    }
    return buf;
}

int 
pref_alphabetical(const void *p1,
                  const void *p2) {
    return strcmp(((struct pref *)p1)->name,
                  ((struct pref *)p2)->name);
}

void
pref_list() {
    int i;
    qsort(prefs, pref_count, sizeof(struct pref), pref_alphabetical);
    for(i = 0; i < pref_count; i++) {
        printf("%s\t\%s\n", prefs[i].name, prefs[i].description);
    }
    pref_optimize();
}

int
pref_accessed(const void *p1, 
              const void *p2) {
    return ((struct pref *)p2)->accessed - ((struct pref *)p1)->accessed;
}

/**
 * Sorts the pref array so that the most accessed prefs are
 * at the top.
 */

void
pref_optimize() {
    int i;

    /*
    for(i = 0; i < pref_count; i++)
        if(prefs[i].accessed > 4)
            DEBUG("[%d] %d %s\n", i, prefs[i].accessed, prefs[i].name);
    */
    qsort(prefs, pref_count, sizeof(struct pref), pref_accessed);
    /*
    for(i = 0; i < pref_count; i++)
        if(prefs[i].accessed > 4)
            DEBUG("[%d] %d %s\n", i, prefs[i].accessed, prefs[i].name);
    */

}

/**
 * Finds a pref by name.
 * @param name The pref to find.
 * @return The requested pref or NULL if it does not exist.
 */

struct pref *
pref_find(const char *name) {
    int i;
    for(i = 0; i < pref_count; i++) 
        if(!strcmp(prefs[i].name, name)) 
            return &(prefs[i]);
    return NULL;
}

/**
 * Returns the value for the pref indicated by name, or NULL on error.
 *
 * If the requested pref has not been set using pref_set() yet, then
 * the default value for the pref is returned. Otherwise the value
 * previously set by pref_set() is returned.
 *
 * If it's certain that the pref exists, use
 * pref_get_as_{int,float,string} instead.
 * 
 * @param name The pref to get.
 * @return The value for the pref or NULL if the pref doesn't exist.
 */

const union pref_value *
pref_get_value(const char *name) {
    struct pref *p = pref_find(name);
    if(!p)
        return NULL;
    p->accessed++;
    if(!(p->flags & PREF_HASBEENSET))
        return &p->def;
    return &p->value;
}

int
pref_get_as_int(const char *name) {
    const union pref_value *v = pref_get_value(name);
    if(v) 
        return v->i;
    FAIL("warning: could not locate int pref %s, returning 0.\n",
         name);
    return 0;
}

float
pref_get_as_float(const char *name) {
    const union pref_value *v = pref_get_value(name);
    if(v) 
        return v->f;
    FAIL("warning: could not locate float pref %s, returning 0.\n",
         name);
    return 0;
}

const char *
pref_get_as_string(const char *name) {
    const union pref_value *v = pref_get_value(name);
    if(v) 
        return v->s;
    FAIL("warning: could not locate string pref %s, returning 0.\n",
         name);
    return 0;
}

/**
 * Sets the pref indicated by name to the value given by value. 
 *
 * If the value is invalid for the given pref and cannot be
 * constrained to a valid value, this function returns a non-zero
 * error code. Otherwise the given value is constrained as necessary,
 * set as the new value for the pref, and 0 is returned.
 *
 * If the given pref has the PREF_WRITEONCE flag set, then only the
 * first successfull call to pref_set() will change the value as
 * returned by pref_get_value() and pref_get_as_*(). Subsequent calls
 * will not modify the value returned by these functions, but the 
 * new value will be written to disk on pref_sync().
 *
 * To test whether the given value was successfully set but
 * constrained, pref_get_*() should be used to retrieve the new value
 * and this value should be compared with the expected value.
 */

int
pref_set(struct pref *p,
         union pref_value *value) {
    char *s;
    union pref_value *dest;
    const char *name = p->name;

    dest = &p->value;

    //    DUMP_VALUE(name, "", p->type, value);

    /* 
     * For WRITEONCE prefs, after initialization, don't update the
     * "real" value but the sync value instead.
     */

    if((p->flags & PREF_WRITEONCE) &&
       (p->flags & PREF_HASBEENSET)) 
        dest = &p->sync;
    
    if(p->constrain && p->constrain(p, value))
        return 1;

    switch(p->type) {
    case PREF_FLOAT_T:
        if(p->min != p->max && 
           (value->f < p->min || value->f > p->max)) {
            DEBUG("clamping value for %s, lower bound: %f, "
                  "upper bound: %f, requested: %f\n",
                  p->name, p->min, p->max, value->f);
            value->f = CLAMP(value->f, p->min, p->max);
        }
        dest->f = value->f;
        break;
    case PREF_INT_T:
        if(p->min != p->max && 
           (value->i < p->min || value->i > p->max)) {
            DEBUG("clamping value for %s, lower bound: %f, "
                  "upper bound: %f, requested: %d\n",
                  p->name, p->min, p->max, value->i);
            value->i = CLAMP(value->i, p->min, p->max);
        }
        dest->i = value->i;
        break;
    case PREF_STRING_T:
        if(value->s == NULL) {
            FAIL("NULL value for %s, not setting.\n", name);
            return 1;
        }
        s = strdup(value->s);
        if(!s) {
            FAIL("could not set %s, could not copy string %s\n",
                 name, value->s);
            return 1;
        }
        if(dest->s)
            free(dest->s);
        dest->s = s;
        break;
    }
    if((p->flags & PREF_WRITEONCE) &&
       !(p->flags & PREF_HASBEENSET))
        p->sync = p->value;
    
    p->flags |= PREF_HASBEENSET;
    return 0;
}

int 
pref_set_float(const char *name,
               float f) {
    union pref_value v;
    struct pref *p = pref_find(name);
    if(!p) 
        return 1;
    if(p->type != PREF_FLOAT_T) {
        FAIL("attempt to set float value %f on non-float type %s.\n",
             f, name);
        return 1;
    }
    v.f = f;
    return pref_set(p, &v);
}

int 
pref_set_int(const char *name,
             int i) {
    union pref_value v;
    struct pref *p = pref_find(name);
    if(!p) 
        return 1;
    if(p->type != PREF_INT_T) {
        FAIL("attempt to set int value %d on non-int type %s.\n",
             i, name);
        return 1;
    }
    v.i = i;
    return pref_set(p, &v);
}

int 
pref_set_string(const char *name,
                const char *s) {
    int r;
    union pref_value v;
    struct pref *p = pref_find(name);
    if(!p) 
        return 1;
    if(p->type != PREF_STRING_T) {
        FAIL("attempt to set string value %s on non-string type %s.\n",
             s, name);
        return 1;
    }
    if(!(v.s = strdup(s))) {
        FAIL("not enough memory to copy string %s while setting %s\n",
             s, name);
        return 1;
    }
    r = pref_set(p, &v);
    free(v.s);
    return r;
}

void
pref_copy_value(enum pref_type type,
                union pref_value *dst,
                union pref_value *src) {
    memcpy(dst, src, sizeof(*dst));
    if(type == PREF_STRING_T && src->s)
        dst->s = strdup(src->s);
}

void
pref_copy(struct pref *dst, 
          struct pref *src) {
    memcpy(dst, src, sizeof(*dst));
    dst->name = strdup(src->name);
    pref_copy_value(dst->type, &dst->value, &src->value);
    pref_copy_value(dst->type, &dst->def, &src->def);
    pref_copy_value(dst->type, &dst->sync, &src->sync);
}

/**
 * Registers an array of prefs.
 * @param count The number of prefs to register.
 * @param new Pointer to an array of count prefs.
 * @return 0 on success, non-zero otherwise.
 */

int
pref_register(int count, struct pref *new) {
    int i;
    struct pref *p;

    if(count < 1) {
        FAIL("cannot register 0 prefs\n");
        return 1;
    }

    p = mem_realloc(prefs, (pref_count + count) * sizeof(struct pref));
    if(!p) {
        FAIL("cannot allocate memory to register prefs\n");
        return 1;
    }

    for(i = 0; i < count; i++)
        pref_copy(&p[i + pref_count], &new[i]);

    prefs = p;
    pref_count += count;

    return 0;
}

/**
 * Loads the values for the preferences which match the given pattern.
 * The pattern can contain * and ? wildcards; wildcard escaping is
 * not supported.
 * @param pattern The pattern to match. If NULL, then all preferences 
 * will be loaded.
 * @return 0 on success, non-zero otherwise.
 */

int 
pref_load(const char *pattern) {
    int i, r = 0;
    char path[512], val[256], *s;

    /* Get stored preferences through gnome_config. */

    for(i = 0; i < pref_count; i++) {
        if(pattern != NULL) 
            if(match_wildcard(pattern, prefs[i].name)) 
                continue;
        
        switch(prefs[i].type) {
        case PREF_FLOAT_T:
            snprintf(val, 255, "%f", prefs[i].def.f);
            snprintf(path, 512, "/gnusound/preferences/%s=%s", prefs[i].name, val);
            if(pref_set_float(prefs[i].name, gnome_config_get_float(path))) {
                FAIL("error setting float pref %s\n", prefs[i].name);
                return 1;
            }
            break;
        case PREF_INT_T:
            snprintf(val, 255, "%d", prefs[i].def.i);
            snprintf(path, 512, "/gnusound/preferences/%s=%s", prefs[i].name, val);
            if(pref_set_int(prefs[i].name, gnome_config_get_int(path))) {
                FAIL("error setting int pref %s\n", prefs[i].name);
                return 1;
            }
            break;
        case PREF_STRING_T:
            snprintf(val, 255, "%s", prefs[i].def.s);
            snprintf(path, 512, "/gnusound/preferences/%s=%s", prefs[i].name, val);
            s = gnome_config_get_string(path);
            r = pref_set_string(prefs[i].name, s);
            g_free(s);
            if(r) {
                FAIL("error setting string pref %s\n", prefs[i].name);
                return 1;
            }
            break;
        }
    }
    return 0;
}

/**
 * Initializes preference system and loads stored preferences
 * into memory.
 * @return 0 on success or non-zero on error.
 */

int
pref_init() {
    int r;
    if((r = pref_register(sizeof(default_prefs) / sizeof(default_prefs[0]),
                          default_prefs)))
        return r;
    if((r = pref_load(NULL)))
        return r;
    return 0;
}

/**
 * Stores preferences.
 */

void
pref_sync() {
    int i;
    char path[512];
    union pref_value *value;
    for(i = 0; i < pref_count; i++) {
        value = &prefs[i].value;
        if(prefs[i].flags & PREF_WRITEONCE)
            value = &prefs[i].sync;
        if(!(prefs[i].flags & PREF_HASBEENSET))
            value = &prefs[i].def;
        switch(prefs[i].type) {
        case PREF_FLOAT_T:
            snprintf(path, 512, "/gnusound/preferences/%s", prefs[i].name);
            gnome_config_set_float(path, value->f);
            break;
        case PREF_INT_T:
            snprintf(path, 512, "/gnusound/preferences/%s", prefs[i].name);
            gnome_config_set_int(path, value->i);
            break;
        case PREF_STRING_T:
            snprintf(path, 512, "/gnusound/preferences/%s", prefs[i].name);
            gnome_config_set_string(path, value->s);
            break;
        }
    }
    gnome_config_sync();
}

/**
 * Stores preferences and releases all memory.
 */

void
pref_exit() {
    int i;
    pref_sync();
    for(i = 0; i < pref_count; i++) {
        if(prefs[i].type == PREF_STRING_T) {
            if(prefs[i].value.s)
                free(prefs[i].value.s);
            if(prefs[i].sync.s)
                free(prefs[i].sync.s);
            if(prefs[i].def.s)
                free(prefs[i].def.s);
        }
        free(prefs[i].name);
    }
    mem_free(prefs);
}

#define TEST(cond, msg) if(cond) { fprintf(stderr, "  " msg); r++; }

int
pref_tests(int level) {
    int i, j, c1, c2, r = 0;
    char **m;
    struct pref *p;

    if(pref_init()) {
        fprintf(stderr, "  cannot initialize pref subsys\n");
        return 1;
    }

    TEST(pref_get_as_int("max_tracks") != 16,
         "wrong default value\n");
    TEST(pref_set_int("max_tracks", 1),
         "error setting max_tracks to 1\n");
    TEST(pref_get_as_int("max_tracks") != 1,
         "max_tracks didn't contain 1\n");
    /* Second setting should succeed but not appear as max_tracks is
       PREF_WRITEONCE */
    TEST(pref_set_int("max_tracks", 2),
         "error setting max_tracks to 2\n");
    TEST(pref_get_as_int("max_tracks") != 1,
         "max_tracks was modified\n");
    p = pref_find("max_tracks");
    TEST(p->sync.i != 2,
         "max_tracks sync value not 2\n");
    TEST(pref_get_as_int("nonexistant"),
         "nonexistant int pref returns non-zero value\n");
    TEST(pref_set_int("nonexistant", 1) == 0,
         "setting non-existant pref returns success\n");

    /* matching */

    m = pref_match("pixmaps.*");
    if(!m) {
        fprintf(stderr, "  failed to get memory");
        return 1;
    }
    
    /* verify count */
    c1 = 0; c2 = 0;
    for(i = 0; i < pref_count; i++) 
        if(strstr(prefs[i].name, "pixmaps.") == prefs[i].name)
            c1++;
    for(i = 0; m[i]; i++)
        c2++;
    
    TEST(c1 != c2, "pref_match() returns wrong number of prefs\n");

    for(i = 0, j = 0; i < MIN(c1, c2); i++) 
        if(strstr(prefs[i].name, "pixmaps.") == prefs[i].name) 
            TEST(!strcmp(prefs[i].name, m[j++]),
                 "pref_match() returns wrong prefs\n");

    pref_exit();

    return r;
}
