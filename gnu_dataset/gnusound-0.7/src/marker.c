/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2005  Pascal Haakmat <a.haakmat@chello.nl>
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

/* FIXME: This file needs documentation.  Really. */

#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <audiofile.h>
#include <gnome.h>
#include <config.h>
#include "mem.h"
#include "marker.h"
#include "error.h"

/**********************************************************************/
/* Marker list array functions.                                       */
/**********************************************************************/

void
marker_list_array_replace(struct marker_list_array *dst,
                          struct marker_list_array *src,
                          track_map_t map,
                          AFframecount offset,
                          AFframecount count,
                          enum marker_type type) {
    int t, i = 0;
    
    for(t = 0; t < MAX_TRACKS; t++) {
        if(dst->lists[t]) {
            marker_list_destroy(dst->lists[t]);
            dst->lists[t] = NULL;
        }
    }
    dst->len = 0;
    
    for(t = 0; t < src->len; t++) 
        if((1 << t) & map) 
            dst->lists[i++] = 
                marker_list_clone(src->lists[t], offset, count, type);
    
    dst->len = i;
}
                          
int
marker_list_array_insert_marker(struct marker_list_array *mla,
                                track_map_t map,
                                AFframecount offset,
                                enum marker_type type,
                                float multiplier,
                                const char *label) {
    int i;

    for(i = 0; i < mla->len; i++) 
        if((1 << i) & map)
            if(!marker_list_insert(mla->lists[i],
                                   offset,
                                   type,
                                   multiplier,
                                   label))
                return 1;

    return 0;
}

struct marker *
marker_list_array_find_previous(struct marker_list_array *mla,
                                track_map_t map,
                                AFframecount frame_offset,
                                enum marker_type type) {
    int i;
    struct marker *m, *previous = NULL;
    for(i = 0; i < mla->len; i++) {
        if((1 << i) & map) {
            m = marker_list_previous(mla->lists[i],
                                     frame_offset,
                                     type);
            if(previous == NULL || 
               (m && m->frame_offset > previous->frame_offset))
                previous = m;
        }
    }
    return previous;
}

struct marker *
marker_list_array_find_next(struct marker_list_array *mla,
                            track_map_t map,
                            AFframecount frame_offset,
                            enum marker_type type) {
    int i;
    struct marker *m, *next = NULL;
    for(i = 0; i < mla->len; i++) {
        if((1 << i) & map) {
            m = marker_list_next(mla->lists[i],
                                 frame_offset,
                                 type);
            if(next == NULL || 
               (m && m->frame_offset < next->frame_offset))
                next = m;
        }
    }
    return next;
}

int
marker_list_array_append_list(struct marker_list_array *mla,
                              struct marker_list *ml) {
    if(mla->len >= MAX_TRACKS - 1) {
        error_set(ERROR(mla),
                  "Cannot append marker list, MAX_TRACKS reached");
        return 1;
    }

    if(!ml) {
        error_set(ERROR(mla),
                  "Cannot append marker list, list is NULL");
        return 1;
    }

    mla->lists[mla->len] = ml;
    mla->len++;

    return 0;
}

int
marker_list_array_insert_lists(struct marker_list_array *mla,
                               struct marker_list_array *src_mla,
                               track_map_t map) {
    int i, j = 0, k = 0, num_sources = 0, num_new_allocs = 0;
    struct marker_list_array dst_shadow, src_shadow;
    struct marker_list *newly_allocated[MAX_TRACKS], *sources[MAX_TRACKS];

    /* Count lists to be inserted. */
    
    for(i = 0; i < mla->len; i++) 
        if((1 << i) & map) 
            j++;
    
    if(mla->len + j >= MAX_TRACKS) {
        error_set(ERROR(mla),
                  "Cannot insert %d lists because that would exceed "
                  "maximum of %d lists", j, MAX_TRACKS);
        return 1;
    }

    /* Create backup to restore if something fails. */

    dst_shadow = *mla;
    if(src_mla)
        src_shadow = *src_mla;

    /* Build a table with the lists that need to be inserted. */

    j = 0;
    for(i = 0; i < MAX_TRACKS; i++) {
        if((1 << i) & map) {
            if(src_mla && j < src_mla->len) {

                /* Use list from source list array. */

                sources[num_sources++] = src_mla->lists[j];
                src_mla->lists[j] = NULL;
                j++;
                
            } else {
                
                /* Allocate new list. */

                sources[num_sources++] = marker_list_new();
                if(!sources[num_sources-1]) {
                    error_set(ERROR(mla),
                              "Could not create new empty list to insert");
                    for(i = 0; i < num_new_allocs; i++)
                        marker_list_destroy(newly_allocated[i]);
                    *mla = dst_shadow;
                    *src_mla = src_shadow;
                    return 1;
                }

                /* Keep track of which lists were allocated so we
                   can release them when something fails. */

                newly_allocated[num_new_allocs++] = sources[num_sources-1];
            }
        }
    }

    /*
     * Now insert the lists. Do it like this because at this point
     * nothing can fail anymore, so there's a guarantee that the
     * operation doesn't leave anything inconsistent when something
     * fails. 
     */

    j = 0;
    k = 0;
    for(i = 0; i < MAX_TRACKS; i++) {
        if((1 << i) & map) {
            for(j = MAX_TRACKS - 2; j >= i; j--)
                mla->lists[j+1] = mla->lists[j];
            mla->lists[i] = sources[k++];
        }
    }
    mla->len += num_sources;

    /* 
     * Remove the lists that were stolen from the src_mla.
     */

    if(src_mla) {
        
        /* Compact src_mla and count remaining tracks. */

        for(i = 0; i < MAX_TRACKS - 1; i++) {
            for(j = i; src_mla->lists[i] == NULL && j < MAX_TRACKS - 1; j++)
                for(k = i; k < MAX_TRACKS - 1; k++)
                    src_mla->lists[k] = src_mla->lists[k+1];
        }
        
        j = 0;
        for(i = 0; i < MAX_TRACKS; i++)
            if(src_mla->lists[i] != NULL)
                j++;

        src_mla->len = j;
    }
    
    return 0;
}

struct marker_list_array *
marker_list_array_delete_lists(struct marker_list_array *mla,
                               track_map_t map) {
    struct marker_list_array *del_mla = marker_list_array_new(0), shadow;
    int i, j, len_copy = mla->len;
    track_map_t map_copy = map;

    if(!del_mla) {
        error_set(ERROR(mla),
                  "Could not create new marker list array for deleted "
                  "lists");
        return NULL;
    }

    shadow = *mla;

    j = 0;
    for(i = 0; i < mla->len; i++) {
        if((1 << i) & map) {
            marker_list_array_append_list(del_mla, mla->lists[i+j++]);
            if(error_thrown(ERROR(del_mla))) {
                error_cascade(ERROR(mla),
                              ERROR(del_mla),
                              "Could not append marker list");

                /* 
                 * Remove lists that were already added otherwise 
                 * the marker_list_array_destroy() below destroys them. 
                 */

                for(i = 0; i < del_mla->len; i++)
                    del_mla->lists[i] = NULL;
                marker_list_array_destroy(del_mla);
                *mla = shadow;
                return NULL;
            }

            mla->len--;
            map >>= 1;
            i--;

        }
    }

    /*
     * Move lists up. Do this here to guarantee atomicity.
     */
            
    for(i = 0; i < len_copy; i++) {
        if((1 << i) & map_copy) {
            for(j = i; j < len_copy - 1; j++) 
                mla->lists[j] = mla->lists[j+1];
            len_copy--;
            map_copy >>= 1;
            i--;
        }
    }

    return del_mla;
}
                     

int
marker_list_array_move_list(struct marker_list_array *mla,
                            int from,
                            int to) {
    int i;
    struct marker_list *ml;

    if(from < 0 || from >= mla->len) {
        error_set(ERROR(mla), 
                  "From list %d out of range", from);
        return 1;
    }

    if(to < 0 || to >= mla->len) {
        error_set(ERROR(mla), 
                  "To list %d out of range", to);
        return 1;
    }    

    if(from == to)
        return 0;

    ml = mla->lists[from];
    if(from > to) {
        for(i = from; i > to; i--)
            mla->lists[i] = mla->lists[i - 1];
    } else {
        for(i = from; i < to; i++)
            mla->lists[i] = mla->lists[i + 1];
    }
    mla->lists[to] = ml;
    return 0;

}

void
marker_list_array_insert_time(struct marker_list_array *mla,
                              track_map_t map,
                              AFframecount offset,
                              AFframecount count,
                              enum marker_type type) {
    int i;
    for(i = 0; i < mla->len; i++) 
        if((1 << i) & map) 
            marker_list_insert_time(mla->lists[i], offset, count, type);
}

void
marker_list_array_delete_time(struct marker_list_array *mla,
                              track_map_t map,
                              AFframecount offset,
                              AFframecount count,
                              enum marker_type type) {
    int i;
    for(i = 0; i < mla->len; i++) 
        if((1 << i) & map) 
            marker_list_delete_time(mla->lists[i], offset, count, type);
}


void
marker_list_array_destroy(struct marker_list_array *mla) {
    int i;
    if(!mla)
        return;
    error_free(ERROR(mla));
    for(i = 0; i < mla->len; i++)
        if(mla->lists[i])
            marker_list_destroy(mla->lists[i]);
    mem_free(mla);
}

struct marker_list_array *
marker_list_array_new(int len) {
    int i;
    struct marker_list_array *mla = mem_alloc(sizeof(*mla));
    if(!mla)
        return NULL;
    for(i = 0; i < MAX_TRACKS; i++)
        mla->lists[i] = NULL;
    error_init((struct error *)mla);
    for(i = 0; i < len; i++) {
        if((mla->lists[i] = marker_list_new()) == NULL) {
            marker_list_array_destroy(mla);
            return NULL;
        }
    }
    mla->len = len;
    return mla;
}



struct marker_list_array *
marker_list_array_delete(struct marker_list_array *mla,
                         track_map_t map,
                         AFframecount offset,
                         AFframecount count,
                         enum marker_type type) {
    int i, j;
    struct marker_list_array *del_mla = marker_list_array_new(0);
    struct marker_list *ml;

    if(!del_mla) {
        error_set(ERROR(mla), 
                  "Could not create marker list array for deleted markers");
        return NULL;
    }
    
    for(i = 0, j = 0; i < mla->len; i++) {
        if((1 << i) & map) {
            ml = marker_list_delete(mla->lists[i], offset, count, type);
            if(marker_list_array_append_list(del_mla, ml)) {
                error_cascade(ERROR(mla),
                              ERROR(del_mla),
                              "Could not get deleted marker list for "
                              "track %d", i);
                marker_list_array_destroy(del_mla);
                return NULL;
            }
        }
    }
    
    return mla;
}

int
marker_list_array_insert(struct marker_list_array *mla,
                         struct marker_list_array *src_mla,
                         track_map_t map,
                         AFframecount offset,
                         AFframecount count,
                         enum marker_type type) {
    int i, j;
    
    for(i = 0, j = 0; i < mla->len && j < src_mla->len; i++)
        if((1 << i) & map)
            marker_list_insert_list(mla->lists[i],
                                    src_mla->lists[j++], offset, count, type);
    
    return 0;
}

/**********************************************************************/
/* Marker list functions.                                             */
/**********************************************************************/

void
marker_list_load(struct marker_list *ml,
                 const char *path,
                 int channel) {
    int i, argc;
    char **argv;
    void *iter;
    char tmp[512], *key, *value;
    snprintf(tmp, 512, "=%s=/Markers for track %d", path, channel);
    iter = gnome_config_init_iterator(tmp);        
    while(iter && gnome_config_iterator_next(iter, &key, &value)) {
        gnome_config_make_vector(value, &argc, &argv);
        if(argc != 4) {
            FAIL("unexpected value count %d (expected 4).\n", argc);
        } else {
            marker_list_insert(ml,
                               atoi(key),
                               atoi(argv[0]),
                               atof(argv[2]),
                               argv[3]);
        }            
        for(i = 0; i < argc; i++) 
            g_free(argv[i]);
        g_free(argv);
        g_free(key);
        g_free(value);
    }
}

void
marker_list_save(struct marker_list *ml,
                 const char *path,
                 int channel) {
    struct marker *m;
    GList *l;
    char *argv[4], args[4][100], key[512];
    argv[0] = args[0]; argv[1] = args[1]; argv[2] = args[2]; argv[3] = args[3];
    for(l = ml->markers; l; l = l->next) {
        m = (struct marker *)l->data;
        if(m->flags & MARKER_IS_DISABLED)
            continue;
        snprintf(key, 512, "=%s=/Markers for track %d/%ld", 
                 path, channel, m->frame_offset);
        snprintf(argv[0], 100, "%d", m->type);
        snprintf(argv[1], 100, "%d", m->flags);
        snprintf(argv[2], 100, "%f", m->multiplier);
        snprintf(argv[3], MIN(100, m->label ?
                              (strlen(m->label) + 1) : 2), "%s",
                 m->label ? 
                 m->label : " ");
        gnome_config_set_vector(key, 4, (const char * const *) argv);
    }
}

double
marker_list_slope_value(struct marker_list *ml,
                        AFframecount frame_offset,
                        enum marker_type type) {
    struct marker *mn = NULL, *mp = NULL;
    double a, r, b, max, min;
    int gradient_is_negative;
    AFframecount x;

    if((ml->marker_types_enabled & type) == 0) 
        return 0;

    mn = marker_list_next(ml, frame_offset, type);
    mp = marker_list_previous(ml, frame_offset, type);

    if(!mn || !mp)
        return 0;

    g_return_val_if_fail(mn->frame_offset - mp->frame_offset > 0, 0);

    if(mp->multiplier == mn->multiplier) 
        return mp->multiplier;

    gradient_is_negative = mp->multiplier > mn->multiplier ? 1 : 0;

    max = gradient_is_negative ? mp->multiplier : mn->multiplier;
    min = gradient_is_negative ? mn->multiplier : mp->multiplier;

    a = (max - min) / (mn->frame_offset - mp->frame_offset);

    if(gradient_is_negative)
        a = -a;

    x = frame_offset - mp->frame_offset;
    b = mp->multiplier;
    
    r = (a * x) + b;

    return r;
}

void
marker_list_invert(struct marker_list *ml,
                   AFframecount frame_offset,
                   AFframecount frame_count,
                   enum marker_type type) {
    
    GList *l;
    struct marker *m;

    for(l = ml->markers; l; l = l->next) {
        
        m = (struct marker *)l->data;

        if(m->frame_offset < frame_offset)
            continue;
        
        if(m->frame_offset > frame_offset + frame_count)
            break;
        
        if(m->type & type)
            m->multiplier *= -1;

    }
}

void
marker_list_delete_marker(struct marker_list *ml,
                          struct marker *m) {
    GList *l;

    for(l = ml->markers; l; l = l->next) {
        
        if((struct marker *)l->data != m) 
            continue;
        
        ml->markers = g_list_remove_link(ml->markers, l);
        marker_destroy(m);
        g_list_free(l);

        return;

    }
}

void
marker_list_set_marker_position(struct marker_list *ml,
                                struct marker *m,
                                AFframecount frame_offset) {
    struct marker *mn = NULL, *mp = NULL;

    mn = marker_list_next(ml,
                          m->frame_offset + 1,
                          m->type);

    mp = marker_list_previous(ml,
                              m->frame_offset,
                              m->type);

    if(mn && frame_offset >= mn->frame_offset)
        frame_offset = mn->frame_offset - 1;

    if(mp && frame_offset <= mp->frame_offset)
        frame_offset = mp->frame_offset + 1;

    m->frame_offset = frame_offset;
}

void
marker_list_dump(struct marker_list *ml) {
    GList *l;
    DEBUG("marker_list: marker_types_enabled: %d, markers: %p\n",
          ml->marker_types_enabled, ml->markers);
    for(l = ml->markers; l; l = l->next) 
        marker_dump((struct marker *)(l->data));
}

/* FIXME: slow */

struct marker *
marker_list_next(struct marker_list *ml,
                 AFframecount frame_offset,
                 enum marker_type mask) {
    GList *l;
    struct marker *m;

    for(l = ml->markers; l; l = l->next) {

        m = (struct marker *)l->data; 

        if(m->frame_offset >= frame_offset && 
           (m->type & mask) &&
           !(m->flags & MARKER_IS_DISABLED)) 
            return m;

    }
    return NULL;
}

/* FIXME: slow */

struct marker *
marker_list_previous(struct marker_list *ml,
                     AFframecount frame_offset,
                     enum marker_type mask) {
    GList *l;
    struct marker *m, *prev = NULL;

    for(l = ml->markers; l; l = l->next) {

        m = (struct marker *)l->data; 

        if(m->frame_offset < frame_offset &&
           (m->type & mask) &&
           (m->flags & MARKER_IS_DISABLED) == 0)
            prev = m;

        if(m->frame_offset >= frame_offset && 
           (m->type & mask)) 
            break;

    }

    return prev;
}


struct marker_list *
marker_list_delete(struct marker_list *ml,
                   AFframecount frame_offset,
                   AFframecount frame_count,
                   enum marker_type type) {
    GList *l, *l2;
    struct marker *m;
    struct marker_list *ml_deleted = 
        marker_list_clone(ml, frame_offset, frame_count, type);

    if(!ml_deleted)
        return NULL;

    for(l = ml->markers; l; ) {

        m = (struct marker *)l->data;

        if(m->frame_offset > frame_offset + frame_count)
            break;

        if(m->frame_offset >= frame_offset &&
           m->frame_offset <= frame_offset + frame_count &&
           (m->type & type)) {

            DEBUG("destroying marker:\n");
            marker_dump(m);
            marker_destroy(m);
            l2 = l->next;
            ml->markers = g_list_remove_link(ml->markers, l);
            g_list_free(l);
            l = l2;
            continue;

        }

        l = l->next;

    }
    return ml_deleted;
}

struct marker_list *
marker_list_delete_time(struct marker_list *ml,
                        AFframecount frame_offset,
                        AFframecount frame_count,
                        enum marker_type type) {
    GList *l;
    struct marker *m;
    struct marker_list *ml_deleted = 
        marker_list_delete(ml, frame_offset, frame_count, type);
    
    if(!ml_deleted)
        return NULL;

    for(l = ml->markers; l; l = l->next) {

        m = (struct marker *)(l->data);

        if(m->frame_offset < frame_offset)
            continue;

        if(m->type & type)
            m->frame_offset = MAX(0, m->frame_offset - frame_count);

    }

    return ml_deleted;
}

void
marker_list_insert_time(struct marker_list *ml,
                        AFframecount frame_offset,
                        AFframecount frame_count,
                        enum marker_type type) {
    GList *l;
    struct marker *m;

    for(l = ml->markers; l; l = l->next) {
        m = (struct marker *)l->data;
        if(m->frame_offset < frame_offset)
            continue;
        if(m->type & type)
            m->frame_offset = m->frame_offset + frame_count;
    }
}

gint
marker_list_do_insert(gconstpointer a,
                      gconstpointer b) {
    struct marker *m1, *m2;

    m1 = (struct marker *)a;
    m2 = (struct marker *)b;

    if(m1->frame_offset > m2->frame_offset)
        return 1;

    return 0;
}

/*
void
marker_list_insert_list2(struct marker_list *ml,
                         struct marker_list *ml_source,
                         AFframecount frame_offset) {
    struct marker *m;
    GList *l;
    for(l = ml_source->markers; l; l = l->next) {
        m = (struct marker *)l->data;
        DEBUG("inserting marker:\n");
        marker_dump(m);
        marker_list_insert(ml,
                           frame_offset + m->frame_offset,
                           m->type,
                           m->multiplier,
                           m->label);
    }
}
*/

void
marker_list_insert_list(struct marker_list *ml,
                        struct marker_list *ml_source,
                        AFframecount frame_offset,
                        AFframecount frame_count,
                        enum marker_type type) {
    struct marker *m;
    GList *l;

    for(l = ml_source->markers; l; l = l->next) {

        m = (struct marker *)l->data;

        if(frame_offset + m->frame_offset > frame_offset + frame_count)
            break;

        if(m->type & type) 
            marker_list_insert(ml,
                               frame_offset + m->frame_offset,
                               m->type,
                               m->multiplier,
                               m->label);

    }
}

/* 
 * @return the marker that was inserted or NULL. 
 */

struct marker *
marker_list_insert(struct marker_list *ml,
                   AFframecount frame_offset,
                   enum marker_type type,
                   float multiplier,
                   const char *label) {
    struct marker *m, *m2;

    m2 = marker_list_next(ml, frame_offset, type);

    g_return_val_if_fail(!m2 || m2->frame_offset != frame_offset, NULL);

    m = marker_new(frame_offset, type, multiplier, label);

    if(!m) 
        return NULL;

    ml->markers = g_list_insert_sorted(ml->markers, 
                                       m, 
                                       marker_list_do_insert);
    return m;
}

struct marker_list *
marker_list_new() {
    struct marker_list *ml = mem_calloc(1, sizeof(struct marker_list));
    if(!ml)
        return NULL;
    ml->marker_types_enabled = MARKER_SLOPE | MARKER_SLOPE_AUX;
    ml->markers = NULL;
    return ml;
}

struct marker_list *
marker_list_clone(struct marker_list *ml,
                  AFframecount frame_offset,
                  AFframecount frame_count,
                  enum marker_type type) {
    int fail = 0;
    GList *l, *l_clone = NULL;
    struct marker_list *ml_clone;
    struct marker *m_clone;
    struct marker *m;

    ml_clone = marker_list_new();

    if(!ml_clone)
        return NULL;

    for(l = ml->markers; l; l = l->next) {

        m = (struct marker *)l->data;

        if(!(type & m->type))
            continue;
        if(frame_offset > m->frame_offset)
            continue;
        if(frame_offset + frame_count < m->frame_offset)
            break;

        m_clone = marker_clone(m);
        if(!m_clone) {
            fail = 1;
            break;
        }

        m_clone->frame_offset -= frame_offset;
        l_clone = g_list_append(l_clone, m_clone);
    }
    ml_clone->markers = l_clone;

    if(fail) {
        marker_list_destroy(ml_clone);
        return NULL;
    }

    return ml_clone;
}

void
marker_list_do_destroy(struct marker *m,
                       gpointer user_data) {
    marker_destroy(m);
}

void
marker_list_destroy(struct marker_list *ml) {
    
    g_return_if_fail(ml != NULL);

    if(g_list_length(ml->markers))
        DEBUG("destroying %d markers\n", g_list_length(ml->markers));
    //    marker_list_dump(ml);
    g_list_foreach(ml->markers, (GFunc)marker_list_do_destroy, NULL);
    g_list_free(ml->markers);
    free(ml);
}

/**********************************************************************/
/* Marker functions.                                                  */
/**********************************************************************/

void
marker_set_label(struct marker *m,
                 const char *label) {
    char *s;

    g_return_if_fail(m != NULL);
    g_return_if_fail(label != NULL);

    s = strdup(label);

    g_return_if_fail(s != NULL);

    if(m->label)
        free(m->label);
    
    m->label = s;
}

void
marker_dump(struct marker *m) {
    DEBUG("marker: frame_offset: %ld, type: %d, multiplier: %f, label: %s\n",
          m->frame_offset, m->type, m->multiplier, m->label);
}

struct marker *
marker_new(AFframecount frame_offset,
           enum marker_type type, 
           float multiplier,
           const char *label) { 
    struct marker *m = mem_alloc(sizeof(struct marker));

    g_return_val_if_fail(m != NULL, NULL);

    m->frame_offset = frame_offset;
    m->type = type;
    m->label = label ? strdup(label) : NULL;
    m->multiplier = multiplier;
    m->flags = 0;
    return m;
}

struct marker *
marker_clone(struct marker *m) {
    struct marker *m_clone;

    g_return_val_if_fail(m != NULL, NULL);

    m_clone = marker_new(m->frame_offset,
                         m->type,
                         m->multiplier,
                         m->label);
    m_clone->flags = m->flags;
    return m_clone;
}

void
marker_destroy(struct marker *m) {

    g_return_if_fail(m != NULL);

    if(m->label)
        free(m->label);
    free(m);
}

