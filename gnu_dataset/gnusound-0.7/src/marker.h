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

#ifndef MARKER_H
#define MARKER_H

#include <config.h>
#include <audiofile.h>
#include <glib.h>
#include "error.h"

#define MARKER_IS_DISABLED (1 << 0)

enum marker_type {
    MARKER_SLOPE     = (1 << 0),
    MARKER_SLOPE_AUX = (1 << 1),
    MARKER_TEXT      = (1 << 2)
};

struct marker {
    enum marker_type type;
    AFframecount frame_offset;
    int flags;
    float multiplier;
    char *label;
};

struct marker_list {
    int marker_types_enabled;
    AFframecount left_cache[3];
    AFframecount right_cache[3];
    AFframecount last_request[3];
    double last_return[3];
    double slope[3];
    GList *markers;
};

struct marker_list_array {
    struct error error;
    int len;
    struct marker_list *lists[MAX_TRACKS];
};

/* Marker functions. */

void
marker_dump(struct marker *m);

void
marker_set_label(struct marker *m,
                 const char *label);

struct marker *
marker_clone(struct marker *m);

void
marker_destroy(struct marker *m);

struct marker *
marker_new(AFframecount frame_offset,
           enum marker_type type, 
           float multiplier,
           const char *label);

/* Marker list functions. */

void
marker_list_save(struct marker_list *ml,
                 const char *path,
                 int channel);

void
marker_list_load(struct marker_list *ml,
                 const char *path,
                 int channel);

void
marker_list_dump(struct marker_list *ml);


double
marker_list_slope_value(struct marker_list *ml,
                        AFframecount frame_offset,
                        enum marker_type type);

void
marker_list_delete_marker(struct marker_list *ml,
                          struct marker *m);

void
marker_list_set_marker_position(struct marker_list *ml,
                                struct marker *m,
                                AFframecount frame_offset);


void
marker_list_insert_list(struct marker_list *ml,
                        struct marker_list *ml_source,
                        AFframecount frame_offset,
                        AFframecount frame_count,
                        enum marker_type type);

struct marker_list *
marker_list_delete_time(struct marker_list *ml,
                        AFframecount frame_offset,
                        AFframecount frame_count,
                        enum marker_type type);

void
marker_list_insert_time(struct marker_list *ml,
                        AFframecount frame_offset,
                        AFframecount frame_count,
                        enum marker_type type);

void
marker_list_invert(struct marker_list *ml,
                   AFframecount frame_offset,
                   AFframecount frame_count,
                   enum marker_type type);

struct marker *
marker_list_previous(struct marker_list *ml,
                     AFframecount frame_offset,
                     enum marker_type type);

struct marker *
marker_list_next(struct marker_list *ml,
                 AFframecount frame_offset,
                 enum marker_type type);

struct marker_list *
marker_list_delete(struct marker_list *ml,
                   AFframecount frame_offset,
                   AFframecount frame_count,
                   enum marker_type type);

struct marker *
marker_list_insert(struct marker_list *ml,
                   AFframecount frame_offset,
                   enum marker_type type,
                   float multiplier,
                   const char *label);


struct marker_list *
marker_list_clone(struct marker_list *ml,
                  AFframecount frame_offset_start,
                  AFframecount frame_offset_end,
                  enum marker_type type);

void
marker_list_destroy(struct marker_list *ml);

struct marker_list *
marker_list_new();

/*
void
marker_list_insert_list2(struct marker_list *ml,
                         struct marker_list *ml_source,
                         AFframecount frame_offset);
*/

/* Marker list array functions. */

void
marker_list_array_replace(struct marker_list_array *dst,
                          struct marker_list_array *src,
                          track_map_t map,
                          AFframecount offset,
                          AFframecount count,
                          enum marker_type type);

int
marker_list_array_move_list(struct marker_list_array *mla,
                            int from,
                            int to);

struct marker *
marker_list_array_find_next(struct marker_list_array *mla,
                            track_map_t map,
                            AFframecount frame_offset,
                            enum marker_type type);

struct marker *
marker_list_array_find_previous(struct marker_list_array *mla,
                                track_map_t map,
                                AFframecount frame_offset,
                                enum marker_type type);

struct marker_list_array *
marker_list_array_delete_lists(struct marker_list_array *mla,
                               track_map_t map);

int
marker_list_array_insert_lists(struct marker_list_array *mla,
                               struct marker_list_array *src,
                               track_map_t map);

struct marker_list_array *
marker_list_array_delete(struct marker_list_array *mla,
                         track_map_t map,
                         AFframecount offset,
                         AFframecount count,
                         enum marker_type type);

int
marker_list_array_insert(struct marker_list_array *mla,
                         struct marker_list_array *src_mla,
                         track_map_t map,
                         AFframecount offset,
                         AFframecount count,
                         enum marker_type type);

void
marker_list_array_delete_time(struct marker_list_array *mla,
                              track_map_t map,
                              AFframecount offset,
                              AFframecount count,
                              enum marker_type type);

void
marker_list_array_insert_time(struct marker_list_array *mla,
                              track_map_t map,
                              AFframecount offset,
                              AFframecount count,
                              enum marker_type type);

int
marker_list_array_insert_marker(struct marker_list_array *mla,
                                track_map_t map,
                                AFframecount offset,
                                enum marker_type type,
                                float multiplier,
                                const char *label);

void
marker_list_array_destroy(struct marker_list_array *mla);


struct marker_list_array *
marker_list_array_new(int len);

/* New API */

/* Primary challenge: the old marker API cannot be used to implement
   undo, because after inserting markers, the only way to delete them
   again is by deleting a range given by an offset and a count. But
   since the insert operation "merges" a marker list with another
   marker list, this means the delete operation can delete markers
   which weren't inserted by the insert operation. */

/* Marker functions. */

#if 0
void
marker_set_label(struct marker *m,
                 const char *label);

void
marker_set_position(struct marker *m,
                    AFframecount offset);

void
marker_set_value(struct marker *m,
                 float value);

struct marker *
marker_clone(struct marker *m);

void
marker_destroy(struct marker *m);

struct marker *
marker_new(AFframecount offset,
           enum marker_type type, 
           float multiplier,
           const char *label);

/* Marker list functions. */

/* Return 0 for false, > 0 for true, < 0 to stop iterating. */

#define STOP_ITERATING -1

typedef int (*markerlist_callback)(const struct marker *m, 
                                   void *user_data);

void
ml_foreach(struct ml *ml,
                   ml_callback callback,
                   void *user_data);

struct ml *
ml_select(struct ml *ml,
                  ml_callback select,
                  void *user_data);

double
ml_get_value(struct ml *ml,
                     AFframecount offset,
                     enum marker_type type);

int
ml_insert_marker(struct ml *ml,
                         const struct marker *m);

int
ml_delete_marker(struct ml *ml,
                         const struct marker *m);

int
ml_insert(struct ml *target,
                  struct ml *source,
                  AFframecount offset);

struct ml *
ml_delete(struct ml *ml,
                  struct ml *delete_spec);

int
ml_move(struct ml *ml,
                AFframecount delta,
                ml_callback select,
                void *user_data);

struct marker *
ml_previous(struct ml *ml,
                    AFframecount offset,
                    enum marker_type type);

struct marker *
ml_next(struct ml *ml,
                AFframecount offset,
                enum marker_type type);

struct ml *
ml_copy(struct ml *ml,
                AFframecount offset,
                AFframecount count,
                enum marker_type type);

void
ml_destroy(struct ml *ml);

struct ml *
ml_new();


/* Marker list array functions. */

void
marker_list_array_replace(struct marker_list_array *dst,
                          struct marker_list_array *src,
                          track_map_t map,
                          AFframecount offset,
                          AFframecount count,
                          enum marker_type type);

int
mla_move_list(struct marker_list_array *mla,
                      int from,
                      int to);

struct marker *
mla_find_next(struct marker_list_array *mla,
                      track_map_t map,
                      AFframecount offset,
                      enum marker_type type);

struct marker *
mla_find_previous(struct marker_list_array *mls,
                          track_map_t map,
                          AFframecount offset,
                          enum marker_type type);

struct mla *
mla_delete_lists(struct marker_list_array *mls,
                         track_map_t map);

int
mla_insert_lists(struct marker_list_array *target,
                         struct marker_list_array *source,
                         track_map_t map);

struct mla *
mla_delete(struct marker_list_array *mla,
                   track_map_t map,
                   ml_callback select,
                   void *user_data);

int
mla_insert(struct marker_list_array *target,
                   track_map_t target_map,
                   struct marker_list_array *source,
                   track_map_t source_map,
                   AFframecount offset);

void
mla_foreach(struct mla *mls,
            track_map_t map,
            ml_callback select,
            void *user_data);

void
mla_destroy(struct mla *mls);

struct mla *
mla_new(int len);
#endif

#endif /* ! MARKER_H */

