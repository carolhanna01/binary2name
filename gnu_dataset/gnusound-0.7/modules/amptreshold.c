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

#define AMPTRESHOLD_GLADE_FILE "amptreshold" GUI_GTK_VERSION_TAG ".glade"

struct amptreshold_params {
    AFframecount duration;
    float treshold;
    int invert;
    int ganging;
    int delete;
};

struct extraction {
    AFframecount begin;
    AFframecount end;
};

static int self_id = 0;

void
extraction_list_dump(GList *l) {
    GList *l2;
    for(l2 = l; l2; l2 = l2->next) {
        DEBUG("start: %ld, end: %ld\n", 
              ((struct extraction *)l->data)->begin, 
              ((struct extraction *)l->data)->end); 
    }
}

AFframecount
extraction_list_apply(shell *shl,
                      int track,
                      GList *extraction,
                      int delete) {
    GList *l, *l_del;
    struct extraction *e;
    AFframecount deleted = 0;
    rwlock_wlock(&shl->clip->sr->tracks[track]->rwl);
    for(l = extraction; l; l = l->next) {
        e = (struct extraction *)l->data;
        /*
        DEBUG("erasing: %ld, end: %ld\n", 
              ((struct extraction *)l->data)->begin, 
              ((struct extraction *)l->data)->end); 
        */
        DEBUG("deleting %ld frames from %ld on track %d\n",
              e->end - e->begin, e->begin - deleted, track);
        if(track_delete(shl->clip->sr->tracks[track],
                        &l_del,
                        e->begin - deleted,
                        e->end - e->begin)) {
            FAIL("***** TROUBLE ****\n");
            break;
        }
        blocklist_blocks_destroy(l_del);
        if(!delete)
            track_insert_silence(shl->clip->sr->tracks[track],
                              e->begin,
                              e->end - e->begin);
        else
            deleted += (e->end - e->begin);
    }
    rwlock_wunlock(&shl->clip->sr->tracks[track]->rwl);
    return deleted;
    
}

GList *
extraction_list_intersect(GList *el1, GList *el2) {
    GList *l1 = el1, *l2 = el2, *li = NULL;
    struct extraction *eli, *e1, *e2;

    while(l1 && l2) {
        e1 = (struct extraction *)l1->data;
        e2 = (struct extraction *)l2->data;
        
        if(e1->begin > e2->end) {
            l2 = l2->next;
            continue;
        } else if(e2->begin > e1->end) {
            l1 = l1->next;
            continue;
        }
        
        eli = mem_alloc(sizeof(struct extraction));
        if(!eli) {
            FAIL("could not allocate memory for extraction element!\n");
            return li;
        }
        eli->begin = MAX(e1->begin, e2->begin);
        eli->end = MIN(e1->end, e2->end);
        li = g_list_append(li, eli);

        if(e1->end <= e2->end)
            l1 = l1->next;
        else if(e2->end < e1->end) 
            l2 = l2->next;
    }
    return li;
}

void
extraction_list_destroy(GList *extraction) {
    GList *l;
    for(l = extraction; l; l = l->next) {
        mem_free(l->data);
        l->data = NULL;
    }
    g_list_free(l);
}

GList *
extraction_list_invert(GList *l,
                       AFframecount start_offset,
                       AFframecount end_offset) {
    GList *l2, *li = NULL;
    struct extraction store, *el, *eli;
    store.begin = start_offset;
    store.end = end_offset;
    for(l2 = l ; l2; l2 = l2->next) {
        el = (struct extraction *)l2->data;
        if(store.begin == start_offset &&
           el->begin == start_offset) {
            store.begin = el->begin;
            store.end = el->end;
            continue;
        }
        
        if(store.begin == start_offset &&
           el->begin != start_offset) 
            store.end = start_offset;
        
        eli = mem_alloc(sizeof(struct extraction));
        if(!eli) {
            FAIL("could not allocate memory for extraction element!\n");
            break;
        }
        eli->begin = store.end;
        eli->end = el->begin;
        li = g_list_append(li, eli);
        store.begin = el->begin;
        store.end = el->end;
    }
    if(store.end != end_offset || 
       (store.begin == start_offset && store.end == end_offset)) {
        if(store.end != end_offset) 
            store.begin = store.end;            
        eli = mem_alloc(sizeof(struct extraction));
        if(!eli) {
            FAIL("could not allocate memory for extraction element!\n");
        } else {
            eli->begin = store.begin;
            eli->end = end_offset;
            li = g_list_append(li, eli);
        }
    }            
    return li;
}

GList *
extraction_list_new(shell *shl,
                    int track,
                    AFframecount start_offset,
                    AFframecount end_offset,
                    float treshold,
                    AFframecount duration) {
    GList *l = NULL;
    int i, begin_found = 0;
    struct extraction store, *el;
    float begin_val = 0;
    AFframecount abs_offset;
    ITERATORF_INIT(start_offset, end_offset - start_offset);
    ITERATORF(shl, shl->clip->sr->tracks[track],
              abs_offset = iter_frame_offset;
              for(i = 0; i < iter_read; i++, abs_offset++) {

                  if(!begin_found && fabs(float_frame_bits[i]) <= treshold) {

                      begin_val = fabs(float_frame_bits[i]);
                      begin_found = 1;
                      store.begin = abs_offset;

                  } else if(begin_found && 
                            fabs(float_frame_bits[i]) > treshold) {
                      
                      begin_found = 0;
                      store.end = abs_offset;

                      if(store.end - store.begin > duration) {

                          DEBUG("range %ld-%ld\n",
                                store.begin, store.end);
                          el = mem_alloc(sizeof(struct extraction));

                          if(!el) {
                              FAIL("could not allocate memory for extraction element!\n");
                              ITERATOR_ESCAPE();
                              break;
                          }

                          *el = store;
                          l = g_list_append(l, el);
                      }
                  }
              });

    if(begin_found) {

        store.end = end_offset;

        if(store.end - store.begin > duration) {
            el = mem_alloc(sizeof(struct extraction));

            if(!el) 
                FAIL("could not allocate memory for extraction element!\n");
            else {
                *el = store;
                l = g_list_append(l, el);
            }
        }

    }
    ITERATORF_EXIT();
    return l;
    
}
                
static void 
amptreshold_dialog_apply(struct dialog *dialog,
                         gpointer user_data) {
    struct amptreshold_params *p = mem_alloc(sizeof(*p));

    g_return_if_fail(p != NULL);
    
    p->duration = gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "duration"))) * dialog->shl->grid.gap;
    p->treshold = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(dialog->pane, "treshold")))->value;
    p->ganging = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "ganging"))) ? 1 : 0;
    p->invert = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "invert"))) ? 1 : 0;
    p->delete = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane_get_widget(dialog->pane, "delete"))) ? 1 : 0;
    
    shell_dispatch_as(dialog->shl, 
                      CMD_NEW("execute-module", 
                              cmd_new_shellp_val(dialog->shl),
                              cmd_new_int_val(self_id),
                              cmd_new_voidp_val_with_dtor(p, cmd_voidp_dtor)),
                      module_get(self_id)->name);
}

static void
amptreshold_dialog_close(struct dialog *dialog,
                         void *user_data) {
    struct module_state *module_state = 
        shell_get_module_state(dialog->shl, self_id);

    module_state->is_open = 0;
    dialog_destroy(dialog);
}

static struct cmd_value *
amptreshold_open(int id,
                 shell *shl) {
    char path[4096];
    struct module_state *module_state;
    struct dialog *dialog;

    snprintf(path, 4096, "%s/%s", module_get_path(id), AMPTRESHOLD_GLADE_FILE);

    dialog = dialog_new(sizeof(*dialog),
                        path,
                        "dialog",
                        shl,
                        0,
                        NULL,
                        NULL);
    
    if(!dialog) 
        return cmd_new_error_val("Could not load interface %s.", path);

    ((struct dialog *)dialog)->apply = amptreshold_dialog_apply;
    ((struct dialog *)dialog)->close = amptreshold_dialog_close;

    module_state = shell_get_module_state(shl, id);
    module_state->is_open = 1;
    module_state->data = dialog;

    return cmd_new_void_val();
}

static void
amptreshold_close(int id,
                  shell *shl) {
    struct module_state *module_state = shell_get_module_state(shl, id);
    dialog_destroy((struct dialog *)module_state->data);
}

static struct cmd_value *
amptreshold_execute(int id,
                    shell *shl,
                    void *data) {
    AFframecount start = shl->select_start,
        end = shl->select_end;
    int map = shl->select_channel_map;
    int t, stracks = 0, i = 0, invert, ganging, delete;
    AFframecount track_newlen[MAX_TRACKS], duration;
    GList *l = NULL, *extractions[MAX_TRACKS], *intersection = NULL;
    float treshold;
    struct cmd_value *r;
    struct cmd *cmd;
    struct amptreshold_params *p = data;
    
    if(!p)
        return cmd_new_error_val("Amplitude Treshold does not have defaults.");

    if(shl->select_end - shl->select_start < p->duration) 
        return cmd_new_error_val("The selection length is less than "
                                 "the minimum required duration.");
    
    invert = p->invert;
    delete = p->delete;
    treshold = p->treshold;
    duration = p->duration;
    ganging = p->ganging;

    /* Preserve the selection. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(start),
                  cmd_new_long_val(end - start));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) 
        return r;
    cmd_destroy_value(r);
    
    rwlock_rlock(&shl->clip->sr->rwl);
    
    /* Calculate extraction lists for every selected track. */

    for(t = 0; t < snd_track_count(shl->clip->sr); t++) 
        if((1 << t) & map) 
            extractions[stracks++] = extraction_list_new(shl,
                                                         t,
                                                         start,
                                                         end,
                                                         treshold,
                                                         duration);
    
    /* If ganging, find the smallest intersection of all extraction
       lists. */

    if(ganging) {
        intersection = extractions[0];
        for(i = 0; i < stracks - 1; i++) {
            intersection = extraction_list_intersect(extractions[i], 
                                                     extractions[i+1]);
            extraction_list_destroy(extractions[i]);
            extraction_list_destroy(extractions[i+1]);
            extractions[i] = intersection;
            extractions[i+1] = intersection;
        }
        for(i = 0; i < stracks - 2; i++) {
            extraction_list_destroy(extractions[i]);
            extractions[i] = intersection;
        }
    }

    /* If inverting, invert all extraction lists. */

    if(invert) {
        for(i = 0; i < stracks; i++) {
            if(!ganging || (ganging && i == 0)) {
                l = extraction_list_invert(extractions[i], start, end);
                extraction_list_destroy(extractions[i]);
            }
            extractions[i] = l;
        }
    }

    /* Finally apply the extraction lists. */

    for(t = 0, i = 0; t < snd_track_count(shl->clip->sr); t++) {
        if((1 << t) & map) {
            track_newlen[t] = (end - start) -
                extraction_list_apply(shl, t, extractions[i], delete);
            if(!ganging)
                extraction_list_destroy(extractions[i]);
            i++;
        }
    }

    if(ganging)
        extraction_list_destroy(intersection);

    rwlock_runlock(&shl->clip->sr->rwl);
    return cmd_new_void_val();
}

static int
amptreshold_init(int id) {
    self_id = id;
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Amplitude Treshold",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002,2003,2004,2005",
    "GPL",
    NULL,
    0,

    amptreshold_init,
    NULL,
    amptreshold_open,
    amptreshold_execute,
    amptreshold_close,
    NULL,
    NULL
};
