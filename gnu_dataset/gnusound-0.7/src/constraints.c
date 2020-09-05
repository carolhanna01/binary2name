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
 * A copy of the GNU General Public License can be found in the file
 * LICENSE in the top directory of the source distribution. If not,
 * write to the Free Software * Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 */

#include <config.h>
#include <assert.h>
#include "mem.h"
#include "constraints.h"
#include "region.h"

struct constraints *
constraints_new() {
    struct constraints *cs = mem_alloc(sizeof(*cs));

    g_return_val_if_fail(cs != NULL, NULL);

    cs->regions = NULL;
    cs->reasons = NULL;
    cs->props = NULL;
    return cs;
}

void
constraints_destroy_all_reasons(void *data,
                                void *user_data) {
    mem_free(data);
}

void
constraints_destroy_all_regions(void *data,
                                void *user_data) {
    region_destroy(data);
}

void 
constraints_destroy(struct constraints *cs) {
    g_list_foreach(cs->reasons, constraints_destroy_all_reasons, NULL);
    g_list_foreach(cs->regions, constraints_destroy_all_regions, NULL);
    g_list_free(cs->reasons);
    g_list_free(cs->regions);
    mem_free(cs);
}

/**
 * Constrains properties on a region.
 * @param cs The constraints to augment.
 * @param reason Why the region is being constrained or NULL.
 * @param region The region to constrain.
 * @param props Which properties of the region to constrain.
 * @return 0 on success, non-zero otherwise.
 */

int 
constraints_push(struct constraints *cs, 
                 const char *reason, 
                 struct region *region, 
                 enum constraints_props props) {
    char *s;

    assert(props);

    if(!reason) 
        reason = "Region is locked";
    
    s = mem_calloc(1, strlen(reason) + 1);

    g_return_val_if_fail(s != NULL, 1);

    strcpy(s, reason);
    cs->reasons = g_list_prepend(cs->reasons, s);
    cs->props = g_list_prepend(cs->props, GINT_TO_POINTER(props));
    cs->regions = g_list_prepend(cs->regions, region);

    return 0;
}

/**
 * Removes previously pushed constraints.
 * @param cs The constraints to diminish.
 * @return 0 on success, 1 if there were no constraints to pop.
 */

int
constraints_pop(struct constraints *cs) {
    GList *link;

    if(cs->reasons == NULL)
        return 1;

    link = cs->reasons;
    cs->reasons = g_list_remove_link(cs->reasons, cs->reasons);
    mem_free(link->data);
    g_list_free(link);
    
    link = cs->props;
    cs->props = g_list_remove_link(cs->props, cs->props);
    g_list_free(link);

    link = cs->regions;
    cs->regions = g_list_remove_link(cs->regions, cs->regions);
    region_destroy((struct region *)link->data);
    g_list_free(link);

    return 0;
}

const char *
constraints_test_one(struct constraints *cs,
                     struct region *rgn,
                     enum constraints_oper oper,
                     int pos) {
    char *reason = g_list_nth(cs->reasons, pos)->data;
    struct region *region = g_list_nth(cs->regions, pos)->data;
    enum constraints_props props = 
        GPOINTER_TO_INT(g_list_nth(cs->props, pos)->data);
    int64_t k, l, m, q, r, s;

    /* We need to check if the given region has any tracks in common
       with regions for which constraints are active; if no tracks
       match, then the region as a whole can never match. */

    /* Look at the track maps first; check wildcards. */
    
    if(rgn->map == REGION_MATCH_NOTHING || region->map == REGION_MATCH_NOTHING)
        return NULL;
    
    /* If regions are on different tracks they're completely disjoint
       and can never match */
    
    if((rgn->map != REGION_MATCH_ANYTHING && 
        region->map != REGION_MATCH_ANYTHING) &&
       (rgn->map & region->map) == 0)
        return NULL;

    /* At this point there is a least one matching track. Further
       matching depends on the operation. */

    k = region->offset;
    l = region->offset + region->count;
    m = region->count;
    q = rgn->offset;
    r = rgn->offset + rgn->count;
    s = rgn->count;

    /* Shortcut treatment for wildcards. */

    if(q == REGION_MATCH_NOTHING || s == REGION_MATCH_NOTHING ||
       k == REGION_MATCH_NOTHING || m == REGION_MATCH_NOTHING)
        return NULL;
    
    if(q == REGION_MATCH_ANYTHING || k == REGION_MATCH_ANYTHING ||
       s == REGION_MATCH_ANYTHING || m == REGION_MATCH_ANYTHING)
        return reason;

    switch(oper) {
        
    case CONSTRAINTS_OPER_DELETE:
    case CONSTRAINTS_OPER_INSERT:

        /*
         * The same constraints apply under both INSERT and DELETE
         * operations:
         *
         * Given a constrained region (k, l) with length m and a test
         * region (q, r) with length s:
         *
         * POSITION constraint is violated when:
         * - q < k && s > 0
         * LENGTH constraint is violated when:
         * - ! ((q < k && r < k) || (q > l && r > 0))
         * CONTENT constraint is violated when:
         * - whenever LENGTH constraint is violated.
         *
         */

        if(props & CONSTRAIN_POSITION)
            if(q < k && s > 0)
                return reason;
        
        if(props & (CONSTRAIN_LENGTH | CONSTRAIN_CONTENTS))
            if(!((q < k && r < k) || (q > l && r > 0)))
                return reason;

        break;

    case CONSTRAINTS_OPER_REPLACE:
        if(props & (CONSTRAIN_CONTENTS))
            if(!((q < k && r < k) || (q > l && r > 0)))
                return reason;

        break;
    default:
        assert("unknown operation");
    }

    return NULL;
}

/**
 * Tests whether the given region is constrained under the given
 * operation. 
 * @param cs The constraints.
 * @param r The region.
 * @param oper The operation.
 * @return NULL if the region is not constrained, reason why
 * it is constrained otherwise.
 */

const char *
constraints_test(struct constraints *cs,
                 struct region *rgn,
                 enum constraints_oper oper) {
    int i;
    const char *reason = NULL;
    for(i = 0; i < g_list_length(cs->reasons); i++) 
        if((reason = constraints_test_one(cs, rgn, oper, i)))
            goto exit;
    reason = NULL;

 exit:
    region_destroy(rgn);
    return reason;
}

