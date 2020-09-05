#ifndef REGION_H
#define REGION_H

#include <config.h>

/** Wildcard for map, offset or count: matches nothing. */
#define REGION_MATCH_NOTHING    -1
/** Wildcard for map, offset or count: matches anything. */
#define REGION_MATCH_ANYTHING   -2

struct region {
    int64_t map;
    int64_t offset;
    int64_t count;
};

struct region *
region_new(int64_t map, 
           int64_t offset,
           int64_t count);

void 
region_destroy(struct region *rgn);

#endif /* ! REGION_H */
