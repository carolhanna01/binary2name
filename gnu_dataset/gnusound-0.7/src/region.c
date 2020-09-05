#include <config.h>
#include "mem.h"
#include "region.h"

struct region *
region_new(int64_t map, 
           int64_t offset,
           int64_t count) {
    struct region *rgn = mem_alloc(sizeof(*rgn));
    if(!rgn)
        return NULL;
    rgn->map = map;
    rgn->offset = offset;
    rgn->count = count;

    return rgn;
}


void 
region_destroy(struct region *rgn) {
    mem_free(rgn);
}
