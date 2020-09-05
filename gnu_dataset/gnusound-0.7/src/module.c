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

#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <libgen.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <dlfcn.h>
#include <wordexp.h>
#include <config.h>
#include "module.h"

static int module_count = 0;
static struct gnusound_module_container containers[MAX_MODULES];

void
module_exit() {
    int i;
    for(i = 0; i < module_count; i++) {
        //        DEBUG("destroying module '%s'\n", containers[i].mod->name);        
        if(containers[i].mod->exit) 
            containers[i].mod->exit(i);
        dlclose(containers[i].handle);
        free(containers[i].filename);
        free(containers[i].path);
    }
}

int
module_get_count() {
    return module_count;
}

struct gnusound_module *
module_get(int id) {
    if(id < 0) 
        return NULL;

    if(id >= module_count)
        return NULL;

    return containers[id].mod;
}

const char *
module_get_filename(int id) {
    if(id < 0) 
        return NULL;

    if(id >= module_count)
        return NULL;

    return containers[id].filename;
}


const char *
module_get_path(int id) {
    if(id < 0) 
        return NULL;

    if(id >= module_count)
        return NULL;

    return containers[id].path;
}


void
module_load(const char *path) {
    int i;
    void *handle;
    char *error;
    struct gnusound_module *mod;

    if(module_count + 1 >= MAX_MODULES) {
        FAIL("won't load %s because maximum number of modules exceeded.\n",
             path);
        return;
    }

    handle = dlopen(path, RTLD_NOW);
    if (!handle) {
        DEBUG("could not load %s: %s\n", path, dlerror());
        return;
    }

    mod = dlsym(handle, "manifest");
    if((error = dlerror()) != NULL) {
        FAIL("no manifest in %s\n", path);
        dlclose(handle);
        return;
    }

    /* Test magic. */

    if(mod->magic != MODULE_MAGIC) {
        FAIL("bad magic %x for %s\n", mod->magic, path);
        dlclose(handle);
        return;
    }

    /* Test API version. */

    if(mod->api_version != MODULE_API_VERSION_4) {
        FAIL("unsupported API revision %x for %s\n", mod->api_version, path);
        dlclose(handle);
        return;
    }

    /* Reject loading modules with the same name. */

    for(i = 0; i < module_count; i++) {
        if(!strcmp(mod->name, containers[i].mod->name)) {
            FAIL("already loaded %s\n", mod->name);
            dlclose(handle);
            return;
        }
    }

    /* Test module initialization. */

    if(mod->init && mod->init(module_count)) {
        FAIL("%s: module initialization failed\n", path);
        dlclose(handle);
        return;
    }
    
    containers[module_count].mod = mod;
    containers[module_count].handle = handle;
    containers[module_count].filename = strdup(path);
    containers[module_count].path = dirname(strdup(path));
    module_count++;
    
    //    DEBUG("created module %d, '%s'\n", module_count, mod->name);    
}

int
module_compare(const void *a, 
               const void *b) {
    return strcmp(((struct gnusound_module_container *)a)->mod->name, 
                  ((struct gnusound_module_container *)b)->mod->name);
}

int
module_select(const struct dirent *d) {
    if(strstr(d->d_name, ".so"))
        return 1;
    return 0;
}

int
module_collect(const char *path) {
    char tmp[512];
    struct dirent **namelist;
    int i, n;

    n = scandir(path, &namelist, module_select, alphasort);
    if (n < 0) 
        return 0;

    for(i = 0; i < n; i++) {
        snprintf(tmp, 512, "%s/%s", path, namelist[i]->d_name);
        free(namelist[i]);
        module_load(tmp);
    }

    free(namelist);

    // FIXME: Sorting here is the wrong approach, because it changes
    // the module id's.

    //    qsort(containers, module_count, sizeof(containers[0]), module_compare);

    return 0;
}

int
module_init() {
    int i, r;
    char *sp = strdup(MODULE_SEARCH_PATH), *t, *sp2;
    wordexp_t exp;
    if(!sp) {
        FAIL("unable to initialize modules, not enough memory to copy MODULE_SEARCH_PATH, bye\n");
        return 1;
    }
    sp2 = sp;
    while((t = strsep(&sp, ":"))) {
        r = wordexp(t, &exp, 0);
        if(r) {
            FAIL("unable to initalize modules in path %s, invalid path?\n",
                 t);
            continue;
        }
        DEBUG("collecting modules in path %s\n", t);
        for(i = 0; i < exp.we_wordc; i++) {
            DEBUG("expansion %d: %s\n", i, exp.we_wordv[i]);
            module_collect(exp.we_wordv[i]);
        }
        wordfree(&exp);
    }
    free(sp2);

    return 0;
}

