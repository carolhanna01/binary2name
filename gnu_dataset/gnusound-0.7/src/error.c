/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2005  Pascal Haakmat <a.haakmat@chello.nl>
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
#include <stdarg.h>
#include "error.h"
#include "mem.h"

static char *error_msg_nomem = "Out of memory";

int
error_get_code(struct error *error) {
    return error->code;
}

const char *
error_get_message(struct error *error) {
    if(!error)
        return "Out of memory or invalid NULL pointer";

    if(!error->message)
        return "No error";

    return error->message;
}

int
error_thrown(struct error *error) {
    if(!error)
        return 1;

    if(error->message)
        return 1;
    return 0;
}

/**
 * Propagates an error from one error object to another, clearing
 * the error on the source.
 * @param error The error object to propagate the error to.
 * @param source The error object containing the error to propagate.
 * @param format The format string to prepend to the propagated error.
 */

void
error_cascade(struct error *error,
              struct error *source,
              const char *format,
              ...) {
    va_list ap;
    char cascade[4096];
    
    if(!error) {
        FAIL("error == NULL\n");
        return;
    }

    DEBUG("source: %s\n", error_get_message(source));
    snprintf(cascade, sizeof(cascade), "%s (%s)",
             format,
             error_get_message(source));
    
    error_free(error);
    error_free(source);

    error->message = mem_alloc(strlen(cascade) + 4096);
    if(!error->message) {
        error->message = error_msg_nomem;
        return;
    }
    va_start(ap, format);
    vsnprintf(error->message, strlen(cascade) + 4096, cascade, ap);
    va_end(ap);
}

void
error_set(struct error *error,
          const char *format,
          ...) {
    va_list ap;

    if(!error) {
        FAIL("error == NULL\n");
        return;
    }

    error_free(error);

    error->code = 1;
    error->message = (char *) mem_alloc(strlen(format) + 4097);

    if(!error->message) {
        error->message = error_msg_nomem;
        return;
    }
    
    va_start(ap, format);
    vsnprintf(error->message, strlen(format) + 4096, format, ap);
    va_end(ap);
    FAIL("error message: %s\n", error->message);
}

void
error_init(struct error *error) {
    error->magic = ERROR_MAGIC;
    error->message = NULL;
    error->code = 0;
}

void
error_free(struct error *error) {
    if(error->message == error_msg_nomem)
        return;

    if(error->message) {
        free(error->message);
        error->message = NULL;
    }
}


struct error *
ERROR(void *p) {
    struct error *err;
    if(!p) {
        FAIL("NULL pointer\n");
        return p;
    }
    err = p;
    if(err->magic != ERROR_MAGIC) {
        FAIL("invalid cast to ERROR from %p\n", p);
        abort();
        return NULL;
    }
    return err;
}
