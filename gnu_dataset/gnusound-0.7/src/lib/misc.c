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
#include <sys/time.h>
#include <unistd.h>
#include <math.h>
#include <stdarg.h>
#include <ctype.h>
#include <dirent.h>
#include <glib.h>
#include "misc.h"
#include "mem.h"

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define ABS(a) (((a) < 0) ? -(a) : (a))

/**
 * Finds the extension of a filename.
 * @param path The filename.
 * @return NULL if no extension could be found, pointer to the
 * dot (.) otherwise.
 */

const char *
find_extension(const char *path) {
    char *dot, *slash;

    dot = strrchr(path, '.');
    slash = strrchr(path, '/');

    if(!dot)
        return NULL;

    /*
     * Make sure we don't misidentify dots that are part of a directory,
     * e.g. /some.path/file.
     */
    
    if(slash && dot < slash) 
        return NULL;

    return dot;
}

/**
 * Looks for a file in the given paths.
 * @param paths Colon-separated list of paths.
 * @param filename The file to find. If the filename contains an
 * absolute path, no search across the search paths is performed.
 * @param mode Mask consisting of one or more of R_OK, W_OK, X_OK and
 * F_OK (see access(2)).
 * @return NULL on error (file not found or out of memory), malloc()'d 
 * filename otherwise.
 */

char *
findfile(const char *search_paths, 
         const char *filename,
         int mode) {
    int i, maxlen = 0;
    char **paths = g_strsplit(search_paths, ":", 255);
    char *candidate = NULL;

    g_return_val_if_fail(filename != NULL, NULL);

    /* If the filename is absolute, don't search. */

    if(g_path_is_absolute(filename)) {
        if(!access(filename, mode))
            return strdup(filename);

        return NULL;
    }
        
    if(!paths)
        return NULL;

    /* Get largest possible path + filename. */

    for(i = 0; paths[i]; i++) 
        if(strlen(paths[i]) + strlen(filename) + 1 > maxlen)
            maxlen = strlen(paths[i]) + strlen(filename) + 2;
    
    if(!maxlen) {
        g_strfreev(paths);
        return NULL;
    }

    candidate = mem_alloc(maxlen);
    if(!candidate) {
        g_strfreev(paths);
        return NULL;
    }

    for(i = 0; paths[i]; i++) {
        snprintf(candidate, maxlen, "%s/%s", paths[i], filename);
        if(!access(candidate, mode)) {
            g_strfreev(paths);
            return candidate;
        }
    }

    g_strfreev(paths);
    free(candidate);
    return NULL;
}

/**
 * Converts a symbol to words by replacing '-' by ' ' and 
 * capitalizing the first character of each word.
 * @param sym The symbol to convert.
 * @return The words.
 */

char *
sym2words(char *sym) {
    char *p;
    int capitalize_next = 0;

    for(p = sym; *p; p++) {
        if(*p == '-')
            *p = ' ';
        if(*p == ' ') {
            capitalize_next = 1;
            continue;
        }
        if(capitalize_next) {
            *p = toupper(*p);
            capitalize_next = 0;
        }
    }
    return sym;
}

/**
 * Replaces spaces by newlines at intervals as close as possible
 * to, but no longer than, line_length. If no space is available
 * within the line_length interval, then the first available space 
 * character will be replaced.
 * @param s The string to wrap.
 * @param line_length The maximum requested line length.
 * @return The wrapped string.
 */

char *
wordwrap(char *s,
         int line_length) {
    char *start = s, *pos, *prev;

    while(*start) {

        pos = start;
        prev = NULL;

        while(*pos) {

            /* Scan until end of string, newline or space. */

            while(*pos != '\0' && *pos != ' ' && *pos != '\n') pos++;

            /* Stop if end of string or newline were encountered. */

            if(*pos == '\0' || *pos == '\n')
                break;

            /* Otherwise, determine whether we've exceeded the maximum
               line length -- if we have, then stop and replace the
               space character found in the previous iteration. */

            if(pos - start > line_length)
                break;
            
            /* Store the position of the found space character and
               advance to the next character. */

            prev = pos;
            pos++;
        }

        /* Exit if we're at the end of the string. */

        if(*pos == '\0') 
            break;

        /* If pos is a space character and prev is non-NULL, then prev
           contains the line break position. */

        if(prev && *pos == ' ')
            pos = prev;

        /* Replace character at pos by newline and advance to next
           character. */

        *pos = '\n';
        pos++;
        start = pos;
    }
    
    return s;
}


/**
 * Returns the difference, in seconds, between tv_start and tv_stop.
 * 
 * @param tv_start The earlier time.
 * @param tv_stop The later time.
 * @return Difference in seconds.
 */

double 
tv_diff_secs(const struct timeval *tv_start,
             const struct timeval *tv_stop) {
    return (tv_stop->tv_sec == tv_start->tv_sec ? 
            ((double)(tv_stop->tv_usec - tv_start->tv_usec) / 1000000) : 
            ((double)(tv_stop->tv_sec - tv_start->tv_sec) + 
             (double)(tv_stop->tv_usec < tv_start->tv_usec ? 
                      -(1000000 - ((1000000 - tv_start->tv_usec) + 
                                   tv_stop->tv_usec)) : 
                      (((1000000 - tv_start->tv_usec) + 
                        tv_stop->tv_usec) - 1000000)) / 1000000));
}

/**
 * Matches a string against a pattern that may contain * and ?
 * wildcards. Bugs: no provisions for matching on literal * and ?.
 * 
 * @param pattern The pattern to match against.
 * @param str The string to match.
 * @return 0, negative value, or positive value, for equal, smaller
 * than, or greater than respectively.
 */

int
match_wildcard(const char *pattern, 
               const char *str) {
    const char *p = pattern, *s = str;
    int d;
    
    while(p[0]) {

        /* Fail if the string was exhausted before the pattern. */

        if(!s[0] && p[0] != '*')
            return -1;

        switch(p[0]) {
        case '*':

            /* If this * is last char, then match always succeeds. */

            if(!p[1])
                return 0;

            /* Advance to next pattern position only if * is satisfied. */

            if(p[1] == s[0])
                p += 2;
            break;
        case '?':
            p++;
            break;
        default:
            if((d = s[0] - p[0]))
                return d;
            p++;
            break;
        }
        s++;
    }

    /* Fail if there remain unmatched characters. */

    if(s[0])
        return 1;

    return 0;
}

static char human_readable_size[40];
static char *human_readable_quantities[] = {
    "KB",
    "MB",
    "GB",
    "TB",
    "PB"
};

/**
 * Converts a byte quantity to a human readable string.
 * @param sz The byte quantity to convert.
 * @return Human readable string denoting the byte quantity.
 * Subsequent calls overwrite this string.
 */

char *
printable_byte_count(int64_t sz) {
    int64_t i;
    int64_t m = sz / 1024;
    if(sz < 1024) {
        snprintf(human_readable_size, sizeof human_readable_size, "%ld %s",
                 sz, sz == 1 ? "byte" : "bytes");
        return human_readable_size;
    }
    for(i = 0; m > 1024; i++) 
        m /= 1024;
    i = MIN((sizeof(human_readable_quantities) / 
             sizeof(human_readable_quantities[0])) - 1, i);
    snprintf(human_readable_size, sizeof human_readable_size, "%.2f %s",
             (double)sz / pow(1024, i+1), human_readable_quantities[i]);
    return human_readable_size;
}

/*
int
searchpath(const char *suffix,
           const char *prefixes,
           char *buf,
           size_t bufsz) {
    int i;
    char **paths = g_strsplit(prefixes, ":", 0);
    for(i = 0; paths[i]; i++) {
        snprintf(buf, sizeof buf, "%s/%s", paths[i], suffix);
        if(!access(buf, R_OK)) {
            g_strfreev(paths);
            return 0;
        }
    }
    g_strfreev(paths);
    return 1;
}
*/

#ifndef HAVE_VASPRINTF
int
vasprintf(char **strp,
          const char *fmt,
          va_list ap) {
    /* Guess we need no more than 100 bytes. */
    int n, size = 100;
    char *p, *q;
    *strp = NULL;
    if ((p = malloc (size)) == NULL)
        return 0;
    while (1) {
        /* Try to print in the allocated space. */
        n = vsnprintf (p, size, fmt, ap);
        /* If that worked, return the string. */
        if (n > -1 && n < size)
            return strlen(p);
        /* Else try again with more space. */
        if (n > -1)    /* glibc 2.1 */
            size = n+1; /* precisely what is needed */
        else           /* glibc 2.0 */
            size *= 2;  /* twice the old size */
        if ((p = realloc (p, size)) == NULL) {
            return n;
        }
    }
    *strp = p;
    return n;
}
#endif

#ifndef HAVE_ASPRINTF
int
asprintf(char **strp,
         const char *fmt, 
         ...) {
    va_list ap;
    int n;
    va_start(ap, fmt);
    n = vasprintf(strp, fmt, ap);
    va_end(ap);
    return n;
}

#endif
