/*
 * Copyright (C) 1998-1999 Ian Jackson
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with userv-utils; if not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * $Id: ucgicommon.c,v 1.3 1999/11/09 23:04:32 ian Exp $
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "ucgi.h"

const char *const envok[]= {
  "CONTENT_LENGTH",
  "CONTENT_TYPE",
  "DOCUMENT_ROOT",
  "GATEWAY_INTERFACE",
  "HTTP_ACCEPT",
  "HTTP_ACCEPT_ENCODING",
  "HTTP_ACCEPT_LANGUAGE",
  "HTTP_CACHE_CONTROL",
  "HTTP_HOST",
  "HTTP_NEGOTIATE",
  "HTTP_PRAGMA",
  "HTTP_USER_AGENT",
  "PATH_INFO",
  "PATH_TRANSLATED",
  "QUERY_STRING",
  "REMOTE_ADDR",
  "REMOTE_HOST",
  "REMOTE_USER",
  "REMOTE_IDENT",
  "REQUEST_METHOD",
  "SCRIPT_FILENAME",
  "SCRIPT_NAME",
  "SCRIPT_URI",
  "SCRIPT_URL",
  "SERVER_ADMIN",
  "SERVER_NAME",
  "SERVER_PORT",
  "SERVER_PROTOCOL",
  "SERVER_SOFTWARE",
  0
};
const int nenvok= sizeof(envok)/sizeof(envok[0]);

int debugmode= 0;

static void outerror(void) {
  perror("stdout");
  exit(debugmode ? 0 : -1);
}

void syserror(const char *m) {
  if (printf("Content-Type: text/plain\n\n"
	     "ucgi: system call error:\n"
	     "%s: %s\n",
	     m,strerror(errno))==EOF || fflush(stdout)) outerror();
  exit(0);
}

void error(const char *m) {
  if (printf("Content-Type: text/plain\n\n"
	     "ucgi: error:\n"
	     "%s\n",
	     m)==EOF || fflush(stdout)) outerror();
  exit(0);
}

void *xmalloc(size_t sz) {
  void *r;

  r= malloc(sz);
  if (!r) syserror("malloc failed");
  return r;
}

void xsetenv(const char *en, const char *ev, int overwrite) {
  if (setenv(en,ev,overwrite)) syserror("setenv");
}
