/*
 * Usage: as CGI script, but called by userv
 * environment variables are USERV_U_E_...
 */
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
 * $Id: ucgitarget.c,v 1.2 1999/11/09 23:04:32 ian Exp $
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include "ucgi.h"

static void *xrealloc(void *ptr, size_t sz) {
  void *r;

  r= realloc(ptr,sz);
  if (!r) syserror("realloc failed");
  return r;
}

int main(int argc, const char **argv) {
  char *uservarn, *scriptpath, *newvar;
  const char *nextslash, *lastslash, *pathi, *ev, *ev2, *en, *scriptdir, *av;
  const char *const *ep;
  const char **arguments;
  size_t scriptdirlen, scriptpathlen, l, uservarnl;
  struct stat stab;
  int r, nargs;

  ev= getenv("USERV_U_DEBUG");
  if (ev && *ev) debugmode= 1;
  
  if (argc > MAX_ARGS) error("too many arguments");

  if (!*++argv) error("no script directory argument");
  ev= getenv("HOME"); if (!ev) error("no HOME env. var");
  l= strlen(*argv)+strlen(ev);
  newvar= xmalloc(l+2);
  sprintf(newvar,"%s/%s",ev,*argv);
  scriptdir= newvar;
  scriptdirlen= strlen(scriptdir);

  uservarn= 0;
  uservarnl= 0;
  for (ep= envok; (en= *ep); ep++) {
    l= strlen(en)+11;
    if (uservarnl<l) { uservarn= xrealloc(uservarn,l); uservarnl= l; }
    sprintf(uservarn,"USERV_U_E_%s",en);
    ev= getenv(uservarn); if (!ev) continue;
    if (strlen(ev) > MAX_ENVVAR_VALUE) error("environment variable too long");
    if (setenv(en,ev,1)) syserror("setenv");
    unsetenv(uservarn);
  }

  scriptpath= 0;
  pathi= getenv("PATH_INFO");
  if (!pathi) error("PATH_INFO not found");
  lastslash= pathi;
  for (;;) {
    if (*lastslash != '/') error("PATH_INFO expected slash not found");
    if (lastslash[1]=='.' || lastslash[1]=='#' || !lastslash[1]) error("bad char begin");
    nextslash= strchr(lastslash+1,'/');
    if (!nextslash) nextslash= lastslash+1+strlen(lastslash+1);
    if (!nextslash) error("insufficient elements in PATH_INFO");
    if (nextslash==lastslash+1) error("empty component in PATH_INFO");
    if (nextslash-pathi > MAX_SCRIPTPATH_LEN) error("PATH_INFO script path too long");
    scriptpathlen= scriptdirlen+(nextslash-pathi);
    scriptpath= xrealloc(scriptpath,scriptpathlen+1);
    strcpy(scriptpath,scriptdir);
    memcpy(scriptpath+scriptdirlen,pathi,nextslash-pathi);
    scriptpath[scriptpathlen]= 0;
    if (scriptpath[scriptpathlen-1]=='~') error("bad char end");
    r= stat(scriptpath,&stab); if (r) syserror("stat script");
    if (S_ISREG(stab.st_mode)) break;
    if (!S_ISDIR(stab.st_mode)) syserror("script not directory or file");
    lastslash= nextslash;
  }
  if (*nextslash) xsetenv("PATH_INFO",nextslash,1);
  else unsetenv("PATH_INFO");

  newvar= xmalloc(scriptpathlen+strlen(nextslash)+3);
  sprintf(newvar,"%s%s",scriptpath,nextslash);
  xsetenv("PATH_TRANSLATED",newvar,1);

  xsetenv("SCRIPT_FILENAME",scriptpath,1);

  ev= getenv("SCRIPT_NAME");
  if (ev) {
    ev2= getenv("USER"); if (!ev2) error("no USER variable");
    newvar= xmalloc(strlen(ev)+2+strlen(ev2)+scriptpathlen-scriptdirlen+2);
    sprintf(newvar,"%s/~%s%s",ev,ev2,scriptpath+scriptdirlen);
    xsetenv("SCRIPT_NAME",newvar,1);
  }

  arguments= xmalloc(sizeof(const char*)*(argc+5));
  nargs= 0;
  
  arguments[nargs++]= scriptpath;
  while ((av= (*++argv))) arguments[nargs++]= av;
  arguments[nargs++]= 0;

  execvp(scriptpath,(char*const*)arguments);
  syserror("exec script");
  return -1;
}
