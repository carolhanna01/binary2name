/*
 * Usage: as CGI script
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
 * $Id: ucgi.c,v 1.2 1999/11/09 23:04:32 ian Exp $
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "ucgi.h"

int main(int argc, const char **argv) {
  char *defarg, *username;
  const char *slash2, *pathi, *ev, *en, *av;
  const char *const *ep;
  const char **arguments;
  size_t usernamelen, l;
  pid_t child, rchild;
  int nargs, status;

  l= strlen(argv[0]);
  if (l>6 && !strcmp(argv[0]+l-6,"-debug")) debugmode= 1;

  if (debugmode) {
    if (fputs("Content-Type: text/plain\n\n",stdout)==EOF || fflush(stdout))
      syserror("write stdout");
    if (dup2(1,2)<0) { perror("dup stdout to stderr"); exit(-1); }
  }
  
  if (argc > MAX_ARGS) error("too many arguments");

  pathi= getenv("PATH_INFO");
  if (!pathi) error("PATH_INFO not found");
  if (pathi[0] != '/' || pathi[1] != '~') error("PATH_INFO must start with /~");
  slash2= strchr(pathi+2,'/'); if (!slash2) error("PATH_INFO must have more than one /");
  usernamelen= slash2-(pathi+2);
  if (usernamelen > MAX_USERNAME_LEN) error("PATH_INFO username too long");
  username= xmalloc(usernamelen+1);
  memcpy(username,pathi+2,usernamelen); username[usernamelen]= 0;
  if (!isalpha(username[0])) error("username 1st character is not alphabetic");
  xsetenv("PATH_INFO",slash2,1);
  
  arguments= xmalloc(sizeof(const char*)*(nenvok+argc+10));
  nargs= 0;
  
  arguments[nargs++]= "userv";
  if (debugmode) arguments[nargs++]= "-DDEBUG=1";
  
  for (ep= envok; (en= *ep); ep++) {
    ev= getenv(en); if (!ev) continue;
    l= strlen(ev); if (l > MAX_ENVVAR_VALUE) error("environment variable too long");
    defarg= xmalloc(strlen(en)+l+6);
    sprintf(defarg,"-DE_%s=%s",en,ev);
    arguments[nargs++]= defarg;
  }

  arguments[nargs++]= username;
  arguments[nargs++]= "www-cgi";
  while ((av= (*++argv))) arguments[nargs++]= av;
  arguments[nargs++]= 0;

  if (debugmode) {
    child= fork(); if (child==-1) syserror("fork");
    if (child) {
      rchild= waitpid(child,&status,0);
      if (rchild==-1) syserror("waitpid");
      printf("\nexit status %d %d\n",(status>>8)&0x0ff,status&0x0ff);
      exit(0);
    }
  }
      
  execvp("userv",(char*const*)arguments);
  syserror("exec userv");
  return -1;
}
