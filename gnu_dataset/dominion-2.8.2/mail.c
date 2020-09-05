  /* mail.c -- dominion mail system */

/*
 * Copyright (C) 1990 Free Software Foundation, Inc.
 * Written by the dominion project.
 *
 * This file is part of dominion.
 *
 * dominion is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

#ifdef AMIGA
# include <exec/types.h>
#else
# include <sys/types.h>
#endif

#include <time.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <errno.h>

#include "dominion.h"

extern Suser user;
extern Sworld world;
extern char *libdir;
extern int ruid, euid;

char *mail_forwarding(nation)
     int nation;
{
    return world.nations[nation].opts->mail_forward;
}

/* This function checks if mailbox for nation 'id' is locked. */
int has_mail_lock(int id)
{
  FILE *lock_fp;
  char lock_fn[PATHLEN];
  int ret;

/*  ruid = getuid();
  euid = geteuid();
*/
  if (mail_forwarding(id)) {
    return 0;
  }

  sprintf(lock_fn,"%s/%d.lock", MAIL_DIR, id);
    /* if it's locked: close and return 1 */
  if ((lock_fp = fopen(lock_fn, "r")) != NULL) {
    fclose(lock_fp);
    ret=1;
  } else {
    ret=0;
  }
  return ret;
} /* has_mail_lock */

/* This function locks a user's mailbox */
void lock_mail(int nation)
{
  FILE *lock_fp;
  char lock_fn[100];
  
  if (mail_forwarding(nation)) {
    return;
  }

  sprintf(lock_fn, "%s/%d.lock", MAIL_DIR, nation);
  if ((lock_fp = fopen(lock_fn, "w")) != NULL) {
    fprintf(lock_fp, "%ld; Nation %s\n", time(0L), user.np->name);
    fclose(lock_fp);
  }
}

/* This unlock's a user's mailbox */
void unlock_mail(int nation)
{
  char lock_fn[100];
  
  if (mail_forwarding(nation)) {
    return;
  }

  sprintf(lock_fn, "%s/%d.lock", MAIL_DIR, nation);
  unlink(lock_fn);
}

/* This calls the system to edit the given file with the preferred editor */
void edit(char *fname)
{
  char *edit_prog, *getenv();
  char edit_command[200];
  int child;			/* child's process id */
  extern int errno;

  if (user.np->opts->editor) {
    edit_prog = user.np->opts->editor;
  } else if ((edit_prog=getenv("DOMINION_EDITOR"))==NULL
      && (edit_prog = getenv("VISUAL")) == NULL
      && (edit_prog = getenv("EDITOR")) == NULL) {
    edit_prog = DEFAULT_EDITOR;
  }
 
  critical();
#ifdef UID_SECURITY
    /* we must fork, so that in the child we set the
       real user id, whereas the parent continues with
       the effective user id.
     */
  if ((child = fork()) == 0) {		/* child has fork() == 0 */
    setuid(ruid);		/* so this user cannot poke around */
    close(creat(fname, 0600));
    sprintf(edit_command, "%s %s", edit_prog, fname);
    system(edit_command);
    /* change owner so that it can be processed once the uid changes back */
    chown(fname, euid, getgid()); /* use the system call to chown() */
/*
    printf("waiting; type a few returns\n");
    fflush(stdout);
    getchar();
    getchar();
*/
    exit(0);
  }

  {
    int wait_val, wait_stat = 0;
    errno = 0;
    while ((wait_val = wait(&wait_stat)) != child) {
      FILE *fp;
      int save_errno = errno;
      if ((fp = fopen("errlog", "a")) != NULL) {
	fprintf(fp, "wait() return was not equal to child (%d):\n", child);
	fprintf(fp, "  wait_val = %d; wait_stat = 0x%08x; errno = %d\n",
		wait_val, wait_stat, save_errno);
	fclose(fp);
      }
    }
  }

#else /* UID_SECURITY */
  close(creat(fname, 0666));
  sprintf(command, "chmod 666 %s", fname);
  system(command);
  sprintf(edit_command, "%s %s", edit_prog, fname);
  system(edit_command);
#endif /* UID_SECURITY */
  noncritical();
}

/* Insert a file (with the name in_name) into the open file stream pointed
   to by out_pntr
 */
void insert_mail(char *in_name, FILE *out_pntr)
{
  FILE *in_pntr;
  char line[PATHLEN];

  /* read the message in line by line so we can handle
     special things such as a line beginning with "From".
   */
  if ((in_pntr = fopen(in_name, "r")) != NULL) {
    while (fgets(line, PATHLEN-1, in_pntr) != NULL) {
      /* now if the line starts with "From ", prepend a '>'.
	 Note that this is not "From:", but "From ".
       */
      if (strncmp(line, "From ", strlen("From ")) == 0) {
	fputc('>', out_pntr);
      }
      fputs(line, out_pntr);
    }
    fclose(in_pntr);
  }
/*
  if ((in_pntr=fopen(in_name, "r"))!=NULL) {
    while((c=fgetc(in_pntr))!=EOF) {
      fputc(c, out_pntr);
    }
    fclose(in_pntr);
  }
*/
} /* insert_mail */

char *fix_name(s,fixed)
/* Elm etc. want the name that mail is from to contain no white space */
     char *s, *fixed;
{
  char *poss = s, *posf = fixed;

  if (s == NULL) { return NULL; }
  for ( poss = s;(poss != '\0') && (posf - fixed < NAMELEN);  poss++ , posf++)
  {
    if ((*poss == ' ') || (*poss == '\t')) {
      *posf = '_';
    } else {
      *posf = *poss;
    }
  }
  if (posf - fixed < NAMELEN) { *posf = '\0' ; }
  else { *(fixed + NAMELEN - 1) = '\0'; }
  return fixed;
}
/* Send mail from one nation to another. mailfile is the _name_ of the
   file containing the body of the mail. sender and receiver are the full
   names of the appropriate nations. Guess what subject is...
 */
int mail_send(char mailfile[], int sender, int receiver, char subject[])
{
  time_t now_secs;
  char dest_fn[200], *now_chars;
  FILE *dest_fp;
  char *forward, fixed_name[NAMELEN], fixed_name2[NAMELEN];
  int fd;

  if ((forward = world.nations[receiver].opts->mail_forward)) {
    FILE *temp_fp;
    char tmp_fname[PATHLEN];
    char command[2048];
#ifdef UID_SECURITY

#endif

      strcpy(tmp_fname, "/usr/tmp/domXXXXXX");
      if ((fd = mkstemp(tmp_fname)) == -1) {
        fprintf(stderr, "Error getting temp file name\n");
        return 1;
      }
      close(fd);
      temp_fp = fopen(tmp_fname, "w");
      if (!temp_fp) {
	perror("Could not open temp file");
	return 1;
      }
      fprintf(temp_fp,"From: %s of %s\n",world.nations[sender].leader,
	      world.nations[sender].name);
      fprintf(temp_fp,"To: %s of %s\n",world.nations[receiver].leader,
	      world.nations[receiver].name);
      fprintf(temp_fp,"Subject: %s\n\n",subject);
      fclose(temp_fp);

#ifdef UID_SECURITY
      sprintf(command, "chmod +r %s", tmp_fname);
      system(command);
#endif

      sprintf(command, "cat %s %s | %s '%s' ; sleep 3",
	      tmp_fname, mailfile, MAILER, forward);

/*
      printf("\r\n ready to run the cat command; type some returns \r\n");
      fflush(stdout);
      getchar();
      getchar();
*/
      system(command);
      unlink(tmp_fname);
      unlink(mailfile);		/* remove the file the message was kept in */
      return 0;			/* we've forwarded, so that's all */
    }

  /* Set the names of the files used in mail */
  sprintf(dest_fn, "%s/mail.%d", MAIL_DIR, receiver);

#ifdef OLD_WAY
/*  sprintf(lock_fn, "%d.lock", receiver); */
  strcpy(tmp_fname, "dommaXXXXXX");
  mktemp(tmp_fname);
#endif /* OLD_WAY */

    /* Get the time right now */
  now_secs=time(0L);
  now_chars=ctime(&now_secs);

    /* If not make sure it won't be used */
/*  lock_fp=fopen(lock_fn, "w");
  fprintf(lock_fp, "Mail being sent by %s at %s\n", user.np->name, now_chars);
  fclose(lock_fp);
*/

  if ((dest_fp = fopen(dest_fn, "a")) == NULL) {
    fprintf(stderr, "Error: Cannot append to mail file %s\n", dest_fn);
    clean_exit();
    exit(1);
  }

  /* now append the new mail (from mailfile) to the
     player's mailbox (called dest_fn).
   */
  fix_name(world.nations[sender].leader, fixed_name);
  fprintf(dest_fp, "From %s %s", fixed_name, now_chars);
  fprintf(dest_fp, "Date: %s", now_chars);
  fprintf(dest_fp, "From: %s_of_%s\n", world.nations[sender].leader,
	  world.nations[sender].name);
  fix_name(world.nations[receiver].leader, fixed_name);
  fix_name(world.nations[receiver].name, fixed_name2);
  fprintf(dest_fp, "To: %s_of_%s\n", fixed_name, fixed_name2);
  fprintf(dest_fp, "Subject: %s\n\n", subject);
  insert_mail(mailfile, dest_fp);
  fprintf(dest_fp, "\n\n");
  fclose(dest_fp);
  unlink(mailfile);		/* remove the file the message was kept in */
  return 0;

#ifdef OLD_WAY
    /* Copy the mail that's there right now out */
  if ((temp_fp = fopen(tmp_fname, "w")) == NULL) { 
    fprintf(stderr,"Error: Cannot write to mail file %s\n",tmp_fname);
    clean_exit();
    exit(1);
  }
  insert_mail(dest_fn, temp_fp);
  fclose(temp_fp);
    /* Put in the new mail */
  if ((dest_fp=fopen(dest_fn, "w"))!=NULL) {
       /* Some header stuff */
      fix_name(world.nations[sender].leader, fixed_name);
      fprintf(dest_fp, "From %s %s", fixed_name,now_chars);
      fprintf(dest_fp, "Date: %s", now_chars);
      fprintf(dest_fp, "From: %s of %s\n", world.nations[sender].leader,
	      world.nations[sender].name);
      fprintf(dest_fp, "To: %s of %s\n", world.nations[receiver].leader,
	      world.nations[receiver].name);
      fprintf(dest_fp, "Subject: %s\n\n", subject);

        /* Now the body of the message */
      insert_mail(mailfile, dest_fp);
      fprintf(dest_fp, "\n");
        /* and now copy the old mail back in */
      insert_mail(tmp_fname, dest_fp);
      fclose(dest_fp);
    } /* if fopen dest_fp */
  else {
    if (sender==0) {
      fprintf(stderr, "Couldn't open dest_fn\n");
    }
  }
    /* Remove the old unnecessary files */
  unlink(tmp_fname);
  unlink(mailfile);
/*  unlink(lock_fn); */
  unlock_mail(receiver);
  return 0;
#endif /* OLD_WAY */
} /* mail_send */

