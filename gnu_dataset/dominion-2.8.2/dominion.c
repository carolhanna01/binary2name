  /* dominion.c -- main loop of dominion */

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

#include "dominion.h"
#include "misc.h"

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

/* most of these data structures are declared in ext.c */
extern Suser user;
extern Sworld world;
extern struct race_list *races;
extern char libdir[];
extern int euid, ruid;
extern char help_tag[];
extern int timeout_secs, slow_flag;
extern void interrupt(), alarm_interrupt();
extern int debug;
extern int (*keymap[128])();
extern char *get_update_time(), *update_time;

int old_umask;			/* to reset when we leave */
#ifdef ANDREW
int beroot = 1;
#endif

int main(int argc, char *argv[])
{
  int c;
  int innation = 0;
  char nation[NAMELEN];
  extern char *optarg;
  extern int optind;
#ifdef BSD
    /* BSD uses getwd(), which does not allocate static space */
  extern char current_dir[], *getwd();
#else /* BSD */
  extern char *current_dir, *getcwd();
#endif /* BSD */
    /* starting help tag is Top */
  strcpy(help_tag, "Top");

    /* now get some unix system information */
  ruid = getuid();
  euid = geteuid();
  old_umask = umask(077);
/*  printf("ruid = %d, euid = %d\n", ruid, euid); */
#ifdef BSD
  getwd(current_dir);
#else /* BSD */
  current_dir = getcwd(NULL, PATHLEN);
#endif /* BSD */
/*  printf("current dir is %s\n", current_dir); */
  strcpy(libdir, DEF_LIBDIR);

  user.xmode = 0;
#ifndef ANDREW
  while ((c = getopt(argc, argv, "xn:d:vphcts--")) != EOF)
#else
  while ((c = getopt(argc, argv, "xn:d:vuphcts--")) != EOF)
#endif
    switch (c) {
#ifdef ANDREW
    case 'u':
      beroot = 0;
      break;
#endif
    case 'x':			/* set debugging mode */
      debug = 1;
      break;
    case 'n':			/* choose to be a specific nation */
      innation = 1;
      strcpy(nation, optarg);
      break;
    case 'v':			/* old viewall option; not used any more */
      /*      viewall = 1; */
      break;
    case 'd':			/* specifiy a certain directory */
      strcpy(libdir, optarg);
      break;
    case 'h':			/* just give the online help */
      chdir(libdir);
      init_screen();
      online_info();
      resetty();
      endwin();
      exit(1);
    case 'p':			/* print out the list of nations; don't play */
      printf("Dominion, version %s; prefix is\n%s\n\n", VERSION, PREFIX);
      chdir(libdir);
      read_races();
      read_world(&world, WORLD_FILE);
      load_master_execs();
      print_nations();
      exit(1);
    case 'c':			/* print last update time */
      chdir(libdir);
      printf("Last update at %s\n", get_update_time());
      exit(0);
      /* -t: play with the timeout option (undocumented). Should
	 go when we make the opt.0 file with global options, then
	 the game master can set the time out without recompiling.
       */
/* Actually, you could probably do better by adding an option -t # -SHU */
    case 't':
      timeout_secs = TIMEOUT_SECS; /* timeout after this many seconds */
      break;
      /* -s: slow the reading of the world file by inserting some
	 pauses; this is because on dirac (in spring 1992 dominion
	 game) things get really slow while world file is being loaded.
       */
    case 's':
      slow_flag = 1;
      break;
    default:
      usageerr(argc,argv);
      exit(1);
    }
#ifdef ANDREW
  if (beroot == 1) {
    Authenticate();
    beGames();
  }
#endif
  if (chdir(libdir) != 0) {
    fprintf(stderr,"Error: cannot cd to directory %s\n",libdir);
    clean_exit();
    exit(1);
  }
  update_time = get_update_time();

  init();			/* set up data structures */
  init_user(innation, nation);
    /* init keymap after user is loaded, because of master commands */
  init_keymap();
  init_screen();
  noncritical();		/* normal signal operation */
  intro(&world, user.np);
  main_loop();
  cleanup();
  clean_exit();
  exit(0);
}

void init()
{
  extern Sdiplo **allocate_diplo();

  printf("Initializing...\r\n");

      /* check if there is a master lock file */
  if (is_master_lock()) {
    printf("There is a master lock file.  You cannot play right now.\n");
    exit(1);
  }

  SRND(time(0L));		/* initialize random number generator */
  read_races();			/* get races from races file */
    /* read in the world */
  read_world(&world, WORLD_FILE);
  initial_diplo = allocate_diplo(world.n_nations);
  read_initial_diplo(initial_diplo, world.n_nations);
}

	/* this is where all the work is done  */
void main_loop()
{
  char c;
  char s[80];

  for ( ; ; ) {
    sprintf(s, "Nation %s; money %d; Thon %d;   type %c for help",
	    user.np->name, user.np->money, world.turn, user.help_char);
    statline(s, "draw_map_regular");
    if (user.just_moved) {
      draw_map();
      user.just_moved = 0;
    }
    set_cursor();
    c = to_getch();
    (*keymap[c % 128])();	/* make sure no high bits get there */
    (void) re_center(user.cursor.x, user.cursor.y); /* ignore return val */
    if (user.just_moved) {
      wrap(&user.cursor);
      if (!army_is_in_sector(&world.map[user.cursor.x][user.cursor.y],
			     user.id, user.current_army)) {
	user.current_army
	  = first_sect_army(&world.map[user.cursor.x][user.cursor.y]);
      }
    }
  }
}

  /* clean up graphics, and flush out the exec file */
void cleanup()
{
  statline("", "cleanup");
/*  delay_output(200); */
  resetty();
  endwin();
  putchar('\n');
  putchar('\n');
  gen_exec(NULL);
}

void clean_exit()
{
  extern int is_in_diplo;

  if (is_in_diplo) {
    unlink("DIPLOCK");
  }
  del_lock(user.id);
  umask(old_umask);
#ifdef ANDREW
  if (beroot == 1) {
    bePlayer();
    unAuth();
    beroot = 0;
  }
#endif
}

void critical()			/* while writing data files, don't bug me!! */
{
  signal(SIGINT, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);
  signal(SIGHUP, SIG_IGN);
  signal(SIGALRM, SIG_IGN);
#ifdef SIGTSTP
  signal(SIGTSTP, SIG_IGN);
#endif /* SIGTSTP */
}

void noncritical()		/* normal operation */
{
  signal(SIGINT, interrupt);
  signal(SIGQUIT, interrupt);
  signal(SIGHUP, interrupt);
  signal(SIGALRM, alarm_interrupt);
    /* SIGTSTP is dangerous for exec files so we just quit the game */
#ifdef SIGTSTP
  signal(SIGTSTP, interrupt);
#endif /* SIGTSTP */
}

  /* just prints a list of all nations to the terminal */
void print_nations()
{
  int i;
  Snation *np;

  printf("%s %-14s[%s]    %-15s %-10s\n", "Id", "Name",
	 "mark", "Leader", "Race");
  printf("----------------------------------------------------------------------\n");
  for (i = 1; i < world.n_nations; ++i) {
    np = &world.nations[i];
    printf("%2d %-15s [%c]     %-15s %-10s %-10s %-5s\n", i, np->name,np->mark,
	   np->leader, np->race.name, is_active_ntn(np) ? "" : "DESTROYED",
	   np->cn_flag ? "cn" : "");
  }
}

