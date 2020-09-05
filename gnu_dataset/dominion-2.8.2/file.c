/* file.c -- file operations */

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

/* this module contains routines that read and write the
   world data file, and also routines that handle lock files.
 */

#include <stdio.h>
#include <fcntl.h>
#include <time.h>
#include <stdlib.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <unistd.h>
#include <string.h>

#define IN_FILE_C
#include "dominion.h"
#include "misc.h"
#define UPDATE_FILE "last_update"

extern int debug, compressed_world, slow_flag;
extern struct race_list *races;
extern Suser user;

void read_world(Sworld *wp, char fname[])
{
  int i, j;
  FILE *fp, *fopen();
  char cmd[200];

  load_world_options();
  compressed_world = check_compressed_world(fname);
  if (compressed_world) {
    sprintf(cmd, "zcat %s", fname);
    if ((fp = popen(cmd, "r")) == NULL) {
      printf("\r\ncannot open pipe <%s> for reading\r\n", cmd);
      clean_exit(1);
      exit(1);
    }
  } else {
    if ((fp = fopen(fname, "r")) == NULL) {
      printf("\r\ncannot open file <%s> for reading\r\n", fname);
      clean_exit();
      exit(1);
    }
  }
  printf("Reading world data file '%s'", fname);

  read_int( fp, &wp->turn); /* get current turn */
  read_int( fp, &wp->xmax);	/* get world size */
  read_int( fp, &wp->ymax);
  if (debug >= 3) {
    printf("File position is %ld\n",ftell(fp));
  }
  if (debug >= 1) {
    fprintf(stderr, "World size is %dx%d\n", wp->xmax, wp->ymax);
  }

  /* make space for world sectors */
  wp->map = (Ssector **) malloc(wp->xmax*sizeof(Ssector *));
  for (i =  0; i < wp->xmax; ++i) {
    wp->map[i] = (Ssector *) malloc(wp->ymax * sizeof(Ssector));
  }
  /* now read in the world map */
  for (i = 0; i < wp->xmax; ++i) {
    for (j = 0; j < wp->ymax; ++j) {
      if (debug >= 3) {		/* warning! lots of output! */
        printf("File position is %ld\n", ftell(fp));
      }

      read_sector(fp, &wp->map[i][j]);
      wp->map[i][j].alist = NULL;
    }
    if (i % (wp->xmax/8) == 0) {
      printf(".");
      fflush(stdout);
    }
    if (slow_flag && ((4*i) % wp->xmax < 4)) {
      printf("[p]");
      fflush(stdout);
      sleep(1);			/* slow it down a little */
    }
  }
  if (debug >= 3) {
    printf("File position is %ld\n", ftell(fp));
  }

  /* load geography */
  read_geo(fp, &wp->geo);
  printf(".");
  fflush(stdout);

  /* now read in nations */
  read_int( fp, &wp->n_nations);
/*  if (debug >= 1) { */
  fprintf(stderr, "\nReading in %d nations", wp->n_nations);
  fflush(stderr);
/*  } */
  wp->nations = (Snation *) malloc(wp->n_nations*sizeof(Snation));
  for (i = 0; i < wp->n_nations; ++i) {
    /* must find a better way of loading linked lists
       (when I make the nations be a linked list!!)
       */
    read_nation(&wp->nations[i], fp, wp);
    load_options(&wp->nations[i]);
    load_bonds(&wp->nations[i]);
    if (slow_flag && ((4*i) % wp->n_nations < 4)) {
      printf("[p]");
      fflush(stdout);
      sleep(1);			/* slow it down a little */
    }

    fprintf(stderr, ".");
    fflush(stdout);
  }
  if (compressed_world) {
    pclose(fp);
  } else {
    fclose(fp);
  }
  init_wrap();
  printf("\n");
}

/* save the world data file */
void write_world(Sworld *wp, char fname[])
{
  int i, j;
  FILE *fp, *fopen();
  char cmd[200];		/* for compression command */

  if (compressed_world) {
    sprintf(cmd, "gzip > %s.gz", fname);
    if ((fp = popen(cmd, "w")) == NULL) {
      printf("\r\ncannot open pipe <%s> for writing\r\n", cmd);
      clean_exit();
      exit(1);
    }
  } else {
    if ((fp = fopen(fname, "w")) == NULL) {
      printf("cannot open file <%s> for writing\n", fname);
      clean_exit();
      exit(1);
    }
  }
  critical();
  write_int( fp, wp->turn); /* write current turn number */
  write_int( fp, wp->xmax);
  write_int( fp, wp->ymax);
  if (debug >= 3) {
    printf("File position is %ld\n", ftell(fp));
  }
  for (i = 0; i < wp->xmax; ++i) {
    for (j = 0; j < wp->ymax; ++j) {
/* This statement is also ridiculous */
/*      if (debug >= 3) {
        printf("File position is %d\n",ftell(fp));
      }
*/
      write_sector(fp, &wp->map[i][j]);
    }
  }
    /* write out geography */
  if (debug >= 3) {
    printf("File position is %ld\n", ftell(fp));
  }
  write_geo(fp, &(wp->geo));
/*  fwrite(&wp->geo, sizeof(wp->geo), 1, fp); */

  printf("writing out %d nation%c\n",
	 wp->n_nations, wp->n_nations == 1 ? ' ' : 's');
  write_int( fp, wp->n_nations);
	/* only save as many nations as there are */
  for (i = 0; i < wp->n_nations; ++i) {
    write_nation(&wp->nations[i], fp);
    if (debug) {
      putchar('.');
    }
  }

  if (compressed_world) {
    pclose(fp);
  } else {
    fclose(fp);
  }
  noncritical();		/* delicate phase over */
}


void write_nation(Snation *np, FILE *fp)
{
  int i;
  Sarmy *ap;			/* for armies */
  struct pt_list *pp;		/* for owned sectors */

    /* here is how we save a nation:  write out the actual nation data
       structure first, and after it write out the various linked lists
       (which are the army list and the list of owned points).
     */
  write_nation_struct(fp, np);
/*  fwrite(np, sizeof(Snation), 1, fp); */

    /* write out list of country's armies */
  ap = np->armies;
  for (i = 0; i < np->n_armies; ++i) {
    write_army(fp, ap);
/*    fwrite(ap, sizeof(Sarmy), 1, fp); */
    if (debug >= 3) {
      putchar('a');
    }
    ap = ap->next;
  }

    /* write list of owned locations */
  pp = np->ptlist;
  for (i = 0; i < np->n_sects; ++i) {
    write_point_list(fp, pp);
/*    fwrite(pp, sizeof(struct pt_list), 1, fp); */
    if (debug >= 3) {
      putchar('s');
    }
    pp = pp->next;
  }
}


void read_nation(Snation *np, FILE *fp, Sworld *wp)
{
  int i, x, y;
  Ssector *sp;			/* for tmp use */
  Sarmy army;			/* for armies */
  struct pt_list *pp;		/* for owned sectors */

    /* here is how we load a nation:  read in the actual nation data
       structure first, and after it read in the various linked lists
       (which are the army list and the list of owned
       points).  This is harder than writing because we must insert
       all into linked lists.
     */

  read_nation_struct(fp, np);

    /* now reset to zero the nation's "current" R&D values:
       that way the special R&D investments of the previous turn
       are zeroed, and only the percent-of-revenue values are saved
     */
  np->cur_mag_r_d = np->cur_mag_r_d_jewels = 0;
  np->cur_tech_r_d = np->cur_tech_r_d_metal = 0;
  np->cur_spy_r_d = 0;

  /* read list of country's armies add each army to the list for
     its nation and for its sector. Special case for first army.
   */
  np->armies = NULL;
  for (i = 0; i < np->n_armies; ++i) {
    read_army(fp, &army);
/*    fread(&army, sizeof(Sarmy), 1, fp); */
    army.next = NULL;
    x = army.pos.x;
    y = army.pos.y;
    sp = &(wp->map[x][y]);
    if (i == 0) {
/*      army.id = 0; */
      np->armies = (Sarmy *) malloc(sizeof(Sarmy));
      *(np->armies) = army;
      np->armies->next = NULL;
    } else {
/*      if (debug) {
	printf("Calling insert_army_nation, id=%d\n", army.id);
      }
*/
      insert_army_nation(np, &army, army.id);
/*      if (debug) {
	printf("Called insert_army_nation.\n");
      }
*/
    }
/*    if (debug) {
      printf("Calling insert_army_sector (%d,%d)\n", sp->loc.x, sp->loc.y);
    }
*/
    insert_army_sector(sp, &army);
  /*  if (debug) {
      printf("Called insert_army_sector.\n");
    }
*/
  }

  /* read list of owned locations (special case for first one) */
  if (np->n_sects > 0) {
    np->ptlist = (struct pt_list *) malloc(sizeof(struct pt_list));
    read_point_list(fp, np->ptlist);
/*    fread(np->ptlist, sizeof(struct pt_list), 1, fp); */
    /*    np->ptlist->pt = np->capital; */
    np->ptlist->next = NULL;
    pp = np->ptlist;
  }
  for (i = 1; i < np->n_sects; ++i) {
    pp->next = (struct pt_list *) malloc(sizeof(struct pt_list));
    read_point_list(fp, pp->next);
/*    fread(pp->next, sizeof(struct pt_list), 1, fp); */
    pp = pp->next;
    pp->next = NULL;
    if (debug >= 3) {
      putchar('s');
    }
  }
}

  /* reads in the list of all races */
void read_races()
{
  FILE *fp, *fopen();
  char s[200];
  int i, n_races;
  Srace tmp_race;		/* for temporary use */
  struct race_list *rlp;	/* to make the linked list */

  if ((fp = fopen(RACES_FILE, "r")) == NULL) {
    printf("cannot open races file.  you might have the wrong directory..\n");
    clean_exit();
    exit(1);
  }

    /* the initial race is the master, and is hardwired */
  races = (struct race_list *) malloc(sizeof(struct race_list));
  strcpy(races->race.name, "Master");
  races->race.mark = 'C';
  races->race.strength = 0;
  races->race.repro = 0;
  races->race.mortality = 0;
  races->race.intel = 0;
  races->race.speed = 0;
  races->race.stealth = 0;
  races->race.pref_alt = 0;
  races->race.pref_terrain = 0;
  races->race.pref_climate = 0;
  races->race.mag_apt = 0;
  races->race.farming = 0;
  races->race.mining = 0;
  races->next = NULL;

  rlp = races;

    /* now get the number of races from the file
       (we trust that the file is consistent)
     */
  fgets(s, 180, fp);
  while (s[0] == '#') {		/* ignore comments */
/*    if (debug) {
      printf("<%s>", s);
    }
*/
    fgets(s, 180, fp);
  }
/*  if (debug) {
    printf("<%s>", s);
  }
*/
  sscanf(s, "%d", &n_races);	/* first line has number of races */

  for (i = 0; i < n_races; ) { /* now read them in!! */
    fgets(s, 180, fp);
    s[strlen(s)-1] = '\0';
    if (s[0] != '#') {		/* skip comments */
      ++i;
      sscanf(s,
        "%s : %1s : %d : %d : %d : %d : %d : %d : %d : %d : %d : %d : %d : %d",
	     tmp_race.name, &tmp_race.mark, &tmp_race.strength,
	     &tmp_race.repro, &tmp_race.mortality, &tmp_race.intel,
	     &tmp_race.speed, &tmp_race.stealth, &tmp_race.pref_alt,
	     &tmp_race.pref_terrain, &tmp_race.pref_climate,
	     &tmp_race.mag_apt, &tmp_race.farming, &tmp_race.mining);

/*      if (debug) {
	show_race(&tmp_race);
      }
*/
        /* now that we have loaded it, add it to the list */
      rlp->next = (struct race_list *) malloc(sizeof(struct race_list));
      rlp = rlp->next;
      rlp->race = tmp_race;
      rlp->next = NULL;
    }
  }
  fclose(fp);
}

  /* this routine checks to see if the world file is
     in a compressed format.  It it is, it returns 1.
     Otherwise, 0.
   */
int check_compressed_world(char fname[])
{
  char Zname[200];
  FILE *fopen(),*fp;

  strcpy(Zname, fname);
  strcat(Zname, ".gz");		/* if .gz file exists, it must be compressed! */
  if ((fp = fopen(Zname, "r")) != NULL) {
    fclose(fp);
    return 1;
  }
  return 0;
}

  /* sets a master lock file */
void set_master_lock()
{
  close(creat("lock.master", 0600));
}

  /* sets a lock for the given nation id */
void set_lock(int id)
{
  char fname[PATHLEN], host[NAMELEN];
  FILE *fp, *fopen();
  extern Sworld world;
  extern int ruid;
  long now_secs;

  sprintf(fname, "lock.%d", id);
  if ((fp = fopen(fname, "w")) == NULL) {
    printf("cannot open the lock file file <%s> for writing\n", fname);
    clean_exit();
    exit(1);
  }
  now_secs = time(0L);
    /* now put some titbits of information into the lock file */
  fprintf(fp, "Nation %s; real uid %d; pid: %ld; time: %s", 
	world.nations[id].name, ruid, (long int) getpid(), asctime(localtime(&now_secs)));
  /* If you don't have networking, replace the following gethostname() line
     with something like: sprintf(host, "mycomputer"); */
  gethostname(host, NAMELEN);
  fprintf (fp, "machine: %s\n", host);
  fclose(fp);
}

  /* removes a lock for the given nation id */
void del_lock(int id)
{
  char fname[PATHLEN];

  sprintf(fname, "lock.%d", id);
  unlink(fname);
}

  /* removes the master lock file */
void del_master_lock()
{
  unlink("lock.master");
}

  /* tries to open the lock file.  returns the file pointer
     it gets, with the file open for reading (if it exists)
   */
FILE *is_locked(id)
     int id;
{
  FILE *fopen();
  char fname[PATHLEN];

  sprintf(fname, "lock.%d", id);
  return fopen(fname, "r");
}

  /* checks if even a single nation has a lock or not */
int is_any_lock()
{
  int i;
  FILE *fp;
  extern Sworld world;

  for (i = 0; i < world.n_nations; ++i) {
    if ((fp = is_locked(i)) != NULL) {
      fclose(fp);
      return 1;
    }
  }
  return 0;
}

/* cheks if there is a master lock file */
int is_master_lock()
{
  FILE *fp, *fopen();

  if ((fp = fopen("lock.master", "r")) != NULL) {
    fclose(fp);
    return 1;
  }
  return 0;
}

void set_update_time()
{
  char fname[PATHLEN];
  FILE *fp, *fopen();
  long now_secs;

  sprintf(fname, UPDATE_FILE);
  if ((fp = fopen(fname, "w")) == NULL) {
    printf("cannot open the update time file <%s> for writing\n", fname);
    clean_exit();
    exit(1);
  }
  now_secs = time(0L);
    /* now put some titbits of information into the lock file */
  fprintf(fp, "%ld; time: %s", now_secs, ctime(&now_secs));
  fclose(fp);
}

char *get_update_time()
{
  char fname[PATHLEN];
  FILE *fp, *fopen();
  char s[300], *rtvl, *tmp;

  sprintf(fname, UPDATE_FILE);
  if ((fp = fopen(fname, "r")) == NULL) {
    set_update_time();
  }
  if ((fp = fopen(fname, "r")) == NULL) {
    printf("cannot open update time file <%s> for reading\n", fname);
    clean_exit(1);
    exit(1);
  }
  fgets(s, 299, fp);
  if ((rtvl = (char *) malloc(strlen(s) * sizeof(char))) == NULL) {
    mem_error();
  }
  tmp = strchr(s,(int)':');
  strcpy(rtvl,(tmp+1));
  fclose(fp);
  return rtvl;
}

/* 
  If we can't allocate any more memory, then tell the user that's the
  case, and then die quietly, rather than the horrible death not 
  checking mallocs would cause.
*/
void mem_error()
{
  fprintf(stderr,"Error: Couldn't allocate requested memory");
  cleanup();
  clean_exit();
  exit(1);
}     

/* Loads various options form the options file into the user's option record */
void load_options(Snation *np)
{
  char opt_file[PATHLEN], opt_line[100];
  FILE *fopt;
  int len;

  init_options(np);
  sprintf(opt_file, "%s/opt.%d", OPT_DIR, np->id);
  if ((fopt = fopen(opt_file,"r")) == NULL) {
    save_options(np);
    return;
  }
  while (fgets(opt_line, 99, fopt) != NULL) {
    if (is_option(opt_line, "EXPERT_MODE: "))
    {
      np->opts->expert_mode = atoi(&(opt_line[13]));
      user.xmode = np->opts->expert_mode;
    } else if (is_option(opt_line, "CIV_MOVEMODE: ")) {
      np->opts->civ_movemode = atoi(&(opt_line[14]));
    } else if (is_option(opt_line, "NEWS_FORWARD: ")) {
      if (opt_line[14] == '\n') { 
        np->opts->news_forward = NULL;
        continue;
      }
      len = strlen(&(opt_line[14]));
      if ((np->opts->news_forward = (char *)malloc((len + 1)* sizeof(char)))
           == NULL) { mem_error(); }
      strcpy(np->opts->news_forward,&(opt_line[14]));
      len = strlen(np->opts->news_forward);
      if (np->opts->news_forward[len - 1] == '\n') {
        np->opts->news_forward[len - 1] = '\0';
      }
    } else if (is_option(opt_line, "MAIL_FORWARD: ")) {
      if (opt_line[14] == '\n') { 
        np->opts->mail_forward = NULL;
        continue;
      }
      len = strlen(&(opt_line[14]));
      if ((np->opts->mail_forward = (char *)malloc((len + 1)* sizeof(char)))
           == NULL) { mem_error(); }
      strcpy(np->opts->mail_forward,&(opt_line[14]));
      len = strlen(np->opts->mail_forward);
      if (np->opts->mail_forward[len - 1] == '\n') {
        np->opts->mail_forward[len - 1] = '\0';
      }
    } else if (is_option(opt_line, "MAIL_READER: ")) {
      if (opt_line[13] == '\n') { 
        np->opts->mail_reader = NULL;
        continue;
      }
      len = strlen(&(opt_line[13]));
      if ((np->opts->mail_reader = (char *)malloc((len + 1)* sizeof(char)))
           == NULL) { mem_error(); }
      strcpy(np->opts->mail_reader,&(opt_line[13]));
      len = strlen(np->opts->mail_reader);
      if (np->opts->mail_reader[len - 1] == '\n') {
        np->opts->mail_reader[len - 1] = '\0';
      }
    } else if (is_option(opt_line, "OPTS_EDITOR: ")) {
      if (opt_line[13] == '\n') { 
        np->opts->editor = NULL;
        continue;
      }
      len = strlen(&(opt_line[13]));
      if ((np->opts->editor = (char *)malloc((len + 1)* sizeof(char)))
           == NULL) { mem_error(); }
      strcpy(np->opts->editor,&(opt_line[13]));
      len = strlen(np->opts->editor);
      if (np->opts->editor[len - 1] == '\n') {
        np->opts->editor[len - 1] = '\0';
      }
    } else {
      fprintf(stderr,"Error: Bad Option %s\n",opt_line);
    }
  }
  fclose(fopt);
}

/* Save all of the user's optoins to a file. */
void save_options(Snation *np)
{
  FILE *fopt;
  char opt_file[PATHLEN];

  sprintf(opt_file, "%s/opt.%d", OPT_DIR, np->id);
  if ((fopt = fopen(opt_file,"w")) == NULL) {
    clean_exit();
    fprintf(stderr,"Error: Cannot Open Options File %s\n",opt_file);
    exit(1);
  }

  fprintf(fopt,"EXPERT_MODE: %d\n",np->opts->expert_mode);
  fprintf(fopt,"CIV_MOVEMODE: %d\n", np->opts->civ_movemode);
  if (np->opts->news_forward == NULL) {
    fprintf(fopt,"NEWS_FORWARD: \n");
  } else {
    fprintf(fopt,"NEWS_FORWARD: %s\n",np->opts->news_forward);
  }
  if (np->opts->mail_forward == NULL) {
    fprintf(fopt,"MAIL_FORWARD: \n");
  } else {
    fprintf(fopt,"MAIL_FORWARD: %s\n",np->opts->mail_forward);
  }
  if (np->opts->mail_reader == NULL) {
    fprintf(fopt,"MAIL_READER: \n");
  } else {
    fprintf(fopt,"MAIL_READER: %s\n",np->opts->mail_reader);
  }
  if (np->opts->editor == NULL) {
    fprintf(fopt,"OPTS_EDITOR: \n");
  } else {
    fprintf(fopt,"OPTS_EDITOR: %s\n",np->opts->editor);
  }
  fclose (fopt);
}

/* Init an unused option record */
void init_options(Snation *np)
{
  if ((np->opts = (Soptions *)malloc(sizeof(Soptions))) == NULL){mem_error();}
  
  np->opts->expert_mode = 0;
  np->opts->news_forward = NULL;
  np->opts->civ_movemode = 2; /* Move freely unless restricted by gov */
  np->opts->mail_forward = NULL;
  np->opts->mail_reader = NULL;
  np->opts->editor = NULL;
}

/* a hack to set some special races */
int assign_race(Snation *np, char racename[])
{
  struct race_list *rlp = races;

  while (rlp && (strcmp(rlp->race.name, racename) != 0)) {
    rlp = rlp->next;
  }
  if (!rlp) {
    return 0;
  }
  if (strcmp(rlp->race.name, racename) != 0) { /* did not find it */
    return 0;			/* failure */
  }
  printf ("%s ->> %s\n", np->name, rlp->race.name);  
np->race = rlp->race;
  np->farm_skill = np->race.farming;
  np->mine_skill = np->race.mining;

  return 1;			/* got it!! */
}

int extract_int_opt(string)
/* Pull an integer option out of a string. */
char *string;
{
  int rtvl = 0;
  char *pos = strchr(string, ':');

  pos++;
  sscanf(pos, "%d",&rtvl);
  
  return rtvl;
}

double extract_double_opt(string)
/* Pull an double option out of a string. */
char *string;
{
  double rtvl = 0.0;
  char *pos = strchr(string, ':');

  pos++;
  sscanf(pos, "%lf",&rtvl);
  
  return rtvl;
}

/* This loads global options into the global variables to be used in      */
/* various places.  This allows for different bubble constructions costs  */
/* for example. We case on the fist character, and then us strcmp to find */
/* out which particular option we're looking for.  New options should be  */
/* added under the appropriate case, or a case should be made for them.   */
void load_world_options()
{
  char opt_line[200];
  FILE *fopt;
  int did = 0;

  if (debug >= 1) {
    printf("Checking for world options in %s: ", WORLD_OPTIONS);
  }
  if ((fopt = fopen(WORLD_OPTIONS,"r")) == NULL) {
    if (debug >= 1) { printf("not found\n"); }
    return;
  }
  if (debug >= 1) { printf("found!\n"); }
  while (fgets(opt_line, 199, fopt)) {
    switch (opt_line[0]) {
      case 'A': {
        if (is_option(opt_line, "ARMY_SIGHT")) {
          ARMY_SIGHT = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "ARMY_OVERHEAD")) {
          ARMY_OVERHEAD = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'B': {
        if (is_option(opt_line, "BUBBLE_COST_METAL")) {
          BUBBLE_COST_METAL = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "BUBBLE_COST")) {
          BUBBLE_COST = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "BATTLE_INTENSITY")) {
          BATTLE_INTENSITY = extract_double_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'C': {
        if (is_option(opt_line, "CARAVAN_CAPACITY")) {
          CARAVAN_CAPACITY = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'D': {
        if (is_option(opt_line, "DISBAND_RETURN")) {
          DISBAND_RETURN = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "DESTROY_METAL_REC")) {
          DESTROY_METAL_REC = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "DESTROY_MONEY_COST")) {
          DESTROY_MONEY_COST = extract_double_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'E': {
        if (is_option(opt_line, "EAT")) {
          EAT = extract_double_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'F': {
        if (is_option(opt_line, "FORT_COST_MONEY")) {
          FORT_COST_MONEY = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "FORT_COST_METAL")) {
          FORT_COST_METAL = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "FORT_BONUS_INCREASE")) {
          FORT_BONUS_INCREASE = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "FOOD_WEIGHT")) {
          FOOD_WEIGHT = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "FOOD_PROD")) {
          FOOD_PROD = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'G': {
	if (is_option(opt_line, "GEOGRAPHY_BONUS")) {
          GEOGRAPHY_BONUS = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'H': {
        if (is_option(opt_line, "HOSPITAL_MAINT_COST")) {
          HOSPITAL_MAINT_COST = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'I': {
        if (is_option(opt_line, "INITIATION_JEWELS")) {
          INITIATION_JEWELS = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'J': {
        if (is_option(opt_line, "JEWEL_WEIGHT")) {
          JEWEL_WEIGHT = extract_double_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'L': {
        if (is_option(opt_line, "LAND_SIGHT")) {
          LAND_SIGHT = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'M': {
        if (is_option(opt_line, "MEN_PER_MACHINE")) {
          MEN_PER_MACHINE = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "MAGE_MOVE_FACTOR")) {
          MAGE_MOVE_FACTOR = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "MAGE_JEWELS_MAINT")) {
          MAGE_JEWELS_MAINT = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "MAG_JEWEL_FACTOR")) {
          MAG_JEWEL_FACTOR = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "MAG_MONEY_FACTOR")) {
          MAG_MONEY_FACTOR = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "METAL_WEIGHT")) {
          METAL_WEIGHT = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "MONEY_WEIGHT")) {
          MONEY_WEIGHT = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "MIN_STOP_CIV")) {
          MIN_STOP_CIV = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'O': {
        if (is_option(opt_line, "OCCUPYING_SOLDIERS")) {
          OCCUPYING_SOLDIERS = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'R': {
        if (is_option(opt_line, "REFINERY_FACT")) {
          REFINERY_FACT = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "ROADS_COST_METAL")) {
          ROADS_COST_METAL = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "ROADS_COST_MONEY")) {
          ROADS_COST_MONEY = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "REFINE_COST_METAL")) {
          REFINE_COST_METAL = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "REFINE_COST")) {
          REFINE_COST = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'S': {
        if (is_option(opt_line, "SORCERER_MOVE_FACTOR")) {
          SORCERER_MOVE_FACTOR = extract_int_opt(opt_line);
        } else if (is_option(opt_line, "SPY_SECRECY_FACTOR")) {
          SPY_SECRECY_FACTOR = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "SPY_MONEY_FACTOR")) {
          SPY_MONEY_FACTOR = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "SOLD_EAT_FACTOR")) {
          SOLD_EAT_FACTOR = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "SACRIFICED_FRACT")) {
          SACRIFICED_FRACT = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'T': {
        if (is_option(opt_line, "TECH_METAL_FACTOR")) {
          TECH_METAL_FACTOR = extract_double_opt(opt_line);
        } else if (is_option(opt_line, "TECH_MONEY_FACTOR")) {
          TECH_MONEY_FACTOR = extract_double_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'U': {
        if (is_option(opt_line, "UNIV_MAINT_COST")) {
          UNIV_MAINT_COST = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'V': {
        if (is_option(opt_line, "VAMPIRE_FRACT")) {
          VAMPIRE_FRACT = extract_double_opt(opt_line);
	} else { did = -1; }
        break;
      }
      case 'W': {
        if (is_option(opt_line, "WATER_SIGHT")) {
          WATER_SIGHT = extract_int_opt(opt_line);
	} else { did = -1; }
        break;
      }
      default: { did = -1; }
    }
    if (did == -1 ) {
      fprintf(stderr,"Error: Bad Option %s\n",opt_line);
    }
    did = 0;
  }
  fclose(fopt);
}

/* This reads an int from a file in a network independant fashion */
int read_int(FILE *fp, int *valp)
{
  long long_val;
  int ret_value;

  ret_value = fread(&long_val, sizeof(long_val), 1, fp);
  *valp = (int) ntohl(long_val);
  return ret_value;
}

/* This writes an int to a file in a network independant fashion */
int write_int(FILE *fp, int value)
{
  unsigned long send = htonl((long) value);

  return fwrite(&send, sizeof(unsigned long), 1, fp);
}

int read_short(fp, value)
/* This reads an int from a file in a network independent fashion */
FILE *fp;
unsigned short *value;
{
  int rtvl;

  rtvl = fread(value, sizeof(unsigned short), 1, fp);
  *value = ntohs(*value);
  return rtvl;
}

int write_short(fp, value)
/* This writes a short to a file in a network independant fashion */
FILE *fp;
unsigned short value;
{
  unsigned short send = htons(value);

  return fwrite(&send, sizeof(unsigned short), 1, fp);
}

/* This writes a diplo item to file */
void write_diplo(FILE *fp, Sdiplo *dm)
{
  Sdiplo send;

  send.self_id = htonl(dm->self_id);
  send.neighbor_id = htonl(dm->neighbor_id);
  send.status = htonl(dm->status);
  fwrite(&send, sizeof(Sdiplo), 1, fp);
}

/* This reads a diplo item from a file */
void read_diplo_item(FILE *fp, Sdiplo *dm)
{
  fread(dm, sizeof(Sdiplo), 1, fp);
  dm->self_id = ntohl(dm->self_id);
  dm->neighbor_id = ntohl(dm->neighbor_id);
  dm->status = ntohl(dm->status);
}

/* Convert from regular to network byte order */
void convert_point_to_net(Pt *to, Pt *from)
{
  to->x = htonl(from->x);
  to->y = htonl(from->y);
}

/* Convert from host to netowrk byte order */
void convert_cargo_to_net(Scargo *to, Scargo *from)
{
  to->money = htonl(from->money);
  to->metal = htonl(from->metal);
  to->jewels = htonl(from->jewels);
  to->food = htonl(from->food);
  to->people = htonl(from->people);
  to->army = htonl(from->army);
  convert_point_to_net(&(to->title), &(from->title));
}

/* Convert to host from network byte order */
void convert_point_to_host(Pt *to, Pt *from)
{
  to->x = ntohl(from->x);
  to->y = ntohl(from->y);
}

/* Convert to host from netowrk byte order */
void convert_cargo_to_host(Scargo *to, Scargo *from)
{
  to->money = ntohl(from->money);
  to->metal = ntohl(from->metal);
  to->jewels = ntohl(from->jewels);
  to->food = ntohl(from->food);
  to->people = ntohl(from->people);
  to->army = ntohl(from->army);
  convert_point_to_net(&(to->title), &(from->title));
}

/* This writes an army structure out to file */
int write_army(FILE *fp, Sarmy *ap)
{
  Sarmy send;

  strcpy(send.type,ap->type);
  strcpy(send.name,ap->name);
  send.n_soldiers = htonl(ap->n_soldiers);
  send.status = htonl(ap->status);
  send.id = htonl(ap->id);
  send.owner = htonl(ap->owner);
  send.mvpts = htonl(ap->mvpts);
  send.mvratio = htonl(ap->mvratio);
  send.flags = htonl(ap->flags);
  send.sp_bonus = htonl(ap->sp_bonus);
  send.money_maint = htonl(ap->money_maint);
  send.metal_maint = htonl(ap->metal_maint);
  send.jewel_maint = htonl(ap->jewel_maint);
  send.spell_pts_maint = htonl(ap->spell_pts_maint);
  convert_point_to_net(&(send.pos), &(ap->pos));
  convert_cargo_to_net(&(send.cargo),&(ap->cargo));
  send.next = ap->next;
  return fwrite(&send, sizeof(Sarmy), 1, fp);
}

/* This reads an army structure in from file */
int read_army(FILE *fp, Sarmy *ap)
{
  int rtvl;

  fread(ap, sizeof(Sarmy), 1, fp);
  ap->n_soldiers = ntohl(ap->n_soldiers);
  ap->status = ntohl(ap->status);
  ap->id = ntohl(ap->id);
  ap->owner = ntohl(ap->owner);
  ap->mvpts = ntohl(ap->mvpts);
  ap->mvratio = ntohl(ap->mvratio);
  ap->flags = ntohl(ap->flags);
  ap->sp_bonus = ntohl(ap->sp_bonus);
  ap->money_maint = ntohl(ap->money_maint);
  ap->metal_maint = ntohl(ap->metal_maint);
  ap->jewel_maint = ntohl(ap->jewel_maint);
  ap->spell_pts_maint = ntohl(ap->spell_pts_maint);
  convert_point_to_host(&(ap->pos), &(ap->pos));
  convert_cargo_to_host(&(ap->cargo),&(ap->cargo));
  return rtvl;
}

/* This writes a cargo out to file */
int write_cargo(FILE *fp, Scargo *cargo)
{
  Scargo send;

  convert_cargo_to_net(&send, cargo);
  return fwrite(&send, sizeof(Scargo), 1, fp);
}

/* This writes a cargo out to file */
int read_cargo(FILE *fp, Scargo *cargo)
{
  int rtvl;

  rtvl = fread(cargo, sizeof(Scargo), 1, fp);
  convert_cargo_to_host(cargo, cargo);
  return rtvl;
}

/* Writes a point out to file */
int write_point_list(FILE *fp, struct pt_list *plist)
{
  struct pt_list send;

  convert_point_to_net(&(send.pt), &(plist->pt));
  send.next = plist->next;
  return fwrite(&send, sizeof(struct pt_list), 1, fp);
}

/* Reads a point in from file */
void read_point_list(FILE *fp, struct pt_list *plist)
{
  fread(plist, sizeof(struct pt_list), 1, fp);
  convert_point_to_net(&(plist->pt), &(plist->pt));
}

#define GEOMULTCONST 1000000

/* This writes a struct geo to file.  Note the kludge for doubles. */
void write_geo(FILE *fp, Sgeo *geo)
{
  int x;

  write_int(fp,geo->topology);
  write_int(fp,geo->pwater);
  x = geo->metal_avg * GEOMULTCONST;  
  write_int(fp,x);
  x = geo->jewel_avg * GEOMULTCONST;  
  write_int(fp,x);
  x = geo->soil_avg * GEOMULTCONST;  
  write_int(fp,x);
}

/* This reads a struct geo from a file.  Note the kludge for doubles. */
void read_geo(FILE *fp, Sgeo *geo)
{
  int x;

  if (debug >= 1) {
    printf("\nReading geography");
  }
  read_int(fp, &geo->topology);
  read_int(fp, &geo->pwater);
  read_int(fp, &x);
  geo->metal_avg = (double)x / (double)GEOMULTCONST;  
  read_int(fp, &x);
  geo->jewel_avg = (double)x / (double)GEOMULTCONST;  
  read_int(fp, &x);
  geo->soil_avg = (double)x / (double)GEOMULTCONST;  
}

/* this writes a single sector out to file */
void write_sector(FILE *fp, Ssector *sp)
{
  Ssector send;

  convert_point_to_net(&send.loc, &sp->loc);
  send.terrain = htons(sp->terrain);
  send.altitude = htons(sp->altitude);
  send.climate = htons(sp->climate);
  send.designation = htons(sp->designation);
  send.soil = htons(sp->soil);
  send.metal = htons(sp->metal);
  send.jewels = htons(sp->jewels);
  send.defense = htons(sp->defense);
  send.roads = htons(sp->roads);
  send.owner = htons(sp->owner);
  send.n_people = htonl(sp->n_people);
  send.flags = htonl(sp->flags);
  send.alist = sp->alist;
  send.name = sp->name; /* Just to make sure it's null or not */
  fwrite(&send, sizeof(Ssector), 1, fp);
  if (sp->name != NULL) {
    fwrite(sp->name, strlen(sp->name)+1, 1, fp);
  }
}

/* This reads a sector from a file */
void read_sector(FILE *fp, Ssector *sp)
{
  char tmp_name[NAMELEN];
  char form_str[100];

  sprintf(form_str, "%%[%c-%c]", 1, 255);

  fread(sp, sizeof(Ssector), 1, fp);
  if (sp->name != NULL) {
    fscanf(fp, form_str, tmp_name);
    sp->name = malloc(strlen(tmp_name)+1);
    strcpy(sp->name, tmp_name);
    /* now discard the null char in the input file, since it was
       not read in, but was written out as part of the sector name.
     */
    (void) fgetc(fp);
  }
  convert_point_to_host(&sp->loc, &sp->loc);
  sp->terrain = ntohs(sp->terrain);
  sp->altitude = ntohs(sp->altitude);
  sp->climate = ntohs(sp->climate);
  sp->designation = ntohs(sp->designation);
  sp->soil = ntohs(sp->soil);
  sp->metal = ntohs(sp->metal);
  sp->jewels = ntohs(sp->jewels);
  sp->defense = ntohs(sp->defense);
  sp->roads = ntohs(sp->roads);
  sp->owner = ntohs(sp->owner);
  sp->n_people = ntohl(sp->n_people);
  sp->flags = ntohl(sp->flags);
}

/* Converts a race structure to net format */
void convert_race_to_net(Srace *to, Srace *from)
{
  strcpy(to->name, from->name);
  to->mark = from->mark;
  to->strength = htonl(from->strength);
  to->repro = htonl(from->repro);
  to->mortality = htonl(from->mortality);
  to->intel = htonl(from->intel);
  to->speed = htonl(from->speed);
  to->stealth = htonl(from->stealth);
  to->pref_alt = htonl(from->pref_alt);
  to->pref_terrain = htonl(from->pref_terrain);
  to->pref_climate = htonl(from->pref_climate);
  to->mag_apt = htonl(from->mag_apt);
  to->farming = htonl(from->farming);
  to->mining = htonl(from->mining);
}

/* Converts a race structure to host format */
void convert_race_to_host(Srace *to, Srace *from)
{
  strcpy(to->name, from->name);
  to->mark = from->mark;
  to->strength = ntohl(from->strength);
  to->repro = ntohl(from->repro);
  to->mortality = ntohl(from->mortality);
  to->intel = ntohl(from->intel);
  to->speed = ntohl(from->speed);
  to->stealth = ntohl(from->stealth);
  to->pref_alt = ntohl(from->pref_alt);
  to->pref_terrain = ntohl(from->pref_terrain);
  to->pref_climate = ntohl(from->pref_climate);
  to->mag_apt = ntohl(from->mag_apt);
  to->farming = ntohl(from->farming);
  to->mining = ntohl(from->mining);
}

void convert_const_to_net(to, from)
Sconstruction *to, *from;
{ 
  to->roads = htons(from->roads);
  to->forts = htons(from->forts);
  to->reserved = htons(from->reserved);
  to->flags = htonl(from->flags);
}

void convert_const_to_host(to, from)
Sconstruction *to, *from;
{ 
  to->roads = ntohs(from->roads);
  to->forts = ntohs(from->forts);
  to->reserved = ntohs(from->reserved);
  to->flags = ntohl(from->flags);
}

/* Write a nation structure out ot file */
void write_nation_struct(FILE *fp, Snation *np)
{
  Snation send;

  strcpy(send.name,np->name);
  strcpy(send.leader,np->leader);
  strcpy(send.passwd,np->passwd);
  strcpy(send.mag_order,np->mag_order);
  convert_point_to_net(&send.capital, &np->capital);
  convert_race_to_net(&send.race, &np->race);
  convert_const_to_net(&send.consts, &np->consts);
  send.mark = np->mark; /* a character */
  send.id = htonl(np->id);
  send.taxes = htonl(np->taxes);
  send.taxtype = htonl(np->taxtype);
  send.charity = htonl(np->charity);
  send.money = htonl(np->money);
  send.jewels = htonl(np->jewels);
  send.metal = htonl(np->metal);
  send.food = htonl(np->food);
  send.n_sects = htonl(np->n_sects);
  send.tech_r_d = htonl(np->tech_r_d);
  send.tech_r_d_metal = htonl(np->tech_r_d_metal);
  send.mag_r_d = htonl(np->mag_r_d);
  send.mag_r_d_jewels = htonl(np->mag_r_d_jewels);
  send.spy_r_d = htonl(np->spy_r_d);
  send.cn_flag = htonl(np->cn_flag);
  /* send.cn_agg = htonl(np->cn_agg);
  send.cn_exp = htonl(np->cn_exp);
  send.cn_iso = htonl(np->cn_iso); */
  send.tech_skill = htonl(np->tech_skill);
  send.mag_skill = htonl(np->mag_skill);
  send.farm_skill = htonl(np->farm_skill);
  send.mine_skill = htonl(np->mine_skill);
  send.spell_pts = htonl(np->spell_pts);
  send.combat_bonus = htonl(np->combat_bonus);
  send.spy = htonl(np->spy);
  send.secrecy = htonl(np->secrecy);
  send.n_armies = htonl(np->n_armies);
  send.cur_mag_r_d = htonl(np->cur_mag_r_d);
  send.cur_mag_r_d_jewels = htonl(np->cur_mag_r_d_jewels);
  send.cur_tech_r_d = htonl(np->cur_tech_r_d);
  send.cur_tech_r_d_metal = htonl(np->cur_tech_r_d_metal);
  send.cur_spy_r_d = htonl(np->cur_spy_r_d);
  send.armies = np->armies;
  send.ptlist = np->ptlist;
  send.opts = np->opts;
  send.cn_params = np->cn_params;
  fwrite(&send, sizeof(Snation), 1, fp);
}

/* Read a nation structure in from file */
void read_nation_struct(FILE *fp, Snation *np)
{
  int rtvl;

  rtvl = fread(np, sizeof(Snation), 1, fp);
  convert_point_to_host(&np->capital, &np->capital);
  convert_race_to_host(&np->race, &np->race);
  convert_const_to_host(&np->consts, &np->consts);
  np->id = ntohl(np->id);
  np->taxes = ntohl(np->taxes);
  np->taxtype = ntohl(np->taxtype);
  np->charity = ntohl(np->charity);
  np->money = ntohl(np->money);
  np->jewels = ntohl(np->jewels);
  np->metal = ntohl(np->metal);
  np->food = ntohl(np->food);
  np->n_sects = ntohl(np->n_sects);
  np->tech_r_d = ntohl(np->tech_r_d);
  np->tech_r_d_metal = ntohl(np->tech_r_d_metal);
  np->mag_r_d = ntohl(np->mag_r_d);
  np->mag_r_d_jewels = ntohl(np->mag_r_d_jewels);
  np->spy_r_d = ntohl(np->spy_r_d);
  np->cn_flag = ntohl(np->cn_flag);
  /* np->cn_agg = ntohl(np->cn_agg);
  np->cn_exp = ntohl(np->cn_exp);
  np->cn_iso = ntohl(np->cn_iso); */
  np->tech_skill = ntohl(np->tech_skill);
  np->mag_skill = ntohl(np->mag_skill);
  np->farm_skill = ntohl(np->farm_skill);
  np->mine_skill = ntohl(np->mine_skill);
  np->spell_pts = ntohl(np->spell_pts);
  np->combat_bonus = ntohl(np->combat_bonus);
  np->spy = ntohl(np->spy);
  np->secrecy = ntohl(np->secrecy);
  np->n_armies = ntohl(np->n_armies);
  np->cur_mag_r_d = ntohl(np->cur_mag_r_d);
  np->cur_mag_r_d_jewels = ntohl(np->cur_mag_r_d_jewels);
  np->cur_tech_r_d = ntohl(np->cur_tech_r_d);
  np->cur_tech_r_d_metal = ntohl(np->cur_tech_r_d_metal);
  np->cur_spy_r_d = ntohl(np->cur_spy_r_d);
}
