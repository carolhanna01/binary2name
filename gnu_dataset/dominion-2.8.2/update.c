 /* update.c -- update the dominion world */

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
#include <signal.h>
#include <math.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "dominion.h"
#include "misc.h"
#include "army.h"

extern Sworld world;
extern Suser user;
extern struct race_list *races;	/* list of races */
extern struct army_type *army_types; /* array of available armies */
extern Sh_spell *hanging_spells;
extern char libdir[];
extern int debug;
extern int (*wrapx)(), (*wrapy)();
FILE *mailfile;
extern struct s_desig_map desig_map[];
Suser *temp_users;
Sdiplo **allocate_diplo();
Sarmy *army_is_held();
void init_work_data(int xmax, int ymax);
void docargos();
void cn_moves(Snation *np);
void dospy(Snation *np, FILE *mailfile);
void domagic(Snation *np, FILE *mailfile);
void domoney(Snation *np);
void dometal(Snation *np);
void dojewels(Snation *np);
void dofood(Snation *np);
void docivilians(Snation *np);
void take_sectors();
void reset_armies();
void update_send_mail();
void clear_dead_hspells();
void clear_work_data();
void move_people_restrict(Snation *np);
void move_people_free(Snation *np);
int emp_desire(Snation *np, int x, int y);
void free_ptlist(struct pt_list *ptlist);


struct move_data
{
  int moved, adj;
  double des;
} **work_data;


int main(int argc, char *argv[])
{
  int i, c;
  Snation *np;
  extern char *optarg;
  extern int optind;
  char mailname[NAMELEN], passwd[PASSLEN], default_pass[NAMELEN];

  strcpy(libdir, DEF_LIBDIR);	/* default libdir */
  default_pass[0] = '\0';

  while ((c = getopt(argc, argv, "xp:d:--")) != EOF) {
    switch (c) {
    case 'x':
      debug++;
      break;
    case 'd':
      strcpy(libdir, optarg);
      break;
    case 'p':			/* allow user to give passwd on command line */
      strcpy(default_pass, optarg);
      break;
    }
  }

  if (debug >= 1) {
    printf("libdir=%s, def_libdir=%s\n", libdir, DEF_LIBDIR);
  }

  if (chdir(libdir) == -1) {
    fprintf(stderr,"Error: cannot cd to directory %s\n", libdir);
    clean_exit();
    exit(1);
  }

  /* how about asking if you want to remove it? -MDF */
  if (is_master_lock()) {
    fprintf(stderr, "There is a master lock\n");
    exit(1);
  }

  set_master_lock();
  if (is_any_lock()) {
    printf("There is a lock; you should see if the nation is still playing\n");
    clean_exit();
    exit(1);
  }

  printf("\n DOMINION UPDATE COMMENCING\n\n");

  set_update_time();
  SRND(time(0L));
  load_army_types();		/* it is important to load these in */
  load_spirit_types();		/* these must also be loaded */

  read_world(&world, WORLD_FILE);
  /* load master exec lines here in case there is a password change */
  load_master_execs();

  initial_diplo = allocate_diplo(world.n_nations);
  read_initial_diplo(initial_diplo, world.n_nations);
  get_crypt_pass("\nGamemaster password: ", passwd, NULL, default_pass);
  if (strcmp(world.nations[0].passwd, passwd)) {
    printf("\r\nTry again\r\n");
    get_crypt_pass("Gamemaster password: ", passwd, NULL, default_pass);
    if (strcmp(world.nations[0].passwd, passwd)) {
      printf("You must be a gamemaster to update the world!\n");
      exit(1);
    }
  }

  printf("Updating from thon %d to thon %d.\n", world.turn, world.turn+1);
  world.turn++; /* HAPPY NEW THON */

  /* Run through each nation doing calculations for money, etc. */
  /* this is a kludge:  we need the user struct for things like spells */

  init_work_data(world.xmax, world.ymax);
    /* you must load each nation before you can load the hanging
       spells, in case a user cast a spell on an army s/he just made
       this turn.
     */
  for (i=1; i<world.n_nations; ++i) {
    np = &world.nations[i];
    if (is_active_ntn(np)) {
      user.spirit_list = NULL;
      user.np = np;
      get_spirits(&user, np->mag_skill);
      load_nation(i, np);

      if (gets_mail(np)) {
	sprintf(mailname, "mail%d", i);
	mailfile = fopen(mailname, "a");
	fprintf(mailfile,"\n---------MESSAGE FROM UPDATE PROGRAM---------\n");
	fprintf(mailfile, "During the time between thon %d and thon %d...\n",
		 world.turn-1, world.turn);
	fclose(mailfile);
      } else {
	mailfile = NULL;
      }
    }
  }
  load_h_spells(NULL);
  docargos();			/* picks up cargos dropped by caravans */
  for (i = 1; i < world.n_nations; ++i) {
    np = &world.nations[i];
    if (is_active_ntn(np)) {

      user.id = i;
      user.np = &world.nations[i];

      /* start the user off with all spells s/he deserves.
         note that, because of spirits, this has to be done
         before load_nation(), since the spirit list is used
         in the exec parsing.  THIS MIGHT NOT BE TRUE ANY MORE.
         It still is true. moreso than ever -SHU
        */
      user.spell_list = NULL;
      user.spirit_list = NULL;
        /* prepare for sending them mail */
      if (gets_mail(np)) {
	sprintf (mailname, "mail%d", i);
	mailfile = fopen(mailname, "a");
      } else {
	mailfile = NULL;
      }
      printf("---> Updating nation %s (%d) %s\n", np->name, i,
	     (np->cn_flag)?"[cn]":" ");
      fflush(stdout);
      if (np->cn_flag) {
	cn_moves(np);		/* MDF: Put debug stuff here */
      }

      dotechno(np, mailfile);
      dospy(np, mailfile);
      domagic(np, mailfile);
      domoney(np);
      dometal(np);
      dojewels(np);
      dofood(np);
      docivilians(np);
      if (mailfile) {
        fclose (mailfile);
        mailfile = NULL;
      }
    }
  }
  dobattles();
  printf("Taking sectors...\n");
  take_sectors();
  printf("Updating diplomacy...\n");
  update_diplo();
  reset_armies();
  update_send_mail();
  post_statistics();
    /* clears the hanging spells before writing the world */
  clear_dead_hspells();
  remove_all_dead_hspells();
  clear_h_spells();
  write_h_spells();
  
  printf("Saving world data...\n");
  fflush(stdout);
  write_world(&world, WORLD_FILE);

  system("rm -f exec/exec*");

  printf("\n DOMINION UPDATE FINISHED\n\n");
    /* at the end, remove the cargo file */
    /* remove the master lock file */
  del_master_lock();

  return 0;
}

/* This will send mail to each nation that has a mail temp file. */
void update_send_mail()
{
  char mailname[200], receiver[200], mail_subject[200];
  int i;

  printf("Sending mail... "); fflush(stdout);

  sprintf(mail_subject,"Update to thon %d", world.turn);
  for (i = 1; i < world.n_nations; i++) {
    sprintf(mailname, "mail%d", i);
    mailfile = fopen(mailname, "r");
    if (mailfile) {
      fclose (mailfile);
      mailfile = NULL;
      sprintf(receiver,"%s of %s",
	      world.nations[i].leader, world.nations[i].name);
      if (debug >= 2) {
	printf("Sending mail to %s using file %s.\n",world.nations[i].name,
	       mailname);
	printf("Receiver => %s\n",receiver);
      }
        /* make sure we send no mail to CNs */
      if (gets_mail(&world.nations[i])) {
	if (mail_send(mailname, 0, i, mail_subject) > 0) {
	  fprintf(stderr,"Couldn't send mail to nation %d\n",i);
	}
      }
      unlink(mailname);
    }
  }
}


/* This function is called at the end of the update to change all armies
   that are in occupy mode to defend mode.  It also restores full move
   points to all armies.
 */
void reset_armies()
{
  Sarmy *ap, *next_ap, *holding_ap, *held_ap;
  struct army_type this_atype;
  extern struct army_type *army_types;
  struct spirit_type this_spirit_type;
  extern struct spirit_type *spirit_types;
  Snation *np;
  Ssector *sp;
  int i, j;
  FILE *mfile, *fopen();
  char mailfname[PATHLEN];

  printf("Resetting armies...\n");
  fflush(stdout);
  for(j = 0; j < world.n_nations; ++j) {
    np = &world.nations[j];
    sprintf(mailfname, "mail%d", j);
    mfile = fopen(mailfname, "a");
    ap = np->armies;
    for(i = 0; ap != NULL; ++i) {
      next_ap = ap->next;
      sp = &world.map[ap->pos.x][ap->pos.y];
      if (ap->status == A_OCCUPY) {
	ap->status = A_ATTACK;
      }
      ap->mvpts = army_move_rate(np, ap);
      if (is_uncontrolled(ap)) {
        ap->mvpts = 0;
      }
      /* make sure that armies are not too small for patrol/intercept */
      if ((ap->status == A_PATROL || ap->status == A_INTERCEPT)
	  && !can_patrol(ap)) {
	ap->status = A_ATTACK;
      }
      /* patrol and garrison move less */
      if (ap->status == A_PATROL || ap->status == A_GARRISON) {
	ap->mvpts /= 2;
      }
      if (ap->status == A_INTERCEPT) {
	ap->mvpts /= 4;
      }
        /* if they were in a fort, add some bonus */
      if (is_army(ap)) {
	if (sp->owner == np->id && sp->designation == D_FORT) {
	  ap->sp_bonus += FORT_BONUS_INCREASE;
	}
      }
      /* now come a few consistency checks to see if
	 army transports are in good order.
       */
      /* make sure that if an army was in transport, and its
	 transporter died, that the /T flag gets removed.
       */
      if (is_in_transport(ap) && !army_is_held(np, ap)) {
	if (debug >= 1) {
	  printf("Army %d (%s) in %s was orphaned; removing the /T flag\n",
		 ap->id, ap->name, np->name);
	}
	ap->flags &= ~AF_IN_TRANSPORT;
      }
      /* now see if the ship/caravan is in a different place (also bad) */
      holding_ap = army_is_held(np, ap);
      if (holding_ap && is_in_transport(ap) &&
	  (holding_ap->pos.x != ap->pos.x
	   || holding_ap->pos.y != ap->pos.y)) {
	if (debug >= 1) {
	  printf("Army %d (%s) in %s was away from its holder %d;\n",
		 ap->id, ap->name, np->name, holding_ap->id);
	  printf("Moving army %d to same place as army %d (%d, %d)\n",
		 ap->id, holding_ap->id, holding_ap->pos.x,
		 holding_ap->pos.y);
	}
	ap->pos = holding_ap->pos;
      }
      /* another check: if a caravan/navy is listed as having an army,
	 that army had better exist and be the right one.
       */
      if (is_cargo(ap) && ap->cargo.army != -1) {
	held_ap = get_army(np, ap->cargo.army);
	if (!held_ap) {		/* cannot find the army */
	  if (debug >= 1) {
	    printf("    Strange: army %d (%s) in %s has %d in cargo\n",
		   ap->id, ap->name, np->name, ap->cargo.army);
	    printf("    but that army seems to not exist (reset cargo)\n");
	  }
	  ap->cargo.army = -1;
	} else {		/* OK: the army is there */
	  if (!is_in_transport(held_ap)) {
	    if (debug >= 1) {
	      printf("    Strange: army %d (%s) in %s has %d in cargo\n",
		     ap->id, ap->name, np->name, ap->cargo.army);
	      printf("    but that army has no /T flag (setting it)\n");
	    }
	    held_ap->flags |= AF_IN_TRANSPORT;
	  }
	  if (held_ap->pos.x != ap->pos.x || held_ap->pos.y != ap->pos.y) {
	    if (debug >= 1) {
	      printf("    Strange: army %d (%s) in %s has %d in cargo\n",
		     ap->id, ap->name, np->name, ap->cargo.army);
	      printf("    but that army was in a different place\n");
	    }
	    /* this problem is actually fixed in another check */
	  }
	}
      }
      /* Make sure that sectors being carried are still owned. */
      if (is_cargo(ap)&&(ap->cargo.title.x != -1)&&(ap->cargo.title.y != -1)) {
        Ssector *sect = &world.map[ap->cargo.title.x][ap->cargo.title.y];

        if (sect->owner != ap->owner) {
	  if (debug >= 1) {
	    printf("Nation %d cargo %d title for %d,%d removed, not owned.\n",
		   ap->id,ap->owner,ap->cargo.title.x,ap->cargo.title.y);
	  }
          sect->flags &= ~SF_TRADED;
          ap->cargo.title.x = -1;
          ap->cargo.title.y = -1;
	}
      }

      /* make sure that armies and spirits have their permanent flags */
      if (is_army(ap)) {
	this_atype = army_types[army_type_index(ap->type)];
	ap->flags |= this_atype.flags;
      }
      if (is_spirit(ap)) {
	this_spirit_type = spirit_types[spirit_type_index(ap->type)];
	ap->flags |= this_spirit_type.flags;
      }
      if (is_mage(ap)) {
	ap->flags |= AF_WIZARD;
      }
        /* special handling of the INVERSE_ALT flag:  if they
	   are a land race, it becomes a WATER flag;  if they
	   are a water race, it becomes a LAND flag.
	 */
      if (ap->flags & AF_INVERSE_ALT) {
	if (np->race.pref_alt >= SEA_LEVEL) {
	  ap->flags |= AF_WATER;
	} else {
	  ap->flags |= AF_LAND;
	}
      }
      /* if they are underwater without the right flag, make them drown */
      if (!good_army_altitude(np, sp, ap)) {
	if (debug >= 1) {
	  printf("Army %d (%s) in %s %s.  x%d,y%d\n", ap->id, ap->name,
	       np->name, (sp->altitude < SEA_LEVEL) ? "drowned" : "suffocated",
	       ap->pos.x, ap->pos.y);
	}
	if (mfile) {		/* elaborate printing statement!! */
	  fprintf(mfile, "Your army %d (%s) %s.\n", ap->id, ap->name,
		  (sp->altitude < SEA_LEVEL) ? "drowned" : "suffocated");
	}
	delete_army_sector(sp, ap);
	delete_army_nation(np, ap);
      }

      ap = next_ap;
    }
    if (mailfile) {
      fclose(mailfile);
      mailfile = NULL;
    }
  }
  if (mfile) { fclose(mfile); }
}


/* This will check which sectors have armies in them in occupy mode.  If there
   are two armies in occupy mode, the one with the highest move ratio gets the
   sector...
  */
void take_sectors()
{
  int i, j, k, occuflag, availflag;
  Ssector *sp;
  int old_owner;		/* old owner of the sector that was taken */
  Sarmy *ap, *best, *get_army();
  int n_armies;			/* how many are in sector */
  struct armyid *alist;		/* to run through the sector's army list */
  char mailname[200], subj[200];
  Sdiplo **dm = allocate_diplo(world.n_nations);
  FILE *newsfp;

  read_in_diplo(dm, world.n_nations);
  
  for (i = 0; i < world.ymax; i++) /* check each sector for armies */
    for (j = 0; j < world.xmax; j++) {
      sp = &(world.map[j][i]);
      sp->flags &= ~SF_HOSTILE; /* Turn off the old hostility */
      if (sp->alist != NULL) { 	/* we have some armies in this sector */
	occuflag = 0;		/* is anyone trying to occupy this sector?  */
	availflag = 1;		/* is this sector available */
	best = NULL;		/* this keeps track of army with best ratio */
	alist = sp->alist;

	/* Now we'll run though each army in the sector and try to find      */
	/* which (if any) are attempting to occupy that sector.  An army can */
	/* only occupy a sector if it's status is OCCUPY, it has enough      */
	/* soldiers (OCCUPYING_SOLDIERS) and either the sector is unowned or */
	/* the nation which owns the sector is the foe of the army and the   */
	/* sector owner has no armies remaining in the sector.               */

	n_armies = sect_n_armies(sp);
	for (k = 0; k < n_armies; k++) {
	  ap = get_army(&world.nations[alist->owner], alist->id);

	  if (ap->status == A_OCCUPY && ap->n_soldiers >= OCCUPYING_SOLDIERS &&
	      (sp->owner == 0 || 
	       (sp->owner != 0 && 
		(get_diplo_status(dm,ap->owner,sp->owner) == WAR || 
		 get_diplo_status(dm,ap->owner,sp->owner) == JIHAD)))) {

	    if (best == NULL) best = ap; /* if first army checked, best yet */
	    occuflag = 1;	/* yes, someone's trying to get the sector */
	    if (ap->mvratio > best->mvratio) best = ap;
	  }

      /* If sector owner has an army remaining on it, nobody can take it */
      /* However if the army is uncontrolled, it will not prevent occupation */
	  if ((ap->owner == sp->owner) && !is_uncontrolled(ap)) {
	    availflag = 0;
	  }
	  alist = alist->next;	/* now get the next army */
	}

	if (occuflag && availflag) {
	  old_owner = sp->owner;
	  if (debug >= 2) printf("Winner is %s's army %d... ",
			     world.nations[best->owner].name, best->id);
	  if (best->owner != sp->owner) {
	    if (sp->owner != 0) subtsector(&world.nations[sp->owner], j, i);
	    addsector(&world.nations[best->owner], j, i);
            if (sp->n_people) {
            /* If there were any civilians in the sector */
              sp->flags |= SF_HOSTILE; /* It's hostile for 1 update */
	    }
	    if (gets_mail(&world.nations[old_owner]) && old_owner != 0) {
	      sprintf(mailname, "mail%d", old_owner);
	      mailfile = fopen(mailname, "a");
	    } else {mailfile = NULL; }
	    if (sp->designation == D_CAPITAL) {
	      take_capital(&(world.nations[old_owner]),
			   &(world.nations[best->owner]), mailfile);
	    } else {
	      if (mailfile) {
		fprintf(mailfile,
			"Sector %d, %d has been taken from you by %s!\n",
			xrel(j,i,world.nations[old_owner].capital),
			yrel(j,i,world.nations[old_owner].capital),
			world.nations[best->owner].name);
	      }
	    }
	    if (mailfile) {
	      fclose(mailfile);
              mailfile = NULL;
	    }
	    if (gets_mail(&world.nations[best->owner])) {
	      sprintf(mailname, "mail%d", best->owner);
	      if ((mailfile = fopen(mailname, "a")) == NULL) {
                fprintf(stderr,"Error: cannot append to %s\n",mailname);
                clean_exit();
                exit(1);
              }
	      fprintf(mailfile, "You successfully captured sector %d, %d!\n",
		      xrel(j,i,world.nations[best->owner].capital),
		      yrel(j,i,world.nations[best->owner].capital));
	      fclose(mailfile);
              mailfile = NULL;
	    }
	  }
	  else if (debug >= 2) printf("They already own that sector!\n");
	}
      }
    }
  strcpy(mailname, "sack_news");
  if ((newsfp = fopen(mailname, "r")) != NULL) {	/* have some sacks */
    strcpy(subj, "Capital sackings");
    post_news_file(mailname, NEWS_GROUP, subj, 0);
  }
  free_diplo(dm,world.n_nations);
}

/* nation np2 takes capital of nation np1.  mfile is mail file of nation
   np1.  If np1 has any cities, make the first one their new capital.
   1/2 of np1's metal, jewels, and money are transferred to np2.
 */
void take_capital(Snation *np1, Snation *np2, FILE *mfile)
{
  struct pt_list *ptlist, *bestpt;
  int done = 0, x = np1->capital.x, y = np1->capital.y;
  FILE *newsfp, *fopen();	/* to print this to the news */
  FILE *sacker;
  char *contents();
  char subj[100];

  printf("   ** %s has lost their capital to %s!\n", np1->name, np2->name);

  sprintf(subj, "mail%d", np2->id);
  if ((sacker = fopen(subj, "a")) == NULL) {
    fprintf (stderr, "Error opening file %s for writing\n", subj);
    return;
  }
  if ((newsfp = fopen("sack_news", "a")) == NULL) {
    fprintf(stderr, "Error opening sack_news file\n");
    return;
  }
  if (newsfp) {
    fprintf(newsfp,"Capital of %s was sacked by %s\n", np1->name,np2->name);
    fclose(newsfp);
  }    
  if (mfile) {
    fprintf(mfile, "\nYour capital was captured by %s!\n", np2->name);
  }
  ptlist = np1->ptlist;
    /* first we see if we can get another city for capital */
  while (ptlist != NULL && !done) {
    Ssector *sp;
    sp = &world.map[ptlist->pt.x][ptlist->pt.y];
    if (sp->designation == D_CITY && good_altitude(sp, np1)) {
      done = 1;
      world.map[ptlist->pt.x][ptlist->pt.y].designation = D_CAPITAL;
      break;
    }
    ptlist = ptlist->next;
  }
  if (done) {			/* found a replacement city */
    if (mfile) {
      fprintf(mfile, "Your city at %d,%d is your new capital\n",
	      xrel(ptlist->pt.x, ptlist->pt.y, np1->capital),
	      yrel(ptlist->pt.x, ptlist->pt.y, np1->capital));
    }
    world.map[ptlist->pt.x][ptlist->pt.y].designation = D_CAPITAL;
    np1->capital = ptlist->pt;
  } else {			/* np1 had no more cities!! */
    if (mfile) {
      fprintf(mfile, "You have no cities to put your new capital!\n");
    }

    ptlist = np1->ptlist;       /* give them the most populated sector */
    if (ptlist) {               /* if they have any left!! */
      bestpt = ptlist;
      while (ptlist != NULL) {
        if (world.map[ptlist->pt.x][ptlist->pt.y].n_people >
            world.map[bestpt->pt.x][bestpt->pt.y].n_people
	    && good_altitude(&world.map[ptlist->pt.x][ptlist->pt.y], np1)) {
          bestpt = ptlist;
	}
	ptlist = ptlist->next;
      }
      if (mfile) {
	fprintf(mfile, "I am putting your new capital at (%d,%d)\n",
		xrel(bestpt->pt.x, bestpt->pt.y, np1->capital),
		yrel(bestpt->pt.x, bestpt->pt.y, np1->capital));
      }
      np1->capital.x = bestpt->pt.x;
      np1->capital.y = bestpt->pt.y;
      world.map[bestpt->pt.x][bestpt->pt.y].designation = D_CAPITAL;
      done = 1;
    } else {			/* if you have no sectors... you die */
      done = 1; /* Done move the capital if nowhere to move it to. */
      if (mfile) {
	fprintf(mfile,"You have no sectors left. Your nation is destroyed.\n");
      }
      destroy_nation(np1->id);
      printf("*** nation %d (%s) is destroyed ***\n", np1->id, np1->name);
      done = 1;			/* don't try to move the capital */
    }
  }
  /* if we are not done then it means that the only sectors found were
     bad land/water type with no bubble; we use the first sector on the list.
   */
  if (!done) {
    np1->capital = np1->ptlist->pt;
    world.map[np1->capital.x][np1->capital.y].designation = D_CAPITAL;
    printf(" nation %d (%s) has no more %s sectors\n", np1->id, np1->name,
	   np1->race.pref_alt < SEA_LEVEL ? "water" : "land");
    printf("putting capital at (%d, %d)\n", np1->capital.x, np1->capital.y);
  }

  world.map[x][y].designation = D_CITY; /* old capital -> city (for new guy) */

  /* deplete nation */
  fprintf(sacker, "\nSacked capital of %s.  Gained:\n", np1->name);
  fprintf(sacker, "\t%s\n\n", contents(dom_max_int(np1->money/2, 0),
				       dom_max_int(np1->metal/2, 0),
				       dom_max_int(np1->jewels/2, 0),
				       dom_max_int(np1->food/2, 0), 0, -1, NULL, 0));
  fclose (sacker);

  np2->money += dom_max_int(np1->money/2, 0); /* don't acquire debts */
  np1->money = np1->money/2;
  np2->jewels += dom_max_int(np1->jewels/2, 0);
  np1->jewels = np1->jewels/2;
  np2->metal += dom_max_int(np1->metal/2, 0);
  np1->metal = np1->metal/2;
  np2->food += dom_max_int(np1->food/2, 0);
  np1->food =np1->food/2;				      
}

/* runs throught the list of sectors belonging to a nation
   and does things like reproduction and such.
 */
void docivilians(Snation *np)
{
  int current, born, died, tborn, tdied, rep, mort;
  Ssector *sp;
  struct pt_list *lp;

  rep  = np->race.repro;
  mort = np->race.mortality;
  tborn = tdied = 0;
  lp = np->ptlist;
  while (lp != NULL) {
    sp = &world.map[lp->pt.x][lp->pt.y];
    if (debug >= 3) {
      printf ("Doing population in %d,%d\n", lp->pt.x, lp->pt.y);
    }
    current = sp->n_people;
    born = (int) (current * rep  / 100.0);
    died = (int) (current * mort / 100.0);
    tborn += born;
    tdied += died;
    sp->n_people += born - died;
      /* they might suffocate or drown */
    if (sp->n_people > 0 && !good_altitude(sp, np)) {
      if (debug >= 1) {
	printf("%d people %s in sector (%d, %d)\n", sp->n_people,
	       (sp->altitude < SEA_LEVEL) ? "drowned" : "suffocated",
	       sp->loc.x, sp->loc.y);
      }
      if (mailfile) {
	fprintf(mailfile, "%d people %s in sector (%d, %d)\n", sp->n_people,
		(sp->altitude < SEA_LEVEL) ? "drowned" : "suffocated",
		xrel(sp->loc.x, sp->loc.y, np->capital),
		yrel(sp->loc.x, sp->loc.y, np->capital) );
      }
      sp->n_people = 0;
    }
/* Old civ movement code.  Has been replaced.
    for (i=sp->loc.x-1; i<=sp->loc.x+1; i++)
      for (j=sp->loc.y-1; j<=sp->loc.y+1; j++) { 
	if (world.map[(*wrapx)(i,j)][(*wrapy)(i,j)].owner == np->id)
	  movepeople(np, sp->loc.x, sp->loc.y, (*wrapx)(i,j), (*wrapy)(i,j));
      }
*/
    lp = lp->next;
  }
  clear_work_data();		/* Setup for moving */
  switch(np->opts->civ_movemode) {
    case 1: { move_people_restrict(np); break; }
    case 2: { move_people_free(np); break; }
    default: { /* the default (0) is to not move at all */; }
  }
  if (mailfile) {
    fprintf(mailfile,
	 "There were %d births and %d deaths, for a population change of %d\n",
	 tborn, tdied, tborn-tdied);
    fprintf(mailfile, "Your nation now has %d civilians\n", get_n_civil(np));
  }
}

/* This routine is outdated and should be removed */
void movepeople(Snation *np, int a, int b, int x, int y)
{
  int p1, p2, moved, des1, des2;

  if ((a != x || b != y) && world.map[x][y].owner == world.map[a][b].owner) {
/*    des1 = dom_max_int(0,sect_desire(np, a, b));
    des2 = dom_max_int(0,sect_desire(np, x, y));
*/
      /* desireability = % of people employed */
    des1 = emp_desire(np, a, b);
    des2 = emp_desire(np, x, y);
    p1 = world.map[a][b].n_people;
    p2 = world.map[x][y].n_people;

    moved = (p1*(100-des1))/400 - (p2*(100-des2))/400;
    moved = dom_min_int(moved, p1);
      /* hold it!! we cannot move people underwater without bubbles */
    if (!good_altitude(&world.map[a][b], np)
	|| !good_altitude(&world.map[x][y], np)) {
      moved = 0;
    }
    p2 += moved;
    p1 -= moved;
    world.map[a][b].n_people = p1;
    world.map[x][y].n_people = p2;
    if (debug >= 3) {
      printf ("Moved %d people from %d,%d to %d,%d\n", moved, a, b, x, y);
    }
  }
}

void domoney(Snation *np)
{
  int temp;

  temp = np->money;
  np->money = next_thon_money(np);

  if (mailfile) {
    fprintf(mailfile,
	    "Your net MONEY change is %d, bringing your total to %d\n",
	    np->money-temp, np->money);
  }
}


void dometal(Snation *np)
{
  int temp;

  temp = np->metal;
  np->metal = next_thon_metal(np);

  if (mailfile) fprintf(mailfile, "Your net METAL change is %d, bringing your total to %d\n", np->metal-temp, np->metal);
}

  /* calculates the new amount of jewels a nation has,
     and if it is negative, it kills off some mages
   */
void dojewels(Snation *np)
{
  int temp;
  Sarmy *ap, *get_first_mage();
  Ssector *sp;

  temp = np->jewels;

  np->jewels = next_thon_jewels(np);

  while (np->jewels < 0) {
    if ((ap = get_first_mage (np)) == NULL) {
      break;
    }
    sp = &world.map[ap->pos.x][ap->pos.y];
    printf ("\ndeleting mage %d\n", ap->id);

    if (mailfile) {
      fprintf (mailfile,
	       "\nYou lost your mage %d because you had no jewels...\n",
	       ap->id);
    }

    delete_army_sector (sp, ap);
    delete_army_nation (np, ap);
    np->jewels += MAGE_JEWELS_MAINT;
  }

  /* now we have the final data on jewels: report it to mail file */
  if (mailfile) {
    fprintf(mailfile,
	    "Your net JEWEL change is %d, bringing your total to %d\n",
	    np->jewels-temp, np->jewels);
  }
}


/* calculate the new amount of food.  also takes care of starving */
void dofood(Snation *np)
{
  int n_should_starve, n_starved, n_sold_starved;
  int oldfood, n_civil, n_sold, x;
  double starved_fraction;
  Ssector *sp;
  struct pt_list *ptlist = np->ptlist;
  Sarmy *ap, *next;		/* for army starvation */
  struct argument args[N_EXEC_ARGS];
  char s[EXECLEN];

  n_civil = get_n_civil(np);
  n_sold = get_n_soldiers(np);

  oldfood = np->food;
  np->food += calc_food(np) - calc_expend_food(np);
  if (mailfile) {
    if (oldfood == np->food) {
      fprintf(mailfile, "Your farmers produced as much food as was needed\n");
    } else if (oldfood < np->food) {
      fprintf(mailfile,	"Your farmers produced more food than was consumed\n");
      fprintf(mailfile, "Your food in storage increased %d tons, to %d\n",
	      np->food-oldfood, np->food);
    } else if (oldfood > np->food) {
      fprintf(mailfile,
	      "Your farmers did not produce enough to feed everyone\n");
      if (np->food >= 0) {
	fprintf(mailfile,
		"%d tons of food were used from storage, leaving %d tons\n",
		oldfood-np->food, np->food);
      } else {
	fprintf(mailfile, "Your remaining storage, %d tons, was used up\n",
		   oldfood);
      }
    }
  }
  if (np->food < 0) {
    if (debug >= 1) {
      printf("people starving in nation %s\n", np->name);
    }
    /* all those in cities who cannot eat starve */
    n_should_starve = - (int) (np->food / EAT);
    if (n_civil == 0) {
      starved_fraction = 0;
    } else {
      starved_fraction = (1.0*n_should_starve)/(1.0*get_n_civil(np));
    }
      
    if (starved_fraction > 1.0) {
      starved_fraction = 1.0;
    }
    n_starved = 0;
    while (ptlist != NULL) {
      sp = &world.map[ptlist->pt.x][ptlist->pt.y];
	n_starved += (int) (starved_fraction*sp->n_people);
	sp->n_people -= (int) (starved_fraction*sp->n_people);
	if (sp->n_people < 0) {
	  sp->n_people = 0;
	}
      ptlist = ptlist->next;
    }
    if (mailfile) {
      fprintf(mailfile,
      "%d people starve in your %d sectors, leaving %d surviving civilians.\n",
	       n_starved, np->n_sects, get_n_civil(np));
    }
      /* if there is still not enough food, starve soldiers */
    if (n_should_starve > n_civil) {
      n_sold_starved = (int) (n_should_starve-n_starved)/SOLD_EAT_FACTOR;
    } else {
      n_sold_starved = 0;
    }
    
    if (get_n_soldiers(np) == 0) {
      starved_fraction = 0;
    } else {
      starved_fraction = (1.0*n_sold_starved)/(1.0*get_n_soldiers(np));
    }
    if (starved_fraction > 1.0) {
      starved_fraction = 1.0;
    }
    if (n_sold_starved > 0) {
      if (mailfile) {
	fprintf(mailfile,
		"%d of your soldiers will starve too\n", n_sold_starved);
      }
      ap = np->armies;
      while (ap && n_sold_starved > 0) {
	x = starved_fraction*ap->n_soldiers;
	ap->n_soldiers -= x;
	n_sold_starved -= x;
	if (x > 0 && mailfile) {
	  fprintf(mailfile,"in army %d, %d soldiers starve\n", ap->id, x);
	}
	next = ap->next;
	if (ap->n_soldiers <= 0) {
	  sprintf(s, "ADISBAND:%d\n", ap->id);
	  parse_exec_line(s,args);
	  run_exec_line(np,args);
	}
	ap = next;
      }
    }
    np->food = 0;
  }
}

void cleanup()
{
  /* not much to do here */
}

void clean_exit()
{
  del_master_lock();
}

  /* reads in the cargo file, then it gives the recipient
     nation all the stuff.  if there is an army, it reads
     in that army and inserts it into the sector and nation
   */
void docargos()
{
  Scargo cargo;			/* the cargo that is being traded */
  int from_id, to_id;
  Snation *from_np, *to_np;
  char mailname[NAMELEN];
  FILE *fp, *fopen(), *mailf;
  Sarmy army;			/* was an army traded? */
  Ssector *sp;			/* on which sector does it happen? */
  int x, y;			/* location of the trade */
  char * contents ();    	/* returns string of cargo_contents */

  printf("Doing cargos...\n");
  if ((fp = fopen(CARGO_FILE, "r")) == NULL) {
    printf("No cargo file\n");
    return;
  }
  critical();
  while (read_int(fp, &x) > 0) {
    read_int(  fp, &y);
    read_int(  fp, &from_id);
    read_int(  fp, &to_id);
    from_np = &world.nations[from_id];
    to_np = &world.nations[to_id];
    read_cargo(fp,&cargo);
/*    fread(&cargo, sizeof(Scargo), 1, fp); */
      /* send mail to the donor, if it does not go to root */
    if (to_id != 0) {
      if (gets_mail(&world.nations[from_id])) {
	sprintf(mailname, "mail%d", from_id);
	mailf = fopen(mailname, "a");
      } else {mailf = NULL; }
      if (mailf) {
	fprintf(mailf,
		"Cargo given by %s to %s:\n", from_np->name, to_np->name);
	fprintf(mailf,"\tCargo=%s\n",
		contents (cargo.money, cargo.metal, cargo.jewels,
			  cargo.food, cargo.people, cargo.army,
			  &cargo.title, 0));
	fclose(mailf);
      }
        /* send mail to the recipient */
      if (gets_mail(&world.nations[to_id])) {
	sprintf(mailname, "mail%d", to_id);
	mailf = fopen(mailname, "a");
      } else {mailf = NULL; }
      if (mailf) {
	fprintf(mailf,
		"Cargo given by %s to %s:\n", from_np->name, to_np->name);
	fprintf(mailf,"\tCargo=%s\n",
		contents (cargo.money, cargo.metal, cargo.jewels,
			  cargo.food, cargo.people, cargo.army,
			  &cargo.title, 0));
	fclose(mailf);
	if (debug >= 2) {
	  printf("wrote to mail files about cargo\n");
	}
      }
    }
    if (debug >= 1) {
      printf("Got a cargo from nation %d to nation %d\n", from_id, to_id);
      printf("\tCargo=%s\n",
	     contents (cargo.money, cargo.metal, cargo.jewels,
		       cargo.food, cargo.people, cargo.army,
		       &cargo.title, 0));
    }
    fflush(stdout);
    to_np->money += cargo.money;
    to_np->metal += cargo.metal;
    to_np->jewels += cargo.jewels;
    to_np->food += cargo.food;
      /* put people in the sector */
    sp = &world.map[x][y];
    sp->n_people += cargo.people;
      /* if an army is there, read it in from the file */
    if (cargo.army != -1) {
      read_army(fp, &army);
/*      fread(&army, sizeof(Sarmy), 1, fp); */
      army.owner = to_np->id;
      army.flags &= ~AF_IN_TRANSPORT;
      insert_army_nation(to_np, &army, -1);
      ++to_np->n_armies;
      insert_army_sector(sp, &army);
    }
      /* if there is a sector, give it to the new nation */
    if (cargo.title.x != -1 && cargo.title.y != -1) {
      Ssector *sp = &world.map[cargo.title.x][cargo.title.y];
      if (subtsector(from_np, cargo.title.x, cargo.title.y) == 0) {
      /* Only add the sector if the nation still owned it */
        addsector(to_np, cargo.title.x, cargo.title.y);
      }
      sp->flags &= ~SF_TRADED;
    }
  }
  fclose(fp);
  unlink(CARGO_FILE);
  noncritical();
}

/* update the nation's spy values */
void dospy(Snation *np, FILE *mailfile)
{
  int stealth = np->race.stealth;

  np->spy += stealth * (sqrt(1.0 * np->spy_r_d * calc_revenue(np) / 100 +
			     np->cur_spy_r_d * np->money / 100)) *
			     SPY_MONEY_FACTOR;
  np->secrecy += (stealth*stealth) * 
    (sqrt (1.0*np->spy_r_d * calc_revenue(np) / 100 +
	   (np->cur_spy_r_d * np->money / 100)) * SPY_SECRECY_FACTOR);
}

  /* critical() for the update/make/add is different from the game */
void critical()
{
  signal(SIGINT, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);
}
void noncritical()
{
  signal(SIGINT, SIG_DFL);
  signal(SIGQUIT, SIG_DFL);
}

/* posts some news articles of general interest */
void post_statistics()
{
  FILE *tmp_fp, *fopen();
  char tmp_name[PATHLEN];
  char subj[100];
  int fd;

  /* a temporary file name for this news posting */
/*  tmp_name = tmpnam(NULL, "dominion"); */
  strcpy(tmp_name, "dominionXXXXXX");
  if ((fd = mkstemp(tmp_name)) == -1) {
    fprintf(stderr, "Error getting temp file name\n");
    return;
  }
  close(fd);
  if ((tmp_fp = fopen(tmp_name, "w")) == NULL) {
    fprintf(stderr, "Error opening file %s for writing\n", tmp_name);
    return;
  }
  sprintf(subj, "statistics at thon %d", world.turn);
  fprintf(tmp_fp, "\nThere are %d active nations.\n", get_n_act_ntn(&world));
  fprintf(tmp_fp, "Average population: %d\n", get_avg_civil(&world));
  fprintf(tmp_fp, "Average soldiers:   %d\n", get_avg_soldiers(&world));
  fprintf(tmp_fp, "Average treasury:   %d\n", get_avg_money(&world));
  fprintf(tmp_fp, "Average metal:      %d\n", get_avg_metal(&world));
  fprintf(tmp_fp, "Average jewels:     %d\n", get_avg_jewels(&world));
  fprintf(tmp_fp, "Average food:       %d\n", get_avg_food(&world));
  fprintf(tmp_fp, "Average sectors:    %d\n", get_avg_sectors(&world));
  fprintf(tmp_fp, "World occupation:   %d%% of land\n",
	  get_per_occu_land(&world));
  fprintf(tmp_fp, "                    %d%% of water\n",
	  get_per_occu_water(&world));
  fclose(tmp_fp);
  post_news_file(tmp_name, NEWS_GROUP,subj,0);
}

/*
   Allocate the memory for the temporary array the size of the world
   (with one extra for wrap around) for manipulation before scaling 
*/
void init_work_data(int xmax, int ymax)
{
  int i;
  
  if ((work_data = (struct move_data **) malloc((xmax) * 
                           (sizeof(struct move_data *)))) == NULL)
  {
    mem_error();
  }
  for (i=0; i <xmax ; i++)
  {
    if ((work_data[i] = (struct move_data *) malloc((ymax) * 
                                  (sizeof(struct move_data)))) == NULL)
    {
      mem_error();
    }
  }
}

/* Clear the work_peop array */
void clear_work_data()
{
  int x,y;

  for (x = 0 ; x < world.xmax ; x++)
  {
    for (y = 0 ; y < world.ymax ; y++)
    {
      work_data[x][y].moved = 0;
      work_data[x][y].adj = 0;
      work_data[x][y].des = 0.0;
    }
  }
}

#define MOVE_FREE 1

void move_people_free(Snation *np)
{
  struct pt_list *curr_pt;
  int tot_peop, tot_emp,x,y,moving,i,j,xx,yy;
  double race_factor, desire,divisor;
  Ssector *sp;

  race_factor = sqrt(world.nations[np->id].race.repro/10.0);
  curr_pt = np->ptlist;
/* Loop over every point the user has. */
  while (curr_pt != NULL)
  {
/* If the sector is too high, don't move people ther */
    if (world.map[curr_pt->pt.x][curr_pt->pt.y].altitude == MOUNTAIN_PEAK) {
      curr_pt = curr_pt->next;
      continue;
    }
    if (!good_altitude(&world.map[curr_pt->pt.x][curr_pt->pt.y],np)) {
      curr_pt = curr_pt->next;
      continue;
    }
    x = curr_pt->pt.x;
    y = curr_pt->pt.y; 
    sp = &(world.map[curr_pt->pt.x][curr_pt->pt.y]);
    work_data[x][y].des = get_max_employed(sp);
    desire = (double) sect_desire(np, x, y);
    /* Determine the desireability of the sector */
    desire = (desire - 650.0)/1000.0;
    /* And have that modify the % of people wanting to live there */
    work_data[x][y].des *= (1.0 + desire);
    for (i = 0 ; i < (MOVE_FREE * 2) + 1  ; i++) {
      for (j = 0 ; j < (MOVE_FREE * 2) + 1 ; j++) {
        x = (*wrapx)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
        y = (*wrapy)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
        if (world.map[x][y].owner == np->id)
        { 
          work_data[curr_pt->pt.x][curr_pt->pt.y].adj++;
	}
      }
    }
    curr_pt = curr_pt->next;
  }    
  curr_pt = np->ptlist;
  while (curr_pt != NULL)
  {
    tot_peop = 0;  /* Sum up the total owned people in a 1 sector radius */
    tot_emp = 0;  /* Sum up the total available employment in a 1 sect rad */ 
/* If the sector cannot suport civilians, then we don't move to or from it */
    if (!good_altitude(&world.map[curr_pt->pt.x][curr_pt->pt.y],np))
    {
      curr_pt = curr_pt->next;
      continue;
    }
/* If the sector is too high, don't move people ther */
    if (world.map[curr_pt->pt.x][curr_pt->pt.y].altitude == MOUNTAIN_PEAK) {
      curr_pt = curr_pt->next;
      continue;
    }
/* First loop over the neighboorhod and find totals */
    for (i = 0 ; i < (MOVE_FREE * 2) + 1  ; i++)
    {
      for (j = 0 ; j < (MOVE_FREE * 2) + 1 ; j++)
      {
        x = (*wrapx)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
        y = (*wrapy)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
        if (world.map[x][y].owner == np->id)
        { 
          /* Start with the number of people employable */
          tot_peop += world.map[x][y].n_people;
          tot_emp += work_data[x][y].des;
	}
      }
    }
/* Find the max divisor for this neighboorhood */
    divisor = 1.0;
    for (i = 0 ; i < (MOVE_FREE * 2) + 1  ; i++)
    {
      for (j = 0 ; j < (MOVE_FREE * 2) + 1 ; j++)
      {
        x = (*wrapx)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
        y = (*wrapy)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
        if (work_data[x][y].adj > divisor)
        {
          divisor = work_data[x][y].adj;
        }
      }
    }
/* Then distribute people over the neighboorhood. */
    for (i = 0 ; i < (MOVE_FREE * 2) + 1  ; i++)
    {
      for (j = 0 ; j < (MOVE_FREE * 2) + 1 ; j++)
      {
        x = (*wrapx)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
        y = (*wrapy)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
/* Again, only work with sectors owned.  No inter-national movement */
        if ((world.map[x][y].owner == np->id) && 
             good_altitude(&world.map[x][y],np))
        {
          if (tot_emp) { /* If there is somewhere to move to. */
/*
   The percentage of modified available employment times the number of
   people available is the number of people who want to be in that sector 
*/
            moving = (int)( (work_data[x][y].des/tot_emp) * tot_peop);
/* Then adjust for the people already in that sector */
            moving -= world.map[x][y].n_people;
	  } else {
            moving = 0;
	  }
/* 
  If we are working with a sector with N adjacent sectors, then we
  only move 1/N th of the people we'd like, since the other adjacent
  sectors may decide to all do the same thing.
*/
          moving =(int)((double)moving/(double)divisor);
/*
   And lastly put the people into the array stating that they will be moving.
   this is important since all moves are considered before they see where
   everybody else is moving.
*/
          work_data[x][y].moved += moving;
          work_data[curr_pt->pt.x][curr_pt->pt.y].moved -= moving;
	}
      }
    }
    curr_pt = curr_pt->next;
  }
/* Loop over all points again */
  curr_pt = np->ptlist;
  while (curr_pt != NULL)
  {
    x = curr_pt->pt.x;
    y = curr_pt->pt.y;
/* Now add all the movement of people to the state of the world */
    world.map[x][y].n_people += work_data[x][y].moved;
    work_data[x][y].moved = 0;
/* If we less than zero people (roundoff error happens) */
    if (world.map[x][y].n_people < 0) {
      if ((debug >= 1) || (world.map[x][y].n_people < -9)) {
        fprintf(stderr,"Error: Point %d, %d has %d people\n",x,y,
                       world.map[x][y].n_people);
      }
/* Look around the neighboorhood for a place to get the people from */
      for (i=0 ; (i < (MOVE_FREE*2)+1) && (world.map[x][y].n_people < 0); i++)
      {
        for (j=0 ;(j < (MOVE_FREE*2)+1) && (world.map[x][y].n_people < 0); j++)
        {
          xx=(*wrapx)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
          yy=(*wrapy)(i+(curr_pt->pt.x-MOVE_FREE),j+(curr_pt->pt.y-MOVE_FREE));
/* If they have people to spare */
          if (world.map[xx][yy].n_people + work_data[xx][yy].moved > 0)
          {
/* Then we move as many as we can, up to as many as we need */
            moving = dom_min_int(world.map[xx][yy].n_people
				 + work_data[xx][yy].moved,
				 -1 * world.map[x][y].n_people);
            world.map[xx][yy].n_people -= moving;
            world.map[x][y].n_people += moving;
	  }
	}
      }
    }
/* If we still have not enough people, then the algorithm is buggy */
    if (world.map[x][y].n_people < 0)
    {
       fprintf(stderr,"Error: Point %d, %d still has %d people\n",x,y,
                       world.map[x][y].n_people);
    }
    curr_pt = curr_pt->next;
  }
}

void fill_work_data(Snation *np)
{
  struct pt_list *curr_pt;
  double desire,race_factor;
  int x,y;
  Ssector *sp;

  race_factor = sqrt(world.nations[np->id].race.repro/10.0);
  curr_pt = np->ptlist;
  while (curr_pt != NULL)
  {
    if (!good_altitude(&world.map[curr_pt->pt.x][curr_pt->pt.y],np)) {
      curr_pt = curr_pt->next;
      continue;
    }
    x = curr_pt->pt.x;
    y = curr_pt->pt.y;
    sp = &(world.map[curr_pt->pt.x][curr_pt->pt.y]);
    work_data[x][y].des = get_max_employed(sp);
    if (sp->designation == D_NODESIG)
      work_data[x][y].des = 0.0;
/* the adj holds the number of jobs available in that sector */
    work_data[x][y].adj = work_data[x][y].des - world.map[x][y].n_people;
    desire = (double)sect_desire(np,x,y);
    /* Determine the desireability of the sector */
    desire = (desire - 650.0)/1000.0;
    /* And have that modify the % of people wanting to live there */
    work_data[x][y].des *= (1.0 + desire);
    curr_pt = curr_pt->next;
  }    
}

#define MOVE_RESTR 4

void move_the_people(Snation *np)
{
  struct pt_list *curr_pt;
  int x,y, xx,yy,i,j, moving;

  curr_pt = np->ptlist;
  while (curr_pt != NULL)
  {
    x = curr_pt->pt.x;
    y = curr_pt->pt.y;
/* Now add all the movement of people to the state of the world */
    world.map[x][y].n_people += work_data[x][y].moved;
    work_data[x][y].moved = 0;
/* If we less than zero people (roundoff error happens) */
    if (world.map[x][y].n_people < 0)
    {
      if ((debug >= 1) || (world.map[x][y].n_people < -9)) {
        fprintf(stderr,"Error: Point %d, %d has %d people\n",x,y,
                       world.map[x][y].n_people);
      }
/* Look around the neighboorhood for a place to get the people from */
      for (i=0;(i < (MOVE_RESTR*2)+1) && (world.map[x][y].n_people < 0); i++)
      {
        for (j=0;(j < (MOVE_RESTR*2)+1) && (world.map[x][y].n_people < 0); j++)
        {
          xx=(*wrapx)(i+(curr_pt->pt.x-MOVE_RESTR),
                                           j+(curr_pt->pt.y-MOVE_RESTR));
          yy=(*wrapy)(i+(curr_pt->pt.x-MOVE_RESTR),
                                           j+(curr_pt->pt.y-MOVE_RESTR));
/* If they have people to spare */
          if (world.map[xx][yy].n_people + work_data[xx][yy].moved > 0)
          {
/* Then we move as many as we can, up to as many as we need */
            moving = dom_min_int(world.map[xx][yy].n_people
				 + work_data[xx][yy].moved,
				 -1 * world.map[x][y].n_people);
            world.map[xx][yy].n_people -= moving;
            world.map[x][y].n_people += moving;
	  }
	}
      }
    }
/* If we still have not enough people, then the algorithm is buggy */
    if (world.map[x][y].n_people < 0)
    {
       fprintf(stderr,"Error: Point %d, %d still has %d people\n",x,y,
                       world.map[x][y].n_people);
    }
    curr_pt = curr_pt->next;
  }
}

void move_space_avail(Snation *np, struct pt_list *curr_pt,
		     int *peop, int tot_emp, int tot_peop)
{
  struct pt_list *nhbd_list,*curr_plist;
  int moving,i,j,x,y,peop_left = *peop;

  nhbd_list = NULL;
  for (i = 0 ; i < (MOVE_RESTR * 2) + 1 ; i++) {
    for (j = 0 ; j < (MOVE_RESTR * 2) + 1 ; j++) {
      x = (*wrapx) (i+(curr_pt->pt.x-MOVE_RESTR),
                                      j+(curr_pt->pt.y-MOVE_RESTR));
      y = (*wrapy) (i+(curr_pt->pt.x-MOVE_RESTR),
                                      j+(curr_pt->pt.y-MOVE_RESTR));
/* Again, only work with sectors owned.  No inter-national movement */
      if ((world.map[x][y].owner == np->id) && good_altitude(
           &world.map[x][y],np) && ((i != j) || (i != MOVE_RESTR))) {
         moving = (work_data[x][y].adj * tot_peop / (2 *tot_emp));
         if (moving < 0) { moving = 0; }
         peop_left -= moving;
         work_data[x][y].moved += moving;
         add_to_plist(&nhbd_list, x, y);
      }
    }
  }
  sort_ptlist(&nhbd_list);
  curr_plist = nhbd_list;
  while ((curr_plist != NULL) && (peop_left != 0))
  {
    x = curr_plist->pt.x;
    y = curr_plist->pt.y;
/* First figure out how many were moved here already */
    moving = (work_data[x][y].adj * tot_peop / (2 *tot_emp));
/* Then figure out how many more can move there. */
    moving = work_data[x][y].adj - moving;
    if (moving < 0 ) { moving = 0 ; }
    if (moving > peop_left) {
      moving = peop_left;
      peop_left = 0;
    } else {
      peop_left -= moving; 
    }
    work_data[x][y].moved += moving;
    curr_plist = curr_plist->next;
  }
  free_ptlist(nhbd_list);
  *peop = peop_left;
}

void move_somespace_avail(Snation *np, struct pt_list *curr_pt, int *peop)
{
  int i,j,x,y,moving,peop_left = *peop;

  for (i = 0 ; i < (MOVE_RESTR * 2) + 1 ; i++) {
    for (j = 0 ; j < (MOVE_RESTR * 2) + 1 ; j++) {
      x = (*wrapx)(i+(curr_pt->pt.x-MOVE_RESTR),j+(curr_pt->pt.y-MOVE_RESTR));
      y = (*wrapy)(i+(curr_pt->pt.x-MOVE_RESTR),j+(curr_pt->pt.y-MOVE_RESTR));
      if ((world.map[x][y].owner == np->id) && good_altitude(
           &world.map[x][y],np) && ((i != j) || (i != MOVE_RESTR))) {
         moving = work_data[x][y].adj;
         if (moving < 0 ) { moving = 0; }
         if (moving > peop_left ) {
           moving = peop_left;
           peop_left = 0; 
         } else {
           peop_left -= moving;
         }
         work_data[x][y].moved += moving;
      }
    }
  }
  *peop = peop_left;
}

void move_people_restrict(Snation *np)
{
  struct pt_list *curr_pt;
  int tot_peop, peop_left, tot_emp,x,y,moving,i,j;
  double race_factor, desire;

  race_factor = sqrt(world.nations[np->id].race.repro/10.0);
  fill_work_data(np);
/* Loop over every point the user has. */
  curr_pt = np->ptlist;
  while (curr_pt != NULL)
  {
    tot_emp = 0;  /* Sum up the total available employment in a 1 sect rad */ 
/* If the sector cannot suport civilians, then we don't move to or from it */
    if (!good_altitude(&world.map[curr_pt->pt.x][curr_pt->pt.y],np)) {
      curr_pt = curr_pt->next;
      continue;
    }
/* If the sector is too high, don't move people ther */
    if (world.map[curr_pt->pt.x][curr_pt->pt.y].altitude == MOUNTAIN_PEAK) {
      curr_pt = curr_pt->next;
      continue;
    }
/* If there are no unemployed people here, then go on. */
    if ((tot_peop = work_data[curr_pt->pt.x][curr_pt->pt.y].adj) >= 0) {
      curr_pt = curr_pt->next;
      continue;
    }
    tot_peop *= -1;
    peop_left = tot_peop;
    work_data[curr_pt->pt.x][curr_pt->pt.y].moved -= peop_left; 

/* First loop over the neighboorhod and find totals */
    for (i = 0 ; i < (MOVE_RESTR * 2) + 1  ; i++)
    {
      for (j = 0 ; j < (MOVE_RESTR * 2) + 1 ; j++)
      {
        x =(*wrapx)(i+(curr_pt->pt.x-MOVE_RESTR),j+(curr_pt->pt.y-MOVE_RESTR));
        y =(*wrapy)(i+(curr_pt->pt.x-MOVE_RESTR),j+(curr_pt->pt.y-MOVE_RESTR));
        if ((world.map[x][y].owner == np->id) && ((i!=j) || (i != MOVE_RESTR)))
        { 
          /* Start with the number of available jobs */
          if (work_data[x][y].adj > 0) {
            tot_emp += work_data[x][y].adj;
	  }
	}
      }
    }
    if (tot_emp > 0) {
      if (tot_emp > tot_peop) {
        move_space_avail(np,curr_pt,&peop_left,tot_emp,tot_peop);
      } else {
        move_somespace_avail(np,curr_pt,&peop_left);
      }
    }
    if (peop_left > 0) {
      tot_peop = peop_left; 
      desire = 0.0;
      for (i = 0 ; i < (MOVE_RESTR * 2) + 1 ; i++) {
        for (j = 0 ; j < (MOVE_RESTR * 2) + 1 ; j++) {
          x =(*wrapx)(i+(curr_pt->pt.x-MOVE_RESTR),
                                          j+(curr_pt->pt.y-MOVE_RESTR));
          y =(*wrapy)(i+(curr_pt->pt.x-MOVE_RESTR),
                                          j+(curr_pt->pt.y-MOVE_RESTR));
          if ((world.map[x][y].owner == np->id) && good_altitude(
               &world.map[x][y],np) && ((i != j) || (i != MOVE_RESTR))) {
            desire += work_data[x][y].des;
          }
        }
      }
      for (i = 0 ; i < (MOVE_RESTR * 2) + 1 ; i++) {
        for (j = 0 ; j < (MOVE_RESTR * 2) + 1 ; j++) {
          x =(*wrapx)(i+(curr_pt->pt.x-MOVE_RESTR),
                                          j+(curr_pt->pt.y-MOVE_RESTR));
          y =(*wrapy)(i+(curr_pt->pt.x-MOVE_RESTR),
                                          j+(curr_pt->pt.y-MOVE_RESTR));
          if ((world.map[x][y].owner == np->id) && good_altitude(
               &world.map[x][y],np) && ((i != j) || (i != MOVE_RESTR))) {
            moving = (int)((work_data[x][y].des * tot_peop)/desire);
            if (moving < 0 ) { moving = 0; } 
            peop_left -= moving;
            work_data[x][y].moved += moving;
          }
        }
      }
      work_data[curr_pt->pt.x][curr_pt->pt.y].moved += peop_left;
    }
    curr_pt = curr_pt->next;
  }
  move_the_people(np);
/* Loop over all points again */
}

void sort_ptlist(struct pt_list **ptlist)
{
  struct pt_list *pnext, *pprev, *pcurr, *ptemp;
  int switched;

  if (*ptlist == NULL) { return; }
  if ((*ptlist)->next == NULL) {return; }
  do {
    switched = 0;
    pcurr = *ptlist;
    pnext = pcurr->next;
    if (work_data[pcurr->pt.x][pcurr->pt.y].des <
                  work_data[pnext->pt.x][pnext->pt.y].des) {
      switched++;
      ptemp = pnext->next;
      *ptlist = pnext;
      (*ptlist)->next = pcurr;
      (*ptlist)->next->next = ptemp;
    }
    pprev = *ptlist;
    pcurr = (*ptlist)->next;
    while (pcurr->next != NULL) {
      pnext = pcurr->next;
      if (work_data[pcurr->pt.x][pcurr->pt.y].des <
                         work_data[pnext->pt.x][pnext->pt.y].des) {
        ptemp = pnext->next;
        pprev->next = pnext;
        pprev->next->next = pcurr;
        pprev->next->next->next = ptemp;
        switched++;
      }
      pprev = pprev->next;
      pcurr = pprev->next;
    }
  } while (switched > 0) ;
  return;
}

void free_ptlist(struct pt_list *ptlist)
{
  struct pt_list *tmp_pt;
  
  while (ptlist != NULL) {
    tmp_pt = ptlist->next;
    free(ptlist);
    ptlist = tmp_pt;
  }
}

void add_to_plist(struct pt_list **ptlist, int x, int y)
{
  struct pt_list *ptemp = *ptlist; 

  if (*ptlist == NULL) {
    if ((*ptlist = (struct pt_list *)malloc(sizeof(struct pt_list)))== NULL) {
      mem_error();
    }
    (*ptlist)->pt.x = x;
    (*ptlist)->pt.y = y;
    (*ptlist)->next = NULL;
    return;
  }
  while (ptemp->next != NULL) {
    ptemp = ptemp->next;
  }
  if ((ptemp->next = (struct pt_list *)malloc(sizeof(struct pt_list)))
            == NULL) { mem_error(); }
  ptemp = ptemp->next;
  ptemp->pt.x = x;
  ptemp->pt.y = y;
  ptemp->next = NULL;
  return;
}
    
void setup_user_arr(int num)
{
  if ((temp_users = (Suser *)malloc(num * sizeof(Suser))) == NULL) {
    mem_error();
  }
}

void copy_to_user_arr(int val)
{
  temp_users[val] = user;
}

void copy_from_user_arr(int val)
{
  user = temp_users[val];
}

/* retuns 1 if no ship/caravan in the nation is holding that given army */
Sarmy *army_is_held(np, ap)
     Snation *np;
     Sarmy *ap;
{
  Sarmy *alist;

  for (alist = np->armies; alist != NULL; alist = alist->next) {
    if (is_cargo(alist) && alist->cargo.army == ap->id) {
      return alist;		/* someone was holding this army!! */
    }
  }
  return NULL;			/* nobody had it; return true */
}
