/* battle.c -- functions relating to resolving battles between nations    */

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
#include <math.h>
#include <string.h>
#include <unistd.h>

#include "dominion.h"
#include "misc.h"
#include "army.h"

struct army_info {
  struct armyid *armyid;	/* pointer to army in sector list */
  int size;			/* actual size of the army */
  int bonus;			/* total bonus for this army */
  int force;			/* total force of this army */
  int against;			/* total enemy force against this army */
  int casualty;			/* casualties sustained by this army */
  struct army_info *next;	/* next army in sector */
  int mailsent;			/* has mail been sent to owner of this army? */
  int printed;			/* has this army been printed in this mail? */
};

/* some prototypes used here */
void move_intercepts(Sdiplo **dm);
int is_war(Sdiplo **dm, int x, int y);
void battle(Sdiplo **dm, int x, int y);
int against(Sdiplo **dm, struct armyid *list, Sarmy *intercept);
int are_enemies(Sdiplo **dm, int id1, int id2);
int are_treatied(Sdiplo **dm, int id1, int id2);
int total_bonus(Snation *np, Ssector *sp, Sarmy *ap, Sdiplo **dm);
int calc_casualty(int army1, int army2, double death_ratio);
void do_non_fighters(struct army_info *list, Ssector *sp);
void report_battles(struct army_info *head, Sdiplo **dm,
		    int x, int y, int metal_recovered);
void extract_losses(struct army_info *head, Ssector *sp);
void print_army_info(struct army_info *army, int mail_nation,
		     Sdiplo **dm, FILE *fp);
void print_news(struct army_info *army);
void reset_print_flags(struct army_info *head);
void print_army_summary(int men, int dead, FILE *fp);


extern int debug;
extern struct army_type *army_types;
extern Sworld world;
FILE *news_fp;


void dobattles()
{
  int x,y;
  Sdiplo **dm, **allocate_diplo();
  FILE *fopen();	/* for the news posting */
  char tmp_name[100];
  char subj[100];
  int had_battles = 0;
  int fd;

  dm = allocate_diplo(world.n_nations);
  read_in_diplo(dm,world.n_nations);

  /* a temporary file name for this news posting */
  strcpy(tmp_name, "dominionXXXXXX");
  if ((fd = mkstemp(tmp_name)) == -1) {
    fprintf(stderr, "Error getting temp file name\n");
    return;
  }
  close(fd);
  if ((news_fp = fopen(tmp_name, "w")) == NULL) {
    fprintf(stderr, "Error opening file %s for writing\n", tmp_name);
    return;
  }
  sprintf(subj,"battles from thon %d to thon %d",world.turn-1,world.turn);

  printf("Moving intercepts...\n"); fflush(stdout);
  move_intercepts(dm);
  printf("Doing battles...\n"); fflush(stdout);
  for (x = 0; x < world.xmax; x++) {
    for (y = 0; y < world.ymax; y++) {
      /* if there are two or more armies check for a battle and do it */
      if (world.map[x][y].alist && world.map[x][y].alist->next &&
	  is_war(dm, x, y)) {
	if (debug >= 1) {
	  printf("%03d,%03d ", x, y); fflush(stdout);
	}
	battle(dm, x, y);	/* do battles for this sector */
	had_battles = 1;
      }
    }
  }
  if (debug >= 1) {printf("\n");}
  fclose(news_fp);
  if (had_battles) {
    printf("Posting battles to \"%s\"...\n", NEWS_GROUP);
    fflush(stdout);
    post_news_file(tmp_name, NEWS_GROUP, subj, 0);
  }
  free_diplo(dm, world.n_nations);
}
/*
  takes all armies in intercept mode and sees if it can move them to
  attack an enemy army. (I should check this over. --MDF)
*/
void move_intercepts(Sdiplo **dm)
{
  struct position {
    int x,y,value; 
    };
  struct position pos;
  int nation,x,y,value;
  Snation *np;
  Ssector *sp;
  Sarmy *ap;
  
  pos.x=0;pos.y=0;pos.value=0;
  for (nation = 1; nation < world.n_nations; nation++) {
    np = &world.nations[nation];
    ap = np->armies;
    while (ap != NULL) {
      if (ap->status == A_INTERCEPT) {
/* 1st check sector army is in.  If there a force large enough to occupy here 
   then change status to attack and leave army here. */
	sp = &world.map[(*wrapx)(ap->pos.x,ap->pos.y)]
	  [(*wrapy)(ap->pos.x,ap->pos.y)];
	if (ap->status == A_INTERCEPT && (against(dm, sp->alist, ap) == 2)
                && (good_army_altitude(np, sp, ap) || is_kamikaze(ap))) {
          printf("%s army %d is intercepting but not moving\n",
                     np->name, ap->id); 
	  ap->status = A_ATTACK;
	}
	else {
/* 2nd loop through surrounding sectors and find sector with highest intercept
   value based on return from against and +1 if sector is owned 
   by intercepting army's nation.  Uses first sector it finds with highest
   value. */
	  pos.value=0;pos.x=0;pos.y=0;
	  for (x = ap->pos.x - 1; x <= ap->pos.x + 1; x++) {
	    for (y = ap->pos.y - 1; y <= ap->pos.y + 1; y++) {
	      sp = &world.map[(*wrapx)(x,y)][(*wrapy)(x,y)];
/*	      if (ap->status == A_INTERCEPT && against(dm, sp->alist, ap)
		&& (good_army_altitude(np, sp, ap) || is_kamikaze(ap))) { */
	      value = against(dm,sp->alist,ap);
	      if (( value > 0 ) && ( sp->owner == ap->owner))
		value=value+ 2;
	      if (value > pos.value) {
		pos.value = value;
		pos.x = x;
		pos.y = y;
	      }
	    }
	  }
	}
/* if pos.value is >0 then intercept to that sector */
	if (pos.value > 0 ) {
	  ap->status = A_ATTACK;
	  delete_army_sector(&world.map[ap->pos.x][ap->pos.y],ap);
	  ap->pos.x = (*wrapx)(pos.x,pos.y);
	  ap->pos.y = (*wrapy)(pos.x,pos.y);
	  sp = &world.map[(*wrapx)(pos.x,pos.y)][(*wrapy)(pos.x,pos.y)];
	  insert_army_sector(sp, ap);
          printf("%s army %d is intercepting from (%d,%d) to (%d,%d)\n",
		     np->name, ap->id, ap->pos.x, ap->pos.y, x, y); }

      }
      ap = ap->next;
    }
  }

}


/*
  against(): searches through a list of armies (a sector army list) and
  checks if any are at war/jihad with given army. Used for moving intercepts.
  Returns 2 if there is an enemy army >= OCCUPYING_SOLDIERS
  Returns 1 if there is an enemy army < OCCUPYING_SOLDIERS
  Returns the highest value for all the armies in the sector.
*/
int against(Sdiplo **dm, struct armyid *list, Sarmy *intercept)
{
  struct armyid *tmp;
  Sarmy *ap, *get_army ();	/* Also check if you can see the army... */
  Snation * np;
  int value;
  
  value = 0;
  tmp = list;
  while (tmp != NULL) {
    if (are_enemies(dm, tmp->owner, intercept->owner)) {
      np = &world.nations [tmp->owner];
      ap = get_army(np, tmp->id);
      if (is_hidden(ap) || is_underground(ap)) {
	;
      }
      else {
	 if (ap->n_soldiers >= OCCUPYING_SOLDIERS) {
	   value = 2;
	 }
	 else {
	   if (value < 2)
	     value = 1;
	 }
       }
    }
    tmp = tmp->next;
  }
  return value;
}


/* is there a war on this sector? */
int is_war(Sdiplo **dm, int x, int y)
{
  Sarmy *get_army();
  Ssector *sp = &world.map[x][y];
  Snation *np, *np2 = NULL;
  Sarmy *ap, *ap2;
  struct armyid *alist, *alist2;
  int iswar = 0;

  alist = sp->alist;

  /* for each army on the sector, see if they are at war with another
     army on the sector.  Ignore any uncontrolled spirit armies. */

  while (alist != NULL) {
    np = &world.nations[alist->owner];
    ap = get_army(np, alist->id);
    if (is_uncontrolled(ap)) {
      alist = alist->next;
      continue;
    }
    alist2 = sp->alist;
    while (alist2 != NULL) {
      np2 = &world.nations[alist2->owner];
      ap2 = get_army(np2, alist2->id);
      if (is_uncontrolled(ap2)) {
	alist2 = alist2->next;
	continue;
      }
      if (are_enemies(dm, alist2->owner, alist->owner)) {
	if (ap->status == A_OCCUPY || ap->status == A_ATTACK ||
	    ap2->status == A_OCCUPY || ap2->status == A_ATTACK) {
	  iswar = 1;
	  break;
	}
      }
      alist2 = alist2->next;
    }
    alist = alist->next;
  }
  return iswar;
}


/* Since for some reason are_allied doesn't work for me... */
int are_allied2(Sdiplo **dm, int id1, int id2)
{
  int ds1, ds2;

  ds1 = get_diplo_status (dm, id1, id2);
  ds2 = get_diplo_status (dm, id2, id1);

  if (ds1 >= ALLIED && ds2 >= ALLIED) {
    return 1;
  }
  return 0;
}


/* If the two nations are treatied */
int are_treatied(Sdiplo **dm, int id1, int id2)
{
  int ds1, ds2;

  ds1 = get_diplo_status(dm, id1, id2);
  ds2 = get_diplo_status(dm, id2, id1);

  if (ds1 == TREATY && ds2 == TREATY) {
    return 1;
  }
  return 0;
}


/* Checks if the two nations are enemies */
int are_enemies(Sdiplo **dm, int id1, int id2)
{
  int ds1, ds2;

  ds1 = get_diplo_status(dm, id1, id2);
  ds2 = get_diplo_status(dm, id2, id1);
  if (ds1 == WAR || ds1 == JIHAD || ds2 == WAR || ds2 == JIHAD) {
    return 1;
  }
  return 0;
}


/* total_bonus() returns all bonuses of an army */
int total_bonus(Snation *np, Ssector *sp, Sarmy *ap, Sdiplo **dm)
{
  int bonus = 0;
  int sbonus = 0;		/* Sector bonus */
  int temp;                     /* for terrain subsideray bonus's*/

  bonus += np->combat_bonus;	/* nation's combat bonus */
  bonus += ap->sp_bonus;	/* army's special bonus */
  temp = abs(np->race.pref_terrain - sp->terrain);
  temp = min (3, temp);      /* Worse than 3 off doesn't get worse */
  if (temp == 0) { temp = -1; }  /* Bonus for exact match */
  temp *= GEOGRAPHY_BONUS;
  bonus -= temp;             /* Take out terrain difficulty from bonus */
  temp = abs(np->race.pref_alt - sp->altitude);
  temp = min (3, temp);      /* Worse than 3 off doesn't get worse */
  if (temp == 0) { temp = -1; }  /* Bonus for exact match */
  temp *= GEOGRAPHY_BONUS;
  bonus -= temp;             /* Take out altitiude difficulty from bonus */
  if (sp->owner == np->id || are_allied2(dm, sp->owner, ap->owner)) {
    sbonus += sp->defense;	/* sector fortification bonus */
    sbonus += 20;		/* friendly sector bonus */
    switch (ap->status) {
    case A_GARRISON:		/* garrison special bonus */
      sbonus += 20;
      break;
    case A_PATROL:
	/* for now patrol gives no extra bonus; should it? */
      break;
    default:
      break;
    }
  } else {			/* Not allied with owner, just regular bonus */
    /* should we put something special here? */
  }
  if (are_treatied(dm, sp->owner, ap->owner) || sp->owner == ap->owner) {
    bonus += sbonus;		/* you get full sector bonus */
  } else if (are_allied2(dm, sp->owner, ap->owner)) {
    bonus += sbonus / 2;	/* only get half the bonus */
  }
  return bonus;
}


/* returns the effective force of an army after adjustments for bonuses */
int army_force(a, sp, dm)
     struct army_info *a;
     Ssector *sp;
     Sdiplo **dm;
{
  Snation *np;
  Sarmy *ap, *transport;
  struct armyid *p;
  int bonus, force;







  np = &world.nations[a->armyid->owner];
  ap = get_army(np, a->armyid->id);

  bonus = total_bonus(np, sp, ap, dm);

  if (is_in_transport(ap)) {	/* is this army on a caravan, ship, etc? */
    p = sp->alist;
    while (p) {			/* find the transport it is on */
      if (p->owner == ap->owner && p->id == ap->id) {
	transport = get_army(np, p->id);
	bonus += transport->sp_bonus; /* add the bonus of the caravan */
	break;
      }
      p = p->next;
    }
  }
  force = (int)(ap->n_soldiers * (1 + bonus / 100.0));
  return force;
}

/* Get the automatic anihilation ratio for the nation at that sector */
double get_death_ratio(np, sp, ap, dm)
     Snation *np;
     Ssector *sp;
     Sarmy *ap;
     Sdiplo **dm;
{
  double ratio;
  int bonus = 0;
  int sbonus = 0;

  bonus = abs(sp->altitude) * GEOGRAPHY_BONUS;
  /* Harder to wipe people out in mountains or deep trenches. */
  if ((sp->owner == ap->owner) || are_allied2(dm, sp->owner, ap->owner)) {
    sbonus += sp->defense;	/* sector fortification bonus */
    sbonus += 20;		/* friendly sector bonus */
  }
  if (are_treatied(dm, sp->owner, np->id) || (sp->owner == ap->owner)) {
    bonus += sbonus;		/* you get full sector bonus */
  } else if (are_allied2(dm, sp->owner, ap->owner)) {
    bonus += sbonus / 2;	/* only get half the bonus */
  }
  switch(sp->designation) {
  case D_CAPITAL:
    ratio = 5.55;
    break;
  case D_CITY:
    ratio = 5.10;
    break;
  case D_FORT:
    ratio = 4.65;
    break;
  case D_TOWN:
    ratio = 4.20;
    break;
  default:
    ratio = 3.75;
  }
  ratio = 1.0 / (ratio * (1.0 + ((double)bonus/200.0)));
  /* Automatic death ratio is the ratio listed, plus a percentage increase */
  /* for the sector related bonus's.  Then I invert ti so that it's listed */
  /* as 1/4 ratio rather than a 4/1 ratio.  */

  return ratio;
}


/* Do all battles in a sector */
void battle(Sdiplo **dm, int x, int y)
{
  struct army_info *head = NULL, *t1, *t2, *army;
  struct armyid *alist;
  double death_ratio;
  Snation *np;
  Sarmy *ap;
  Ssector *sp;
  int force_vs_us;	     /* the amount of an opponent that is vs. us */
  int oo_force;		     /* total force opposing an enemy of ours */
  int total_o_force;	     /* total force opposing us */
  int index;
  int metal_recovered = 0;   /* The total metal recovered off dead bodies. */

  sp = &world.map[x][y];
  alist = sp->alist;

  /* first make a new army list with the army_info structures */
  /* head is the beggining of this new list. */

  while (alist) {
    t1 = (struct army_info *) malloc(sizeof(struct army_info));
    t1->armyid = alist;
    t1->next = NULL;
    t1->casualty = 0;
    if (head == NULL) {		/* first one */
      head = t2 = t1;
    } else {
      t2->next = t1;
      t2 = t1;
    }
    alist = alist->next;
  }

  /* for each army in sector, calculate the opposing force and casualties */

  for (army = head; army != NULL; army = army->next) {
    np = &world.nations[army->armyid->owner];
    ap = get_army(np, army->armyid->id);
    if (is_uncontrolled(ap)) { continue; } /* don't calculate this one */
    total_o_force = 0;

    /* Go through sector army list looking at each enemy. */
    /* Add to total_o_force the force each enemy devotes to us */

    for (t1 = head; t1 != NULL; t1 = t1->next) {
      ap = get_army(&world.nations[t1->armyid->owner], t1->armyid->id);
      if (is_uncontrolled(ap)) { continue; } /* don't count this as an enemy */
      if (are_enemies(dm, army->armyid->owner, t1->armyid->owner)) {

	/* For each enemy of ours, find the total force opposing THEM. */
	/* The force they dedicate to us can then be computed into oo_force. */

	oo_force = 1;
	for (t2 = head; t2 != NULL; t2 = t2->next) {
	  ap = get_army(&world.nations[t2->armyid->owner], t2->armyid->id);
	  if (is_uncontrolled(ap)) { continue; } /* don't count as an enemy */
	  if (are_enemies(dm, t1->armyid->owner, t2->armyid->owner)) {
	    oo_force += army_force(t2, sp, dm);
	  }
	}
	force_vs_us = (army_force(t1, sp, dm)) *
	  (army_force(army, sp, dm)) / (oo_force);
	total_o_force += force_vs_us;
      } /* are_enemies */
    } /* for t1=head */
    np = &world.nations[army->armyid->owner];
    ap = get_army(np, army->armyid->id);
    army->size = ap->n_soldiers;
    army->bonus = total_bonus(np, sp, ap, dm);
    army->force = army_force(army, sp, dm);
    army->against = total_o_force;
    army->mailsent = army->printed = 0;
    death_ratio = get_death_ratio(np, sp, ap, dm);
    army->casualty = calc_casualty(army->force, army->against,death_ratio);
/* SHU Kluge Time */
/* This hard coded the information within army_force.  Bad plan. */
/*    army->casualty = (int)(army->casualty / (1 + army->bonus / 100.0)); */
/* This way we get the right number of _people_ casualties from the force */
/* casualties that would have been inflicted, even if other code changes. */
    army->casualty = (int) (((double)army->casualty/(double)army->force)
                     * (double)army->size);
    ap = get_army(np, army->armyid->id);
    if ( ((army->size - army->casualty) <= 5) || (ap->flags & AF_KAMIKAZE) ) {
      army->casualty = army->size;
    }
    if ((index = army_type_index(ap->type)) != -1) {
    /* if it's not a spirit */
      metal_recovered += army->casualty * army_types[index].metal_draft *
                       (DISBAND_RETURN * 2.0 / 3.0);
   }
  } /* for army=head */
  do_non_fighters(head, sp);
  report_battles(head, dm, x, y, metal_recovered);
  extract_losses(head, sp);

  /* free the army_info list */
  t1 = t2 = head;
  while (t1) {
    t2 = t1->next;
    free(t1);
    t1 = t2;
  }
  t1 = t2 = head = NULL;
}


/*
   This handles mages and caravans which only die if 85%
   of the surrounding force is killed.
*/
void do_non_fighters(struct army_info *list, Ssector *sp)
{
  struct army_info *p, *q;
  int nation_total, nation_cas;
  Snation *np;
  Sarmy *ap;

  /* go through the list of armies on the sector.  If an army is a cargo
     or a mage, calculate the total force of that nation and the total
     casualties of the nation.  If the casualties are > 85%, kill the
     army in question
     */
  for (p = list; p != NULL; p = p->next) {
    nation_total = nation_cas = 0;
    np = &world.nations[p->armyid->owner];
    ap = get_army(np, p->armyid->id);
    if (is_cargo(ap) || (!strcmp(ap->type, "Mage"))) { /* cargo or mage */
      for (q = list; q != NULL; q = q->next) {
	if (q->armyid->owner == p->armyid->owner) {
	  nation_total += q->size;
	  nation_cas += q->casualty;
	}
      } /* for loop */
      if (( (float)nation_cas / nation_total) >= 0.85) {
	p->casualty = p->size;
      } else {
	p->casualty = 0;
      }
    }
  } /* for loop */
}


/*
  Calculate the casualties to army1 in a battle between army1 and army2.
  These are just the force of the army.
*/
/*Replased by SHU.  In all many cases, this produces a non-sensical result.*/
int old_calc_casualty(army1, army2)
     int army1, army2;
{
  int larger_army;        /* Troops in the larger army. */
  int smaller_army;       /* Troops in the smaller army. */
  int larger_force;       /* Number of men in larger army squared. */
  int smaller_force;      /* Number of men in the smaller army squared. */
  double army_difference;  /* The square root of the difference in forces. */
  int larger_casualties;  /* casualties for the larger force. */
  int smaller_casualties; /* casualties for the smaller force. */

  /* Determine which army is larger. */

  larger_army = dom_max_int(army1, army2);
  smaller_army = dom_min_int(army1, army2);

  /* Calculate forces and army difference. */

  larger_force = larger_army * larger_army;
  smaller_force = smaller_army * smaller_army;

  army_difference = sqrt(larger_force - smaller_force);

  /* Find the actual casualties. */

  larger_casualties = (int) ( (float) (larger_army - army_difference) / 3.0 +
			     (float) smaller_army / 10.0 + 0.5);
  smaller_casualties = (int) ( (float) larger_force / (float) smaller_force *
			       (float) larger_casualties );

  if (dom_max_int(army1, army2) == army1) {
    return larger_casualties;
  } else {
    return smaller_casualties;
  }
}

/*
  Calculate the casualties to army1 in a battle between army1 and army2.
  These are just the force of the army.
*/
int calc_casualty(int army1, int army2, double death_ratio)
{
  int larger_army;        /* Troops in the larger army. */
  int smaller_army;       /* Troops in the smaller army. */
  double casualty_ratio;  /* amount of casualties lost from "winning" army */
  double adjustment;      /* Adjustment for cases less than 2-1 odds */
  int larger_casualties;  /* casualties for the larger force. */
  int smaller_casualties; /* casualties for the smaller force. */
  int extra_small;        /* extra damage to small army based on intensity */
  int extra_large;        /* extra damage to large army based on intensity */

  /* Determine which army is larger. */

  larger_army = dom_max_int(army1, army2);
  smaller_army = dom_min_int(army1, army2);

/* 
  Calculate the ratio of forces lost, related to the sqare of the
  ratio of the respective forces.  If it's lower than 2-1, adjust.
*/
   
  casualty_ratio = (double)smaller_army/(double)larger_army;
  if ((casualty_ratio < death_ratio) && (dom_max_int(army1,army2) == army2)) {
  /* If it's an automatic victory situation */
    return smaller_army;
  }
  casualty_ratio *= casualty_ratio;
  if (casualty_ratio > 0.25) {
    /* Linear tranformation for odds less than 2 - 1 */
    /* Make sure that even odds is 50% loss to both */
    adjustment = (2.0/3.0 * casualty_ratio) - 1.0/6.0;
  } else {
    adjustment = 0; 
  }
  casualty_ratio -= adjustment;

  /* Find the actual casualties. */
  /*
     Note the larger army takes the square of the ratio (adjusted), and
     the smaller army takes the arithmatic inverse of that (1.0 - value).
     Example: I outforce my opponent 2 to 1.  Therefor I loose 1/4 of
     my troops (1/2) ^2 and he looses 3/4's of his (1.0 - 1/4).
  */ 

  /*
    The BATTLE_LENGTH factor is meant to account for the length that
    a battle lasts (how long a thon is.)  At 0.0 you'll have a sum of
    100% casualties (at 1-1 odds, 50% each)  at 1.0 the battle continues
    until one side is completely eliminated.  It's suggested that you
    have a value between 0.0 and 0.3, as higher values makes it
    impossible to defend a sector.
  */

  larger_casualties = (int)((double)larger_army * casualty_ratio);
  smaller_casualties = (int) ((double)smaller_army * (1.0 - casualty_ratio));

/*
   The battle intensity variable modifies the number of people left
   alive on the smaller side.  This means that if it is 2.0, then only
   half of the number of survivors on the smaller side that would have
   been alive under normal rules are actually alive.
   The larger army looses a proportional number of casualties.  That
   means if they were looking 1 man to every 3 of the smaller army,
   and the smaller army looses another 30, then the large looses
   another 10.  In actuality it would be lower than this, but this
   estimates in favor of the looser of the battle.
*/

  if (smaller_casualties) {
    extra_small = (smaller_army - smaller_casualties);
    extra_small = (int)((double)extra_small / BATTLE_INTENSITY);
    extra_small = (smaller_army - smaller_casualties) - extra_small;
    extra_large = (int)(((double)extra_small / (double) smaller_casualties) *
       (double)larger_casualties);
  } else {
    extra_large = 0;
    extra_small = 0;
  }

  if (debug) {
 printf("large %d small %d rat %g large %d %g small %d %g extra %d %d %g %g\n",
      larger_army, smaller_army, (double)larger_army/(double)smaller_army,
      larger_casualties,(double)larger_casualties/(double)larger_army,
      smaller_casualties,(double)smaller_casualties/(double)smaller_army,
      extra_small, extra_large, BATTLE_INTENSITY, death_ratio);
  } 

  smaller_casualties += extra_small;
  larger_casualties += extra_large;

  if (dom_max_int(army1, army2) == army1) {
    return larger_casualties;
  } else {
    return smaller_casualties;
  }
}

/* send mail to nations involved in a battle.  Send news. */
void report_battles(struct army_info *head, Sdiplo **dm,
		    int x, int y, int metal_recovered)
{
  struct army_info *army, *t1, *t2;
  Ssector *sp;
  Snation *np;
  Sarmy *ap;
  char fname[NAMELEN];
  FILE *fp;
  int overall_dead, overall_men, single_recovered;
  int owner_dead, owner_men;
  int total_dead, total_men;	/* total dead for a nation, total fighters */
  int owner;			/* owner of current army */
  int done_news = 0;

  sp = &world.map[x][y];

  fprintf(news_fp, "\n----------------------------------------------------\n");
  fprintf(news_fp, "Battle reported in %s\n", world.nations[sp->owner].name);
  fprintf(news_fp, "  Participants:");

  overall_dead = 0; overall_men = 0;
  owner_dead = 0; owner_men = 0;

  /* go through each army on the sector.  Open mail to that nation.
     Find all armies of that nation and print info on them.  Mark as
     mailsent. For each other nation, print their armies and mark them
     as printed.
     */
  for (army = head; army != NULL; army = army->next) {
    if (army->mailsent) { continue; } /* skip armies we've already done */
    owner = army->armyid->owner;
    np = &world.nations[owner];
    ap = get_army(np, army->armyid->id);
    sprintf(fname, "mail%d", owner);
    fp = fopen(fname, "a");
    if (!fp) {
      fprintf(stderr, "Error in battle.c: Can't open mail file\n");
      exit(1);
    }
    fprintf(fp, "----------------------------------------"); /* 75 dashes */
    fprintf(fp, "-----------------------------------\n");
    fprintf(fp, "Battle reported in sector %d,%d [owner: %s]\n",
	   xrel(x, y, np->capital), yrel(x, y, np->capital),
	   world.nations[sp->owner].name);
    fprintf(fp, "  Your armies:\n");
    total_dead = total_men = 0;

    /* go through list and print armies owned by this nation */
    for (t1 = head; t1 != NULL; t1 = t1->next) {
      if (t1->armyid->owner != owner) { continue; }
      print_army_info(t1, army->armyid->owner, dm, fp);
      total_men += t1->size;
      total_dead += t1->casualty;
      if (!done_news) { print_news(t1); }
      t1->mailsent = t1->printed = TRUE;
    }
    owner_men = total_men;
    owner_dead = total_dead;
    overall_dead = total_dead;
    overall_men = total_men;
    print_army_summary(total_men, total_dead, fp);
    fprintf(fp, "\n  Participants:");

    /* Go through list and find an army who's nation has not been done
       yet.  Print all armies of that nation. */
    for (t1 = head; t1 != NULL; t1 = t1->next) {
      if (t1->printed == TRUE) { continue; } /* already did this one */
      owner = t1->armyid->owner;
      fprintf(fp, "\n    %s", world.nations[owner].name);
      if (are_enemies(dm, army->armyid->owner, owner)) {
	fprintf(fp, " [enemy]:\n");
      } else if (are_allied2(dm, army->armyid->owner, owner)) {
	fprintf(fp, " [ally]:\n");
      } else {
	fprintf(fp, " [neutral]:\n");
      }
      total_dead = total_men = 0;
      for (t2 = t1; t2 != NULL; t2 = t2->next) {
	if (t2->armyid->owner != owner) { continue; } /* not our army */
	print_army_info(t2, army->armyid->owner, dm, fp);
	total_men += t2->size;
	total_dead += t2->casualty;
	if (!done_news) { print_news(t2); }
	t2->printed = TRUE;
      }
      overall_dead += total_dead;
      overall_men += total_men;
      print_army_summary(total_men, total_dead, fp);
    }
    if ((owner_men - owner_dead) > 0) { /* mutual anihilation exception */
      single_recovered =  (int) ((double)metal_recovered * 
        ((double)(owner_men - owner_dead)/(double)(overall_men-overall_dead)));
      if (single_recovered) {
        fprintf(fp,"\nTotal Metal Recovered %d\n",single_recovered);
        world.nations[owner].metal += single_recovered;
      }
      if (debug) {
        fprintf(fp,"single %d owner %d %d ratio %g overall %d %d total %d\n",
          single_recovered, owner_men, owner_dead, (double)owner_dead/
          (double)overall_dead, overall_men, overall_dead, metal_recovered);
      }
    }
    reset_print_flags(head);
    fclose(fp);
    done_news = TRUE;
  }
}


/* Print_news takes an army_info structure and prints a message to the
   news file with minimal info on that army.
*/
void print_news(struct army_info *army)
{
  Snation *np;
  Sarmy *ap;

  np = &world.nations[army->armyid->owner];
  ap = get_army(np, army->armyid->id);

  if (!strcmp(ap->name, ap->type)) {
    fprintf(news_fp, "\n\t%s: unnamed army", np->name);
  } else {
    fprintf(news_fp, "\n\t%s: Army \"%s\"", np->name, ap->name);
  }
}

void print_army_summary(int men, int dead, FILE *fp)
{
  fprintf(fp, "\tTotal soldiers: %d   Total casualties: %d   Loss: %d%%\n",
	  men, dead, (int)((float)dead / men * 100));
}


/* This prints info on an army in a battle to a mailfile.
   The nation who is receiving the mail is given, so that the
   function knows whether to print army id numbers or not.
*/
void print_army_info(struct army_info *army, int mail_nation,
		     Sdiplo **dm, FILE *fp)
/*   army:	    the army to print info about */
/*   mail_nation:   nation we are sending mail to */
/*   dm:            diplo matrix */
/*   fp:            mail file */
{
  Sarmy *ap;
  int print_name;		/* should we print the name of the army? */
  int print_number;		/* should we print the army id number? */
  int wipe_out;			/* was the army totally destroyed? */

  ap = get_army(&world.nations[army->armyid->owner], army->armyid->id);
  print_name = strcmp(ap->name, ap->type);
  print_number = (mail_nation == army->armyid->owner);
  wipe_out = (army->size == army->casualty);

  if (wipe_out) {
    if (print_number) {
      if (print_name) {
	fprintf(fp, "\tArmy \"%s\" (#%d), %d %s (bonus %d), was wiped out.\n",
		ap->name, ap->id, ap->n_soldiers, ap->type, army->bonus);
      } else {
	fprintf(fp, "\tArmy #%d, %d %s (bonus %d), was wiped out.\n",
		ap->id, ap->n_soldiers, ap->type, army->bonus);
      }
    } else {			/* don't show id # */
      if (print_name) {
	fprintf(fp, "\tArmy \"%s\", %d %s (bonus %d), was wiped out.\n",
		ap->name, ap->n_soldiers, ap->type, army->bonus);
      } else {
	fprintf(fp, "\t%d %s (bonus %d) were wiped out.\n", ap->n_soldiers,
		ap->type, army->bonus);
      }
    }
  } else {			/* army not wiped out... */
    if (print_number) {
      if (print_name) {
	fprintf(fp,
	   "\tArmy \"%s\" (#%d), %d %s (bonus %d), sustained %d casualties.\n",
		ap->name, ap->id, ap->n_soldiers, ap->type, army->bonus,
		army->casualty);
      } else {
	fprintf(fp,"\tArmy #%d, %d %s (bonus %d), sustained %d casualties.\n",
		 ap->id, ap->n_soldiers, ap->type, army->bonus,
		 army->casualty);
      }
    } else {			/* don't print army id number */
      if (print_name) {
	fprintf(fp,
		 "\tArmy \"%s\", %d %s (bonus %d), sustained %d casualties.\n",
		 ap->name, ap->n_soldiers, ap->type, army->bonus,
		 army->casualty);
      } else {
	fprintf(fp, "\t%d %s (bonus %d) sustained %d casualties.\n",
		ap->n_soldiers, ap->type, army->bonus, army->casualty);
      }
    }
  }
}


void reset_print_flags(struct army_info *head)
{
  while (head) {
    head->printed = 0;
    head = head->next;
  }
}


void extract_losses(struct army_info *head, Ssector *sp)
{
  FILE *fp;
  char fname[NAMELEN];
  struct army_info *list;
  Snation *np;
  Sh_spell *h_spell;
  extern Sh_spell *is_spelled();
  Sarmy *ap, *ap2;
  int total_dead = 0;
  int total_nonvamp = 0;
  int n_vampires = 0;
  int revived;

  for (list = head; list != NULL; list = list->next) {
    np = &world.nations[list->armyid->owner];
    ap = get_army(np, list->armyid->id);
    if (is_uncontrolled(ap)) { continue; } /* ignore this army */
    ap->n_soldiers -= list->casualty;    
    total_dead += list->casualty;
    if (is_vampire(ap)) { n_vampires++; }
      else {
	if (!is_kamikaze(ap))
	   total_nonvamp += list->casualty;
      }
    if (ap->n_soldiers == 0) {	/* bye bye */
      if (is_cargo(ap) && ap->cargo.army > -1) { /* dead transport has army */
	ap2 = get_army(np, ap->cargo.army);
	if (ap2) {
	  ap2->flags &= ~AF_IN_TRANSPORT;	/* out of transport mode */
	} else {
	  printf("Error! Destroyed transport has an army I can't find!\n");
	}
      }
      if ((h_spell = is_spelled(ap))) {
        delete_hanging_spell(h_spell);
      }
      delete_army_sector(sp, ap);
      delete_army_nation(np, ap);
      /* Army is gone, so indicate as such on the local list */
      list->armyid = NULL;
    }
  }
  if (n_vampires == 0) { return; }

  for (list = head; list != NULL; list = list->next) {
    /* If the army has been deleted above, dont' worry about vampires */
    if (list->armyid == NULL) { continue; }
    np = &world.nations[list->armyid->owner];
    ap = get_army(np, list->armyid->id);
    if (ap && is_uncontrolled(ap)) { continue; }
    if (ap && is_vampire(ap)) {
      revived = dom_min_int((int)(total_nonvamp * VAMPIRE_FRACT),
			    3 * ap->n_soldiers);
      revived = dom_min_int(revived, total_nonvamp / n_vampires);
      sprintf(fname, "mail%d", ap->owner);
      fp = fopen(fname, "a");
      if (!fp) {
	fprintf(stderr, "Error in battle.c: Can't open mail file\n");
	exit(1);
      }
      fprintf(fp, "\nYour vampire army %d (%d %s) resurrected %d corpses!\n",
	      ap->id, ap->n_soldiers, ap->type, revived);
      fclose(fp);
      ap->n_soldiers += revived;
    }
  }
}
