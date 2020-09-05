/* nation.c -- commands needed to remember changes to a nation;
               basically a lot of routines to parse the exec file
 */

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
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "dominion.h"
#include "misc.h"
#include "army.h"

extern Sworld world;
extern int debug;

/* note: this structure will be initialized at the bottom
   so it doesn't need prototypes
 */
struct exec_cmd { char name[NAMELEN]; void (*func)(); };

/*
void cmd_sname(Snation *np, struct argument args[]);
void cmd_aname(Snation *np, struct argument args[]);
*/
/*
void cmd_amove(), cmd_astat(), cmd_aflag_set(), cmd_aflag_clear(),
  cmd_flag_set_sector(), cmd_flag_clear_sector(),
  cmd_acargo(), cmd_aname(),
  cmd_taxrate(), cmd_desig_sector(), cmd_amake(),
  cmd_cpeople_sector(), cmd_cowner_sector(), cmd_csoil_sector(),
  cmd_cmetal_sector(), cmd_cjewels_sector(), cmd_caltitude_sector(),
  cmd_cmoney(), cmd_cmetal(), cmd_cjewels(),
  cmd_cur_tech_money (), cmd_cur_mag_money (), cmd_cur_mag_jewels (),
  cmd_cur_tech_metal (),
  cmd_cur_spy_money (),
  cmd_cspell_pts(), cmd_ctech_skill(), cmd_cmag_skill(),
  cmd_cfood(), cmd_tech_money(), cmd_tech_metal(),
  cmd_spy_money(),
  cmd_mag_money(), cmd_mag_jewels(), cmd_cmine(), cmd_cfarm(),
  cmd_cspeed(), cmd_nation_name(), cmd_nation_leader(), cmd_nation_order(),
  cmd_nation_mark(), cmd_set_cn(),
  cmd_ccombat(), cmd_crepro(), cmd_cmortality(),
  cmd_cintel(), cmd_cmag_apt(), cmd_cstrength(),
  cmd_new_army_type(), cmd_aincrease(), cmd_amerge(), cmd_asplit(),
  cmd_adisband(),
  cmd_cabonus(), cmd_cfort_sector(), cmd_croads_sector(), cmd_cpass(),
  cmd_destroy(), cmd_acastle (), cmd_new_construct(), cmd_cmax_roads(),
  cmd_cmax_fort();
*/

/* rename a sector */
void cmd_sname(Snation *np, struct argument args[])
{
  int x, y;
  char name[NAMELEN];

  x = args[1].data.num;
  y = args[2].data.num;
  strcpy (name, args[3].data.str);
  
  if (debug >= 2) printf ("Naming sector %d,%d as %s.\n", x, y, name); 
  if (world.map[x][y].name) { free(world.map[x][y].name); }
  world.map[x][y].name = malloc(strlen(name)+1);
  strcpy(world.map[x][y].name, name);
}

/* rename an army */
void cmd_aname(Snation *np, struct argument args[])
{
  int id;
  char *name;
  Sarmy *ap, *get_army();

  id = args[1].data.num;
  name = args[2].data.str;
  
  if ((ap = get_army(np, id))) {
    if (debug >= 2) {
      printf ("Naming army %d as %s.\n", id, name);
    }
    strcpy(ap->name, name);
  } else {
    printf("funny:  cannot find army %d\n", id);
  }
}


/* Move an army... change it's move points to the move points left, which
   is given in the exec string. */
void cmd_amove(Snation *np, struct argument args[])
{
  Sarmy *armypt;
  Ssector *sp;
  int a, x, y, fm;
  
  a = args[1].data.num;		/* army number */
  x = args[2].data.num;		/* new x coordinate */
  y = args[3].data.num;		/* new y coordinate */
  fm = args[4].data.num;	/* final moves */
  armypt = np->armies;
  while (armypt->id != a && armypt->next != NULL) armypt = armypt->next;
  if (armypt->id == a) {
    if (debug >= 2) {
      printf ("Moving army %d from %d,%d to %d,%d\n", a, armypt->pos.x,
	    armypt->pos.y, x, y);
    }
    sp = &(world.map[armypt->pos.x][armypt->pos.y]);
    delete_army_sector(sp, armypt);

    armypt->pos.x = x;
    armypt->pos.y = y;
    armypt->mvpts = fm;
    sp = &(world.map[x][y]);
    insert_army_sector(sp, armypt);
  }
  else printf ("Error! Could not find army!\n"); 
}


/* Change status of an army... if the army is being changed to OCCUPY mode,
   and it is _already_ in occupy mode, DO NOT change it's move ratio!
   Otherwise set the move ratio, change the moves left to zero, and
   change the status. */
void cmd_astat(Snation *np, struct argument args[])
{
  int a, s;
  Sarmy *ap, *get_army();
  
  a = args[1].data.num;
  s = args[2].data.num;
  if (debug >= 2) printf ("Changing status of army %d to %d\n", a, s);
  ap = get_army(np, a);
  if (ap) {
    if (s == A_OCCUPY && ap->status != A_OCCUPY) {
      ap->mvratio = (int)(100.0*(float)(ap->mvpts) / basic_move_rate(np) );
      ap->mvpts = 0;
    }
    if (s == A_PATROL && ap->status != A_PATROL) {
      ap->mvpts = ap->mvpts/2;
    }
    if (s == A_INTERCEPT && ap->status != A_INTERCEPT) {
      ap->mvpts = ap->mvpts/4;
    }
    if (s == A_GARRISON && ap->status != A_GARRISON) {
      ap->mvpts = ap->mvpts/2;
    }
    ap->status = s;
  } else {
    printf("Trying to change status on army %d which does not exist\n", a);
  }
}

/* Set/clear a flag of an army, such as AF_HIDDEN, AF_FLIGHT and so on... */
void cmd_aflag_set(Snation *np, struct argument args[])
{
  int id, flag;
  Sarmy *ap, *get_army();
  
  id = args[1].data.num;
  flag = args[2].data.num;
  if (debug >= 2) printf ("Setting bit %x of army %d\n", flag, id);
  if ((ap = get_army(np, id))) {
    ap->flags |= flag;
  }
}

void cmd_aflag_clear(Snation *np, struct argument args[])
{
  int id, flag;
  Sarmy *ap, *get_army();
  
  id = args[1].data.num;
  flag = args[2].data.num;
  if (debug >= 2) printf ("Clearing bit %x of army %d\n", flag, id);
  if ((ap = get_army(np, id))) {
    ap->flags &= ~flag;
  }
}

/* Set/clear a flag of a sector, such as SF_HIDDEN, SF_BUBBLE and so on... */
void cmd_flag_set_sector(Snation *np, struct argument args[])
{
  int x, y, flag;
  Ssector *sp;
  
  x = args[1].data.num;
  y = args[2].data.num;
  flag = args[3].data.num;
  sp = &world.map[x][y];
  sp->flags |= flag;
}

void cmd_flag_clear_sector(Snation *np, struct argument args[])
{
  int x, y, flag;
  Ssector *sp;
  
  x = args[1].data.num;
  y = args[2].data.num;
  flag = args[3].data.num;
  sp = &world.map[x][y];
  sp->flags &= ~flag;
}

/* Change cargo of an army */
void cmd_acargo(Snation *np, struct argument args[])
{
  int id;
  Sarmy *ap, *get_army();
  
  id = args[1].data.num;

  if (debug >= 2) printf ("Changing cargo of army %d\n", id);
  if ((ap = get_army(np, id)) != NULL) {
    ap->cargo.money = args[2].data.num;
    ap->cargo.metal = args[3].data.num;
    ap->cargo.jewels = args[4].data.num;
    ap->cargo.food = args[5].data.num;
    ap->cargo.people = args[6].data.num;
    ap->cargo.army = args[7].data.num;
    ap->cargo.title.x = args[8].data.num;
    ap->cargo.title.y = args[9].data.num;
  } else {
    printf("[%s] Could not find army %d, nation %d\n", args[0].data.str,
	   id, np->id);
  }
}

void cmd_taxrate(Snation *np, struct argument args[])
{
  int t;

  t = args[1].data.num;  
  if (debug >= 2) printf ("Taxes changed from %d to %d.\n", np->taxes, t);
  np->taxes = t;
}

void cmd_desig_sector(Snation *np, struct argument args[])
{
  int x, y, d;
  Snation *owner_np;

  x = args[1].data.num;
  y = args[2].data.num;
  d = args[3].data.num;		/* the new designation */
  owner_np = &world.nations[world.map[x][y].owner];
  if (debug >= 2) printf ("Redesignating %d,%d to %d.\n", x, y, d);
  world.map[x][y].designation = d;
    /* special case for capital: */
  if (d == D_CAPITAL) {
    owner_np->capital.x = x;	/* make sure Gamemaster does not get it */
    owner_np->capital.y = y;
  }
}


void cmd_amake(Snation *np, struct argument args[])
{
  Sarmy army, make_army();
  Ssector *sp;
  int a, no_sold, x, y;
  char *type, *name;
  
  a = args[1].data.num;		/* army number */
  no_sold = args[2].data.num;	/* number of soldiers drafted */
  x = args[3].data.num;		/* x coordinate where army is drafted */
  y = args[4].data.num;		/* y coordinate where army is drafted */
  type = args[5].data.str;	/* army type */
  name = args[6].data.str;	/* army name */

  sp = &(world.map[x][y]);
  army = make_army(type,name,no_sold,A_DEFEND,np->id,sp->loc);
/*  army.id  = free_army_id(np); */
  army.id = a;
  army.next = NULL;

  if (debug >= 2) printf("Permanently drafting army #%d\n",army.id);
  if (np->armies == NULL) {
    np->armies = (Sarmy *) malloc(sizeof(Sarmy));
    *(np->armies) = army;
  } else {
    insert_army_nation(np, &army, a);
  }
  insert_army_sector(sp, &army);
  ++np->n_armies;
}

void cmd_cmoney(Snation *np, struct argument args[])
{
  int change, nat_id;

  nat_id = args[1].data.num;
  change = args[2].data.num;	/* changed money */
  np = &world.nations[nat_id];
     
  np->money = np->money + change;
  if (debug >= 2) printf("New nation money (after difference of %d) = %d\n",
	 change,np->money);
}

void cmd_cmetal(Snation *np, struct argument args[])
{
  int change, nat_id;

  nat_id = args[1].data.num;
  change = args[2].data.num;   /* changed money */
  np = &world.nations[nat_id];
  
  np->metal = np->metal + change;
  if (debug >= 2) printf("New nation metal (after difference %d) = %d\n",
	 change,np->metal);
}

void cmd_cjewels(Snation *np, struct argument args[])
{
  int change, nat_id;

  nat_id = args[1].data.num;
  change = args[2].data.num;   /* changed money */
  np = &world.nations[nat_id];
  
  np->jewels = np->jewels + change;
  if (debug >= 2) printf("New nation jewel (after difference %d) = %d\n",
	 change,np->jewels);
}

void cmd_cspell_pts(Snation *np, struct argument args[])
{
  int change, nat_id;

  nat_id = args[1].data.num;
  change = args[2].data.num;   /* changed money */
  np = &world.nations[nat_id];
  
  np->spell_pts = np->spell_pts + change;
  if (debug >= 2) printf("New nation spell pts (after difference %d) = %d\n",
		    change, np->spell_pts);
}

void cmd_ctech_skill(Snation *np, struct argument args[])
{
  int change, nat_id;

  nat_id = args[1].data.num;
  change = args[2].data.num;   /* changed money */
  np = &world.nations[nat_id];
  
  np->tech_skill = np->tech_skill + change;
}

void cmd_cmag_skill(Snation *np, struct argument args[])
{
  int change, nat_id;

  nat_id = args[1].data.num;
  change = args[2].data.num;   /* changed money */
  np = &world.nations[nat_id];
  
  np->mag_skill = np->mag_skill + change;
}

void cmd_cfood(Snation *np, struct argument args[])
{
  int change, nat_id;

  nat_id = args[1].data.num;
  change = args[2].data.num;   /* changed money */
  np = &world.nations[nat_id];
  
  np->food = np->food + change;
  if (debug >= 2) printf("New nation food (after difference %d) = %d\n",
	 change,np->food);
}

void cmd_tech_money(Snation *np, struct argument args[])
{
  np->tech_r_d = args[1].data.num;
  if (debug >= 2) printf("Tech R&D money changed to %d\n",np->tech_r_d);
}


void cmd_tech_metal(Snation *np, struct argument args[])
{
  np->tech_r_d_metal = args[1].data.num;
  if (debug >= 2) printf("Tech R&D metal changed to %d\n",np->tech_r_d_metal);
}


void cmd_spy_money(Snation *np, struct argument args[])
{
     np->spy_r_d = args[1].data.num;
   }


void cmd_mag_money(Snation *np, struct argument args[])
{
  np->mag_r_d = args[1].data.num;
  if (debug >= 2) printf("Magic money changed to %d\n",np->mag_r_d);
}

void cmd_mag_jewels(Snation *np, struct argument args[])
{
  np->mag_r_d_jewels = args[1].data.num;
  if (debug >= 2) printf("Magic jewels changed to %d\n",np->mag_r_d_jewels);
}

void cmd_cmax_roads(Snation *np, struct argument args[])
{
  np->consts.roads += args[1].data.num;
}

void cmd_cmax_fort(Snation *np, struct argument args[])
{
  np->consts.forts += args[1].data.num;
}

void cmd_cmine(Snation *np, struct argument args[])
{
  np->mine_skill += args[1].data.num;
}
void cmd_cfarm(Snation *np, struct argument args[])
{
  np->farm_skill += args[1].data.num;
}
void cmd_cspeed(Snation *np, struct argument args[])
{
  np->race.speed += args[1].data.num;
}

void cmd_nation_name(Snation *np, struct argument args[])
{
  Snation *changed_np = &world.nations[args[1].data.num];
  strcpy(changed_np->name, args[2].data.str);
}

void cmd_nation_leader(Snation *np, struct argument args[])
{
  Snation *changed_np = &world.nations[args[1].data.num];
  strcpy(changed_np->leader, args[2].data.str);
}

void cmd_nation_order(Snation *np, struct argument args[])	/* change magic order */
{
  Snation *changed_np = &world.nations[args[1].data.num];
  strcpy(changed_np->mag_order, args[2].data.str);
}

void cmd_nation_mark(Snation *np, struct argument args[])	/* change a nation's mark */
{
  Snation *changed_np = &world.nations[args[1].data.num];
  changed_np->mark = args[2].data.str[0];
}

  /* set and clear the cn flag on a nation */
void cmd_set_cn(Snation *np, struct argument args[])
{
  Snation *changed_nation = &world.nations[args[1].data.num];
  int new_val = args[2].data.num;

  if (new_val > 0 || new_val < 2) { /* the only good values */
    changed_nation->cn_flag = new_val;
  }
}

/*
void cmd_clear_cn(Snation *np, struct argument args[])
{
  Snation *changed_nation = &world.nations[args[1].data.num];

  changed_nation->cn_flag = 0;
}
*/

/*  cmd_cn_param no longer used - values are stored in sepearate files.
void cmd_cn_param(Snation *np, struct argument args[])
{
  Snation *changed_nation = &world.nations[args[1].data.num];

  changed_nation->cn_agg = args[2].data.num;
  changed_nation->cn_exp = args[3].data.num;
  changed_nation->cn_iso = args[4].data.num;
} */

void cmd_ccombat(Snation *np, struct argument args[])
{
  np->combat_bonus += args[1].data.num;
}

void cmd_crepro(Snation *np, struct argument args[])
{
  np->race.repro += args[1].data.num;
}

void cmd_cmortality(Snation *np, struct argument args[])
{
  if ((np->race.mortality += args[1].data.num) < 1) {
    np->race.mortality = 1;
  }
}

void cmd_cintel(Snation *np, struct argument args[])
{
  if ((np->race.intel += args[1].data.num) < 1) {
    np->race.intel = 1;
  }
}

void cmd_cmag_apt(Snation *np, struct argument args[])
{
  if ((np->race.mag_apt += args[1].data.num) < 1) {
    np->race.mag_apt = 1;
  }
}

void cmd_cstrength(Snation *np, struct argument args[])
{
  if ((np->race.strength += args[1].data.num) < 1) {
    np->race.strength = 1;
  }
    np->combat_bonus += args[1].data.num/2.5;
}

  /* change the number of people in that sector */
void cmd_cpeople_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->n_people += args[3].data.num;
}
  /* change the owner in that sector */
void cmd_cowner_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->owner = args[3].data.num;
  addsector(&world.nations[sp->owner], x, y);
  if (debug >= 2)
    printf("sector (%d,%d) owned by %s\n", x, y,
	   world.nations[sp->owner].name);
}

  /* change the soil in that sector */
void cmd_csoil_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->soil += args[3].data.num;
}

  /* change the metal in that sector */
void cmd_cmetal_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->metal += args[3].data.num;
}
  /* change the jewels in that sector */
void cmd_cjewels_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->jewels += args[3].data.num;
}

  /* change the altitude in that sector */
void cmd_caltitude_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->altitude += args[3].data.num;
}

/* adds a new type of army to the nation's list of available army types */
void cmd_new_army_type(Snation *np, struct argument args[])
{
  Savail_army *aa;
  extern Suser user;
  int falready = 0;

  /* Check to see that they don't already have the army type */
  aa = user.avail_armies;
  if (aa != NULL) {
    do {
      if (!strcmp(aa->type, args[1].data.str)) {
	falready = 1;
	break;
      }
    } while (aa->next && (aa=aa->next));
  }
  if (aa == NULL) {
    aa = (Savail_army *) malloc(sizeof(Savail_army));
    strcpy (aa->type, args[1].data.str);
    user.n_army_types++;
  } else if (!falready) {
    do { ; } while (aa->next && (aa = aa->next));
    aa->next = (Savail_army *) malloc(sizeof(Savail_army));
    aa->next->next = NULL;
    strcpy(aa->next->type, args[1].data.str);
    user.n_army_types++;
  }
}

/* allows nation to construct a new type of object, such
   as increasing roads in a sector, or making land into water
   or vice-versa.
*/
void cmd_new_construct(Snation *np, struct argument args[])
{
  if (!strcmp(args[1].data.str,"Bubble")) {
    np->consts.flags |= CF_BUBBLE;
  } else if (!strcmp(args[1].data.str,"Refinery")) {
    np->consts.flags |= CF_REFINE;
  } else {
    fprintf(stderr,"Error! Unknown construction %s\n",args[1].data.str);
  }
}

  /* adds the first army to the second.  Requires the two army ids. */
void cmd_amerge(Snation *np, struct argument args[])
{
  Sarmy *ap, *ap2;
  Ssector *sp;
  ap = get_army(np, args[1].data.num);
  ap2 = get_army(np, args[2].data.num);
  sp = &world.map[ap->pos.x][ap->pos.y];
  ap->n_soldiers += ap2->n_soldiers;
  ap->sp_bonus = dom_min_int(ap2->sp_bonus, ap->sp_bonus);
  ap->mvpts = dom_min_int(ap2->mvpts, ap->mvpts);
  ap->mvratio = dom_min_int(ap2->mvratio, ap->mvratio);
  delete_army_sector(sp, ap2);
  delete_army_nation(np, ap2);
}

  /* increases the army (of given id) by the given number of people */
void cmd_aincrease(Snation *np, struct argument args[])
{
  Sarmy *ap;

  ap = get_army(np, args[1].data.num);
  ap->n_soldiers += args[2].data.num;
  ap->mvpts = 0;
  ap->mvratio = 0;
}

/* splits an army up.  requires armyid and number of men to split */
void cmd_asplit(Snation *np, struct argument args[])
{
  Sarmy *ap, army2, make_army(), *get_army();

  ap = get_army(np,args[1].data.num);
  army2 = make_army(ap->type, "", args[2].data.num, ap->status,
		    ap->owner, ap->pos);
  ap->n_soldiers -= args[2].data.num;
  army2.mvpts = ap->mvpts;
  army2.mvratio = ap->mvratio;
  army2.flags = ap->flags;
  army2.sp_bonus = ap->sp_bonus;
  army2.money_maint = ap->money_maint;
  army2.metal_maint = ap->metal_maint;
  army2.jewel_maint = ap->jewel_maint;
  army2.spell_pts_maint = ap->spell_pts_maint;
  sprintf(army2.name, "%s %d", army2.type, army2.id);
  np->n_armies++;
  insert_army_nation(np, &army2, -1);
  insert_army_sector(&world.map[army2.pos.x][army2.pos.y], &army2);
    /* to give them a name, we must know their id *after* they are inserted */
  ap = get_army(np, army2.id);
  sprintf(ap->name, "%s %d", army2.type, army2.id);
}

void cmd_adisband(Snation *np, struct argument args[])
{
  Sarmy *ap;
  Ssector *sp;
  Sh_spell *h_spell;
  extern Sh_spell *is_spelled();

  ap = get_army(np,args[1].data.num);
  sp = &world.map[ap->pos.x][ap->pos.y];
    /* you don't get the population back from a spirit */
  if (!is_spirit(ap) && !is_in_transport(ap)) {
    sp->n_people += ap->n_soldiers;
  }
  if ((h_spell = is_spelled(ap))) {
    delete_hanging_spell(h_spell);
  }
  delete_army_sector(sp, ap);
  delete_army_nation(np, ap);
}

/* give an army more/less special bonus */
void cmd_cabonus(Snation *np, struct argument args[])
{
  Sarmy *ap;
  int bonus_change;

  bonus_change = args[2].data.num;
  if ((ap = get_army(np,args[1].data.num))) {
    ap->sp_bonus += bonus_change;
  }
}

  /* change fortification level of a sector */
void cmd_cfort_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->defense += args[3].data.num;
}

  /* change number of roads in a sector */
void cmd_croads_sector(Snation *np, struct argument args[])
{
  Ssector *sp;
  int x, y;

  x = args[1].data.num;
  y = args[2].data.num;
  sp = &world.map[x][y];
  sp->roads += args[3].data.num;
}

void cmd_cpass(Snation *np, struct argument args[])
{
  Snation *dest_np;
  int id;
  char *new_pass;

  id = args[1].data.num;
  new_pass = args[2].data.str;
  dest_np = &world.nations[id];
  strcpy(dest_np->passwd, new_pass);
}

void cmd_cur_mag_money(Snation *np, struct argument args[])

{
  np->cur_mag_r_d = args[1].data.num;
}

void cmd_cur_tech_money(Snation *np, struct argument args[])

{
  np->cur_tech_r_d = args[1].data.num;
}

void cmd_cur_mag_jewels(Snation *np, struct argument args[])

{
  np->cur_mag_r_d_jewels = args[1].data.num;
}

void cmd_cur_tech_metal(Snation *np, struct argument args[])

{
  np->cur_tech_r_d_metal = args[1].data.num;
}

void cmd_cur_spy_money(Snation *np, struct argument args[])

{
  np->cur_spy_r_d = args[1].data.num;
}

void cmd_destroy(Snation *np, struct argument args[])
{
  int id;

  id = args[1].data.num;
  destroy_nation(id);
}

void cmd_acastle(Snation *np, struct argument args[])
{
  int army_id;
  Ssector * sp;
  Ssector * capital = &world.map [np->capital.x][np->capital.y];
  Sarmy * ap;

  army_id = args[1].data.num;
  if ((ap = get_army (np, army_id)) == NULL) {
    printf ("funny, can't find army %d\n", army_id);
  }
  sp = &world.map [ap->pos.x][ap->pos.y];

  delete_army_sector (sp, ap);

  ap->pos.x = capital->loc.x;
  ap->pos.y = capital->loc.y;
  
  insert_army_sector (capital, ap);
}

  /* read in the exec file for the master nation; this is done by all
     users, in case the master has changed things for them.  It is not
     done by the master nation, since that happens automaticaly.
   */
void load_master_execs()
{
  int argc;
  Snation *np = &world.nations[0];
  FILE *exec_file, *fopen();
  struct argument args[N_EXEC_ARGS];
  char filename[NAMELEN];

  sprintf(filename, "exec/exec%d", np->id);
  exec_file = fopen(filename, "r");
  if (!exec_file && np->cn_flag == 0) {
    /* printf("Nation %d (%s) did not move this turn.\n", np->id, np->name); */
  } else {
    while ((argc = getexec(exec_file, args) != -1)) {
      run_exec_line(np, args);
    }
    if (exec_file != NULL) { fclose(exec_file); }
  }
}


/* now I go and initialize the structure with all the fuction pointers */
static struct exec_cmd commands[] = {
  {"AMOVE", cmd_amove} , {"ASTAT", cmd_astat} , {"AMAKE", cmd_amake} ,
  {"AFLAG_SET", cmd_aflag_set} , {"AFLAG_CLEAR", cmd_aflag_clear} ,
  {"FLAG_SET_SECTOR", cmd_flag_set_sector} ,
  {"FLAG_CLEAR_SECTOR", cmd_flag_clear_sector} ,
  {"ACARGO", cmd_acargo} ,
  {"SNAME", cmd_sname} , {"ANAME", cmd_aname},
  {"DESIG_SECTOR", cmd_desig_sector} ,
  {"TAXRATE", cmd_taxrate},
  {"CMONEY", cmd_cmoney} , {"CJEWELS", cmd_cjewels}, {"CMETAL", cmd_cmetal},
  {"CUR_MAGR&Dmoney", cmd_cur_mag_money},
  {"CUR_TECHR&Dmoney", cmd_cur_tech_money},
  {"CUR_TECHR&Dmetal", cmd_cur_tech_metal},
  {"CUR_MAGR&Djewels", cmd_cur_mag_jewels},
  {"CUR_SPYR&Dmoney", cmd_cur_spy_money},
  {"CSPELL_PTS", cmd_cspell_pts}, {"CTECH_SKILL", cmd_ctech_skill} ,
  {"CMAG_SKILL", cmd_cmag_skill} ,
  {"CFOOD", cmd_cfood} , {"TECHR&Dmoney", cmd_tech_money} ,
  {"TECHR&Dmetal", cmd_tech_metal} , {"SPYR&Dmoney", cmd_spy_money} ,
  {"MAGR&Dmoney" , cmd_mag_money} , {"MAGR&Djewels" , cmd_mag_jewels} ,
  {"CMINE" , cmd_cmine} , {"CFARM" , cmd_cfarm} , {"CSPEED", cmd_cspeed} ,
  {"NATION_NAME", cmd_nation_name} , {"NATION_LEADER", cmd_nation_leader} ,
  {"NATION_ORDER", cmd_nation_order} , {"NATION_MARK", cmd_nation_mark} ,
  {"SET_CN", cmd_set_cn} , /* {"CLEAR_CN", cmd_clear_cn} , */
  /* {"CN_PARAM", cmd_cn_param} ,*/ {"CCOMBAT" , cmd_ccombat} ,
  {"CREPRO" , cmd_crepro} , {"CINTEL", cmd_cintel} ,
  {"CMAG_APT", cmd_cmag_apt} , {"CSTRENGTH", cmd_cstrength} ,
  {"CMORTALITY" , cmd_cmortality} , {"CPEOPLE_SECTOR" , cmd_cpeople_sector} ,
  {"COWNER_SECTOR" , cmd_cowner_sector} , {"CSOIL_SECTOR", cmd_csoil_sector} ,
  {"CMETAL_SECTOR", cmd_cmetal_sector} ,
  {"CJEWELS_SECTOR", cmd_cjewels_sector} ,
  {"CALTITUDE_SECTOR", cmd_caltitude_sector} ,
  {"NEW_ARMY_TYPE", cmd_new_army_type} , {"AMERGE", cmd_amerge} ,
  {"AINCREASE", cmd_aincrease},
  {"ASPLIT", cmd_asplit} , {"ADISBAND", cmd_adisband} ,
  {"CABONUS", cmd_cabonus} ,
  {"CFORT_SECTOR", cmd_cfort_sector} , {"CROADS_SECTOR", cmd_croads_sector} ,
  {"CPASS", cmd_cpass} , {"DESTROY", cmd_destroy},
  {"ACASTLE", cmd_acastle}, {"NEW_CONSTRUCT", cmd_new_construct }, 
  {"CMAX_ROADS", cmd_cmax_roads}, {"CMAX_FORT", cmd_cmax_fort}
};

/*******************************************************/
/* gets nation data for nation 'id', put it into '*np' */
/* takes in an integer nation id, and a pointer to a   */
/* nation structure.                                   */
/*******************************************************/
void load_nation(int id, Snation *np)
{
  FILE *exec_file;
  struct argument args[N_EXEC_ARGS];
  char filename[NAMELEN];
  int argc;
  extern Suser user;

  *np = world.nations[id];
  user.np = np;		/* hmm.. must cleanup use of "Suser" in update */
    /* remember what the initial values were, before reading the exec file */
  user.init_money = user.np->money;
  user.init_metal = user.np->metal;
  user.init_jewels = user.np->jewels;
  user.init_food = user.np->food;
  user.id = np->id;
  load_options(np);
  load_params(np);	/* Load the comp nation params.  KAH 6/9/93 */

  sprintf(filename, "exec/exec%d", np->id);
  exec_file = fopen(filename, "r");
  if (!exec_file && np->cn_flag == 0) {
    fprintf(stderr,
       "Nation %d (%s) has not yet played this turn.\n", np->id, np->name);
  } else {
    while ((argc = getexec(exec_file, args)) != -1) {
      run_exec_line(np, args);
    }
    if (exec_file != NULL) { fclose(exec_file); }
  }
}

/****************************************************************/
/* this takes a pointer to the exec file and a pointer to an    */
/* array of argument structures.  The structures are filled in  */
/* with the arguments of the next line of the file.  The        */
/* function returns an integer which is the number of arguments */
/****************************************************************/
int getexec(FILE *fp, struct argument args[])
{
  char buff[EXECLEN];
  if (fp == NULL) {
    return -1;
  }
  if (fgets(buff, EXECLEN, fp) != NULL) {
    return parse_exec_line(buff, args);
  }
  else return -1;
}



/* this takes the exec line args (already parsed) and runs it */
void run_exec_line(Snation *np, struct argument args[])
{
  int i, found = 0;

  for (i=0; i < sizeof(commands)/sizeof(struct exec_cmd); i++) {
    if (!(strcmp(commands[i].name, args[0].data.str))) {
      found = 1;
      (*commands[i].func)(np, args);
    }
  }
  if (!found) {
    printf("Error: exec command <%s> not implemented\n", args[0].data.str);
  }
}


/* this parses a single exec line */
int parse_exec_line(char line[], struct argument args[])
{
  char arg_tmp[NAMELEN];
  int count = 0, place;

    /* make sure there is a newline at the end of this line */
  if (line[strlen(line)-1] != '\n') {
    line[strlen(line)+1] = '\0'; /* extend the string */
    line[strlen(line)] = '\n'; /* put the newline in */
  }
  sscanf(line, "%[^:]", args[0].data.str);
  place = strlen(args[0].data.str)+1;
  args[0].type = TXT;
  do {
    sscanf(&line[place], "%[^:\n]", arg_tmp);
    place += strlen(arg_tmp)+1;
    count++;
    if ((isdigit(arg_tmp[0]) || (arg_tmp[0] == '-'))
	&& are_all_digits(arg_tmp+1)) {
      args[count].type = NUM;
      args[count].data.num = atoi(arg_tmp);
    }
    else {
      args[count].type = TXT;
      strcpy(args[count].data.str, arg_tmp);
    }
  } while (place+1 < strlen(line));
  return count;
}



/******************************************************************/
/* this takes the name of a nation (a pointer to an array of      */
/* characters) and returns the id number of that nation.          */
/******************************************************************/
int get_nation_id(char name[])
{
  int i;

  for (i = 0; i < world.n_nations; ++i) {
    if (!strcmp(world.nations[i].name, name)) {
      return i;		/* found it! */
    }
  }
  return -1;			/* it's not there */
}
