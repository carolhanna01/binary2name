   /* cn.c -- modules involved in CN movemaking */

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
#include <math.h>
#include <curses.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <string.h>

#include "config.h"
#include "dominion.h"
#include "misc.h"
#include "army.h"

/* some prototypes used here */
void init_cn(Snation *np);
void do_cn_draft(Snation *np);
void do_cn_merge(Snation *np);
void do_cn_split(Snation *np);
void do_cn_summon(Snation *np);
void do_cn_armies(Snation *np, struct desire **des_array);
void do_cn_redesig(Snation *np);
void init_cn(Snation *np);
void do_cn_diplo(Snation *np);
int count_troops_with(Snation *np, int status);
int get_good_types(int good_armies[]);
void cn_draft_army(Sarmy army, Snation *np, Ssector *sp);
Sarmy *get_first_mage(Snation *np);
void init_cn_mage(Snation *np, Ssector *sp);
void check_moves(Snation *np, Sarmy *ap, struct tmp_map ary[CN_SIDE][CN_SIDE]);
int tmp_army_better(Sarmy *ap, Sarmy *tmpap, Sarmy *oldap);
void cmd_amove(Snation *np, struct argument args[]);
void cmd_amerge(Snation *np, struct argument args[]);
void cmd_asplit(Snation *np, struct argument args[]);
void find_desire(Snation *np, struct desire **des_array);
void cmd_astat(Snation *np, struct argument args[]);


#define NSTATS 4		/* different statuses to move */
#define CN_PATROL		/* comment out to keep cns from patrolling */
#define CN_INTER                /* comment out to keep cns from intercepting */

extern Sworld world;
extern Suser user;
extern struct s_desig_map desig_map[];
extern int (*wrapx)(), (*wrapy)();
extern Sdiplo **allocate_diplo();
extern int debug;
extern struct army_type *army_types;
extern struct spirit_type *spirit_types;


int statary[NSTATS] = { A_ATTACK,A_DEFEND, A_PATROL, A_INTERCEPT };
float cn_food_need,cn_metal_need,cn_jewel_need,cn_money_need;
int opt_army_size,atwar,cn_specific;

/*-----------------------------cn_moves()------------------------------------
	This function makes the moves for an cn
----------------------------------------------------------------------------*/
void cn_moves(Snation *np)
{
  int i;
  struct desire **des_array;


  des_array = (struct desire **) malloc(world.xmax*sizeof(struct desire*));
  for (i = 0; i < world.xmax; ++i)
    des_array[i] = (struct desire *) malloc(world.ymax*sizeof(struct desire));
  
  if (debug >= 2) printf("doing init\n");    /* initialize globals and stuff */
  init_cn(np);

  if ((world.turn > 1) || cn_specific){	/* don't draft first turn */
    if (debug >= 2) printf("Drafting\n");	/* unless race specific army */
    do_cn_draft(np);
  }

  if (debug >= 2) printf("doing merge\n"); /* merge small armies */
  do_cn_merge(np);

  if (debug >= 2) printf("doing split\n"); /* split large armies */
  do_cn_split(np);

  if (debug >= 2) printf("doing summon\n"); /* summon spirits */
  do_cn_summon(np);

  if (debug >= 2) printf("doing armies\n"); /* move armies around */
  do_cn_armies(np,des_array);

  if (debug >= 2) printf("doing redesig\n"); /* redesignate sectors */
  do_cn_redesig(np);

  for (i = 0; i < world.xmax; i++)	/* free desire array */
    free(des_array[i]);
  free(des_array);

  for (i = 0; i < world.xmax; ++i) {	/* free visible sectors array */
    free(user.visible_sectors[i]);
  }
  free(user.visible_sectors);

  free_diplo(user.diplo_matrix, world.n_nations);
}

/*-------------------------------init_cn()----------------------------------
	This is basically the function init_user, copied from user.c.  There
are some things that init_user does that this does not do.
	Also, this function sets global varibles like needs and race-specific
army types.
---------------------------------------------------------------------------*/
void init_cn(Snation *np)
{
  int i;
  FILE *fp, *fopen();
  char line[EXECLEN];

    /* find out which army types are available to the user */
  user.avail_armies = NULL;
  get_avail_armies(&user, np->tech_skill);
  user.spirit_list = NULL;
  get_spirits (&user, np->mag_skill);

	/*------ set cn_specific to be 1 + the index of the race 
		 specific army type, if any.  ---------------*/
  cn_specific = 0;
  if ((fp = fopen(RACES_FILE, "r")) != NULL) {
    while (1) {
      if (fgets(line, EXECLEN, fp) == NULL)
        break;
      if (strncmp(line, np->race.name, strlen(np->race.name)) == 0
	  && strncmp(line+strlen(np->race.name), "_armies:",
		     strlen("_armies:")) == 0) {
        if (line[strlen(line)-1] == '\n') {
          line[strlen(line)-1] = '\0';
	}
	cn_specific = army_type_index(strchr(line, ':')+1) + 1;
      }
    }
    fclose(fp);
  }

    /* now set fields for the ustruct */
  user.underwater = 0;		/* user is not underwater at start */
  if (np->race.pref_alt < 0) {
    user.underwater = 1;	/* merfolk or whatever */
  }
    /* load user's diplomacy statuses, for fast access later;
       also remember the initial values, so users cannot change
       their status by more than one step at a time.
     */
  user.diplo_matrix = allocate_diplo(world.n_nations);
  read_in_diplo(user.diplo_matrix, world.n_nations);

/* now do diplomacy and set atwar to # of nations cn is at war with */
  do_cn_diplo(np);

    /* calculate visibility matrix for this user.
       this might depend on spells, so do it after
       loading spells.
     */
  user.visible_sectors = (int **) malloc(world.xmax*sizeof(int *));
  for (i = 0; i < world.xmax; ++i) {
    user.visible_sectors[i] = (int *) malloc(world.ymax*sizeof(int));
  }
  find_visible_sectors(user.visible_sectors);


/*  cn_money_need =(float)calc_expend(np)/((float)calc_revenue(np)+1.0);
  cn_metal_need =(float)calc_expend_metal(np)/((float)calc_metal(np)+1.0)+.3;
cn_jewel_need =(float)calc_expend_jewels(np)/((float)calc_jewels(np)+1.0)+.3;*/

  cn_metal_need = atwar ? 1.3:1.0; /* for now */
  cn_jewel_need = atwar ? 1.3:1.0; /* for now */
  cn_food_need = (float)calc_expend_food(np)/((float)calc_food(np)+1.0);
  if (debug >= 2) printf("food need = %f\n",cn_food_need);
  if(atwar){
    opt_army_size = get_n_soldiers (np) * np->cn_params->army_size /
		    (np->n_sects * 10);
    opt_army_size = max (np->cn_params->min_size, opt_army_size);
  }
  else{
    opt_army_size = OCCUPYING_SOLDIERS;
  }

  if(debug >= 2)
    printf("opt size = %d.\n",opt_army_size);

		/*----  Handle the CN's Economy ----*/
  if(atwar){
    np->taxes = 15;
    np->spy_r_d = 0;
    if (calc_metal(np) < 30000 ) {
      np->tech_r_d_metal = 0;
      np->tech_r_d = 15;
    }
    else {
      np->tech_r_d_metal = 15;
      np->tech_r_d = 0;
    }
    if ( (calc_jewels(np) - MAGE_JEWELS_MAINT) < 0 ) {
      np->mag_r_d_jewels = 0;
    } else {
      if (next_thon_jewels(np) < INITIATION_JEWELS * 2) {
	/* stockpile jewels fast */
	np->mag_r_d_jewels = (((calc_jewels(np) - MAGE_JEWELS_MAINT - 
	   INITIATION_JEWELS*2) * 100 )/calc_jewels(np));
      } else {
	/* stockpile jewels slow */
	np->mag_r_d_jewels = (((calc_jewels(np) - MAGE_JEWELS_MAINT - 
	   INITIATION_JEWELS / 5) *100 )/calc_jewels(np));
      }
    }
    if ( np->mag_r_d_jewels > 0 ) {
      np->mag_r_d = 0;
    } else {
      np->mag_r_d = 15;
    }
  } else {
    np->taxes = 13;
    if (calc_metal(np) < 10000 ) {
      np->tech_r_d_metal = 30;
    } else {
	np->tech_r_d_metal = 75;
    }
    if (((calc_jewels(np) < INITIATION_JEWELS) && (next_thon_jewels(np) 
      < INITIATION_JEWELS * 2)) || (calc_jewels(np) == 0)) {
      np->mag_r_d_jewels = 0;
    } else {
      np->mag_r_d_jewels = (((calc_jewels(np) - INITIATION_JEWELS / 5 ) 
      * 100)/calc_jewels(np)) ;
      if (np->mag_r_d_jewels < 0) 
	np->mag_r_d_jewels = 0;
    }
    if (np->mag_r_d_jewels == 0) {
      np->mag_r_d = 15;
    } else {
      np->mag_r_d = 0;
    }
    np->tech_r_d = 10;
    np->spy_r_d = 10;
    }
}

/*---------------------------do_cn_draft()-----------------------------------
	Draft armies.  This routine has been altered to take technology
level into account.

----------------------------------------------------------------------------*/
void do_cn_draft(Snation *np)
{
  Sarmy army, make_army();
  Ssector *sp;
  struct army_type this_atype;
  struct army_info {
    int force, m_cost, s_cost;
    } a_info[MAX_TYPES];

  int max_sold,ngood,good_armies[MAX_TYPES],i,tmp,patrol_sold,inter_sold;
  int to_draft, ndraft, indx, f_max, cur_sold;
  char type[NAMELEN];

	/*-------- Initialize variables for cn drafting -------*/
  sp = &world.map[np->capital.x][np->capital.y]; /* always draft from cap */
  tmp = get_n_civil(np);
  if(atwar) max_sold = tmp * np->cn_params->mil/200;
  else max_sold = tmp * np->cn_params->mil/300;
  cur_sold = get_n_soldiers(np);
  if (cur_sold >= max_sold)
    return;
  patrol_sold = count_troops_with(np, A_PATROL);
  inter_sold = count_troops_with(np, A_INTERCEPT);
  if (debug >= 2) printf("pat %d tot %d:",patrol_sold, cur_sold);

  tmp = tmp * (np->race.repro - np->race.mortality) / 100;
  if (atwar)
    to_draft = ((max_sold - cur_sold) + (tmp * np->cn_params->mil / 100)) / 2;
  else
    to_draft = tmp * np->cn_params->mil / 100;

  to_draft = min (to_draft, sp->n_people) / opt_army_size;
  if (debug >= 2){
    printf("CN wants %d armies of %d troops\n",to_draft,opt_army_size);
    printf("%d metal and %d money available.\n", np->metal, np->money);
    }

  ngood = get_good_types(good_armies);
  if (ngood == 0) return;
  for (i = 0; i < ngood; i++){
      this_atype = army_types[good_armies[i]];
      strcpy(type,this_atype.type);
      army = make_army(type,type, opt_army_size, A_DEFEND, np->id, sp->loc);
 /*     bonus = army.sp_bonus + (np->attack + np->defense) / 2; */
      a_info[i].force = (opt_army_size * (100 + army.sp_bonus)) / 100; 
      a_info[i].m_cost = army_cost_metal(&army) + 1;
      a_info[i].s_cost = army_cost(&army) + 1;	/* avoid /0 */
      }

  while (to_draft > 0) {
    f_max = 0;
    indx = 0;
    ndraft = 0;
    for (i = 0; i < ngood; i++) {
      tmp = min (np->metal / a_info[i].m_cost, np->money / a_info[i].s_cost);
      tmp = min (tmp, to_draft);
      if (tmp * a_info[i].force > f_max) {
	f_max = tmp * a_info[i].force;
	indx = i;
	ndraft = 1;
      }
    }
    if (f_max == 0)		/* can't draft anything else */
      return;

    this_atype = army_types[good_armies[indx]];
    strcpy(type,this_atype.type);
#ifdef CN_PATROL
    if (( atwar && (patrol_sold < cur_sold * np->cn_params->pat / 100)) || 
	(!atwar && (patrol_sold < cur_sold * np->cn_params->pat / 200)
	 && (world.turn > 5))) {
      army = make_army(type,type, opt_army_size, A_PATROL, np->id, sp->loc);
      patrol_sold += opt_army_size;
      if (debug >= 2) printf("patrol ");
      }
    else 
#endif
#ifdef CN_INTER
    if (( atwar && (inter_sold < cur_sold * np->cn_params->pat / 100)) ||
      	(!atwar && (inter_sold < cur_sold * np->cn_params->pat / 200)
	 && (world.turn > 5) )) {
      army = make_army(type,type, opt_army_size, A_INTERCEPT, np->id, sp->loc);
      inter_sold += opt_army_size;
      if (debug >= 2) printf("intercept ");
      }
    else 
#endif
      army = make_army(type,type, opt_army_size, A_DEFEND, np->id, sp->loc);
    cn_draft_army(army, np, sp);
    
    if (debug >= 2)		/* print message about what happened */
      printf("%s,  ",type);
    sp->n_people -= opt_army_size;
    np->money -= a_info[indx].s_cost;
    np->metal -= a_info[indx].m_cost;
    to_draft--;
    cur_sold += opt_army_size;
    }
  if (debug >= 2) printf("\n");
}

/*-------------------------cn_draft_army()----------------------------------*/
void cn_draft_army(Sarmy army, Snation *np, Ssector *sp)
{
  army.id = free_army_id(np);
  army.next = NULL;
  ++np->n_armies;
  if (np->armies == NULL) {
    np->armies = (Sarmy *) malloc(sizeof(Sarmy));
    *(np->armies) = army;
    np->armies->next = NULL;
  } else {
    insert_army_nation(np, &army, -1);
  }
  insert_army_sector(sp, &army);
}


/*----------------------------do_cn_summon()--------------------------------
	Have the cn draft a mage, if necessary, and summon spirits.
Basically, find the net spell points and draft the biggest spirit you can.
Don't draft caravan spirits.
---------------------------------------------------------------------------*/
void do_cn_summon(Snation *np)
{
  int have_mage = 0,sindex,net_points,badflags;
  Sarmy army,make_army();
  struct spirit_type this_stype;
  Ssector *sp;
  Sspirit *sptr,*tmp_sptr;

  if(!atwar || np->spell_pts < 2)
    return;

  badflags = AF_INVERSE_ALT | AF_CARGO;
  if (user.underwater)
    badflags |= AF_LAND;
  else
    badflags |= AF_WATER;

  sp = &world.map[np->capital.x][np->capital.y];

  have_mage = (get_first_mage(np) != NULL);
  if (!have_mage) {
    if (np->jewels >= INITIATION_JEWELS && next_thon_jewels(np) > 6000)
      init_cn_mage(np,sp);
    else
      return;			/* no mage, can't summon */
  }
  
  net_points = new_spell_pts(np) - military_maint_spell_pts(np);
  while(net_points){		/* while we can still summon */
    tmp_sptr = NULL;
    for(sptr = user.spirit_list ; sptr ; sptr = sptr->next){
      if (np->spell_pts < sptr->cost)
	break;
      sindex = spirit_type_index(sptr->type);
      this_stype = spirit_types[sindex];
      if (! (this_stype.flags & badflags)){
	if (debug >= 2)  printf("looking at %s\n",sptr->type);
	tmp_sptr = sptr;
	}
      }

    if (tmp_sptr == NULL)
      break;

    if (debug >= 2)
      printf("cn summons %s\n",tmp_sptr->type);

    sindex = spirit_type_index(tmp_sptr->type);
    this_stype = spirit_types[sindex];
    army = make_army(this_stype.type, this_stype.type, this_stype.size,
		     A_DEFEND, np->id, sp->loc);
    army.id = free_army_id(np);
    army.next = NULL;
    army.flags = this_stype.flags;

    /***************** now insert it into the list *****************/

    ++np->n_armies;
    if (np->armies == NULL) { 		/* special case:  empty list */
      np->armies = (Sarmy *) malloc(sizeof(Sarmy));
      *(np->armies) = army;
      np->armies->next = NULL;
    } else {
      insert_army_nation(np, &army, -1);
    }
    insert_army_sector(sp, &army);
    np->spell_pts -= spirit_types[sindex].spell_pts_draft;
/*    printf("summon cost %d\n",spirit_types[sindex].spell_pts_draft);
    printf("net points %d\n",net_points); */
    net_points = new_spell_pts(np) - military_maint_spell_pts(np);
  }
}

/*-----------------------------do_cn_merge()--------------------------------
	merge cn armies together.  If an army has less than an optimal
number of units, check for other armies of the cn that are reachable and
within CN_VIEW sectors.  Move to the one that will give the greatest
movement left and merge.  Prefer to merge with other armies that have
less than the optimal number.

1-31-92 Merges armies that are weak to make larger armies.
10-25-92:  Merge armies according to merge_size_pct and pat_size_pct.
---------------------------------------------------------------------------*/
void do_cn_merge(Snation *np)
{
  int i,tx,ty,x,y,finalmv;
  Pt finalpos;
  Sarmy *bestarmy,*ap,*tmpap,*get_army();
  struct armyid *alist;
  struct argument args[N_EXEC_ARGS];
  struct tmp_map legal_moves[CN_SIDE][CN_SIDE];

  for(i = 0;i < CN_SIDE; i++){	/* fill in border of legal mv */
    legal_moves[0][i].mvleft = -2;
    legal_moves[CN_SIDE-1][i].mvleft = -2;
    legal_moves[i][0].mvleft = -2;
    legal_moves[i][CN_SIDE-1].mvleft = -2;
  }

  for(ap = np->armies ;ap ;ap = ap->next){	/* for all cns armies */
    printf("%s -- 0x%08lx\n", np->name, (unsigned long) ap);
    printf("%s -- 0x%08lx  %d\n", np->name, (unsigned long) ap, ap->id);
    i = max (opt_army_size * np->cn_params->merge_size_pct / 100,
	     OCCUPYING_SOLDIERS);
    printf("%s -- 0x%08lx  %d\n", np->name, (unsigned long) ap, ap->id);
    if((ap->n_soldiers >= i) &&
	(!atwar || (ap->n_soldiers * (ap->sp_bonus + 100) / 100 >= i)))
      continue;					/* skip loop if army ok. */
    if ((ap->status == A_PATROL) &&
	(ap->n_soldiers >= dom_min_int(OCCUPYING_SOLDIERS,
			       i * np->cn_params->pat_size_pct / 100))) {
      continue;					/* skip loop if army ok. */
    }
    if(is_mage(ap))
      continue;
    bestarmy = 0;
    check_moves(np,ap,legal_moves);		/* new function */

    /* now check for armies belonging to this player that can be moved to */

 /*   printf("%d needs to merge.\n",ap->id); */
    for (x = ap->pos.x-CN_VIEW; x <= ap->pos.x+CN_VIEW; x++) {
      for (y = ap->pos.y-CN_VIEW; y <= ap->pos.y+CN_VIEW; y++) {
	alist = world.map[(*wrapx)(x,y)][(*wrapy)(x,y)].alist;
	while(alist){
	  if(alist->owner == np->id){		/* if same owner */
	    tx = x - ap->pos.x + CN_VIEW + 1;
	    ty = y - ap->pos.y + CN_VIEW + 1;	/* coords in legal_move */
	    if(legal_moves[tx][ty].mvleft >= 0){
	      tmpap = get_army(np, alist->id);
	    /* printf("looking at army %d\n",tmpap->id); */
	      if (tmp_army_better(ap,tmpap,bestarmy) && tmpap != ap){
	    /* printf("best is %d\n",tmpap->id); */
	        bestarmy = tmpap;
		finalpos.x = x;
		finalpos.y = y;
		finalmv = legal_moves[tx][ty].mvleft;
              }
	    }
	  }
	  alist = alist->next;
	}	/* while alist */
      }		/* for y */
    }		/* for x */

    /* now move the army to the correct location and merge.  */
    if (bestarmy && (bestarmy->id != ap->id)){
      if (debug >= 2) {
        printf("cn army %d merges with %d.\n",ap->id,bestarmy->id);
      }
      args[1].data.num = ap->id;
      alist = world.map[(*wrapx)(x,y)][(*wrapy)(x,y)].alist;
      args[2].data.num = (*wrapx)(finalpos.x,finalpos.y);
      args[3].data.num = (*wrapy)(finalpos.x,finalpos.y);
      args[4].data.num = finalmv;
      cmd_amove(np,args);		/* move army to new location */
      args[1].data.num = ap->id;
      args[2].data.num = bestarmy->id;
      printf("%s -- 0x%08lx about to cmd_amerge\n", np->name, (unsigned long) ap);
      printf("%s -- 0x%08lx  %d about to cmd_amerge %d\n", np->name, (unsigned long) ap, ap->id, bestarmy->id);
      cmd_amerge(np,args);
    }
    printf("%s -- 0x%08lx end of ap loop\n", np->name, (unsigned long) ap);
    printf("%s -- 0x%08lx  %d end of ap loop\n", np->name, (unsigned long) ap, ap->id);
  }		/* for ap */
}

/*--------------------------------do_cn_split()------------------------------
	Split armies that have grown too large into manageable armies
1-31-92: Does not split weak armies as soon.
2-24-92: Splits patrolling armies into small ones.
10-25-92:  Split armies according to pat_size_pct.
----------------------------------------------------------------------------*/
void do_cn_split(Snation *np)
{
  Sarmy *ap;
  struct argument args[N_EXEC_ARGS];
  int patrol_size;

  for(ap = np->armies ;ap ;ap = ap->next){	/* for all cns armies */
    if (ap->status != A_PATROL) {
      if ((ap->n_soldiers < opt_army_size * 2) || is_spirit(ap))
        continue;				/* skip loop if small army */
      if (atwar &&
          (ap->n_soldiers * (ap->sp_bonus + 100) / 100 < opt_army_size * 2))
        continue;				/* skip loop if weak army */
      while(ap->n_soldiers >= opt_army_size*2){
        if (debug >= 2)
	  printf ("cn splits %d troops from army %d\n",opt_army_size,ap->id);
        args[1].data.num = ap->id;
        args[2].data.num = opt_army_size;
        cmd_asplit(np,args);
      }
    } else {			/*--- this is a patrol army, can be small ---*/
      if (!atwar)
	patrol_size = OCCUPYING_SOLDIERS/2;
      else
	patrol_size = dom_max_int(OCCUPYING_SOLDIERS/2, 
			opt_army_size * np->cn_params->pat_size_pct / 100);
      if ((ap->n_soldiers < patrol_size * 2) || is_spirit(ap))
        continue;				/* skip loop if small army */
      while(ap->n_soldiers >= patrol_size * 2){
        if (debug >= 2)
	  printf ("cn splits %d troops from army %d\n",patrol_size,ap->id);
        args[1].data.num = ap->id;
        args[2].data.num = patrol_size;
        cmd_asplit(np,args);
      }
    }
  }
}

/*-------------------------------do_cn_armies()------------------------------
	This routine moves cn armies to desirable sectors.  Des_array is
an array containing the desirability of each sector on the map.  legal_moves
is an array containing the sectors that the army can see, with the sectors
that the army can move to marked with the number of movepoints the army will
have left at that point.  The function looks for the most desirable sectors
that the army can move to and puts them in the highlist array.  It then
randomly picks a point from this array and moves the army to it.
----------------------------------------------------------------------------*/
void do_cn_armies(Snation *np, struct desire **des_array)
{
  int i,statidx,x,y,tx,ty,des_here,high_des,nhigh;
  struct pt highlist[CN_SIDE*CN_SIDE];
  Sarmy *ap;
  struct argument args[N_EXEC_ARGS];
  struct tmp_map legal_moves[CN_SIDE][CN_SIDE];

  find_desire(np,des_array);		       /* fill desireability array */
  for(i = 0;i < CN_SIDE; i++){		       /* fill in border of legal mv */
    legal_moves[0][i].mvleft = -2;
    legal_moves[CN_SIDE-1][i].mvleft = -2;
    legal_moves[i][0].mvleft = -2;
    legal_moves[i][CN_SIDE-1].mvleft = -2;
  }

  /* Have CN nation take sectors */
  
  if (np->n_armies == 0) {	/* If there are no armies, forget it */
    return;
  }
		/*--- now move defends(occupys) and patrols ---*/
  for (statidx = 0; statidx < NSTATS; statidx++) {
    for (ap = np->armies; ap; ap = ap->next) {
      if (ap->status != statary[statidx]) /* bad status */
	continue; 
      if(is_mage(ap) && ap->n_soldiers == 1)
        continue;
      if ( is_cargo(ap) || is_in_transport(ap) || is_uncontrolled(ap))
	continue;
      check_moves(np,ap,legal_moves);		/* new function */
      high_des = -1;
      for (x = ap->pos.x-CN_VIEW; x <= ap->pos.x+CN_VIEW; x++) {
        for (y = ap->pos.y-CN_VIEW; y <= ap->pos.y+CN_VIEW; y++) {
  
  /* Now check if it's the most desireable sector so far  */
  /* and if it can be moved to. Check only sectors with a */
  /* good altitude, and only look at enemy sectors if the */
  /* army is on defend (the only kind that can occupy)    */

          if (good_altitude(&world.map[(*wrapx)(x,y)][(*wrapy)(x,y)],np) &&
	        ((ap->status & (A_DEFEND|A_ATTACK)) ||
		(world.map[(*wrapx)(x,y)][(*wrapy)(x,y)].owner == np->id))) {
	    switch (ap->status) {	    
	      case A_PATROL:
	        des_here = des_array[(*wrapx)(x,y)][(*wrapy)(x,y)].patrol;
		break;
	      case A_INTERCEPT:
	        des_here = des_array[(*wrapx)(x,y)][(*wrapy)(x,y)].inter;
		break;
	      default:
		des_here = des_array[(*wrapx)(x,y)][(*wrapy)(x,y)].final;
	      }
	    tx = x - ap->pos.x + CN_VIEW + 1;
	    ty = y - ap->pos.y + CN_VIEW + 1;
	    if (legal_moves[tx][ty].mvleft >= 0){
	      if(des_here > high_des) {
	        nhigh = 1;
	        highlist[0].x = x;
	        highlist[0].y = y;
	        high_des = des_here;
	      }
	      if(des_here == high_des) {
	        highlist[nhigh].x = x;
	        highlist[nhigh++].y = y;
	      }
            }
	  }
        }		/* end y loop */
      }		/* end x loop */
  
    /* Move army to one of the desireable sectors and change status to OCCUPY */
    /* Change the desireability of the sector so not all the armies go there */
    /* 2-20-92:  Change the desirability for surrounding sectos if the army  */
    /* is on patrol. */

      if (nhigh == 0)		/* don't know how this could happen, but...*/
	continue;
      i = RND() % nhigh;
      tx = highlist[i].x - ap->pos.x + CN_VIEW + 1;
      ty = highlist[i].y - ap->pos.y + CN_VIEW + 1;
      x = (*wrapx)(highlist[i].x,highlist[i].y);
      y = (*wrapy)(highlist[i].x,highlist[i].y);
      args[1].data.num = ap->id;
      args[2].data.num = x;
      args[3].data.num = y;
      args[4].data.num = legal_moves[tx][ty].mvleft;
      cmd_amove(np,args);		/* move army to new location */
      if (debug >= 2) { 
	printf ("moving to desire value %d\n",des_array[x][y].final);
      }
      switch (ap->status){
	case A_DEFEND:
	case A_ATTACK:
	  if (world.map[x][y].owner != np->id){
            args[2].data.num = A_OCCUPY;
            cmd_astat(np,args);
	    }
	  else {
	    args[2].data.num = A_DEFEND;
	    cmd_astat (np,args);
	    }
          des_array[x][y].final = des_array[x][y].final * opt_army_size /
            (opt_army_size+ap->n_soldiers);
	  break;
	case A_PATROL:
	  for (tx = x-2; tx <= x+2; tx++)
	    for (ty = y - 2; ty <= y+2; ty++)
	      if ((tx == x-2)||(tx == x+2)||(ty == y-2)||(ty == y+2))	
		  des_array[(*wrapx)(tx,ty)][(*wrapy)(tx,ty)].patrol =
		    des_array[(*wrapx)(tx,ty)][(*wrapy)(tx,ty)].patrol/2;
	      else
		  des_array[(*wrapx)(tx,ty)][(*wrapy)(tx,ty)].patrol = 0;
	  des_array[(*wrapx)(tx,ty)][(*wrapy)(x,y)].inter = 
	    des_array[(*wrapx)(tx,ty)][(*wrapy)(x,y)].inter/2;
	  break;
	case A_INTERCEPT:
	  for (tx = x-1; tx <= x+1; tx++)
	    for (ty = y - 1; ty <= y+1; ty++)
	      des_array[(*wrapx)(tx,ty)][(*wrapy)(tx,ty)].inter = 0;
	  des_array[(*wrapx)(tx,ty)][(*wrapy)(x,y)].patrol = 
	    des_array[(*wrapx)(tx,ty)][(*wrapy)(x,y)].patrol/2;
	  break;
	default:
	  break;

      }
    }
  }
}

/*------------------------------do_cn_redesig()-------------------------------
	Redesignate sectors that the cn owns.
Step 1:
	If the cn is in danger of starving, i.e. the unemployment is low
and not enough food is being produced, the cn redesignates metal and jewel
mines into farms.  It creates lists of its metal and jewel mines, met_array
and jwl_array, ordered from highest metal/jewel value to lowest.  Then
the designations are changed.  At most, under half of the mines are changed,
and never more than the number needed to produce enough food.

Step 2:
	Sectors with no designation are designated, with preference to metal
mines and jewel mines. Sectors are only designated to farms if food_need > .85
if food_need is not > 1.1 then sectors of soil < 3 will not be used.
-----------------------------------------------------------------------------*/
void do_cn_redesig(Snation *np)
{
  Ssector *sp,*met_array[100],*jwl_array[100];
  struct pt_list *Pointer;
  int Loop, x, y, i, j, met_sect = 0, jwl_sect = 0, expend, income;
 
  if (np->money <= 0) return;

 		 /* Have CN nation redesignate sectors */
  
  if ((world.turn > 5) && (cn_food_need > 1.0) &&
      (get_unemployed(np) < get_n_civil(np) / 10)) {	/*DANGER of starving*/
    Pointer = np->ptlist;
    for (Loop = 0; Loop < np->n_sects; Loop++) {
      sp = &world.map[Pointer->pt.x][Pointer->pt.y];
      if (sp->designation == D_METAL_MINE && sp->soil > 0) {
	for (i = met_sect - 1; i >= 0; i--) {
	  if (sp->metal > met_array[i]->metal)
	    met_array[i+1] = met_array[i];
	  else
	    break;
	  }
	met_array[i+1] = sp;
	met_sect++;
	}
      else if (sp->designation == D_JEWEL_MINE && sp->soil > 0) {
	for (i = jwl_sect - 1; i >= 0; i--) {
	  if (sp->jewels > jwl_array[i]->jewels)
	    jwl_array[i+1] = jwl_array[i];
	  else
	    break;
	  }
	jwl_array[i+1] = sp;
	jwl_sect++;
	}
      Pointer = Pointer->next;
      }			/* end of for loop */

		/*----  Now Change the sectors into farms ----*/
    expend = calc_expend_food(np);
    income = calc_food(np);
    i = met_sect - 1;
    j = jwl_sect - 1;
    while ((i > met_sect/2) || (j > jwl_sect/2)){
      if (i > met_sect/2) {
        met_array[i]->designation = D_FARM;
        np->money -= desig_map[D_FARM].price;
        income += sector_food(met_array[i--]);
        if (debug >= 2) printf("cn changing metal mine to farm %d\n",income);
        }

      if(j > jwl_sect/2){
        jwl_array[j]->designation = D_FARM;
        np->money -= desig_map[D_FARM].price;
        income += sector_food(jwl_array[j--]);
        if (debug >= 2) printf("cn changing jewel mine to farm %d\n",income);
        }

      if (expend <= income)		/* check exit conditions: making */
	break;				/* enough food or no money left  */
      if (np->money < desig_map[D_FARM].price)
	break;
      }
    }
		/*--- Designate undesignated sectors ---*/

  Pointer = np->ptlist;
  for (Loop = 0; (Loop < np->n_sects) && (np->money > 0); Loop++) {
    x = Pointer->pt.x;
    y = Pointer->pt.y;
    if ((world.map[x][y].n_people > 0 || world.turn < 3) &&
	world.map[x][y].designation == D_NODESIG)  {
      if (world.map[x][y].metal + world.map[x][y].jewels > 0 ){
	if (world.map[x][y].metal >= world.map[x][y].jewels) {
	  world.map[x][y].designation = D_METAL_MINE;
	  np->money -= desig_map[D_METAL_MINE].price;
	} else {
	  world.map[x][y].designation = D_JEWEL_MINE;
	  np->money -= desig_map[D_JEWEL_MINE].price;
	}
      } else {
	if ((world.map[x][y].soil > 0) && (cn_food_need > .85 )) {
	  if (cn_food_need > 1.1 || world.map[x][y].soil > 3 ) {
	    world.map[x][y].designation = D_FARM;
 	    np->money -= desig_map[D_FARM].price; }
	}
      }
    }
  Pointer = Pointer->next;
  }
}


