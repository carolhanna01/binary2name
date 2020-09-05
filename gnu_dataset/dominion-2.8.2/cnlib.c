   /* nplib.c -- modules involved in CN movemaking */

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
#include <assert.h>
#include <string.h>

#include "config.h"
#include "dominion.h"
#include "misc.h"
#include "army.h"

/* some prototypes used here */
int count_unowned_sectors(Snation *np);
int count_enemy_sectors(Snation *np);
int cn_des(Snation *np, int x,int y, int unowned,
	   int mine, int enemy, int smax);
int add_square(int x, int y, int radius, int add,
	       struct desire **des_array,Snation *np);



#define CUTOFF1 -30		/* army bonus for draft cutoff 1 (cn_draft) */
#define CUTOFF2 0		/* army bonus for draft cutoff 2 (cn_draft) */

extern Sworld world;
extern Suser user;
extern struct s_desig_map desig_map[];
extern int (*wrapx)(), (*wrapy)();
extern int debug;
extern struct army_type *army_types;
extern struct spirit_type *spirit_types;

extern float cn_food_need,cn_metal_need,cn_jewel_need,cn_money_need;
extern int opt_army_size,atwar,cn_specific;

/*------------------------------get_good_types()-----------------------------
	This function fills an array with the indexed to army_types that will
be good for this cn.
	As of 2-2-92, this function only eliminates armies with bad flags.
The decision to draft cavemen or not is made in cn_draft.
	For now, cns will not draft transports, water-walk armies,
kamikaze armies, front-line armies, wizard armies or machines.  Also, the
cns will not consider armies with a lower or equal bonus to that of their
race-specific army (if any).  (Cavemen and spearmen are exceptions to this)
---------------------------------------------------------------------------*/
int get_good_types(int good_armies[])
{
  Savail_army *tmp_avail;
  struct army_type this_atype;
  int type_index,ngood = 0,badflags;

  badflags = AF_WATER |  AF_LAND | AF_INVERSE_ALT | AF_KAMIKAZE | AF_FRONT_LINE
	   | AF_MACHINE | AF_CARGO | AF_WIZARD;

  for(tmp_avail = user.avail_armies;tmp_avail ; tmp_avail = tmp_avail->next){
    type_index = army_type_index(tmp_avail->type);
    this_atype = army_types[type_index];
    if (this_atype.flags & badflags)
	 continue;

/*    if(this_atype.bonus < CUTOFF1 && current > cut1)
      continue;
    else if(this_atype.bonus < CUTOFF2 && current > cut2)
      continue; */

		/* replace army type with race-specific type if appropriate */
    if(cn_specific &&
	army_types[cn_specific-1].bonus >= this_atype.bonus &&
	this_atype.bonus >= 0)
      good_armies[ngood++] = cn_specific - 1;
    else
      good_armies[ngood++] = type_index;
    }
  if (debug >= 2) printf("ngood = %d\n",ngood);
  return(ngood);
}

/*---------------------------tmp_army_better()-------------------------------
	This function checks tmpap and oldap to see if tmpap would be better
for ap to merge with.  Better is defined as big enough after the merge
to take sectors, but smaller after the merge than the other army.
This is a crude way of preventing one army from growing too large.
Edit to your heart's content, if you think it will make the cn's better.
	(This function is called by cn_merge)
	As of 2-18-92, 'better' includes the requirement that the statuses
(A_PATROL, for example) are the same.
	3-5-92:  Patrols must be merged with patrols, attack and defend
can be merged together.
------------------------------------------------------------------------*/
int tmp_army_better(Sarmy *ap, Sarmy *tmpap, Sarmy *oldap)
{
  if(strcmp(tmpap->type,ap->type)){	/* different types */
    return (0);
  }
  
  if (ap->status == A_PATROL && ap->status != tmpap->status)
    return (0);			/* different statuses */

  if(!oldap)				/* anything is better than nothing */
    return(1);

  if(tmpap->n_soldiers + ap->n_soldiers < opt_army_size)
    return(0);				/* already have oldap */

  if(tmpap->n_soldiers < oldap->n_soldiers)
    return(1);				/* enough soldiers but not too big */
  else
    return(0);				/* too big */
}

/*---------------------------find_desire()------------------------------------
	Fill the desire array with the desireability for each sector on the
map.  The base desire goes from zero to half the length of the shortest side.
To find the final desire, the base desire is squared.  The desire for the
surrounding sectors is modified.  If the base desireOA is N, then all the
adjacent sectors have their desireability increased by N-1, the next sectors
out are increased by N-2, etc.  This will hopefully keep cn's moving in the
right general direction. (the base desire of owned sectors is 0, unless at war)
---------------------------------------------------------------------------*/
void find_desire(Snation *np, struct desire **des_array)
{
  int x,y,i,d,mine,unowned,enemy,smax;

  unowned = count_unowned_sectors(np);
  mine = np->n_sects;
  enemy = count_enemy_sectors(np) * 2;	/* over-estimate enemy */
  smax = dom_max_int(mine, dom_max_int(enemy,unowned));

  for(x = 0; x < world.xmax;x++)
    for(y = 0; y < world.ymax; y++){
      des_array[x][y].base = cn_des(np,x,y,unowned,mine,enemy,smax);
      des_array[x][y].final = 0;
    }

  for(x = 0; x < world.xmax;x++)
    for(y = 0; y < world.ymax; y++){
      d = des_array[x][y].base;
      des_array[x][y].final += d*d;
      if(atwar && d > 0){		/* sector owned by us or enemy */
        des_array[x][y].final += world.map[x][y].n_people/25;
        if (world.map[x][y].designation == D_CAPITAL) {
            des_array[x][y].final *= 2;
	    }
        }
      if (world.map[x][y].owner == np->id ) {
	des_array[x][y].inter = des_array[x][y].patrol 
	  = world.map[x][y].n_people/25;
      }
      else
	des_array[x][y].inter = des_array[x][y].patrol = 0;
      if(user.visible_sectors[x][y])
        for(i = 1;i < d && add_square(x,y,i,d-i,des_array,np);i++);
      }
/* This makes a big mess--------
    for(y = 0; y < world.ymax; y++){
      for(x = 0; x < world.xmax;x++)
        printf("%2d,",des_array[x][y].base);
      printf("\n");
    }
    if(debug){
    for(y = 0; y < world.ymax; y++){
      for(x = 0; x < world.xmax;x++)
        printf("%3d,",des_array[x][y].final);
      printf("\n");
    }
  } */
}

/*-------------------------------cn_des()-------------------------------------
	This function calculates the desireability of a sector to an cn.
When at peace, only unowned sectors are considered.  When at war, owned,
unowned, and enemy sectors are considered.  The desireability for our sectors
is modified by the number of enemy sectors we can see and the number of armies
we have.  Basically, this helps the cns finish off opponents and keeps them
from overextending themselves.
-----------------------------------------------------------------------------*/
int cn_des(Snation *np, int x,int y, int unowned,
	   int mine, int enemy, int smax)
{
  int num,i,j,dstat,tmp;
  Sdiplo **dm = user.diplo_matrix;

  if(!good_altitude(&world.map[x][y],np))
    return(0);
	
	/*-------  Ignore nations we are not at war with ---------*/

  if(atwar && world.map[x][y].owner){
    if (world.map[x][y].owner != np->id){
      dstat = get_diplo_status(dm, np->id, world.map[x][y].owner);
      if(dstat != WAR && dstat != JIHAD)
  	return(0);
      }
  } else if (world.map[x][y].owner)
    return (0);

	/*----- If at war, this sector is friendly, enemy, or unowned.---*/
	/*----- If not at war, this sector is unowned.		---------*/
  if(atwar && world.map[x][y].owner){		/* us or enemy */
    if(world.map[x][y].owner == np->id){	/* our sector */
      switch(world.map[x][y].designation){
        case D_FARM:
          num = 3 + world.map[x][y].soil * cn_food_need;
	  break;
        case D_METAL_MINE:
          num = 3 + world.map[x][y].metal * cn_metal_need;
	  break;
        case D_JEWEL_MINE:
          num = 3 + world.map[x][y].jewels * cn_jewel_need;
	  break;
        case D_CITY: case D_UNIVERSITY: case D_CAPITAL:
          num = 11;
	  break;
        default:
	  num = 2;
	  break;
        }
		/*----- Increase des. for being near enemy sects ------*/
      for(i = -1;i < 2;i++)
        for(j = -1;j < 2;j++)
          if((tmp = world.map[(*wrapx)(x+j,y+i)][(*wrapy)(x+j,y+i)].owner))
            if (tmp != np->id){
    	      dstat = get_diplo_status(dm, np->id, world.map[x][y].owner);
    	      if(dstat == WAR || dstat == JIHAD)
                num++;
	      }

      /* now adjust the number according to a 'magic' formula */
      num = ( num * enemy * (mine+6) ) / ( smax * (3 * np->n_armies + 1));
      }

    else{			/*----- Enemy sector -------*/
      num = 12;			/*--- 12 seems to work well ---*/
  
      for(i = -1;i < 2;i++)
        for(j = -1;j < 2;j++)
          if (world.map[(*wrapx)(x+j,y+i)][(*wrapy)(x+j,y+i)].owner == np->id)
            num++;
      num = num * np->cn_params->agg / 50;
      }
    }
  
  else{
    num = world.map[x][y].soil * cn_food_need;
    num += world.map[x][y].metal * cn_metal_need;
    num += world.map[x][y].jewels * cn_jewel_need;
    tmp  = 0;
    for(i = -1;i < 2;i++)
      for(j = -1;j < 2;j++)
        if(world.map[(*wrapx)(x+j,y+i)][(*wrapy)(x+j,y+i)].owner == np->id)
          tmp++;
    if(atwar)
      num = (num + tmp) * unowned / smax;  /* 2/3 un/smax */
    else
      num =  num + tmp * 20 / np->cn_params->exp;

/*    else num = num + tmp*2; */
    }

  num = min ( num, min (world.xmax, world.ymax) );
  return(num/2);
}

/*----------------------------add_square()---------------------------------
	This function adds a number, add to the desireability of the sectors
radius away from x,y.  Radius is defined rather loosely here, since the sectors
form a square.
---------------------------------------------------------------------------*/
int add_square(int x, int y, int radius, int add,
	       struct desire **des_array,Snation *np)
{
int i,tx,ty,id,flag = 0;
Ssector *sp;

  id = np->id;
  tx = x-radius;			/* bottom side from lower left corner */
  ty = y-radius;
  for(i = 0; i < radius*2;i++){
    sp = &world.map[(*wrapx)(tx+i,ty)][(*wrapy)(tx+i,y)];
    if(!good_altitude(sp,np))
      flag++;
    des_array[(*wrapx)(tx+i,ty)][(*wrapy)(tx+i,ty)].final += add;
  }

  ty = y+radius;			/* left side from upper left corner */
  for(i = 0; i < radius*2;i++){
    sp = &world.map[(*wrapx)(tx,ty-i)][(*wrapy)(tx-i,y-i)];
    if(!good_altitude(sp,np))
      flag++;
    des_array[(*wrapx)(tx,ty-i)][(*wrapy)(tx,ty-i)].final += add;
  }

  tx = x+radius;			/* top side from upper right corner */
  for(i = 0; i < radius*2;i++){
    sp = &world.map[(*wrapx)(tx-i,ty)][(*wrapy)(tx-i,y)];
    if(!good_altitude(sp,np))
      flag++;
    des_array[(*wrapx)(tx-i,ty)][(*wrapy)(tx-i,ty)].final += add;
  }

  ty = y-radius;			/* right side from lower right corner */
  for(i = 0; i < radius*2;i++){
    sp = &world.map[(*wrapx)(tx,ty+i)][(*wrapy)(tx,y+i)];
    if(!good_altitude(sp,np))
      flag++;
    des_array[(*wrapx)(tx,ty+i)][(*wrapy)(tx,ty+i)].final += add;
  }

  return((flag > 4*radius) ? 0:1);	/* equal to zero if lots of water */
}

/*--------------------------count_unowned_sectors()------------------------*/
int count_unowned_sectors(Snation *np)
{
int x,y,ret;

  ret = 0;
  for(y = 0; y < world.ymax; y++)
    for(x = 0; x < world.xmax;x++)
      if(user.visible_sectors[x][y] && good_altitude(&world.map[x][y],np))
	if(!world.map[x][y].owner) ret++;
  if (debug >= 2) printf("unowned = %d ",ret);
return ret;
}

/*--------------------------count_enemy_sectors()------------------------*/
int count_enemy_sectors(Snation *np)
{
  int x,y,ret,dstat;
  Sdiplo **dm;
  
  ret = 0;
  for(y = 0; y < world.ymax; y++)
    for(x = 0; x < world.xmax;x++)
      if(user.visible_sectors[x][y])
	if(world.map[x][y].owner && world.map[x][y].owner != np->id){
          dm = user.diplo_matrix;
          dstat = get_diplo_status(dm, np->id, world.map[x][y].owner);
          if(dstat == WAR || dstat == JIHAD)
	    ret++;
	}
  if (debug >= 2) printf("enemy = %d \n",ret);
  return ret;
}

/*----------------------------check_moves()-----------------------------------
	Ary represents the area of the map centered at the army ap.  This
function fills ary with the maximum movepoints the army can have left when
it arrives at the sector.  Of course, routes that are outside of the section
of the map in ary are not considered, so a larger value for CN_SIDE will
sometimes result in better movement.  However, since the algorithm includes
an insertion, it's best to keep the value small in the interest of time.
--------------------------------------------------------------------------*/
void check_moves(Snation *np, Sarmy *ap, struct tmp_map ary[CN_SIDE][CN_SIDE])
{
  int i,j,k;
  int x,y,tx,ty;
  int mv,mvleft;
  int head=0,tail=0;
  struct mvlist{
    int mv;
    Pt pos;
  } list[CN_SIDE * CN_SIDE];	/* doesn't really need to be this big */
  Ssector *sect;

  for(x = 1; x < CN_SIDE-1; x++) {
    for(y = 1; y < CN_SIDE-1; y++) {
      tx = x - (CN_VIEW+1) + ap->pos.x;	/* translate ary */
      ty = y - (CN_VIEW+1) + ap->pos.y;	/* coords to map */
      sect = &world.map[(*wrapx)(tx,ty)][(*wrapy)(tx,ty)];
      assert(x < CN_SIDE);
      assert(y < CN_SIDE);
      ary[x][y].mvleft = -1;
      ary[x][y].mvcost = get_army_move_cost(np,sect,ap);
    }
  }
 
  for(i = 0;i < CN_SIDE * CN_SIDE;i++) { /* initialize list of points */
    list[i].mv = 0;
  }
 
  /* start army in center of ary with whatever movement it has */
  ary[CN_VIEW+1][CN_VIEW+1].mvleft = ap->mvpts;
  list[0].mv = ap->mvpts;
  list[0].pos.x = CN_VIEW+1;
  list[0].pos.y = CN_VIEW+1;

  for(head = 0; list[head].mv; head++){		/* until list is empty */
    assert(head < CN_SIDE*CN_SIDE-1); /* make sure we're not going too far */
    mv = list[head].mv;
    for(x = list[head].pos.x-1;x <= list[head].pos.x + 1;x++) {
      for(y = list[head].pos.y-1;y <= list[head].pos.y + 1;y++) {
        assert(x < CN_SIDE);
        assert(y < CN_SIDE);
        if(ary[x][y].mvleft == -1 && ary[x][y].mvcost <= mv){
          mvleft = mv - ary[x][y].mvcost;
          ary[x][y].mvleft = mvleft;
          if(mvleft){
            for(j = tail;list[j].mv < mvleft;j--) {
              assert(j >= 0 && j < CN_SIDE*CN_SIDE);
            }
            for(k = tail;k > j;k--) {
              assert(k+1 >= 0 && k+1 < CN_SIDE*CN_SIDE);
              list[k+1] = list[k];
            }
            list[j+1].mv = mvleft;
            list[j+1].pos.x = x;
            list[j+1].pos.y = y;
            tail++;
          }
        }
      }
    }
  }

  if(debug){
    printf("ary for army %d= :\n",ap->id);
    for(y = 0; y < CN_SIDE; y++){
      for(x = 0;x < CN_SIDE;x++)
        printf("[%d,%d],",ary[x][y].mvleft,ary[x][y].mvcost);
      printf("\n");
    }
  }
}

/*--------------------------do_cn_diplo()------------------------------------
	Update the diplomacy for cns.  Change the diplomacy according to
a random number, and the cn hostility.  If the number is less than
the cn hostility, then the diplomacy status goes down.  Otherwise
it goes up.
1-17-92: The aggressiveness is now modified by the number of nations at
	war with the cn.
10-25-92:
	Implementing new cn parameters hos and dip_chan.
---------------------------------------------------------------------------*/
void do_cn_diplo(Snation *np)
{
  int i,dip_from,dip_to,num,up,down,hos;
  Snation *tmpnp;
  
  /* Take care of CN diplomacy here */
  
  atwar = 0;
  for(i = 1;i < world.n_nations;i++){
    tmpnp = &world.nations[i];
    if (is_active_ntn(tmpnp)) {
      if (!have_met(user.diplo_matrix, np->id, i))
        continue;
      dip_from = get_diplo_status(user.diplo_matrix, i, np->id);
      dip_to = get_diplo_status(user.diplo_matrix, np->id, i);
      if(dip_from == WAR || dip_from == JIHAD ||
	  dip_to == WAR || dip_to == JIHAD)
	atwar++;
      }
    }
  hos = np->cn_params->hos / (atwar + 1);

  for(i = 1;i < world.n_nations;i++){
    tmpnp = &world.nations[i];
    if (is_active_ntn(tmpnp) && (!tmpnp->cn_flag || CN_FIGHT)) { 
      if (!have_met(user.diplo_matrix, np->id, i))
        continue;
      dip_to = get_diplo_status(user.diplo_matrix, np->id, i);
      dip_from = get_diplo_status(user.diplo_matrix, i, np->id);
/*      if (debug >= 2) */ printf("from = %d, to = %d\n",dip_from,dip_to);

				/* calculate chances of changing up and down */
      down = hos * np->cn_params->dip_chan / 100;
      up = (100 - hos) * np->cn_params->dip_chan / 100;
      num = RND()%100;
      if (debug >= 2) printf("num = %d ",num);

      if(num < hos || dip_from < dip_to){		/* lower diplo */
        if(num < down/2 || dip_from < dip_to)
          dip_to--;
        if(num < down || dip_from < dip_to)
	  dip_to--;
	}
      else{					/* increase diplo */
	num = 100 - num;
        if(num < up/2)
	  dip_to++;
        if(num < up && dip_to < dip_from)
	  dip_to++;
	}

      dip_to = dom_max_int(dip_to, JIHAD); /* stay in bounds */
      dip_to = dom_min_int(dip_to, TREATY);
  /*    if (debug >= 2) { */
	printf("up = %d, down = %d, newto = %d\n",up,down,dip_to);
  /*   } */
      set_diplo_status(user.diplo_matrix,np->id,i,dip_to);
      }
    }
  dump_diplo(np,user.diplo_matrix, world.n_nations);
  if (debug >= 2) {
    printf ("atwar = %d\n",atwar);
  }
}

/*---------------------------init_cn_mage()-----------------------------------
	Initiate a mage for the cn.  Copied from menus.c
-----------------------------------------------------------------------------*/
void init_cn_mage(Snation *np, Ssector *sp)
{
  Sarmy army, make_army();

  army = make_army("Mage", "Mage", 1, A_DEFEND, np->id, sp->loc);
  army.jewel_maint = 1000;
  army.id = free_army_id(np);
  army.flags |= AF_WIZARD;
  army.next = NULL;
    /* now insert it into the list */
  np->n_armies++;
  if (np->armies == NULL) { /* special case:  empty list */
    np->armies = (Sarmy *) malloc(sizeof(Sarmy));
    *(np->armies) = army;
    np->armies->next = NULL;
  } else {
    insert_army_nation(np, &army, -1);
  }
  insert_army_sector(sp, &army);
  np->jewels -= INITIATION_JEWELS;
}


int count_troops_with(Snation *np, int status)
{
Sarmy *ap;
int ret = 0;

for (ap = np->armies; ap; ap = ap->next)
  if (ap->status == status)
    ret += ap->n_soldiers;

return (ret);
}
